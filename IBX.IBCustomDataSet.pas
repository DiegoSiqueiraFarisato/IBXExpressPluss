{$A8} {$R-}
{*************************************************************}
{                                                             }
{       Embarcadero Delphi Visual Component Library           }
{       InterBase Express core components                     }
{                                                             }
{       Copyright (c) 1998-2015 Embarcadero                   }
{                                                             }
{    InterBase Express is based in part on the product        }
{    Free IB Components, written by Gregory H. Deatz for      }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.          }
{    Free IB Components is used under license.                }
{                                                             }
{    Additional code created by Jeff Overcash and used        }
{    with permission.                                         }
{*************************************************************}

unit IBX.IBCustomDataSet;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.Types, IBX.IBExternals,
  IBX.IB, IBX.IBHeader, IBX.IBDatabase, IBX.IBSQL, Data.Db, IBX.IBBlob,
  System.Generics.Collections;

const
  BufferCacheSize    =  1000;  { Allocate cache in this many record chunks}
  UniCache           =  2;     { Uni-directional cache is 2 records big }

type
  TIBCustomDataSet = class;
  TIBDataSet = class;
  {$ifdef compilerversion < 12}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid)]
  {$else}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidiOSDevice64 or pidAndroidArm32)]
  {$endif}

  TIBDataSetUpdateObject = class(TComponent)
  private
    FRefreshSQL: TStrings;
    procedure SetRefreshSQL(value: TStrings);
  protected
    function GetDataSet: TIBCustomDataSet; virtual; abstract;
    procedure SetDataSet(ADataSet: TIBCustomDataSet); virtual; abstract;
    procedure Apply(UpdateKind: TUpdateKind); virtual; abstract;
    function GetSQL(UpdateKind: TUpdateKind): TStrings; virtual; abstract;
    property DataSet: TIBCustomDataSet read GetDataSet write SetDataSet;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RefreshSQL: TStrings read FRefreshSQL write SetRefreshSQL;
  end;

  TBlobDataArray = array[0..0] of TIBBlobStream;
  PBlobDataArray = ^TBlobDataArray;

  { TIBCustomDataSet }
  TFieldData = record
    fdDataType: Short;
    fdDataScale: Short;
    fdNullable: Boolean;
    fdIsNull: Boolean;
    fdDataSize: integer;
    fdDataLength: integer;
    fdDataOfs: Integer;
    fdCharsetSize : Short;
    fdPersistedFloatField : Boolean;
  end;
  PFieldData = ^TFieldData;

  TCachedUpdateStatus = ( cusUnmodified, cusModified, cusInserted,
                         cusDeleted, cusUninserted );
  TIBDBKey = record
    DBKey: array[0..7] of Byte;
  end;
  PIBDBKey = ^TIBDBKey;

  TRecordData = record
    rdBookmarkFlag: TBookmarkFlag;
    rdFieldCount: Short;
    rdRecordNumber: Long;
    rdCachedUpdateStatus: TCachedUpdateStatus;
    rdUpdateStatus: TUpdateStatus;
    rdSavedOffset: DWORD;
    rdDBKey: TIBDBKey;
    rdFields: array[1..1] of TFieldData;
  end;
  PRecordData = ^TRecordData;

  { TIBStringField allows us to have strings longer than 8196 }

  TIBStringField = class(TWideStringField)
  private
    buffer : TValueBuffer;
    FEmptyAsNull: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue(var Value: string): Boolean;
    procedure SetAsString(const Value: string); override;
    property Value: UnicodeString read GetAsWideString write SetAsWideString;
  published
    property EmptyAsNull : Boolean read FEmptyAsNull write FEmptyAsNull default true;
  end;

  { TIBBCDField }
  {  Actually, there is no BCD involved in this type,
     instead it deals with currency types.
     In IB, this is an encapsulation of Numeric (x, y)
     where x < 18 and y <= 4.
     Note: y > 4 will default to Floats
  }
  TIBBCDField = class(TBCDField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsCurrency: Currency; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    [default (8)]
    property Size default 8;
  end;

  TIBDataLink = class(TDetailDataLink)
  private
    [weak] FDataSet: TIBCustomDataSet;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    function GetDetailDataSet: TDataSet; override;
    procedure CheckBrowseMode; override;
  public
    constructor Create(ADataSet: TIBCustomDataSet);
    destructor Destroy; override;
  end;

  TIBGeneratorApplyEvent = (gamOnNewRecord, gamOnPost, gamOnServer);

  TIBGeneratorField = class(TPersistent)
  private
    FField: string;
    FGenerator: string;
    FIncrementBy: Integer;
    [weak]
    DataSet: TIBCustomDataSet;
    
    FApplyEvent: TIBGeneratorApplyEvent;
    function  IsComplete: Boolean;
  public
    constructor Create(ADataSet: TIBCustomDataSet);
    function  ValueName: string;
    procedure Apply;
    procedure Assign(Source: TPersistent); override;
  published
    property Field : string read FField write FField;
    property Generator : string read FGenerator write FGenerator;
    [default (1)]
    property IncrementBy : Integer read FIncrementBy write FIncrementBy;
    [default (Ord(gamOnNewRecord))]
    property ApplyEvent : TIBGeneratorApplyEvent read FApplyEvent write FApplyEvent;
  end;

  { TIBCustomDataSet }
  TIBUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApply, uaApplied);

  TIBUpdateErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
                                 UpdateKind: TUpdateKind; var UpdateAction: TIBUpdateAction)
                                 of object;
  TIBUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
                                   var UpdateAction: TIBUpdateAction) of object;

  TIBUpdateRecordTypes = set of TCachedUpdateStatus;

  TLiveMode = (lmInsert, lmModify, lmDelete, lmRefresh);
  TLiveModes = Set of TLiveMode;

  TIBCustomDataSet = class(TDataSet)
  private
    FNeedsRefresh: Boolean;
    FForcedRefresh: Boolean;
    FBase: TIBBase;
    FBlobCacheOffset: Integer;
    FBlobStreamList: TObjectList<TIBBlobStream>;
    FBufferChunks: Integer;
    FBufferCache,
    FOldBufferCache: TRecBuf;
    FFilterBuffer: TRecBuf;
    FModelBuffer,
    FOldBuffer, FTempBuffer: TRecBuf;
    FBufferChunkSize,
    FCacheSize,
    FOldCacheSize: Integer;
    FBPos,
    FOBPos,
    FBEnd,
    FOBEnd: DWord;
    FCachedUpdates: Boolean;
    FCalcFieldsOffset: Integer;
    FCurrentRecord: Long;
    FDeletedRecords: Long;
    FOpen: Boolean;
    FInternalPrepared: Boolean;
    FQDelete,
    FQInsert,
    FQRefresh,
    FQSelect,
    FQModify: TIBSQL;
    FRecordBufferSize: Integer;
    FRecordCount: Integer;
    FRecordSize: Integer;
    FUniDirectional: Boolean;
    FUpdateMode: TUpdateMode;
    FUpdateObject: TIBDataSetUpdateObject;
    FParamCheck: Boolean;
    FUpdatesPending: Boolean;
    FUpdateRecordTypes: TIBUpdateRecordTypes;
    FMappedFieldPosition: array of Integer;
    FDataLink: TIBDataLink;
    FStreamedActive : Boolean;
    FLiveMode: TLiveModes;
    FGeneratorField: TIBGeneratorField;
    FRowsAffected: Integer;

    FBeforeDatabaseDisconnect,
    FAfterDatabaseDisconnect,
    FDatabaseFree: TNotifyEvent;
    FOnUpdateError: TIBUpdateErrorEvent;
    FOnUpdateRecord: TIBUpdateRecordEvent;
    FBeforeTransactionEnd,
    FAfterTransactionEnd,
    FTransactionFree: TNotifyEvent;

    function GetSelectStmtHandle: TISC_STMT_HANDLE;
    procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetUpdateObject(Value: TIBDataSetUpdateObject);

    function AdjustCurrentRecord(Buffer: TRecBuf; GetMode: TGetMode): TGetResult;
    procedure AdjustRecordOnInsert(Buffer: TRecBuf);
    function CanEdit: Boolean;
    function CanInsert: Boolean;
    function CanDelete: Boolean;
    function CanRefresh: Boolean;
    procedure CheckEditState;
    procedure ClearBlobCache;
    procedure CopyRecordBuffer(Source, Dest: TRecBuf);
    procedure DoBeforeDatabaseDisconnect(Sender: TObject);
    procedure DoAfterDatabaseDisconnect(Sender: TObject);
    procedure DoDatabaseFree(Sender: TObject);
    procedure DoBeforeTransactionEnd(Sender: TObject);
    procedure DoAfterTransactionEnd(Sender: TObject);
    procedure DoTransactionFree(Sender: TObject);
{$IFDEF NEXTGEN}
    procedure FetchCurrentRecordToBuffer(Qry: TIBSQL; RecordNumber: Integer;
                                         Buffer: TRecBuf);
{$ELSE}
    procedure FetchCurrentRecordToBuffer(Qry: TIBSQL; RecordNumber: Integer;
                                         Buffer: TRecBuf);
{$ENDIF NEXTGEN}
    function GetDatabase: TIBDatabase;
    function GetDBHandle: PISC_DB_HANDLE;
    function GetDeleteSQL: TStrings;
    function GetInsertSQL: TStrings;
    function GetSQLParams: TIBXSQLDA;
    function GetRefreshSQL: TStrings;
    function GetSelectSQL: TStrings;
    function GetStatementType: TIBSQLTypes;
    function GetModifySQL: TStrings;
    function GetTransaction: TIBTransaction;
    function GetTRHandle: PISC_TR_HANDLE;
    procedure InternalDeleteRecord(Qry: TIBSQL; Buff: TRecBuf);
    function InternalLocate(const KeyFields: string; const KeyValues: Variant;
                            Options: TLocateOptions): Boolean;
    procedure InternalPostRecord(Qry: TIBSQL; Buff: TRecBuf);
    procedure InternalRevertRecord(RecordNumber: Integer);
    function IsVisible(Buffer: TRecBuf): Boolean;
    procedure SaveOldBuffer(Buffer: TRecBuf);
    procedure SetBufferChunks(Value: Integer);
    procedure SetDatabase(Value: TIBDatabase);
    procedure SetDeleteSQL(Value: TStrings);
    procedure SetInsertSQL(Value: TStrings);
    procedure SetInternalSQLParams(Qry: TIBSQL; Buffer: TRecBuf);
    procedure SetRefreshSQL(Value: TStrings);
    procedure SetSelectSQL(Value: TStrings);
    procedure SetModifySQL(Value: TStrings);
    procedure SetTransaction(Value: TIBTransaction);
    procedure SetUpdateRecordTypes(Value: TIBUpdateRecordTypes);
    procedure SetUniDirectional(Value: Boolean);
    procedure RefreshParams;
    procedure SQLChanging(Sender: TObject);
    function AdjustPosition(FCache: TRecBuf; Offset: DWORD;
                            Origin: Integer): Integer;
    procedure ReadCache(FCache: TRecBuf; Offset: DWORD; Origin: Integer;
                       Buffer: TRecBuf);
    procedure ReadRecordCache(RecordNumber: Integer; Buffer: TRecBuf;
                              ReadOldBuffer: Boolean);
    procedure WriteCache(FCache: TRecBuf; Offset: DWORD; Origin: Integer;
                        Buffer: TRecBuf);
    procedure WriteRecordCache(RecordNumber: Integer; Buffer: TRecBuf);
    function InternalGetRecord(Buffer: TRecBuf; GetMode: TGetMode;
                       DoCheck: Boolean): TGetResult;
    procedure SetGeneratorField(const Value: TIBGeneratorField);
    function InternalGetFieldData(Field: TField; Buffer: TValueBuffer): Boolean;
    procedure InternalSetFieldData(Field: TField; Buffer: TValueBuffer); virtual;
    function GetPlan: String;
    function GetCanceled: Boolean;

  protected
    procedure ActivateConnection;
    function ActivateTransaction: Boolean;
    procedure DeactivateTransaction;
    procedure CheckDatasetClosed;
    procedure CheckDatasetOpen;
    function GetActiveBuf: TRecBuf;
    procedure InternalBatchInput(InputObject: TIBBatchInput); virtual;
    procedure InternalBatchOutput(OutputObject: TIBBatchOutput); virtual;
    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
    procedure InternalExecQuery; virtual;
    procedure InternalRefreshRow; virtual;
    procedure InternalSetParamsFromCursor; virtual;
    procedure CheckNotUniDirectional;
    procedure SetActive(Value: Boolean); override;

    { IProviderSupport }
    procedure PSEndTransaction(Commit: Boolean); override;
    procedure PSExecute; override;
    function PSExecuteStatement(const ASQL: String; AParams: TParams): Integer; overload; override;
    function PSExecuteStatement(const ASQL: String; AParams: TParams;
      var ResultSet: TDataSet): Integer; overload; override;
    function PSGetTableName: String; override;
    function PSGetQuoteChar: String; override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    function PSInTransaction: Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSStartTransaction; override;
    procedure PSReset; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;

    { TDataSet support }
    procedure InternalInsert; override;
    procedure InitRecord(Buffer: TRecBuf); override;
    procedure Disconnect; virtual;
    function ConstraintsStored: Boolean;
    procedure ClearCalcFields(Buffer: NativeInt);overload; override;
{$IFNDEF NEXTGEN}
    procedure ClearCalcFields(Buffer: TRecordBuffer); overload; override;
{$ENDIF !NEXTGEN}
    procedure CreateFields; override;
{$IFDEF NEXTGEN}
    function AllocRecBuf: TRecBuf; override;
    procedure FreeRecBuf(var Buffer: TRecBuf); override;
{$ELSE}
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
{$ENDIF NEXTGEN}
    procedure DoBeforeDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); override;
    function GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag; override;
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function GetRecNo: Integer; override;
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
{$IFNDEF NEXTGEN}
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
                       DoCheck: Boolean): TGetResult; override;
{$ENDIF NEXTGEN}
    function GetRecordCount: Integer; override;
    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: TRecBuf; Append: Boolean); override;
    procedure InternalCancel; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
{$IFNDEF NEXTGEN}
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
{$ENDIF !NEXTGEN}

    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecBuf); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    procedure InternalSetToRecord(Buffer: TRecBuf); override;
    function IsCursorOpen: Boolean; override;
    procedure ReQuery;
    procedure SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: TBookmark); override;
    procedure SetCachedUpdates(Value: Boolean);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFieldData(Field : TField; Buffer: TValueBuffer); overload; override;
    procedure SetFieldData(Field : TField; Buffer: TValueBuffer; NativeFormat : Boolean); overload; override;
    procedure SetRecNo(Value: Integer); override;
    procedure DoOnNewRecord; override;
    procedure Loaded; override;

  protected
    {Likely to be made public by descendant classes}
    property SQLParams: TIBXSQLDA read GetSQLParams;
    property Params: TIBXSQLDA read GetSQLParams;
    property InternalPrepared: Boolean read FInternalPrepared;
    property QDelete: TIBSQL read FQDelete;
    property QInsert: TIBSQL read FQInsert;
    property QRefresh: TIBSQL read FQRefresh;
    property QSelect: TIBSQL read FQSelect;
    property QModify: TIBSQL read FQModify;
    property StatementType: TIBSQLTypes read GetStatementType;
    property SelectStmtHandle: TISC_STMT_HANDLE read GetSelectStmtHandle;
    property LiveMode : TLiveModes read FLiveMode;

    {Likely to be made published by descendant classes}
    [default (1000)]
    property BufferChunks: Integer read FBufferChunks write SetBufferChunks;
    [default (false)]
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates;
    [default (false)]
    property UniDirectional: Boolean read FUniDirectional write SetUniDirectional;
    property DeleteSQL: TStrings read GetDeleteSQL write SetDeleteSQL;
    property InsertSQL: TStrings read GetInsertSQL write SetInsertSQL;
    property RefreshSQL: TStrings read GetRefreshSQL write SetRefreshSQL;
    property SelectSQL: TStrings read GetSelectSQL write SetSelectSQL;
    property ModifySQL: TStrings read GetModifySQL write SetModifySQL;
    [default (Ord(upWhereAll))]
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode;
    [default (true)]
    property ParamCheck: Boolean read FParamCheck write FParamCheck;
    property GeneratorField : TIBGeneratorField read FGeneratorField write SetGeneratorField;

    property BeforeDatabaseDisconnect: TNotifyEvent read FBeforeDatabaseDisconnect
                                                 write FBeforeDatabaseDisconnect;
    property AfterDatabaseDisconnect: TNotifyEvent read FAfterDatabaseDisconnect
                                                write FAfterDatabaseDisconnect;
    property DatabaseFree: TNotifyEvent read FDatabaseFree
                                        write FDatabaseFree;
    property BeforeTransactionEnd: TNotifyEvent read FBeforeTransactionEnd
                                             write FBeforeTransactionEnd;
    property AfterTransactionEnd: TNotifyEvent read FAfterTransactionEnd
                                            write FAfterTransactionEnd;
    property TransactionFree: TNotifyEvent read FTransactionFree
                                           write FTransactionFree;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ApplyUpdates;
    function CachedUpdateStatus: TCachedUpdateStatus;
    procedure CancelUpdates;
    procedure FetchAll;
    function LocateNext(const KeyFields: string; const KeyValues: Variant;
                        Options: TLocateOptions): Boolean;
    procedure RecordModified(Value: Boolean);
    procedure RevertRecord;
    procedure Undelete;
    procedure Post; override;
    function Current : TIBXSQLDA;
    function SQLType : TIBSQLTypes;

    { TDataSet support methods }
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetCurrentRecord(Buffer: TRecBuf): Boolean; override;
{$IFNDEF NEXTGEN}
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override; deprecated;
{$ENDIF !NEXTGEN}
    function GetFieldData(Field : TField; var Buffer: TValueBuffer) : Boolean; overload; override;
    function GetFieldData(Field: TField; var Buffer: TValueBuffer; NativeFormat: Boolean): Boolean; overload; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
                    Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
                    const ResultFields: string): Variant; override;
    function UpdateStatus: TUpdateStatus; override;
    function IsSequenced: Boolean; override;
    procedure OutputXML(OutputObject: TIBOutputXML);

    property DBHandle: PISC_DB_HANDLE read GetDBHandle;
    property TRHandle: PISC_TR_HANDLE read GetTRHandle;
    property UpdateObject: TIBDataSetUpdateObject read FUpdateObject write SetUpdateObject;
    property UpdatesPending: Boolean read FUpdatesPending;
    property UpdateRecordTypes: TIBUpdateRecordTypes read FUpdateRecordTypes
                                                      write SetUpdateRecordTypes;
    property RowsAffected : Integer read FRowsAffected;
    property Plan: String read GetPlan;
    property Canceled : Boolean read GetCanceled;

  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Transaction: TIBTransaction read GetTransaction
                                          write SetTransaction;
    [default (false)]
    property ForcedRefresh: Boolean read FForcedRefresh
                                    write FForcedRefresh;
    property AutoCalcFields;
    [default (false)]
    property ObjectView;
    property FieldOptions;
    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property OnUpdateError: TIBUpdateErrorEvent read FOnUpdateError
                                                 write FOnUpdateError;
    property OnUpdateRecord: TIBUpdateRecordEvent read FOnUpdateRecord
                                                   write FOnUpdateRecord;
  end;

  {$ifdef compilerversion < 12}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid)]
  {$else}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidiOSDevice64 or pidAndroidArm32)]
  {$endif}

  TIBDataSet = class(TIBCustomDataSet)
  private
    FPSParams: TParams; // Temporary needed for DataSnap
    function GetPrepared: Boolean;
    procedure CreateParams;

  protected
    function PSGetParams: TParams; override;
    procedure PSSetParams(AParams: TParams); override;
    procedure PSSetCommandText(const CommandText: string); override;
    function PSGetCommandText: String; override;
    function PSGetCommandType: TPSCommandType; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure InternalOpen; override;

  public
    procedure Prepare;
    procedure UnPrepare;
    procedure BatchInput(InputObject: TIBBatchInput);
    procedure BatchOutput(OutputObject: TIBBatchOutput);
    procedure ExecSQL;

  public
    destructor Destroy; override;
    function ParamByName(Idx : String) : TIBXSQLVAR;
    property Params;
    property Prepared : Boolean read GetPrepared;
    property StatementType;
    property SelectStmtHandle;
    property LiveMode;

  published
    { TIBCustomDataSet }
    property BufferChunks;
    property CachedUpdates;
    property DeleteSQL;
    property InsertSQL;
    property RefreshSQL;
    property SelectSQL;
    property ModifySQL;
    property ParamCheck;
    property UniDirectional;
    property Filtered;
    property GeneratorField;
    property BeforeDatabaseDisconnect;
    property AfterDatabaseDisconnect;
    property DatabaseFree;
    property BeforeTransactionEnd;
    property AfterTransactionEnd;
    property TransactionFree;
    property UpdateObject;

    { TIBDataSet }
    property Active;
    property AutoCalcFields;
    property DataSource read GetDataSource write SetDataSource;

    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  { TIBDSBlobStream }
  TIBDSBlobStream = class(TStream)
  protected
    FField: TField;
    FBlobStream: TIBBlobStream;
    FModified : Boolean;
    FConvertedPosition : Int64;
  public
    constructor Create(AField: TField; ABlobStream: TIBBlobStream;
                       Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TIBDefaultFieldClasses = TDictionary<TFieldType, TFieldClass>;

var
  DefaultFieldClasses : TIBDefaultFieldClasses;

implementation

uses 
  Data.FmtBcd, Data.DBCommon, Data.DBConsts, IBX.IBUtils, System.Math;

const
  BUFFER_BEGIN = 0;
  BUFFER_CURRENT = 1;
  BUFFER_END = 2;

{ TIBStringField}

constructor TIBStringField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEmptyAsNull := true;
end;

class procedure TIBStringField.CheckTypeSize(Value: Integer);
begin
  if (Value < 0) then
    DatabaseError(SInvalidFieldSize);
end;

function TIBStringField.GetAsString: string;
begin
  if not GetValue(Result) then
    Result := '';
end;

function TIBStringField.GetAsVariant: Variant;
var
  S: string;
begin
  if GetValue(S) then
    Result := S
  else
    Result := Null;
end;

function TIBStringField.GetValue(var Value: string): Boolean;
begin
  if not Assigned(Buffer) then
    SetLength(Buffer, (Size + 1) * SizeOf(PChar));

  Result := GetData(Buffer);
  if Result then
    Value := String(PChar(Buffer));
end;

procedure TIBStringField.SetAsString(const Value: string);
var
  Buffer: TValueBuffer;
begin
  Buffer := TEncoding.Unicode.GetBytes(Value);
  SetLength(Buffer, Length(Buffer) + SizeOf(Char));
  Buffer[Length(Buffer) - 2] := 0;
  Buffer[Length(Buffer) - 1] := 0;
  SetData(Buffer, False);
end;

destructor TIBStringField.Destroy;
begin
  inherited;
end;

{ TIBBCDField }

constructor TIBBCDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBCD);
  Size := 8;
end;

class procedure TIBBCDField.CheckTypeSize(Value: Integer);
begin
{ No need to check as the base type is currency, not BCD }
end;

function TIBBCDField.GetAsCurrency: Currency;
begin
  if not GetValue(Result) then
    Result := 0;
end;

function TIBBCDField.GetAsString: string;
var
  C: System.Currency;
begin
  if GetValue(C) then
    Result := CurrToStr(C)
  else
    Result := '';
end;

function TIBBCDField.GetAsVariant: Variant;
var
  C: System.Currency;
begin
  if GetValue(C) then
    Result := C
  else
    Result := Null;
end;

function TIBBCDField.GetDataSize: Integer;
begin
  Result := 8;
end;

{ TIBDataLink }

constructor TIBDataLink.Create(ADataSet: TIBCustomDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

destructor TIBDataLink.Destroy;
begin
  FDataSet.FDataLink := nil;
  inherited Destroy;
end;


procedure TIBDataLink.ActiveChanged;
begin
  if FDataSet.Active then
    FDataSet.RefreshParams;
end;


function TIBDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TIBDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FDataSet.Active then
    FDataSet.RefreshParams;
end;

procedure TIBDataLink.CheckBrowseMode;
begin
  if FDataSet.Active then
    FDataSet.CheckBrowseMode;
end;

{ TIBCustomDataSet }

constructor TIBCustomDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBase := TIBBase.Create(Self);
  FCurrentRecord := -1;
  FDeletedRecords := 0;
  FUniDirectional := False;
  FBufferChunks := BufferCacheSize;
  FBlobStreamList := TObjectList<TIBBlobStream>.Create(True);
  FDataLink := TIBDataLink.Create(Self);
  FQDelete := TIBSQL.Create(Self);
  FQDelete.OnSQLChanging := SQLChanging;
  FQDelete.GoToFirstRecordOnExecute := False;
  FQInsert := TIBSQL.Create(Self);
  FQInsert.OnSQLChanging := SQLChanging;
  FQInsert.GoToFirstRecordOnExecute := False;
  FQRefresh := TIBSQL.Create(Self);
  FQRefresh.OnSQLChanging := SQLChanging;
  FQRefresh.GoToFirstRecordOnExecute := False;
  FQSelect := TIBSQL.Create(Self);
  FQSelect.OnSQLChanging := SQLChanging;
  FQSelect.GoToFirstRecordOnExecute := False;
  FQModify := TIBSQL.Create(Self);
  FQModify.OnSQLChanging := SQLChanging;
  FQModify.GoToFirstRecordOnExecute := False;

  if AOwner is TIBDatabase then
    Database := TIBDatabase(AOwner)
  else
    if AOwner is TIBTransaction then
      Transaction := TIBTransaction(AOwner);

  FUpdateRecordTypes := [cusUnmodified, cusModified, cusInserted];
  FParamCheck := True;
  FForcedRefresh := False;
  FGeneratorField := TIBGeneratorField.Create(Self);
  {Bookmark Size is Integer for IBX}
  BookmarkSize := SizeOf(Integer);
  FBase.BeforeDatabaseDisconnect := DoBeforeDatabaseDisconnect;
  FBase.AfterDatabaseDisconnect := DoAfterDatabaseDisconnect;
  FBase.OnDatabaseFree := DoDatabaseFree;
  FBase.BeforeTransactionEnd := DoBeforeTransactionEnd;
  FBase.AfterTransactionEnd := DoAfterTransactionEnd;
  FBase.OnTransactionFree := DoTransactionFree;
  FLiveMode := [];
  FRowsAffected := 0;
  FStreamedActive := false;
end;

destructor TIBCustomDataSet.Destroy;
begin

  Close;
  FreeAndNil(FDataLink);
  FBase.Free;
  FBase := nil;
  ClearBlobCache;
  FreeAndNil(FBlobStreamList);
  FreeMem(Pointer(FBufferCache), 0);
  FreeAndNil(FGeneratorField);
  FreeMem(Pointer(FOldBufferCache), 0);
  FCacheSize := 0;
  FOldCacheSize := 0;
  FMappedFieldPosition := nil;

  inherited Destroy;
end;

function TIBCustomDataSet.AdjustCurrentRecord(Buffer: TRecBuf; GetMode: TGetMode):
                                             TGetResult;
begin
  while not IsVisible(Buffer) do
  begin
    if GetMode = gmPrior then
    begin
      Dec(FCurrentRecord);
      if FCurrentRecord = -1 then
      begin
        result := grBOF;
        exit;
      end;
      ReadRecordCache(FCurrentRecord, Buffer, False);
    end
    else
    begin
      Inc(FCurrentRecord);
      if (FCurrentRecord = FRecordCount) then
      begin
        if (not FQSelect.EOF) and (FQSelect.Next <> nil) then
        begin
          FetchCurrentRecordToBuffer(FQSelect, FCurrentRecord, Buffer);
          Inc(FRecordCount);
        end
        else
        begin
          result := grEOF;
          exit;
        end;
      end
      else
        ReadRecordCache(FCurrentRecord, Buffer, False);
    end;
  end;
  result := grOK;
end;

procedure TIBCustomDataSet.ApplyUpdates;
var
  CurBookmark: TBytes;
  Buffer: PRecordData;
  CurUpdateTypes: TIBUpdateRecordTypes;
  UpdateAction: TIBUpdateAction;
  UpdateKind: TUpdateKind;
  bRecordsSkipped: Boolean;

  procedure GetUpdateKind;
  begin
    case Buffer^.rdCachedUpdateStatus of
      cusModified:
        UpdateKind := ukModify;
      cusInserted:
        UpdateKind := ukInsert;
      else
        UpdateKind := ukDelete;
    end;
  end;

  procedure ResetBufferUpdateStatus;
  begin
    case Buffer^.rdCachedUpdateStatus of
      cusModified:
      begin
        PRecordData(Buffer)^.rdUpdateStatus := usUnmodified;
        PRecordData(Buffer)^.rdCachedUpdateStatus := cusUnmodified;
      end;
      cusInserted:
      begin
        PRecordData(Buffer)^.rdUpdateStatus := usUnmodified;
        PRecordData(Buffer)^.rdCachedUpdateStatus := cusUnmodified;
      end;
      cusDeleted:
      begin
        PRecordData(Buffer)^.rdUpdateStatus := usDeleted;
        PRecordData(Buffer)^.rdCachedUpdateStatus := cusUnmodified;
      end;
    end;
    WriteRecordCache(PRecordData(Buffer)^.rdRecordNumber, TRecBuf(Buffer));
  end;

  procedure UpdateUsingOnUpdateRecord;
  begin
    UpdateAction := uaFail;
    try
      FOnUpdateRecord(Self, UpdateKind, UpdateAction);
    except
      on E: Exception do
      begin
        if (E is EDatabaseError) and Assigned(FOnUpdateError) then
          FOnUpdateError(Self, EIBError(E), UpdateKind, UpdateAction);
        if UpdateAction = uaFail then
          raise;
      end;
    end;
  end;

  procedure UpdateUsingUpdateObject;
  begin
    UpdateAction := uaApply;
    try
      FUpdateObject.Apply(UpdateKind);
      ResetBufferUpdateStatus;
    except
      on E: Exception do
      begin
        UpdateAction := uaFail;
        if (E is EDatabaseError) and Assigned(FOnUpdateError) then
          FOnUpdateError(Self, EIBError(E), UpdateKind, UpdateAction);
        if UpdateAction = uaFail then
          raise;
      end;
    end;
  end;

  procedure UpdateUsingInternalquery;
  begin
    try
      case Buffer^.rdCachedUpdateStatus of
        cusModified:
          InternalPostRecord(FQModify, TRecBuf(Buffer));
        cusInserted:
          InternalPostRecord(FQInsert, TRecBuf(Buffer));
        cusDeleted:
          InternalDeleteRecord(FQDelete, TRecBuf(Buffer));
      end;
    except
      on E: EIBError do begin
        UpdateAction := uaFail;
        if Assigned(FOnUpdateError) then
          FOnUpdateError(Self, E, UpdateKind, UpdateAction);
        case UpdateAction of
          uaFail: raise;
          uaAbort: System.SysUtils.Abort;
          uaSkip: bRecordsSkipped := True;
        end;
      end;
    end;
  end;

begin
  if not FCachedUpdates then
    IBError(ibxeNotCachedUpdates, [nil]);
  if State in [dsEdit, dsInsert] then
    Post;
  FBase.CheckDatabase;
  FBase.CheckTransaction;
  DisableControls;
  CurBookmark := Bookmark;
  CurUpdateTypes := FUpdateRecordTypes;
  FUpdateRecordTypes := [cusModified, cusInserted, cusDeleted];
  try
    First;
    bRecordsSkipped := False;
    while not EOF do
    begin
      Buffer := PRecordData(GetActiveBuf);
      GetUpdateKind;
      UpdateAction := uaApply;
      if Assigned(FUpdateObject) or Assigned(FOnUpdateRecord) then
      begin
        if (Assigned(FOnUpdateRecord)) then
          UpdateUsingOnUpdateRecord
        else
          if Assigned(FUpdateObject) then
            UpdateUsingUpdateObject;
        case UpdateAction of
          uaFail:
            IBError(ibxeUserAbort, [nil]);
          uaAbort:
            System.SysUtils.Abort;
          uaApplied:
            ResetBufferUpdateStatus;
          uaSkip:
            bRecordsSkipped := True;
          uaRetry:
            Continue;
        end;
      end;
      if (not Assigned(FUpdateObject)) and (UpdateAction = UaApply) then
      begin
        UpdateUsingInternalquery;
        UpdateAction := uaApplied;
      end;
      Next;
    end;
    FUpdatesPending := bRecordsSkipped;
  finally
    FUpdateRecordTypes := CurUpdateTypes;
    {$ifdef compilerversion < 12}
    if BookmarkValid(Pointer(CurBookmark)) then
      Bookmark := CurBookmark
    else
      First;
    {$else}
    { TODO -odiego : review code }
//    if BookmarkValid(CurBookmark)) then
//      Bookmark := CurBookmark
//    else
//      First;
    {$endif}
    EnableControls;
  end;
end;

procedure TIBCustomDataSet.InternalBatchInput(InputObject: TIBBatchInput);
begin
  FQSelect.BatchInput(InputObject);
end;

procedure TIBCustomDataSet.InternalBatchOutput(OutputObject: TIBBatchOutput);
var
  Qry: TIBSQL;
begin
  Qry := TIBSQL.Create(Self);
  try
    Qry.Database := FBase.Database;
    Qry.Transaction := FBase.Transaction;
    Qry.SQL.Assign(FQSelect.SQL);
    Qry.BatchOutput(OutputObject);
  finally
    Qry.Free;
  end;
end;

procedure TIBCustomDataSet.CancelUpdates;
var
  CurUpdateTypes: TIBUpdateRecordTypes;
begin
  if State in [dsEdit, dsInsert] then
    Cancel;
  if FCachedUpdates and FUpdatesPending then
  begin
    DisableControls;
    CurUpdateTypes := UpdateRecordTypes;
    UpdateRecordTypes := [cusModified, cusInserted, cusDeleted];
    try
      First;
      while not EOF do
      begin
        if UpdateStatus = usInserted then
          RevertRecord
        else
        begin
          RevertRecord;
          Next;
        end;
      end;
    finally
      UpdateRecordTypes := CurUpdateTypes;
      First;
      FUpdatesPending := False;
      EnableControls;
    end;
  end;
end;

procedure TIBCustomDataSet.ActivateConnection;
begin
  if not Assigned(Database) then
    IBError(ibxeDatabaseNotAssigned, [nil]);
  if not Assigned(Transaction) then
    IBError(ibxeTransactionNotAssigned, [nil]);
  if not Database.Connected then Database.Open;
end;

function TIBCustomDataSet.ActivateTransaction: Boolean;
begin
  Result := False;
  if not Assigned(Transaction) then
    IBError(ibxeTransactionNotAssigned, [nil]);
  if not Transaction.Active then
  begin
    Result := True;
    Transaction.AutoStartTransaction;
  end;
end;

procedure TIBCustomDataSet.DeactivateTransaction;
begin
  if not Assigned(Transaction) then
    IBError(ibxeTransactionNotAssigned, [nil]);
  Transaction.CheckAutoStop;
end;

procedure TIBCustomDataSet.CheckDatasetClosed;
begin
  if FOpen then
    IBError(ibxeDatasetOpen, [nil]);
end;

procedure TIBCustomDataSet.CheckDatasetOpen;
begin
  if not FOpen then
    IBError(ibxeDatasetClosed, [nil]);
end;

procedure TIBCustomDataSet.CheckNotUniDirectional;
begin
  if UniDirectional then
    IBError(ibxeDataSetUniDirectional, [nil]);
end;

procedure TIBCustomDataSet.AdjustRecordOnInsert(Buffer: TRecBuf);
begin
  if (State = dsInsert) and (not Modified) then
  begin
    PRecordData(Buffer)^.rdRecordNumber := FRecordCount;
    FCurrentRecord := FRecordCount;
  end;
end;

function TIBCustomDataSet.CanEdit: Boolean;
var
  Buff: PRecordData;
begin
  Buff := PRecordData(GetActiveBuf);
  result := ((FQModify.SQL.Text <> '') and (lmModify in FLiveMode)) or
    (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukModify).Text <> '')) or
    ((Buff <> nil) and (Buff^.rdCachedUpdateStatus = cusInserted) and
      (FCachedUpdates));
end;

function TIBCustomDataSet.CanInsert: Boolean;
begin
  result := ((FQInsert.SQL.Text <> '') and (lmInsert in FLiveMode)) or
    (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukInsert).Text <> ''));
end;

function TIBCustomDataSet.CanDelete: Boolean;
begin
  if ((FQDelete.SQL.Text <> '') and (lmDelete in FLiveMode)) or
    (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukDelete).Text <> '')) then
    result := True
  else
    result := False;
end;

function TIBCustomDataSet.CanRefresh: Boolean;
begin
  result := ((FQRefresh.SQL.Text <> '') and (lmRefresh in FLiveMode)) or
    (Assigned(FUpdateObject) and (FUpdateObject.RefreshSQL.Text <> ''));
end;

procedure TIBCustomDataSet.CheckEditState;
begin
  case State of
    { Check all the wsEditMode types }
    dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter,
    dsNewValue, dsInternalCalc :
    begin
      if (State in [dsEdit]) and (not CanEdit) then
        IBError(ibxeCannotUpdate, [nil]);
      if (State in [dsInsert]) and (not CanInsert) then
        IBError(ibxeCannotInsert, [nil]);
    end;
  else
    IBError(ibxeNotEditing, [])
  end;
end;

procedure TIBCustomDataSet.ClearBlobCache;
var
  i: Integer;
begin
  for i := FBlobStreamList.Count - 1 downto 0 do
    FBlobStreamList.Delete(i);
  FBlobStreamList.Pack;
end;

procedure TIBCustomDataSet.CopyRecordBuffer(Source, Dest: TRecBuf);
begin
  Move(PByte(Source)[0], PByte(Dest)[0], FRecordBufferSize);
end;

procedure TIBCustomDataSet.DoBeforeDatabaseDisconnect(Sender: TObject);
begin
  if Active then
    Active := False;
  FInternalPrepared := False;
  if Assigned(FBeforeDatabaseDisconnect) then
    FBeforeDatabaseDisconnect(Sender);
end;

procedure TIBCustomDataSet.DoAfterDatabaseDisconnect(Sender: TObject);
begin
  if Assigned(FAfterDatabaseDisconnect) then
    FAfterDatabaseDisconnect(Sender);
end;

procedure TIBCustomDataSet.DoDatabaseFree(Sender: TObject);
begin
  if Assigned(FDatabaseFree) then
    FDatabaseFree(Sender);
end;

procedure TIBCustomDataSet.DoBeforeTransactionEnd(Sender: TObject);
begin
  if Active then
    Active := False;
  if FQSelect <> nil then
    FQSelect.FreeHandle;
  if FQDelete <> nil then
    FQDelete.FreeHandle;
  if FQInsert <> nil then
    FQInsert.FreeHandle;
  if FQModify <> nil then
    FQModify.FreeHandle;
  if FQRefresh <> nil then
    FQRefresh.FreeHandle;
  FInternalPrepared := false;
  if Assigned(FBeforeTransactionEnd) then
    FBeforeTransactionEnd(Sender);
end;

procedure TIBCustomDataSet.DoAfterTransactionEnd(Sender: TObject);
begin
  if Assigned(FAfterTransactionEnd) then
    FAfterTransactionEnd(Sender);
end;

procedure TIBCustomDataSet.DoTransactionFree(Sender: TObject);
begin
  if Assigned(FTransactionFree) then
    FTransactionFree(Sender);
end;

{ Read the record from FQSelect.Current into the record buffer
  Then write the buffer to in memory cache }
{$IFDEF NEXTGEN}
procedure TIBCustomDataSet.FetchCurrentRecordToBuffer(Qry: TIBSQL;
  RecordNumber: Integer; Buffer: TRecBuf);
{$ELSE}
procedure TIBCustomDataSet.FetchCurrentRecordToBuffer(Qry: TIBSQL;
  RecordNumber: Integer; Buffer: TRecBuf);
{$ENDIF NEXTGEN}
var
  p: PRecordData;
  pbd: PBlobDataArray;
  i, j: Integer;
  LocalData: Pointer;
  LocalDate, LocalDouble: Double;
  LocalInt: Integer;
  LocalInt64: Int64;
  LocalCurrency: Currency;
  LocalBCD : TBcd;
  FieldsLoaded: Integer;
  S: String;
  aField : TField;
begin
  p := PRecordData(Buffer);
  { Make sure blob cache is empty }
  pbd := PBlobDataArray(PByte(Buffer) + FBlobCacheOffset);
  if RecordNumber > -1 then
    for i := 0 to BlobFieldCount - 1 do
      pbd^[i] := nil;
  { Get record information }
  p^.rdBookmarkFlag := bfCurrent;
  p^.rdFieldCount := Qry.Current.Count;
  p^.rdRecordNumber := RecordNumber;
  p^.rdUpdateStatus := usUnmodified;
  p^.rdCachedUpdateStatus := cusUnmodified;
  p^.rdSavedOffset := $FFFFFFFF;

  { Load up the fields }
  FieldsLoaded := FQSelect.Current.Count;
  for i := 0 to Qry.Current.Count - 1 do
  begin
    if (Qry = FQSelect) then
      j := i + 1
    else
    begin
      if FieldsLoaded = 0 then
        break;
      j := FQSelect.FieldIndex[Qry.Current[i].Name] + 1;
      if j < 1 then
        continue
      else
        Dec(FieldsLoaded);
    end;
    if FQSelect.Current[j - 1].Data.aliasname = 'IBX_INTERNAL_DBKEY' then {do not localize}
    begin
      if FQSelect.Current[j - 1].Data.sqllen <= 8 then
        p^.rdDBKey := PIBDBKEY(Qry.Current[i].AsPointer)^;
      continue;
    end;
    if j > 0 then
    begin
      p^.rdFields[j].fdDataType := Qry.Current[i].Data.SqlDef;
      p^.rdFields[j].fdDataScale := Qry.Current[i].Data.sqlscale;
      p^.rdFields[j].fdNullable := (Qry.Current[i].Data.sqltype and 1 = 1);
      p^.rdFields[j].fdIsNull :=
           (p^.rdFields[j].fdNullable and (Qry.Current[i].Data.sqlind^ = -1));
      p^.rdFields[j].fdCharSetSize := Qry.Current[i].CharSetSize;
      // Only the Model needs this set, otherwise set in AllocatBuffer
      //   when the model is copied to the record.
      if RecordNumber < 0 then
        p^.rdFields[j].fdPersistedFloatField := false;
      LocalData := Qry.Current[i].Data.sqldata;
      case p^.rdFields[j].fdDataType of
        SQL_TIMESTAMP:
        begin
          p^.rdFields[j].fdDataSize := SizeOf(TDateTime);
          if RecordNumber >= 0 then
            LocalDate := TimeStampToMSecs(DateTimeToTimeStamp(Qry.Current[i].AsDateTime));
          LocalData := Pointer(@LocalDate);
        end;
        SQL_TYPE_DATE:
        begin
          p^.rdFields[j].fdDataSize := SizeOf(TDateTime);
          if RecordNumber >= 0 then
            LocalInt := DateTimeToTimeStamp(Qry.Current[i].AsDateTime).Date;
          LocalData := Pointer(@LocalInt);
        end;
        SQL_TYPE_TIME:
        begin
          p^.rdFields[j].fdDataSize := SizeOf(TDateTime);
          if RecordNumber >= 0 then
            LocalInt := DateTimeToTimeStamp(Qry.Current[i].AsDateTime).Time;
          LocalData := Pointer(@LocalInt);
        end;
        SQL_SHORT, SQL_LONG:
        begin
          if (p^.rdFields[j].fdDataScale = 0) then
          begin
            p^.rdFields[j].fdDataSize := SizeOf(Integer);
            if RecordNumber >= 0 then
              LocalInt := Qry.Current[i].AsLong;
            LocalData := Pointer(@LocalInt);
          end
          else
            if (p^.rdFields[j].fdDataScale >= (-4)) then
            begin
              p^.rdFields[j].fdDataSize := SizeOf(Currency);
              if RecordNumber >= 0 then
                LocalCurrency := Qry.Current[i].AsCurrency;
              LocalData := Pointer(@LocalCurrency);
            end
            else
            begin
              // This is needed for very old persistant field.  IBX at one time created
              //    float fields for this type before BCDFields existed.  So
              //    if there is an old persisted float field the assignment is different.
              //    Only needs to be done on the Model buffer which has a RecordNumber of -1
              //    for performance reasons.
              if RecordNumber < 0 then
              begin
                aField := Fields.FindField(String(FQSelect.Current[j - 1].Data.SqlName));
                if Assigned(aField) and
                   (aField.DataType = ftFloat) then
                  p^.rdFields[j].fdPersistedFloatField := true;
              end;
              if p^.rdFields[j].fdPersistedFloatField then
              begin
                p^.rdFields[j].fdDataSize := SizeOf(Double);
                if RecordNumber >= 0 then
                  LocalDouble := Qry.Current[i].AsDouble;
                LocalData := Pointer(@LocalDouble);
              end
              else
              begin
                p^.rdFields[j].fdDataSize := SizeOf(tBcd);
                if RecordNumber >= 0 then
                  LocalBCD := Qry.Current[i].AsBcd;
                LocalData := Pointer(@LocalBCD);
              end
            end;
        end;
        SQL_INT64:
        begin
          if (p^.rdFields[j].fdDataScale = 0) then
          begin
            p^.rdFields[j].fdDataSize := SizeOf(Int64);
            if RecordNumber >= 0 then
              LocalInt64 := Qry.Current[i].AsInt64;
            LocalData := Pointer(@LocalInt64);
          end
          else
            if (p^.rdFields[j].fdDataScale >= (-4)) then
            begin
              p^.rdFields[j].fdDataSize := SizeOf(Currency);
              if RecordNumber >= 0 then
                LocalCurrency := Qry.Current[i].AsCurrency;
              LocalData := Pointer(@LocalCurrency);
            end
            else
            begin
              // This is needed for very old persistant field.  IBX at one time created
              //    float fields for this type before BCDFields existed.  So
              //    if there is an old persisted float field the assignment is different.
              //    Only needs to be done on the Model buffer which has a RecordNumber of -1
              //    for performance reasons.
              if RecordNumber < 0 then
              begin
                aField := Fields.FindField(String(FQSelect.Current[j - 1].Data.SqlName));
                if Assigned(aField) and
                   (aField.DataType = ftFloat) then
                  p^.rdFields[j].fdPersistedFloatField := true;
              end;
              if p^.rdFields[j].fdPersistedFloatField then
              begin
                p^.rdFields[j].fdDataSize := SizeOf(Double);
                if RecordNumber >= 0 then
                  LocalDouble := Qry.Current[i].AsDouble;
                LocalData := Pointer(@LocalDouble);
              end
              else
              begin
                p^.rdFields[j].fdDataSize := SizeOf(tBcd);
                if RecordNumber >= 0 then
                  LocalBCD := Qry.Current[i].AsBcd;
                LocalData := Pointer(@LocalBCD);
              end
            end
        end;
        SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        begin
          p^.rdFields[j].fdDataSize := SizeOf(Double);
          if RecordNumber >= 0 then
            LocalDouble := Qry.Current[i].AsDouble;
          LocalData := Pointer(@LocalDouble);
        end;
        SQL_VARYING, SQL_TEXT:
        begin
        // computed Char fields return un encoded (IB Bug)
          if Qry.Current[i].Data.RelName = '' then
            p^.rdFields[j].fdDataSize := Qry.Current[i].Data.sqllen * SizeOf(Char)
          else
            p^.rdFields[j].fdDataSize := Qry.Current[i].Data.sqllen div p^.rdFields[j].fdCharSetSize * 2;
          p^.rdFields[j].fdDataLength := Database.GDSLibrary.isc_vax_integer(Qry.Current[i].Data.sqldata, 2);
          if RecordNumber >= 0 then
          begin
            if (p^.rdFields[j].fdDataLength = 0) then
              LocalData := nil
            else
            begin
              S := Qry.Current[i].AsString;
              // Adjust for the right size of the Unicode string.  Because of Database
              //   connection string overrides, the underlying characterset size
              //   can not be used.
              if (p^.rdFields[j].fdDataType = SQL_TEXT) then
                // computed Char fields return un encoded (IB Bug)
                if Qry.Current[i].Data.RelName = '' then
                begin
                  p^.rdFields[j].fdDataSize := Qry.Current[i].Data.sqllen * SizeOf(Char);
                  p^.rdFields[j].fdDataLength := Qry.Current[i].Data.sqllen * SizeOf(Char);
                end
                else
                  p^.rdFields[j].fdDataLength := p^.rdFields[j].fdDataSize
              else
                p^.rdFields[j].fdDataLength := min(Length(S) * SizeOf(Char), p^.rdFields[j].fdDataSize);
              LocalData := @s[low(s)];
            end;
          end;
        end;
        else { SQL_BLOB, SQL_ARRAY, SQL_QUAD }
        begin
          p^.rdFields[j].fdDataSize := Qry.Current[i].Data.sqllen;
        end;
      end;
      if RecordNumber < 0 then
      begin
        p^.rdFields[j].fdIsNull := True;
        p^.rdFields[j].fdDataOfs := FRecordSize;
        Inc(FRecordSize, p^.rdFields[j].fdDataSize);
      end
      else
      begin
        if (p^.rdFields[j].fdDataType = SQL_VARYING) or
           (p^.rdFields[j].fdDataType = SQL_TEXT) then
        begin
          if LocalData <> nil then
            Move(LocalData^, PByte(Buffer)[p^.rdFields[j].fdDataOfs], p^.rdFields[j].fdDataLength)
        end
        else
          Move(LocalData^, PByte(Buffer)[p^.rdFields[j].fdDataOfs], p^.rdFields[j].fdDataSize)
      end;
    end;
  end;
  WriteRecordCache(RecordNumber, TRecBuf(p));
end;

function TIBCustomDataSet.GetActiveBuf: TRecBuf;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        result := 0
      else
        result := ActiveBuffer;
    dsEdit, dsInsert:
      result := ActiveBuffer;
    dsCalcFields:
      result := CalcBuffer;
    dsFilter:
      result := FFilterBuffer;
    dsNewValue:
      result := ActiveBuffer;
    dsOldValue:
      if (PRecordData(ActiveBuffer)^.rdSavedOffset <> $FFFFFFFF) then
      begin
        ReadCache(FOldBufferCache, PRecordData(ActiveBuffer)^.rdSavedOffset, BUFFER_BEGIN,
                     FTempBuffer);
        result := FTempBuffer;
      end
      else
        if (PRecordData(ActiveBuffer)^.rdRecordNumber =
          PRecordData(FOldBuffer)^.rdRecordNumber) then
          result := FOldBuffer
        else
          result := ActiveBuffer;
  else if not FOpen then
    result := TRecBuf(nil)
  else
    result := ActiveBuffer;
  end;
end;

function TIBCustomDataSet.CachedUpdateStatus: TCachedUpdateStatus;
begin
  if Active then
    result := PRecordData(GetActiveBuf)^.rdCachedUpdateStatus
  else
    result := cusUnmodified;
end;

function TIBCustomDataSet.GetDatabase: TIBDatabase;
begin
  result := FBase.Database;
end;

function TIBCustomDataSet.GetDBHandle: PISC_DB_HANDLE;
begin
  result := FBase.DBHandle;
end;

function TIBCustomDataSet.GetDeleteSQL: TStrings;
begin
  result := FQDelete.SQL;
end;

function TIBCustomDataSet.GetInsertSQL: TStrings;
begin
  result := FQInsert.SQL;
end;

function TIBCustomDataSet.GetSQLParams: TIBXSQLDA;
begin
  if not FInternalPrepared then
    InternalPrepare;
  result := FQSelect.Params;
end;

function TIBCustomDataSet.GetRefreshSQL: TStrings;
begin
  result := FQRefresh.SQL;
end;

function TIBCustomDataSet.GetSelectSQL: TStrings;
begin
  result := FQSelect.SQL;
end;

function TIBCustomDataSet.GetStatementType: TIBSQLTypes;
begin
  result := FQSelect.SQLType;
end;

function TIBCustomDataSet.GetModifySQL: TStrings;
begin
  result := FQModify.SQL;
end;

function TIBCustomDataSet.GetTransaction: TIBTransaction;
begin
  result := FBase.Transaction;
end;

function TIBCustomDataSet.GetTRHandle: PISC_TR_HANDLE;
begin
  result := FBase.TRHandle;
end;

procedure TIBCustomDataSet.InternalDeleteRecord(Qry: TIBSQL; Buff: TRecBuf);
begin
  if (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukDelete).Text <> '')) then
    FUpdateObject.Apply(ukDelete)
  else
  begin
    SetInternalSQLParams(FQDelete, Buff);
    FQDelete.ExecQuery;
    FRowsAffected := FQDelete.RowsAffected;
  end;
  PRecordData(Buff)^.rdUpdateStatus := usDeleted;
  PRecordData(Buff)^.rdCachedUpdateStatus := cusUnmodified;
  WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
end;

function TIBCustomDataSet.InternalLocate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  fl: TList<TField>;
  CurBookmark: TBytes;
  fld : Variant;
  val : Array of Variant;
  i, fld_cnt: Integer;
  fld_str : String;
begin
  fl := TList<TField>.Create;
  try
    GetFieldList(fl, KeyFields);
    fld_cnt := fl.Count;
    CurBookmark := Bookmark;
    result := False;
    SetLength(val, fld_cnt);
    if not Eof then
      for i := 0 to fld_cnt - 1 do
      begin
        if VarIsArray(KeyValues) then
          val[i] := KeyValues[i]
        else
          val[i] := KeyValues;
        if (TField(fl[i]).DataType = ftWideString) and
           not VarIsNull(val[i]) then
        begin
          if (loCaseInsensitive in Options) then
            val[i] := AnsiUpperCase(val[i]);
        end;
      end;
    while ((not result) and (not Eof)) do
    begin
      i := 0;
      result := True;
      while (result and (i < fld_cnt)) do
      begin
        fld := TField(fl[i]).Value;
        if VarIsNull(fld) then
          result := result and VarIsNull(val[i])
        else
        begin
          // We know the Field is not null so if the passed value is null we are
                                    
          result := result and not VarIsNull(val[i]);
          if result then
          begin
            try
              fld := VarAsType(fld, VarType(val[i]));
            except
              on E: EVariantError do result := False;
            end;
            if TField(fl[i]).DataType = ftWideString then
            begin
              fld_str := TField(fl[i]).AsString;
              if (loCaseInsensitive in Options) then
                fld_str := AnsiUpperCase(fld_str);
              if (loPartialKey in Options) then
                result := result and fld_str.StartsWith(val[i])
              else
                result := result and (fld_str = val[i]);
            end
            else
              if TField(fl[i]).DataType in [ftDate, ftTime, ftDateTime] then
                Result := Result and (DateTimeToStr(val[i]) = DateTimeToStr(fld))
              else
              result := result and (val[i] = fld);
          end;
        end;
        Inc(i);
      end;
      if not result then
        Next;
    end;
    if not result then
      Bookmark := CurBookmark
    else
      CursorPosChanged;
  finally
    fl.Free;
    val := nil;
  end;
end;

procedure TIBCustomDataSet.InternalPostRecord(Qry: TIBSQL; Buff: TRecBuf);
var
  i, j, k: Integer;
  pbd: PBlobDataArray;
begin
  pbd := PBlobDataArray(PByte(Buff) + FBlobCacheOffset);
  j := 0;
  for i := 0 to FieldCount - 1 do
    if Fields[i].IsBlob then
    begin
      k := FMappedFieldPosition[Fields[i].FieldNo -1];
      if pbd^[j] <> nil then
      begin
        pbd^[j].Finalize;
        PISC_QUAD(
          PByte(Buff) + PRecordData(Buff)^.rdFields[k].fdDataOfs)^ :=
          pbd^[j].BlobID;
        PRecordData(Buff)^.rdFields[k].fdIsNull := pbd^[j].Size = 0;
      end;
      Inc(j);
    end;
  if Assigned(FUpdateObject) then
  begin
    if (Qry = FQDelete) then
      FUpdateObject.Apply(ukDelete)
    else
      if (Qry = FQInsert) then
        FUpdateObject.Apply(ukInsert)
      else
        FUpdateObject.Apply(ukModify);
  end
  else
  begin
    SetInternalSQLParams(Qry, Buff);
    Qry.ExecQuery;
    FRowsAffected := Qry.RowsAffected; 
  end;
  PRecordData(Buff)^.rdUpdateStatus := usUnmodified;
  PRecordData(Buff)^.rdCachedUpdateStatus := cusUnmodified;
  SetModified(False);
  WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
  if (FForcedRefresh or FNeedsRefresh) and CanRefresh then
    InternalRefreshRow;
end;

procedure TIBCustomDataSet.InternalRefreshRow;
var
  Buff: TRecBuf;
  ofs: DWORD;
  Qry: TIBSQL;
begin
  Qry := nil;
  Buff := GetActiveBuf;
  if CanRefresh then
  begin
    if Buff <> TRecBuf(nil) then
    begin
      if (Assigned(FUpdateObject) and (FUpdateObject.RefreshSQL.Text <> '')) then
      begin
        Qry := TIBSQL.Create(self);
        Qry.Database := Database;
        Qry.Transaction := Transaction;
        Qry.GoToFirstRecordOnExecute := False;
        Qry.SQL.Text := FUpdateObject.RefreshSQL.Text;
      end
      else
        Qry := FQRefresh;
      SetInternalSQLParams(Qry, Buff);
      Qry.ExecQuery;
      try
        if (Qry.SQLType = SQLExecProcedure) or
           (Qry.Next <> nil) then
        begin
          ofs := PRecordData(Buff)^.rdSavedOffset;
          FetchCurrentRecordToBuffer(Qry,
                                     PRecordData(Buff)^.rdRecordNumber,
                                     Buff);
          if FCachedUpdates and (ofs <> $FFFFFFFF) then
          begin
            PRecordData(Buff)^.rdSavedOffset := ofs;
            WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
            SaveOldBuffer(Buff);
          end;
        end;
      finally
        Qry.Close;
      end;
    end;
    if Qry <> FQRefresh then
      Qry.Free;
  end
  else
    IBError(ibxeCannotRefresh, [nil]);
end;

procedure TIBCustomDataSet.InternalRevertRecord(RecordNumber: Integer);
var
  NewBuffer, OldBuffer: PRecordData;

begin
  NewBuffer := nil;
  OldBuffer := nil;
{$IFDEF NEXTGEN}
  NewBuffer := PRecordData(AllocRecBuf);
  OldBuffer := PRecordData(AllocRecBuf);
{$ELSE}
  NewBuffer := PRecordData(AllocRecordBuffer);
  OldBuffer := PRecordData(AllocRecordBuffer);
{$ENDIF NEXTGEN}
  try
    ReadRecordCache(RecordNumber, TRecBuf(NewBuffer), False);
    ReadRecordCache(RecordNumber, TRecBuf(OldBuffer), True);
    case NewBuffer^.rdCachedUpdateStatus of
      cusInserted:
      begin
        NewBuffer^.rdCachedUpdateStatus := cusUninserted;
        Inc(FDeletedRecords);
      end;
      cusModified,
      cusDeleted:
      begin
        if (NewBuffer^.rdCachedUpdateStatus = cusDeleted) then
          Dec(FDeletedRecords);
        CopyRecordBuffer(TRecBuf(OldBuffer), TRecBuf(NewBuffer));
      end;
    end;

    if State in dsEditModes then
      Cancel;

    WriteRecordCache(RecordNumber, TRecBuf(NewBuffer));

    if (NewBuffer^.rdCachedUpdateStatus = cusUninserted ) then
      ReSync([]);
  finally
{$IFDEF NEXTGEN}
    FreeRecBuf(TRecBuf(NewBuffer));
    FreeRecBuf(TRecBuf(OldBuffer));
{$ELSE}
    FreeRecordBuffer(TRecordBuffer(NewBuffer));
    FreeRecordBuffer(TRecordBuffer(OldBuffer));
{$ENDIF NEXTGEN}
  end;
end;

{ A visible record is one that is not truly deleted,
  and it is also listed in the FUpdateRecordTypes set }

function TIBCustomDataSet.IsVisible(Buffer: TRecBuf): Boolean;
begin
  result := True;
  if not (State = dsOldValue) then
    result :=
      (PRecordData(Buffer)^.rdCachedUpdateStatus in FUpdateRecordTypes) and
      (not ((PRecordData(Buffer)^.rdCachedUpdateStatus = cusUnmodified) and
        (PRecordData(Buffer)^.rdUpdateStatus = usDeleted)));
end;


function TIBCustomDataSet.LocateNext(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  b : TBookmark;
begin
  DisableControls;
  b := GetBookmark;
  try
    Next;
    Result := InternalLocate(KeyFields, KeyValues, Options);
    if not Result then
      GotoBookmark(b);  // Get back on the record we started with on failure
  finally
    FreeBookmark(b);
    EnableControls;
  end;
end;

procedure TIBCustomDataSet.InternalPrepare;
var
  DidActivate: Boolean;

  procedure PrepareSQL(iSQL : TIBSQL; SQLText : TStrings; lm : TLiveMode);
  begin
    try
      if Trim(SQLText.Text) <> '' then
      begin
        if not iSQL.Prepared then
          iSQL.Prepare;
        Include(FLiveMode, lm);
      end;
    except
     on E: Exception do
       if not (E is EIBInterbaseRoleError) then
         Raise;
    end;
  end;

begin
  if FInternalPrepared then
    Exit;
  if Trim(FQSelect.SQL.Text) = '' then
    IBError(ibxeEmptySQLStatement, []);
  DidActivate := False;
  try
    ActivateConnection;
    DidActivate := ActivateTransaction;
    FBase.CheckDatabase;
    FBase.CheckTransaction;
    if Trim(FQSelect.SQL.Text) <> '' then
    begin
      if not FQSelect.Prepared then
      begin
        FQSelect.ParamCheck := ParamCheck;
        FQSelect.Prepare;
      end;
      FLiveMode := [];
      PrepareSQL(FQDelete, FQDelete.SQL, lmDelete);
      PrepareSQL(FQInsert, FQInsert.SQL, lmInsert);
      PrepareSQL(FQModify, FQModify.SQL, lmModify);
      PrepareSQL(FQRefresh, FQRefresh.SQL, lmRefresh);

      FInternalPrepared := True;
      InternalInitFieldDefs;
    end
    else
      IBError(ibxeEmptyQuery, [nil]);
  finally
    if DidActivate then
      DeactivateTransaction;
  end;
end;

procedure TIBCustomDataSet.RecordModified(Value: Boolean);
begin
  SetModified(Value);
end;

procedure TIBCustomDataSet.RevertRecord;
var
  Buff: PRecordData;
begin
  if FCachedUpdates and FUpdatesPending then
  begin
    Buff := PRecordData(GetActiveBuf);
    InternalRevertRecord(Buff^.rdRecordNumber);
    ReadRecordCache(Buff^.rdRecordNumber, TRecBuf(Buff), False);
    DataEvent(deRecordChange, 0);
  end;
end;

procedure TIBCustomDataSet.SaveOldBuffer(Buffer: TRecBuf);
var
  OldBuffer: TRecBuf;

  procedure CopyOldBuffer;
  begin
    CopyRecordBuffer(Buffer, OldBuffer);
    if BlobFieldCount > 0 then
      FillChar(PByte(OldBuffer)[FBlobCacheOffset], BlobFieldCount * SizeOf(TIBBlobStream),
               0);
  end;

begin
  if (Buffer <> TRecBuf(nil)) and (PRecordData(Buffer)^.rdRecordNumber >= 0) then
  begin
{$IFDEF NEXTGEN}
    OldBuffer := AllocRecBuf;
{$ELSE}
    OldBuffer := TRecBuf(AllocRecordBuffer);
{$ENDIF NEXTGEN}
    try
      if (PRecordData(Buffer)^.rdSavedOffset = $FFFFFFFF) then
      begin
        PRecordData(Buffer)^.rdSavedOffset := AdjustPosition(FOldBufferCache, 0,
                                                             BUFFER_END);
        CopyOldBuffer;
        WriteCache(FOldBufferCache, 0, BUFFER_CURRENT, OldBuffer);
        WriteCache(FBufferCache, PRecordData(Buffer)^.rdRecordNumber * FRecordBufferSize,
                     BUFFER_BEGIN, Buffer);
      end
      else begin
        CopyOldBuffer;
        WriteCache(FOldBufferCache, PRecordData(Buffer)^.rdSavedOffset, BUFFER_BEGIN,
                   OldBuffer);
      end;
    finally
{$IFDEF NEXTGEN}
      FreeRecBuf(OldBuffer);
{$ELSE}
      FreeRecordBuffer(TRecordBuffer(OldBuffer));
{$ENDIF NEXTGEN}
    end;
  end;
end;

procedure TIBCustomDataSet.SetBufferChunks(Value: Integer);
begin
  if (Value <= 0) then
    FBufferChunks := BufferCacheSize
  else
    FBufferChunks := Value;
end;

procedure TIBCustomDataSet.SetDatabase(Value: TIBDatabase);
begin
  if (FBase.Database <> Value) then
  begin
    CheckDatasetClosed;
    FBase.Database := Value;
    FQDelete.Database := Value;
    FQInsert.Database := Value;
    FQRefresh.Database := Value;
    FQSelect.Database := Value;
    FQModify.Database := Value;
  end;
end;

procedure TIBCustomDataSet.SetDeleteSQL(Value: TStrings);
begin
  if FQDelete.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQDelete.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetInsertSQL(Value: TStrings);
begin
  if FQInsert.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQInsert.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetInternalSQLParams(Qry: TIBSQL; Buffer: TRecBuf);
var
  i, j: Integer;
  cr, data: PByte;
  fn, st: string;
  OldBuffer: TRecBuf;
  ts: TTimeStamp;

begin
  if (Buffer = 0) then
    IBError(ibxeBufferNotSet, [nil]);
  if (not FInternalPrepared) then
    InternalPrepare;
  OldBuffer := TRecBuf(nil);
  try
    for i := 0 to Qry.Params.Count - 1 do
    begin
      fn := Qry.Params[i].Name;
      if fn.StartsWith('OLD_') then {mbcs ok}
      begin
        fn := fn.Substring(4);
        if OldBuffer = 0 then
        begin
{$IFDEF NEXTGEN}
          OldBuffer := AllocRecBuf;
{$ELSE}
          OldBuffer := TRecBuf(AllocRecordBuffer);
{$ENDIF NEXTGEN}
          ReadRecordCache(PRecordData(Buffer)^.rdRecordNumber, OldBuffer, True);
        end;
        cr := PByte(OldBuffer);
      end
      else
        if fn.StartsWith('NEW_') then {mbcs ok}
        begin
          fn := fn.Substring(4);
          cr := PByte(Buffer);
        end
        else
          cr := PByte(Buffer);
      j := FQSelect.FieldIndex[fn] + 1;
      if (j > 0) then
      begin
        if Qry.Params[i].name = 'IBX_INTERNAL_DBKEY' then {do not localize}
        begin
          PIBDBKey(Qry.Params[i].AsPointer)^ := PRecordData(cr)^.rdDBKey;
          continue;
        end;
        if PRecordData(cr)^.rdFields[j].fdIsNull then
          Qry.Params[i].IsNull := True
        else
        begin
          Qry.Params[i].IsNull := False;
          data := cr + PRecordData(cr)^.rdFields[j].fdDataOfs;
          case PRecordData(cr)^.rdFields[j].fdDataType of
            SQL_TEXT, SQL_VARYING:
            begin
              SetString(st, PChar(data), PRecordData(cr)^.rdFields[j].fdDataLength div 2);
              Qry.Params[i].AsString := st;
            end;
          SQL_FLOAT, SQL_DOUBLE, SQL_D_FLOAT:
            Qry.Params[i].AsDouble := PDouble(data)^;
          SQL_SHORT, SQL_LONG:
          begin
            if PRecordData(cr)^.rdFields[j].fdDataScale = 0 then
              Qry.Params[i].AsLong := PLong(data)^
            else
              if PRecordData(cr)^.rdFields[j].fdDataScale >= (-4) then
                Qry.Params[i].AsCurrency := PCurrency(data)^
              else
                if PRecordData(cr)^.rdFields[j].fdPersistedFloatField then
                  Qry.Params[i].AsDouble := PDouble(data)^
                else
                  Qry.Params[i].AsBcd := PBcd(data)^;
          end;
          SQL_INT64:
          begin
            if PRecordData(cr)^.rdFields[j].fdDataScale = 0 then
              Qry.Params[i].AsInt64 := PInt64(data)^
            else
              if PRecordData(cr)^.rdFields[j].fdDataScale >= (-4) then
                Qry.Params[i].AsCurrency := PCurrency(data)^
              else
                if PRecordData(cr)^.rdFields[j].fdPersistedFloatField then
                  Qry.Params[i].AsDouble := PDouble(data)^
                else
                  Qry.Params[i].AsBcd := PBcd(data)^;
          end;
          SQL_BLOB, SQL_ARRAY, SQL_QUAD:
            Qry.Params[i].AsQuad := PISC_QUAD(data)^;
          SQL_TYPE_DATE:
          begin
            ts.Date := PInt(data)^;
            ts.Time := 0;
            Qry.Params[i].AsDate :=
              TimeStampToDateTime(ts);
          end;
          SQL_TYPE_TIME:
          begin
            ts.Date := 1;
            ts.Time := PInt(data)^;
            Qry.Params[i].AsTime :=
              TimeStampToDateTime(ts);
          end;
          SQL_TIMESTAMP:
            Qry.Params[i].AsDateTime :=
              TimeStampToDateTime(
                MSecsToTimeStamp(PDouble(data)^));
          SQL_BOOLEAN:
             Qry.Params[i].AsBoolean := (PShort(data)^ = ISC_TRUE);
        end;
      end;
    end;
    end;
  finally
    if (OldBuffer <> 0) then
{$IFDEF NEXTGEN}
      FreeRecBuf(TRecBuf(OldBuffer));
{$ELSE}
      FreeRecordBuffer(TRecordBuffer(OldBuffer));
{$ENDIF NEXTGEN}
  end;
end;

procedure TIBCustomDataSet.SetRefreshSQL(Value: TStrings);
begin
  if FQRefresh.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQRefresh.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetSelectSQL(Value: TStrings);
begin
  if FQSelect.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQSelect.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetModifySQL(Value: TStrings);
begin
  if FQModify.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQModify.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetTransaction(Value: TIBTransaction);
begin
  if (FBase.Transaction <> Value) then
  begin
    CheckDatasetClosed;
    if FInternalPrepared then
      InternalUnPrepare;
    FBase.Transaction := Value;
    FQDelete.Transaction := Value;
    FQInsert.Transaction := Value;
    FQRefresh.Transaction := Value;
    FQSelect.Transaction := Value;
    FQModify.Transaction := Value;
  end;
end;

procedure TIBCustomDataSet.SetUniDirectional(Value: Boolean);
begin
  CheckDatasetClosed;
  FUniDirectional := Value;
end;

procedure TIBCustomDataSet.SetUpdateRecordTypes(Value: TIBUpdateRecordTypes);
begin
  FUpdateRecordTypes := Value;
  if Active then
    First;
end;

procedure TIBCustomDataSet.RefreshParams;
var
  DataSet: TDataSet;

  function NeedsRefreshing : Boolean;
  var
    i : Integer;
    cur_param: TIBXSQLVAR;
    cur_field: TField;

  begin
    Result := true;
    i := 0;
    while (i < SQLParams.Count) and (Result) do
    begin
      cur_field := DataSource.DataSet.FindField(SQLParams[i].Name);
      cur_param := SQLParams[i];
      if (cur_field <> nil) then
      begin
        if (cur_field.IsNull) then
          Result := Result and cur_param.IsNull
        else
        case cur_field.DataType of
          ftString, ftWideString:
            Result := Result and (cur_param.AsString = cur_field.AsString);
          ftBoolean:
            Result := Result and (cur_param.AsBoolean = cur_field.AsBoolean);
          ftShortint, ftSmallInt, ftWord, ftByte:
            Result := Result and (cur_param.AsShort = cur_field.AsInteger);
          ftInteger:
            Result := Result and (cur_param.AsLong = cur_field.AsInteger);
          ftLargeInt:
            Result := Result and (cur_param.AsInt64 = cur_field.AsLargeInt);
          ftFloat, ftCurrency:
            Result := Result and (cur_param.AsDouble = cur_field.AsFloat);
          ftBCD:
            Result := Result and (cur_param.AsCurrency = cur_field.AsCurrency);
          ftFMTBCD:
            Result := Result and (BcdCompare(cur_param.AsBcd, cur_field.AsBCD) = 0);
          ftDate:
            Result := Result and (cur_param.AsDate = cur_field.AsDateTime);
          ftTime:
            Result := Result and (cur_param.AsTime = cur_field.AsDateTime);
          ftDateTime:
            Result := Result and (cur_param.AsDateTime = cur_field.AsDateTime);
          else
            Result := false;
        end;
      end;
      Inc(i);
    end;
    Result := not Result;
  end;

begin
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> nil then
        if DataSet.Active and (DataSet.State <> dsSetKey) and NeedsRefreshing then
        begin
          Close;
          Open;
        end;
    end;
  finally
    EnableControls;
  end;
end;


procedure TIBCustomDataSet.SQLChanging(Sender: TObject);
begin
  if FOpen then
    Close;
  if FInternalPrepared then
    InternalUnPrepare;
  if Sender = FQSelect then
    FieldDefs.Clear;
end;

{ I can "undelete" uninserted records (make them "inserted" again).
  I can "undelete" cached deleted (the deletion hasn't yet occurred) }
procedure TIBCustomDataSet.Undelete;
var
  Buff: PRecordData;
begin
  CheckActive;
  Buff := PRecordData(GetActiveBuf);
  if Buff^.rdCachedUpdateStatus = cusUninserted then
  begin
    Buff^.rdCachedUpdateStatus := cusInserted;
    Dec(FDeletedRecords);
  end
  else if (Buff^.rdUpdateStatus = usDeleted) and
          (Buff^.rdCachedUpdateStatus = cusDeleted) then
  begin
    Buff^.rdCachedUpdateStatus := cusUnmodified;
    Buff^.rdUpdateStatus := usUnmodified;
    Dec(FDeletedRecords);
  end;
  WriteRecordCache(Buff^.rdRecordNumber, TRecBuf(Buff));
end;

function TIBCustomDataSet.UpdateStatus: TUpdateStatus;
begin
  if Active then
    if GetActiveBuf <> 0 then
      result := PRecordData(GetActiveBuf)^.rdUpdateStatus
    else
      result := usUnmodified
  else
    result := usUnmodified;
end;

function TIBCustomDataSet.IsSequenced: Boolean;
begin
  Result := Assigned( FQSelect ) and FQSelect.EOF;
end;

function TIBCustomDataSet.AdjustPosition(FCache: TRecBuf; Offset: DWORD;
                                        Origin: Integer): Integer;
var
  OldCacheSize: Integer;
begin
  if (FCache = FBufferCache) then
  begin
    case Origin of
      BUFFER_BEGIN:    FBPos := Offset;
      BUFFER_CURRENT:  FBPos := FBPos + Offset;
      BUFFER_END:      FBPos := DWORD(FBEnd) + Offset;
    end;
    OldCacheSize := FCacheSize;
    while (FBPos >= DWORD(FCacheSize)) do
      Inc(FCacheSize, FBufferChunkSize);
    if FCacheSize > OldCacheSize then
      IBAlloc(FBufferCache, FCacheSize, FCacheSize);
    result := FBPos;
  end
  else begin
    case Origin of
      BUFFER_BEGIN:    FOBPos := Offset;
      BUFFER_CURRENT:  FOBPos := FOBPos + Offset;
      BUFFER_END:      FOBPos := DWORD(FOBEnd) + Offset;
    end;
    OldCacheSize := FOldCacheSize;
    while (FBPos >= DWORD(FOldCacheSize)) do
      Inc(FOldCacheSize, FBufferChunkSize);
    if FOldCacheSize > OldCacheSize then
      IBAlloc(FOldBufferCache, FOldCacheSize, FOldCacheSize);
    result := FOBPos;
  end;
end;

procedure TIBCustomDataSet.ReadCache(FCache: TRecBuf; Offset: DWORD; Origin: Integer;
                                    Buffer: TRecBuf);
var
  pCache: PByte;
  bOld: Boolean;
begin
  bOld := (FCache = FOldBufferCache);
  pCache := PByte(AdjustPosition(FCache, Offset, Origin));
  if not bOld then
    pCache := PByte(FBufferCache) + Integer(pCache)
  else
    pCache := PByte(FOldBufferCache) + Integer(pCache);
  Move(pCache^, PByte(Buffer)[0], DWORD(FRecordBufferSize));
  AdjustPosition(FCache, FRecordBufferSize, BUFFER_CURRENT);
end;

procedure TIBCustomDataSet.ReadRecordCache(RecordNumber: Integer; Buffer: TRecBuf;
                                          ReadOldBuffer: Boolean);
begin
  if FUniDirectional then
    RecordNumber := RecordNumber mod UniCache;
  if (ReadOldBuffer) then
  begin
    ReadRecordCache(RecordNumber, Buffer, False);
    if FCachedUpdates and
      (PRecordData(Buffer)^.rdSavedOffset <> $FFFFFFFF) then
      ReadCache(FOldBufferCache, PRecordData(Buffer)^.rdSavedOffset, BUFFER_BEGIN,
                Buffer)
    else
      if ReadOldBuffer and
         (PRecordData(FOldBuffer)^.rdRecordNumber = RecordNumber) then
         CopyRecordBuffer( FOldBuffer, Buffer )
  end
  else
    ReadCache(FBufferCache, RecordNumber * FRecordBufferSize, BUFFER_BEGIN, Buffer);
end;

procedure TIBCustomDataSet.WriteCache(FCache: TRecBuf; Offset: DWORD; Origin: Integer;
                                     Buffer: TRecBuf);
var
  pCacheOffset: Integer;
  pCache : TRecBuf;
  bOld: Boolean;
  dwEnd: DWORD;
begin
  bOld := (FCache = FOldBufferCache);
  pCacheOffset := AdjustPosition(FCache, Offset, Origin);
  if not bOld then
    pCache := FBufferCache
  else
    pCache := FOldBufferCache;
  Move(PByte(Buffer)[0], PByte(pCache)[pCacheOffset], FRecordBufferSize);
  dwEnd := AdjustPosition(FCache, FRecordBufferSize, BUFFER_CURRENT);
  if not bOld then
  begin
    if (dwEnd > FBEnd) then
      FBEnd := dwEnd;
  end
  else begin
    if (dwEnd > FOBEnd) then
      FOBEnd := dwEnd;
  end;
end;

procedure TIBCustomDataSet.WriteRecordCache(RecordNumber: Integer; Buffer: TRecBuf);
begin
  if RecordNumber >= 0 then
  begin
    if FUniDirectional then
      RecordNumber := RecordNumber mod UniCache;
    WriteCache(FBufferCache, RecordNumber * FRecordBufferSize, BUFFER_BEGIN, Buffer);
  end;
end;

{$IFDEF NEXTGEN}
function TIBCustomDataSet.AllocRecBuf: TRecBuf;
begin
  result := 0;
  IBAlloc(result, FRecordBufferSize, FRecordBufferSize);
  Move(PByte(FModelBuffer)[0], PByte(result)[0], FRecordBufferSize);
end;

procedure TIBCustomDataSet.FreeRecBuf(var Buffer: TRecBuf);
begin
  if Buffer <> 0 then
    ReallocMem(Pointer(Buffer), 0);
end;
{$ELSE}
function TIBCustomDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  result := nil;
  IBAlloc(result, FRecordBufferSize, FRecordBufferSize);
  Move(PByte(FModelBuffer)[0], result^, FRecordBufferSize);
end;

procedure TIBCustomDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  if Assigned(Buffer) then
    ReallocMem(Buffer, 0);
end;
{$ENDIF NEXTGEN}

function TIBCustomDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  pb: PBlobDataArray;
  fs: TIBBlobStream;
  Buff: TRecBuf;
  bTr, bDB: Boolean;
begin
  Buff := GetActiveBuf;
  if Buff = 0 then
  begin
    fs := TIBBlobStream.Create();
    fs.Mode := bmReadWrite;
    fs.Database := Database;
    fs.Transaction := Transaction;
    FBlobStreamList.Add(fs);
    result := TIBDSBlobStream.Create(Field, fs, Mode);
    exit;
  end;
  pb := PBlobDataArray(PByte(Buff) + FBlobCacheOffset);
  if pb^[Field.Offset] = nil then
  begin
    AdjustRecordOnInsert(Buff);
    pb^[Field.Offset] := TIBBlobStream.Create();
    fs := pb^[Field.Offset];
    FBlobStreamList.Add(fs);
    fs.Mode := bmReadWrite;
    fs.Database := Database;
    fs.Transaction := Transaction;
    fs.BlobID :=
      PISC_QUAD(@(PByte(Buff)[PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataOfs]))^;
    if (CachedUpdates) then
    begin
      bTr := not Transaction.InTransaction;
      bDB := not Database.Connected;
      if bDB then
        Database.Open;
      if bTr then
        Transaction.StartTransaction;
      fs.Seek(0, soFromBeginning);
      if bTr then
        Transaction.Commit;
      if bDB then
        Database.Close;
    end;
    WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
  end
  else
    fs := pb^[Field.Offset];
  result := TIBDSBlobStream.Create(Field, fs, Mode);
end;

function TIBCustomDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  CMPLess = -1;
  CMPEql  =  0;
  CMPGtr  =  1;
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2, CMPLess),
                                                   (CMPGtr, CMPEql));
begin
  result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];

  if Result = 2 then
  begin
    if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
      Result := CMPLess
    else
    if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
      Result := CMPGtr
    else
      Result := CMPEql;
  end;
end;

procedure TIBCustomDataSet.DoBeforeDelete;
var
  Buff: PRecordData;
begin
  if not CanDelete then
    IBError(ibxeCannotDelete, [nil]);
  Buff := PRecordData(GetActiveBuf);
  if FCachedUpdates and
    (Buff^.rdCachedUpdateStatus in [cusUnmodified]) then
    SaveOldBuffer(TRecBuf(Buff));
  inherited DoBeforeDelete;
end;

procedure TIBCustomDataSet.DoBeforeEdit;
var
  Buff: PRecordData;
begin
  inherited DoBeforeEdit;
  Buff := PRecordData(GetActiveBuf);
  if not(CanEdit or
    (FCachedUpdates and Assigned(FOnUpdateRecord))) then
    IBError(ibxeCannotUpdate, [nil]);
  if FCachedUpdates and (Buff^.rdCachedUpdateStatus in [cusUnmodified, cusInserted]) then
    SaveOldBuffer(TRecBuf(Buff));
  CopyRecordBuffer(GetActiveBuf, FOldBuffer);
end;

procedure TIBCustomDataSet.DoBeforeInsert;
begin
  if not CanInsert then
    IBError(ibxeCannotInsert, [nil]);
  inherited DoBeforeInsert;
end;

procedure TIBCustomDataSet.FetchAll;
var
  CurBookmark: TBytes;
begin
  if FQSelect.EOF or not FQSelect.Open then
    exit;
  DisableControls;
  try
    CurBookmark := Bookmark;
    Last;
    Bookmark := CurBookmark;
  finally
    EnableControls;
  end;
end;

procedure TIBCustomDataSet.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  if not IsEmpty then
    Move(PRecordData(Buffer)^.rdRecordNumber, Data[0], BookmarkSize)
end;

function TIBCustomDataSet.GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag;
begin
  result := PRecordData(Buffer)^.rdBookmarkFlag;
end;

function TIBCustomDataSet.GetCanceled: Boolean;
begin
  Result := FQSelect.Canceled;
end;

function TIBCustomDataSet.GetCanModify: Boolean;
begin
  result := ([lmInsert, lmModify, lmDelete] * FLiveMode <> []) or
            (Assigned(FUpdateObject));
end;

function TIBCustomDataSet.GetCurrentRecord(Buffer: TRecBuf): Boolean;
begin
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    ReadRecordCache(PRecordData(ActiveBuffer)^.rdRecordNumber, Buffer, False);
    result := True;
  end
  else
    result := False;
end;

function TIBCustomDataSet.GetDataSource: TDataSource;
begin
  if FDataLink = nil then
    result := nil
  else
    result := FDataLink.DataSource;
end;

function TIBCustomDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := DefaultFieldClasses[FieldType];
end;

{$IFNDEF NEXTGEN}
function TIBCustomDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  lTempCurr : System.Currency;
begin
  if (Field.DataType = ftBCD) and (Buffer <> nil) then
  begin
    Result := InternalGetFieldData(Field, TValueBuffer(@lTempCurr));
    if Result then
      CurrToBCD(lTempCurr, TBCD(Buffer^), 32, Field.Size);
  end
  else
    Result := InternalGetFieldData(Field, TValueBuffer(Buffer));
end;
{$ENDIF !NEXTGEN}

function TIBCustomDataSet.InternalGetFieldData(Field: TField; Buffer: TValueBuffer): Boolean;
var
  Buff: TRecBuf;
  Data : PByte;
  CurrentRecord: PRecordData;
  cFieldData : TFieldData;
begin
  result := False;
  Buff := GetActiveBuf;
  if (Buff = 0) or (not IsVisible(Buff)) then
    exit;
  { The intention here is to stuff the buffer with the data for the
   referenced field for the current record }
  CurrentRecord := PRecordData(Buff);
  if (Field.FieldNo < 0) then
  begin
    result := Boolean(PByte(Buff)[FRecordSize + Field.Offset]);
    if result and (Buffer <> nil) then
      Move(PByte(Buff)[FRecordSize + Field.Offset + 1], PByte(Buffer)[0], Field.DataSize);
  end
  else
  if (FMappedFieldPosition[Field.FieldNo - 1] > 0) and
     (FMappedFieldPosition[Field.FieldNo - 1] <= CurrentRecord^.rdFieldCount) then
  begin
    result := not CurrentRecord^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdIsNull;
    if result and (Buffer <> nil) then
    begin
      cFieldData := CurrentRecord^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]];
      Data := PByte(Buff) + cFieldData.fdDataOfs;
      if (cFieldData.fdDataType = SQL_VARYING) or (cFieldData.fdDataType = SQL_TEXT) then
      begin
        // fdDatalength represents the internal bytes length which is now Unicode
        if cFieldData.fdDataLength div 2 <= Field.Size then
        begin
          if (Field is TStringfield) and TStringField(Field).FixedChar then
            StrPCopy(PChar(Buffer), StringOfChar(' ', Field.Size));
          Move(Data^, PByte(Buffer)[0], cFieldData.fdDataLength);
          if (Field is TStringfield) and TStringField(Field).FixedChar then
            PChar(Buffer)[Field.Size] := #0
          else
            PChar(Buffer)[cFieldData.fdDataLength div 2] := #0;
          if (cFieldData.fdDataType = SQL_TEXT) and (not TStringField(Field).FixedChar) then
            PChar(Buffer)[Length(TrimRight(PChar(Buffer)))] := #0;
        end
        else
          IBError(ibxeFieldSizeMismatch, [Field.FieldName]);
      end
      else
        Move(Data^, PByte(Buffer)[0], Field.DataSize);
    end;
  end;
end;

function TIBCustomDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
var
  lTempCurr : System.Currency;
  lBcd: TBcd;
begin
  if (Field.DataType = ftBCD) and (Buffer <> nil) then
  begin
    Result := InternalGetFieldData(Field, TValueBuffer(@lTempCurr));
    if Result then
    begin
      CurrToBCD(lTempCurr, lBcd, 32, Field.Size);
      Move(lBcd, Buffer[0], SizeOf(TBCD));
    end;
  end
  else
    Result := InternalGetFieldData(Field, Buffer);
end;

function TIBCustomDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer; NativeFormat: Boolean): Boolean;
begin
  if (Field.DataType = ftBCD) and not NativeFormat then
    Result := InternalGetFieldData(Field, Buffer)
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;

{ GetRecNo and SetRecNo both operate off of 1-based indexes as
 opposed to 0-based indexes.
 This is because we want LastRecordNumber/RecordCount = 1 }

function TIBCustomDataSet.GetRecNo: Integer;
begin
  if GetActiveBuf = 0 then
    result := 0
  else
    result := PRecordData(GetActiveBuf)^.rdRecordNumber + 1;
end;

function TIBCustomDataSet.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
  SaveState: TDataSetState;
begin
  Result := grOK;
  if Filtered and Assigned(OnFilterRecord) then
  begin
    Accept := False;
    SaveState := SetTempState(dsFilter);
    while not Accept do
    begin
      Result := InternalGetRecord(Buffer, GetMode, DoCheck);
      if Result <> grOK then
        break;
      FFilterBuffer := Buffer;
      Accept := True;
      OnFilterRecord(Self, Accept);
      if not Accept and (GetMode = gmCurrent) then
        GetMode := gmPrior;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck);
end;


{$IFNDEF NEXTGEN}
function TIBCustomDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
  SaveState: TDataSetState;
begin
  Result := grOK;
  if Filtered and Assigned(OnFilterRecord) then
  begin
    Accept := False;
    SaveState := SetTempState(dsFilter);
    while not Accept do
    begin
      Result := InternalGetRecord(TRecBuf(Buffer), GetMode, DoCheck);
      if Result <> grOK then
        break;
      FFilterBuffer := TRecBuf(Buffer);
      Accept := True;
      OnFilterRecord(Self, Accept);
      if not Accept and (GetMode = gmCurrent) then
        GetMode := gmPrior;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(TRecBuf(Buffer), GetMode, DoCheck);
end;
{$ENDIF NEXTGEN}

function TIBCustomDataSet.InternalGetRecord(Buffer: TRecBuf; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  result := grError;
  case GetMode of
    gmCurrent:
    begin
      if (FCurrentRecord >= 0) then
      begin
        if FCurrentRecord < FRecordCount then
          ReadRecordCache(FCurrentRecord, Buffer, False)
        else
        begin
          while (not FQSelect.EOF) and
                (FCurrentRecord >= FRecordCount) do
          begin
            if FQSelect.Next = nil then
              break;
            FetchCurrentRecordToBuffer(FQSelect, FRecordCount, Buffer);
            Inc(FRecordCount);
          end;
          FCurrentRecord := FRecordCount - 1;
          if (FCurrentRecord >= 0) then
            ReadRecordCache(FCurrentRecord, Buffer, False);
        end;
        result := grOk;
      end
      else
        result := grBOF;
    end;
    gmNext:
    begin
      result := grOk;
      if FCurrentRecord = FRecordCount then
        result := grEOF
      else
      if FCurrentRecord = FRecordCount - 1 then
      begin
        if (not FQSelect.EOF) then
        begin
          FQSelect.Next;
          Inc(FCurrentRecord);
        end;
        if (FQSelect.EOF) then
        begin
          result := grEOF;
        end
        else
        begin
          Inc(FRecordCount);
          FetchCurrentRecordToBuffer(FQSelect, FCurrentRecord, Buffer);
        end;
      end
      else
        if (FCurrentRecord < FRecordCount) then
        begin
          Inc(FCurrentRecord);
          ReadRecordCache(FCurrentRecord, Buffer, False);
        end;
    end;
    else { gmPrior }
    begin
      if (FCurrentRecord = 0) then
      begin
        Dec(FCurrentRecord);
        result := grBOF;
      end
      else
        if (FCurrentRecord > 0) and
                    (FCurrentRecord <= FRecordCount) then
        begin
          Dec(FCurrentRecord);
          ReadRecordCache(FCurrentRecord, Buffer, False);
          result := grOk;
        end
        else
          if (FCurrentRecord = -1) then
            result := grBOF;
    end;
  end;
  if result = grOk then
    result := AdjustCurrentRecord(Buffer, GetMode);
  if result = grOk then
  begin
    PRecordData(Buffer)^.rdBookmarkFlag := bfCurrent;
    GetCalcFields(Buffer);
  end
  else
    if (result = grEOF) then
    begin
      CopyRecordBuffer(FModelBuffer, Buffer);
      PRecordData(Buffer)^.rdBookmarkFlag := bfEOF;
    end
    else
      if (result = grBOF) then
      begin
        CopyRecordBuffer(FModelBuffer, Buffer);
        PRecordData(Buffer)^.rdBookmarkFlag := bfBOF;
      end
      else
        if (result = grError) then
        begin
          CopyRecordBuffer(FModelBuffer, Buffer);
          PRecordData(Buffer)^.rdBookmarkFlag := bfEOF;
        end;
end;

function TIBCustomDataSet.GetRecordCount: Integer;
begin
  result := FRecordCount - FDeletedRecords;
end;

function TIBCustomDataSet.GetRecordSize: Word;
begin
  result := FRecordBufferSize;
end;

procedure TIBCustomDataSet.InternalAddRecord(Buffer: TRecBuf; Append: Boolean);
begin
  CheckEditState;
  begin
     { When adding records, we *always* append.
       Insertion is just too costly }
    AdjustRecordOnInsert(Buffer);
    PRecordData(Buffer)^.rdUpdateStatus := usInserted;
    PRecordData(Buffer)^.rdCachedUpdateStatus := cusInserted;
    if not CachedUpdates then
      InternalPostRecord(FQInsert, Buffer)
    else begin
      WriteRecordCache(FCurrentRecord, Buffer);
      FUpdatesPending := True;
    end;
    Inc(FRecordCount);
    InternalSetToRecord(Buffer);
  end
end;

procedure TIBCustomDataSet.InternalCancel;
var
  Buff: TRecBuf;
  CurRec, i, j : Integer;
  pbd: PBlobDataArray;
begin
  inherited InternalCancel;
  Buff := GetActiveBuf;
  if Buff <> 0 then
  begin
    CurRec := FCurrentRecord;
    AdjustRecordOnInsert(Buff);
    if (State = dsEdit) then begin
      CopyRecordBuffer(FOldBuffer, Buff);
      WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
    end
    else
    begin
      CopyRecordBuffer(FModelBuffer, Buff);
      PRecordData(Buff)^.rdUpdateStatus := usDeleted;
      PRecordData(Buff)^.rdCachedUpdateStatus := cusUnmodified;
      PRecordData(Buff)^.rdBookmarkFlag := bfEOF;
      FCurrentRecord := CurRec;
    end;
  end;
  pbd := PBlobDataArray(PByte(Buff) + FBlobCacheOffset);
  j := 0;
  for i := 0 to FieldCount - 1 do
    if Fields[i].IsBlob then
    begin
      if pbd^[j] <> nil then
        pbd^[j].Cancel;
      Inc(j);
    end;
end;

procedure TIBCustomDataSet.InternalClose;
begin
  if Assigned(Transaction) then
    Transaction.CheckAutoStop;

  {note - to not raise an exception if the conenction is lost
          before destroying the object, only close it if it is
          not being destroyed.  If it is being destroyed the
          IBSQL will be cleaned up normally when it is destroyed}
  if not (csDestroying in ComponentState) then
    FQSelect.Close;
  ClearBlobCache;

{$IFDEF NEXTGEN}
  FreeRecBuf(FModelBuffer);
  FreeRecBuf(FOldBuffer);
  FreeRecBuf(FTempBuffer);
{$ELSE}
  FreeRecordBuffer(TRecordBuffer(FModelBuffer));
  FreeRecordBuffer(TRecordBuffer(FOldBuffer));
  FreeRecordBuffer(TRecordBuffer(FTempBuffer));
{$ENDIF NEXTGEN}
  FCurrentRecord := -1;
  FOpen := False;
  FRecordCount := 0;
  FDeletedRecords := 0;
  FRecordSize := 0;
  FBPos := 0;
  FOBPos := 0;
  FCacheSize := 0;
  FOldCacheSize := 0;
  FBEnd := 0;
  FOBEnd := 0;
  if FBufferCache <> 0 then
    ReallocMem(Pointer(FBufferCache), 0);
  if FOldBufferCache <> 0 then
    ReallocMem(Pointer(FOldBufferCache), 0);

  FUpdatesPending := false;
  if not (csDestroying in ComponentState) then
  begin
    BindFields(False);

    DestroyFields;
  end;
end;

procedure TIBCustomDataSet.InternalDelete;
var
  Buff: TRecBuf;
begin
  Buff := GetActiveBuf;
  if CanDelete then
  begin
    if not CachedUpdates then
      InternalDeleteRecord(FQDelete, Buff)
    else
    begin
      if PRecordData(Buff)^.rdCachedUpdateStatus = cusInserted then
        PRecordData(Buff)^.rdCachedUpdateStatus := cusUninserted
      else
      begin
        PRecordData(Buff)^.rdUpdateStatus := usDeleted;
        PRecordData(Buff)^.rdCachedUpdateStatus := cusDeleted;
      end;
      WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
    end;
    Inc(FDeletedRecords);
    FUpdatesPending := True;
  end
  else
    IBError(ibxeCannotDelete, [nil]);
end;

procedure TIBCustomDataSet.InternalFirst;
begin
  FCurrentRecord := -1;
end;

procedure TIBCustomDataSet.InternalGotoBookmark(Bookmark: TBookmark);
var
  curRecord : Integer;
begin
  move(Bookmark[0], curRecord, Sizeof(curRecord));
  FCurrentRecord := curRecord;
end;

{$IFNDEF NEXTGEN}
procedure TIBCustomDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  InternalGotoBookmark(TBookmark(Bookmark));
end;
{$ENDIF !NEXTGEN}

procedure TIBCustomDataSet.InternalHandleException;
begin
end;

procedure TIBCustomDataSet.InternalInitFieldDefs;
var
  FieldType: TFieldType;
  FieldSize: Word;
  FieldNullable : Boolean;
  i, FieldPosition, FieldPrecision, FieldIndex: Integer;
  FieldAliasName: String;
  RelationName, FieldName: String;
  cFieldDef : TFieldDef;
begin
  if not InternalPrepared then
  begin
    InternalPrepare;
    exit;
  end;
  FNeedsRefresh := False;
  try
    FieldDefs.BeginUpdate;
    FieldDefs.Clear;
    FieldIndex := 0;
    if (Length(FMappedFieldPosition) < FQSelect.Current.Count) then
      SetLength(FMappedFieldPosition, FQSelect.Current.Count);
    for i := 0 to FQSelect.Current.Count - 1 do
    begin
      { Get the field name }
      FieldAliasName := String(FQSelect.Current[i].Data.aliasname);
      RelationName := String(FQSelect.Current[i].Data.relname);
      FieldName := String(FQSelect.Current[i].Data.sqlname);
      FieldSize := 0;
      FieldPrecision := 0;
      FieldNullable := FQSelect.Current[i].IsNullable;
      case FQSelect.Current[i].Data.sqltype and not 1 of
        { All VARCHAR's must be converted to strings before recording
         their values }
        SQL_VARYING, SQL_TEXT:
        begin
          // Workaround an IB Bug
          if FQSelect.Current[i].Data.RelName = '' then
            FieldSize := FQSelect.Current[i].Data.sqllen
          else
            FieldSize := FQSelect.Current[i].Data.sqllen div FQSelect.Current[i].CharsetSize;
          FieldType := ftWideString;
        end;
        { All Doubles/Floats should be cast to doubles }
        SQL_DOUBLE, SQL_FLOAT:
          FieldType := ftFloat;
        SQL_SHORT:
        begin
          if (FQSelect.Current[i].Data.sqlscale = 0) then
            FieldType := ftSmallInt
          else
          begin
            FieldType := ftBCD;
            FieldPrecision := FQSelect.Current[i].Data.SqlPrecision;
            FieldSize := -FQSelect.Current[i].Data.sqlscale;
            if FieldPrecision = FieldSize then
              Inc(FieldPrecision);
          end;
        end;
        SQL_LONG:
        begin
          if (FQSelect.Current[i].Data.sqlscale = 0) then
            FieldType := ftInteger
          else
            if (FQSelect.Current[i].Data.sqlscale >= (-4)) then
            begin
              FieldType := ftBCD;
              FieldPrecision := FQSelect.Current[i].Data.SqlPrecision;
              FieldSize := -FQSelect.Current[i].Data.sqlscale;
              if FieldPrecision = FieldSize then
                Inc(FieldPrecision);
            end
            else
              if Database.SQLDialect = 1 then
                FieldType := ftFloat
              else
                if (FieldCount > 0) and
                   (Fields.FieldByName(FieldAliasName) is TFloatField) then
                  FieldType := ftFloat
                else
                begin
                  FieldType := ftFMTBCD;
                  FieldPrecision := FQSelect.Current[i].Data.SqlPrecision;
                  FieldSize := -FQSelect.Current[i].Data.sqlscale;
                  if FieldPrecision = FieldSize then
                    Inc(FieldPrecision);
                end;
            end;
        SQL_INT64:
        begin
          if (FQSelect.Current[i].Data.sqlscale = 0) then
            FieldType := ftLargeInt
          else
            if (FQSelect.Current[i].Data.sqlscale >= (-4)) then
            begin
              FieldType := ftBCD;
              FieldPrecision := FQSelect.Current[i].Data.SqlPrecision;
              FieldSize := -FQSelect.Current[i].Data.sqlscale;
            end
            else
            begin
              if (FieldCount > 0) and
                 (Fields.FieldByName(FieldAliasName) is TFloatField) then
                FieldType := ftFloat
              else
              begin
                FieldType := ftFMTBCD;
                FieldPrecision := FQSelect.Current[i].Data.SqlPrecision;
                FieldSize := -FQSelect.Current[i].Data.sqlscale;
                if FieldPrecision = FieldSize then
                  Inc(FieldPrecision);
              end;
            end;
        end;
        SQL_TIMESTAMP: FieldType := ftDateTime;
        SQL_TYPE_TIME: FieldType := ftTime;
        SQL_TYPE_DATE: FieldType := ftDate;
        SQL_BLOB:
        begin
          FieldSize := sizeof (TISC_QUAD);
          if (FQSelect.Current[i].Data.sqlsubtype = 1) then
            FieldType := ftWideMemo
          else
            FieldType := ftBlob;
        end;
        SQL_ARRAY:
        begin
          FieldSize := sizeof (TISC_QUAD);
          FieldType := ftUnknown;
        end;
        SQL_BOOLEAN:
          FieldType := ftBoolean;
        else
          FieldType := ftUnknown;
      end;
      FieldPosition := i + 1;
      if (FieldType <> ftUnknown) and (FieldAliasName <> 'IBX_INTERNAL_DBKEY') then {do not localize}
      begin
        FMappedFieldPosition[FieldIndex] := FieldPosition;
        Inc(FieldIndex);
        cFieldDef := FieldDefs.AddFieldDef;
        cFieldDef.Name := FieldAliasName;
        cFieldDef.FieldNo := FieldPosition;
        cFieldDef.DataType := FieldType;
        cFieldDef.Size := FieldSize;
        cFieldDef.Precision := FieldPrecision;
        cFieldDef.Required := (not FieldNullable) and (Trim(RelationName) <> '');
        cFieldDef.InternalCalcField := False;
        if (FieldName <> '') then
        begin
          if(RelationName <> '')  then
          begin
            if Database.Has_COMPUTED_BLR(RelationName, FieldName) then
            begin
              cFieldDef.Attributes := [faReadOnly];
              cFieldDef.InternalCalcField := True;
              FNeedsRefresh := True;
            end
            else
            begin
              if Database.Has_DEFAULT_VALUE(RelationName, FieldName) then
              begin
                cFieldDef.Attributes := cFieldDef.Attributes - [faRequired];
                FNeedsRefresh := True;
              end;
            end;
          end
          else
            cFieldDef.Attributes := [faReadOnly];
        end;
        if ((FQSelect.Current[i].Data.SQLType and not 1) = SQL_TEXT) then
          cFieldDef.Attributes := cFieldDef.Attributes + [faFixed];
      end;
    end;
  finally
    FieldDefs.EndUpdate;
  end;
end;

procedure TIBCustomDataSet.InternalInitRecord(Buffer: TRecBuf);
begin
  CopyRecordBuffer(FModelBuffer, Buffer);
end;

procedure TIBCustomDataSet.InternalLast;
var
  Buffer: TRecBuf;
begin
  if (FQSelect.EOF) then
    if FRecordCount = 0 then
      FCurrentRecord := -1
    else
      FCurrentRecord := FRecordCount
  else
  begin
{$IFDEF NEXTGEN}
    Buffer := AllocRecBuf;
{$ELSE}
    Buffer := TRecBuf(AllocRecordBuffer);
{$ENDIF NEXTGEN}
    try
      while FQSelect.Next <> nil do
      begin
        FetchCurrentRecordToBuffer(FQSelect, FRecordCount, Buffer);
        Inc(FRecordCount);
      end;
      FCurrentRecord := FRecordCount;
    finally
{$IFDEF NEXTGEN}
      FreeRecBuf(Buffer);
{$ELSE}
      FreeRecordBuffer(TRecordBuffer(Buffer));
{$ENDIF NEXTGEN}
    end;
  end;
end;

procedure TIBCustomDataSet.InternalSetParamsFromCursor;
var
  i: Integer;
  cur_param: TIBXSQLVAR;
  cur_field: TField;
  s: TStream;
begin
  if FQSelect.SQL.Text = '' then
    IBError(ibxeEmptyQuery, [nil]);
  if not FInternalPrepared then
    InternalPrepare;
  if (SQLParams.Count > 0) and (DataSource <> nil) and (DataSource.DataSet <> nil) then
  begin
    for i := 0 to SQLParams.Count - 1 do
    begin
      cur_field := DataSource.DataSet.FindField(SQLParams[i].Name);
      cur_param := SQLParams[i];
      if (cur_field <> nil) then
      begin
        if (cur_field.IsNull) then
          cur_param.IsNull := True
        else case cur_field.DataType of
          ftString, ftWideString:
            cur_param.AsString := cur_field.AsString;
          ftBoolean:
            cur_param.AsBoolean := cur_field.AsBoolean;
          ftShortInt, ftSmallint, ftWord, ftByte:
            cur_param.AsShort := cur_field.AsInteger;
          ftInteger:
            cur_param.AsLong := cur_field.AsInteger;
          ftLargeInt:
            cur_param.AsInt64 := cur_field.AsLargeInt;
          ftFloat, ftCurrency:
           cur_param.AsDouble := cur_field.AsFloat;
          ftBCD:
            cur_param.AsCurrency := cur_field.AsCurrency;
          ftFMTBcd :
            cur_param.AsBcd := cur_field.AsBCD;
          ftDate:
            cur_param.AsDate := cur_field.AsDateTime;
          ftTime:
            cur_param.AsTime := cur_field.AsDateTime;
          ftDateTime:
            cur_param.AsDateTime := cur_field.AsDateTime;
          ftBlob, ftMemo, ftWideMemo:
          begin
            s := nil;
            try
              s := DataSource.DataSet.CreateBlobStream(cur_field, bmRead);
              cur_param.LoadFromStream(s);
            finally
              s.free;
            end;
          end;
          else
            IBError(ibxeNotSupported, [nil]);
        end;
      end;
    end;
  end;
end;

procedure TIBCustomDataSet.ReQuery;
begin
  FQSelect.Close;
  ClearBlobCache;
  FCurrentRecord := -1;
  FRecordCount := 0;
  FDeletedRecords := 0;
  FBPos := 0;
  FOBPos := 0;
  FBEnd := 0;
  FOBEnd := 0;
  FQSelect.Close;
  FQSelect.ExecQuery;
  FOpen := FQSelect.Open;
  FRowsAffected := FQSelect.RowsAffected;
  First;
end;

procedure TIBCustomDataSet.InternalOpen;

  function RecordDataLength(n: Integer): Long;
  begin
    result := SizeOf(TRecordData) + ((n - 1) * SizeOf(TFieldData));
  end;

begin
  ActivateConnection;
  ActivateTransaction;
  if FQSelect.SQL.Text = '' then
    IBError(ibxeEmptyQuery, [nil]);
  if not FInternalPrepared then
    InternalPrepare;
  if FQSelect.SQLType = SQLSelect then
  begin
    CreateFields;
    BindFields(True);
    FCurrentRecord := -1;
    FQSelect.ExecQuery;
    FOpen := FQSelect.Open;

    { Initialize offsets, buffer sizes, etc...
      1. Initially FRecordSize is just the "RecordDataLength".
      2. Allocate a "model" buffer and do a dummy fetch
      3. After the dummy fetch, FRecordSize will be appropriately
         adjusted to reflect the additional "weight" of the field
         data.
      4. Set up the FCalcFieldsOffset, FBlobCacheOffset and FRecordBufferSize.
      5. Now, with the BufferSize available, allocate memory for chunks of records
      6. Re-allocate the model buffer, accounting for the new
         FRecordBufferSize.
      7. Finally, calls to AllocRecordBuffer will work!.
     }
    {Step 1}
    FRecordSize := RecordDataLength(FQSelect.Current.Count);
    {Step 2, 3}
    IBAlloc(FModelBuffer, 0, FRecordSize);
    FetchCurrentRecordToBuffer(FQSelect, -1, FModelBuffer);
    {Step 4}
    FCalcFieldsOffset := FRecordSize;
    FBlobCacheOffset := FCalcFieldsOffset + CalcFieldsSize;
    FRecordBufferSize := (FBlobCacheOffset + (BlobFieldCount * SizeOf(TIBBlobStream)));
    {Step 5}
    if UniDirectional then
      FBufferChunkSize := FRecordBufferSize * UniCache
    else
      FBufferChunkSize := FRecordBufferSize * BufferChunks;
    IBAlloc(FBufferCache, FBufferChunkSize, FBufferChunkSize);
    if FCachedUpdates or (csReading in ComponentState) then
      IBAlloc(FOldBufferCache, FBufferChunkSize, FBufferChunkSize);
    FBPos := 0;
    FOBPos := 0;
    FBEnd := 0;
    FOBEnd := 0;
    FCacheSize := FBufferChunkSize;
    FOldCacheSize := FBufferChunkSize;
    {Step 6}
    IBAlloc(FModelBuffer, RecordDataLength(FQSelect.Current.Count),
                           FRecordBufferSize);
    {Step 7}
{$IFDEF NEXTGEN}
    FOldBuffer := AllocRecBuf;
    FTempBuffer := AllocRecBuf;
 {$ELSE}
    FOldBuffer := TRecBuf(AllocRecordBuffer);
    FTempBuffer := TRecBuf(AllocRecordBuffer);
{$ENDIF NEXTGEN}
  end
  else
    FQSelect.ExecQuery;
  FRowsAffected := FQSelect.RowsAffected;
end;

procedure TIBCustomDataSet.InternalPost;
var
  Qry: TIBSQL;
  Buff: TRecBuf;
  bInserting: Boolean;
begin
  inherited InternalPost;
  Buff := GetActiveBuf;
  CheckEditState;
  AdjustRecordOnInsert(Buff);
  bInserting := False;
  Qry := nil;
  case State of
    dsInsert :
    begin
      bInserting := True;
      Qry := FQInsert;
      PRecordData(Buff)^.rdUpdateStatus := usInserted;
      PRecordData(Buff)^.rdCachedUpdateStatus := cusInserted;
      PRecordData(Buff)^.rdRecordNumber := FRecordCount;
      WriteRecordCache(FRecordCount, Buff);
      FCurrentRecord := FRecordCount;
    end;
    dsEdit :
    begin
      Qry := FQModify;
      if PRecordData(Buff)^.rdCachedUpdateStatus = cusUnmodified then
      begin
        PRecordData(Buff)^.rdUpdateStatus := usModified;
        PRecordData(Buff)^.rdCachedUpdateStatus := cusModified;
      end
      else
        if PRecordData(Buff)^.rdCachedUpdateStatus = cusUninserted then
        begin
          PRecordData(Buff)^.rdCachedUpdateStatus := cusInserted;
          Dec(FDeletedRecords);
        end;
    end;
  end;
  if (not CachedUpdates) then
    InternalPostRecord(Qry, Buff)
  else
  begin
    WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
    FUpdatesPending := True;
  end;
  if bInserting then
    Inc(FRecordCount);
end;

procedure TIBCustomDataSet.InternalRefresh;
begin
  inherited InternalRefresh;
  InternalRefreshRow;
end;

procedure TIBCustomDataSet.InternalSetToRecord(Buffer: TRecBuf);
begin
  FCurrentRecord := PRecordData(Buffer)^.rdRecordNumber;
//  InternalGotoBookmark(@(PRecordData(Buffer)^.rdRecordNumber));
end;

function TIBCustomDataSet.IsCursorOpen: Boolean;
begin
  result := FOpen;
end;

function TIBCustomDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
                                 Options: TLocateOptions): Boolean;
var
  CurBookmark: TBytes;
begin
  DisableControls;
  try
    CurBookmark := Bookmark;
    First;
    result := InternalLocate(KeyFields, KeyValues, Options);
    if not result then
      Bookmark := CurBookmark;
  finally
    EnableControls;
  end;
end;

function TIBCustomDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
                                 const ResultFields: string): Variant;
var
  CurBookmark: TBytes;
begin
  DisableControls;
  CurBookmark := Bookmark;
  try
    First;
    if InternalLocate(KeyFields, KeyValues, []) then
    begin
      if (ResultFields <> '') then
        result := FieldValues[ResultFields]
      else
        result := NULL;
    end
    else
      result := Null;
  finally
    Bookmark := CurBookmark;
    EnableControls;
  end;
end;

procedure TIBCustomDataSet.SetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  PRecordData(Buffer)^.rdRecordNumber := PInteger(Data)^;
end;

procedure TIBCustomDataSet.SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag);
begin
  PRecordData(Buffer)^.rdBookmarkFlag := Value;
end;

procedure TIBCustomDataSet.SetCachedUpdates(Value: Boolean);
begin
  if not Value and FCachedUpdates then
    CancelUpdates;
  if (not (csReading in ComponentState)) and Value then
    CheckDatasetClosed;
  FCachedUpdates := Value;
end;

procedure TIBCustomDataSet.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    IBError(ibxeCircularReference, [nil]);
  if FDataLink <> nil then
    FDataLink.DataSource := Value;
end;

procedure TIBCustomDataSet.InternalSetFieldData(Field: TField; Buffer: TValueBuffer);
var
  Buff : PByte;
begin
  Buff := PByte(GetActiveBuf);
  if Field.FieldNo < 0 then
  begin
    Boolean(Buff[FRecordSize + Field.Offset]) := Buffer <> nil;
    if Boolean(Buff[FRecordSize + Field.Offset]) then
      Move(Buffer[0], Buff[FRecordSize + Field.Offset + 1], Field.DataSize);
    WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, TRecBuf(Buff));
  end
  else
  begin
    CheckEditState;
    { If inserting, Adjust record position }
    AdjustRecordOnInsert(TRecBuf(Buff));
    if (FMappedFieldPosition[Field.FieldNo - 1] > 0) and
       (FMappedFieldPosition[Field.FieldNo - 1] <= PRecordData(Buff)^.rdFieldCount) then
    begin
      Field.Validate(Buffer);
      if (Buffer = nil) or
         ((Field is TIBStringField) and (PChar(Buffer)[0] = #0)) then
        if (not (Field is TIBStringField)) or TIBStringField(Field).EmptyAsNull then
          PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdIsNull := True
        else
        begin
          PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataLength := 0;
          PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdIsNull := False;
        end
      else
      begin
        Move(Buffer[0], Buff[PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataOfs],
               PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataSize);
        if (PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataType = SQL_TEXT) or
           (PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataType = SQL_VARYING) then
          PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataLength := StrLen(PChar(Buffer)) * 2;
        PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdIsNull := False;
        if PRecordData(Buff)^.rdUpdateStatus = usUnmodified then
        begin
          if CachedUpdates then
          begin
            FUpdatesPending := True;
            if State = dsInsert then
              PRecordData(Buff)^.rdCachedUpdateStatus := cusInserted
            else if State = dsEdit then
              PRecordData(Buff)^.rdCachedUpdateStatus := cusModified;
          end;

          if State = dsInsert then
            PRecordData(Buff)^.rdUpdateStatus := usInserted
          else
            PRecordData(Buff)^.rdUpdateStatus := usModified;
        end;
        WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, TRecBuf(Buff));
        SetModified(True);
      end;
    end;
  end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Field));
end;

procedure TIBCustomDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value < 1) then
    Value := 1
  else if Value > FRecordCount then
  begin
    InternalLast;
    Value := Min(FRecordCount, Value);
  end;
  if (Value <> RecNo) then
  begin
    DoBeforeScroll;
    FCurrentRecord := Value - 1;
    Resync([]);
    DoAfterScroll;
  end;
end;

procedure TIBCustomDataSet.Disconnect;
begin
  Close;
  InternalUnPrepare;
end;

procedure TIBCustomDataSet.SetUpdateMode(const Value: TUpdateMode);
begin
  if not CanModify then
    IBError(ibxeCannotUpdate, [nil])
  else
    FUpdateMode := Value;
end;


procedure TIBCustomDataSet.SetUpdateObject(Value: TIBDataSetUpdateObject);
begin
  if Value <> FUpdateObject then
  begin
    if Assigned(FUpdateObject) and (FUpdateObject.DataSet = Self) then
    begin
      FUpdateObject.RemoveFreeNotification(Self);
      FUpdateObject.DataSet := nil;
    end;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then
    begin
      FUpdateObject.FreeNotification(Self);
      if Assigned(FUpdateObject.DataSet) and
        (FUpdateObject.DataSet <> Self) then
        FUpdateObject.DataSet.UpdateObject := nil;
      FUpdateObject.DataSet := Self;
    end;
  end;
end;

function TIBCustomDataSet.ConstraintsStored: Boolean;
begin
  Result := Constraints.Count > 0;
end;

procedure TIBCustomDataSet.ClearCalcFields(Buffer: NativeInt);
begin
  FillChar(PByte(Buffer)[FRecordSize], CalcFieldsSize, 0);
end;

{$IFNDEF NEXTGEN}
procedure TIBCustomDataSet.ClearCalcFields(Buffer: TRecordBuffer);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;
{$ENDIF !NEXTGEN}

procedure TIBCustomDataSet.InternalUnPrepare;
begin
  if FInternalPrepared then
  begin
    CheckDatasetClosed;
    FieldDefs.Clear;
    FInternalPrepared := False;
    FLiveMode := [];
  end;
end;

procedure TIBCustomDataSet.InternalExecQuery;
var
  DidActivate: Boolean;
begin
  DidActivate := False;
  try
    ActivateConnection;
    DidActivate := ActivateTransaction;
    if FQSelect.SQL.Text = '' then
      IBError(ibxeEmptyQuery, [nil]);
    if not FInternalPrepared then
      InternalPrepare;
    if FQSelect.SQLType = SQLSelect then
    begin
      IBError(ibxeIsASelectStatement, [nil]);
    end
    else
      FQSelect.ExecQuery;
  finally
    if DidActivate then
      DeactivateTransaction;
  end;
end;

function TIBCustomDataSet.GetSelectStmtHandle: TISC_STMT_HANDLE;
begin
  Result := FQSelect.Handle;
end;

procedure TIBCustomDataSet.InitRecord(Buffer: TRecBuf);
begin
  inherited InitRecord(Buffer);
  InternalInitrecord(Buffer);
  PRecordData(Buffer)^.rdUpdateStatus := TUpdateStatus(usInserted);
  PRecordData(Buffer)^.rdBookMarkFlag := bfInserted;
  PRecordData(Buffer)^.rdRecordNumber := -1;
end;

procedure TIBCustomDataSet.InternalInsert;
begin
  CursorPosChanged;
end;

{ TIBDataSet IProviderSupport }

procedure TIBCustomDataSet.PSEndTransaction(Commit: Boolean);
begin
  if Transaction.InTransaction then
  begin
    if Commit then
      Transaction.Commit
    else
      Transaction.Rollback;
  end;
end;

function TIBCustomDataSet.PSExecuteStatement(const ASQL: String; AParams: TParams): Integer;
var
  FQuery: TIBDataSet;
  i : Integer;
begin
    FQuery := TIBDataSet.Create(nil);
    try
      FQuery.Database := Database;
      FQuery.Transaction := Transaction;
      if not Transaction.InTransaction then
        Transaction.StartTransaction;
      FQuery.QSelect.GenerateParamNames := True;
      FQuery.SelectSQL.Text := ASQL;
      for i := 0 to AParams.Count - 1 do
        FQuery.Params[i].Value := AParams[i].Value;
      FQuery.ExecSQL;
      if FQuery.SQLType = SQLSelect then
      begin
        FQuery.FetchAll;
        Result := FQuery.RecordCount;
      end
      else
        Result := FQuery.RowsAffected;
    finally
      FQuery.Free;
    end;
  end;

function TIBCustomDataSet.PSExecuteStatement(const ASQL: String; AParams: TParams;
  var ResultSet: TDataSet): Integer;
var
  i : Integer;
begin
  ResultSet := TIBDataSet.Create(nil);

  TIBDataSet(ResultSet).Database := self.Database;
  TIBDataSet(ResultSet).Transaction := self.Transaction;
  if not TIBDataSet(ResultSet).Transaction.InTransaction then
    TIBDataSet(ResultSet).Transaction.StartTransaction;
  TIBDataSet(ResultSet).QSelect.GenerateParamNames := true;
  TIBDataSet(ResultSet).SelectSQL.Text := ASQL;
  for i := 0 to AParams.Count - 1 do
    TIBDataSet(ResultSet).Params[i].Value := AParams[i].Value;
  TIBDataSet(ResultSet).Open;
  if TIBDataSet(ResultSet).SQLType = SQLSelect then
  begin
    TIBDataSet(ResultSet).FetchAll;
    Result := TIBDataSet(ResultSet).RecordCount;
  end
  else
    Result := TIBDataSet(ResultSet).RowsAffected;
end;

function TIBCustomDataSet.PSGetQuoteChar: String;
begin
  Result := '';
  if Assigned(Database) and (Database.SQLDialect = 3) then
    Result := '"'
end;

function TIBCustomDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
var
  PrevErr: Integer;
begin
  if Prev <> nil then
    PrevErr := Prev.ErrorCode else
    PrevErr := 0;
  if E is EIBError then
    Result := EUpdateError.Create(E.Message, '', EIBError(E).SQLCode, PrevErr, E)
  else
      Result := inherited PSGetUpdateException(E, Prev);
end;

function TIBCustomDataSet.PSInTransaction: Boolean;
begin
  Result := Transaction.InTransaction;
end;

function TIBCustomDataSet.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

function TIBCustomDataSet.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

procedure TIBCustomDataSet.PSReset;
begin
  inherited PSReset;
  if Active then
  begin
    Close;
    Open;
  end;
end;

function TIBCustomDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
var
  UpdateAction: TIBUpdateAction;
  SQL: String;
  Params: TParams;

  procedure AssignParams(DataSet: TDataSet; Params: TParams);
  var
    I: Integer;
    Old: Boolean;
    Param: TParam;
    PName: string;
    Field: TField;
    Value: Variant;
  begin
    for I := 0 to Params.Count - 1 do
    begin
      Param := Params[I];
      PName := Param.Name;
      Old := PName.ToUpper.StartsWith('OLD_'); {do not localize}
      if Old then
        PName := PName.Remove(0, 4);
      Field := DataSet.FindField(PName);
      if not Assigned(Field) then
        Continue;
      if Old then
        Param.AssignFieldValue(Field, Field.OldValue)
      else
      begin
        Value := Field.NewValue;
        if VarIsEmpty(Value) then
          Value := Field.OldValue;
        Param.AssignFieldValue(Field, Value);
      end;
    end;
  end;

begin
  Result := False;
  if Assigned(OnUpdateRecord) then
  begin
    UpdateAction := uaFail;
    if Assigned(FOnUpdateRecord) then
    begin
      FOnUpdateRecord(Delta, UpdateKind, UpdateAction);
      Result := UpdateAction = uaApplied;
    end;
  end
  else if Assigned(FUpdateObject) then
  begin
    SQL := FUpdateObject.GetSQL(UpdateKind).Text;
    if SQL <> '' then
    begin
      Params := TParams.Create;
      try
        Params.ParseSQL(SQL, True);
        AssignParams(Delta, Params);
        if PSExecuteStatement(SQL, Params) = 0 then
          IBError(ibxeNoRecordsAffected, [nil]);
        Result := True;
      finally
        Params.Free;
      end;
    end;
  end;
end;

procedure TIBCustomDataSet.PSStartTransaction;
begin
  ActivateConnection;
  Transaction.StartTransaction;
end;

function TIBCustomDataSet.PSGetTableName: String;
var
  i : Integer;
begin
//  if not FInternalPrepared then
//    InternalPrepare;
  { It is possible for the FQSelectSQL to be unprepared
    with FInternalPreprepared being true (see DoBeforeTransactionEnd).
    So check the Prepared of the SelectSQL instead }
  if not FQSelect.Prepared then
    FQSelect.Prepare;
  Result := FQSelect.UniqueRelationName;
  If Result = '' then
  begin
    i := 0;
    while (i < FQSelect.FieldCount) and (Result = '') do
    begin
      Result := Trim(String(FQSelect.Current.Vars[i].SqlVar.RelName));
      Inc(i);
    end;
  end;
end;

procedure TIBDataSet.BatchInput(InputObject: TIBBatchInput);
begin
  InternalBatchInput(InputObject);
end;

procedure TIBDataSet.BatchOutput(OutputObject: TIBBatchOutput);
begin
  InternalBatchOutput(OutputObject);
end;

procedure TIBDataSet.ExecSQL;
begin
  InternalExecQuery;
  FRowsAffected := FQSelect.RowsAffected;
end;

procedure TIBDataSet.Prepare;
begin
  InternalPrepare;
end;

procedure TIBDataSet.UnPrepare;
begin
  InternalUnPrepare;
end;

function TIBDataSet.GetPrepared: Boolean;
begin
  Result := InternalPrepared;
end;

procedure TIBDataSet.InternalOpen;

  procedure SetParams(AParams : TParams);
  var
    i : Integer;
    Param : TIBXSQLVar;
    Buffer: TValueBuffer;
  begin
    for i := 0 to AParams.Count - 1 do
    begin
      Param := Params.ByName(AParams[i].Name);
      if AParams[i].IsNull then
        Param.IsNull := True
      else
      begin
        Param.IsNull := False;
        case AParams[i].DataType of
          ftBytes:
          begin
            SetLength(Buffer,AParams[i].GetDataSize);
            AParams[i].GetData(Buffer);
            Param.AsBytes := Buffer;
          end;
          ftString, ftFixedChar, ftWideString:
            Param.AsString := AParams[i].AsString;
          ftBoolean:
            Param.AsBoolean := AParams[i].AsBoolean;
          ftShortInt, ftSmallInt, ftWord, ftByte:
            Param.AsShort := AParams[i].AsSmallInt;
          ftInteger:
            Param.AsLong := AParams[i].AsInteger;
          ftLargeInt:
            Param.AsInt64 := AParams[i].Value;
          ftFloat:
           Param.AsDouble := AParams[i].AsFloat;
          ftBCD, ftCurrency:
            Param.AsCurrency := AParams[i].AsCurrency;
          ftFMTBCD :
            Param.AsBcd := AParams[i].AsFMTBCD;
          ftDate:
            Param.AsDate := AParams[i].AsDateTime;
          ftTime:
            Param.AsTime := AParams[i].AsDateTime;
          ftDateTime:
            Param.AsDateTime := AParams[i].AsDateTime;
          ftBlob, ftVarBytes :
            Param.AsBytes := AParams[i].AsBytes;
          ftMemo, ftWideMemo :
            Param.AsString := AParams[i].AsString;
          else
            IBError(ibxeNotSupported, [nil]);
        end;
      end;
    end;
  end;

begin
  ActivateConnection;
  ActivateTransaction;
  InternalSetParamsFromCursor;
  Inherited InternalOpen;
end;

procedure TIBDataSet.SetFiltered(Value: Boolean);
begin
  if (Filtered <> Value) then
  begin
    inherited SetFiltered(value);
    if Active then
    begin
      Close;
      Open;
    end;
  end
  else
    inherited SetFiltered(value);
end;

function TIBCustomDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  TempCurrent : long;
  Buff: TRecBuf;
begin
  Result := false;
  if not Assigned(Bookmark) then
    exit;
  Result := PInteger(Bookmark)^ < FRecordCount;
  // check that this is not a fully deleted record slot
  if Result then
  begin
    TempCurrent := FCurrentRecord;
    FCurrentRecord := PInteger(Bookmark)^;
    Buff := TRecBuf(ActiveBuffer);
    if (PRecordData(Buff)^.rdUpdateStatus = usDeleted) and
       (PRecordData(Buff)^.rdCachedUpdateStatus = cusUnmodified) then
      Result := false;
    FCurrentRecord := TempCurrent;
  end;
end;

procedure TIBCustomDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
var
  lTempCurr : System.Currency;
  lBuff: TValueBuffer;
begin
  if (Field.DataType = ftBCD) and (Buffer <> nil) then
  begin
    BCDToCurr(TDBBitConverter.UnsafeInto<TBcd>(Buffer), lTempCurr);
    SetLength(lBuff, SizeOf(System.Currency));
    Move(lTempCurr, lBuff[0], SizeOf(System.Currency));
    InternalSetFieldData(Field, lBuff);
  end
  else
    InternalSetFieldData(Field, Buffer);
end;

procedure TIBCustomDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer;
  NativeFormat: Boolean);
begin
  if (not NativeFormat) and (Field.DataType = ftBCD) then
    InternalSetfieldData(Field, Buffer)
  else
    inherited SetFieldData(Field, buffer, NativeFormat);
end;

procedure TIBCustomDataSet.DoOnNewRecord;

  procedure SetFieldsFromParams;
  var
    i : Integer;
    master_field, cur_field: TField;
  begin
    if (SQLParams.Count > 0) then
      for i := 0 to SQLParams.Count - 1 do
      begin
        master_field := FDataLink.DataSource.DataSet.FindField(SQLParams[i].Name);
        cur_field :=  FindField(SQLParams[i].Name);
        if (master_field <> nil) and (cur_field <> nil) and cur_field.IsNull then
        begin
          if (master_field.IsNull) then
            cur_field.Clear
          else
          case cur_field.DataType of
            ftBoolean, ftShortInt, ftSmallInt, ftWord, ftByte, ftInteger, ftString,
            ftFloat, ftCurrency, ftBCD, ftFMTBCD, ftDate, ftTime, ftDateTime,
            ftWideString, ftFixedChar, ftFixedWideChar:
              cur_field.Value := master_field.Value;
            ftLargeInt:
              cur_field.AsLargeInt := master_field.AsLargeInt;
          end;
        end;
      end;
  end;

begin
  if FGeneratorField.ApplyEvent = gamOnNewRecord then
    FGeneratorField.Apply;
  if CopyMasterFieldToDetail then
  begin
    if FDataLink.DataSource <> nil then
      if FDataLink.DataSource.DataSet <> nil then
        SetFieldsFromParams;
  end;
  inherited DoOnNewRecord;
end;

procedure TIBCustomDataSet.Post;
var
  i : Integer;
begin
  if (FGeneratorField.ApplyEvent = gamOnServer) and
      FGeneratorField.IsComplete then
    FieldByName(FGeneratorField.Field).Required := false;

  UpdateRecord;
  if State = dsInsert then
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      if (Fields[i].IsNull) and (Fields[i].DefaultExpression <> '') then
        Fields[i].Value := Fields[i].DefaultExpression;
    end;

    if FGeneratorField.ApplyEvent = gamOnPost then
      FGeneratorField.Apply;
  end;
  inherited Post;
end;

procedure TIBCustomDataSet.SetGeneratorField(
  const Value: TIBGeneratorField);
begin
  FGeneratorField.Assign(Value);
end;

procedure TIBCustomDataSet.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) and
     (not (csDesigning in ComponentState)) then
    FStreamedActive := Value
  else
    inherited SetActive(Value);
end;

procedure TIBCustomDataSet.Loaded;
begin
  if Assigned(FBase.Database) and
      (not FBase.Database.AllowStreamedConnected) and
      (not FBase.Database.Connected) and
       FStreamedActive then
    Active := false
  else
    if FStreamedActive then
      Active := true;
  inherited Loaded;
end;

function TIBCustomDataSet.Current: TIBXSQLDA;
begin
  if not FInternalPrepared then
    InternalPrepare;
  Result := FQSelect.Current;
end;

function TIBCustomDataSet.SQLType: TIBSQLTypes;
begin
  Result := FQSelect.SQLType;
end;

function TIBCustomDataSet.GetPlan: String;
begin
  Result := FQSelect.Plan;
end;

procedure TIBCustomDataSet.CreateFields;
var
  FieldAliasName, RelationName, FieldName : String;
  i : Integer;
  f : TField;
begin
  inherited;
  for i := 0 to FQSelect.Current.Count - 1 do
  begin
    { Get the field name }
    FieldAliasName := String(FQSelect.Current[i].Data.aliasname);
    RelationName := String(FQSelect.Current[i].Data.relname);
    FieldName := String(FQSelect.Current[i].Data.sqlname);
    f := FindField(FieldAliasname);
    if Assigned(f) then
    begin
      if (RelationName <> '') and (FieldName <> '') then
        f.Origin := QuoteIdentifier(FBase.Database.SQLDialect, RelationName) + '.' +
                    QuoteIdentifier(FBase.Database.SQLDialect, FieldName);
      if Database.In_Key(RelationName, FieldName) then
        f.ProviderFlags := f.ProviderFlags + [pfInKey]
      else
        if Database.Has_COMPUTED_BLR(RelationName, FieldName) or (Trim(RelationName) = '') then
          f.ProviderFlags := [];
      if f.IsBlob then
        f.ProviderFlags := f.ProviderFlags - [pfInWhere];
    end;
  end;
end;

procedure TIBCustomDataSet.OutputXML(OutputObject: TIBOutputXML);
begin
  QSelect.OutputXML(OutputObject);
end;

procedure TIBCustomDataSet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FUpdateObject) then
    FUpdateObject := nil;
end;

procedure TIBCustomDataSet.PSExecute;
begin
  FQSelect.ExecQuery;
end;

{ TIBDataSetUpdateObject }

constructor TIBDataSetUpdateObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRefreshSQL := TStringList.Create;
end;

destructor TIBDataSetUpdateObject.Destroy;
begin
  FRefreshSQL.Free;
  inherited Destroy;
end;

procedure TIBDataSetUpdateObject.SetRefreshSQL(Value: TStrings);
begin
  FRefreshSQL.Assign(Value);
end;

{ TIBDSBlobStream }

constructor TIBDSBlobStream.Create(AField: TField; ABlobStream: TIBBlobStream;
                                    Mode: TBlobStreamMode);

begin
  FModified := false;
  FField := AField;
  FBlobStream := ABlobStream;
  FBlobStream.Seek(0, soFromBeginning);
  if (Mode = bmWrite) then
    FBlobStream.Truncate;
  FConvertedPosition := 0;
end;

destructor TIBDSBlobStream.Destroy;
begin
  if FModified then
  begin
    FModified := false;
    if not TBlobField(FField).Modified then
      TBlobField(FField).Modified := True;
    TIBCustomDataSet(FField.DataSet).DataEvent(deFieldChange, NativeInt(FField));
  end;
  inherited Destroy;
end;

function TIBDSBlobStream.Read(var Buffer; Count: Longint): Longint;

  procedure DecodeStream;
  var
    ss : TBytesStream;
    bt : TBytes;
    s : String;
  begin
    ss := TBytesStream.Create;
    try
      FBlobStream.SaveToStream(ss);
      s := FBlobStream.Database.Encoding.GetString(ss.bytes, 0, ss.Size);
      bt := TEncoding.Unicode.GetBytes(s);
      Result := min(Length(bt) - FConvertedPosition, count);
      Move(bt[FConvertedPosition], Buffer, Result);
      FConvertedPosition := FConvertedPosition + Result;
    finally
      ss.free;
    end;
  end;

begin
  if FField is TWideMemoField then
    DecodeStream
  else
    result := FBlobStream.Read(Buffer, Count);
end;

function TIBDSBlobStream.Seek(Offset: Longint; Origin: Word): Longint;

  function DecodeLength : LongInt;
  var
    ss : TBytesStream;
    s : String;
  begin
    ss := TBytesStream.Create;
    try
      FBlobStream.SaveToStream(ss);
      s := FBlobStream.Database.Encoding.GetString(ss.bytes);
      Result := StrLen(PChar(s)) * sizeof(Char);
    finally
      ss.free;
    end;
  end;

begin
  if FField is TWideMemoField then
    Result := DecodeLength
  else
    result := FBlobStream.Seek(Offset, Origin);
end;

procedure TIBDSBlobStream.SetSize(NewSize: Longint);
begin
  FBlobStream.SetSize(NewSize);
end;

function TIBDSBlobStream.Write(const Buffer; Count: Longint): Longint;

  function DecodeLength : LongInt;
  var
    bt : TBytes;
  begin
    SetLength(bt, Count);
    Move(Buffer, bt[0], Count);
    bt := FBlobStream.Database.Encoding.Convert(TEncoding.Unicode, FBlobStream.Database.Encoding, bt);
    FBlobStream.Write(bt[0], Length(bt));
    Result := Count;
  end;

begin
  FModified := true;
  if not (FField.DataSet.State in [dsEdit, dsInsert]) then
    IBError(ibxeNotEditing, [nil]);
  TIBCustomDataSet(FField.DataSet).RecordModified(True);
  if FField is TWideMemoField then
    Result := DecodeLength
  else
    result := FBlobStream.Write(Buffer, Count);
end;

procedure TIBDataSet.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    SelectSQL.Text := CommandText;
end;

function TIBDataSet.ParamByName(Idx: String): TIBXSQLVAR;
begin
  if not FInternalPrepared then
    InternalPrepare;
  result := FQSelect.ParamByName(Idx);
end;

{ TGeneratorField }

procedure TIBGeneratorField.Apply;
const
  SGENSQL = 'SELECT GEN_ID(%s, %d) FROM RDB$DATABASE';  {do not localize}
var
  sqlGen : TIBSQL;
begin
  if IsComplete and (DataSet.FieldByName(Field).Value = Null) then
  begin
    sqlGen := TIBSQL.Create(Dataset.Database);
    sqlGen.Transaction := DataSet.Transaction;
    try
      sqlGen.SQL.Text := Format(SGENSQL, [QuoteIdentifier(DataSet.Database.SQLDialect, FGenerator), FIncrementBy]);
      sqlGen.ExecQuery;
      DataSet.FieldByName(Self.Field).AsLargeInt := sqlGen.Current.Vars[0].AsInt64;
      sqlGen.Close;
    finally
      sqlGen.Free;
    end;
  end;
end;

procedure TIBGeneratorField.Assign(Source: TPersistent);
var
  STemp : TIBGeneratorField;
begin
  if Source is TIBGeneratorField then
  begin
    STemp := Source as TIBGeneratorField;
    FField := STemp.Field;
    FGenerator := STemp.Generator;
    FIncrementBy := STemp.IncrementBy;
    FApplyEvent := STemp.ApplyEvent;
  end
  else
    inherited Assign(Source);
end;

constructor TIBGeneratorField.Create(ADataSet: TIBCustomDataSet);
begin
  inherited Create;
  FField := '';
  FGenerator := '';
  FIncrementBy := 1;
  FApplyEvent := gamOnNewRecord;
  DataSet := ADataSet;
end;

function TIBGeneratorField.IsComplete: Boolean;
begin
  Result := (FGenerator <> '') and (FField <> '');
end;

function TIBGeneratorField.ValueName: string;
begin
  if IsComplete then
    Result := FGenerator + ' -> ' + FField + ' By ' + IntToStr(FIncrementBy) {do not localize}
  else
    Result := '';
end;

destructor TIBDataSet.Destroy;
begin
  FreeAndNil(FPSParams);
  inherited;
end;

function TIBDataSet.PSGetParams: TParams;
begin
  CreateParams;
  Result := FPSParams;
end;

procedure TIBDataSet.PSSetParams(AParams: TParams);
var
  i : Integer;
  Param : TIBXSQLVar;
  Buffer: TValueBuffer;
begin
  // Note - We are copying the sent params to their IBX Param counterpart.
  //     There is no reason to use FBSParams elsewhere.  Copying the values
  //     to it in the end mainly for PSGetParam to have the values.
  //     Even this is not needed as calls to CreateParams copies the
  //     current values into the TParams.

  if (AParams.Count > 0) and (SelectSQL.Text <> '') then
  begin
    CreateParams;
    for i := 0 to AParams.Count - 1 do
    begin
      Param := Params.ByName(AParams[i].Name);
      if AParams[i].IsNull then
        Param.IsNull := True
      else
      begin
        Param.IsNull := False;
        case AParams[i].DataType of
          ftBytes:
          begin
            SetLength(Buffer,AParams[i].GetDataSize);
            AParams[i].GetData(Buffer);
            Param.AsBytes := Buffer;
          end;
          ftString, ftFixedChar, ftFixedWideChar, ftWideString, ftMemo, ftWideMemo:
            Param.AsString := AParams[i].AsString;
          ftBoolean:
            Param.AsBoolean := AParams[i].AsBoolean;
          ftShortInt, ftSmallInt, ftWord, ftByte:
            Param.AsShort := AParams[i].AsSmallInt;
          ftInteger:
            Param.AsLong := AParams[i].AsInteger;
          ftLargeInt:
            Param.AsInt64 := AParams[i].Value;
          ftFloat:
           Param.AsDouble := AParams[i].AsFloat;
          ftBCD, ftCurrency:
            Param.AsCurrency := AParams[i].AsCurrency;
          ftFMTBCD :
            Param.AsBcd := AParams[i].AsFMTBCD;
          ftDate:
            Param.AsDate := AParams[i].AsDateTime;
          ftTime:
            Param.AsTime := AParams[i].AsDateTime;
          ftDateTime:
            Param.AsDateTime := AParams[i].AsDateTime;
          ftBlob, ftVarBytes :
            Param.AsBytes := AParams[i].AsBytes;
          else
            IBError(ibxeNotSupported, [nil]);
        end;
      end;
    end;
    FPSParams.Assign(AParams);
  end;
end;

{ Used in the PSXxxx interface for Datasnap.  Creates a TParams internal variable
    if it does not exist (IBDataset uses IBSQLDA for its params as that is tighter
    to the IB API) and then creates and populates TParam's based on the
    values/types in Params.  This is only used in PSGetParams.  PSSetPatams
    actually updates the Params varaible and then copies into FBSParams the values
    sent jsut more for housekeeping pusposes.   }

procedure TIBDataSet.CreateParams;
var
  i : Integer;
  Param: TParam;
  DataType: TFieldType;
begin
  if not Assigned(FPSParams) then
    FPSParams := TParams.Create(Self);
  FPSParams.Clear;
  DataType := ftUnknown;
  if Trim(SelectSQL.Text) = '' then
    exit;
  for i := 0 to Params.Count - 1 do
  begin
    case Params[i].SQLtype of
      SQL_TYPE_DATE: DataType := ftDate;
      SQL_TYPE_TIME: DataType := ftTime;
      SQL_TIMESTAMP: DataType := ftDateTime;
      SQL_SHORT:
        if (Params[i].SQLVar.sqlscale = 0) then
          DataType := ftSmallInt
        else
          DataType := ftBCD;
      SQL_LONG:
        if (Params[i].SQLVar.sqlscale = 0) then
          DataType := ftInteger
        else if (Params[i].SQLVar.sqlscale >= (-4)) then
          DataType := ftBCD
        else
          if Database.SQLDialect = 1 then
            DataType := ftFloat
          else
            DataType := ftFMTBCD;
      SQL_INT64:
        if (Params[i].SQLVar.sqlscale = 0) then
          DataType := ftLargeInt
        else if (Params[i].SQLVar.sqlscale >= (-4)) then
          DataType := ftBCD
        else DataType := ftFMTBCD;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: DataType := ftFloat;
      SQL_TEXT: DataType := ftWideString;
      SQL_VARYING: DataType := ftWideString;
      SQL_BLOB, SQL_ARRAY, SQL_QUAD: DataType := ftBlob;
    end;
    Param := FPSParams.CreateParam(DataType, Trim(Params[i].Name), ptInput);
    if DataType <> ftBlob then
      Param.Value := Params[i].Value;
  end;
end;

function TIBDataSet.PSGetCommandText: String;
begin
  Result := SelectSQL.Text;
end;

function TIBDataSet.PSGetCommandType: TPSCommandType;
begin
  if not Prepared then
    Prepare;
  case SQLType of
    SQLSelect, SQLSelectForUpdate : Result := ctSelect;
    SQLInsert : Result := ctInsert;
    SQLUpdate : Result := ctUpdate;
    SQLDelete : Result := ctDelete;
    SQLDDL, SQLSetGenerator : Result := ctDDL;
    SQLExecProcedure : Result := ctStoredProc;
    else
      Result := ctUnknown;
  end;
end;

procedure InitializeDefaultFieldClasses;
begin
  DefaultFieldClasses := TDictionary<TFieldType, TFieldClass>.Create;

  DefaultFieldClasses.Add(ftUnknown, nil);
  DefaultFieldClasses.Add(ftString, TIBStringField);
  DefaultFieldClasses.Add(ftSmallint, TSmallintField);
  DefaultFieldClasses.Add(ftInteger, TIntegerField);
  DefaultFieldClasses.Add(ftWord, TWordField);
  DefaultFieldClasses.Add(ftBoolean, TBooleanField);
  DefaultFieldClasses.Add(ftFloat, TFloatField);
  DefaultFieldClasses.Add(ftCurrency, TCurrencyField);
  DefaultFieldClasses.Add(ftBCD, TIBBCDField);
  DefaultFieldClasses.Add(ftDate, TDateField);
  DefaultFieldClasses.Add(ftTime, TTimeField);
  DefaultFieldClasses.Add(ftDateTime, TDateTimeField);
  DefaultFieldClasses.Add(ftBytes, TBytesField);
  DefaultFieldClasses.Add(ftVarBytes, TVarBytesField);
  DefaultFieldClasses.Add(ftAutoInc, TAutoIncField);
  DefaultFieldClasses.Add(ftBlob, TBlobField);
  DefaultFieldClasses.Add(ftMemo, TWideMemoField);
  DefaultFieldClasses.Add(ftGraphic, TGraphicField);
  DefaultFieldClasses.Add(ftFmtMemo, TBlobField);
  DefaultFieldClasses.Add(ftParadoxOle, TBlobField);
  DefaultFieldClasses.Add(ftDBaseOle, TBlobField);
  DefaultFieldClasses.Add(ftTypedBinary, TBlobField);
  DefaultFieldClasses.Add(ftCursor, nil);
  DefaultFieldClasses.Add(ftFixedChar, TIBStringField);
  DefaultFieldClasses.Add(ftWideString, TIBStringField);
  DefaultFieldClasses.Add(ftLargeInt, TLargeIntField);
  DefaultFieldClasses.Add(ftADT, TADTField);
  DefaultFieldClasses.Add(ftArray, TArrayField);
  DefaultFieldClasses.Add(ftReference, TReferenceField);
  DefaultFieldClasses.Add(ftDataSet, TDataSetField);
  DefaultFieldClasses.Add(ftOraBlob, TBlobField);
  DefaultFieldClasses.Add(ftOraClob, TWideMemoField);
  DefaultFieldClasses.Add(ftVariant, TVariantField);
  DefaultFieldClasses.Add(ftInterface, TInterfaceField);
  DefaultFieldClasses.Add(ftIDispatch, TIDispatchField);
  DefaultFieldClasses.Add(ftGuid, TGuidField);
  DefaultFieldClasses.Add(ftTimeStamp, TSQLTimeStampField);
  DefaultFieldClasses.Add(ftFMTBcd, TFMTBcdField);
  DefaultFieldClasses.Add(ftFixedWideChar, nil);
  DefaultFieldClasses.Add(ftWideMemo, TWideMemoField);
  DefaultFieldClasses.Add(ftOraTimeStamp, nil);
  DefaultFieldClasses.Add(ftOraInterval, nil);
  DefaultFieldClasses.Add(ftLongWord, nil);
  DefaultFieldClasses.Add(ftShortint, nil);
  DefaultFieldClasses.Add(ftByte, nil);
  DefaultFieldClasses.Add(ftExtended, nil);
  DefaultFieldClasses.Add(ftConnection, nil);
  DefaultFieldClasses.Add(ftParams, nil);
  DefaultFieldClasses.Add(ftStream, nil);
  DefaultFieldClasses.Add(ftTimeStampOffset, nil);
  DefaultFieldClasses.Add(ftObject, nil);
  DefaultFieldClasses.Add(ftSingle, nil);
end;

initialization
  InitializeDefaultFieldClasses;

finalization
  DefaultFieldClasses.Free;
end.
