{$A8} {$R-}
{*************************************************************}
{                                                             }
{       Embarcadero Delphi Visual Component Library           }
{       InterBase Express core components                     }
{                                                             }
{       Copyright (c) 1998-2015 Embarcadero                   }
{       Refactored and improved version                       }
{                                                             }
{*************************************************************}

unit IBX.IBStoredProc;

interface

uses 
  System.Classes, 
  System.Generics.Collections,
  System.SysUtils,
  Data.DB,
  IBX.IB,
  IBX.IBDatabase,
  IBX.IBCustomDataSet,
  IBX.IBHeader, IBX.IBSQL;

type
  // Exception classes for better error handling
  EIBStoredProcException = class(Exception);
  EIBStoredProcNotPrepared = class(EIBStoredProcException);
  EIBStoredProcInvalidName = class(EIBStoredProcException);

  // Enums for better type safety
  TIBProcParamDirection = (pdInput, pdOutput, pdInputOutput);
  TIBProcState = (psIdle, psPreparing, psPrepared, psExecuting);

  // Forward declaration
  TIBStoredProc = class;
  
  // Custom parameter information class
  TIBProcParamInfo = class
  private
    FName: string;
    FDirection: TIBProcParamDirection;
    FDataType: TFieldType;
    FSize: Integer;
    FPrecision: Integer;
    FScale: Integer;
  public
    constructor Create(const AName: string; ADirection: TIBProcParamDirection; 
      ADataType: TFieldType; ASize: Integer = 0; APrecision: Integer = 0; AScale: Integer = 0);
    
    property Name: string read FName;
    property Direction: TIBProcParamDirection read FDirection;
    property DataType: TFieldType read FDataType;
    property Size: Integer read FSize;
    property Precision: Integer read FPrecision;
    property Scale: Integer read FScale;
  end;

  // Main stored procedure class
  {$ifdef compilerversion < 12}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid)]
  {$else}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidiOSDevice64 or pidAndroidArm32)]
  {$endif}
  TIBStoredProc = class(TIBCustomDataSet)
  private
    // Core fields
    FStmtHandle: TISC_STMT_HANDLE;
    FProcName: string;
    FParams: TParams;
    FState: TIBProcState;
    FParamInfoCache: TObjectList<TIBProcParamInfo>;
    FProcNamesCache: TStringList;
    FCacheValid: Boolean;
    
    // Property getters/setters
    function GetPrepared: Boolean;
    function GetParams: TParams;
    function GetParamsCount: Word;
    function GetStoredProcedureNames: TStrings;
    procedure SetProcName(const Value: string);
    procedure SetParamsList(const Value: TParams);
    procedure SetPrepared(const Value: Boolean);
    
    // Internal methods - reorganized for better clarity
    procedure ValidateState(const AllowedStates: array of TIBProcState; const Operation: string);
    procedure ValidateProcName;
    procedure ValidateDatabase;
    procedure ValidateConnection;
    
    // Statement management
    procedure InternalPrepare;reintroduce;
    procedure InternalUnprepare;reintroduce;
    procedure FreeStatement;
    
    // Parameter handling
    procedure BuildParameterInfo;
    procedure CreateParameterDescriptors;
    procedure MapParametersToSQL;
    procedure ExtractOutputParameters;
    procedure ValidateInputParameters;
    
    // SQL generation and metadata
    procedure GenerateExecuteSQL;
    procedure LoadStoredProcedureNames;
    function BuildParameterList: string;
    
    // Data type mapping
    function MapSQLTypeToFieldType(SQLType: Integer; Scale: Integer): TFieldType;
    function GetParameterDirection(ParamType: Integer): TIBProcParamDirection;
    
    // Serialization support
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    procedure AssignParamValue(SQLParam: TIBXSQLVAR; Param: TParam);
    procedure MapParametersFromDataSource;
    
  protected
    // IProviderSupport interface
    procedure PSExecute; override;
    function PSGetTableName: String; override;
    function PSGetParams: TParams; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    function PSGetCommandText: String; override;
    function PSGetCommandType: TPSCommandType; override;

    // Component lifecycle
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure Disconnect; override;
    procedure InternalOpen; override;
    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    // Public interface methods
    procedure Prepare;
    procedure Unprepare;
    procedure Execute;
    procedure CopyParams(const DestParams: TParams);
    function ParamByName(const ParamName: string): TParam;
    procedure RefreshParameterInfo;
    procedure ClearParameterCache;
    
    // Properties
    property ParamCount: Word read GetParamsCount;
    property StmtHandle: TISC_STMT_HANDLE read FStmtHandle;
    property Prepared: Boolean read GetPrepared write SetPrepared;
    property StoredProcedureNames: TStrings read GetStoredProcedureNames;
    property State: TIBProcState read FState;

  published
    // Published properties
    property StoredProcName: string read FProcName write SetProcName;
    property Params: TParams read GetParams write SetParamsList stored false;
    property Filtered;

    // Events
    property BeforeDatabaseDisconnect;
    property AfterDatabaseDisconnect;
    property DatabaseFree;
    property BeforeTransactionEnd;
    property AfterTransactionEnd;
    property TransactionFree;
    property OnFilterRecord;
  end;

procedure Register;

implementation

uses
  System.TypInfo,
  IBX.IBUtils;

const
  // SQL constants for metadata queries
  SQL_GET_PROC_PARAMS = 
    'SELECT RDB$PARAMETER_NAME, RDB$PARAMETER_TYPE, RDB$FIELD_TYPE, ' +
    'RDB$FIELD_LENGTH, RDB$FIELD_PRECISION, RDB$FIELD_SCALE ' +
    'FROM RDB$PROCEDURE_PARAMETERS PP ' +
    'JOIN RDB$FIELDS F ON PP.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME ' +
    'WHERE RDB$PROCEDURE_NAME = ? ' +
    'ORDER BY RDB$PARAMETER_NUMBER';
    
  SQL_GET_PROC_NAMES = 
    'SELECT RDB$PROCEDURE_NAME FROM RDB$PROCEDURES ' +
    'ORDER BY RDB$PROCEDURE_NAME';

{ TIBProcParamInfo }

constructor TIBProcParamInfo.Create(const AName: string; 
  ADirection: TIBProcParamDirection; ADataType: TFieldType; 
  ASize, APrecision, AScale: Integer);
begin
  inherited Create;
  FName := AName;
  FDirection := ADirection;
  FDataType := ADataType;
  FSize := ASize;
  FPrecision := APrecision;
  FScale := AScale;
end;

{ TIBStoredProc }

constructor TIBStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TParams.Create(Self);
  FParamInfoCache := TObjectList<TIBProcParamInfo>.Create(True);
  FProcNamesCache := TStringList.Create;
  FProcNamesCache.Sorted := True;
  FProcNamesCache.Duplicates := dupIgnore;
  FState := psIdle;
  FCacheValid := False;
end;

destructor TIBStoredProc.Destroy;
begin
  try
    Destroying;
    Disconnect;
  finally
    FreeAndNil(FParams);
    FreeAndNil(FParamInfoCache);
    FreeAndNil(FProcNamesCache);
    inherited Destroy;
  end;
end;

procedure TIBStoredProc.ValidateState(const AllowedStates: array of TIBProcState; 
  const Operation: string);
var
  I: Integer;
  StateFound: Boolean;
begin
  StateFound := False;
  for I := Low(AllowedStates) to High(AllowedStates) do
  begin
    if FState = AllowedStates[I] then
    begin
      StateFound := True;
      Break;
    end;
  end;
  
  if not StateFound then
    raise EIBStoredProcException.CreateFmt(
      'Cannot perform operation "%s" in current state. Current state: %s',
      [Operation, GetEnumName(TypeInfo(TIBProcState), Ord(FState))]);
end;

procedure TIBStoredProc.ValidateProcName;
begin
  if Trim(FProcName) = '' then
    raise EIBStoredProcInvalidName.Create('Stored procedure name cannot be empty');
end;

procedure TIBStoredProc.ValidateDatabase;
begin
  if not Assigned(Database) then
    raise EIBStoredProcException.Create('Database connection is required');
end;

procedure TIBStoredProc.ValidateConnection;
begin
  ValidateDatabase;
  if not Database.Connected then
    raise EIBStoredProcException.Create('Database must be connected');
end;

function TIBStoredProc.GetPrepared: Boolean;
begin
  Result := (FState = psPrepared);
end;

procedure TIBStoredProc.SetPrepared(const Value: Boolean);
begin
  if GetPrepared <> Value then
  begin
    if Value then
      Prepare
    else
      Unprepare;
  end;
end;

procedure TIBStoredProc.Prepare;
begin
  ValidateState([psIdle], 'Prepare');
  ValidateProcName;
  ValidateConnection;
  
  try
    FState := psPreparing;
    InternalPrepare;
    FState := psPrepared;
  except
    FState := psIdle;
    FreeStatement;
    raise;
  end;
end;

procedure TIBStoredProc.Unprepare;
begin
  if FState in [psPrepared, psPreparing] then
  begin
    InternalUnprepare;
    FState := psIdle;
  end;
end;

procedure TIBStoredProc.InternalPrepare;
begin
  if SelectSQL.Text = '' then
    GenerateExecuteSQL;
    
  inherited InternalPrepare;
  
  if FParams.Count = 0 then
    CreateParameterDescriptors;
end;

procedure TIBStoredProc.InternalUnprepare;
begin
  FreeStatement;
end;

procedure TIBStoredProc.FreeStatement;
begin
  try
    inherited InternalUnPrepare;
    if Assigned(QSelect) then
      QSelect.FreeHandle;
  except
    // Swallow exceptions during cleanup
  end;
end;

procedure TIBStoredProc.Execute;
var
  DidActivateTransaction: Boolean;
begin
  ValidateState([psPrepared], 'Execute');
  CheckInActive;
  
  ActivateConnection;
  DidActivateTransaction := ActivateTransaction;
  
  try
    FState := psExecuting;
    
    // Set parameters from data source if available
    if Assigned(DataSource) and Assigned(DataSource.DataSet) then
      MapParametersFromDataSource;
      
    ValidateInputParameters;
    MapParametersToSQL;
    
    // Execute the procedure
    InternalExecQuery;
    
    // Extract output parameters
    ExtractOutputParameters;
    
  finally
    FState := psPrepared;
    if DidActivateTransaction then
      DeactivateTransaction;
  end;
end;

procedure TIBStoredProc.GenerateExecuteSQL;
var
  ParamList: string;
begin
  ValidateConnection;
  
  BuildParameterInfo;
  ParamList := BuildParameterList;
  
  if ParamList <> '' then
    SelectSQL.Text := Format('EXECUTE PROCEDURE %s (%s)', 
      [QuoteIdentifier(Database.SQLDialect, FProcName), ParamList])
  else
    SelectSQL.Text := Format('EXECUTE PROCEDURE %s', 
      [QuoteIdentifier(Database.SQLDialect, FProcName)]);
end;

procedure TIBStoredProc.BuildParameterInfo;
var
  Query: TIBSQL;
  ParamInfo: TIBProcParamInfo;
  ParamName: string;
  ParamType: Integer;
  FieldType: Integer;
  FieldLength: Integer;
  FieldPrecision: Integer;
  FieldScale: Integer;
  Direction: TIBProcParamDirection;
  DataType: TFieldType;
begin
  FParamInfoCache.Clear;
  
  ActivateConnection;
  Database.InternalTransaction.StartTransaction;
  Query := TIBSQL.Create(Self);
  try
    Query.Database := Database;
    Query.Transaction := Database.InternalTransaction;
    Query.SQL.Text := SQL_GET_PROC_PARAMS;
    Query.Prepare;
    Query.ParamByName('RDB$PROCEDURE_NAME').AsString := 
      FormatIdentifierValue(Database.SQLDialect, FProcName);
    Query.ExecQuery;
    
    while not Query.EOF do
    begin
      ParamName := Trim(Query.FieldByName('RDB$PARAMETER_NAME').AsString);
      ParamType := Query.FieldByName('RDB$PARAMETER_TYPE').AsInteger;
      FieldType := Query.FieldByName('RDB$FIELD_TYPE').AsInteger;
      FieldLength := Query.FieldByName('RDB$FIELD_LENGTH').AsInteger;
      FieldPrecision := Query.FieldByName('RDB$FIELD_PRECISION').AsInteger;
      FieldScale := Query.FieldByName('RDB$FIELD_SCALE').AsInteger;
      
      Direction := GetParameterDirection(ParamType);
      DataType := MapSQLTypeToFieldType(FieldType, FieldScale);
      
      ParamInfo := TIBProcParamInfo.Create(ParamName, Direction, DataType, 
        FieldLength, FieldPrecision, FieldScale);
      FParamInfoCache.Add(ParamInfo);
      
      Query.Next;
    end;
  finally
    Query.Free;
    Database.InternalTransaction.Commit;
  end;
end;

function TIBStoredProc.BuildParameterList: string;
var
  I: Integer;
  ParamInfo: TIBProcParamInfo;
  ParamNames: TStringList;
begin
  Result := '';
  ParamNames := TStringList.Create;
  try
    for I := 0 to FParamInfoCache.Count - 1 do
    begin
      ParamInfo := FParamInfoCache[I];
      if ParamInfo.Direction in [pdInput, pdInputOutput] then
        ParamNames.Add(':' + QuoteIdentifier(Database.SQLDialect, ParamInfo.Name));
    end;
    Result := ParamNames.CommaText;
  finally
    ParamNames.Free;
  end;
end;

procedure TIBStoredProc.CreateParameterDescriptors;
var
  I: Integer;
  ParamInfo: TIBProcParamInfo;
  ParamType: TParamType;
begin
  FParams.Clear;
  
  for I := 0 to FParamInfoCache.Count - 1 do
  begin
    ParamInfo := FParamInfoCache[I];
    
    case ParamInfo.Direction of
      pdInput: ParamType := ptInput;
      pdOutput: ParamType := ptOutput;
      pdInputOutput: ParamType := ptInputOutput;
    else
      ParamType := ptUnknown;
    end;
    
    FParams.CreateParam(ParamInfo.DataType, ParamInfo.Name, ParamType);
  end;
end;

function TIBStoredProc.MapSQLTypeToFieldType(SQLType, Scale: Integer): TFieldType;
begin
  case SQLType of
    SQL_TYPE_DATE: Result := ftDate;
    SQL_TYPE_TIME: Result := ftTime;
    SQL_TIMESTAMP: Result := ftDateTime;
    SQL_SHORT:
      if Scale = 0 then
        Result := ftSmallInt
      else
        Result := ftBCD;
    SQL_LONG:
      if Scale = 0 then
        Result := ftInteger
      else if Scale >= -4 then
        Result := ftBCD
      else
        Result := ftFloat;
    SQL_INT64:
      if Scale = 0 then
        Result := ftLargeInt
      else if Scale >= -4 then
        Result := ftBCD
      else
        Result := ftFloat;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: 
      Result := ftFloat;
    SQL_TEXT, SQL_VARYING: 
      Result := ftWideString;
    SQL_BLOB, SQL_ARRAY, SQL_QUAD: 
      Result := ftBlob;
    SQL_BOOLEAN: 
      Result := ftBoolean;
  else
    Result := ftUnknown;
  end;
end;

function TIBStoredProc.GetParameterDirection(ParamType: Integer): TIBProcParamDirection;
begin
  case ParamType of
    0: Result := pdInput;
    1: Result := pdOutput;
  else
    Result := pdInput; // Default to input
  end;
end;

procedure TIBStoredProc.ValidateInputParameters;
var
  I: Integer;
  Param: TParam;
begin
  for I := 0 to FParams.Count - 1 do
  begin
    Param := FParams[I];
    if (Param.ParamType = ptInput) and not Param.Bound then
      raise EIBStoredProcException.CreateFmt(
        'Required input parameter "%s" is not set', [Param.Name]);
  end;
end;

procedure TIBStoredProc.MapParametersToSQL;
var
  I, SQLParamIndex: Integer;
  Param: TParam;
begin
  SQLParamIndex := 0;
  
  for I := 0 to FParams.Count - 1 do
  begin
    Param := FParams[I];
    if Param.ParamType <> ptInput then
      Continue;
      
    if Param.IsNull then
      SQLParams[SQLParamIndex].IsNull := True
    else
      AssignParamValue(SQLParams[SQLParamIndex], Param);
      
    Inc(SQLParamIndex);
  end;
end;

procedure TIBStoredProc.AssignParamValue(SQLParam: TIBXSQLVAR; Param: TParam);
begin
  SQLParam.IsNull := False;
  
  case Param.DataType of
    ftString, ftFixedChar, ftFixedWideChar, ftWideString:
      SQLParam.AsString := Param.AsString;
    ftShortInt, ftSmallint, ftWord, ftByte:
      SQLParam.AsShort := Param.AsSmallInt;
    ftInteger:
      SQLParam.AsLong := Param.AsInteger;
    ftLargeInt:
      SQLParam.AsInt64 := Param.Value;
    ftFloat, ftCurrency:
      SQLParam.AsDouble := Param.AsFloat;
    ftBCD:
      SQLParam.AsCurrency := Param.AsCurrency;
    ftDate:
      SQLParam.AsDate := Param.AsDateTime;
    ftTime:
      SQLParam.AsTime := Param.AsDateTime;
    ftDateTime:
      SQLParam.AsDateTime := Param.AsDateTime;
    ftMemo, ftWideMemo:
      SQLParam.AsString := Param.AsString;
    ftBlob, ftVarBytes:
      SQLParam.AsBytes := Param.AsBytes;
    ftBoolean:
      SQLParam.AsBoolean := Param.AsBoolean;
  else
    raise EIBStoredProcException.CreateFmt(
      'Unsupported parameter data type for parameter "%s"', [Param.Name]);
  end;
end;

procedure TIBStoredProc.ExtractOutputParameters;
var
  I, FieldIndex: Integer;
  Param: TParam;
begin
  FieldIndex := 0;
  
  for I := 0 to FParams.Count - 1 do
  begin
    Param := FParams[I];
    if Param.ParamType = ptOutput then
    begin
      if FieldIndex < QSelect.Current.Count then
      begin
        if QSelect.Fields[FieldIndex].SQLType = SQL_BLOB then
          Param.Value := QSelect.Fields[FieldIndex].AsBytes
        else
          Param.Value := QSelect.Fields[FieldIndex].Value;
      end;
      Inc(FieldIndex);
    end;
  end;
end;

procedure TIBStoredProc.MapParametersFromDataSource;
var
  I: Integer;
  Param: TParam;
  DataSet: TDataSet;
  Field: TField;
begin
  if not Assigned(DataSource) or not Assigned(DataSource.DataSet) then
    Exit;
    
  DataSet := DataSource.DataSet;
  DataSet.FieldDefs.Update;
  
  for I := 0 to FParams.Count - 1 do
  begin
    Param := FParams[I];
    if (not Param.Bound) and (Param.ParamType in [ptInput, ptInputOutput]) then
    begin
      Field := DataSet.FindField(Param.Name);
      if Assigned(Field) then
        Param.AssignField(Field);
    end;
  end;
end;

// Property implementations
function TIBStoredProc.GetParams: TParams;
begin
  if not (csDesigning in ComponentState) and 
     (FProcName <> '') and 
     Assigned(Database) and 
     not GetPrepared then
    SetPrepared(True);
  Result := FParams;
end;

function TIBStoredProc.GetParamsCount: Word;
begin
  if not (csDesigning in ComponentState) and 
     (FProcName <> '') and 
     Assigned(Database) and 
     not GetPrepared then
    SetPrepared(True);
  Result := FParams.Count;
end;

procedure TIBStoredProc.SetProcName(const Value: string);
begin
  if not (csReading in ComponentState) then
  begin
    CheckInactive;
    if Value <> FProcName then
    begin
      FProcName := Value;
      Unprepare;
      FParams.Clear;
      FCacheValid := False;
      
      if (Value <> '') and Assigned(Database) then
        GenerateExecuteSQL;
    end;
  end
  else
  begin
    FProcName := Value;
    if (Value <> '') and Assigned(Database) then
      GenerateExecuteSQL;
  end;
end;

procedure TIBStoredProc.SetParamsList(const Value: TParams);
begin
  CheckInactive;
  if GetPrepared then
  begin
    SetPrepared(False);
    FParams.Assign(Value);
    SetPrepared(True);
  end
  else
    FParams.Assign(Value);
end;

function TIBStoredProc.ParamByName(const ParamName: string): TParam;
begin
  if not GetPrepared and (FParams.Count = 0) then
    Prepare;
  Result := FParams.ParamByName(ParamName);
end;

procedure TIBStoredProc.CopyParams(const DestParams: TParams);
begin
  if not GetPrepared and (FParams.Count = 0) then
  begin
    try
      Prepare;
      DestParams.Assign(FParams);
    finally
      Unprepare;
    end;
  end
  else
    DestParams.Assign(FParams);
end;

function TIBStoredProc.GetStoredProcedureNames: TStrings;
begin
  if not FCacheValid then
    LoadStoredProcedureNames;
  Result := FProcNamesCache;
end;

procedure TIBStoredProc.LoadStoredProcedureNames;
var
  Query: TIBSQL;
begin
  if csReading in ComponentState then
    Exit;
    
  FProcNamesCache.Clear;
  
  try
    ValidateConnection;
    
    Database.InternalTransaction.StartTransaction;
    Query := TIBSQL.Create(Self);
    try
      Query.GoToFirstRecordOnExecute := False;
      Query.Database := Database;
      Query.Transaction := Database.InternalTransaction;
      Query.SQL.Text := SQL_GET_PROC_NAMES;
      Query.Prepare;
      Query.ExecQuery;
      
      while not Query.EOF do
      begin
        FProcNamesCache.Add(Trim(Query.FieldByName('RDB$PROCEDURE_NAME').AsString));
        Query.Next;
      end;
      
      FCacheValid := True;
    finally
      Query.Free;
      Database.InternalTransaction.Commit;
    end;
  except
    // If we can't load procedure names, just leave the cache empty
    FCacheValid := False;
  end;
end;

procedure TIBStoredProc.RefreshParameterInfo;
begin
  FCacheValid := False;
  if GetPrepared then
  begin
    Unprepare;
    Prepare;
  end;
end;

procedure TIBStoredProc.ClearParameterCache;
begin
  FCacheValid := False;
  FParamInfoCache.Clear;
  FProcNamesCache.Clear;
end;

// Component lifecycle methods
procedure TIBStoredProc.Disconnect;
begin
  Close;
  Unprepare;
end;

procedure TIBStoredProc.SetFiltered(Value: Boolean);
begin
  if Filtered <> Value then
  begin
    inherited SetFiltered(Value);
    if Active then
    begin
      Close;
      Open;
    end;
  end
  else
    inherited SetFiltered(Value);
end;

procedure TIBStoredProc.InternalOpen;
begin
  raise EIBStoredProcException.Create('Cannot open a stored procedure as a dataset. Use Execute instead.');
end;

// Serialization support
procedure TIBStoredProc.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := not FParams.IsEqual(TIBStoredProc(Filer.Ancestor).FParams)
    else
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

procedure TIBStoredProc.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

procedure TIBStoredProc.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Params);
end;

// IProviderSupport implementation
function TIBStoredProc.PSGetParams: TParams;
begin
  Result := Params;
end;

procedure TIBStoredProc.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Params.Assign(AParams);
  Close;
end;

function TIBStoredProc.PSGetTableName: String;
begin
  Result := '';
end;

procedure TIBStoredProc.PSExecute;
begin
  Execute;
end;

procedure TIBStoredProc.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    StoredProcName := CommandText;
end;

function TIBStoredProc.PSGetCommandText: String;
begin
  Result := StoredProcName;
end;

function TIBStoredProc.PSGetCommandType: TPSCommandType;
begin
  Result := ctStoredProc;
end;

// Component registration
procedure Register;
begin
  RegisterComponents('Interbase', [TIBStoredProc]);
end;

end.