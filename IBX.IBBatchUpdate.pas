{$A8} {$R-}
{*************************************************************}
{                                                             }
{       Embarcadero Delphi Visual Component Library           }
{       InterBase Express core components                     }
{                                                             }
{       Copyright (c) 1998-2015 Embarcadero                   }
{                                                             }
{    Additional code created by Jeff Overcash and used        }
{    with permission.                                         }
{*************************************************************}

unit IBX.IBBatchUpdate;

interface

uses System.Classes, IBX.IBDatabase, IBX.IBSQL, IBX.IBExternals,
     Generics.Collections;

type

  TIBBatchErrors = array of ISC_STATUS;

  TIBBatchError = class
  public
    Index : Integer;
    ErrorCode : ISC_Status;
  end;

  TIBBatchErrorList = TList<TIBBatchError>;

  TIBBatchErrorsEnumerator = class
  private
    FIndex : Integer;
    FErrors : TIBBatchErrorList;
  public
    constructor Create(Errors: TIBBatchErrorList);
    function GetCurrent: TIBBatchError; inline;
    function MoveNext: Boolean;
    property Current: TIBBatchError read GetCurrent;
  end;

  {$ifdef compilerversion < 12}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid)]
  {$else}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidiOSDevice64 or pidAndroidArm32)]
  {$endif}

  TIBBatchUpdate = class(TComponent)
  private
    FBatchCount: Integer;
    [weak] FTransaction: TIBTransaction;
    [weak] FDatabase: TIBDatabase;
    FBatchSQL : TIBSQL;
    FIBBatchErrorList : TIBBatchErrorList;
    function GetSQLParams: TIBXSQLDA;
    procedure SetBatchCount(const Value: Integer);
    function GetHasErrors: Boolean;
    procedure SetDatabase(const Value: TIBDatabase);
    procedure SetTransaction(const Value: TIBTransaction);
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute : Boolean;
    procedure Prepare;
    procedure Unprepare;
    procedure First;
    procedure Next;
    procedure Prior;
    procedure Insert;
    procedure Post;
    procedure Clear;
    property Params: TIBXSQLDA read GetSQLParams;
    property HasErrors : Boolean read GetHasErrors;
    function GetEnumerator: TIBBatchErrorsEnumerator;
  published
    property SQL : TStrings read GetSQL write SetSQL;
    property Database : TIBDatabase read FDatabase write SetDatabase;
    property Transaction : TIBTransaction read FTransaction write SetTransaction;
    [ default(1000)]
    property BatchCount : Integer read FBatchCount write SetBatchCount;
  end;

  procedure Register;

implementation

uses IBX.IBHeader, IBX.IBIntf;

{ TIBBatchUpdate }

procedure TIBBatchUpdate.Clear;
begin

end;

constructor TIBBatchUpdate.Create(AOwner: TComponent);
begin
  inherited;
  FBatchSQL := TIBSQL.Create(Self);
  FBatchCount := 1000;
  FIBBatchErrorList := TIBBatchErrorList.Create;
end;

destructor TIBBatchUpdate.Destroy;
begin
  FBatchSQL.Free;
  FIBBatchErrorList.Free;
  inherited;
end;

function TIBBatchUpdate.Execute: Boolean;
begin
  Result := false;
end;

procedure TIBBatchUpdate.First;
begin

end;

function TIBBatchUpdate.GetEnumerator: TIBBatchErrorsEnumerator;
begin
  Result := TIBBatchErrorsEnumerator.Create(FIBBatchErrorList);
end;

function TIBBatchUpdate.GetHasErrors: Boolean;
begin
  Result := FIBBatchErrorList.Count > 0;
end;

function TIBBatchUpdate.GetSQL: TStrings;
begin
  Result := FBatchSQL.SQL;
end;

function TIBBatchUpdate.GetSQLParams: TIBXSQLDA;
begin
  Result := FBatchSQL.Params;
end;

procedure TIBBatchUpdate.Insert;
begin

end;

procedure TIBBatchUpdate.Next;
begin

end;

procedure TIBBatchUpdate.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification( AComponent, Operation);
  if (Operation = opRemove) then
    if (AComponent = FDatabase) then
      FDatabase := nil
    else
      if (AComponent = FTransaction) then
        FTransaction := nil;
end;

procedure TIBBatchUpdate.Post;
begin

end;

procedure TIBBatchUpdate.Prepare;
begin
  FBatchSQL.Prepare;
end;

procedure TIBBatchUpdate.Prior;
begin

end;

procedure TIBBatchUpdate.SetBatchCount(const Value: Integer);
begin
  FBatchCount := Value;
end;

procedure TIBBatchUpdate.SetDatabase(const Value: TIBDatabase);
begin
  if (Value <> FDatabase) then
  begin
    if Assigned(FDatabase) then
      FDatabase.RemoveFreeNotification(self);
    FDatabase := Value;
    if Assigned(FDatabase) then
      FDatabase.FreeNotification(self);
  end;

  if Assigned(Value) and
     not Assigned(FTransaction) and
     Assigned(Value.DefaultTransaction) then
    Transaction := Value.DefaultTransaction;
  FBatchSQL.Database := Value;

end;

procedure TIBBatchUpdate.SetSQL(const Value: TStrings);
begin
  if FBatchSQL.SQL.Text <> Value.Text then
  begin
                        
    FBatchSQL.SQL.Assign(Value);
  end;
end;

procedure TIBBatchUpdate.SetTransaction(const Value: TIBTransaction);
begin
  if (Value <> FTransaction) then
  begin
    if Assigned(FTransaction) then
      FTransaction.RemoveFreeNotification(self);
    FTransaction := Value;
    if Assigned(FTransaction) then
      FTransaction.FreeNotification(self);
  end;

  if Assigned(Value) and
     not Assigned(FDatabase) and
     Assigned(Value.DefaultDatabase) then
    Database := Value.DefaultDatabase;
  FBatchSQL.Transaction := Value;
end;

procedure TIBBatchUpdate.Unprepare;
begin
  FBatchSQL.Unprepare;
end;

{ TIBBatchErrorsEnumerator }

constructor TIBBatchErrorsEnumerator.Create(Errors: TIBBatchErrorList);
begin
  inherited Create;
  FIndex := -1;
  FErrors := Errors;
end;

function TIBBatchErrorsEnumerator.GetCurrent: TIBBatchError;
begin
  Result := FErrors.Items[FIndex];
end;

function TIBBatchErrorsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FErrors.Count - 1;
  if Result then
    Inc(FIndex);
end;

procedure Register;
begin

  // componentes
  RegisterComponents('Interbase', [TIBBatchUpdate]);

end;

end.
