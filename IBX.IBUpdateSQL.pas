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

unit IBX.IBUpdateSQL;

interface

uses
  System.Classes, System.SysUtils, System.Variants,
  Data.DB, IBX.IB, IBX.IBCustomDataSet, IBX.IBQuery;

type
{ TIBUpdateSQL }

  {$ifdef compilerversion < 12}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid)]
  {$else}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidiOSDevice64 or pidAndroidArm32)]
  {$endif}
  TIBUpdateSQL = class(TIBDataSetUpdateObject)
  private
    FDataSet: TIBCustomDataSet;
    FQueries: array[TUpdateKind] of TIBQuery;
    FSQLText: array[TUpdateKind] of TStrings;

    function GetQuery(UpdateKind: TUpdateKind): TIBQuery;
    function GetSQL(UpdateKind: TUpdateKind): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SQLChanged(Sender: TObject);
    procedure InitializeQuery(UpdateKind: TUpdateKind);
  protected
    function GetDataSet: TIBCustomDataSet; override;
    procedure SetDataSet(ADataSet: TIBCustomDataSet); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Apply(UpdateKind: TUpdateKind); override;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    procedure SetParams(UpdateKind: TUpdateKind);

    property Query[UpdateKind: TUpdateKind]: TIBQuery read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
    property DataSet;
    property ModifySQL: TStrings index Ord(ukModify) read GetSQL write SetSQL;
    property InsertSQL: TStrings index Ord(ukInsert) read GetSQL write SetSQL;
    property DeleteSQL: TStrings index Ord(ukDelete) read GetSQL write SetSQL;
  end;

procedure Register;

implementation

{ TIBUpdateSQL }

constructor TIBUpdateSQL.Create(AOwner: TComponent);
var
  Kind: TUpdateKind;
begin
  inherited Create(AOwner);
  for Kind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[Kind] := TStringList.Create;
    TStringList(FSQLText[Kind]).OnChange := SQLChanged;
  end;
end;

destructor TIBUpdateSQL.Destroy;
var
  Kind: TUpdateKind;
begin
  if Assigned(FDataSet) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;

  for Kind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FreeAndNil(FQueries[Kind]);
    FreeAndNil(FSQLText[Kind]);
  end;

  inherited Destroy;
end;

function TIBUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TIBQuery;
begin
  if not Assigned(FQueries[UpdateKind]) then
    InitializeQuery(UpdateKind);
  Result := FQueries[UpdateKind];
end;

procedure TIBUpdateSQL.InitializeQuery(UpdateKind: TUpdateKind);
var
  Q: TIBQuery;
begin
  Q := TIBQuery.Create(Self);
  Q.SQL.Assign(FSQLText[UpdateKind]);

  if Assigned(FDataSet) then
  begin
    Q.Database := FDataSet.Database;
    Q.Transaction := FDataSet.Transaction;
  end;

  FQueries[UpdateKind] := Q;
end;

procedure TIBUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
var
  Q: TIBQuery;
begin
  Q := Query[UpdateKind];
  if not Assigned(Q) then
    Exit;

  Q.Prepare;
  Q.ExecSQL;

  if Q.RowsAffected <> 1 then
    IBError(ibxeUpdateFailed, [nil]);
end;

function TIBUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

procedure TIBUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TIBUpdateSQL.SQLChanged(Sender: TObject);
var
  Kind: TUpdateKind;
begin
  for Kind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    if Sender = FSQLText[Kind] then
    begin
      if Assigned(FQueries[Kind]) then
      begin
        FQueries[Kind].Params.Clear;
        FQueries[Kind].SQL.Assign(FSQLText[Kind]);
      end;
      Break;
    end;
  end;
end;

function TIBUpdateSQL.GetDataSet: TIBCustomDataSet;
begin
  Result := FDataSet;
end;

procedure TIBUpdateSQL.SetDataSet(ADataSet: TIBCustomDataSet);
begin
  FDataSet := ADataSet;
end;

procedure TIBUpdateSQL.SetParams(UpdateKind: TUpdateKind);
var
  I: Integer;
  Param: TParam;
  Field: TField;
  Value: Variant;
  FieldName: string;
  IsOld: Boolean;
begin
  if not Assigned(FDataSet) then
    Exit;

  for I := 0 to Query[UpdateKind].Params.Count - 1 do
  begin
    Param := Query[UpdateKind].Params[I];
    FieldName := Param.Name;
    IsOld := FieldName.ToUpper.StartsWith('OLD_');
    if IsOld then
      Delete(FieldName, 1, 4);

    Field := FDataSet.FindField(FieldName);
    if not Assigned(Field) then
      Continue;

    if IsOld then
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

procedure TIBUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  ExecSQL(UpdateKind);
end;

procedure Register;
begin
  RegisterComponents('InterBase', [TIBUpdateSQL]);
end;

end.
