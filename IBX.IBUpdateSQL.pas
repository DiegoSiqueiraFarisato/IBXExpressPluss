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
  System.Classes, Data.DB, IBX.IB, IBX.IBCustomDataSet, IBX.IBQuery;

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
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);
  protected
    function GetSQL(UpdateKind: TUpdateKind): TStrings; override;
    function GetDataSet: TIBCustomDataSet; override;
    procedure SetDataSet(ADataSet: TIBCustomDataSet); override;
    procedure SQLChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind); override;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    procedure SetParams(UpdateKind: TUpdateKind);
    property DataSet;
    property Query[UpdateKind: TUpdateKind]: TIBQuery read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
  end;

  procedure Register;
implementation

uses
  System.SysUtils, System.Variants;

{ TIBUpdateSQL }

constructor TIBUpdateSQL.Create(AOwner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  inherited Create(AOwner);
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind] := TStringList.Create;
    TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
  end;
end;

destructor TIBUpdateSQL.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  if Assigned(FDataSet) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
  inherited Destroy;
end;

procedure TIBUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
begin
  Query[UpdateKind].Prepare;
  Query[UpdateKind].ExecSQL;
  if Query[UpdateKind].RowsAffected <> 1 then
    IBError(ibxeUpdateFailed, [nil]);
end;

function TIBUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TIBQuery;
begin
  if not Assigned(FQueries[UpdateKind]) then
  begin
    FQueries[UpdateKind] := TIBQuery.Create(Self);
    FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
    if (FDataSet is TIBCustomDataSet) then
    begin
      FQueries[UpdateKind].Database := TIBCustomDataSet(FDataSet).DataBase;
      FQueries[UpdateKind].Transaction := TIBCustomDataSet(FDataSet).Transaction;
    end;
  end;
  Result := FQueries[UpdateKind];
end;

function TIBUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

function TIBUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := FSQLText[TUpdateKind(Index)];
end;

function TIBUpdateSQL.GetDataSet: TIBCustomDataSet;
begin
  Result := FDataSet;
end;

procedure TIBUpdateSQL.SetDataSet(ADataSet: TIBCustomDataSet);
begin
  FDataSet := ADataSet;
end;

procedure TIBUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TIBUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

procedure TIBUpdateSQL.SQLChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    if Sender = FSQLText[UpdateKind] then
    begin
      if Assigned(FQueries[UpdateKind]) then
      begin
        FQueries[UpdateKind].Params.Clear;
        FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
      end;
      Break;
    end;
end;

procedure TIBUpdateSQL.SetParams(UpdateKind: TUpdateKind);
var
  I: Integer;
  Old: Boolean;
  Param: TParam;
  PName: string;
  Field: TField;
  Value: Variant;
begin
  if not Assigned(FDataSet) then
    Exit;
  for I := 0 to Query[UpdateKind].Params.Count - 1 do
  begin
    Param := Query[UpdateKind].Params[I];
    PName := Param.Name;
    Old := PName.ToUpper.StartsWith('OLD_'); {do not localize}
    if Old then
      PName := PName.Remove(0, 4);
    Field := FDataSet.FindField(PName);
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

procedure TIBUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  ExecSQL(UpdateKind);
end;

procedure Register;
begin

  // componentes
  RegisterComponents('Interbase', [TIBUpdateSQL]);

end;


end.
