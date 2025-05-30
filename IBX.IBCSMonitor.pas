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

unit IBX.IBCSMonitor;

interface

uses
  System.Classes, IBX.IBSQLMonitor, IBX.IBSQL, IBX.IBDatabase,
  IBX.IBServices, IBX.IB,  IPPeerAPI, System.Generics.Collections;

type

  TIBContext = class
  private
    FContext : IIPContext;
  public
    constructor Create(AContext : IIPContext);
    property Context : IIPContext read FContext;
  end;

  {$ifdef compilerversion < 12}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid)]
  {$else}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidiOSDevice64 or pidAndroidArm32)]
  {$endif}

  TIBMonitorServer = class(TComponent, IIBSQLMonitorHook)
  private
    FidServer : IIPTCPServer;
    FContexts : TObjectList<TIBContext>;
    FEnabled: Boolean;
    FTraceFlags : TTraceFlags;
    FOldmonitor : IIBSQLMonitorHook;
    FActive : Boolean;
    FPort: Integer;

    procedure OnConnect (AContext: IIPContext);
    procedure OnDisconnect(AContext: IIPContext);
    procedure ServerExecute(AContext: IIPContext);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure Start;
    procedure Stop;
  protected
    procedure WriteSQLData(Text: String; DataType: TTraceFlag);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure RegisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure UnregisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure ReleaseMonitor(Arg : TIBCustomSQLMonitor);
    procedure SQLPrepare(qry: TIBSQL); virtual;
    procedure SQLExecute(qry: TIBSQL); virtual;
    procedure SQLFetch(qry: TIBSQL); virtual;
    procedure DBConnect(db: TIBDatabase); virtual;
    procedure DBDisconnect(db: TIBDatabase); virtual;
    procedure TRStart(tr: TIBTransaction); virtual;
    procedure TRCommit(tr: TIBTransaction); virtual;
    procedure TRCommitRetaining(tr: TIBTransaction); virtual;
    procedure TRRollback(tr: TIBTransaction); virtual;
    procedure TRRollbackRetaining(tr: TIBTransaction); virtual;
    procedure ServiceAttach(service: TIBCustomService); virtual;
    procedure ServiceDetach(service: TIBCustomService); virtual;
    procedure ServiceQuery(service: TIBCustomService); virtual;
    procedure ServiceStart(service: TIBCustomService); virtual;
    procedure SendMisc(Msg : String);
    procedure SendError(Msg : String; db: TIBDatabase); overload;
    procedure SendError(Msg : String); overload;
    function GetEnabled: Boolean;
    function GetTraceFlags: TTraceFlags;
    function GetMonitorCount : Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetTraceFlags(const Value: TTraceFlags);
  published
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
    [default (True)]
    property Enabled : Boolean read GetEnabled write SetEnabled default true;
    [default (212)]
    property Port : Integer read FPort write FPort default 212;
    property Active : Boolean read GetActive write SetActive;
  end;

  {$ifdef compilerversion < 12}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid)]
  {$else}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidiOSDevice64 or pidAndroidArm32)]
  {$endif}

  TIBMonitorClient = class(TIBCustomSQLMonitor)
  private
    FidClient : IIPTCPClient;
    FThread : TThread;
    FEnabled : Boolean;

    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetHost: String;
    procedure SetHost(const Value: String);
    function GetIPVersion: TIPVersionPeer;
    procedure SetIPVersion(const Value: TIPVersionPeer);
    procedure ClientConnected;
    procedure FOnTerminate(Sender : TObject);
    procedure OnData(TraceFlag : TTraceFlag; EventTime : TDateTime; Msg : String);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    [default (212)]
    property Port : Integer read GetPort write SetPort default 212;
    property Host : String read GetHost write SetHost;
    property IPVersion : TIPVersionPeer read GetIPVersion write SetIPVersion;
    property OnSQL;
    property TraceFlags;
    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;

  procedure Register;
implementation

uses
  IBX.IBCustomDataset, IBX.IBXConst, IBX.IBHeader, System.SysUtils;

{ TIBServerMonitor }

const
  cTSOffset = SizeOf(TTraceFlag);
  cTextOffset = cTSOffSet + SizeOf(TDateTime);

type

  TIBOnData = procedure(TraceFlag : TTraceFlag; EventTime : TDateTime; Msg : String) of object;

  TClientReader = class(TThread)
  private
    FClient: IIPTCPClient;
    FOnData: TIBOnData;
  protected
    procedure Execute; override;
  public
    constructor Create(aClient : IIPTCPClient);
    property OnData : TIBOnData read FOnData write FOnData;
    property Client : IIPTCPClient read FClient write FClient;
  end;


constructor TIBMonitorServer.Create(AOwner : TComponent);
begin
  inherited;
  FEnabled := true;
  FContexts := TObjectList<TIBContext>.Create;
  FPort := 212;
  FTraceFlags := [tfQPrepare..tfMisc];
  FOldMonitor := SetMonitorHook(Self);
end;

procedure TIBMonitorServer.DBConnect(db: TIBDatabase);
var
  st : String;
begin
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Connect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

procedure TIBMonitorServer.DBDisconnect(db: TIBDatabase);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Disconnect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

destructor TIBMonitorServer.Destroy;
begin
  if Assigned(FidServer) then
    FidServer.Active := false;
  FContexts.Free;
  SetMonitorHook(FOldmonitor);
  inherited;
end;

function TIBMonitorServer.GetActive: Boolean;
begin
  Result := FActive;
end;

function TIBMonitorServer.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TIBMonitorServer.GetMonitorCount: Integer;
begin
  Result := FContexts.Count;
end;

function TIBMonitorServer.GetTraceFlags: TTraceFlags;
begin
  Result := FTraceFlags;
end;

procedure TIBMonitorServer.OnConnect(AContext: IIPContext);
begin
  TMonitor.Enter(FContexts);
  FContexts.Add(TIBContext.Create(AContext));
  TMonitor.Exit(FContexts);
end;

procedure TIBMonitorServer.OnDisconnect(AContext: IIPContext);
var
  i: Integer;
begin
  TMonitor.Enter(FContexts);
  for i := FContexts.Count - 1 downto 0 do
    if FContexts[i].Context.GetObject = AContext.GetObject then
      FContexts.Delete(i);
  TMonitor.Exit(FContexts);
end;

procedure TIBMonitorServer.RegisterMonitor(SQLMonitor: TIBCustomSQLMonitor);
begin

end;

procedure TIBMonitorServer.ReleaseMonitor(Arg: TIBCustomSQLMonitor);
begin

end;

procedure TIBMonitorServer.SendError(Msg: String; db: TIBDatabase);
begin
  if FEnabled and (tfError in FTraceFlags * db.TraceFlags) then
    WriteSQLData(StrError + Msg, tfError);
end;

procedure TIBMonitorServer.SendError(Msg: String);
begin
  if FEnabled and (tfError in FTraceFlags) then
    WriteSQLData(StrError + Msg, tfError);
end;

procedure TIBMonitorServer.SendMisc(Msg: String);
begin
  if FEnabled then
    WriteSQLData(StrMisc + Msg, tfMisc);
end;

procedure TIBMonitorServer.ServerExecute(AContext: IIPContext);
begin

end;

procedure TIBMonitorServer.ServiceAttach(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + StrAttach;
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBMonitorServer.ServiceDetach(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + StrDetach;
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBMonitorServer.ServiceQuery(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + StrQuery;
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBMonitorServer.ServiceStart(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + StrStart;
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBMonitorServer.SetActive(const Value: Boolean);
begin
  if not (csDesigning in ComponentState) and
     (FActive <> Value) then
  begin
    if Value then
      Start
    else
      Stop;
  end;
  FActive := Value;
end;

procedure TIBMonitorServer.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TIBMonitorServer.SetTraceFlags(const Value: TTraceFlags);
begin
  FTraceFlags := Value
end;

procedure TIBMonitorServer.SQLExecute(qry: TIBSQL);
var
  st: String;
  i: Integer;
begin
  if FEnabled then
  begin
    if not ((tfQExecute in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags)) ) then
      Exit;
    if qry.Owner is TIBCustomDataSet then
      st := TIBCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + StrExecute + qry.SQL.Text;
    if qry.Params.Count > 0 then
    begin
      for i := 0 to qry.Params.Count - 1 do
      begin
        st := st + CRLF + '  ' + qry.Params[i].Name + ' = ';  {do not localize}
        try
          if qry.Params[i].IsNull then
            st := st + StrNULL
          else
          if qry.Params[i].SQLType <> SQL_BLOB then
            st := st + qry.Params[i].AsString
          else
            st := st + StrBLOB;
        except
          st := st + '<' + SCantPrintValue + '>';  {do not localize}
        end;
      end;
    end;
    WriteSQLData(st, tfQExecute);
  end;
end;

procedure TIBMonitorServer.SQLFetch(qry: TIBSQL);
var
  st: String;
begin
  if FEnabled then
  begin
    if not ((tfQFetch in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TIBCustomDataSet then
      st := TIBCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + StrFetch + qry.SQL.Text;
    if (qry.EOF) then
      st := st + CRLF + '  ' + SEOFReached;   {do not localize}
    WriteSQLData(st, tfQFetch);
  end;
end;

procedure TIBMonitorServer.SQLPrepare(qry: TIBSQL);
var
  st : String;
begin
  if FEnabled then
  begin
    if not ((tfQPrepare in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TIBCustomDataSet then
      st := TIBCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + StrPrepare + qry.SQL.Text + CRLF;
    try
      st := st + StrPlan + qry.Plan;
    except
      st := st + StrPlanCantRetrive;
    end;
    WriteSQLData(st, tfQPrepare);
  end;
end;

procedure TIBMonitorServer.Start;
var
  LSocketHandle: IIPSocketHandle;
begin
  try
    FidServer := PeerFactory.CreatePeer('', IIPTCPServer, nil) as IIPTCPServer;
  except
    on e : EIPAbstractError do
      raise EIPAbstractError.Create('Can not start IBMOnitorServer. IIPTCPServer peer not registered.  Make sure IPPeerServer (or an alternative IP Implementation unit) is in the uses clause');
  end;

  FidServer.OnConnect := OnConnect;
  FidServer.OnDisconnect := OnDisconnect;
  FidServer.OnExecute := ServerExecute;
  FidServer.UseNagle := false;
  FidServer.Bindings.Add.Port := FPort; //default IPv4
  if GStackPeers('').SupportsIPv6 then
  begin
    LSocketHandle := FidServer.Bindings.Add;
    LSocketHandle.Port := FPort; //default IPv4
    LSocketHandle.IPVersion := TIPVersionPeer.IP_IPv6
  end;
  FidServer.Active := true;
end;

procedure TIBMonitorServer.Stop;
begin
  if FidServer <> nil then
  begin
    FidServer.Active := False;
    FidServer := nil;
  end;
  FContexts.Clear;
end;

procedure TIBMonitorServer.TRCommit(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags)) then
    begin
      st := tr.Name + StrCommitHardComm;
      WriteSQLData(st, tfTransact);
    end;
  end;
end;

procedure TIBMonitorServer.TRCommitRetaining(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags)) then
    begin
      st := tr.Name + StrCommitRetaining;
      WriteSQLData(st, tfTransact);
    end;
  end;
end;

procedure TIBMonitorServer.TRRollback(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags)) then
    begin
      st := tr.Name + StrRollback;
      WriteSQLData(st, tfTransact);
    end;
  end;
end;

procedure TIBMonitorServer.TRRollbackRetaining(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags)) then
    begin
      st := tr.Name + StrRollbackRetainin;
      WriteSQLData(st, tfTransact);
    end;
  end;
end;

procedure TIBMonitorServer.TRStart(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags)) then
    begin
      st := tr.Name + StrStartTransaction;
      WriteSQLData(st, tfTransact);
    end;
  end;
end;

procedure TIBMonitorServer.UnregisterMonitor(SQLMonitor: TIBCustomSQLMonitor);
begin

end;

procedure TIBMonitorServer.WriteSQLData(Text: String; DataType: TTraceFlag);
var
  b : TBytes;
  i : Integer;
  d : Double;
  ts : TDateTime;

begin
  if FContexts.Count > 0 then
  begin
    SetLength(b, cTextOffSet + (Text.Length * SizeOf(char)));
    b[0] := Ord(DataType);
    // I'm sure there is an easier way to get at the bytes of a TDateTime...
    ts := now;
    d := Double(ts);
    for i := 0 to 7 do
      b[cTSOffset + i] := d.Bytes[i];

    Move(Text[low(Text)], b[cTextOffset], Text.Length * SizeOf(char));
    TMonitor.Enter(FContexts);
    try
      for i := 0 to FContexts.Count - 1 do
      begin
        FContexts[i].context.Connection.IOHandler.Write(b);
      end;
    finally
      TMonitor.Exit(FContexts);
    end;
  end;
end;

{ TIBMonitorClient }

procedure TIBMonitorClient.ClientConnected;
begin
  FThread := TClientReader.Create(FidClient);
  begin
    FThread.OnTerminate := FOnTerminate;
    TClientReader(FThread).OnData := OnData;
    FThread.Start;
  end;
end;

constructor TIBMonitorClient.Create(AOwner: TComponent);
begin
  inherited;
  try
    FidClient := PeerFactory.CreatePeer('', IIPTCPClient, nil) as IIPTCPClient;
    FidClient.UseNagle := false;
  except
    on e : EIPAbstractError do
      raise EIPAbstractError.Create('Can not create TIBMonitorClient. IIPTCPClient peer not registered.  Make sure IPPeerClient (or an alternative IP Implementation unit) is in the uses clause');
  end;
  FidClient.Port := 212;
end;

destructor TIBMonitorClient.Destroy;
begin
  if Assigned(FThread) then
  begin
    TClientReader(FThread).OnTerminate := nil;
    FThread.Terminate;
  end;
  if Assigned(FidClient) and FidClient.Connected then
    FidClient.Disconnect;
  inherited;
end;

function TIBMonitorClient.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TIBMonitorClient.GetHost: String;
begin
  Result := FidClient.Host;
end;

function TIBMonitorClient.GetIPVersion: TIPVersionPeer;
begin
  Result := FidClient.IPVersion;
end;

function TIBMonitorClient.GetPort: Integer;
begin
  Result := FidClient.Port;
end;

procedure TIBMonitorClient.OnData(TraceFlag: TTraceFlag; EventTime: TDateTime;
  Msg: String);
begin
  if (Assigned(OnSQL)) and
     (TraceFlag in TraceFlags) then
    OnSQL(Msg, EventTime);
end;

procedure TIBMonitorClient.FOnTerminate(Sender: TObject);
begin
  FThread := nil;
end;

procedure TIBMonitorClient.SetEnabled(const Value: Boolean);
begin
  if not (csDesigning in ComponentState) then
  begin
    if FidClient.Connected <> Value then
      if Value then
      begin
        FidClient.Connect;
        ClientConnected;
      end
      else
      begin
        FidClient.Disconnect;
        if Assigned(FThread) then
          FThread.Terminate;
      end;
  end;
  FEnabled := Value;
end;

procedure TIBMonitorClient.SetHost(const Value: String);
begin
  FidClient.Host := Value;
end;

procedure TIBMonitorClient.SetIPVersion(const Value: TIPVersionPeer);
begin
  FidClient.IPVersion := Value;
end;

procedure TIBMonitorClient.SetPort(const Value: Integer);
begin
  FidClient.Port := Value;
end;

{ TClientReader }

constructor TClientReader.Create(aClient : IIPTCPClient);
begin
  inherited create(true);
  FreeOnTerminate := true;
  FClient := aClient;
end;

procedure TClientReader.Execute;
var
  b : TIPBytesPeer;
  tf : TTraceFlag;
  EventDate : TDateTime;
  Msg : String;
  I: Integer;
  ActualReadCount: Integer;
begin
  try
    while (not Terminated) and Assigned(FClient) and (FClient.Connected) do
    begin
      if FClient.IOHandler.InputBuffer.Size = 0 then
         FClient.IOHandler.CheckForDataOnSource(10)
      else
      begin
        ActualReadCount := FClient.IOHandler.InputBuffer.Size;
        SetLength(b, ActualReadcount);
        FClient.IOHandler.ReadBytes(b, ActualReadCount, false);
        if Length(b) > 0 then
        begin
          tf := TTraceFlag(b[0]);
          for I := 0 to 7 do
            Double(EventDate).Bytes[i] := b[cTSOffset + i];
          SetString(Msg, PChar(@(b[cTextOffset])), (Length(b) - cTextOffset) div SizeOf(Char));
          if Assigned(FOnData) then
            Synchronize(procedure begin FOnData(tf, EventDate, Msg); end);
         end;
      end;
    end;
  except
    on e: Exception do
    begin
      if Assigned(FClient) and FClient.Connected then
        FClient.Disconnect;
    end;
  end;
end;

{ TIBContext }

constructor TIBContext.Create(AContext: IIPContext);
begin
  inherited Create;
  FContext := AContext;
end;

procedure Register;
begin
  // componentes
  RegisterComponents('Interbase', [TIBMonitorClient,TIBMonitorServer]);
end;
end.
