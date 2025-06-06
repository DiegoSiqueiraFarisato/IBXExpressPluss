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

unit IBX.IBEvents;

interface

uses
  System.Classes, IBX.IBHeader, IBX.IBExternals, IBX.IB, IBX.IBDatabase,
  System.Generics.Collections, System.SysUtils;

type

  TEventAlert = procedure( Sender: TObject; EventName: string; EventCount: longint;
                           var CancelAlerts: Boolean) of object;
  TErrorEvent=procedure( Sender: TObject; ErrorCode:integer)of object;

  TEventStrings = TList<TBytes>;

  {$ifdef compilerversion < 12}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid)]
  {$else}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidiOSDevice64 or pidAndroidArm32)]
  {$endif}

  TIBEvents = class(TComponent, IIBEventNotifier)
  private
    FEvents: TEventStrings;
    FUniEvents : TStrings;
    FOnEventAlert: TEventAlert;
    FThreads : TList<TThread>;
    FNativeHandle : TISC_DB_HANDLE;
    ThreadException : Boolean;
    [weak] FDatabase: TIBDatabase;
    FOnError: TErrorEvent;
    FAutoRegister: Boolean;
    FRegistered : Boolean;
    procedure SetDatabase( value: TIBDatabase);
    procedure SetEvents(Value: TStrings);
    function GetRegistered: Boolean;
    procedure SetRegistered(const Value: Boolean);
  protected
    { Protected declarations }
    function GetNativeHandle: TISC_DB_HANDLE; virtual;
    procedure EventChange(Sender: TObject); virtual;
    procedure ThreadEnded(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ValidateDatabase(DataBase: TIBDatabase); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    property NativeHandle: TISC_DB_HANDLE read GetNativeHandle;
    procedure SetAutoRegister(const Value: Boolean);
    function GetAutoRegister: Boolean;
  published
    { Published declarations }
    property AutoRegister: Boolean read GetAutoRegister write SetAutoRegister;
    property Database: TIBDatabase read FDatabase write SetDatabase;
    property Events: TStrings read FUniEvents write SetEvents;
    property Registered: Boolean read GetRegistered write SetRegistered;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
    property OnError:TErrorEvent read FOnError write FOnError;
  end;

  procedure Register;

implementation

uses
  System.SyncObjs;

const
  IB_MAX_EVENT_BLOCK   = 15;   // maximum events handled per block by InterBase
  IB_MAX_EVENT_LENGTH  = 64;  // maximum event name length

type

  { TIBEventThread }
  TIBEventThread = class(TThread)
  private
    // IB API call parameters
    WhichEvent: Integer;
    EventID: ISC_LONG;
    EventBuffer: PByte;
    EventBufferLen: Short;
    ResultBuffer: PByte;
    FStatus : array[0..14] of UISC_LONG;
    // Local use variables
    Signal : TSimpleEvent;
    EventsReceived, FirstTime : Boolean;
    EventGroup, EventCount, CountForEvent: Integer;
    Parent: TIBEvents;
    FExceptObject: TObject;
    FExceptAddr: Pointer;
    FCancelAlerts : Boolean;
  protected
    procedure Execute; override;
    procedure SignalEvent; virtual;
    procedure SignalTerminate; virtual;
    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    procedure QueueEvents; virtual;
    procedure SQueEvents;
    procedure ProcessEvents; virtual;
    procedure DoEvent;
    procedure DoHandleException;
    function HandleException: Boolean; virtual;
    procedure UpdateResultBuffer(Length: UShort; Updated: PByte);
  public
    constructor Create(Owner: TIBEvents; EventGrp: Integer; TermEvent: TNotifyEvent); virtual;
    destructor Destroy; override;
  end;

function TIBEvents.GetNativeHandle: TISC_DB_HANDLE;
begin
  ValidateDatabase(FDatabase);
  Result := FDatabase.Handle;
end;

procedure TIBEvents.ValidateDatabase( Database: TIBDatabase);
begin
  if not assigned( Database) then
    IBError(ibxeDatabaseNameMissing, [nil]);
  if not Database.Connected then
    IBError(ibxeDatabaseClosed, [nil]);
end;

{ TIBEvents }

constructor TIBEvents.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);
  ThreadException := False;
  FOnEventAlert := nil;
  FNativeHandle := nil;
  FDatabase := nil;
  FAutoRegister := False;
  FEvents := TEventStrings.Create;
  FUniEvents := TStringList.Create;
  TStringList(FUniEvents).Sorted := true;  // dupIgnore only works when the TStringList is sorted
  TStringList(FUniEvents).OnChange := EventChange; // assign the routine which validates the event lenghts
  TStringList(FUniEvents).Duplicates := dupIgnore; // don't allow duplicate events
  FThreads := TList<TThread>.Create;
end;

destructor TIBEvents.Destroy;
begin
  try
    if Registered then
      UnRegisterEvents;
  except
    // silence any exceptions which might be raised
    // by UnRegisterEvents during destruction
  end;
  If Assigned(FDatabase) then
    FDatabase.RemoveEventNotifier(Self);
  FThreads.Free;
  TStringList(FUniEvents).OnChange := nil;
  FUniEvents.Free;
  FEvents.Free;
  inherited Destroy;
end;

procedure TIBEvents.Notification( AComponent: TComponent;
                                        Operation: TOperation);
begin
  inherited Notification( AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
  begin
    if Registered then
      UnRegisterEvents;
    FDatabase := nil;
  end;
end;

procedure TIBEvents.RegisterEvents;
var
  i: Integer;
  s: String;
  b : TBytes;
begin
  if csDesigning in ComponentState then
    exit;
  try
    if not Assigned(FDatabase) then
      IBError(ibxeDatabaseNameMissing, []);
    if not FDatabase.Connected then
      IBError(ibxeDatabaseClosed, []);
    // We need the strings to be permanent AnsiStrings in TBytes form
    FEvents.Clear;
    for s in FUniEvents do
    begin
      b := TEncoding.ANSI.GetBytes(s);
      SetLength(b, Length(b) + 1);
      FEvents.Add(b);
    end;
    if (FThreads.Count = 0) then
    begin
      if (FEvents.Count > 0) then
      begin
        for i := 0 to ((FEvents.Count - 1) div IB_MAX_EVENT_BLOCK) do
          FThreads.Add(TIBEventThread.Create(Self, i, ThreadEnded));
      end;
    end
    else
      IBError(ibxeEventAlreadyRegistered, []);
  finally
    FRegistered := FThreads.Count <> 0;
  end;
end;

procedure TIBEvents.SetEvents( value: TStrings);
begin
  FUniEvents.Assign(Value);
end;

procedure TIBEvents.SetDatabase( value: TIBDatabase);
var
  WasRegistered: Boolean;
begin
  if (Value <> FDatabase) then
  begin
    if (csDesigning in ComponentState) then
      FDatabase := Value
    else
    begin
      WasRegistered := Registered;
      if WasRegistered then
        UnRegisterEvents;
      try
        if Assigned(FDatabase) then
        begin
          FDatabase.RemoveFreeNotification(self);
          FDatabase.RemoveEventNotifier(Self);
        end;

        FDatabase := Value;

        if Assigned(FDatabase) then
        begin
          FDatabase.FreeNotification(self);
          FDatabase.AddEventNotifier(Self);
        end;

      finally
        if WasRegistered then
          RegisterEvents;
      end;
    end;
  end;
end;

procedure TIBEvents.SetRegistered(const Value : Boolean);
begin
  FRegistered := Value;
  if csDesigning in ComponentState then
    exit;
  if Value then
    RegisterEvents
  else
    UnRegisterEvents;
end;

procedure TIBEvents.UnregisterEvents;
var
  i: Integer;
  Temp: TIBEventThread;
begin
  if csDesigning in ComponentState then
    exit;
  if (FThreads.Count > 0) then
  begin
    for i := (FThreads.Count - 1) downto 0 do
    begin
      Temp := TIBEventThread(FThreads[i]);
      FThreads.Delete(i);

      Temp.SignalTerminate;
      Temp.WaitFor;
      Temp.Free;
    end;
  end;
  FRegistered := FThreads.Count <> 0;
end;

{ TIBEventThread }

procedure EventCallback(P: Pointer; Length: Short; Updated: PByte); cdecl;
begin
  if (Assigned(P) and Assigned(Updated)) then
  begin
    TIBEventThread(P).UpdateResultBuffer(Length, Updated);
    TIBEventThread(P).SignalEvent;
  end;
end;

procedure TIBEventThread.DoEvent;
var
  bt : TBytes;
begin
  bt := Parent.FEvents[((EventGroup * IB_MAX_EVENT_BLOCK) + WhichEvent)];
  Parent.FOnEventAlert(Parent,
     TEncoding.ANSI.GetString(bt, 0, Length(bt) - 1),
     CountForEvent, FCancelAlerts)
end;

procedure TIBEventThread.UpdateResultBuffer(Length: UShort; Updated: PByte);
begin
  Move(Updated[0], ResultBuffer[0], Length);
end;

procedure TIBEventThread.QueueEvents;
begin
  EventsReceived := False;
  Signal.ResetEvent;
  SQueEvents;
end;

procedure TIBEventThread.ProcessEvents;
var
  i: Integer;
begin
  Parent.Database.GDSLibrary.isc_event_counts(@FStatus, EventBufferLen, EventBuffer, ResultBuffer);
  if Assigned(Parent.FOnEventAlert) and (not FirstTime) then
  begin
    FCancelAlerts := false;
    for i := 0 to (EventCount - 1) do
    begin
      if (FStatus[i] <> 0) then
      begin
        WhichEvent := i;
        CountForEvent := FStatus[WhichEvent];
        Synchronize(DoEvent)
      end;
    end;
  end;
  FirstTime := false;
end;

procedure TIBEventThread.UnRegisterEvents;
begin
  Parent.Database.Call(Parent.Database.GDSLibrary.isc_cancel_events(StatusVector, @Parent.Database.Handle,
     @EventID), True);
  Parent.Database.GDSLibrary.isc_free(EventBuffer);
  EventBuffer := nil;
  Parent.Database.GDSLibrary.isc_free(ResultBuffer);
  ResultBuffer := nil;
end;

procedure TIBEventThread.RegisterEvents;
var
  EBPArray : Array[1..15] of PByte;
  i : Integer;

  function EBP(Index: Integer): PByte;
  var
    ab : TBytes;
  begin
    Inc(Index, (EventGroup * IB_MAX_EVENT_BLOCK));
    if (Index > Parent.FEvents.Count) then
      Result := nil
    else
    begin
      ab := Parent.FEvents[Index - 1];
      Result := PByte(ab);
    end;
  end;

begin
  EventBuffer := nil;
  ResultBuffer := nil;
  EventBufferLen := 0;
  FirstTime := true;
  EventCount := (Parent.FEvents.Count - (EventGroup * IB_MAX_EVENT_BLOCK));
  if (EventCount > IB_MAX_EVENT_BLOCK) then
    EventCount := IB_MAX_EVENT_BLOCK;
  for i := 1 to 15 do
    EBPArray[i] := EBP(i);
  EventBufferLen := Parent.Database.GDSLibrary.isc_event_block(@EventBuffer,
    @ResultBuffer, SmallInt(EventCount), EBPArray);
end;

procedure TIBEventThread.SignalEvent;
begin
  EventsReceived := True;
  Signal.SetEvent;
end;

procedure TIBEventThread.SignalTerminate;
begin
  if not Terminated then
  begin
    Terminate;
    Signal.SetEvent;
  end;
end;

procedure TIBEventThread.DoHandleException;
begin
  System.SysUtils.ShowException(FExceptObject, FExceptAddr);
end;

function TIBEventThread.HandleException: Boolean;
begin
  if not Parent.ThreadException then
  begin
    Result := True;
    Parent.ThreadException := True;
    FExceptObject := ExceptObject;
    FExceptAddr := ExceptAddr;
    try
      if not (FExceptObject is EAbort) then
        Synchronize(DoHandleException);
    finally
      FExceptObject := nil;
      FExceptAddr := nil;
    end;
  end
  else
    Result := False;
end;

procedure TIBEventThread.Execute;
begin
  RegisterEvents;
  QueueEvents;
  try
    repeat
      Signal.WaitFor(INFINITE);
      if EventsReceived then
      begin
        ProcessEvents;
        QueueEvents;
      end;
    until Terminated;
    ReturnValue := 0;
  except
    if HandleException then
      ReturnValue := 1
    else
      ReturnValue := 0;
  end;
end;

constructor TIBEventThread.Create(Owner: TIBEvents; EventGrp: Integer; TermEvent: TNotifyEvent);
begin
  FCancelAlerts := false;
  Signal := TSimpleEvent.Create;
  Parent := Owner;
  EventGroup := EventGrp;
  OnTerminate := TermEvent;
  inherited Create;
end;

destructor TIBEventThread.Destroy;
begin
  try
    UnRegisterEvents;
  except
    if HandleException then
      ReturnValue := 1
    else
      ReturnValue := 0;
  end;
  Signal.Free;
  inherited Destroy;
end;

procedure TIBEvents.EventChange(Sender: TObject);
var
  i: Integer;
  TooLong,
  AnyEmpty,
  WasRegistered: Boolean;
  ErrorStr: String;
begin
  ErrorStr := EmptyStr;
  WasRegistered := Registered;
  try
    if WasRegistered then
      UnRegisterEvents;
    TStringList(FUniEvents).OnChange := nil;
    try
      TooLong := False;
      AnyEmpty := False;
      for i := (FUniEvents.Count - 1) downto 0 do
      begin
        if (FUniEvents[i] = EmptyStr) then
        begin
          AnyEmpty := True;
          FUniEvents.Delete(i);
        end
        else
        if (FUniEvents[i].Length > (IB_MAX_EVENT_LENGTH - 1)) then
        begin
          TooLong := True;
          FUniEvents[i] := FUniEvents[i].Substring(0, IB_MAX_EVENT_LENGTH - 1);
        end;
      end;
      if AnyEmpty then
        IBError(ibxeInvalidEvent, []);
      if TooLong then
        IBError(ibxeInvalidEvent, []);
    finally
      TStringList(FUniEvents).OnChange := EventChange;
    end;
  finally
    if WasRegistered then
      RegisterEvents;
  end;
end;

function TIBEvents.GetRegistered: Boolean;
begin
  Result := FRegistered;
end;

procedure TIBEvents.ThreadEnded(Sender: TObject);
var
  ThreadIdx: Integer;
begin
  if (Sender is TIBEventThread) then
  begin
    ThreadIdx := FThreads.IndexOf(TIBEventThread(Sender));
    if (ThreadIdx > -1) then
      FThreads.Delete(ThreadIdx);
    if (TIBEventThread(Sender).ReturnValue = 1) then
    begin
      if Registered then
        UnRegisterEvents;
      ThreadException := False;
    end
  end;
end;

procedure TIBEvents.SetAutoRegister(const Value: Boolean);
begin
  if FAutoRegister <> Value then
  begin
    FAutoRegister := Value;
    if FAutoRegister and (not Registered) and
       Assigned(FDatabase) and FDatabase.Connected then
      RegisterEvents;
  end;
end;

function TIBEvents.GetAutoRegister: Boolean;
begin
  Result := FAutoRegister;
end;

procedure TIBEventThread.SQueEvents;
begin
  try
    Parent.Database.Call(Parent.Database.GDSLibrary.isc_que_events(StatusVector, @Parent.Database.Handle,
      @EventID, EventBufferLen, EventBuffer, TISC_CALLBACK(@EventCallback),
      PVoid(Self)), True);
  except
    on E : Exception do
      if Assigned(Parent.OnError) then
        if E is EIBError then
          Parent.OnError(Parent, EIBError(E).IBErrorCode)
        else
          Parent.OnError(Parent, 0);
  end;
end;

procedure Register;
begin

  // componentes
  RegisterComponents('Interbase', [
    TIBEvents
  ]);

end;

end.
