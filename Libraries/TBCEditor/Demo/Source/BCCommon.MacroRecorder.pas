unit BCCommon.MacroRecorder;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls;

type
  TMacroWinControl = class
  strict private
    FHandle: THandle;
    FWinControl: TWinControl;
  public
    function Self: TMacroWinControl;
    function ScreenX: Integer;
    function ScreenY: Integer;
    property Handle: THandle read FHandle write FHandle;
    property WinControl: TWinControl read FWinControl write FWinControl;
  end;

  TMacroWinControlList = class(TObject)
  strict private
    FWinControlList: TStringList;
  public
    constructor CreateList;
    destructor Destroy; override;
    function Add(Name: string): TMacroWinControl; overload;
    function WinControlByHandle(HandleParam: THandle): TMacroWinControl;
    procedure Add(Name: string; WinControl: TWinControl); overload;
    procedure BuildList(WinControl: TWinControl);
  end;

  TWindowsMessage = packed record
    Msg: Cardinal;
    case Integer of
      0 : (
        WParam: Longint;
        LParam: Longint;
        Time: DWORD;
        WindowHandle: HWND);
      1 : (
        case Integer of
          0: (
            WParamLo: Word;
            WParamHi: Word;
            LParamLo: Word;
            LParamHi: Word);
          1: (
            WParamBytes, LParamBytes: packed array[0..3] of Byte));
  end;

  PWindowsMessage = ^TWindowsMessage;

  TMacroMessageList = class;

  TMacroMessage = class
  strict private
    FParentMessageList: TMacroMessageList;
  public
    { WindowsMessage can't be a property because it makes the TWindowsMessage record read-only even though
      there is a write attribute. }
    WindowsMessage: TWindowsMessage;
    function IsMouseMessage: Boolean;
    function ToString: string; override;
    property ParentMessageList: TMacroMessageList read FParentMessageList write FParentMessageList;
  end;

  TMacroMessageList = class
  strict private
    FMessageList: TStringList;
    FWinControlList: TMacroWinControlList;
    FStartTickCount: Integer;
  public
    constructor Create(WinControl: TWinControl); overload;
    destructor Destroy; override;

    function Add(Name: string): TMacroMessage; overload;
    function Add(WindowsMessage: PWindowsMessage): TMacroMessage; overload;
    function Count: Integer;
    function GetMessage(Index: Integer): TMacroMessage;
    function ToString: string; override;
    procedure Add(Name: string; MacroMessage: TMacroMessage); overload;
    procedure Clear;
    procedure DeleteLast;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure UpdateRelative;
    property StartTickCount: Integer read FStartTickCount write FStartTickCount;
    property WinControlList: TMacroWinControlList read FWinControlList;
  end;

  TMacroRecorder = class
  strict private
    FMacroMessageList: TMacroMessageList;
    FNextMacroMessage: TMacroMessage;
    FIsRecording: Boolean;
    FIsPaused: Boolean;
    FIsPlaying: Boolean;
    FHookHandle: hHook;
  public
    MessageIndex: Integer;
    class function ClassCreate(WinControl: TWinControl): TMacroRecorder;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Play;
    procedure StopPlayback;
    procedure GetMessage(Index: Integer);

    property HookHandle: hHook read FHookHandle;
    property MacroMessageList: TMacroMessageList read FMacroMessageList;
    property NextMacroMessage: TMacroMessage read FNextMacroMessage;
  end;

implementation

uses
  System.Types, System.SysUtils;

const
  MAXCONTROLLENGTH = 50;

type
  TMacroFile = packed record
    WindowsMessage: TWindowsMessage;
  end;

var
  FMacroRecorder: TMacroRecorder = nil;

{ TMacroWinControl }

function TMacroWinControl.Self: TMacroWinControl;
begin
  Result := Self;
end;

function TMacroWinControl.ScreenX: Integer;
begin
  with FWinControl do
    Result:= ClientToScreen(Point(Left, Top)).X;
end;

function TMacroWinControl.ScreenY: Integer;
begin
  with FWinControl do
    Result:= ClientToScreen(Point(Left, Top)).Y;
end;

{ TWinControlList }

constructor TMacroWinControlList.CreateList;
begin
  inherited Create;
  FWinControlList := TStringList.Create;
end;

destructor TMacroWinControlList.Destroy;
begin
  FWinControlList.Free;
  inherited;
end;

function TMacroWinControlList.Add(Name: string): TMacroWinControl;
begin
  Result := TMacroWinControl.Create;
  FWinControlList.AddObject(Name, Result);
end;

procedure TMacroWinControlList.Add(Name: string; WinControl: TWinControl);
var
  MacroWinControl: TMacroWinControl;
begin
  MacroWinControl := Add(Name);
  MacroWinControl.Handle := WinControl.Handle;
  MacroWinControl.WinControl := WinControl;
end;

procedure TMacroWinControlList.BuildList(WinControl: TWinControl);

  procedure BuildListRecursive(Level: integer; WinControl: TWinControl; TotalX, TotalY: Integer);
  var
    i: integer;
    X, Y: Integer;
  begin
    with WinControl do
    begin
      X := TotalX + Left;
      Y := TotalY + Top;

      Add(Name, WinControl);

      for i := 0 to ControlCount - 1 do
        if Controls[i] is TWinControl then
          BuildListRecursive(Level + 1, Controls[i] as TWinControl, X, Y);
    end;
  end;

begin
  BuildListRecursive(0, WinControl, 0, 0);
end;

function TMacroWinControlList.WinControlByHandle(HandleParam: THandle): TMacroWinControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FWinControlList.Count - 1 do
  with FWinControlList.Objects[i] as TMacroWinControl do
  if Handle = HandleParam then
  begin
    Result := Self;
    Break;
  end;
end;

{ TMacroMessage }

function TMacroMessage.IsMouseMessage: Boolean;
begin
  with WindowsMessage do
    Result:= (Msg = WM_LBUTTONDOWN) or (Msg = WM_LBUTTONUP) or (Msg = WM_RBUTTONUP) or (Msg = WM_RBUTTONUP) or
      (Msg = WM_MBUTTONUP) or (Msg = WM_MBUTTONUP)
end;

function TMacroMessage.ToString: string;
var
  WLParam: string;
  MacroWinControl: TMacroWinControl;

  function VirtualKeyCodeToName(KeyCode: Word): string;
  begin
    case KeyCode of
      VK_ADD: Result := 'vkAdd';
      VK_BACK: Result := 'vkBackspace';
      VK_CANCEL: Result := 'vkControl_Break';
      VK_CAPITAL: Result := 'vkCapsLock';
      VK_CLEAR: Result := 'vkClear';
      VK_CONTROL: Result := 'vkControl';
      VK_DECIMAL: Result := 'vkDecimal';
      VK_DELETE: Result := 'vkDelete';
      VK_DIVIDE: Result := 'vkDivide';
      VK_DOWN: Result := 'vkDown';
      VK_END: Result := 'vkEnd';
      VK_ESCAPE: Result := 'vkEscape';
      VK_EXECUTE: Result := 'vkExecute';
      VK_F1..VK_F24 : Result := 'vkF'+ IntToStr(KeyCode - VK_F1);
      VK_HELP: Result := 'vkHelp';
      VK_HOME: Result := 'vkHome';
      VK_INSERT: Result := 'vkInsert';
      VK_LBUTTON: Result := 'vkLeftMouseButton';
      VK_LEFT: Result := 'vkLeft';
      VK_MBUTTON:  Result := 'vkMiddleMouseButton';
      VK_MENU: Result := 'vkAlt';
      VK_MULTIPLY: Result := 'vkMultiply';
      VK_NEXT: Result := 'vkPageDown';
      VK_NUMLOCK: Result := 'vkNumLock';
      VK_NUMPAD0..VK_NUMPAD9: Result := 'vkNumPad'+ IntToStr(KeyCode - vk_Numpad0);
      VK_PAUSE: Result := 'vkPause';
      VK_PRINT: Result := 'vkPrint';
      VK_PRIOR: Result := 'vkPageUp';
      VK_RBUTTON: Result := 'vkRightMouseButton';
      VK_RETURN: Result := 'vkReturn';
      VK_RIGHT: Result := 'vkRight';
      VK_SCROLL: Result := 'vkScrollLock';
      VK_SELECT: Result := 'vkSelect';
      VK_SEPARATOR: Result := 'vkSeparator';
      VK_SHIFT: Result := 'vkShift';
      VK_SNAPSHOT: Result := 'vkPrintScreen';
      VK_SPACE: Result := 'vkSpace';
      VK_SUBTRACT: Result := 'vkSubtract';
      VK_TAB: Result := 'vktab';
      VK_UP: Result := 'vkUp';
      Ord('A')..Ord('Z'): Result := 'vk'+ Chr(KeyCode);
    else
      Result:= 'vk'+ Chr(KeyCode);
    end;
  end;

  function MessageName(Msg: Cardinal): string;

    function IntegerToHex(Value: Integer): string;
    var
      i: Integer;
    begin
      Result := IntToHex(Value, 4);
      for i := Length(Result) + 1 to 8 do
        Result := '0' + Result;
      Insert('.', Result, 5);
    end;

  begin
    case Msg of
      WM_CHAR: Result:= 'Char (Key Press)';
      WM_KEYDOWN: Result:= 'Key Down';
      WM_KEYUP: Result:= 'Key Up';
      WM_LBUTTONDOWN: Result:= 'Left Mouse Button Down';
      WM_LBUTTONUP: Result:= 'Left Mouse Button Up';
      WM_MOUSEMOVE: Result:= 'Mouse Move';
    else
      Result:= 'WM_' + IntegerToHex(Integer(Msg));
    end;
  end;

begin
  with WindowsMessage do
  begin
    case Msg of
      WM_LBUTTONDOWN, WM_LBUTTONUP:
          WLParam := Format('X: %4d Y: %4d ', [WParamLo, LParamLo]);
      WM_KEYDOWN, WM_KEYUP :
          WLParam := Format('Virtual Keycode=%s ', [VirtualKeyCodeToName(WParamBytes[0])]);
    end;

    Result := Format('%-22s %-35s Time=%10d Handle=%8x Control=', [MessageName(Msg), WLParam, Time, WindowHandle]);

    if Assigned(FParentMessageList) and Assigned(FParentMessageList.WinControlList) then
    begin
      MacroWinControl := FParentMessageList.WinControlList.WinControlByHandle(WindowHandle);
      if Assigned(MacroWinControl)  then
        Result:= Result + MacroWinControl.WinControl.Name // f_handle_to_name(m_window_handle)
      else
        Result := Result + 'Not found';
    end
    else
      Result:= Result + '-';
  end;
end;

{ TMacroMessageList }

constructor TMacroMessageList.Create(WinControl: TWinControl);
begin
  inherited Create;
  FWinControlList := TMacroWinControlList.CreateList;
  FWinControlList.BuildList(WinControl);
  FMessageList := TStringList.Create;
end;

destructor TMacroMessageList.Destroy;
var
  i: Integer;
begin
  FWinControlList.Free;
  for i := 0 to FMessageList.Count - 1 do
    FMessageList.Objects[i].Free;
  FMessageList.Free;
  inherited;
end;

procedure TMacroMessageList.Clear;
begin
  FMessageList.Clear;
end;

procedure TMacroMessageList.Add(Name: string; MacroMessage: TMacroMessage);
begin
  MacroMessage.ParentMessageList := Self;
  FMessageList.AddObject(Name, MacroMessage);
end;

function TMacroMessageList.Add(Name: string): TMacroMessage;
begin
  Result := TMacroMessage.Create;
  Add(Name, Result);
end;

function TMacroMessageList.Add(WindowsMessage: PWindowsMessage): TMacroMessage;
begin
  Result := Add('');
  Result.WindowsMessage := WindowsMessage^;
end;

procedure TMacroMessageList.DeleteLast;
begin
  GetMessage(FMessageList.Count - 1).Free;
  FMessageList.Delete(FMessageList.Count - 1);
end;

function TMacroMessageList.GetMessage(Index: Integer): TMacroMessage;
begin
  Result := FMessageList.Objects[Index] as TMacroMessage;
end;

function TMacroMessageList.Count: Integer;
begin
  Result := FMessageList.Count;
end;

procedure TMacroMessageList.UpdateRelative;
var
  i: Integer;
begin
  for i := 0 to FMessageList.Count - 1 do
  with FMessageList.Objects[i] as TMacroMessage, WindowsMessage do
  begin
    Dec(WindowsMessage.Time, FStartTickCount);

    if IsMouseMessage then
    with FWinControlList.WinControlByHandle(WindowsMessage.WindowHandle) do
    begin
      Dec(WParam, ScreenX);
      Dec(LParam, ScreenY);
    end;
  end;
end;

procedure TMacroMessageList.SaveToFile(const FileName: string);
var
  i: Integer;
  F: File of TMacroFile;
  MacroFile: TMacroFile;
begin
  AssignFile(F, FileName);
  Reset(F);
  for i := 0 to FMessageList.Count - 1 do
  with GetMessage(i) do
  begin
    MacroFile.WindowsMessage := WindowsMessage;
    Write(F, MacroFile);
  end;
  Close(F);
end;

procedure TMacroMessageList.LoadFromFile(const FileName: string);
var
  F: File of TMacroFile;
  MacroFile: TMacroFile;
begin
  Clear;
  AssignFile(F, FileName);
  Reset(F);
  while not Eof(F) do
  begin
    Read(F, MacroFile);
    Add(@MacroFile.WindowsMessage)
  end;
  Close(F);
end;

function TMacroMessageList.ToString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FMessageList.Count - 1 do
    Result := Result + GetMessage(i).ToString + Chr(13) + Chr(10);
end;

{ TMacroRecorder }

class function TMacroRecorder.ClassCreate(WinControl: TWinControl): TMacroRecorder;
begin
  FMacroRecorder := TMacroRecorder.Create;
  with FMacroRecorder do
  begin
    FIsRecording := False;
    FIsPaused := False;

    FMacroMessageList := TMacroMessageList.Create(WinControl);

    FNextMacroMessage := TMacroMessage.Create;
    FNextMacroMessage.ParentMessageList := FMacroMessageList;
  end;
  Result := FMacroRecorder;
end;

destructor TMacroRecorder.Destroy;
begin
  FMacroMessageList.Free;
  inherited;
end;

{ The JournalRecordProc hook procedure is an application-defined or library-defined callback function used with the
  SetWindowsHookEx function. The function records messages the system removes from the system message queue.
  A JournalRecordProc hook procedure does not need to live in a dynamic-link library. A JournalRecordProc hook
  procedure can live in the application itself.

  WH_JOURNALPLAYBACK Hook Function

  Syntax

  JournalRecordProc(
  nCode: Integer;  // a hook code
  wParam: WPARAM;  // this parameter is not used
  lParam: LPARAM   // a pointer to a TEventMsg structure
  ): LResult;      // returns a wait time in clock ticks }

function JournalRecordHookProc(Code: Integer; WParam: WPARAM; LParam: LPARAM): LResult; stdcall;
var
  WindowsMessage: PWindowsMessage;
begin
  Result := 0;
  with FMacroRecorder do
  begin
    if GetKeyState(VK_PAUSE) < 0 then
    begin
      Stop;
      Result := CallNextHookEx(HookHandle, Code, WParam, LParam);
      Exit;
    end;

    case Code of
      HC_ACTION:
        begin
          WindowsMessage := PWindowsMessage(LParam);
          with WindowsMessage^ do
            if (Msg <> WM_MOUSEMOVE) and (Msg <> $FF) then
              MacroMessageList.Add(WindowsMessage);
        end;
    else
      Result := CallNextHookEx(HookHandle, Code, WParam, LParam);
    end;
  end;
end;

function JournalPlaybackHookProc(Code: Integer; WParam: WPARAM; LParam: LPARAM): LResult; stdcall;
begin
  Result := 0;
  with FMacroRecorder do
  case Code of
    HC_SKIP:
      begin
        Inc(MessageIndex);

        if MessageIndex < MacroMessageList.Count then
          GetMessage(MessageIndex)
        else
          StopPlayback;
      end;
    HC_GETNEXT:
      PWindowsMessage(LParam)^ := NextMacroMessage.WindowsMessage;
  else
    Result := CallNextHookEx(HookHandle, Code, WParam, LParam);
  end;
end;

procedure TMacroRecorder.Start;
begin
  if not FIsRecording then
  begin
    if not FIsPaused then
      FMacroMessageList.Clear;
    FHookHandle := SetWindowsHookEx(WH_JOURNALRECORD, JournalRecordHookProc, hInstance, 0);
  end;
  FIsRecording := True;
  FIsPaused := False;
end;

procedure TMacroRecorder.Stop;
begin
  if FIsRecording then
    UnhookWindowsHookEx(FHookHandle);

  if not FIsPaused then
    FMacroMessageList.UpdateRelative;

  FIsRecording := False;

  { remove the stop click message }
  FMacroMessageList.DeleteLast; { mouse down }
  FMacroMessageList.DeleteLast; { mouse up }
end;

procedure TMacroRecorder.Pause;
begin
  FIsPaused := True;
  Stop;
end;

procedure TMacroRecorder.Play;
begin
  if FIsPlaying then
    Exit;

  MessageIndex := 0;
  FMacroMessageList.StartTickCount := GetTickCount;

  FHookHandle := SetWindowsHookEx(WH_JOURNALPLAYBACK, JournalPlaybackHookProc, hInstance, 0);
  if FHookHandle = 0 then
    Exit;

  FIsPlaying := True;
end;

procedure TMacroRecorder.StopPlayback;
begin
  if FIsPlaying then
    UnhookWindowsHookEx(FHookHandle);
  FIsPlaying := False;
end;

procedure TMacroRecorder.GetMessage(Index: Integer);
begin
  with MacroMessageList.GetMessage(Index) do
    NextMacroMessage.WindowsMessage := WindowsMessage;

  with NextMacroMessage, WindowsMessage do
  begin
    if IsMouseMessage then
    with MacroMessageList.WinControlList.WinControlByHandle(WindowHandle) do
    begin
      Inc(WParam, ScreenX);
      Inc(LParam, ScreenY);
    end;
    Inc(Time, MacroMessageList.StartTickCount);
  end;
end;

end.
