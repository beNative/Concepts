unit BCEditor.MacroRecorder;

interface

uses
  Winapi.Windows, System.WideStrUtils, System.Classes, Vcl.Controls, Vcl.Graphics, BCEditor.Language,
  Vcl.Menus, BCEditor.Editor.Base, BCEditor.Editor.KeyCommands, BCEditor.Types, System.SysUtils;

type
  TBCEditorMacroState = (msStopped, msRecording, msPlaying, msPaused);

  TBCEditorMacroEvent = class(TObject)
  protected
    FRepeatCount: Byte;
    function GetAsString: string; virtual; abstract;
    procedure InitEventParameters(AString: string); virtual; abstract;
  public
    constructor Create; virtual;
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); virtual; abstract;
    procedure LoadFromStream(AStream: TStream); virtual; abstract;
    procedure Playback(AEditor: TBCBaseEditor); virtual; abstract;
    procedure SaveToStream(AStream: TStream); virtual; abstract;
    property AsString: string read GetAsString;
    property RepeatCount: Byte read FRepeatCount write FRepeatCount;
  end;

  TBCEditorBasicEvent = class(TBCEditorMacroEvent)
  protected
    FCommand: TBCEditorCommand;
    function GetAsString: string; override;
    procedure InitEventParameters(AString: string); override;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
    procedure SaveToStream(AStream: TStream); override;
  public
    property Command: TBCEditorCommand read FCommand write FCommand;
  end;

  TBCEditorCharEvent = class(TBCEditorMacroEvent)
  protected
    FKey: Char;
    function GetAsString: string; override;
    procedure InitEventParameters(AString: string); override;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
    procedure SaveToStream(AStream: TStream); override;
  public
    property Key: Char read FKey write FKey;
  end;

  TBCEditorStringEvent = class(TBCEditorMacroEvent)
  protected
    FString: string;
    function GetAsString: string; override;
    procedure InitEventParameters(AString: string); override;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
    procedure SaveToStream(AStream: TStream); override;
  public
    property Value: string read FString write FString;
  end;

  TBCEditorPositionEvent = class(TBCEditorBasicEvent)
  protected
    FPosition: TBCEditorTextPosition;
    function GetAsString: string; override;
    procedure InitEventParameters(AString: string); override;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
    procedure SaveToStream(AStream: TStream); override;
  public
    property Position: TBCEditorTextPosition read FPosition write FPosition;
  end;

  TBCEditorDataEvent = class(TBCEditorBasicEvent)
  protected
    FData: Pointer;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
    procedure SaveToStream(AStream: TStream); override;
  end;

  TBCBaseEditorMacroRecorder = class;

  TBCEditorUserCommandEvent = procedure(aSender: TBCBaseEditorMacroRecorder; ACommand: TBCEditorCommand;
    var AEvent: TBCEditorMacroEvent) of object;

  TBCBaseEditorMacroRecorder = class(TComponent)
  strict private
    FMacroName: string;
    FOnStateChange: TNotifyEvent;
    FOnUserCommand: TBCEditorUserCommandEvent;
    FPlaybackShortCut: TShortCut;
    FRecordShortCut: TShortCut;
    FSaveMarkerPos: Boolean;
    function GetAsString: string;
    function GetEditor: TBCBaseEditor;
    function GetEditorCount: Integer;
    function GetEditors(AIndex: Integer): TBCBaseEditor;
    function GetEvent(AIndex: Integer): TBCEditorMacroEvent;
    function GetEventCount: Integer;
    procedure SetAsString(const AValue: string);
    procedure SetEditor(const AValue: TBCBaseEditor);
  protected
    FCurrentEditor: TBCBaseEditor;
    FEditors: TList;
    FEvents: TList;
    FPlaybackCommandID: TBCEditorCommand;
    FRecordCommandID: TBCEditorCommand;
    FState: TBCEditorMacroState;
    function CreateMacroEvent(ACommand: TBCEditorCommand): TBCEditorMacroEvent;
    function GetIsEmpty: Boolean;
    procedure DoAddEditor(AEditor: TBCBaseEditor);
    procedure DoRemoveEditor(AEditor: TBCBaseEditor);
    procedure Notification(AComponent: TComponent; aOperation: TOperation); override;
    procedure OnCommand(ASender: TObject; AAfterProcessing: Boolean; var AHandled: Boolean; var ACommand: TBCEditorCommand;
      var AChar: Char; AData: Pointer; AHandlerData: Pointer);
    procedure SetPlaybackShortCut(const AValue: TShortCut);
    procedure SetRecordShortCut(const AValue: TShortCut);
    procedure StateChanged;
  protected
    procedure HookEditor(AEditor: TBCBaseEditor; ACommandID: TBCEditorCommand; AOldShortCut, ANewShortCut: TShortCut);
    procedure UnHookEditor(AEditor: TBCBaseEditor; ACommandID: TBCEditorCommand; AShortCut: TShortCut);
    property PlaybackCommandID: TBCEditorCommand read FPlaybackCommandID;
    property RecordCommandID: TBCEditorCommand read FRecordCommandID;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddEditor(AEditor: TBCBaseEditor): Integer;
    function RemoveEditor(AEditor: TBCBaseEditor): Integer;
    procedure AddCustomEvent(AEvent: TBCEditorMacroEvent);
    procedure AddEvent(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
    procedure Clear;
    procedure DeleteEvent(AIndex: Integer);
    procedure Error(const AMessage: string);
    procedure InsertCustomEvent(AIndex: Integer; AEvent: TBCEditorMacroEvent);
    procedure InsertEvent(AIndex: Integer; ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromStream(ASource: TStream; AClear: Boolean = True);
    procedure Pause;
    procedure PlaybackMacro(AEditor: TBCBaseEditor);
    procedure RecordMacro(AEditor: TBCBaseEditor);
    procedure Resume;
    procedure SaveToFile(const AFilename: string);
    procedure SaveToStream(ADestination: TStream);
    procedure Stop;
    property AsString: string read GetAsString write SetAsString;
    property EditorCount: Integer read GetEditorCount;
    property Editors[AIndex: Integer]: TBCBaseEditor read GetEditors;
    property EventCount: Integer read GetEventCount;
    property Events[AIndex: Integer]: TBCEditorMacroEvent read GetEvent;
    property IsEmpty: Boolean read GetIsEmpty;
    property MacroName: string read FMacroName write FMacroName;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnUserCommand: TBCEditorUserCommandEvent read FOnUserCommand write FOnUserCommand;
    property PlaybackShortCut: TShortCut read FPlaybackShortCut write SetPlaybackShortCut;
    property RecordShortCut: TShortCut read FRecordShortCut write SetRecordShortCut;
    property SaveMarkerPos: Boolean read FSaveMarkerPos write FSaveMarkerPos default False;
    property State: TBCEditorMacroState read FState;
  published
    property Editor: TBCBaseEditor read GetEditor write SetEditor;
  end;

  TBCEditorMacroRecorder = class(TBCBaseEditorMacroRecorder)
  published
    property SaveMarkerPos;
    property RecordShortCut;
    property PlaybackShortCut;
    property OnStateChange;
    property OnUserCommand;
  end;

  EBCEditorMacroRecorderException = class(Exception);

implementation

uses
  Vcl.Forms, BCEditor.Editor.Utils, BCEditor.Consts, System.Types;

{ TBCEditorDatAEvent }

procedure TBCEditorDataEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  FCommand := ACommand;
  Assert(AChar = BCEDITOR_NONE_CHAR);
  FData := AData;
end;

procedure TBCEditorDataEvent.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FData, SizeOf(FData));
end;

procedure TBCEditorDataEvent.Playback(AEditor: TBCBaseEditor);
begin
  AEditor.CommandProcessor(Command, BCEDITOR_NONE_CHAR, FData);
end;

procedure TBCEditorDataEvent.SaveToStream(AStream: TStream);
begin
  inherited;
  AStream.Write(FData, SizeOf(FData));
end;

{ TBCBaseEditorMacroRecorder }

procedure TBCBaseEditorMacroRecorder.AddCustomEvent(AEvent: TBCEditorMacroEvent);
begin
  InsertCustomEvent(EventCount, AEvent);
end;

function TBCBaseEditorMacroRecorder.AddEditor(AEditor: TBCBaseEditor): Integer;
begin
  if not Assigned(FEditors) then
    FEditors := TList.Create
  else
  if FEditors.IndexOf(AEditor) >= 0 then
  begin
    Result := -1;
    Exit;
  end;
  AEditor.FreeNotification(Self);
  Result := FEditors.Add(AEditor);
  DoAddEditor(AEditor);
end;

procedure TBCBaseEditorMacroRecorder.AddEvent(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  InsertEvent(EventCount, ACommand, AChar, AData);
end;

procedure TBCBaseEditorMacroRecorder.Clear;
var
  I: Integer;
  LObject: TObject;
begin
  if Assigned(FEvents) then
  begin
    for I := FEvents.Count - 1 downto 0 do
    begin
      LObject := FEvents[I];
      FEvents.Delete(I);
      LObject.Free;
    end;
    FEvents.Free;
    FEvents := nil;
  end;
end;

const
  ecPluginBase = 64000;

var
  GCurrentCommand: Integer = ecPluginBase;

function NewPluginCommand: TBCEditorCommand;
begin
  Result := GCurrentCommand;
  Inc(GCurrentCommand);
end;

constructor TBCBaseEditorMacroRecorder.Create(AOwner: TComponent);
begin
  inherited;
  FMacroName := 'unnamed';
  FRecordCommandID := NewPluginCommand;
  FPlaybackCommandID := NewPluginCommand;
  FRecordShortCut := Vcl.Menus.ShortCut(Ord('R'), [ssCtrl, ssShift]);
  FPlaybackShortCut := Vcl.Menus.ShortCut(Ord('P'), [ssCtrl, ssShift]);
end;

function TBCBaseEditorMacroRecorder.CreateMacroEvent(ACommand: TBCEditorCommand): TBCEditorMacroEvent;

  function WantDefaultEvent(var AEvent: TBCEditorMacroEvent): Boolean;
  begin
    if Assigned(OnUserCommand) then
      OnUserCommand(Self, ACommand, AEvent);
    Result := not Assigned(AEvent);
  end;

begin
  case ACommand of
    ecGotoXY, ecSelectionGotoXY, ecSetBookmark1 .. ecSetBookmark9:
      begin
        Result := TBCEditorPositionEvent.Create;
        TBCEditorPositionEvent(Result).Command := ACommand;
      end;
    ecChar:
      Result := TBCEditorCharEvent.Create;
    ecString:
      Result := TBCEditorStringEvent.Create;
  else
    begin
      Result := nil;
      if (ACommand < ecUserFirst) or WantDefaultEvent(Result) then
      begin
        Result := TBCEditorBasicEvent.Create;
        TBCEditorBasicEvent(Result).Command := ACommand;
      end;
    end;
  end;
end;

function TBCBaseEditorMacroRecorder.GetEditors(AIndex: Integer): TBCBaseEditor;
begin
  Result := TBCBaseEditor(FEditors[AIndex]);
end;

procedure TBCBaseEditorMacroRecorder.DeleteEvent(AIndex: Integer);
var
  LObject: Pointer;
begin
  LObject := FEvents[AIndex];
  FEvents.Delete(AIndex);
  TObject(LObject).Free;
end;

procedure ReleasePluginCommand(ACommand: TBCEditorCommand);
begin
  if ACommand = GCurrentCommand - 1 then
    GCurrentCommand := ACommand;
end;

destructor TBCBaseEditorMacroRecorder.Destroy;
begin
  while Assigned(FEditors) do
    RemoveEditor(Editors[0]);
  Clear;
  inherited;
  ReleasePluginCommand(PlaybackCommandID);
  ReleasePluginCommand(RecordCommandID);
end;

function TBCBaseEditorMacroRecorder.GetEditor: TBCBaseEditor;
begin
  if Assigned(FEditors) then
    Result := FEditors[0]
  else
    Result := nil;
end;

procedure TBCBaseEditorMacroRecorder.SetEditor(const AValue: TBCBaseEditor);
var
  LEditor: TBCBaseEditor;
begin
  LEditor := Editor;
  if LEditor <> AValue then
    try
      if Assigned(LEditor) and (FEditors.Count = 1) then
        RemoveEditor(LEditor);
      if Assigned(AValue) then
        AddEditor(AValue);
    except
      if [csDesigning] * ComponentState = [csDesigning] then
        Application.HandleException(Self)
      else
        raise;
    end;
end;

function TBCBaseEditorMacroRecorder.GetEditorCount: Integer;
begin
  if Assigned(FEditors) then
    Result := FEditors.Count
  else
    Result := 0;
end;

procedure TBCBaseEditorMacroRecorder.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if AOperation = opRemove then
    if (AComponent = Editor) or (AComponent is TBCBaseEditor) then
      RemoveEditor(TBCBaseEditor(AComponent));
end;

procedure TBCBaseEditorMacroRecorder.DoAddEditor(AEditor: TBCBaseEditor);
begin
  HookEditor(AEditor, RecordCommandID, 0, RecordShortCut);
  HookEditor(AEditor, PlaybackCommandID, 0, PlaybackShortCut);
end;

procedure TBCBaseEditorMacroRecorder.DoRemoveEditor(AEditor: TBCBaseEditor);
begin
  UnHookEditor(AEditor, RecordCommandID, RecordShortCut);
  UnHookEditor(AEditor, PlaybackCommandID, PlaybackShortCut);
end;

procedure TBCBaseEditorMacroRecorder.Error(const AMessage: string);
begin
  raise EBCEditorMacroRecorderException.Create(AMessage);
end;

function TBCBaseEditorMacroRecorder.GetEvent(AIndex: Integer): TBCEditorMacroEvent;
begin
  Result := TBCEditorMacroEvent(FEvents[AIndex]);
end;

function TBCBaseEditorMacroRecorder.GetEventCount: Integer;
begin
  if not Assigned(FEvents) then
    Result := 0
  else
    Result := FEvents.Count;
end;

function TBCBaseEditorMacroRecorder.GetIsEmpty: Boolean;
begin
  Result := not Assigned(FEvents) or (FEvents.Count = 0);
end;

procedure TBCBaseEditorMacroRecorder.InsertCustomEvent(AIndex: Integer; AEvent: TBCEditorMacroEvent);
begin
  if not Assigned(FEvents) then
    FEvents := TList.Create;
  FEvents.Insert(AIndex, AEvent);
end;

procedure TBCBaseEditorMacroRecorder.InsertEvent(AIndex: Integer; ACommand: TBCEditorCommand; AChar: Char;
  AData: Pointer);
var
  LEvent: TBCEditorMacroEvent;
begin
  LEvent := CreateMacroEvent(ACommand);
  try
    LEvent.Initialize(ACommand, AChar, AData);
    InsertCustomEvent(AIndex, LEvent);
  except
    LEvent.Free;
    raise;
  end;
end;

procedure TBCBaseEditorMacroRecorder.LoadFromStream(ASource: TStream; AClear: Boolean = True);
var
  LCommand: TBCEditorCommand;
  LEvent: TBCEditorMacroEvent;
  LCount, i: Integer;
begin
  Stop;
  if AClear then
    Clear;
  FEvents := TList.Create;
  ASource.Read(LCount, SizeOf(LCount));
  i := 0;
  FEvents.Capacity := ASource.Size div SizeOf(TBCEditorCommand);
  while (ASource.Position < ASource.Size) and (i < LCount) do
  begin
    ASource.Read(LCommand, SizeOf(TBCEditorCommand));
    LEvent := CreateMacroEvent(LCommand);
    LEvent.Initialize(LCommand, BCEDITOR_NONE_CHAR, nil);
    LEvent.LoadFromStream(ASource);
    FEvents.Add(LEvent);
    Inc(i);
  end;
end;

procedure TBCBaseEditorMacroRecorder.OnCommand(ASender: TObject; AAfterProcessing: Boolean; var AHandled: Boolean;
  var ACommand: TBCEditorCommand; var AChar: Char; AData, AHandlerData: Pointer);
var
  LEvent: TBCEditorMacroEvent;
begin
  if AAfterProcessing then
  begin
    if (ASender = FCurrentEditor) and (State = msRecording) and (not AHandled) then
    begin
      LEvent := CreateMacroEvent(ACommand);
      LEvent.Initialize(ACommand, AChar, AData);
      FEvents.Add(LEvent);
      if SaveMarkerPos and (ACommand >= ecSetBookmark1) and (ACommand <= ecSetBookmark9) and not Assigned(AData) then
        TBCEditorPositionEvent(LEvent).Position := FCurrentEditor.TextCaretPosition;
    end;
  end
  else
  begin
    { not AfterProcessing }
    case State of
      msStopped:
        if ACommand = RecordCommandID then
        begin
          RecordMacro(TBCBaseEditor(ASender));
          AHandled := True;
        end
        else
        if ACommand = PlaybackCommandID then
        begin
          PlaybackMacro(TBCBaseEditor(ASender));
          AHandled := True;
        end;
      msPlaying:
        ;
      msPaused:
        if ACommand = PlaybackCommandID then
        begin
          Resume;
          AHandled := True;
        end;
      msRecording:
        if ACommand = PlaybackCommandID then
        begin
          Pause;
          AHandled := True;
        end
        else
        if ACommand = RecordCommandID then
        begin
          Stop;
          AHandled := True;
        end;
    end;
  end;
end;

procedure TBCBaseEditorMacroRecorder.Pause;
begin
  if State <> msRecording then
    Error(SBCEditorCannotPause);
  FState := msPaused;
  StateChanged;
end;

procedure TBCBaseEditorMacroRecorder.PlaybackMacro(AEditor: TBCBaseEditor);
var
  i: Integer;
begin
  if State <> msStopped then
    Error(SBCEditorCannotPlay);
  FState := msPlaying;
  try
    StateChanged;
    for i := 0 to EventCount - 1 do
    begin
      Events[i].Playback(AEditor);
      if State <> msPlaying then
        break;
    end;
  finally
    if State = msPlaying then
    begin
      FState := msStopped;
      StateChanged;
    end;
  end;
end;

procedure TBCBaseEditorMacroRecorder.RecordMacro(AEditor: TBCBaseEditor);
begin
  if FState <> msStopped then
    Error(SBCEditorCannotRecord);
  Clear;
  FEvents := TList.Create;
  FEvents.Capacity := 512;
  FState := msRecording;
  FCurrentEditor := AEditor;
  StateChanged;
end;

function TBCBaseEditorMacroRecorder.RemoveEditor(AEditor: TBCBaseEditor): Integer;
begin
  if not Assigned(FEditors) then
  begin
    Result := -1;
    Exit;
  end;
  Result := FEditors.Remove(AEditor);
  if FEditors.Count = 0 then
  begin
    FEditors.Free;
    FEditors := nil;
  end;
  if Result >= 0 then
    DoRemoveEditor(AEditor);
end;

procedure TBCBaseEditorMacroRecorder.Resume;
begin
  if FState <> msPaused then
    Error(SBCEditorCannotResume);
  FState := msRecording;
  StateChanged;
end;

procedure TBCBaseEditorMacroRecorder.SaveToStream(ADestination: TStream);
var
  i, LCount: Integer;
begin
  LCount := EventCount;
  ADestination.Write(LCount, SizeOf(LCount));
  for i := 0 to LCount - 1 do
    Events[i].SaveToStream(ADestination);
end;

procedure TBCBaseEditorMacroRecorder.SetRecordShortCut(const AValue: TShortCut);
var
  i: Integer;
begin
  if FRecordShortCut <> AValue then
  begin
    if Assigned(FEditors) then
      if AValue <> 0 then
      for i := 0 to FEditors.Count - 1 do
        HookEditor(Editors[i], FRecordCommandID, FRecordShortCut, AValue)
      else
      for i := 0 to FEditors.Count - 1 do
        UnHookEditor(Editors[i], FRecordCommandID, FRecordShortCut);
    FRecordShortCut := AValue;
  end;
end;

procedure TBCBaseEditorMacroRecorder.SetPlaybackShortCut(const AValue: TShortCut);
var
  i: Integer;
begin
  if FPlaybackShortCut <> AValue then
  begin
    if Assigned(FEditors) then
      if AValue <> 0 then
      for i := 0 to FEditors.Count - 1 do
        HookEditor(Editors[i], FPlaybackCommandID, FPlaybackShortCut, AValue)
      else
      for i := 0 to FEditors.Count - 1 do
        UnHookEditor(Editors[i], FPlaybackCommandID, FPlaybackShortCut);
    FPlaybackShortCut := AValue;
  end;
end;

procedure TBCBaseEditorMacroRecorder.StateChanged;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

procedure TBCBaseEditorMacroRecorder.Stop;
begin
  if FState = msStopped then
    Exit;
  FState := msStopped;
  FCurrentEditor := nil;
  if FEvents.Count = 0 then
  begin
    FEvents.Free;
    FEvents := nil;
  end;
  StateChanged;
end;

function TBCBaseEditorMacroRecorder.GetAsString: string;
var
  i: Integer;
  s: string;
begin
  Result := 'macro ' + MacroName + SLineBreak + 'begin' + SLineBreak;
  if Assigned(FEvents) then
  begin
    for i := 0 to FEvents.Count - 1 do
    begin
      s := Events[i].AsString;
      if s <> '' then
        Result := Result + '  ' + s + SLineBreak;
    end;
  end;
  Result := Result + 'end';
end;

procedure TBCBaseEditorMacroRecorder.SetAsString(const AValue: string);
var
  i, LPosition, LCommand: Integer;
  LStringList: TStrings;
  LCommandString: string;
  LEvent: TBCEditorMacroEvent;
begin
  Stop;
  Clear;
  FEvents := TList.Create;
  LStringList := TStringList.Create;
  try
    LStringList.Text := AValue;
    for i := 0 to LStringList.Count - 1 do
    begin
      LCommandString := Trim(LStringList[i]);
      LPosition := Pos(' ', LCommandString);
      if LPosition = 0 then
        LPosition := Length(LCommandString) + 1;
      LCommand := ecNone;
      if IdentToEditorCommand(Copy(LCommandString, 1, LPosition - 1), Longint(LCommand)) then
      begin
        Delete(LCommandString, 1, LPosition);
        LEvent := CreateMacroEvent(LCommand);
        try
          FEvents.Add(LEvent);
          LEvent.InitEventParameters(LCommandString);
        except
          LEvent.Free;
        end;
      end;
    end;
  finally
    LStringList.Free;
  end;
end;

procedure TBCBaseEditorMacroRecorder.LoadFromFile(const AFilename: string);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFilename, fmOpenRead);
  try
    LoadFromStream(LFileStream);
    MacroName := ChangeFileExt(ExtractFileName(AFilename), '');
  finally
    LFileStream.Free;
  end;
end;

procedure TBCBaseEditorMacroRecorder.SaveToFile(const AFilename: string);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFilename, fmCreate);
  try
    SaveToStream(LFileStream);
  finally
    LFileStream.Free;
  end;
end;

procedure TBCBaseEditorMacroRecorder.HookEditor(AEditor: TBCBaseEditor; ACommandID: TBCEditorCommand;
  AOldShortCut, ANewShortCut: TShortCut);
var
  LIndex: Integer;
  LKeyCommand: TBCEditorKeyCommand;
begin
  Assert(ANewShortCut <> 0);
  if [csDesigning] * ComponentState = [csDesigning] then
    if TBCBaseEditor(AEditor).KeyCommands.FindShortcut(ANewShortCut) >= 0 then
      raise EBCEditorMacroRecorderException.Create(SBCEditorShortcutAlreadyExists)
    else
      Exit;
  if AOldShortCut <> 0 then
  begin
    LIndex := TBCBaseEditor(AEditor).KeyCommands.FindShortcut(AOldShortCut);
    if LIndex >= 0 then
    begin
      LKeyCommand := TBCBaseEditor(AEditor).KeyCommands[LIndex];
      if LKeyCommand.Command = ACommandID then
      begin
        LKeyCommand.ShortCut := ANewShortCut;
        Exit;
      end;
    end;
  end;
  LKeyCommand := TBCBaseEditor(AEditor).KeyCommands.NewItem;
  try
    LKeyCommand.ShortCut := ANewShortCut;
  except
    LKeyCommand.Free;
    raise;
  end;
  LKeyCommand.Command := ACommandID;
  AEditor.RegisterCommandHandler(OnCommand, Self);
end;

procedure TBCBaseEditorMacroRecorder.UnHookEditor(AEditor: TBCBaseEditor; ACommandID: TBCEditorCommand;
  AShortCut: TShortCut);
var
  LIndex: Integer;
begin
  AEditor.UnregisterCommandHandler(OnCommand);
  if Assigned(AEditor) and Assigned(AEditor.KeyCommands) then
  begin
    LIndex := AEditor.KeyCommands.FindShortcut(AShortCut);
    if (LIndex >= 0) and (AEditor.KeyCommands[LIndex].Command = ACommandID) then
      AEditor.KeyCommands[LIndex].Free;
  end;
end;

{ TBCEditorBasicEvent }

function TBCEditorBasicEvent.GetAsString: string;
var
  LIdent: string;
begin
  EditorCommandToIdent(Command, LIdent);
  Result := LIdent;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TBCEditorBasicEvent.InitEventParameters(AString: string);
begin
  RepeatCount := StrToIntDef(Trim(AString), 1);
end;

procedure TBCEditorBasicEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  Command := ACommand;
end;

procedure TBCEditorBasicEvent.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FRepeatCount, SizeOf(FRepeatCount));
end;

procedure TBCEditorBasicEvent.Playback(AEditor: TBCBaseEditor);
var
  i: Integer;
begin
  for i := 1 to RepeatCount do //FI:W528 FixInsight ignore
    AEditor.CommandProcessor(Command, BCEDITOR_NONE_CHAR, nil);
end;

procedure TBCEditorBasicEvent.SaveToStream(AStream: TStream);
begin
  AStream.Write(Command, SizeOf(TBCEditorCommand));
  AStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TBCEditorCharEvent }

function TBCEditorCharEvent.GetAsString: string;
var
  LIdent: string;
begin
  EditorCommandToIdent(ecChar, LIdent);
  Result := LIdent + ' ' + Key;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TBCEditorCharEvent.InitEventParameters(AString: string);
begin
  if Length(AString) >= 1 then
    Key := AString[1]
  else
    Key := ' ';
  Delete(AString, 1, 1);
  RepeatCount := StrToIntDef(Trim(AString), 1);
end;

procedure TBCEditorCharEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  Key := AChar;
  Assert(not Assigned(AData));
end;

procedure TBCEditorCharEvent.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FKey, SizeOf(Key));
  AStream.Read(FRepeatCount, SizeOf(FRepeatCount));
end;

procedure TBCEditorCharEvent.Playback(AEditor: TBCBaseEditor);
var
  i: Integer;
begin
  for i := 1 to RepeatCount do //FI:W528 FixInsight ignore
    AEditor.CommandProcessor(ecChar, Key, nil);
end;

procedure TBCEditorCharEvent.SaveToStream(AStream: TStream);
const
  CharCommand: TBCEditorCommand = ecChar;
begin
  AStream.Write(CharCommand, SizeOf(TBCEditorCommand));
  AStream.Write(Key, SizeOf(Key));
  AStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TBCEditorPositionEvent }

function TBCEditorPositionEvent.GetAsString: string;
begin
  Result := inherited GetAsString;

  Result := Result + Format(' (%d, %d)', [Position.Char, Position.Line]);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TBCEditorPositionEvent.InitEventParameters(AString: string);
var
  LDotPosition, LOpenPosition, LClosePosition: Integer;
  X, Y: Integer;
  LValue: string;
begin
  inherited;
  AString := Trim(AString);
  LDotPosition := Pos(',', AString);
  LOpenPosition := Pos('(', AString);
  LClosePosition := Pos(')', AString);
  if (not((LDotPosition = 0) or (LOpenPosition = 0) or (LClosePosition = 0))) and ((LDotPosition > LOpenPosition) and
    (LDotPosition < LClosePosition)) then
  begin
    LValue := Copy(AString, LOpenPosition + 1, LDotPosition - LOpenPosition - 1);
    X := StrToIntDef(LValue, 1);
    Delete(AString, 1, LDotPosition);
    AString := Trim(AString);
    LClosePosition := Pos(')', AString);
    LValue := Copy(AString, 1, LClosePosition - 1);
    Y := StrToIntDef(LValue, 1);
    Position := GetTextPosition(X, Y);
    Delete(AString, 1, LClosePosition);
    AString := Trim(AString);
    RepeatCount := StrToIntDef(AString, 1);
  end;
end;

procedure TBCEditorPositionEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  inherited;
  if Assigned(AData) then
    Position := TBCEditorTextPosition(AData^)
  else
    Position := GetTextPosition(0, 0);
end;

procedure TBCEditorPositionEvent.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FPosition, SizeOf(Position));
end;

procedure TBCEditorPositionEvent.Playback(AEditor: TBCBaseEditor);
begin
  if (Position.Char <> 0) or (Position.Line <> 0) then
    AEditor.CommandProcessor(Command, BCEDITOR_NONE_CHAR, @Position)
  else
    AEditor.CommandProcessor(Command, BCEDITOR_NONE_CHAR, nil);
end;

procedure TBCEditorPositionEvent.SaveToStream(AStream: TStream);
begin
  inherited;
  AStream.Write(Position, SizeOf(Position));
end;

{ TBCEditorStringEvent }

function TBCEditorStringEvent.GetAsString: string;
var
  LIdent: string;
begin
  EditorCommandToIdent(ecString, LIdent);
  Result := LIdent + ' ' + WideQuotedStr(Value, #39);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TBCEditorStringEvent.InitEventParameters(AString: string);
var
  LOpenPosition, LClosePosition: Integer;
  LValue: string;

  function WideLastDelimiter(const Delimiters, S: string): Integer;
  var
    P: PChar;
  begin
    Result := Length(S);
    P := PChar(Delimiters);
    while Result > 0 do
    begin
      if (S[Result] <> BCEDITOR_NONE_CHAR) and Assigned(WStrScan(P, S[Result])) then
        Exit;
      Dec(Result);
    end;
  end;

begin
  LOpenPosition := Pos('''', AString);
  LClosePosition := WideLastDelimiter('''', AString);
  LValue := Copy(AString, LOpenPosition + 1, LClosePosition - LOpenPosition - 1);
  Value := StringReplace(LValue, '''''', '''', [rfReplaceAll]);
  Delete(AString, 1, LClosePosition);
  RepeatCount := StrToIntDef(Trim(AString), 1);
end;

procedure TBCEditorStringEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  Value := string(AData);
end;

procedure TBCEditorStringEvent.LoadFromStream(AStream: TStream);
var
  LLength: Integer;
  LPBuffer: PChar;
begin
  AStream.Read(LLength, SizeOf(LLength));
  GetMem(LPBuffer, LLength * SizeOf(Char));
  try
    FillMemory(LPBuffer, LLength, 0);
    AStream.Read(LPBuffer^, LLength * SizeOf(Char));
    FString := LPBuffer;
  finally
    FreeMem(LPBuffer);
  end;
  AStream.Read(FRepeatCount, SizeOf(FRepeatCount));
end;

procedure TBCEditorStringEvent.Playback(AEditor: TBCBaseEditor);
var
  i, j: Integer;
begin
  for i := 1 to RepeatCount do //FI:W528 FixInsight ignore
    for j := 1 to Length(Value) do
      AEditor.CommandProcessor(ecChar, Value[j], nil);
end;

procedure TBCEditorStringEvent.SaveToStream(AStream: TStream);
const
  Command: TBCEditorCommand = ecString;
var
  LLength: Integer;
  LPBuffer: PChar;
begin
  AStream.Write(Command, SizeOf(Command));
  LLength := Length(Value) + 1;
  AStream.Write(LLength, SizeOf(LLength));
  GetMem(LPBuffer, LLength * SizeOf(Char));
  try
    FillMemory(LPBuffer, LLength, 0);
    WStrCopy(LPBuffer, PChar(Value));
    AStream.Write(LPBuffer^, LLength * SizeOf(Char));
  finally
    FreeMem(LPBuffer);
  end;
  AStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TBCEditorMacroEvent }

constructor TBCEditorMacroEvent.Create;
begin
  inherited;

  FRepeatCount := 1;
end;

end.
