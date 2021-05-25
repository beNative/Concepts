{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit klog; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  StdCtrls, Classes, ComCtrls, ExtCtrls, KFunctions;

type
  TKLogDirection = (
    ldAddTop,
    ldAddBottom
  );

const
  cHoverTimeDef = 1000;
  cLogDirectionDef = ldAddTop;
  cLogMaskDef = [lgAll];
  cStatusPanelDef = -1; // SimpleText property

type
  TKLogMask = set of TKLogType;

  TKLogEvent = procedure(Sender: TObject; Code: TKLogType; const Text: string) of object;

  TKLogProc = procedure(Code: TKLogType; const Text: string);

  TKLogEventObject = class(TObject)
  private
    FCode: TKLogType;
    FText: string;
  public
    constructor Create;
    property Code: TKLogType read FCode write FCode;
    property Text: string read FText write FText;
  end;

  TKLogEventObjects = class(TKObjectList)
  private
    function GetItem(Index: Integer): TKLogEventObject;
    procedure SetItem(Index: Integer; const Value: TKLogEventObject);
  public
    property Items[Index: Integer]: TKLogEventObject read GetItem write SetItem; default;
  end;

  TKLog = class(TComponent)
  private
    FEvents: TKLogEventObjects;
    FHoverTime: Cardinal;
    FInternalStorage: Boolean;
    FListBox: TListBox;
    FLogDirection: TKLogDirection;
    FLogMask: TKLogMask;
    FLogText: string;
    FStatusBar: TStatusBar;
    FStatusCode: TKLogType;
    FStatusPanel: Integer;
    FStatusTimer: TTimer;
    FStatusText: string;
    FOnLog: TKLogEvent;
    procedure SetStatusText(const Value: string);
    procedure SetInternalStorage(const Value: Boolean);
  protected
    procedure ClearStatusBar;
    function LogTypeToText(Code: TKLogType): string;
    procedure SetHoverTime(Value: Cardinal);
    procedure StatusLogTimer(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Log(Code: TKLogType; const Text: string);
    procedure LogStr(const BracketText, Text: string; Code: TKLogType = lgNone);
    procedure StatusLog(Code: TKLogType; const Text: string);
    procedure StatusLogStr(const BracketText, Text: string);
  published
    property Events: TKLogEventObjects read FEvents;
    property HoverTime: Cardinal read FHoverTime write SetHoverTime default cHoverTimeDef;
    property InternalStorage: Boolean read FInternalStorage write SetInternalStorage;
    property ListBox: TListBox read FListBox write FListBox;
    property LogDirection: TKLogDirection read FLogDirection write FLogDirection default cLogDirectionDef;
    property LogMask: TKLogMask read FLogMask write FLogMask default cLogMaskDef;
    property LogText: string read FLogText;
    property StatusBar: TStatusBar read FStatusBar write FStatusBar;
    property StatusCode: TKLogType read FStatusCode;
    property StatusPanel: Integer read FStatusPanel write FStatusPanel default cStatusPanelDef;
    property StatusText: string read FStatusText write SetStatusText;
    property OnLog: TKLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  SysUtils, KRes;

{ TLogEventObject }

constructor TKLogEventObject.Create;
begin
  FCode := lgNone;
  FText := '';
end;

{ TKLogEventObjects }

function TKLogEventObjects.GetItem(Index: Integer): TKLogEventObject;
begin
  Result := TKLogEventObject(inherited GetItem(Index));
end;

procedure TKLogEventObjects.SetItem(Index: Integer; const Value: TKLogEventObject);
begin
  inherited SetItem(Index, Value);
end;

{ TKLog }
constructor TKLog.Create(AOwner: TComponent);
begin
  inherited;
  FHoverTime := cHoverTimeDef;
  FInternalStorage := False;
  FListBox := nil;
  FEvents := TKLogEventObjects.Create;
  FLogDirection := cLogDirectionDef;
  FLogMask := cLogMaskDef;
  FLogText := '';
  FStatusBar := nil;
  FStatusPanel := cStatusPanelDef;
  FStatusTimer := TTimer.Create(Self);
  FStatusTimer.OnTimer := StatusLogTimer;
  FStatusText := '';
  FStatusCode := lgNone;
  FOnLog := nil;
end;

destructor TKLog.Destroy;
begin
  FListBox := nil;
  FEvents.Free;
  FStatusBar := nil;
  FStatusTimer.Free;
  inherited;
end;

procedure TKLog.Clear;
begin
  if FListBox <> nil then
    FListBox.Clear;
  FStatusText := '';
  ClearStatusBar;
end;

procedure TKLog.ClearStatusBar;
begin
  if FStatusBar <> nil then
  begin
    if FStatusPanel < 0 then
      FStatusBar.SimpleText := ''
    else if FStatusPanel < FStatusBar.Panels.Count then
      FStatusBar.Panels[FStatusPanel].Text := '';
  end;
end;

function TKLog.LogTypeToText(Code: TKLogType): string;
begin
  Result := '';
  if [Code, lgAll] * FLogMask <> [] then
    case Code of
      lgError: Result := sLogError;
      lgWarning: Result := sLogWarning;
      lgNote: Result := sLogNote;
      lgHint: Result := sLogHint;
      lgInfo: Result := sLogInfo;
      lgInputError: Result := sLogInputError;
      lgIOError: Result := sLogIOError;
    end;
end;

procedure TKLog.Log(Code: TKLogType; const Text: string);
var
  S: string;
begin
  S := LogTypeToText(Code);
  if (S <> '') or (Code = lgNone) then
    LogStr(S, Text, Code);
end;

procedure TKLog.LogStr(const BracketText, Text: string; Code: TKLogType);
var
  Event: TKLogEventObject;
begin
  if BracketText <> '' then
    FLogText := Format('%s: [%s] %s', [FormatDateTime('dd.mm.yy hh:nn:ss', Now), BracketText, Text])
  else
    FLogText:= Format('%s: %s', [FormatDateTime('dd.mm.yy hh:nn:ss', Now), Text]);
  if FListBox <> nil then
  begin
    if FLogDirection = ldAddTop then
    begin
      FListBox.Items.Insert(0, FLogText);
      if not FListBox.MultiSelect then FListBox.ItemIndex := 0;
    end else
    begin
      FListBox.Items.Add(FLogText);
      if not FListBox.MultiSelect then FListBox.ItemIndex := FListBox.Items.Count - 1;
    end
  end;
  if FInternalStorage then
  begin
    Event := TKLogEventObject.Create;
    Event.Code := Code;
    Event.Text := FLogText;
    if FLogDirection = ldAddTop then
      FEvents.Add(Event)
    else
      FEvents.Insert(0, Event)
  end;
  if Assigned(FOnLog) then
    FOnLog(Self, Code, FLogText);
end;

procedure TKLog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FListBox then
      FListBox := nil
    else if AComponent = FStatusBar then
      FStatusBar := nil;
end;

procedure TKLog.SetHoverTime(Value: Cardinal);
begin
  if Value <> FHoverTime then
  begin
    FHoverTime := Value;
    FStatusTimer.Interval := FHoverTime;
  end;
end;

procedure TKLog.SetInternalStorage(const Value: Boolean);
begin
  if Value <> FInternalStorage then
  begin
    FInternalStorage := Value;
    FEvents.Clear;
  end;
end;

procedure TKLog.SetStatusText(const Value: string);
begin
  if (Value <> FStatusText) and (Value <> '') then
  begin
    FStatusText := Value;
    if FStatusBar <> nil then
    begin
      if FStatusPanel < 0 then
        FStatusBar.SimpleText := FStatusText
      else if FStatusPanel < FStatusBar.Panels.Count then
        FStatusBar.Panels[FStatusPanel].Text := FStatusText;
    end;
    FStatusTimer.Enabled := False;
    FStatusTimer.Enabled := True;
  end;
end;

procedure TKLog.StatusLog(Code: TKLogType; const Text: string);
var
  S: string;
begin
  S := LogTypeToText(Code);
  if (S <> '') or (Code = lgNone) then
  begin
    FStatusCode := Code;
    StatusLogStr(S, Text);
  end;
end;

procedure TKLog.StatusLogStr(const BracketText, Text: string);
begin
  if BracketText <> '' then
    FStatusText := Format('[%s] %s', [BracketText, Text])
  else
    FStatusText := Text;
  if FStatusBar <> nil then
  begin
    if FStatusPanel < 0 then
      FStatusBar.SimpleText := FStatusText
    else if FStatusPanel < FStatusBar.Panels.Count then
      FStatusBar.Panels[FStatusPanel].Text := FStatusText;
  end;
  FStatusTimer.Enabled := False;
  FStatusTimer.Enabled := True;
end;

procedure TKLog.StatusLogTimer(Sender: TObject);
begin
  FStatusTimer.Enabled := False;
  FStatusText := '';
  FStatusCode := lgNone;
  ClearStatusBar;
end;

end.
