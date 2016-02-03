unit BCCommon.Utils;

interface

uses
  Winapi.Windows, System.Classes, System.Types, BCControl.ComboBox, Vcl.Controls;

function BrowseURL(const AURL: string; const ABrowserPath: string = ''): Boolean;
function GetOSInfo: string;
function InsertTextToCombo(ComboBox: TBCComboBox): Integer;
function SetFormInsideWorkArea(Left, Width: Integer): Integer;
function PostInc(var i: Integer): Integer; inline;
procedure InsertItemsToComboBox(AItems: TStrings; AComboBox: TBCComboBox);
procedure AlignSliders(AWinControl: TWinControl);

implementation

uses
  System.SysUtils, System.IOUtils, Winapi.ShellApi, Vcl.Forms, sLabel, BCCommon.StringUtils, BCCommon.WindowsInfo;

function BrowseURL(const AURL: string; const ABrowserPath: string = ''): Boolean;
begin
  if ABrowserPath = '' then
    Result := ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL) > 32
  else
    Result := ShellExecute(0, 'open', PChar(ABrowserPath), PChar(AURL), nil, SW_SHOWNORMAL) > 32
end;

function GetOSInfo: string;
begin
  Result := GetWindowsVersionString + ' ' + GetWindowsEditionString;
end;

procedure InsertItemsToComboBox(AItems: TStrings; AComboBox: TBCComboBox);
var
  i: Integer;
  s: string;
begin
  AComboBox.Clear;
  for i := AItems.Count - 1 downto 0 do
  begin
    s := GetTokenAfter('=', AItems.Strings[i]);
    if AComboBox.Items.IndexOf(s) = -1 then
      AComboBox.Items.Add(s);
  end;
end;

function InsertTextToCombo(ComboBox: TBCComboBox): Integer;
var
  s: string;
  i: Integer;
begin
  Result := -1;
  with ComboBox do
  begin
    s := Text;
    if s <> '' then
    begin
      i := Items.IndexOf(s);
      if i > -1 then
        Items.Delete(i);
      Items.Insert(0, s);
      Text := s;
      if i = -1 then
        Result := ComboBox.Items.Count - 1;
    end;
  end;
end;

function SetFormInsideWorkArea(Left, Width: Integer): Integer;
var
  i: Integer;
  ScreenPos: Integer;
  LMonitor: TMonitor;
begin
  Result := Left;
  { check if the application is outside left side }
  ScreenPos := 0;
  for i := 0 to Screen.MonitorCount - 1 do
  begin
    LMonitor := Screen.Monitors[i];
    if LMonitor.WorkareaRect.Left < ScreenPos then
      ScreenPos := LMonitor.WorkareaRect.Left;
  end;
  if Left + Width < ScreenPos then
    Result := (Screen.Width - Width) div 2;
  { check if the application is outside right side }
  ScreenPos := 0;
  for i := 0 to Screen.MonitorCount - 1 do
  begin
    LMonitor := Screen.Monitors[i];
    if LMonitor.WorkareaRect.Right > ScreenPos then
      ScreenPos := LMonitor.WorkareaRect.Right;
  end;
  if Left > ScreenPos then
    Result := (Screen.Width - Width) div 2;
end;

function PostInc(var i: Integer): Integer; inline;
begin
  Result := i;
  Inc(i)
end;

procedure AlignSliders(AWinControl: TWinControl);
var
  i: Integer;
  LMaxLength: Integer;
  LLabel: TsStickyLabel;
begin
  LMaxLength := 0;
  for i := 0 to AWinControl.ControlCount - 1 do
    if AWinControl.Controls[i] is TsStickyLabel then
    begin
      LLabel := AWinControl.Controls[i] as TsStickyLabel;
      LLabel.AutoSize := True;
      if LLabel.Width > LMaxLength then
        LMaxLength := LLabel.Width;
      LLabel.AutoSize := False;
    end;
  for i := 0 to AWinControl.ControlCount - 1 do
    if AWinControl.Controls[i] is TsStickyLabel then
    begin
      LLabel := AWinControl.Controls[i] as TsStickyLabel;
      LLabel.Width := LMaxLength;
    end;
end;

end.
