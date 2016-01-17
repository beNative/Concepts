unit BCEditor.Utils;

interface

uses
  Winapi.Windows, System.Math, System.Classes, Vcl.Graphics, System.UITypes, BCEditor.Consts, BCEditor.Types;

function DeleteWhitespace(const AText: string): string;
function GetTabConvertProc(ATabWidth: Integer): TBCEditorTabConvertProc;
function MessageDialog(const AMessage: string; ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
function MinMax(AValue, AMinValue, AMaxValue: Integer): Integer;
function TextWidth(ACanvas: TCanvas; const AText: string): Integer;
function TextHeight(ACanvas: TCanvas; const AText: string): Integer;
procedure ClearList(var AList: TList);
procedure FreeList(var AList: TList);

implementation

uses
  Vcl.Forms, Vcl.Dialogs, System.SysUtils, System.Character;

procedure FreeList(var AList: TList);
begin
  ClearList(AList);
  if Assigned(AList) then
  begin
    AList.Free;
    AList := nil;
  end;
end;

procedure ClearList(var AList: TList);
var
  i: Integer;
begin
  if not Assigned(AList) then
    Exit;
  for i := 0 to AList.Count - 1 do
    if Assigned(AList[i]) then
    begin
      TObject(AList[i]).Free;
      AList[i] := nil;
    end;
  AList.Clear;
end;

function DeleteWhitespace(const AText: string): string;
var
  i, j: Integer;
begin
  SetLength(Result, Length(AText));
  j := 0;
  for i := 1 to Length(AText) do
    if not AText[i].IsWhiteSpace then
    begin
      Inc(j);
      Result[j] := AText[i];
    end;
  SetLength(Result, j);
end;

function MessageDialog(const AMessage: string; ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
begin
  with CreateMessageDialog(AMessage, ADlgType, AButtons) do
    try
      HelpContext := 0;
      HelpFile := '';
      Position := poMainFormCenter;
      Result := ShowModal;
    finally
      Free;
    end;
end;

function MinMax(AValue, AMinValue, AMaxValue: Integer): Integer;
begin
  AValue := Min(AValue, AMaxValue);
  Result := Max(AValue, AMinValue);
end;

function GetHasTabs(ALine: PChar; var ACharsBefore: Integer): Boolean;
begin
  Result := False;
  ACharsBefore := 0;
  if Assigned(ALine) then
  begin
    while ALine^ <> BCEDITOR_NONE_CHAR do
    begin
      if ALine^ = BCEDITOR_TAB_CHAR then
        Exit(True);
      Inc(ACharsBefore);
      Inc(ALine);
    end;
  end
end;

function ConvertTabs(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean): string;
var
  PSource: PChar;
begin
  AHasTabs := False;
  Result := '';
  PSource := PChar(ALine);
  while PSource^ <> BCEDITOR_NONE_CHAR do
  begin
    if PSource^ = BCEDITOR_TAB_CHAR then
    begin
      AHasTabs := True;
      Result := Result + StringOfChar(BCEDITOR_SPACE_CHAR, ATabWidth);
    end
    else
      Result := Result + PSource^;
    Inc(PSource);
  end;
end;

function GetTabConvertProc(ATabWidth: Integer): TBCEditorTabConvertProc;
begin
  Result := TBCEditorTabConvertProc(@ConvertTabs);
end;

function TextWidth(ACanvas: TCanvas; const AText: string): Integer;
var
  LSize: TSize;
begin
  GetTextExtentPoint32(ACanvas.Handle, PChar(AText), Length(AText), LSize);
  Result := LSize.cx;
end;

function TextHeight(ACanvas: TCanvas; const AText: string): Integer;
var
  LSize: TSize;
begin
  GetTextExtentPoint32(ACanvas.Handle, PChar(AText), Length(AText), LSize);
  Result := LSize.cy;
end;

end.
