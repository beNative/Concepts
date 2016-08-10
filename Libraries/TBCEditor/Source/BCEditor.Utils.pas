unit BCEditor.Utils;

interface

uses
  Winapi.Windows, System.Math, System.Classes, Vcl.Graphics, System.UITypes, BCEditor.Consts, BCEditor.Types;

function IsCombiningDiacriticalMark(const AChar: Char): Boolean;
function DeleteWhitespace(const AText: string): string;
function GetTabConvertProc(AColumns: Boolean): TBCEditorTabConvertProc;
function MessageDialog(const AMessage: string; ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
function MiddleColor(AColor1, AColor2: TColor): TColor;
function MinMax(AValue, AMinValue, AMaxValue: Integer): Integer;
function TextWidth(ACanvas: TCanvas; const AText: string): Integer;
function TextHeight(ACanvas: TCanvas; const AText: string): Integer;
procedure ClearList(var AList: TList);
procedure FreeList(var AList: TList);

implementation

uses
  Vcl.Forms, Vcl.Dialogs, System.SysUtils, System.Character;

function IsCombiningDiacriticalMark(const AChar: Char): Boolean;
begin
  case Word(AChar) of
    $0300..$036F:
      Result := True
  else
    Result := False;
  end;
end;

function MiddleColor(AColor1, AColor2: TColor): TColor;
var
  LRed, LGreen, LBlue: Byte;
  LRed1, LGreen1, LBlue1: Byte;
  LRed2, LGreen2, LBlue2: Byte;
begin
  LRed1 := GetRValue(AColor1);
  LRed2 := GetRValue(AColor2);
  LGreen1 := GetRValue(AColor1);
  LGreen2 := GetRValue(AColor2);
  LBlue1 := GetRValue(AColor1);
  LBlue2 := GetRValue(AColor2);

  LRed := (LRed1 + LRed2) div 2;
  LGreen := (LGreen1 + LGreen2) div 2;
  LBlue := (LBlue1 + LBlue2) div 2;

  Result := RGB(LRed, LGreen, LBlue);
end;

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

function ConvertTabs(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean; const ATabChar: Char): string;
var
  PLine: PChar;
begin
  AHasTabs := False;
  Result := '';
  PLine := PChar(ALine);
  while PLine^ <> BCEDITOR_NONE_CHAR do
  begin
    if PLine^ = BCEDITOR_TAB_CHAR then
    begin
      AHasTabs := True;
      Result := Result + StringOfChar(ATabChar, ATabWidth);
    end
    else
      Result := Result + PLine^;
    Inc(PLine);
  end;
end;

function ConvertColumnTabs(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean; const ATabChar: Char): string;
var
  LPLine: PChar;
begin
  AHasTabs := False;
  Result := '';
  LPLine := PChar(ALine);
  while LPLine^ <> BCEDITOR_NONE_CHAR do
  begin
    if LPLine^ = BCEDITOR_TAB_CHAR then
    begin
      AHasTabs := True;
      Result := Result + StringOfChar(ATabChar, ATabWidth - Length(Result) mod ATabWidth);
    end
    else
      Result := Result + LPLine^;
    Inc(LPLine);
  end;
end;

function GetTabConvertProc(AColumns: Boolean): TBCEditorTabConvertProc;
begin
  if AColumns then
    Result := TBCEditorTabConvertProc(@ConvertColumnTabs)
  else
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
