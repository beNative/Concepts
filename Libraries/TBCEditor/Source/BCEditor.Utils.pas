unit BCEditor.Utils;

interface

uses
  Winapi.Windows, System.Math, System.Classes, Vcl.Graphics, System.UITypes, BCEditor.Consts, BCEditor.Types;

function ActivateDropShadow(const AHandle: THandle): Boolean;
function CaseNone(const AChar: Char): Char;
function CaseStringNone(const AString: string): string;
function CaseUpper(const AChar: Char): Char;
function ColorToHex(const AColor: TColor): string;
function ConvertTabs(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean; const AColumns: Boolean): string;
function IsCombiningDiacriticalMark(const AChar: Char): Boolean;
function DeleteWhitespace(const AText: string): string;
function MessageDialog(const AMessage: string; ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
function MiddleColor(AColor1, AColor2: TColor): TColor;
function MinMax(const AValue, AMinValue, AMaxValue: Integer): Integer;
function TextWidth(ACanvas: TCanvas; const AText: string): Integer;
function TextHeight(ACanvas: TCanvas; const AText: string): Integer;
procedure ClearList(var AList: TList);
procedure FreeList(var AList: TList);

implementation

uses
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.SysUtils, System.Character;

function ActivateDropShadow(const AHandle: THandle): Boolean;

  function IsXP: Boolean;
  begin
    Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
      CheckWin32Version(5, 1);
  end;

const
  SPI_SETDROPSHADOW = $1025;
  CS_DROPSHADOW = $00020000;

var
  NewLong: Cardinal;
  B: Boolean;
begin
  B := True;
  if IsXP and SystemParametersInfo(SPI_SETDROPSHADOW, 0, @B, 0) then
  begin
    NewLong := GetClassLong(AHandle, GCL_STYLE);
    NewLong := NewLong or CS_DROPSHADOW;

    Result := SetClassLong(AHandle, GCL_STYLE, NewLong) <> 0;
    if Result then
      SendMessage(AHandle, CM_RECREATEWND, 0, 0);
  end else
    Result := False;
end;

function CaseNone(const AChar: Char): Char;
begin
  Result := AChar;
end;

function CaseStringNone(const AString: string): string;
begin
  Result := AString;
end;

function CaseUpper(const AChar: Char): Char;
begin
  Result := AChar;
  case AChar of
    'a'..'z':
      Result := Char(Word(AChar) and $FFDF);
  end;
end;

function ColorToHex(const AColor: TColor): string;
begin
  Result := IntToHex(GetRValue(AColor), 2) + IntToHex(GetGValue(AColor), 2) + IntToHex(GetBValue(AColor), 2);
end;

function ConvertTabs(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean; const AColumns: Boolean): string;
var
  LPosition: Integer;
  LCount: Integer;
begin
  AHasTabs := False;
  Result := ALine;
  LPosition := 1;
  while True do
  begin
    LPosition := Pos(BCEDITOR_TAB_CHAR, Result, LPosition);
    if LPosition = 0 then
      Break;

    AHasTabs := True;

    Delete(Result, LPosition, Length(BCEDITOR_TAB_CHAR));

    if AColumns then
      LCount := ATabWidth - (LPosition - ATabWidth - 1) mod ATabWidth
    else
      LCount := ATabWidth;

    Insert(StringOfChar(BCEDITOR_SPACE_CHAR, LCount), Result, LPosition);
    Inc(LPosition, LCount);
  end;
end;

function IsCombiningDiacriticalMark(const AChar: Char): Boolean;
begin
  case Word(AChar) of
    $0300..$036F, $1DC0..$1DFF, $20D0..$20FF:
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
  LIndex: Integer;
begin
  if not Assigned(AList) then
    Exit;
  for LIndex := 0 to AList.Count - 1 do
    if Assigned(AList[LIndex]) then
    begin
      TObject(AList[LIndex]).Free;
      AList[LIndex] := nil;
    end;
  AList.Clear;
end;

function DeleteWhitespace(const AText: string): string;
var
  LIndex, LIndex2: Integer;
begin
  SetLength(Result, Length(AText));
  LIndex2 := 0;
  for LIndex := 1 to Length(AText) do
    if not AText[LIndex].IsWhiteSpace then
    begin
      Inc(LIndex2);
      Result[LIndex2] := AText[LIndex];
    end;
  SetLength(Result, LIndex2);
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

function MinMax(const AValue, AMinValue, AMaxValue: Integer): Integer;
var
  LValue: Integer;
begin
  LValue := Min(AValue, AMaxValue);
  Result := Max(LValue, AMinValue);
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
