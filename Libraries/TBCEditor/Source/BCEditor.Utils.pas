unit BCEditor.Utils;

interface

uses
  Winapi.Windows, System.Math, System.Classes, Vcl.Graphics, System.UITypes, BCEditor.Consts, BCEditor.Types;

function CeilOfIntDiv(ADividend: Cardinal; ADivisor: Word): Word;
function DeleteWhitespace(const AText: string): string;
function GetTabConvertProc(ATabWidth: Integer): TBCEditorTabConvertProc;
function GetTextSize(AHandle: HDC; AText: PChar; ACount: Integer): TSize;
function MessageDialog(const AMessage: string; ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
function MinMax(AValue, AMinValue, AMaxValue: Integer): Integer;
function TextExtent(ACanvas: TCanvas; const AText: string): TSize;
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

function CeilOfIntDiv(ADividend: Cardinal; ADivisor: Word): Word;
var
  LRemainder: Word;
begin
  DivMod(ADividend, ADivisor, Result, LRemainder);
  if LRemainder > 0 then
    Inc(Result);
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

function GetTextSize(AHandle: HDC; AText: PChar; ACount: Integer): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
  GetTextExtentPoint32W(AHandle, AText, ACount, Result);
end;

type
  TAccessCanvas = class(TCanvas);

function TextExtent(ACanvas: TCanvas; const AText: string): TSize;
begin
  with TAccessCanvas(ACanvas) do
  begin
    RequiredState([csHandleValid, csFontValid]);
    Result := GetTextSize(Handle, PChar(AText), Length(AText));
  end;
end;

function TextWidth(ACanvas: TCanvas; const AText: string): Integer;
begin
  Result := TextExtent(ACanvas, AText).cx;
end;

function TextHeight(ACanvas: TCanvas; const AText: string): Integer;
begin
  Result := TextExtent(ACanvas, AText).cy;
end;

end.
