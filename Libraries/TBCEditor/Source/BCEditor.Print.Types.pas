unit BCEditor.Print.Types;

interface

uses
  System.Classes, System.SysUtils;

const
  BCEDITOR_DEFAULT_LEFT_MARGIN_MM = 20;
  BCEDITOR_DEFAULT_RIGHT_MARGIN_MM = 15;
  BCEDITOR_DEFAULT_TOP_MARGIN_MM = 18;
  BCEDITOR_DEFAULT_BOTTOM_MM = 18;
  BCEDITOR_DEFAULT_HEADER_MM = 15;
  BCEDITOR_DEFAULT_FOOTER_MM = 15;
  BCEDITOR_DEFAULT_LEFT_TEXT_INDENT_MM = 2;
  BCEDITOR_DEFAULT_RIGHT_TEXT_INDENT_MM = 2;
  BCEDITOR_DEFAULT_INTERNAL_MARGIN_MM = 0.5;
  BCEDITOR_DEFAULT_MARGIN_MM = 0;

type
  TBCEditorFrameType = (ftLine, ftBox, ftShaded);
  TBCEditorFrameTypes = set of TBCEditorFrameType;
  TBCEditorUnitSystem = (usMM, usCm, usInch, muThousandthsOfInches);
  TBCEditorPrintStatus = (psBegin, psNewPage, psEnd);
  TBCEditorPrintStatusEvent = procedure(ASender: TObject; const AStatus: TBCEditorPrintStatus; const APageNumber: Integer;
    var AAbort: Boolean) of object;
  TBCEditorPrintLineEvent = procedure(ASender: TObject; const ALineNumber: Integer; const APageNumber: Integer) of object;

type
  TBCEditorWrapPosition = class
  public
    Index: Integer;
  end;

function IntToRoman(AValue: Integer): string;
function WrapTextEx(const ALine: string; ABreakChars: TSysCharSet; AMaxColumn: Integer; AList: TList): Boolean;

implementation

function WrapTextEx(const ALine: string; ABreakChars: TSysCharSet; AMaxColumn: Integer; AList: TList): Boolean;
var
  LWrapPosition: TBCEditorWrapPosition;
  LPosition, LPreviousPosition: Integer;
  LFound: Boolean;
begin
  if Length(ALine) <= AMaxColumn then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
  LPosition := 1;
  LPreviousPosition := 0;
  LWrapPosition := TBCEditorWrapPosition.Create;
  while LPosition <= Length(ALine) do
  begin
    LFound := (LPosition - LPreviousPosition > AMaxColumn) and (LWrapPosition.Index <> 0);
    if not LFound and (ALine[LPosition] <= High(Char)) and CharInSet(Char(ALine[LPosition]), ABreakChars) then
      LWrapPosition.Index := LPosition;

    if LFound then
    begin
      Result := True;
      AList.Add(LWrapPosition);
      LPreviousPosition := LWrapPosition.Index;

      if ((Length(ALine) - LPreviousPosition) > AMaxColumn) and (LPosition < Length(ALine)) then
        LWrapPosition := TBCEditorWrapPosition.Create
      else
        Break;
    end;
    Inc(LPosition);
  end;

  if (AList.Count = 0) or (AList.Last <> LWrapPosition) then
    LWrapPosition.Free;
end;

function IntToRoman(AValue: Integer): string;
begin
  Result := '';
  while AValue >= 1000 do
  begin
    Result := Result + 'M';
    AValue := AValue - 1000;
  end;

  if AValue >= 900 then
  begin
    Result := Result + 'CM';
    AValue := AValue - 900;
  end;

  while AValue >= 500 do
  begin
    Result := Result + 'D';
    AValue := AValue - 500;
  end;

  if AValue >= 400 then
  begin
    Result := Result + 'CD';
    AValue := AValue - 400;
  end;

  while AValue >= 100 do
  begin
    Result := Result + 'C';
    AValue := AValue - 100;
  end;

  if AValue >= 90 then
  begin
    Result := Result + 'XC';
    AValue := AValue - 90;
  end;

  while AValue >= 50 do
  begin
    Result := Result + 'L';
    AValue := AValue - 50;
  end;

  if AValue >= 40 then
  begin
    Result := Result + 'XL';
    AValue := AValue - 40;
  end;

  while AValue >= 10 do
  begin
    Result := Result + 'X';
    AValue := AValue - 10;
  end;

  if AValue >= 9 then
  begin
    Result := Result + 'IX';
    AValue := AValue - 9;
  end;

  while AValue >= 5 do
  begin
    Result := Result + 'V';
    AValue := AValue - 5;
  end;

  if AValue >= 4 then
  begin
    Result := Result + 'IV';
    AValue := AValue - 4;
  end;

  while AValue > 0 do
  begin
    Result := Result + 'I';
    Dec(AValue);
  end;
end;

end.
