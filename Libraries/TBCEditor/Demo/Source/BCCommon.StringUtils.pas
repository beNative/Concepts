unit BCCommon.StringUtils;

interface

  function AnsiInitCap(Str: string): string;
  function CapitalizeText(AText: string): string;
  function DecryptString(Data: string): string;
  function DeleteChars(const S: string; Chr: Char): string;
  function DeleteWhiteSpace(const s: string): string;
  function EncryptString(Data: string): string;
  function FormatJSON(AJSON: string; AIndentSize: Integer = 3): string;
  function FormatXML(AXML: string): string;
  function GetNextToken(ASeparator: string; AText: string): string;
  function GetTokenAfter(ASeparator: string; AText: string): string;
  function RemoveTokenFromStart(ASeparator: string; AText: string): string;
  function RemoveNonAlpha(Source: string): string;
  function StringBetween(Str: string; SubStr1: string; SubStr2: string): string;
  function StrContainsChar(CharStr, Str: string): Boolean;
  function WordCount(s: string): Integer;

implementation

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Character, Xml.XMLDoc;

function AnsiInitCap(Str: string): string;
begin
  Result := Concat(AnsiUpperCase(Copy(Str, 1, 1)), AnsiLowerCase(Copy(Str, 2, Length(Str))));
end;

function CapitalizeText(AText: string): string;
var
  i: Integer;
  LChar: Char;
  LUpperCase: Boolean;
begin
  LUpperCase := False;
  for i := 1 to Length(AText) do
  if AText[i] <> ' ' then
  begin
    LChar := AText[i];
    if LUpperCase then
      LChar := UpCase(LChar);
    LUpperCase := False;
    Result := Result + LChar
  end
  else
    LUpperCase := True;
end;

function DeleteChars(const S: string; Chr: Char): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = Chr then
      Delete(Result, I, 1);
end;

function StringBetween(Str: string; SubStr1: string; SubStr2: string): string;
begin
  Result := Str;
  Result := Copy(Result, Pos(SubStr1, Result) + 1, Length(Result));
  Result := Copy(Result, 1, Pos(SubStr2, Result) - 1);
end;

function DecryptString(Data: string): string;
var
  i: Integer;
begin
  Result := '';
  if Length(Data) > 0 Then
    for i := 1 to Length(Data) do
    begin
      if Ord(Data[i]) <> 0 Then
        Result:= Result + Chr(Ord(Data[i]) - 1)
      else
        Result:= Result + Chr(255)
    end;
end;

function EncryptString(Data: string): string;
var
  i: Integer;
begin
  Result := '';
  if Length(Data) > 0 then
    for i := 1 to Length(Data) do
    begin
      if Ord(Data[i]) <> 255 Then
        Result := Result + Chr(Ord(Data[i]) + 1)
      else
        Result := Result + Chr(0)
    end;
end;

function GetNextToken(ASeparator: string; AText: string): string;
var
  i: Integer;
begin
  i := Pos(ASeparator, AText);
  if i <> 0 then
    Result := System.Copy(AText, 1, i - 1)
  else
    Result := AText;
end;

function GetTokenAfter(ASeparator: string; AText: string): string;
begin
  Result := System.Copy(AText, Pos(ASeparator, AText) + 1, Length(AText));
end;

function RemoveTokenFromStart(ASeparator: string; AText: string): string;
var
  i: Integer;
begin
  i := Pos(ASeparator, AText);
  if i <> 0 then
    Result := System.Copy(AText, i + Length(ASeparator), Length(AText))
  else
    Result := '';
end;

function DeleteWhiteSpace(const s: string): string;
var
  i, j: Integer;
begin
  SetLength(Result, Length(s));
  j := 0;
  for i := 1 to Length(s) do
    if not s[i].IsWhiteSpace then
    begin
      inc(j);
      Result[j] := s[i];
    end;
  SetLength(Result, j);
end;

{ Minimize when AIndentSize < 0 }
function FormatJSON(AJSON: string; AIndentSize: Integer = 3): string;
var
  LPChar: PChar;
  LInsideString: Boolean;
  LIndentLevel: Integer;

  function Indent(AIndentLevel: Integer): string;
  begin
    Result := StringOfChar(' ', AIndentLevel * AIndentSize);
  end;

  function CountCharsBefore(TextPtr: PChar; Character: Char): Integer;
  var
    TempPtr: PChar;
  begin
    Result := 0;
    TempPtr := TextPtr - 1;
    while TempPtr^ = Character do
    begin
      Inc(Result);
      Dec(TempPtr);
    end;
  end;

begin
  Result := '';
  LInsideString := False;
  LIndentLevel := 0;
  LPChar := PChar(AJSON);
  while LPChar^ <> #0 do
  begin
    case LPChar^ of
      '{', '[':
        begin
          Result := Result + LPChar^;
          if not LInsideString then
          begin
            if AIndentSize > 0 then
              Result := Result + sLineBreak;
            Result := Result + Indent(LIndentLevel + 1);
            Inc(LIndentLevel);
          end
        end;
      '}', ']':
        if not LInsideString then
        begin
          Dec(LIndentLevel);
          if AIndentSize > 0 then
            Result := Result + sLineBreak;
          Result := Result + Indent(LIndentLevel) + LPChar^;
        end
        else
          Result := Result + LPChar^;
      ',':
        begin
          Result := Result + LPChar^;
          if not LInsideString then
          begin
            if AIndentSize > 0 then
              Result := Result + sLineBreak;
            Result := Result + Indent(LIndentLevel);
          end;
        end;
      ':':
        begin
          Result := Result + LPChar^;
          if not LInsideString then
            Result := Result + ' '
        end;
      #9, #10, #13, #32:
        if LInsideString then
          Result := Result + LPChar^;
      '"':
        begin
          if not Odd(CountCharsBefore(LPChar, '\')) then
            LInsideString := not LInsideString;
          Result := Result + LPChar^;
        end
      else
        Result := Result + LPChar^;
    end;
    Inc(LPChar);
  end;
end;

{ Note! The default encoding for XML files is UTF-8, so if no encoding attribute is found UTF-8 is
  assumed. If encoding is UTF-8, FormatXMLData function will remove the encoding attribute. }
function FormatXML(AXML: string): string;
var
  XMLDocument: TXMLDocument;
begin
  XMLDocument := TXMLDocument.Create(nil);
  try
    XMLDocument.LoadFromXML(AXML);
    XMLDocument.XML.Text := FormatXMLData(XMLDocument.XML.Text);
    Result := XMLDocument.XML.Text;
  finally
    XMLDocument.Free;
  end;
end;

function WordCount(s: string): Integer;
var
  i: Integer;
  IsWhite, IsWhiteOld: Boolean;
begin
  IsWhiteOld := True;
  Result := 0;
  if Trim(s) <> '' then
  for i := 0 to Length(s) - 1 do
  begin
    IsWhite := s[i].IsWhiteSpace;
    if IsWhiteOld and not IsWhite then
      Inc(Result);
    IsWhiteOld := IsWhite;
  end;
end;

function RemoveNonAlpha(Source: string): string;
var
  i: Integer;
begin
  Result := '';
  for i :=0 to Length(Source) - 1 do
    if isCharAlpha(Source[i]) then
      Result := Result + Source[i];
end;

function StrContainsChar(CharStr, Str: string): Boolean;
var
  i: Integer;
begin
  for i := 1 to Length(CharStr) do
    if Pos(CharStr[i], Str) <> 0 then
      Exit(True);
  Result := False;
end;

end.
