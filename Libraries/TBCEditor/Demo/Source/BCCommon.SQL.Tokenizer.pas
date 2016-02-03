unit BCCommon.SQL.Tokenizer;

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TSQLTokenType = (ttNone, ttOpenParens, ttCloseParens, ttWhiteSpace, ttOtherNode, ttSingleLineComment,
    ttMultiLineComment, ttString, ttNationalString, ttBracketQuotedName, ttQuotedString, ttComma,
    ttPeriod, ttSemicolon, ttColon, ttAsterisk, ttEqualsSign, ttMonetaryValue, ttNumber, ttBinaryValue,
    ttOtherOperator, ttPseudoName);

  TSQLTokenizationType = (tNull,
    { variable-length types }
    tWhiteSpace, tOtherNode, tSingleLineComment, tBlockComment, tString, tNString, tQuotedString,
    tBracketQuotedName, tOtherOperator, tNumber, tBinaryValue, tMonetaryValue, tDecimalValue,
    tFloatValue, tPseudoName,
    { temporary types }
    tSingleAsterisk, tSingleDollar, tSingleHyphen, tSingleSlash, {tSingleN,} tSingleLT,
    tSingleExclamation, tSinglePeriod, tSingleZero, tSingleOtherCompoundableOperator);

  TSQLToken = class
  private
    FTokenType: TSQLTokenType;
    FValue: string;
  public
    constructor Create(TokenType: TSQLTokenType; Value: string); overload;
    property TokenType: TSQLTokenType read FTokenType write FTokenType;
    property Value: string read FValue write FValue;
  end;

  TSQLTokenList = class(TList<TSQLToken>)
  private
    //FHasErrors: Boolean;
    FHasUnfinishedToken: Boolean;
  public
    //function GetRange(Index: Integer; Count: Integer): TSQLTokenList;
    //function GetRangeByIndex(FromIndex: Integer; ToIndex: Integer): TSQLTokenList;
    //property HasErrors: Boolean read FHasErrors write FHasErrors;
    property HasUnfinishedToken: Boolean read FHasUnfinishedToken write FHasUnfinishedToken;
  end;

  {TSQLTokenPositionsList = class(TList<System.Integer>)
  public
    //function GetRange(Index: Integer; Count: Integer): TSQLTokenPositionsList;
  end; }

  TSQLTokenizer = class
  private
    FSQLTokenList: TSQLTokenList;
    FCurrentTokenID: Integer;
    procedure ProcessOrOpenToken(var CurrentTokenizationType: TSQLTokenizationType;
      CurrentNodeValue: TStringBuilder; CurrentCharacter: Char; TokenContainer: TSQLTokenList);
    function IsWhitespace(TargetCharacter: Char): Boolean;
    procedure CompleteToken(var CurrentTokenizationType: TSQLTokenizationType;
      TokenContainer: TSQLTokenList; CurrentValue: TStringBuilder);
    function IsNonWordCharacter(CurrentCharacter: Char): Boolean;
    function IsCompoundableOperatorCharacter(CurrentCharacter: Char): Boolean;
    function IsOperatorCharacter(CurrentCharacter: Char): Boolean;
//    function IsCurrencyPrefix(CurrentCharacter: Char): Boolean;
    function TokenizeSQL(SQL: string): TSQLTokenList;
    function GetTokenStr: string;
    function GetShortTokenStr: ShortString;
    function GetTokenType: TSQLTokenType;
    function GetEof: Boolean;
  public
    constructor Create(SQL: string); overload;
    destructor Destroy; override;
    property SQLTokenList: TSQLTokenList read FSQLTokenList;
    property TokenStr: string read GetTokenStr;
    property ShortTokenStr: ShortString read GetShortTokenStr;
    property TokenType: TSQLTokenType read GetTokenType;
    property Eof: Boolean read GetEof;
    procedure Next;
    function TokenStrIs(Value: string; CaseSensitive: Boolean = False): Boolean;
    procedure SetText(SQL: string);
  end;

implementation

uses
  StrUtils;

{ TSQLToken }

constructor TSQLToken.Create(TokenType: TSQLTokenType; Value: string);
begin
  inherited Create;
  FTokenType := TokenType;
  FValue := Value;
end;

{ TSQLTokenList }

{function TSQLTokenList.GetRange(Index: Integer; Count: Integer): TSQLTokenList;
var
  i: Integer;
begin
  Result := TSQLTokenList.Create;
  for i := Index to Index + Count - 1 do
    Result.Add(Self.Items[i]);
end;

function TSQLTokenList.GetRangeByIndex(FromIndex: Integer; ToIndex: Integer): TSQLTokenList;
begin
  Result := GetRange(FromIndex, ToIndex - FromIndex + 1);
end; }

{ TSQLTokenPositionsList }

{function TSQLTokenPositionsList.GetRange(Index: Integer; Count: Integer): TSQLTokenPositionsList;
var
  i: Integer;
begin
  Result := TSQLTokenPositionsList.Create;
  for i := Index to Index + Count - 1 do
    Result.Add(Self.Items[i]);
end; }

{ TSQLTokenizer }

constructor TSQLTokenizer.Create(SQL: string);
begin
  inherited Create;
  SetText(SQL);
end;

destructor TSQLTokenizer.Destroy;
begin
  FSQLTokenList.Free;
  inherited Destroy;
end;

procedure TSQLTokenizer.SetText(SQL: string);
begin
  if Assigned(FSQLTokenList) then
    FSQLTokenList.Clear;
  FSQLTokenList := TokenizeSQL(SQL);
  FCurrentTokenID := 0;
end;

function TSQLTokenizer.GetTokenStr: string;
begin
  Result := '';
  if FCurrentTokenID < FSQLTokenList.Count then
    Result := FSQLTokenList.Items[FCurrentTokenID].FValue;
end;

function TSQLTokenizer.GetShortTokenStr: ShortString;
begin
  Result := '';
  if FCurrentTokenID < FSQLTokenList.Count then
    Result := ShortString(FSQLTokenList.Items[FCurrentTokenID].FValue);
end;

function TSQLTokenizer.GetTokenType: TSQLTokenType;
begin
  Result := ttNone;
  if FCurrentTokenID < FSQLTokenList.Count then
    Result := FSQLTokenList.Items[FCurrentTokenID].FTokenType;
end;

function TSQLTokenizer.TokenStrIs(Value: string; CaseSensitive: Boolean): Boolean;
begin
  Result := False;
  if FCurrentTokenID < FSQLTokenList.Count then
  begin
    if not CaseSensitive then
      Result := UpperCase(FSQLTokenList.Items[FCurrentTokenID].Value) = UpperCase(Value)
    else
      Result := FSQLTokenList.Items[FCurrentTokenID].FValue = Value
  end;
end;

function TSQLTokenizer.GetEof: Boolean;
begin
  Result := FCurrentTokenID >= FSQLTokenList.Count;
end;

procedure TSQLTokenizer.Next;
begin
  Inc(FCurrentTokenID);
end;

function TSQLTokenizer.TokenizeSQL(SQL: string): TSQLTokenList;
var
  CommentNesting, CurrentCharInt, NextCharInt: Integer;
  CurrentCharacter, NextCharacter: Char;
  InputReader: TStringReader;
  CurrentTokenValue: TStringBuilder;
  CurrentTokenizationType: TSQLTokenizationType;
begin
  Result := TSQLTokenList.Create;
  InputReader := TStringReader.Create(SQL);
  try
    CurrentTokenValue := TStringBuilder.Create;

    CurrentTokenizationType := tNull;
    CurrentTokenValue.Length := 0;
    CommentNesting := 0;

    CurrentCharInt := InputReader.Read;
    while CurrentCharInt >= 0 do
    begin
      CurrentCharacter := Char(CurrentCharInt);
      if CurrentTokenizationType = tNull then
        ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result)
      else
      case CurrentTokenizationType of
        tWhiteSpace:
        begin
          if IsWhitespace(CurrentCharacter) then
            CurrentTokenValue.Append(CurrentCharacter)
          else
          begin
            CompleteToken(CurrentTokenizationType, Result, currentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, currentTokenValue, currentCharacter, Result);
          end;
        end;
        tSinglePeriod:
        begin
          if (CurrentCharacter >= '0') and (CurrentCharacter <= '9') then
          begin
            currentTokenizationType := tDecimalValue;
            currentTokenValue.Append('.');
            currentTokenValue.Append(CurrentCharacter);
          end
          else
          begin
            currentTokenValue.Append('.');
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end;

        tSingleZero:
        begin
          if (CurrentCharacter = 'x') or (CurrentCharacter = 'X') then
          begin
            currentTokenizationType := tBinaryValue;
            currentTokenValue.Append('0');
            currentTokenValue.Append(CurrentCharacter);
          end
          else
          if (CurrentCharacter >= '0') and (CurrentCharacter <= '9') then
          begin
            currentTokenizationType := tNumber;
            currentTokenValue.Append('0');
            currentTokenValue.Append(CurrentCharacter);
          end
          else
          if CurrentCharacter = '.' then
          begin
            currentTokenizationType := tDecimalValue;
            currentTokenValue.Append('0');
            currentTokenValue.Append(CurrentCharacter);
          end
          else
          begin
            currentTokenValue.Append('0');
            CompleteToken(CurrentTokenizationType, Result, currentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, currentTokenValue, CurrentCharacter, Result);
          end;
        end;

       tNumber:
       begin
          if (CurrentCharacter = 'e') or (CurrentCharacter = 'E') then
          begin
            CurrentTokenizationType := tFloatValue;
            CurrentTokenValue.Append(CurrentCharacter);
          end
          else
          if CurrentCharacter = '.' then
          begin
            CurrentTokenizationType := tDecimalValue;
            CurrentTokenValue.Append(CurrentCharacter);
          end
          else
          if (CurrentCharacter >= '0') and (CurrentCharacter <= '9') then
            CurrentTokenValue.Append(CurrentCharacter)
          else
          begin
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end;

        tDecimalValue:
        begin
          if (CurrentCharacter = 'e') or (CurrentCharacter = 'E') then
          begin
            CurrentTokenizationType := tFloatValue;
            CurrentTokenValue.Append(CurrentCharacter);
          end
          else
          if (CurrentCharacter >= '0') and (CurrentCharacter <= '9') then
            CurrentTokenValue.Append(CurrentCharacter)
          else
          begin
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
         end;

        tFloatValue:
        begin
          if (CurrentCharacter >= '0') and (CurrentCharacter <= '9') then
            CurrentTokenValue.Append(CurrentCharacter)
          else
          if (CurrentCharacter = '-') and EndsText('e', CurrentTokenValue.ToString) then
            CurrentTokenValue.Append(CurrentCharacter)
          else
          begin
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end;

        tBinaryValue:
        begin
          if ( (CurrentCharacter >= '0') and (CurrentCharacter <= '9') ) or
             ( (CurrentCharacter >= 'A') and (CurrentCharacter <= 'F') ) or
             ( (CurrentCharacter >= 'a') and (CurrentCharacter <= 'f') ) then
            CurrentTokenValue.Append(CurrentCharacter)
          else
          begin
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end;

        tSingleDollar:
        begin
          CurrentTokenValue.Append('$');
          CurrentTokenValue.Append(CurrentCharacter);

          if ( (CurrentCharacter >= 'A') and (CurrentCharacter <= 'Z') ) or
             ( (CurrentCharacter >= 'a') and (CurrentCharacter <= 'z') ) then
            CurrentTokenizationType := tPseudoName
          else
            CurrentTokenizationType := tMonetaryValue;
        end;

        tMonetaryValue:
        begin
          if (CurrentCharacter >= '0') and (CurrentCharacter <= '9') then
            CurrentTokenValue.Append(CurrentCharacter)
          else
          if (CurrentCharacter = '-') and (CurrentTokenValue.Length = 1) then
            CurrentTokenValue.Append(CurrentCharacter)
          else
          if (CurrentCharacter = '.') and (Pos('.', CurrentTokenValue.ToString) = 0) then
            CurrentTokenValue.Append(CurrentCharacter)
          else
          begin
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end;

        tSingleHyphen:
        begin
          if CurrentCharacter = '-' then
            CurrentTokenizationType := tSingleLineComment
          else
          if CurrentCharacter = '=' then
          begin
            CurrentTokenizationType := tOtherOperator;
            CurrentTokenValue.Append('-');
            CurrentTokenValue.Append(CurrentCharacter);
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
          begin
            CurrentTokenizationType := tOtherOperator;
            CurrentTokenValue.Append('-');
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end;

        tSingleSlash:
        begin
          if CurrentCharacter = '*' then
          begin
            CurrentTokenizationType := tBlockComment;
            Inc(CommentNesting);
          end
          else
          if CurrentCharacter = '=' then
          begin
            CurrentTokenizationType := tOtherOperator;
            CurrentTokenValue.Append('/');
            CurrentTokenValue.Append(CurrentCharacter);
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
          begin
            CurrentTokenizationType := tOtherOperator;
            CurrentTokenValue.Append('/');
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end;

        tSingleLineComment:
        begin
          if (CurrentCharacter = Chr(13)) or (CurrentCharacter = Chr(10)) then
          begin
            CurrentTokenValue.Append(CurrentCharacter);

            NextCharInt := InputReader.Peek();
            if (CurrentCharacter = Chr(13)) and (NextCharInt = 10) then
              CurrentTokenValue.Append(Char(InputReader.Read));

            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
            CurrentTokenValue.Append(CurrentCharacter);
        end;

        tBlockComment:
        begin
          if CurrentCharacter = '*' then
          begin
            if InputReader.Peek = Integer('/') then
            begin
                Dec(CommentNesting);
                NextCharacter := Char(inputReader.Read);
                if CommentNesting > 0 then
                begin
                  CurrentTokenValue.Append(CurrentCharacter);
                  CurrentTokenValue.Append(nextCharacter);
                end
                else
                  CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            end
            else
              CurrentTokenValue.Append(CurrentCharacter);
          end
          else
          begin
            CurrentTokenValue.Append(CurrentCharacter);

            if (CurrentCharacter = '/') and (InputReader.Peek = Integer('*')) then
            begin
              CurrentTokenValue.Append(Char(inputReader.Read));
              Inc(CommentNesting);
            end;
          end;
        end;

        tOtherNode,
        tPseudoName:
        begin
          if IsNonWordCharacter(CurrentCharacter) then
          begin
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end
          else
            CurrentTokenValue.Append(CurrentCharacter);
        end;

       { tSingleN:
        begin
          if CurrentCharacter = '''' then
            CurrentTokenizationType := tNString
          else
          begin
            CurrentTokenizationType := tOtherNode;
            CurrentTokenValue.Append('N');
            CurrentTokenValue.Append(CurrentCharacter);
          end;
        end; }

        tNString,
        tString:
        begin
          if CurrentCharacter = '''' then
          begin
            if InputReader.Peek = Integer('''') then
            begin
              InputReader.Read;
              CurrentTokenValue.Append(CurrentCharacter);
            end
            else
              CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
            CurrentTokenValue.Append(CurrentCharacter);
        end;

        tQuotedString:
        begin
          if CurrentCharacter = '"' then
          begin
            if InputReader.Peek = Integer('"') then
            begin
              InputReader.Read;
              CurrentTokenValue.Append(CurrentCharacter);
            end
            else
                CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
            CurrentTokenValue.Append(CurrentCharacter);
        end;

        tBracketQuotedName:
        begin
          if CurrentCharacter = ']' then
          begin
            if InputReader.Peek = Integer(']') then
            begin
              InputReader.Read;
              CurrentTokenValue.Append(CurrentCharacter);
            end
            else
              CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
            CurrentTokenValue.Append(CurrentCharacter);
        end;

        tSingleLT:
        begin
          CurrentTokenValue.Append('<');
          CurrentTokenizationType := tOtherOperator;
          if CharInSet(CurrentCharacter, ['=', '>']) then
          begin
            CurrentTokenValue.Append(CurrentCharacter);
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
          begin
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end;

        tSingleAsterisk:
        begin
          CurrentTokenValue.Append('*');
          if CurrentCharacter = '=' then
          begin
            CurrentTokenValue.Append(CurrentCharacter);
            CurrentTokenizationType := tOtherOperator;
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
          begin
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end;

        tSingleOtherCompoundableOperator:
        begin
          CurrentTokenizationType := tOtherOperator;
          if CurrentCharacter = '=' then
          begin
            CurrentTokenValue.Append(CurrentCharacter);
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
          begin
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end
        end;

        tSingleExclamation:
        begin
          CurrentTokenValue.Append('!');
          if CharInSet(CurrentCharacter, ['=', '<', '>']) then
          begin
            CurrentTokenizationType := tOtherOperator;
            CurrentTokenValue.Append(CurrentCharacter);
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
          end
          else
          begin
            CurrentTokenizationType := tOtherNode;
            CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
            ProcessOrOpenToken(CurrentTokenizationType, CurrentTokenValue, CurrentCharacter, Result);
          end;
        end
        else
          raise Exception.Create('In-progress node unrecognized!');
      end;
      CurrentCharInt := InputReader.Read;
    end;

    if CurrentTokenizationType <> tnull then
    begin
      if (CurrentTokenizationType = tBlockComment) or
        (CurrentTokenizationType = tString) or
        (CurrentTokenizationType = tNString) or
        (CurrentTokenizationType = tQuotedString) or
        (CurrentTokenizationType = tBracketQuotedName) then
        Result.HasUnfinishedToken := True;

      CompleteToken(CurrentTokenizationType, Result, CurrentTokenValue);
    end;
  finally
    InputReader.Free;
  end;
end;

function TSQLTokenizer.IsWhitespace(TargetCharacter: Char): Boolean;
begin
  Result := CharInSet(TargetCharacter, [' ', Chr(9), Chr(10), Chr(13)]);
end;

function TSQLTokenizer.IsNonWordCharacter(CurrentCharacter: Char): Boolean;
begin
  //characters that pop you out of a regular "word" context (maybe into a new word)
  Result := IsWhitespace(CurrentCharacter) or IsOperatorCharacter(CurrentCharacter) or
    //(IsCurrencyPrefix(CurrentCharacter) and
    // (CurrentCharacter <> '$') or
    CharInSet(CurrentCharacter, ['''', '"', ',', '.', '[', '(', ')', '!', ';', ':']);
end;

function TSQLTokenizer.IsCompoundableOperatorCharacter(CurrentCharacter: Char): Boolean;
begin
  //operator characters that can be compounded by a subsequent space
  Result := CharInSet(CurrentCharacter, ['/', '-', '+', '*', '%', '&', '^', '|', '<', '>']);
end;

function TSQLTokenizer.IsOperatorCharacter(CurrentCharacter: Char): Boolean;
begin
  //operator characters
  Result := CharInSet(CurrentCharacter,['/', '-', '+', '%', '*', '&', '|', '^', '=', '<', '>', '~']);
end;

(*function TSQLTokenizer.IsCurrencyPrefix(CurrentCharacter: Char): Boolean;

  function HexToChar(Hex: string): Char;
  begin
    Result := Char(StrToInt(Hex));
  end;

begin
  //symbols that SQL Server recognizes as currency prefixes - these also happen to
  // be word-breakers, except the dollar. Ref:
  // http://msdn.microsoft.com/en-us/library/ms188688.aspx
  Result := CharInSet(CurrentCharacter, [
    HexToChar('0x0024'), HexToChar('0x00A2'), HexToChar('0x00A3'), HexToChar('0x00A4'),
    HexToChar('0x00A5'), HexToChar('0x09F2'), HexToChar('0x09F3'), HexToChar('0x0E3F'),
    HexToChar('0x17DB'), HexToChar('0x20A0'), HexToChar('0x20A1'), HexToChar('0x20A2'),
    HexToChar('0x20A3'), HexToChar('0x20A4'), HexToChar('0x20A5'), HexToChar('0x20A6'),
    HexToChar('0x20A7'), HexToChar('0x20A8'), HexToChar('0x20A9'), HexToChar('0x20AA'),
    HexToChar('0x20AB'), HexToChar('0x20AC'), HexToChar('0x20AD'), HexToChar('0x20AE'),
    HexToChar('0x20AF'), HexToChar('0x20B0'), HexToChar('0x20B1'), HexToChar('0xFDFC'),
    HexToChar('0xFE69'), HexToChar('0xFF04'), HexToChar('0xFFE0'), HexToChar('0xFFE1'),
    HexToChar('0xFFE5'), HexToChar('0xFFE6')]);
end; *)

procedure TSQLTokenizer.ProcessOrOpenToken(var CurrentTokenizationType: TSQLTokenizationType;
      CurrentNodeValue: TStringBuilder; CurrentCharacter: Char; TokenContainer: TSQLTokenList);
begin
  if CurrentTokenizationType <> tNull then
    raise Exception.Create('Cannot start a new Token: existing Tokenization Type is not null');

  //start a new value.
  CurrentNodeValue.Length := 0;

  if IsWhitespace(CurrentCharacter) then
  begin
    CurrentTokenizationType := tWhiteSpace;
    currentNodeValue.Append(CurrentCharacter);
  end
  else
  if CurrentCharacter = '-' then
    CurrentTokenizationType := tSingleHyphen
  else
  if CurrentCharacter = '$' then
    CurrentTokenizationType := tSingleDollar
  else
  if CurrentCharacter = '/' then
    CurrentTokenizationType := tSingleSlash
  else
  //if CurrentCharacter = 'N' then
  //    CurrentTokenizationType := tSingleN
  //else
  if CurrentCharacter = '''' then
    CurrentTokenizationType := tString
  else
  if CurrentCharacter = '"' then
      CurrentTokenizationType := tQuotedString
  else
  if CurrentCharacter = '[' then
    CurrentTokenizationType := tBracketQuotedName
  else
  if CurrentCharacter = '(' then
    TokenContainer.Add(TSQLToken.Create(ttOpenParens, CurrentCharacter))
  else
  if CurrentCharacter = ')' then
    TokenContainer.Add(TSQLToken.Create(ttCloseParens, CurrentCharacter))
  else
  if CurrentCharacter = ',' then
    TokenContainer.Add(TSQLToken.Create(ttComma, CurrentCharacter))
  else
  if CurrentCharacter = '.' then
    CurrentTokenizationType := tSinglePeriod
  else
  if CurrentCharacter = '0' then
    CurrentTokenizationType := tSingleZero
  else
  if (CurrentCharacter >= '1') and (CurrentCharacter <= '9') then
  begin
    CurrentTokenizationType := tNumber;
    CurrentNodeValue.Append(CurrentCharacter);
  end
  else
  {if IsCurrencyPrefix(CurrentCharacter) then
  begin
    CurrentTokenizationType := tMonetaryValue;
    CurrentNodeValue.Append(CurrentCharacter);
  end
  else}
  if CurrentCharacter = ';' then
    TokenContainer.Add(TSQLToken.Create(ttSemicolon, CurrentCharacter))
  else
  if CurrentCharacter = ':' then
    TokenContainer.Add(TSQLToken.Create(ttColon, CurrentCharacter))
  else
  if CurrentCharacter = '*' then
    TokenContainer.Add(TSQLToken.Create(ttAsterisk, CurrentCharacter))
  else
  if CurrentCharacter = '<' then
    CurrentTokenizationType := tSingleLT
  else
  if CurrentCharacter = '!' then
    CurrentTokenizationType := tSingleExclamation
  else
  if IsCompoundableOperatorCharacter(CurrentCharacter) then
  begin
    CurrentTokenizationType := tSingleOtherCompoundableOperator;
    CurrentNodeValue.Append(CurrentCharacter);
  end
  else
  if IsOperatorCharacter(CurrentCharacter) then
    TokenContainer.Add(TSQLToken.Create(ttOtherOperator, CurrentCharacter))
  else
  begin
    CurrentTokenizationType := tOtherNode;
    currentNodeValue.Append(CurrentCharacter);
  end;
end;

procedure TSQLTokenizer.CompleteToken(var CurrentTokenizationType: TSQLTokenizationType; TokenContainer: TSQLTokenList;
  CurrentValue: TStringBuilder);
begin
  if CurrentTokenizationType = tNull then
    raise Exception.Create('Cannot complete Token, as there is no current Tokenization Type');

  case CurrentTokenizationType of
    tBlockComment: TokenContainer.Add(TSQLToken.Create(ttMultiLineComment, CurrentValue.ToString));
    tOtherNode: TokenContainer.Add(TSQLToken.Create(ttOtherNode, CurrentValue.ToString));
    tPseudoName: TokenContainer.Add(TSQLToken.Create(ttPseudoName, CurrentValue.ToString));
    tSingleLineComment: TokenContainer.Add(TSQLToken.Create(ttSingleLineComment, CurrentValue.ToString));
    tSingleHyphen: TokenContainer.Add(TSQLToken.Create(ttOtherOperator, '-'));
    tSingleDollar: TokenContainer.Add(TSQLToken.Create(ttMonetaryValue, '$'));
    tSingleSlash: TokenContainer.Add(TSQLToken.Create(ttOtherOperator, '/'));
    tWhiteSpace: TokenContainer.Add(TSQLToken.Create(ttWhiteSpace, CurrentValue.ToString));
    //tSingleN: TokenContainer.Add(TSQLToken.Create(ttOtherNode, 'N'));
    tSingleExclamation: TokenContainer.Add(TSQLToken.Create(ttOtherNode, '!'));
    tNString: TokenContainer.Add(TSQLToken.Create(ttNationalString, CurrentValue.ToString));
    tString: TokenContainer.Add(TSQLToken.Create(ttString, '''' + CurrentValue.ToString + ''''));
    tQuotedString: TokenContainer.Add(TSQLToken.Create(ttQuotedString, '"' + CurrentValue.ToString + '"'));
    tBracketQuotedName: TokenContainer.Add(TSQLToken.Create(ttBracketQuotedName, CurrentValue.ToString));
    tOtherOperator, tSingleOtherCompoundableOperator: TokenContainer.Add(TSQLToken.Create(ttOtherOperator, CurrentValue.ToString));
    tSingleZero: TokenContainer.Add(TSQLToken.Create(ttNumber, '0'));
    tSinglePeriod: TokenContainer.Add(TSQLToken.Create(ttPeriod, '.'));
    tSingleAsterisk: TokenContainer.Add(TSQLToken.Create(ttAsterisk, CurrentValue.ToString));
    tNumber, tDecimalValue, tFloatValue: TokenContainer.Add(TSQLToken.Create(ttNumber, CurrentValue.ToString));
    tBinaryValue: TokenContainer.Add(TSQLToken.Create(ttBinaryValue, CurrentValue.ToString));
    //tMonetaryValue: TokenContainer.Add(TSQLToken.Create(ttMonetaryValue, CurrentValue.ToString));
  else
    raise Exception.Create('Unrecognized SQL Node Type');
  end;

  CurrentTokenizationType := tNull;
end;

end.

