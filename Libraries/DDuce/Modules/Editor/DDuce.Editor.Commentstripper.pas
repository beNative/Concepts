{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit DDuce.Editor.CommentStripper;

{ Comment stripper classes. }

{ TODO: Implement ICommentStripper }

interface

uses
  System.Classes, System.SysUtils;

type
  TSourceTokenKind = (
    skUndefined,
    skCode,
    skBlockComment,
    skLineComment,
    skQuoteString,
    skDittoString,
    skDirective,
    skToReserve
  );

  TStripOption = (
    coAll,
    coExAscii
  );

type
  TCustomCommentStripper = class(TComponent)
  private
    FCurTokenKind    : TSourceTokenKind;
    FCurChar         : AnsiChar;
    FStripDirectives : Boolean;
    FStripOption     : TStripOption;
    FInStream        : TStream;
    FOutStream       : TStream;
    FStripReserved   : Boolean;
    FReservedItems   : TStringList;

    procedure SetInStream(const Value: TStream);
    procedure SetOutStream(const Value: TStream);
    procedure SetReservedItems(const Value: TStringList);

  protected
    procedure DoParse; virtual; abstract;
    procedure ProcessToBlockEnd; virtual; abstract;

    function IsReserved: Boolean;
    function IsBlank(AChar: AnsiChar): Boolean;

    function GetCurChar: AnsiChar;
    function NextChar(Value: Integer = 1): AnsiChar;
    procedure WriteChar(Value: AnsiChar);

    procedure ProcessToLineEnd;
    procedure DoDefaultProcess;
    procedure DoBlockEndProcess;

  public
    procedure Parse;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property InStream: TStream
      read FInStream write SetInStream;

    property OutStream: TStream
      read FOutStream write SetOutStream;

    property StripOption: TStripOption
      read FStripOption write FStripOption;

    property StripDirectives: Boolean
      read FStripDirectives write FStripDirectives;

    property StripReserved: Boolean
      read FStripReserved write FStripReserved;

    property ReservedItems: TStringList
      read FReservedItems write SetReservedItems;
  end;

type
  TPasCommentStripper = class(TCustomCommentStripper)
  protected
    procedure DoParse; override;
    procedure ProcessToBlockEnd; override;
    procedure ProcessToBracketBlockEnd;

  end;

type
  TCPPCommentStripper = class(TCustomCommentStripper)
  protected
    procedure DoParse; override;
    procedure ProcessToBlockEnd; override;

  end;

implementation

{ TCustomCommentStripper }

constructor TCustomCommentStripper.Create(AOwner: TComponent);
begin
  inherited;
  FReservedItems := TStringList.Create;
end;

destructor TCustomCommentStripper.Destroy;
begin
  FInStream := nil;
  FOutStream := nil;
  FReservedItems.Free;
  inherited;
end;

procedure TCustomCommentStripper.DoBlockEndProcess;
begin
  case FCurTokenKind of
  skBlockComment:
    if (FStripOption = coExAscii) and (FCurChar < #128) then
      WriteChar(FCurChar);
  skDirective:
    if not StripDirectives or
      ((FStripOption = coExAscii) and (FCurChar < #128)) then
      WriteChar(FCurChar);
  skToReserve:
    if FStripReserved then
      WriteChar(FCurChar);
  else
    DoDefaultProcess;
  end;
end;

procedure TCustomCommentStripper.DoDefaultProcess;
begin
  if (FStripOption = coAll) or (FCurChar < #128) then
    WriteChar(FCurChar);
end;

function TCustomCommentStripper.GetCurChar: AnsiChar;
begin
  Result := #0;
  if Assigned(FInStream) then
  begin
    try
      FInStream.Read(Result, SizeOf(AnsiChar));
    except
      Exit;
    end;
  end;
end;

function TCustomCommentStripper.IsBlank(AChar: AnsiChar): Boolean;
begin
  Result := AChar in [' ', #13, #10, #7, #9];
end;

function TCustomCommentStripper.IsReserved: Boolean;
var
  i: Integer;
  OldChar: AnsiChar;
  OldPos: Integer;
  MaxLen: Integer;
  PBuf: PChar;
  SToCompare: String;
begin
  Result := False;
  if FInStream = nil then Exit;

  PBuf := nil;
  OldChar := FCurChar;
  OldPos := FInStream.Position;

  MaxLen := 0;
  for i := Self.FReservedItems.Count - 1 downto 0 do
  begin
    if MaxLen < Length(Self.FReservedItems.Strings[i]) then
      MaxLen := Length(Self.FReservedItems.Strings[i]);
    if Self.FReservedItems.Strings[i] = '' then
      Self.FReservedItems.Delete(i);
  end;

  if (FCurChar = '/') or (FCurChar = '(') then
  begin
    FCurChar := GetCurChar;
    if FCurChar <> '*' then
      Exit;
  end;

  try
    PBuf := StrAlloc(MaxLen + 1);
    FillChar(PBuf^, Length(PBuf), 0);
    FInStream.Read(PBuf^, MaxLen);

    for i := 0 to Self.FReservedItems.Count - 1 do
    begin
      SToCompare := Copy(StrPas(PBuf), 1, Length(Self.FReservedItems.Strings[i]));
      if SToCompare = Self.FReservedItems.Strings[i] then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    FCurChar := OldChar;
    FInStream.Position := OldPos;
    if PBuf <> nil then
      StrDispose(PBuf);
  end;
end;

function TCustomCommentStripper.NextChar(Value: Integer): AnsiChar;
begin
  Result := #0;
  if Assigned(FInStream) then
  begin
    try
      FInStream.Seek(Value - 1, soFromCurrent);
      FInStream.Read(Result, SizeOf(AnsiChar));
      FInStream.Seek(-Value, soFromCurrent);
    except
      Exit;
    end;
  end;
end;

procedure TCustomCommentStripper.Parse;
begin
  if (FInStream <> nil) and (FOutStream <> nil) then
  begin
    if FInStream.Size > 0 then
    begin
      FInStream.Position := 0;
      FCurTokenKind := skUndefined;
      DoParse;
    end;
  end;
end;

procedure TCustomCommentStripper.ProcessToLineEnd;
begin
  while not (FCurChar in [#0, #13]) do
  begin
    if (FStripOption = coExAscii) and (FCurChar < #128) then
        WriteChar(FCurChar);
    FCurChar := GetCurChar;
  end;

  if FCurChar = #13 then
    repeat
      WriteChar(FCurChar);
      FCurChar := GetCurChar;
    until FCurChar in [#0, #10];

  if FCurChar = #10 then
    WriteChar(FCurChar);

  FCurTokenKind := skUndefined;
end;

procedure TCustomCommentStripper.SetInStream(const Value: TStream);
begin
  FInStream := Value;
end;

procedure TCustomCommentStripper.SetOutStream(const Value: TStream);
begin
  FOutStream := Value;
end;

procedure TCustomCommentStripper.SetReservedItems(const Value: TStringList);
begin
  if Value <> nil then
    FReservedItems.Assign(Value);
end;

procedure TCustomCommentStripper.WriteChar(Value: AnsiChar);
begin
  if Assigned(FOutStream) then
  begin
    try
      OutStream.Write(Value, SizeOf(Value));
    except
      Exit;
    end;
  end;
end;

{ TCPPCommentStripper }

procedure TCPPCommentStripper.DoParse;
begin
  FCurChar := GetCurChar;
  while FCurChar <> #0 do
  begin
    case FCurChar of
    '/':
      begin
        if (FCurTokenKind in [skCode, skUndefined]) and (NextChar = '/') then
        begin
          FCurTokenKind := skLineComment;
          ProcessToLineEnd;
        end
        else
        if (FCurTokenKind in [skCode, skUndefined]) and (NextChar = '*') then
        begin
          if FStripReserved and IsReserved then
            FCurTokenKind := skToReserve
          else
            FCurTokenKind := skBlockComment;
          ProcessToBlockEnd;
        end
        else
          DoDefaultProcess;
      end;
    '''':
      begin
        if FCurTokenKind in [skCode, skUndefined] then
          FCurTokenKind := skQuoteString
        else if FCurTokenKind = skQuoteString then
           FCurTokenKind := skCode;

        DoDefaultProcess;
      end;
    '"':
      begin
        if FCurTokenKind in [skCode, skUndefined] then
          FCurTokenKind := skDittoString
        else if FCurTokenKind = skDittoString then
           FCurTokenKind := skCode;

        DoDefaultProcess;
      end;
    else
      DoDefaultProcess;
    end;

    FCurChar := GetCurChar;
  end;
end;

procedure TCPPCommentStripper.ProcessToBlockEnd;
begin
  while ((FCurChar <> '*') or (NextChar <> '/')) and (FCurChar <> #0) do
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
  end;

  if FCurChar = '*' then
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
    DoBlockEndProcess;
  end;

  FCurTokenKind := skUndefined;
end;

{ TPasCommentStripper }

procedure TPasCommentStripper.DoParse;
begin
  FCurChar := GetCurChar;
  while FCurChar <> #0 do
  begin
    case FCurChar of
    '/':
      begin
        if (FCurTokenKind in [skCode, skUndefined]) and (NextChar = '/') then
        begin
          FCurTokenKind := skLineComment;
          ProcessToLineEnd;
        end
        else
          DoDefaultProcess;
      end;
    '{':
      begin
        if FCurTokenKind in [skCode, skUndefined] then
        begin
          if NextChar <> '$' then
          begin
            if FStripReserved and IsReserved then
              FCurTokenKind := skToReserve
            else
              FCurTokenKind := skBlockComment
          end
          else
            FCurTokenKind := skDirective;
          ProcessToBlockEnd;
        end
        else
          DoDefaultProcess;
      end;
    '(':
      begin
        if (FCurTokenKind in [skCode, skUndefined]) and (NextChar = '*') then
        begin
          if NextChar(2) = '$' then
            FCurTokenKind := skDirective
          else
            FCurTokenKind := skBlockComment;
          ProcessToBracketBlockEnd;
        end
        else
          DoDefaultProcess;
      end;
    '''':
      begin
        if FCurTokenKind in [skCode, skUndefined] then
          FCurTokenKind := skQuoteString
        else if FCurTokenKind = skQuoteString then
           FCurTokenKind := skCode;

        DoDefaultProcess;
      end;
    else
      DoDefaultProcess;
    end;

    FCurChar := GetCurChar;
  end;
end;

procedure TPasCommentStripper.ProcessToBlockEnd;
begin
  while not (FCurChar in [#0, '}']) do
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
  end;

  DoBlockEndProcess;
  FCurTokenKind := skUndefined;
end;

procedure TPasCommentStripper.ProcessToBracketBlockEnd;
begin
  while ((FCurChar <> '*') or (NextChar <> ')')) and (FCurChar <> #0) do
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
  end;

  if FCurChar = '*' then
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
    DoBlockEndProcess;
  end;

  FCurTokenKind := skUndefined;
end;

end.
