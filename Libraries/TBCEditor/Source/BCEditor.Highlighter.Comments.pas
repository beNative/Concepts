unit BCEditor.Highlighter.Comments;

interface

uses
  BCEditor.Consts, BCEditor.Types;

type
  TBCEditorHighlighterComments = class(TObject)
  strict private
    FChars: TBCEditorCharSet;
    FBlockComments: TBCEditorArrayOfString;
    FLineComments: TBCEditorArrayOfString;
    procedure AddChars(const AToken: string);
  public
    destructor Destroy; override;

    procedure AddBlockComment(const AOpenToken: string; const ACloseToken: string);
    procedure AddLineComment(const AToken: string);
    procedure Clear;
    property Chars: TBCEditorCharSet read FChars write FChars;
    property BlockComments: TBCEditorArrayOfString read FBlockComments;
    property LineComments: TBCEditorArrayOfString read FLineComments;
  end;

implementation

destructor TBCEditorHighlighterComments.Destroy;
begin
  Clear;

  inherited Destroy;
end;

procedure TBCEditorHighlighterComments.AddChars(const AToken: string);
var
  LIndex: Integer;
begin
  for LIndex := 1 to Length(AToken) do
    FChars := FChars + [AToken[LIndex]];
end;

procedure TBCEditorHighlighterComments.AddBlockComment(const AOpenToken: string; const ACloseToken: string);
var
  LIndex, LLength: Integer;
begin
  LLength := Length(FBlockComments);

  for LIndex := 0 to LLength - 1 do
  begin
    if (FBlockComments[LIndex] = AOpenToken) and (FBlockComments[LIndex + 1] = ACloseToken) then
      Exit;
  end;

  SetLength(FBlockComments, LLength + 2);
  FBlockComments[LLength] := AOpenToken;
  FBlockComments[LLength + 1] := ACloseToken;

  AddChars(AOpenToken);
  AddChars(ACloseToken);
end;

procedure TBCEditorHighlighterComments.AddLineComment(const AToken: string);
var
  LIndex, LLength: Integer;
begin
  LLength := Length(FLineComments);

  for LIndex := 0 to LLength - 1 do
    if FLineComments[LIndex] = AToken then
      Exit;

  SetLength(FLineComments, LLength + 1);
  FLineComments[LLength] := AToken;

  AddChars(AToken);
end;

procedure TBCEditorHighlighterComments.Clear;
begin
  SetLength(FBlockComments, 0);
  SetLength(FLineComments, 0);
  FChars := [];
end;

end.
