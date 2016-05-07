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

{ TBCEditorHighlighterComments }

destructor TBCEditorHighlighterComments.Destroy;
begin
  Clear;

  inherited Destroy;
end;

procedure TBCEditorHighlighterComments.AddChars(const AToken: string);
var
  i: Integer;
begin
  for i := 1 to Length(AToken) do
    FChars := FChars + [AToken[i]];
end;

procedure TBCEditorHighlighterComments.AddBlockComment(const AOpenToken: string; const ACloseToken: string);
var
  i, LLength: Integer;
begin
  LLength := Length(FBlockComments);

  for i := 0 to LLength - 1 do
    if (FBlockComments[i] = AOpenToken) and (FBlockComments[i + 1] = ACloseToken) then
      Exit;

  SetLength(FBlockComments, LLength + 2);
  FBlockComments[LLength] := AOpenToken;
  FBlockComments[LLength + 1] := ACloseToken;

  AddChars(AOpenToken);
  AddChars(ACloseToken);
end;

procedure TBCEditorHighlighterComments.AddLineComment(const AToken: string);
var
  i, LLength: Integer;
begin
  LLength := Length(FLineComments);

  for i := 0 to LLength - 1 do
    if FLineComments[i] = AToken then
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
