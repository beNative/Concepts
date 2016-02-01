unit BCEditor.Highlighter.Comments;

interface

uses
  BCEditor.Types;

type
  TBCEditorHighlighterComments = class(TObject)
  strict private
    FBlockComments: TBCEditorArrayOfString;
    FLineComments: TBCEditorArrayOfString;
  public
    destructor Destroy; override;

    procedure AddBlockComment(const AOpenToken: string; const ACloseToken: string);
    procedure AddLineComment(const AToken: string);
    procedure Clear;
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
end;

procedure TBCEditorHighlighterComments.Clear;
begin
  SetLength(FBlockComments, 0);
  SetLength(FLineComments, 0);
end;

end.
