unit BCEditor.Editor.Directories;

interface

uses
  System.Classes;

type
  TBCEditorDirectories = class(TPersistent)
  strict private
    FColors: string;
    FHighlighters: string;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: string read FColors write FColors;
    property Highlighters: string read FHighlighters write FHighlighters;
  end;

implementation

constructor TBCEditorDirectories.Create;
begin
  inherited;

  FColors := 'Colors';
  FHighlighters := 'Highlighters'
end;

procedure TBCEditorDirectories.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorDirectories) then
  with ASource as TBCEditorDirectories do
  begin
    Self.FColors := FColors;
    Self.FHighlighters := FHighlighters;
  end
  else
    inherited Assign(ASource);
end;

end.
