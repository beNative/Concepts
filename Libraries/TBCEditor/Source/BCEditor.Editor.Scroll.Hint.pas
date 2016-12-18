unit BCEditor.Editor.Scroll.Hint;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types;

type
  TBCEditorScrollHint = class(TPersistent)
  strict private
    FFormat: TBCEditorScrollHintFormat;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Format: TBCEditorScrollHintFormat read FFormat write FFormat default shfTopLineOnly;
  end;

implementation

constructor TBCEditorScrollHint.Create;
begin
  inherited;

  FFormat := shfTopLineOnly;
end;

procedure TBCEditorScrollHint.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorScrollHint then
  with ASource as TBCEditorScrollHint do
    Self.FFormat := FFormat
  else
    inherited Assign(ASource);
end;

end.
