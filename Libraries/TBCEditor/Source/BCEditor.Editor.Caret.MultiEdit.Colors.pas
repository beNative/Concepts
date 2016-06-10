unit BCEditor.Editor.Caret.MultiEdit.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorCaretMultiEditColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FForeground: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clBlack;
    property Foreground: TColor read FForeground write FForeground default clWhite;
  end;

implementation

constructor TBCEditorCaretMultiEditColors.Create;
begin
  inherited;

  FBackground := clBlack;
  FForeground := clWhite;
end;

procedure TBCEditorCaretMultiEditColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaretMultiEditColors) then
  with ASource as TBCEditorCaretMultiEditColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
  end
  else
    inherited Assign(ASource);
end;

end.
