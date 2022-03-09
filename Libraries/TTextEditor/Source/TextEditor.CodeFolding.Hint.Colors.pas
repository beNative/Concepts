﻿unit TextEditor.CodeFolding.Hint.Colors;

interface

uses
  System.Classes, System.UITypes;

type
  TTextEditorCodeFoldingHintColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FBorder: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default TColors.SysWindow;
    property Border: TColor read FBorder write FBorder default TColors.SysBtnFace;
  end;

implementation

constructor TTextEditorCodeFoldingHintColors.Create;
begin
  inherited;

  FBackground := TColors.SysWindow;
  FBorder := TColors.SysBtnFace;
end;

procedure TTextEditorCodeFoldingHintColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TTextEditorCodeFoldingHintColors) then
  with ASource as TTextEditorCodeFoldingHintColors do
  begin
    Self.FBackground := FBackground;
    Self.FBorder := FBorder;
  end
  else
    inherited Assign(ASource);
end;

end.
