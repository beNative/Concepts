unit BCEditor.Editor.Search.InSelection;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorSearchInSelection = class(TPersistent)
  strict private
    FActive: Boolean;
    FBackground: TColor;
    FOnChange: TBCEditorSearchChangeEvent;
    FSelectionBeginPosition: TBCEditorTextPosition;
    FSelectionEndPosition: TBCEditorTextPosition;
    procedure DoChange;
    procedure SetActive(AValue: Boolean);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
    property SelectionBeginPosition: TBCEditorTextPosition read FSelectionBeginPosition write FSelectionBeginPosition;
    property SelectionEndPosition: TBCEditorTextPosition read FSelectionEndPosition write FSelectionEndPosition;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Background: TColor read FBackground write FBackground default clSearchInSelectionBackground;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write FOnChange;
  end;

implementation

constructor TBCEditorSearchInSelection.Create;
begin
  inherited;

  FActive := False;
  FBackground := clSearchInSelectionBackground;
end;

procedure TBCEditorSearchInSelection.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scInSelectionActive);
end;

procedure TBCEditorSearchInSelection.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSearchInSelection) then
  with ASource as TBCEditorSearchInSelection do
  begin
    Self.FActive := FActive;
    Self.FBackground := FBackground;
    Self.FSelectionBeginPosition := FSelectionBeginPosition;
    Self.FSelectionEndPosition := FSelectionEndPosition;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearchInSelection.SetActive(AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
    DoChange;
  end;
end;

end.
