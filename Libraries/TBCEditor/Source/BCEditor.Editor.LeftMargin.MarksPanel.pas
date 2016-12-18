unit BCEditor.Editor.LeftMargin.MarksPanel;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types;

type
  TBCEditorLeftMarginMarksPanel = class(TPersistent)
  strict private
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorLeftMarginBookMarkPanelOptions;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure SetWidth(AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorLeftMarginBookMarkPanelOptions read FOptions write FOptions default [bpoToggleBookmarkByClick];
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default 20;
  end;

implementation

uses
  System.Math;

constructor TBCEditorLeftMarginMarksPanel.Create;
begin
  inherited;

  FWidth := 20;
  FOptions := [bpoToggleBookmarkByClick];
  FVisible := True;
end;

procedure TBCEditorLeftMarginMarksPanel.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMarginMarksPanel) then
  with ASource as TBCEditorLeftMarginMarksPanel do
  begin
    Self.FVisible := FVisible;
    Self.FWidth := FWidth;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMarginMarksPanel.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginMarksPanel.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginMarksPanel.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

end.
