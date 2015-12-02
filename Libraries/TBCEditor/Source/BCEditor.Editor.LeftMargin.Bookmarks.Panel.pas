unit BCEditor.Editor.LeftMargin.Bookmarks.Panel;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types;

type
  TBCEditorLeftMarginBookMarkPanel = class(TPersistent)
  strict private
    FLeftMargin: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorLeftMarginBookMarkPanelOptions;
    FOtherMarkXOffset: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure SetLeftMargin(AValue: Integer);
    procedure SetOtherMarkXOffset(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 2;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorLeftMarginBookMarkPanelOptions read FOptions write FOptions default [bpoToggleBookmarkByClick];
    property OtherMarkXOffset: Integer read FOtherMarkXOffset write SetOtherMarkXOffset default 12;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default 20;
  end;

implementation

uses
  System.Math;

constructor TBCEditorLeftMarginBookMarkPanel.Create;
begin
  inherited;

  FWidth := 20;
  FLeftMargin := 2;
  FOptions := [bpoToggleBookmarkByClick];
  FVisible := True;
  FOtherMarkXOffset := 12;
end;

procedure TBCEditorLeftMarginBookMarkPanel.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMarginBookMarkPanel) then
  with ASource as TBCEditorLeftMarginBookMarkPanel do
  begin
    Self.FLeftMargin := FLeftMargin;
    Self.FOtherMarkXOffset := FOtherMarkXOffset;
    Self.FVisible := FVisible;
    Self.FWidth := FWidth;

    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMarginBookMarkPanel.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginBookMarkPanel.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginBookMarkPanel.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginBookMarkPanel.SetLeftMargin(AValue: Integer);
begin
  if FLeftMargin <> AValue then
  begin
    FLeftMargin := AValue;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginBookMarkPanel.SetOtherMarkXOffset(AValue: Integer);
begin
  if FOtherMarkXOffset <> AValue then
  begin
    FOtherMarkXOffset := AValue;
    DoChange;
  end;
end;

end.
