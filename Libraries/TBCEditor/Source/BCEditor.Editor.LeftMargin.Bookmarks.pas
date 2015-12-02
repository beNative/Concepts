unit BCEditor.Editor.LeftMargin.Bookmarks;

interface

uses
  Vcl.Controls, System.Classes, Vcl.Graphics, BCEditor.Editor.LeftMargin.Bookmarks.Panel;

type
  TBCEditorLeftMarginBookMarks = class(TPersistent)
  strict private
    FImages: TImageList;
    FOnChange: TNotifyEvent;
    FOwner: TComponent;
    FPanel: TBCEditorLeftMarginBookMarkPanel;
    FShortCuts: Boolean;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetImages(const AValue: TImageList);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Images: TImageList read FImages write SetImages;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Panel: TBCEditorLeftMarginBookMarkPanel read FPanel write FPanel;
    property ShortCuts: Boolean read FShortCuts write FShortCuts default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

{ TBCEditorBookMarkOptions }

constructor TBCEditorLeftMarginBookMarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FPanel := TBCEditorLeftMarginBookMarkPanel.Create;
  FShortCuts := True;
  FVisible := True;
end;

destructor TBCEditorLeftMarginBookMarks.Destroy;
begin
  FPanel.Free;

  inherited;
end;

procedure TBCEditorLeftMarginBookMarks.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FPanel.OnChange := AValue;
end;

procedure TBCEditorLeftMarginBookMarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMarginBookMarks) then
  with ASource as TBCEditorLeftMarginBookMarks do
  begin
    Self.FImages := FImages;
    Self.FShortCuts := FShortCuts;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMarginBookMarks.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginBookMarks.SetImages(const AValue: TImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginBookMarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

end.
