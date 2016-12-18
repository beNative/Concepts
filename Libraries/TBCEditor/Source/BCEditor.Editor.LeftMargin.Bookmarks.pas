unit BCEditor.Editor.LeftMargin.Bookmarks;

interface

uses
  Vcl.Controls, System.Classes, Vcl.Graphics;

type
  TBCEditorLeftMarginBookMarks = class(TPersistent)
  strict private
    FImages: TImageList;
    FLeftMargin: Integer;
    FOnChange: TNotifyEvent;
    FOwner: TComponent;
    FShortCuts: Boolean;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetImages(const AValue: TImageList);
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(ASource: TPersistent); override;
  published
    property Images: TImageList read FImages write SetImages;
    property LeftMargin: Integer read FLeftMargin write FLeftMargin default 2;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ShortCuts: Boolean read FShortCuts write FShortCuts default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

constructor TBCEditorLeftMarginBookMarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FLeftMargin := 2;
  FShortCuts := True;
  FVisible := True;
end;

procedure TBCEditorLeftMarginBookMarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMarginBookMarks) then
  with ASource as TBCEditorLeftMarginBookMarks do
  begin
    Self.FImages := FImages;
    Self.FLeftMargin := FLeftMargin;
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
