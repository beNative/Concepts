unit BCEditor.Editor.LeftMargin.Marks;

interface

uses
  Vcl.Controls, System.Classes, Vcl.Graphics;

type
  TBCEditorLeftMarginMarks = class(TPersistent)
  strict private
    FDefaultImageIndex: Integer;
    FImages: TImageList;
    FLeftMargin: Integer;
    FOnChange: TNotifyEvent;
    FOverlappingOffset: Integer;
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
    property DefaultImageIndex: Integer read FDefaultImageIndex write FDefaultImageIndex default -1;
    property Images: TImageList read FImages write SetImages;
    property LeftMargin: Integer read FLeftMargin write FLeftMargin default 2;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OverlappingOffset: Integer read FOverlappingOffset write FOverlappingOffset default 4;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

constructor TBCEditorLeftMarginMarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FDefaultImageIndex := -1;
  FLeftMargin := 2;
  FOverlappingOffset := 4;
  FShortCuts := True;
  FVisible := True;
end;

procedure TBCEditorLeftMarginMarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMarginMarks) then
  with ASource as TBCEditorLeftMarginMarks do
  begin
    Self.FDefaultImageIndex := FDefaultImageIndex;
    Self.FImages := FImages;
    Self.FLeftMargin := FLeftMargin;
    Self.FOverlappingOffset := FOverlappingOffset;
    Self.FShortCuts := FShortCuts;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMarginMarks.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginMarks.SetImages(const AValue: TImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginMarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

end.
