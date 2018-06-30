unit BCEditor.Editor.Scroll;

interface

uses
  System.Classes, System.UITypes, BCEditor.Types, BCEditor.Editor.Glyph, BCEditor.Editor.Scroll.Hint,
  BCEditor.Editor.Scroll.Shadow;

const
  BCEDITOR_DEFAULT_SCROLL_OPTIONS = [{soAutosizeMaxWidth,} soPastEndOfLine, soShowVerticalScrollHint, soWheelClickMove];

type
  TBCEditorScroll = class(TPersistent)
  strict private
    FBars: System.UITypes.TScrollStyle;
    FHint: TBCEditorScrollHint;
    FIndicator: TBCEditorGlyph;
    FMaxWidth: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorScrollOptions;
    FShadow: TBCEditorScrollShadow;
    procedure DoChange;
    procedure SetBars(const AValue: System.UITypes.TScrollStyle);
    procedure SetHint(const AValue: TBCEditorScrollHint);
    procedure SetIndicator(const AValue: TBCEditorGlyph);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetOptions(const AValue: TBCEditorScrollOptions);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorScrollOption; const AEnabled: Boolean);
  published
    property Bars: System.UITypes.TScrollStyle read FBars write SetBars default System.UITypes.TScrollStyle.ssBoth;
    property Hint: TBCEditorScrollHint read FHint write SetHint;
    property Indicator: TBCEditorGlyph read FIndicator write SetIndicator;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Options: TBCEditorScrollOptions read FOptions write SetOptions default BCEDITOR_DEFAULT_SCROLL_OPTIONS;
    property Shadow: TBCEditorScrollShadow read FShadow write FShadow;
  end;

implementation

uses
  BCEditor.Consts, Vcl.Graphics;

constructor TBCEditorScroll.Create;
begin
  inherited;

  FOptions := BCEDITOR_DEFAULT_SCROLL_OPTIONS;
  FMaxWidth := 1024;
  FBars := System.UITypes.TScrollStyle.ssBoth;
  FHint := TBCEditorScrollHint.Create;
  FIndicator := TBCEditorGlyph.Create(HInstance, BCEDITOR_MOUSE_MOVE_SCROLL, clFuchsia);
  FShadow := TBCEditorScrollShadow.Create;
end;

destructor TBCEditorScroll.Destroy;
begin
  FHint.Free;
  FIndicator.Free;
  FShadow.Free;

  inherited;
end;

procedure TBCEditorScroll.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FShadow.OnChange := AValue;
end;

procedure TBCEditorScroll.SetBars(const AValue: System.UITypes.TScrollStyle);
begin
  if FBars <> AValue then
  begin
    FBars := AValue;
    DoChange;
  end;
end;

procedure TBCEditorScroll.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorScroll.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorScroll then
  with ASource as TBCEditorScroll do
  begin
    Self.FBars := FBars;
    Self.FHint.Assign(FHint);
    Self.FIndicator.Assign(FIndicator);
    Self.FShadow.Assign(FShadow);
    Self.FOptions := FOptions;
    Self.FMaxWidth := FMaxWidth;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorScroll.SetOption(const AOption: TBCEditorScrollOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorScroll.SetOptions(const AValue: TBCEditorScrollOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

procedure TBCEditorScroll.SetHint(const AValue: TBCEditorScrollHint);
begin
  FHint.Assign(AValue);
end;

procedure TBCEditorScroll.SetIndicator(const AValue: TBCEditorGlyph);
begin
  FIndicator.Assign(AValue);
end;

end.
