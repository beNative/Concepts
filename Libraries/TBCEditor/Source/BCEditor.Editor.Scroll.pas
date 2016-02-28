unit BCEditor.Editor.Scroll;

interface

uses
  System.Classes, System.UITypes, BCEditor.Types, BCEditor.Editor.Glyph, BCEditor.Editor.Scroll.Hint;

const
  BCEDITOR_DEFAULT_SCROLL_OPTIONS = [soAutosizeMaxWidth, soPastEndOfLine, soShowHint, soWheelClickMove];

type
  TBCEditorScroll = class(TPersistent)
  strict private
    FBars: System.UITypes.TScrollStyle;
    FHint: TBCEditorScrollHint;
    FIndicator: TBCEditorGlyph;
    FMaxWidth: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorScrollOptions;
    procedure DoChange;
    procedure SetBars(const AValue: System.UITypes.TScrollStyle);
    procedure SetHint(const AValue: TBCEditorScrollHint);
    procedure SetIndicator(const AValue: TBCEditorGlyph);
    procedure SetOptions(const AValue: TBCEditorScrollOptions);
    procedure SetMaxWidth(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Bars: System.UITypes.TScrollStyle read FBars write SetBars default System.UITypes.TScrollStyle.ssBoth;
    property Hint: TBCEditorScrollHint read FHint write SetHint;
    property Indicator: TBCEditorGlyph read FIndicator write SetIndicator;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 1024;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorScrollOptions read FOptions write SetOptions default BCEDITOR_DEFAULT_SCROLL_OPTIONS;
  end;

implementation

uses
  BCEditor.Utils, BCEditor.Consts, Vcl.Graphics;

{ TBCEditorScroll }

constructor TBCEditorScroll.Create;
begin
  inherited;

  FOptions := BCEDITOR_DEFAULT_SCROLL_OPTIONS;
  FMaxWidth := 1024;
  FBars := System.UITypes.TScrollStyle.ssBoth;
  FHint := TBCEditorScrollHint.Create;
  FIndicator := TBCEditorGlyph.Create(HINSTANCE, BCEDITOR_MOUSE_MOVE_SCROLL, clFuchsia);
end;

destructor TBCEditorScroll.Destroy;
begin
  FHint.Free;
  FIndicator.Free;

  inherited;
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
    Self.FOptions := FOptions;
    Self.FMaxWidth := FMaxWidth;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorScroll.SetOptions(const AValue: TBCEditorScrollOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

procedure TBCEditorScroll.SetMaxWidth(AValue: Integer);
begin
  AValue := MinMax(AValue, 1, MaxInt - 1);
  if FMaxWidth <> AValue then
  begin
    FMaxWidth := AValue;
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
