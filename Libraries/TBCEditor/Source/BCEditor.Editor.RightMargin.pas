unit BCEditor.Editor.RightMargin;

interface

uses
  System.Classes, Vcl.Graphics, System.UITypes, BCEditor.Editor.RightMargin.Colors, BCEditor.Types;

type
  TBCEditorRightMargin = class(TPersistent)
  strict private
    FColors: TBCEditorRightMarginColors;
    FCursor: TCursor;
    FMouseOver: Boolean;
    FMoving: Boolean;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorRightMarginOptions;
    FPosition: Integer;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColors(const AValue: TBCEditorRightMarginColors);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetPosition(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorRightMarginOption; const AEnabled: Boolean);
    property Moving: Boolean read FMoving write FMoving;
    property MouseOver: Boolean read FMouseOver write FMouseOver;
  published
    property Colors: TBCEditorRightMarginColors read FColors write SetColors;
    property Cursor: TCursor read FCursor write FCursor default crHSplit;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Options: TBCEditorRightMarginOptions read FOptions write FOptions default [rmoMouseMove, rmoShowMovingHint];
    property Position: Integer read FPosition write SetPosition default 80;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

constructor TBCEditorRightMargin.Create;
begin
  inherited;

  FVisible := True;
  FPosition := 80;
  FColors := TBCEditorRightMarginColors.Create;
  FOptions := [rmoMouseMove, rmoShowMovingHint];
  FMoving := False;
  FMouseOver := False;
  FCursor := crHSplit;
end;

destructor TBCEditorRightMargin.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorRightMargin.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := AValue;
end;

procedure TBCEditorRightMargin.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorRightMargin) then
  with ASource as TBCEditorRightMargin do
  begin
    Self.FVisible := FVisible;
    Self.FPosition := FPosition;
    Self.FColors.Assign(fColors);
    Self.FOptions := FOptions;
    Self.FCursor := FCursor;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorRightMargin.SetOption(const AOption: TBCEditorRightMarginOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorRightMargin.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorRightMargin.SetColors(const AValue: TBCEditorRightMarginColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorRightMargin.SetPosition(const AValue: Integer);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    DoChange
  end;
end;

procedure TBCEditorRightMargin.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;


end.
