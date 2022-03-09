﻿unit TextEditor.Minimap;

interface

uses
  System.Classes, System.UITypes, Vcl.Graphics, TextEditor.Minimap.Colors, TextEditor.Minimap.Indicator,
  TextEditor.Minimap.Shadow, TextEditor.Types;

type
  TTextEditorMinimap = class(TPersistent)
  strict private
    FAlign: TTextEditorMinimapAlign;
    FCharHeight: Integer;
    FClicked: Boolean;
    FColors: TTextEditorMinimapColors;
    FCursor: TCursor;
    FDragging: Boolean;
    FFont: TFont;
    FIndicator: TTextEditorMinimapIndicator;
    FOnChange: TNotifyEvent;
    FOptions: TTextEditorMinimapOptions;
    FShadow: TTextEditorMinimapShadow;
    FTopLine: Integer;
    FVisible: Boolean;
    FVisibleLineCount: Integer;
    FWidth: Integer;
    procedure DoChange;
    procedure SetAlign(const AValue: TTextEditorMinimapAlign);
    procedure SetColors(const AValue: TTextEditorMinimapColors);
    procedure SetFont(AValue: TFont);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function GetWidth: Integer;
    procedure Assign(ASource: TPersistent); override;
    procedure ChangeScale(const AMultiplier, ADivider: Integer);
    procedure SetOption(const AOption: TTextEditorMinimapOption; const AEnabled: Boolean);
    property CharHeight: Integer read FCharHeight write FCharHeight;
    property Clicked: Boolean read FClicked write FClicked;
    property Dragging: Boolean read FDragging write FDragging;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property TopLine: Integer read FTopLine write FTopLine default 1;
    property VisibleLineCount: Integer read FVisibleLineCount write FVisibleLineCount;
  published
    property Align: TTextEditorMinimapAlign read FAlign write SetAlign default maRight;
    property Colors: TTextEditorMinimapColors read FColors write SetColors;
    property Cursor: TCursor read FCursor write FCursor default crArrow;
    property Font: TFont read FFont write SetFont;
    property Indicator: TTextEditorMinimapIndicator read FIndicator write FIndicator;
    property Options: TTextEditorMinimapOptions read FOptions write FOptions default [];
    property Shadow: TTextEditorMinimapShadow read FShadow write FShadow;
    property Visible: Boolean read FVisible write SetVisible default False;
    property Width: Integer read FWidth write SetWidth default 140;
  end;

implementation

uses
  Winapi.Windows, System.Math;

constructor TTextEditorMinimap.Create;
begin
  inherited;

  FAlign := maRight;

  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 1;
  FFont.Style := [];

  FVisible := False;
  FWidth := 140;
  FDragging := False;
  FOptions := [];
  FCursor := crArrow;

  FClicked := False;

  FTopLine := 1;

  FIndicator := TTextEditorMinimapIndicator.Create;
  FColors := TTextEditorMinimapColors.Create;
  FShadow := TTextEditorMinimapShadow.Create;
end;

destructor TTextEditorMinimap.Destroy;
begin
  FFont.Free;
  FIndicator.Free;
  FColors.Free;
  FShadow.Free;

  inherited Destroy;
end;

procedure TTextEditorMinimap.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TTextEditorMinimap) then
  with ASource as TTextEditorMinimap do
  begin
    Self.FAlign := FAlign;
    Self.FColors.Assign(FColors);
    Self.FShadow.Assign(FShadow);
    Self.FFont.Assign(FFont);
    Self.FOptions := FOptions;
    Self.FVisible := FVisible;
    Self.FWidth := FWidth;
    Self.FCursor := FCursor;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TTextEditorMinimap.ChangeScale(const AMultiplier, ADivider: Integer);
begin
  FWidth := MulDiv(FWidth, AMultiplier, ADivider);
  FFont.Height := MulDiv(FFont.Height, AMultiplier, ADivider);
  DoChange;
end;

procedure TTextEditorMinimap.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FFont.OnChange := AValue;
  FIndicator.OnChange := AValue;
  FShadow.OnChange := AValue;
end;

procedure TTextEditorMinimap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TTextEditorMinimap.SetOption(const AOption: TTextEditorMinimapOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TTextEditorMinimap.SetAlign(const AValue: TTextEditorMinimapAlign);
begin
  if FAlign <> AValue then
  begin
    FAlign := AValue;
    DoChange;
  end;
end;

procedure TTextEditorMinimap.SetColors(const AValue: TTextEditorMinimapColors);
begin
  FColors.Assign(AValue);
end;

procedure TTextEditorMinimap.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TTextEditorMinimap.SetWidth(const AValue: Integer);
var
  LValue: Integer;
begin
  LValue := Max(0, AValue);
  if FWidth <> LValue then
  begin
    FWidth := LValue;
    DoChange;
  end;
end;

function TTextEditorMinimap.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TTextEditorMinimap.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

end.
