unit BCEditor.Editor.Search.Map;

interface

uses
  System.Classes, System.UITypes, BCEditor.Editor.Search.Map.Colors, BCEditor.Types;

type
  TBCEditorSearchMap = class(TPersistent)
  strict private
    FAlign: TBCEditorSearchMapAlign;
    FColors: TBCEditorSearchMapColors;
    FCursor: TCursor;
    FOnChange: TBCEditorSearchChangeEvent;
    FOptions: TBCEditorSearchMapOptions;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure SetAlign(const AValue: TBCEditorSearchMapAlign);
    procedure SetOnChange(AValue: TBCEditorSearchChangeEvent);
    procedure SetColors(const AValue: TBCEditorSearchMapColors);
    procedure SetOptions(const AValue: TBCEditorSearchMapOptions);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function GetWidth: Integer;
  published
    property Align: TBCEditorSearchMapAlign read FAlign write SetAlign default saRight;
    property Colors: TBCEditorSearchMapColors read FColors write SetColors;
    property Cursor: TCursor read FCursor write FCursor default crArrow;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write SetOnChange;
    property Options: TBCEditorSearchMapOptions read FOptions write SetOptions default [moShowActiveLine];
    property Visible: Boolean read FVisible write SetVisible default False;
    property Width: Integer read FWidth write SetWidth default 5;
  end;

implementation

uses
  System.Math;

{ TBCEditorSearchMap }

constructor TBCEditorSearchMap.Create;
begin
  inherited;

  FAlign := saRight;
  FColors := TBCEditorSearchMapColors.Create;
  FOptions := [moShowActiveLine];
  FVisible := False;
  FWidth := 5;
  FCursor := crArrow;
end;

destructor TBCEditorSearchMap.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorSearchMap.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorSearchMap then
  with ASource as TBCEditorSearchMap do
  begin
    Self.FAlign := FAlign;
    Self.FVisible := FVisible;
    Self.FOptions := Options;
    Self.FWidth := FWidth;
    Self.FColors.Assign(FColors);
    Self.FCursor := FCursor;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearchMap.SetOnChange(AValue: TBCEditorSearchChangeEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := FOnChange;
end;

procedure TBCEditorSearchMap.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
    FWidth := AValue;
  DoChange;
end;

procedure TBCEditorSearchMap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scSearch);
end;

procedure TBCEditorSearchMap.SetAlign(const AValue: TBCEditorSearchMapAlign);
begin
  if FAlign <> AValue then
  begin
    FAlign := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearchMap.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearchMap.SetOptions(const AValue: TBCEditorSearchMapOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

function TBCEditorSearchMap.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorSearchMap.SetColors(const AValue: TBCEditorSearchMapColors);
begin
  FColors.Assign(AValue);
end;

end.
