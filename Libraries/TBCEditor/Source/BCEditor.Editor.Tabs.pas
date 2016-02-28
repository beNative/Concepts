unit BCEditor.Editor.Tabs;

interface

uses
  System.Classes, BCEditor.Types;

const
  BCEDITOR_DEFAULT_TAB_OPTIONS = [toColumns, toSelectedBlockIndent];

type
  TBCEditorTabs = class(TPersistent)
  strict private
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorTabOptions;
    FWantTabs: Boolean;
    FWidth: Integer;
    procedure SetWidth(AValue: Integer);
    procedure SetWantTabs(const AValue: Boolean);
    procedure SetOptions(const AValue: TBCEditorTabOptions);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorTabOptions read FOptions write SetOptions default BCEDITOR_DEFAULT_TAB_OPTIONS;
    property WantTabs: Boolean read FWantTabs write SetWantTabs default True;
    property Width: Integer read FWidth write SetWidth default 2;
  end;

implementation

uses
  BCEditor.Utils;

{ TBCEditorTabs }

constructor TBCEditorTabs.Create;
begin
  inherited;

  FOptions := BCEDITOR_DEFAULT_TAB_OPTIONS;
  FWantTabs := True;
  FWidth := 2;
end;

procedure TBCEditorTabs.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorTabs.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTabs then
  with ASource as TBCEditorTabs do
  begin
    Self.FOptions := FOptions;
    Self.FWantTabs := FWantTabs;
    Self.FWidth := FWidth;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTabs.SetOptions(const AValue: TBCEditorTabOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

procedure TBCEditorTabs.SetWidth(AValue: Integer);
begin
  AValue := MinMax(AValue, 1, 256);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange;
  end;
end;

procedure TBCEditorTabs.SetWantTabs(const AValue: Boolean);
begin
  if FWantTabs <> AValue then
  begin
    FWantTabs := AValue;
    DoChange;
  end;
end;

end.
