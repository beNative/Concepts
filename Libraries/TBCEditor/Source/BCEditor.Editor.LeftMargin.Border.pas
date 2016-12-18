unit BCEditor.Editor.LeftMargin.Border;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types;

type
  TBCEditorLeftMarginBorder = class(TPersistent)
  strict private
    FOnChange: TNotifyEvent;
    FStyle: TBCEditorLeftMarginBorderStyle;
    procedure SetStyle(const AValue: TBCEditorLeftMarginBorderStyle);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Style: TBCEditorLeftMarginBorderStyle read FStyle write SetStyle default mbsNone;
  end;

implementation

constructor TBCEditorLeftMarginBorder.Create;
begin
  inherited;

  FStyle := mbsNone;
end;

procedure TBCEditorLeftMarginBorder.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginBorder.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMarginBorder) then
  with ASource as TBCEditorLeftMarginBorder do
  begin
    Self.FStyle := FStyle;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMarginBorder.SetStyle(const AValue: TBCEditorLeftMarginBorderStyle);
begin
  FStyle := AValue;
  DoChange
end;

end.
