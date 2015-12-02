unit BCEditor.Print.Margins;

interface

uses
  System.Classes, System.SysUtils, Vcl.Graphics, BCEditor.Print.Types, BCEditor.Print.PrinterInfo, BCEditor.Utils;

type
  TBCEditorPrintMargins = class(TPersistent)
  strict private
    FBottom: Double;
    FFooter: Double;
    FHeader: Double;
    FInternalMargin: Double;
    FLeft: Double;
    FLeftTextIndent: Double;
    FMargin: Double;
    FMirrorMargins: Boolean;
    FRight: Double;
    FRightTextIndent: Double;
    FTop: Double;
    FUnitSystem: TBCEditorUnitSystem;
    function ConvertFrom(AValue: Double): Double;
    function ConvertTo(AValue: Double): Double;
    function GetBottom: Double;
    function GetFooter: Double;
    function GetHeader: Double;
    function GetInternalMargin: Double;
    function GetLeft: Double;
    function GetLeftTextIndent: Double;
    function GetMargin: Double;
    function GetRight: Double;
    function GetRightTextIndent: Double;
    function GetTop: Double;
    procedure SetBottom(const AValue: Double);
    procedure SetFooter(const AValue: Double);
    procedure SetHeader(const AValue: Double);
    procedure SetInternalMargin(const AValue: Double);
    procedure SetLeft(const AValue: Double);
    procedure SetLeftTextIndent(const AValue: Double);
    procedure SetMargin(const AValue: Double);
    procedure SetRight(const AValue: Double);
    procedure SetRightTextIndent(const AValue: Double);
    procedure SetTop(const AValue: Double);
  public
    PixelBottom: Integer;
    PixelFooter: Integer;
    PixelHeader: Integer;
    PixelInternalMargin: Integer;
    PixelLeft: Integer;
    PixelLeftTextIndent: Integer;
    PixelMargin: Integer;
    PixelRight: Integer;
    PixelRightTextIndent: Integer;
    PixelTop: Integer;
    constructor Create;

    procedure Assign(ASource: TPersistent); override;
    procedure InitPage(ACanvas: TCanvas; APageNumber: Integer; APrinterInfo: TBCEditorPrinterInfo;
      ALineNumbers, ALineNumbersInMargin: Boolean; AMaxLineNumber: Integer);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property Bottom: Double read GetBottom write SetBottom;
    property Footer: Double read GetFooter write SetFooter;
    property Header: Double read GetHeader write SetHeader;
    property InternalMargin: Double read GetInternalMargin write SetInternalMargin;
    property Left: Double read GetLeft write SetLeft;
    property LeftTextIndent: Double read GetLeftTextIndent write SetLeftTextIndent;
    property Margin: Double read GetMargin write SetMargin;
    property MirrorMargins: Boolean read FMirrorMargins write FMirrorMargins;
    property Right: Double read GetRight write SetRight;
    property RightTextIndent: Double read GetRightTextIndent write SetRightTextIndent;
    property Top: Double read GetTop write SetTop;
    property UnitSystem: TBCEditorUnitSystem read FUnitSystem write FUnitSystem default usMM;
  end;

implementation

{ TBCEditorPrintMargins }

const
  mmPerInch = 25.4;
  mmPerCm = 10;

constructor TBCEditorPrintMargins.Create;
begin
  inherited;
  FUnitSystem := usMM;
  FLeft := BCEDITOR_DEFAULT_LEFT_MARGIN_MM;
  FRight := BCEDITOR_DEFAULT_RIGHT_MARGIN_MM;
  FTop := BCEDITOR_DEFAULT_TOP_MARGIN_MM;
  FBottom := BCEDITOR_DEFAULT_BOTTOM_MM;
  FHeader := BCEDITOR_DEFAULT_HEADER_MM;
  FFooter := BCEDITOR_DEFAULT_FOOTER_MM;
  FLeftTextIndent := BCEDITOR_DEFAULT_LEFT_TEXT_INDENT_MM;
  FRightTextIndent := BCEDITOR_DEFAULT_RIGHT_TEXT_INDENT_MM;
  FInternalMargin := BCEDITOR_DEFAULT_INTERNAL_MARGIN_MM;
  FMargin := BCEDITOR_DEFAULT_MARGIN_MM;
  FMirrorMargins := False;
end;

function TBCEditorPrintMargins.ConvertTo(AValue: Double): Double;
begin
  case FUnitSystem of
    usCm:
      Result := AValue * mmPerCm;
    usInch:
      Result := AValue * mmPerInch;
    muThousandthsOfInches:
      Result := mmPerInch * AValue / 1000;
  else
    Result := AValue;
  end;
end;

function TBCEditorPrintMargins.ConvertFrom(AValue: Double): Double;
begin
  case FUnitSystem of
    usCm:
      Result := AValue / mmPerCm;
    usInch:
      Result := AValue / mmPerInch;
    muThousandthsOfInches:
      Result := 1000 * AValue / mmPerInch;
  else
    Result := AValue;
  end;
end;

function TBCEditorPrintMargins.GetBottom: Double;
begin
  Result := ConvertFrom(FBottom);
end;

function TBCEditorPrintMargins.GetFooter: Double;
begin
  Result := ConvertFrom(FFooter);
end;

function TBCEditorPrintMargins.GetMargin: Double;
begin
  Result := ConvertFrom(FMargin);
end;

function TBCEditorPrintMargins.GetHeader: Double;
begin
  Result := ConvertFrom(FHeader);
end;

function TBCEditorPrintMargins.GetLeft: Double;
begin
  Result := ConvertFrom(FLeft);
end;

function TBCEditorPrintMargins.GetRight: Double;
begin
  Result := ConvertFrom(FRight);
end;

function TBCEditorPrintMargins.GetTop: Double;
begin
  Result := ConvertFrom(FTop);
end;

function TBCEditorPrintMargins.GetLeftTextIndent: Double;
begin
  Result := ConvertFrom(FLeftTextIndent);
end;

function TBCEditorPrintMargins.GetRightTextIndent: Double;
begin
  Result := ConvertFrom(FRightTextIndent);
end;

function TBCEditorPrintMargins.GetInternalMargin: Double;
begin
  Result := ConvertFrom(FInternalMargin);
end;

procedure TBCEditorPrintMargins.SetBottom(const AValue: Double);
begin
  FBottom := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.SetFooter(const AValue: Double);
begin
  FFooter := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.SetMargin(const AValue: Double);
begin
  FMargin := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.SetHeader(const AValue: Double);
begin
  FHeader := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.SetLeft(const AValue: Double);
begin
  FLeft := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.SetRight(const AValue: Double);
begin
  FRight := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.SetTop(const AValue: Double);
begin
  FTop := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.SetLeftTextIndent(const AValue: Double);
begin
  FLeftTextIndent := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.SetRightTextIndent(const AValue: Double);
begin
  FRightTextIndent := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.SetInternalMargin(const AValue: Double);
begin
  FInternalMargin := ConvertTo(AValue);
end;

procedure TBCEditorPrintMargins.InitPage(ACanvas: TCanvas; APageNumber: Integer; APrinterInfo: TBCEditorPrinterInfo;
  ALineNumbers, ALineNumbersInMargin: Boolean; AMaxLineNumber: Integer);
begin
  if FMirrorMargins and ((APageNumber mod 2) = 0) then
  begin
    PixelLeft := APrinterInfo.PixFromLeft(FRight);
    PixelRight := APrinterInfo.PrintableWidth - APrinterInfo.PixFromRight(FLeft + FMargin);
  end
  else
  begin
    PixelLeft := APrinterInfo.PixFromLeft(FLeft + FMargin);
    PixelRight := APrinterInfo.PrintableWidth - APrinterInfo.PixFromRight(FRight);
  end;
  if ALineNumbers and (not ALineNumbersInMargin) then
    PixelLeft := PixelLeft + TextWidth(ACanvas, IntToStr(AMaxLineNumber) + ': ');
  PixelTop := APrinterInfo.PixFromTop(FTop);
  PixelBottom := APrinterInfo.PrintableHeight - APrinterInfo.PixFromBottom(FBottom);
  PixelHeader := APrinterInfo.PixFromTop(FHeader);
  PixelFooter := APrinterInfo.PrintableHeight - APrinterInfo.PixFromBottom(FFooter);
  PixelInternalMargin := Round(APrinterInfo.YPixPermm * FInternalMargin);
  PixelMargin := Round(APrinterInfo.XPixPermm * FMargin);
  PixelRightTextIndent := PixelRight - Round(APrinterInfo.XPixPermm * FRightTextIndent);
  PixelLeftTextIndent := PixelLeft + Round(APrinterInfo.XPixPermm * FLeftTextIndent);
end;

procedure TBCEditorPrintMargins.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorPrintMargins) then
  with ASource as TBCEditorPrintMargins do
  begin
    Self.FLeft := FLeft;
    Self.FRight := FRight;
    Self.FTop := FTop;
    Self.FBottom := FBottom;
    Self.FHeader := FHeader;
    Self.FFooter := FFooter;
    Self.FLeftTextIndent := FLeftTextIndent;
    Self.FRightTextIndent := FRightTextIndent;
    Self.FInternalMargin := FInternalMargin;
    Self.FMargin := FMargin;
    Self.FMirrorMargins := FMirrorMargins;
    Self.FUnitSystem := FUnitSystem;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorPrintMargins.LoadFromStream(AStream: TStream);
begin
  with AStream do
  begin
    Read(FUnitSystem, SizeOf(FUnitSystem));
    Read(FLeft, SizeOf(FLeft));
    Read(FRight, SizeOf(FRight));
    Read(FTop, SizeOf(FTop));
    Read(FBottom, SizeOf(FBottom));
    Read(FHeader, SizeOf(FHeader));
    Read(FFooter, SizeOf(FFooter));
    Read(FLeftTextIndent, SizeOf(FLeftTextIndent));
    Read(FRightTextIndent, SizeOf(FRightTextIndent));
    Read(FInternalMargin, SizeOf(FInternalMargin));
    Read(FMargin, SizeOf(FMargin));
    Read(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

procedure TBCEditorPrintMargins.SaveToStream(AStream: TStream);
begin
  with AStream do
  begin
    Write(FUnitSystem, SizeOf(FUnitSystem));
    Write(FLeft, SizeOf(FLeft));
    Write(FRight, SizeOf(FRight));
    Write(FTop, SizeOf(FTop));
    Write(FBottom, SizeOf(FBottom));
    Write(FHeader, SizeOf(FHeader));
    Write(FFooter, SizeOf(FFooter));
    Write(FLeftTextIndent, SizeOf(FLeftTextIndent));
    Write(FRightTextIndent, SizeOf(FRightTextIndent));
    Write(FInternalMargin, SizeOf(FInternalMargin));
    Write(FMargin, SizeOf(FMargin));
    Write(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

end.
