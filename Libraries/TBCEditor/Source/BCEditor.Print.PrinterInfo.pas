unit BCEditor.Print.PrinterInfo;

interface

uses
  Winapi.Windows, Vcl.Printers;

type
  TBCEditorPrinterInfo = class
  strict private
    FBottomMargin: Integer;
    FIsUpdated: Boolean;
    FLeftMargin: Integer;
    FPhysicalHeight: Integer;
    FPhysicalWidth: Integer;
    FPrintableHeight: Integer;
    FPrintableWidth: Integer;
    FRightMargin: Integer;
    FTopMargin: Integer;
    FXPixPerInch: Integer;
    FXPixPermm: Single;
    FYPixPerInch: Integer;
    FYPixPermm: Single;
    function GetBottomMargin: Integer;
    function GetLeftMargin: Integer;
    function GetPhysicalHeight: Integer;
    function GetPhysicalWidth: Integer;
    function GetPrintableHeight: Integer;
    function GetPrintableWidth: Integer;
    function GetRightMargin: Integer;
    function GetTopMargin: Integer;
    function GetXPixPerInch: Integer;
    function GetXPixPermm: Single;
    function GetYPixPerInch: Integer;
    function GetYPixPermm: Single;
    procedure FillDefault;
  public
    function PixFromBottom(const AValue: Double): Integer;
    function PixFromLeft(const AValue: Double): Integer;
    function PixFromRight(const AValue: Double): Integer;
    function PixFromTop(const AValue: Double): Integer;
    procedure UpdatePrinter;
    property BottomMargin: Integer read GetBottomMargin;
    property LeftMargin: Integer read GetLeftMargin;
    property PhysicalHeight: Integer read GetPhysicalHeight;
    property PhysicalWidth: Integer read GetPhysicalWidth;
    property PrintableHeight: Integer read GetPrintableHeight;
    property PrintableWidth: Integer read GetPrintableWidth;
    property RightMargin: Integer read GetRightMargin;
    property TopMargin: Integer read GetTopMargin;
    property XPixPerInch: Integer read GetXPixPerInch;
    property XPixPermm: Single read GetXPixPermm;
    property YPixPerInch: Integer read GetYPixPerInch;
    property YPixPermm: Single read GetYPixPermm;
  end;

implementation

{ TBCEditorPrinterInfo }

function TBCEditorPrinterInfo.PixFromBottom(const AValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(AValue * FYPixPermm - FBottomMargin);
end;

function TBCEditorPrinterInfo.PixFromLeft(const AValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(AValue * FXPixPermm - FLeftMargin);
end;

function TBCEditorPrinterInfo.PixFromRight(const AValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(AValue * FXPixPermm - FRightMargin);
end;

function TBCEditorPrinterInfo.PixFromTop(const AValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(AValue * FYPixPermm - FTopMargin);
end;

procedure TBCEditorPrinterInfo.FillDefault;
begin
  FPhysicalWidth := 2481;
  FPhysicalHeight := 3507;
  FPrintableWidth := 2358;
  FPrintableHeight := 3407;
  FLeftMargin := 65;
  FRightMargin := 58;
  FTopMargin := 50;
  FBottomMargin := 50;
  FXPixPerInch := 300;
  FYPixPerInch := 300;
  FXPixPermm := FXPixPerInch / 25.4;
  FYPixPermm := FYPixPerInch / 25.4;
end;

function TBCEditorPrinterInfo.GetBottomMargin: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FBottomMargin;
end;

function TBCEditorPrinterInfo.GetLeftMargin: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FLeftMargin;
end;

function TBCEditorPrinterInfo.GetPhysicalHeight: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPhysicalHeight;
end;

function TBCEditorPrinterInfo.GetPhysicalWidth: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPhysicalWidth;
end;

function TBCEditorPrinterInfo.GetPrintableHeight: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPrintableHeight;
end;

function TBCEditorPrinterInfo.GetPrintableWidth: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPrintableWidth;
end;

function TBCEditorPrinterInfo.GetRightMargin: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FRightMargin;
end;

function TBCEditorPrinterInfo.GetTopMargin: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FTopMargin;
end;

function TBCEditorPrinterInfo.GetXPixPerInch: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FXPixPerInch;
end;

function TBCEditorPrinterInfo.GetXPixPermm: Single;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FXPixPermm;
end;

function TBCEditorPrinterInfo.GetYPixPerInch: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FYPixPerInch;
end;

function TBCEditorPrinterInfo.GetYPixPermm: Single;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FYPixPermm;
end;

procedure TBCEditorPrinterInfo.UpdatePrinter;
begin
  FIsUpdated := True;
  Printer.Refresh;
  if Printer.Printers.Count <= 0 then
  begin
    FillDefault;
    Exit;
  end;
  FPhysicalWidth := GetDeviceCaps(Printer.Handle, Winapi.Windows.PhysicalWidth);
  FPhysicalHeight := GetDeviceCaps(Printer.Handle, Winapi.Windows.PhysicalHeight);
  FPrintableWidth := Printer.PageWidth;
  FPrintableHeight := Printer.PageHeight;
  FLeftMargin := GetDeviceCaps(Printer.Handle, PhysicalOffsetX);
  FTopMargin := GetDeviceCaps(Printer.Handle, PhysicalOffsetY);
  FRightMargin := FPhysicalWidth - FPrintableWidth - FLeftMargin;
  FBottomMargin := FPhysicalHeight - FPrintableHeight - FTopMargin;
  FXPixPerInch := GetDeviceCaps(Printer.Handle, LogPixelsX);
  FYPixPerInch := GetDeviceCaps(Printer.Handle, LogPixelsY);
  FXPixPermm := FXPixPerInch / 25.4;
  FYPixPermm := FYPixPerInch / 25.4;
end;

end.
