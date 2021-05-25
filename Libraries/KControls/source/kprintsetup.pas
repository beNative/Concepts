{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kprintsetup; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources, {$IFnDEF LCLWinCE}PrintersDlgs, {$ENDIF}
{$ELSE}
  Windows, Messages, Dialogs,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, KControls, KPrintPreview;

type
  TPrintEvent = procedure(PageSetup: TKPrintPageSetup) of object;

  TExtPrintOption = (
    xpoRange,
    xpoScale
  );
  TExtPrintOptions = set of TExtPrintOption;

  { TKPrintSetupForm }

  TKPrintSetupForm = class(TForm)
    BUConfigure: TButton;
    CoBPrinterName: TComboBox;
    EDTitle: TEdit;
    GBFileToPrint: TGroupBox;
    GBPrinter: TGroupBox;
    GBPrintOptions: TGroupBox;
    LBPrinterName: TLabel;
    BUPrint: TButton;
    BUCancel: TButton;
    CBFitToPage: TCheckBox;
    CBPageNumbers: TCheckBox;
    CBUseColor: TCheckBox;
    GBMargins: TGroupBox;
    CoBMarginUnits: TComboBox;
    LBMarginUnits: TLabel;
    CBMirrorMargins: TCheckBox;
    GBPageSelection: TGroupBox;
    RBAll: TRadioButton;
    RBRange: TRadioButton;
    RBSelectedOnly: TRadioButton;
    LBRangeTo: TLabel;
    LBCopies: TLabel;
    EDLeft: TEdit;
    LBLeft: TLabel;
    LBRight: TLabel;
    EDRight: TEdit;
    EDTop: TEdit;
    LBTop: TLabel;
    EDBottom: TEdit;
    LBBottom: TLabel;
    EDRangeFrom: TEdit;
    EDRangeTo: TEdit;
    EDCopies: TEdit;
    Label1: TLabel;
    EDPrintScale: TEdit;
    LBUnitsLeft: TLabel;
    LBUnitsTop: TLabel;
    LBUnitsRight: TLabel;
    LBUnitsBottom: TLabel;
    BUPreview: TButton;
    CBPaintSelection: TCheckBox;
    BUOk: TButton;
    CBPrintTitle: TCheckBox;
    CBCollate: TCheckBox;
    CBLineNumbers: TCheckBox;
    CBWrapLines: TCheckBox;
    procedure BUConfigureClick(Sender: TObject);
    procedure CoBMarginUnitsChange(Sender: TObject);
    procedure RBAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BUPreviewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EDTopExit(Sender: TObject);
    procedure CBPageNumbersClick(Sender: TObject);
    procedure BUPrintClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FPrevSetup: TKPrintPageSetup;
    FPageSetup: TKPrintPageSetup;
    FPreviewForm: TKCustomPrintPreviewForm;
    FPreviewCreated: Boolean;
    FSelAvail: Boolean;
    FUpdateLock: Boolean;
    FOnPrintClick: TPrintEvent;
    FOptionsVisible: TKPrintOptions;
    FOptionsEnabled: TKPrintOptions;
    FExtOptionsEnabled: TExtPrintOptions;
  {$IFnDEF LCLWinCE}
    FPSD: TPrinterSetupDialog;
  {$ENDIF}
    procedure SetPageSetup(const Value: TKPrintPageSetup);
    procedure SetPreviewForm(const Value: TKCustomPrintPreviewForm);
  protected
    procedure PageSetupToForm; virtual;
    procedure FormToPageSetup; virtual;
    procedure ValidateForm;
  public
    { Public declarations }
    property PageSetup: TKPrintPageSetup read FPageSetup write SetPageSetup;
    property PreviewForm: TKCustomPrintPreviewForm read FPreviewForm write SetPreviewForm;
    property SelAvail: Boolean read FSelAvail write FSelAvail;
    property OnPrintClick: TPrintEvent read FOnPrintClick write FOnPrintClick;
    property OptionsVisible: TKPrintOptions read FOptionsVisible write FOptionsVisible;
    property OptionsEnabled: TKPrintOptions read FOptionsEnabled write FOptionsEnabled;
    property ExtOptionsEnabled: TExtPrintOptions read FExtOptionsEnabled write FExtOptionsEnabled;
  end;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  Printers, KFunctions, KRes, KMessageBox;

procedure TKPrintSetupForm.FormCreate(Sender: TObject);
begin
  FPageSetup := nil;
  FPrevSetup := TKPrintPageSetup.Create(nil);
  FPreviewForm := nil;
  FPreviewCreated := False;
  FOptionsVisible := [poCollate..poUseColor];
  FOptionsEnabled := FOptionsVisible;
  FExtOptionsEnabled := [Low(TExtPrintOption)..High(TExtPrintOption)];
{$IfnDEF LCLWinCE}
  FPSD := TPrinterSetupDialog.Create(Self);
 {$IFDEF FPC}
  FPSD.Title := sPSPrinterSetup;
 {$ENDIF}
{$ENDIF}
end;

procedure TKPrintSetupForm.FormDestroy(Sender: TObject);
begin
  if FPreviewCreated then
  begin
    FPreviewForm.Free;
    FPreviewCreated := False;
  end;
  FPrevSetup.Free;
end;

procedure TKPrintSetupForm.FormShow(Sender: TObject);
begin
  PageSetupToForm;
end;

procedure TKPrintSetupForm.PageSetupToForm;
  function FmtMargin(Value: Double): string;
  const
    Fmt = '%.*f';
  var
    Precision: Integer;
  begin
    case FPageSetup.Units of
      puCM: Precision := 1;
      puMM: Precision := 0;
      puInch: Precision := 2;
    else
      Precision := 0;
    end;
    Result := Format(Fmt, [Precision, Value]);
  end;

  function FmtUnit: string;
  begin
    case FPageSetup.Units of
      puMM: Result := 'mm';
      puInch: Result := '"';
      puHundredthInch: Result := '".100';
    else
      Result := 'cm';
    end;
  end;

  procedure SetupCheckBox(CB: TCheckBox; Option: TKPrintOption);
  begin
    CB.Checked := Option in FPageSetup.Options;
    CB.Enabled := Option in FOptionsEnabled;
    CB.Visible := Option in FOptionsVisible;
  end;

var
  S: string;
begin
  if Assigned(FPageSetup) then
  begin
    FUpdateLock := True;
    try
      SetupCheckBox(CBCollate, poCollate);
      SetupCheckBox(CBFitToPage, poFitToPage);
      SetupCheckBox(CBPageNumbers, poPageNumbers);
      SetupCheckBox(CBUseColor, poUseColor);
      SetupCheckBox(CBPaintSelection, poPaintSelection);
      SetupCheckBox(CBPrintTitle, poTitle);
      SetupCheckBox(CBLineNumbers, poLineNumbers);
      SetupCheckBox(CBWrapLines, poWrapLines);
      SetupCheckBox(CBMirrorMargins, poMirrorMargins);

      try
        CoBPrinterName.Text := '';
        CoBPrinterName.Items.Assign(Printer.Printers);
        CoBPrinterName.ItemIndex := CoBPrinterName.Items.IndexOf(FPageSetup.PrinterName);
        if FPageSetup.IsDefaultPrinter then
        begin
          if CoBPrinterName.ItemIndex < 0 then CoBPrinterName.ItemIndex := Printer.PrinterIndex;
        end else
        begin
          // no default printer selected!
          if CoBPrinterName.Items.Count > 0 then
            CoBPrinterName.ItemIndex := 0;
        end;
      except
        // silent, keep default or successfully obtained data
      end;
      RBSelectedOnly.Enabled := FPageSetup.SelAvail and FSelAvail;
      case FPageSetup.Range of
        prRange: RBRange.Checked := True;
        prAll:   RBAll.Checked := True;
        prSelectedOnly: RBSelectedOnly.Checked := True;
      end;
      RBAll.Caption := Format(sPSAllPages, [FPageSetup.PageCount]);
      RBRange.Enabled := xpoRange in FExtOptionsEnabled;

      EDRangeFrom.Enabled := RBRange.Checked and RBRange.Enabled;
      EDRangeFrom.Text := IntToStr(FPageSetup.StartPage);
      EDRangeTo.Enabled := RBRange.Checked and RBRange.Enabled;
      EDRangeTo.Text := IntToStr(FPageSetup.EndPage);
      EDCopies.Text := IntToStr(FPageSetup.Copies);

      EDPrintScale.Enabled := not CBFitTopage.Checked and (xpoScale in FExtOptionsEnabled);
      EDPrintScale.Text := IntToStr(FPageSetup.Scale);

      EDTitle.Text := FPageSetup.Title;
      CoBMarginUnits.ItemIndex := Integer(FPageSetup.Units);
      S := FmtUnit;
      EDBottom.Text := FmtMargin(FPageSetup.UnitMarginBottom); LBUnitsBottom.Caption := S;
      EDLeft.Text := FmtMargin(FPageSetup.UnitMarginLeft); LBUnitsLeft.Caption := S;
      EDRight.Text := FmtMargin(FPageSetup.UnitMarginRight); LBUnitsRight.Caption := S;
      EDTop.Text := FmtMargin(FPageSetup.UnitMarginTop); LBUnitsTop.Caption := S;
    finally
      FUpdateLock := False;
    end;
  end;
end;

procedure TKPrintSetupForm.FormToPageSetup;
var
  Options: TKPrintOptions;
begin
  if Assigned(FPageSetup) and not FUpdateLock then
  begin
    FPageSetup.LockUpdate;
    try
      Options := [];
      if CBCollate.Checked then Include(Options, poCollate);
      if CBFitToPage.Checked then Include(Options, poFitToPage);
      if CBPageNumbers.Checked then Include(Options, poPageNumbers);
      if CBUseColor.Checked then Include(Options, poUseColor);
      if CBPaintSelection.Checked then Include(Options, poPaintSelection);
      if CBPrintTitle.Checked then Include(Options, poTitle);
      if CBMirrorMargins.Checked then Include(Options, poMirrorMargins);
      if CBLineNumbers.Checked then Include(Options, poLineNumbers);
      if CBWrapLines.Checked then Include(Options, poWrapLines);
      FPageSetup.PrinterName := CoBPrinterName.Text;
      FPageSetup.Options := Options;
      if RBSelectedOnly.Checked then FPageSetup.Range := prSelectedOnly
      else if RBRange.Checked then FPageSetup.Range := prRange
      else FPageSetup.Range := prAll;
      FPageSetup.StartPage := StrToIntDef(EDRangeFrom.Text, FPageSetup.StartPage);
      FPageSetup.EndPage := StrToIntDef(EDRangeTo.Text, FPageSetup.EndPage);
      FPageSetup.Copies := StrToIntDef(EDCopies.Text, FPageSetup.Copies);
      FPageSetup.Scale := StrToIntDef(EDPrintScale.Text, FPageSetup.Scale);
      FPageSetup.Title := EDTitle.Text;
      FPageSetup.Units := TKPrintUnits(CoBMarginUnits.ItemIndex);
      FPageSetup.UnitMarginBottom := StrToFloatDef(AdjustDecimalSeparator(EDBottom.Text), FPageSetup.UnitMarginBottom);
      FPageSetup.UnitMarginLeft := StrToFloatDef(AdjustDecimalSeparator(EDLeft.Text), FPageSetup.UnitMarginLeft);
      FPageSetup.UnitMarginRight := StrToFloatDef(AdjustDecimalSeparator(EDRight.Text), FPageSetup.UnitMarginRight);
      FPageSetup.UnitMarginTop := StrToFloatDef(AdjustDecimalSeparator(EDTop.Text), FPageSetup.UnitMarginTop);
    finally
      FPageSetup.UnlockUpdate;
    end;
  end;
end;

procedure TKPrintSetupForm.BUPrintClick(Sender: TObject);
begin
  FormToPageSetup;
  if Assigned(FOnPrintClick) then
    FOnPrintClick(FPageSetup)
  else
    FPageSetup.PrintOut;
end;

procedure TKPrintSetupForm.BUConfigureClick(Sender: TObject);
var
  PrinterCount: Integer;
begin
{$IFDEF LCLWinCE}
  KMsgBox(sPSErrPrintSetup, sPSErrPrinterConfiguration, [mbOk], miStop)
{$ELSE}
  FormToPageSetup;
  if FPageSetup.IsDefaultPrinter then
  begin
    PrinterCount := 0;
    try
      PrinterCount := Printer.Printers.Count;
      Printer.Orientation := FPageSetup.Orientation;
      Printer.Copies := FPageSetup.Copies;
      if FPSD.Execute then
      begin
        FPageSetup.LockUpdate;
        try
          FPageSetup.Orientation := Printer.Orientation;
          FPageSetup.Copies := Printer.Copies;
        finally
          FPageSetup.UnlockUpdate;
        end;
        PageSetupToForm;
      end;
    except
      if PrinterCount = 0 then
        KMsgBox(sPSErrPrintSetup, sPSErrNoPrinterInstalled, [mbOk], miStop)
      else
        KMsgBox(sPSErrPrintSetup, sPSErrPrinterUnknown, [mbOk], miStop);
    end
  end else
    KMsgBox(sPSErrPrintSetup, sPSErrNoDefaultPrinter, [mbOk], miStop)
{$ENDIF}
end;

procedure TKPrintSetupForm.EDTopExit(Sender: TObject);
begin
  if not FUpdateLock then
    ValidateForm;
end;

procedure TKPrintSetupForm.CoBMarginUnitsChange(Sender: TObject);
begin
  if Assigned(FPageSetup) then
  begin
    FPageSetup.Units := TKPrintUnits(CoBMarginUnits.ItemIndex);
    PageSetupToForm;
  end;
end;

procedure TKPrintSetupForm.CBPageNumbersClick(Sender: TObject);
begin
  FormToPageSetup;
end;

procedure TKPrintSetupForm.RBAllClick(Sender: TObject);
begin
  if not FUpdateLock then
    ValidateForm;
end;

procedure TKPrintSetupForm.SetPageSetup(const Value: TKPrintPageSetup);
begin
  if Value <> FPageSetup then
  begin
    FPrevSetup.Assign(Value);
    FPageSetup := Value;
    PageSetupToForm;
  end;
end;

procedure TKPrintSetupForm.SetPreviewForm(const Value: TKCustomPrintPreviewForm);
begin
  if Value <> FPreviewForm then
  begin
    if FPreviewCreated then
    begin
      FPreviewForm.Free;
      FPreviewCreated := False;
    end;
    FPreviewForm := Value;
  end;
end;

procedure TKPrintSetupForm.ValidateForm;
begin
  FormToPageSetup;
  PageSetupToForm;
end;

procedure TKPrintSetupForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FPreviewCreated then
    FPreviewForm.Hide;
  if ModalResult = mrOk then
    FormToPageSetup
  else if Assigned(FPageSetup) then
    FPageSetup.Assign(FPrevSetup);
end;

procedure TKPrintSetupForm.BUPreviewClick(Sender: TObject);
begin
  ValidateForm;
  if FPreviewForm = nil then
  begin
    FPreviewForm := TKPrintPreviewForm.Create(nil);
    FPreviewCreated := True;
  end;
  if FPreviewForm is TKPrintPreviewForm then
    TKPrintPreviewForm(FPreviewForm).Preview.Control := FPageSetup.Control;
  FPreviewForm.Show;
end;

end.
