{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kprintpreview; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources,
{$ELSE}
  Windows, Messages, ToolWin, ImgList,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ActnList, Buttons, StdCtrls,
  ExtCtrls, KControls;

type
  IPrintPreviewAdapter = interface
    function GetControl: TWinControl;
    function CanPrint: Boolean;
    procedure OnShow;

    procedure NextPage;
    procedure PreviousPage;
    procedure FirstPage;
    procedure LastPage;
    procedure Print;

    function GetFirstPageNumber: Integer;
    function GetLastPageNumber: Integer;
    function GetCurrentPageNumber: Integer;
    procedure SetCurrentPageNumber(Page: Integer);
    function GetScaleMode: TKPreviewScaleMode;
    procedure SetScaleMode(ScaleMode: TKPreviewScaleMode);
    function GetScale: Integer;
    procedure SetScale(Value: Integer);
    procedure SetPreviewChangedEvent(Event: TNotifyEvent);

    property FirstPageNumber: Integer read GetFirstPageNumber;
    property LastPageNumber: Integer read GetLastPageNumber;
    property CurrentPageNumber: Integer read GetCurrentPageNumber write SetCurrentPageNumber;
    property Scale: Integer read GetScale write SetScale;
    property ScaleMode: TKPreviewScaleMode read GetScaleMode write SetScaleMode;
  end;

  { TKCustomPrintPreviewForm }

  TKCustomPrintPreviewForm = class(TForm)
    ILMain: TImageList;
    ALMain: TActionList;
    ACPageFirst: TAction;
    ACPageLast: TAction;
    ACPageNext: TAction;
    ACPagePrevious: TAction;
    ACClose: TAction;
    ToBMain: TToolBar;
    TBPageFirst: TToolButton;
    TBPagePrevious: TToolButton;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    TBPageNext: TToolButton;
    TBPageLast: TToolButton;
    PNPage: TPanel;
    EDPage: TEdit;
    UDPage: TUpDown;
    ToolButton6: TToolButton;
    PNScale: TPanel;
    CoBScale: TComboBox;
    TBClose: TToolButton;
    TBPrint: TToolButton;
    ToolButton4: TToolButton;
    ACPrint: TAction;
    procedure CoBScaleExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ACPageFirstExecute(Sender: TObject);
    procedure ACPageFirstUpdate(Sender: TObject);
    procedure ACPagePreviousExecute(Sender: TObject);
    procedure ACPageNextExecute(Sender: TObject);
    procedure ACPageNextUpdate(Sender: TObject);
    procedure ACPageLastExecute(Sender: TObject);
    procedure ACCloseExecute(Sender: TObject);
    procedure ACCloseUpdate(Sender: TObject);
    procedure EDPageExit(Sender: TObject);
    procedure UDPageClick(Sender: TObject; Button: TUDBtnType);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ACPrintExecute(Sender: TObject);
    procedure ACPrintUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EDPageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FAdapter: IPrintPreviewAdapter;
    procedure ScaleChanged;
    procedure CurrentPageChanged(Sender: TObject = nil);
  public
    constructor Create(AOwner: TComponent; AAdapter: IPrintPreviewAdapter); reintroduce;
    property Adapter: IPrintPreviewAdapter read FAdapter;
  end;

  { TKPrintPreviewForm }

  TKPrintPreviewForm = class(TKCustomPrintPreviewForm, IPrintPreviewAdapter)
  private
    FPreview: TKPrintPreview;

    // IPrintPreviewAdapter
    function GetControl: TWinControl;
    function CanPrint: Boolean;
    procedure OnShow;
    procedure NextPage;
    procedure PreviousPage;
    procedure FirstPage;
    procedure LastPage;
    procedure Print;
    function GetFirstPageNumber: Integer;
    function GetLastPageNumber: Integer;
    function GetCurrentPageNumber: Integer;
    procedure SetCurrentPageNumber(Page: Integer);
    function GetScaleMode: TKPreviewScaleMode;
    procedure SetScaleMode(ScaleMode: TKPreviewScaleMode);
    function GetScale: Integer;
    procedure SetScale(Value: Integer);
    procedure SetPreviewChangedEvent(Event: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    property Preview: TKPrintPreview read FPreview;
  end;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  KFunctions;

{ TKCustomPrintPreviewForm }

constructor TKCustomPrintPreviewForm.Create(AOwner: TComponent;
  AAdapter: IPrintPreviewAdapter);
begin
  inherited CReate(AOwner);
  FAdapter := AAdapter;
end;

procedure TKCustomPrintPreviewForm.FormCreate(Sender: TObject);
var
  PreviewControl: TWinControl;
begin
  CoBScale.ItemIndex := 9; // page width
  PreviewControl := Adapter.GetControl;
  PreviewControl.Parent := Self;
  PreviewControl.Align := alClient;
  PreviewControl.TabStop := True;
  PreviewControl.TabOrder := 0;
end;

procedure TKCustomPrintPreviewForm.FormShow(Sender: TObject);
begin
  FAdapter.SetPreviewChangedEvent(CurrentPageChanged);
  Adapter.OnShow;
  UDPage.Min := Adapter.FirstPageNumber;
  UDPage.Max := Adapter.LastPageNumber;
end;

procedure TKCustomPrintPreviewForm.CoBScaleExit(Sender: TObject);
begin
  ScaleChanged;
end;

procedure TKCustomPrintPreviewForm.ACPageFirstExecute(Sender: TObject);
begin
  Adapter.FirstPage;
end;

procedure TKCustomPrintPreviewForm.ACPageFirstUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Adapter.CurrentPageNumber > Adapter.FirstPageNumber;
end;

procedure TKCustomPrintPreviewForm.ACPagePreviousExecute(Sender: TObject);
begin
  Adapter.PreviousPage;
end;

procedure TKCustomPrintPreviewForm.ACPageNextExecute(Sender: TObject);
begin
  Adapter.NextPage;
end;

procedure TKCustomPrintPreviewForm.ACPageNextUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Adapter.CurrentPageNumber < Adapter.LastPageNumber;
end;

procedure TKCustomPrintPreviewForm.ACPageLastExecute(Sender: TObject);
begin
  Adapter.LastPage;
end;

procedure TKCustomPrintPreviewForm.ACPrintExecute(Sender: TObject);
begin
  Adapter.Print;
end;

procedure TKCustomPrintPreviewForm.ACPrintUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Adapter.CanPrint;
end;

procedure TKCustomPrintPreviewForm.ACCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TKCustomPrintPreviewForm.ACCloseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TKCustomPrintPreviewForm.EDPageExit(Sender: TObject);
begin
  Adapter.CurrentPageNumber := MinMax(StrToIntDef(EDPage.Text, Adapter.CurrentPageNumber),
    Adapter.FirstPageNumber, Adapter.LastPageNumber);
  CurrentPageChanged;
end;

procedure TKCustomPrintPreviewForm.EDPageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    EDPageExit(nil);
end;

procedure TKCustomPrintPreviewForm.UDPageClick(Sender: TObject; Button: TUDBtnType);
begin
  EDPageExit(nil);
end;

procedure TKCustomPrintPreviewForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Close;
    Key := 0;
  end;
end;

procedure TKCustomPrintPreviewForm.ScaleChanged;
var
  S: string;
begin
  S := CoBScale.Text;
  if CoBScale.Items.IndexOf(S) < 0 then
    CoBScale.ItemIndex := -1;
  case CoBScale.ItemIndex of
   -1:
    begin
      while (S <> '') and not CharInSetEx(S[Length(S)], ['0'..'9']) do Delete(S, Length(S), 1);
      Adapter.Scale := StrToIntDef(S, 100);
    end;
    0: Adapter.Scale := 25;
    1: Adapter.Scale := 50;
    2: Adapter.Scale := 75;
    3: Adapter.Scale := 100;
    4: Adapter.Scale := 125;
    5: Adapter.Scale := 150;
    6: Adapter.Scale := 200;
    7: Adapter.Scale := 500;
  end;
  case CoBScale.ItemIndex of
    -1:
    begin
      Adapter.ScaleMode := smScale;
      CobScale.Text := Format('%d %%', [Adapter.Scale]);
    end;
    0..7: Adapter.ScaleMode := smScale;
    8: Adapter.ScaleMode := smWholePage;
    9: Adapter.ScaleMode := smPageWidth;
  end;
end;

procedure TKCustomPrintPreviewForm.CurrentPageChanged(Sender: TObject);
begin
  EDPage.Text := IntToStr(Adapter.CurrentPageNumber);
end;

{ TKPrintPreviewForm }

constructor TKPrintPreviewForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, Self);
  FPreview := TKPrintPreview.Create(Self);
  FPreview.DoubleBuffered := True;
end;

procedure TKPrintPreviewForm.FirstPage;
begin
  Preview.FirstPage;
end;

function TKPrintPreviewForm.GetControl: TWinControl;
begin
  Result := Preview;
end;

function TKPrintPreviewForm.CanPrint: Boolean;
begin
  Result := Assigned(Preview.Control) and Preview.Control.CanPrint;
end;

procedure TKPrintPreviewForm.OnShow;
begin
end;

function TKPrintPreviewForm.GetCurrentPageNumber: Integer;
begin
  Result := Preview.Page;
end;

function TKPrintPreviewForm.GetFirstPageNumber: Integer;
begin
  Result := Preview.StartPage;
end;

function TKPrintPreviewForm.GetLastPageNumber: Integer;
begin
  Result := Preview.EndPage;
end;

function TKPrintPreviewForm.GetScale: Integer;
begin
  REsult := Preview.Scale;
end;

function TKPrintPreviewForm.GetScaleMode: TKPreviewScaleMode;
begin
  Result := Preview.ScaleMode;
end;

procedure TKPrintPreviewForm.LastPage;
begin
  Preview.LastPage;
end;

procedure TKPrintPreviewForm.NextPage;
begin
  Preview.NextPage;
end;

procedure TKPrintPreviewForm.PreviousPage;
begin
  Preview.PreviousPage;
end;

procedure TKPrintPreviewForm.Print;
begin
  Preview.Control.PrintOut
end;

procedure TKPrintPreviewForm.SetCurrentPageNumber(Page: Integer);
begin
  Preview.Page := Page;
end;

procedure TKPrintPreviewForm.SetPreviewChangedEvent(Event: TNotifyEvent);
begin
  FPreview.OnChanged := Event;
end;

procedure TKPrintPreviewForm.SetScale(Value: Integer);
begin
  Preview.Scale := Value;
end;

procedure TKPrintPreviewForm.SetScaleMode(ScaleMode: TKPreviewScaleMode);
begin
  Preview.ScaleMode := ScaleMode;
end;

end.
