unit ChildUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Actions, Vcl.ActnList;

type
  TChildForm = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Edit1: TEdit;
    ActionList: TActionList;
    acCloseChildForm: TAction;
    Button1: TButton;
    procedure acCloseChildFormExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  protected
    procedure Loaded; override;
  public
    procedure AfterConstruction; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

var
  ChildForm: TChildForm;

implementation

{$R *.dfm}

uses
  MainUnit;


procedure TChildForm.acCloseChildFormExecute(Sender: TObject);
begin
  Close;
end;

procedure TChildForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  UpdateLabel(Self, Label1, Edit1, Panel1);
end;

procedure TChildForm.AfterConstruction;
begin
  inherited;
end;

procedure TChildForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  if ParentFont and (Application.MainForm.Monitor.Handle <> Self.Monitor.Handle) then
    Font.Height := MulDiv(Font.Height, NewDPI, OldDPI);
end;

procedure TChildForm.FormCreate(Sender: TObject);
begin
  //Change size of form for 600x400 at 96 DPI
  ClientWidth := MulDiv(600, Self.Monitor.PixelsPerInch, Self.PixelsPerInch);
  ClientHeight := MulDiv(400, Self.Monitor.PixelsPerInch, Self.PixelsPerInch);
  //ClientWidth := MulDiv(600, Screen.PixelsPerInch, Self.PixelsPerInch);
  //ClientHeight := MulDiv(400, Screen.PixelsPerInch, Self.PixelsPerInch);
end;

procedure TChildForm.FormShow(Sender: TObject);
begin
  //Only after FormAfterMonitorDpiChanged you can change Font attributes, because ParentFont goes to False
  Font.Color := clRed;
end;

procedure TChildForm.Loaded;
begin
  //Ensure ParentFont always True for Child Forms
  ParentFont := True;

  inherited;
end;

procedure TChildForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  ;
end;



end.
