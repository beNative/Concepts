unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Actions, Vcl.ActnList;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Edit1: TEdit;
    ActionList: TActionList;
    acOpenChildForm: TAction;
    Button1: TButton;
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure FormShow(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acOpenChildFormExecute(Sender: TObject);
  private
    FStart, FEnd: TDateTime;
    procedure UpdateDefaultAndSystemFonts;
  protected
    procedure Loaded; override;
  public
    procedure BuildLabels;
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

procedure UpdateLabel(AForm: TForm; ALabel: TLabel; AEdit: TEdit;
  APanel: TPanel);

implementation

{$R *.dfm}

uses
  ChildUnit;

procedure UpdateLabel(AForm: TForm; ALabel: TLabel; AEdit: TEdit;
  APanel: TPanel);
begin
  ALabel.Caption := Format(
    'Parent: %d%s'+
    'Screen.IconFont.Height: %d%s'+
    'Screen.IconFont.PixelsPerInch: %d%s'+
    'App.DefaultFont.Height: %d%s'+
    'App.DefaultFont.PixelsPerInch: %d%s'+
    'Font.Height: %d%s'+
    'Font.PixelsPerInch: %d%s'+
    'Font.Size: %d%s'+
    'Font.Name: %s%s'+
    'Edit.Font.Height: %d%s'+
    'Edit.Height: %d%s'+
    'Panel.Height: %d%s'+
    'Scale Factor: %1.2f - MonitorPPI. %d',
    [Ord(AForm.ParentFont),sLineBreak,
     Screen.IconFont.Height,sLineBreak,
     Screen.IconFont.PixelsPerInch,sLineBreak,
     Application.DefaultFont.Height,sLineBreak,
     Application.DefaultFont.PixelsPerInch,sLineBreak,
     AForm.Font.Height,sLineBreak,
     AForm.Font.pixelsperinch,sLineBreak,
     AForm.Font.Size,sLineBreak,
     AForm.Font.Name,sLineBreak,
     AEdit.Font.Height,sLineBreak,
     AEdit.Height,sLineBreak,
     APanel.Height,sLineBreak,
     AForm.ScaleFactor, AForm.Monitor.PixelsPerInch]);

  AForm.Caption := Format('ClientWidth:%d - ClientHeight:%d',[
    AForm.ClientWidth, AForm.ClientHeight]);
end;

procedure TMainForm.acOpenChildFormExecute(Sender: TObject);
begin
  ChildForm := TChildForm.Create(nil);
  try
    ChildForm.ShowModal;
  finally
    ChildForm.Free;
  end;
end;

procedure TMainForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  UpdateLabel(Self, Label1, Edit1, Panel1);
end;

procedure TMainForm.UpdateDefaultAndSystemFonts;
var
  LHeight: Integer;
begin
  //Update Application.DefaultFont used by ChildForms with ParentFont = True
  Application.DefaultFont.Assign(Font);
  //Update system fonts as user preferences (without using Assign!)
  LHeight := Muldiv(Font.Height, Screen.PixelsPerInch, Monitor.PixelsPerInch);
  Screen.IconFont.Name := Font.Name;
  Screen.IconFont.Height := LHeight;
  Screen.MenuFont.Name := Font.Name;
  Screen.MenuFont.Height := LHeight;
  Screen.MessageFont.Name := Font.Name;
  Screen.MessageFont.Height := LHeight;
  Screen.HintFont.Name := Font.Name;
  Screen.HintFont.Height := LHeight;
  Screen.CaptionFont.Name := Font.Name;
  Screen.CaptionFont.Height := LHeight;
end;

procedure TMainForm.BuildLabels;
var
  i: Integer;
begin
  inherited;
  for I := 0 to 1000 do
  begin
    with TLabel.Create(Self) do
    begin
      Caption := 'Label: '+IntToStr(I);
      Parent := Self;
      SetBounds(I*10,I*10,100,100);
    end;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  i: Integer;
begin
  FStart := Now;
  inherited;
  //BuildLabels;
end;

procedure TMainForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  inherited;
  UpdateDefaultAndSystemFonts;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  H,M,S,MS: Word;
begin
  FEnd := Now;
  DecodeTime((FEnd - fStart),H,M,S,MS);
  Caption := Caption + Format('W:%d - H:%d - %d.%d sec.',[
    ClientWidth, ClientHeight, S, MS]);
end;

procedure TMainForm.Label1Click(Sender: TObject);
begin
  acOpenChildForm.Execute;
end;

procedure TMainForm.Loaded;
begin
  //Very important for HighDPI: on Main Screen ParentFont must be always be False
  ParentFont := False;

  //Acquire system font and size (eg. for windows 10 Segoe UI and 14 at 96 DPI)
  //but without using Assign!
  Font.Name := Screen.IconFont.Name;
  //If you want to use system font Height:
  Font.Height := Muldiv(Screen.IconFont.Height, 96, Screen.IconFont.PixelsPerInch);

  (*
  //Sample assign Font by user preferences:
  Font.Name := 'Century Gothic';
  Font.Color := clBlue;
  Font.Height := -14;
  *)

  inherited;

  //For Child Forms with ParentFont = True
  UpdateDefaultAndSystemFonts;

  //Test build run-time components
  //BuildLabels;
end;

end.
