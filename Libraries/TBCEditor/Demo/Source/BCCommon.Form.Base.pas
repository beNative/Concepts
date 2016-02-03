unit BCCommon.Form.Base;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BCComponent.SkinProvider, BCComponent.TitleBar,
  BCComponent.SkinManager, BCControl.StatusBar, Vcl.ActnList, BCControl.ProgressBar,
  Vcl.AppEvnts, Vcl.Menus, sSkinManager, System.Win.TaskbarCore, Vcl.Taskbar, System.Actions, sSkinProvider, acTitleBar,
  Vcl.ComCtrls, sStatusBar;

type
  TBCBaseForm = class(TForm)
    ActionFileExit: TAction;
    ActionList: TActionList;
    ApplicationEvents: TApplicationEvents;
    MainMenu: TMainMenu;
    SkinManager: TBCSkinManager;
    SkinProvider: TBCSkinProvider;
    StatusBar: TBCStatusBar;
    TitleBar: TBCTitleBar;
    procedure ActionFileExitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProgressBarHide(Sender: TObject);
    procedure ProgressBarShow(Sender: TObject);
    procedure ProgressBarStepChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProgressBar: TBCProgressBar;
    FSkinChange: TNotifyEvent;
    FTaskbar: TTaskbar;
    procedure CreateProgressBar;
    procedure ResizeProgressBar;
  public
    property ProgressBar: TBCProgressBar read FProgressBar write FProgressBar;
    property Taskbar: TTaskbar read FTaskbar write FTaskbar;
    property OnSkinChange: TNotifyEvent read FSkinChange write FSkinChange;
  end;

implementation

{$R *.dfm}

uses
  Winapi.CommCtrl;

procedure TBCBaseForm.ActionFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TBCBaseForm.FormCreate(Sender: TObject);
begin
  inherited;
  {$WARN SYMBOL_PLATFORM OFF}
  SkinManager.SkinDirectory := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName)) +
    SkinManager.SkinDirectory;
  {$WARN SYMBOL_PLATFORM ON}
  SkinManager.Active := True;
  FTaskbar := TTaskBar.Create(Self);
  CreateProgressBar;
end;

procedure TBCBaseForm.FormDestroy(Sender: TObject);
begin
  FTaskbar.Free;
  FProgressBar.Free;
end;

procedure TBCBaseForm.ResizeProgressBar;
var
  LRect: TRect;
begin
  if Assigned(FProgressBar) then
  begin
    Statusbar.Perform(SB_GETRECT, 4, Integer(@LRect));
    FProgressBar.Top := LRect.Top;
    FProgressBar.Left := LRect.Left - 3;
    FProgressBar.Width := LRect.Right - LRect.Left - 3;
    FProgressBar.Height := LRect.Bottom - LRect.Top - 1;
  end;
end;

procedure TBCBaseForm.ProgressBarStepChange(Sender: TObject);
begin
  Taskbar.ProgressValue := FProgressBar.Progress;
end;

procedure TBCBaseForm.ProgressBarShow(Sender: TObject);
begin
  ResizeProgressBar;
  Taskbar.ProgressMaxValue := FProgressBar.MaxValue;
  Taskbar.ProgressState := TTaskBarProgressState.Normal;
end;

procedure TBCBaseForm.ProgressBarHide(Sender: TObject);
begin
  Taskbar.ProgressState := TTaskBarProgressState.None;
end;

procedure TBCBaseForm.CreateProgressBar;
begin
  FProgressBar := TBCProgressBar.Create(StatusBar);
  FProgressBar.OnStepChange := ProgressBarStepChange;
  FProgressBar.OnShow := ProgressBarShow;
  FProgressBar.OnHide := ProgressBarHide;
  FProgressBar.Hide;
  FProgressBar.Parent := Statusbar;
end;

end.
