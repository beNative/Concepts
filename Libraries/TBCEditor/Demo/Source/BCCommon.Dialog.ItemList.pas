unit BCCommon.Dialog.ItemList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BCCommon.Dialog.Base, Vcl.StdCtrls, BCControl.Panel,
  BCControl.SpeedButton, sListBox, Vcl.ActnList, System.Actions, Vcl.Buttons, sSpeedButton, Vcl.ExtCtrls, sPanel;

type
  TItemListDialog = class(TBCBaseDialog)
    PanelButtons: TBCPanel;
    ButtonFind: TButton;
    ButtonCancel: TButton;
    ListBox: TsListBox;
    PanelTop: TBCPanel;
    SpeedButtonDivider1: TBCSpeedButton;
    SpeedButtonDelete: TBCSpeedButton;
    SpeedButtonInsert: TBCSpeedButton;
    SpeedButtonClear: TBCSpeedButton;
    ActionList: TActionList;
    ActionInsert: TAction;
    ActionDelete: TAction;
    ActionClearAll: TAction;
    SpeedButtonDivider2: TBCSpeedButton;
    SpeedButtonMoveUp: TBCSpeedButton;
    SpeedButtonMoveDown: TBCSpeedButton;
    ActionMoveUp: TAction;
    ActionMoveDown: TAction;
    ActionSort: TAction;
    SpeedButtonSort: TBCSpeedButton;
    procedure ActionInsertExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionClearAllExecute(Sender: TObject);
    procedure ActionSortExecute(Sender: TObject);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  System.Math, BCCommon.Dialog.InputQuery, BCCommon.Language.Strings;

{$R *.dfm}

procedure TItemListDialog.ActionClearAllExecute(Sender: TObject);
begin
  ListBox.Clear;
end;

procedure TItemListDialog.ActionDeleteExecute(Sender: TObject);
begin
  ListBox.Items.Delete(ListBox.ItemIndex);
end;

procedure TItemListDialog.ActionInsertExecute(Sender: TObject);
var
  LValue: string;
begin
  if TInputQueryDialog.ClassShowModal(Self, LanguageDataModule.GetConstant('Insert'), LValue) = mrOk then
    ListBox.Items.Insert(ListBox.ItemIndex, LValue);
end;

procedure TItemListDialog.ActionMoveDownExecute(Sender: TObject);
begin
  inherited;
  if ListBox.ItemIndex <> -1 then
    ListBox.Items.Move(ListBox.ItemIndex, Min(ListBox.ItemIndex + 1, ListBox.Count - 1));
end;

procedure TItemListDialog.ActionMoveUpExecute(Sender: TObject);
begin
  inherited;
  if ListBox.ItemIndex <> -1 then
    ListBox.Items.Move(ListBox.ItemIndex, Max(0, ListBox.ItemIndex - 1));
end;

procedure TItemListDialog.ActionSortExecute(Sender: TObject);
begin
  inherited;
  ListBox.Sorted := True;
  ListBox.Sorted := False;
end;

end.
