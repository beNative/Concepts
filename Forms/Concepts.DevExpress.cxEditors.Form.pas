{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{$I Concepts.inc}

unit Concepts.DevExpress.cxEditors.Form;

interface

uses
  System.Classes, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.Graphics, Vcl.Menus,

  cxGraphics, cxEdit, cxContainer, cxRichEdit, cxCheckBox, cxMemo, cxMaskEdit,
  cxControls, cxTextEdit, cxInplaceContainer, cxVGrid, cxOI, cxClasses,
  cxButtons, dxStatusBar, cxLookAndFeels, cxLookAndFeelPainters, cxStyles;

type
  TfrmcxEditors = class(TForm)
    {$REGION 'designer controls'}
    aclMain                        : TActionList;
    actAssignEditingText           : TAction;
    actAssignEditText              : TAction;
    actAssignEditValue             : TAction;
    actAssignText                  : TAction;
    actClearLog                    : TAction;
    actClearText                   : TAction;
    actValidateEdit                : TAction;
    btnAssignEditingText           : TcxButton;
    btnAssignEditText              : TcxButton;
    btnAssignEditValue             : TcxButton;
    btnAssignText                  : TcxButton;
    btnClearLog                    : TcxButton;
    btnClearText                   : TcxButton;
    btnValidateEdit                : TcxButton;
    chkEditModified                : TcxCheckBox;
    chkModifiedAfterEnter          : TcxCheckBox;
    chkUseDisplayFormatWhenEditing : TCheckBox;
    descMain                       : TcxDefaultEditStyleController;
    edtDisplayFormat               : TcxTextEdit;
    edtEditFormat                  : TcxTextEdit;
    edtEditingText                 : TcxTextEdit;
    edtEditingValue                : TcxTextEdit;
    edtEditMask                    : TcxTextEdit;
    edtEditText                    : TcxTextEdit;
    edtEditValue                   : TcxTextEdit;
    edtMaskEdit                    : TcxMaskEdit;
    edtString                      : TcxTextEdit;
    edtText                        : TcxTextEdit;
    escMain                        : TcxEditStyleController;
    ispTextEdit                    : TcxRTTIInspector;
    lblAssignText                  : TLabel;
    lblDisplayFormat               : TLabel;
    lblEdit                        : TLabel;
    lblEditFormat                  : TLabel;
    lblEditingText                 : TLabel;
    lblEditingValue                : TLabel;
    lblEditMask                    : TLabel;
    lblEditText                    : TLabel;
    lblEditValue                   : TLabel;
    lblText                        : TLabel;
    pnlLeft                        : TPanel;
    pnlRight                       : TPanel;
    pnlStatus                      : TPanel;
    splHorizontal                  : TSplitter;
    splVertical                    : TSplitter;
    sbrMain                        : TdxStatusBar;
    pnlRightBottom                 : TPanel;
    redLog                         : TcxRichEdit;
    pnlBottomRight                 : TPanel;
    lblVarType                     : TLabel;
    lblVariantValue                : TLabel;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAssignEditValueExecute(Sender: TObject);
    procedure actAssignTextExecute(Sender: TObject);
    procedure actAssignEditingTextExecute(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actClearTextExecute(Sender: TObject);
    procedure actAssignEditTextExecute(Sender: TObject);
    procedure actValidateEditExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure edtMaskEditPropertiesChange(Sender: TObject);
    procedure edtMaskEditPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure edtMaskEditPropertiesEditValueChanged(Sender: TObject);
    procedure edtMaskEditFocusChanged(Sender: TObject);
    procedure edtMaskEditEnter(Sender: TObject);
    procedure edtMaskEditExit(Sender: TObject);
    procedure edtMaskEditEditing(Sender: TObject; var CanEdit: Boolean);
    procedure edtEditMaskPropertiesEditValueChanged(Sender: TObject);
    procedure edtDisplayFormatPropertiesEditValueChanged(Sender: TObject);
    procedure edtEditFormatPropertiesEditValueChanged(Sender: TObject);
    procedure chkUseDisplayFormatWhenEditingClick(Sender: TObject);
    procedure edtMaskEditPropertiesNewLookupDisplayText(Sender: TObject;
      const AText: TCaption);
    {$ENDREGION}

  protected
    procedure UpdateActions; override;

    procedure AssignEditValue(AEdit: TcxCustomTextEdit; const AValue: Variant);
    procedure AssignEditText(AEdit: TcxCustomMaskEdit; const AValue: string);
    procedure AssignEditingText(AEdit: TcxCustomTextEdit; const AValue: Variant);
    procedure AssignText(AEdit: TcxCustomTextEdit; const AValue: string);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure LogText(const AText: string; AColor: TColor = clBlack);
    procedure ClearLog;
  end;

implementation

{$R *.dfm}

uses
  System.SysUtils, System.Variants;

{$REGION 'construction and destruction'}
procedure TfrmcxEditors.AfterConstruction;
begin
  inherited;
  edtEditMask.EditValue := edtMaskEdit.Properties.EditMask;
  chkUseDisplayFormatWhenEditing.Checked :=
    edtMaskEdit.Properties.UseDisplayFormatWhenEditing;

  edtEditFormat.EditValue := edtMaskEdit.Properties.EditFormat;
  edtDisplayFormat.EditValue := edtMaskEdit.Properties.DisplayFormat;
end;

procedure TfrmcxEditors.BeforeDestruction;
begin
  inherited;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmcxEditors.actAssignEditValueExecute(Sender: TObject);
begin
  AssignEditValue(edtMaskEdit, edtString.EditValue);
end;

procedure TfrmcxEditors.actAssignEditTextExecute(Sender: TObject);
begin
  AssignEditText(edtMaskEdit, edtString.Text);
end;

procedure TfrmcxEditors.actAssignTextExecute(Sender: TObject);
begin
  AssignText(edtMaskEdit, edtString.Text);
end;

procedure TfrmcxEditors.actAssignEditingTextExecute(Sender: TObject);
begin
  AssignEditingText(edtMaskEdit, edtString.EditValue);
end;

procedure TfrmcxEditors.actClearLogExecute(Sender: TObject);
begin
  ClearLog;
end;

procedure TfrmcxEditors.actClearTextExecute(Sender: TObject);
begin
  edtMaskEdit.Clear;
end;

procedure TfrmcxEditors.actValidateEditExecute(Sender: TObject);
var
  S   : TCaption;
  B   : Boolean;
  V   : Variant;
  Log : string;
begin
  B := edtMaskEdit.Properties.IsEditValueValid(V, False);
  edtMaskEdit.Properties.ValidateDisplayValue(V,S, B, edtMaskEdit);
  Log := Format('%s', [S]);
  LogText(Log, clGreen);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmcxEditors.edtMaskEditPropertiesChange(Sender: TObject);
begin
  LogText('Properties.OnChange', clOlive);
end;

procedure TfrmcxEditors.edtMaskEditPropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  LogText('Properties.OnValidate', clMaroon);
  Error := False;
  ErrorText := 'My ErrorText'
end;

procedure TfrmcxEditors.edtMaskEditPropertiesEditValueChanged(Sender: TObject);
begin
  LogText('Properties.OnEditValueChanged');
end;

procedure TfrmcxEditors.edtMaskEditFocusChanged(Sender: TObject);
var
  S : string;
begin
  S := Format('%s (%s)', ['OnFocusChanged', Screen.ActiveControl.Owner.Name]);
  LogText(S, clGreen);
  if edtMaskEdit.ModifiedAfterEnter then
  begin
    LogText('--- ModifiedAfterEnter ---', clBlue);
  end;
end;

procedure TfrmcxEditors.edtMaskEditEnter(Sender: TObject);
var
  S : string;
begin
  S := Format('%s (%s)', ['OnEnter', Screen.ActiveControl.Owner.Name]);
  LogText(S, clRed);
end;

procedure TfrmcxEditors.edtMaskEditExit(Sender: TObject);
var
  S : string;
begin
  S := Format('%s (%s)', ['OnExit', Screen.ActiveControl.Owner.Name]);
  LogText(S, clRed);
end;

procedure TfrmcxEditors.edtMaskEditEditing(Sender: TObject; var CanEdit: Boolean);
begin
  LogText('OnEditing', clPurple);
end;

procedure TfrmcxEditors.edtMaskEditPropertiesNewLookupDisplayText(
  Sender: TObject; const AText: TCaption);
begin
  LogText('Properties.OnNewLookupDisplayText', clNavy);
end;

procedure TfrmcxEditors.edtEditMaskPropertiesEditValueChanged(Sender: TObject);
begin
  edtMaskEdit.Properties.EditMask := edtEditMask.EditValue;
end;

procedure TfrmcxEditors.edtDisplayFormatPropertiesEditValueChanged(
  Sender: TObject);
begin
  edtMaskEdit.Properties.DisplayFormat := edtDisplayFormat.EditValue;
end;

procedure TfrmcxEditors.edtEditFormatPropertiesEditValueChanged(
  Sender: TObject);
begin
  edtMaskEdit.Properties.EditFormat := edtEditFormat.EditValue;
end;

procedure TfrmcxEditors.chkUseDisplayFormatWhenEditingClick(Sender: TObject);
begin
  edtMaskEdit.Properties.UseDisplayFormatWhenEditing :=
    chkUseDisplayFormatWhenEditing.Checked;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmcxEditors.ClearLog;
begin
  redLog.Lines.Clear;
end;

procedure TfrmcxEditors.LogText(const AText: string; AColor : TColor);
begin
  redLog.SelAttributes.Style := [fsBold];
  redLog.SelAttributes.Color := AColor;
  redLog.Lines.Add(AText);
  redLog.ScrollContent(dirDown);
end;

procedure TfrmcxEditors.UpdateActions;
begin
  inherited;
  edtEditValue.EditValue        := edtMaskEdit.EditValue;
  edtEditingValue.EditValue     := edtMaskEdit.EditingValue;
  edtText.EditValue             := edtMaskEdit.Text;
  edtEditText.EditValue         := edtMaskEdit.EditText;
  edtEditingText.EditValue      := edtMaskEdit.EditingText;
  chkEditModified.Checked       := edtMaskEdit.EditModified;
  chkModifiedAfterEnter.Checked := edtMaskEdit.ModifiedAfterEnter;
  if ActiveControl is TcxCustomInnerTextEdit then
    sbrMain.Panels[1].Text := ActiveControl.Owner.Name
  else
    sbrMain.Panels[1].Text := ActiveControl.Name;
end;

procedure TfrmcxEditors.AssignEditValue(AEdit: TcxCustomTextEdit; const AValue: Variant);
begin
  AEdit.EditValue := AValue;
end;

procedure TfrmcxEditors.AssignEditingText(AEdit: TcxCustomTextEdit;
  const AValue: Variant);
begin
  AEdit.EditingText := VarToStr(AValue);
end;

procedure TfrmcxEditors.AssignText(AEdit: TcxCustomTextEdit; const AValue: string);
begin
  AEdit.Text := AValue;
end;

procedure TfrmcxEditors.AssignEditText(AEdit: TcxCustomMaskEdit; const AValue: string);
begin
  AEdit.EditText := AValue;
end;
{$ENDREGION}

end.
