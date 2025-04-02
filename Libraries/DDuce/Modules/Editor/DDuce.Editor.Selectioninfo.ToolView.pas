{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.SelectionInfo.ToolView;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.ComCtrls,

  DDuce.Editor.Interfaces;

type
  TfrmSelectionInfo = class(TForm, IEditorToolView)
    {$REGION 'designer controls'}
    pgcMain                          : TPageControl;
    tsSelectionInfo                  : TTabSheet;
    tsReflectedProperties            : TTabSheet;
    pnlSelectionInfo                 : TPanel;
    lblStoredBlockBegin              : TLabel;
    lblStoredBlockEnd                : TLabel;
    lblStoredBlockBeginValue         : TLabel;
    lblStoredBlockEndValue           : TLabel;
    lblStoredBlockLines              : TLabel;
    lblStoredBlockSelectionMode      : TLabel;
    lblStoredBlockSelectionModeValue : TLabel;
    lblBlockBegin                    : TLabel;
    lblBlockEnd                      : TLabel;
    lblBlockBeginValue               : TLabel;
    lblBlockEndValue                 : TLabel;
    lblCaretXY                       : TLabel;
    lblCaretXYValue                  : TLabel;
    lblLogicalCaretXY                : TLabel;
    lblLogicalCaretXYValue           : TLabel;
    lblStoredCaretXY                 : TLabel;
    lblStoredCaretXYValue            : TLabel;
    lblLineCount                     : TLabel;
    lblLineCountValue                : TLabel;
    btnStore                         : TButton;
    btnRestore                       : TButton;
    chkLockUpdates                   : TCheckBox;
    chkExcludeEmptyLines             : TCheckBox;
    mmoBlock                         : TMemo;
    mmoReflected                     : TMemo;
    lblSelStart                      : TLabel;
    lblSelEnd                        : TLabel;
    lblSelStartValue                 : TLabel;
    lblSelEndValue                   : TLabel;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure btnRestoreClick(Sender: TObject);
    procedure btnStoreClick(Sender: TObject);
    procedure mmoBlockChange(Sender: TObject);
    {$ENDREGION}

  protected
    {$REGION 'property access methods'}
    function GetView: IEditorView;
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;
    {$ENDREGION}

    { Lets the view respond to changes. }
    procedure UpdateView;
    procedure UpdateDisplay;
    procedure UpdateActions; override;

    property View: IEditorView
      read GetView;

  public
    procedure SetVisible(AValue: Boolean);

  end;

implementation

{$R *.dfm}

uses
  System.TypInfo,

  TextEditor, TextEditor.Types;

{$REGION 'property access mehods'}
function TfrmSelectionInfo.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TfrmSelectionInfo.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmSelectionInfo.GetName: string;
begin
  Result := Name;
end;

function TfrmSelectionInfo.GetVisible: Boolean;
begin
  Result := Visible;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmSelectionInfo.btnStoreClick(Sender: TObject);
begin
//  View.Selection.Store(chkLockUpdates.Checked, chkExcludeEmptyLines.Checked);
end;

procedure TfrmSelectionInfo.mmoBlockChange(Sender: TObject);
begin
  //View.Selection.Text := mmoBlock.Text;
end;

procedure TfrmSelectionInfo.SetVisible(AValue: Boolean);
begin
  inherited Visible := AValue;
end;

procedure TfrmSelectionInfo.btnRestoreClick(Sender: TObject);
begin
//  View.Selection.Restore;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSelectionInfo.UpdateView;
begin
  UpdateDisplay;
end;

procedure TfrmSelectionInfo.UpdateDisplay;
var
  ES : TTextEditor;
begin
  ES := View.Editor;

  lblStoredBlockSelectionModeValue.Caption :=
    GetEnumName(TypeInfo(TTextEditorSelectionMode), Ord(View.SelectionMode));

  lblBlockBeginValue.Caption := Format(
    '(%d, %d)', [View.BlockBegin.X, View.BlockBegin.Y]
  );
  lblBlockEndValue.Caption := Format(
    '(%d, %d)', [View.BlockEnd.X, View.BlockEnd.Y]
  );

  lblCaretXYValue.Caption := Format(
    '(%d, %d)', [View.CaretX, View.CaretY]
  );
  lblLogicalCaretXYValue.Caption := Format(
    '(%d, %d)', [View.LogicalCaretXY.X, View.LogicalCaretXY.Y]
  );

  lblSelStartValue.Caption := View.SelStart.ToString;
  lblSelEndValue.Caption   := View.SelEnd.ToString;


  lblLineCountValue.Caption := IntToStr(ES.Lines.Count);
  //lblStoredBlockLines.Caption := ES.Text;
  mmoBlock.Lines.Text         := ES.SelectedText;

  //mmoReflected.Text := Reflect.Properties(ES.Highlighter.GetCurrentRange).ToString;


end;

procedure TfrmSelectionInfo.UpdateActions;
begin
  inherited UpdateActions;
  UpdateDisplay;
end;
{$ENDREGION}

end.

