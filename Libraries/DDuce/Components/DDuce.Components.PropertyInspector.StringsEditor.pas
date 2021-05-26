{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DDuce.inc}

unit DDuce.Components.PropertyInspector.StringsEditor;

interface

uses
  System.SysUtils, System.Classes, System.Actions,

  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.StdActns,
  Vcl.ActnList, Vcl.Menus, System.ImageList, Vcl.ImgList;

resourcestring
  SCaption            = 'String List Editor';
  SOkBtnCaption       = '&Ok';
  SCancelBtnCaption   = 'Cancel';
  SLinesCountTemplate = '%d lines';

type
  TStringsEditorDialog = class(TForm)
    btnOk            : TButton;
    btnCancel        : TButton;
    lbLineCount      : TLabel;
    memMain          : TRichEdit;
    ppmMain          : TPopupMenu;
    aclMain          : TActionList;
    actEditCut       : TEditCut;
    actEditCopy      : TEditCopy;
    actEditPaste     : TEditPaste;
    actEditSelectAll : TEditSelectAll;
    actEditUndo      : TEditUndo;
    actEditDelete    : TEditDelete;
    mni1             : TMenuItem;
    mni2             : TMenuItem;
    mni3             : TMenuItem;
    mniN1            : TMenuItem;
    mni4             : TMenuItem;
    mni5             : TMenuItem;
    mniN2            : TMenuItem;
    mni6             : TMenuItem;
    imlMain          : TImageList;

    procedure memMainChange(Sender: TObject);

  private
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);

  public
    function Execute: Boolean;
    property Lines: TStrings
      read GetLines write SetLines;
  end;

implementation

{$R *.dfm}

{$REGION 'property access methods'}
function TStringsEditorDialog.GetLines: TStrings;
begin
  Result := memMain.Lines;
end;

procedure TStringsEditorDialog.SetLines(const Value: TStrings);
begin
  memMain.Lines := Value;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TStringsEditorDialog.memMainChange(Sender: TObject);
begin
  lbLineCount.Caption := Format(SLinesCountTemplate,
    [memMain.Lines.Count]);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TStringsEditorDialog.Execute: Boolean;
begin
  Caption := SCaption;
  btnOk.Caption := SOkBtnCaption;
  btnCancel.Caption := SCancelBtnCaption;
  lbLineCount.Caption := Format(SLinesCountTemplate, [memMain.Lines.Count]);
  Result := (ShowModal = mrOk);
end;
{$ENDREGION}

end.
