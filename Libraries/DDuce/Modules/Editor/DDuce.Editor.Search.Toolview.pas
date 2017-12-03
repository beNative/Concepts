{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.Search.Toolview;

{ Searchform to use in combination with an EditorManager instance. This toolform
  provides search/replace functionality for the active view or all views that
  are managed by the EditorManager. }

{
  TODO:
    - only show function keys when focused.
    - copy all matches to a stringlist when regular expressions are used
}

interface

uses
  System.SysUtils, System.Classes, System.Actions,
  Vcl.StdCtrls, Vcl.Forms, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Grids, Vcl.ActnList,
  Vcl.Controls,

  VirtualTrees,

  DSharp.Core.DataTemplates, DSharp.Windows.TreeViewPresenter,
  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  BCEditor.Types, BCEditor.Search,

  DDuce.Editor.ToolView.Base, DDuce.Editor.Interfaces, DDuce.Editor.Types,

  DDuce.Logger;

type
  TfrmSearchForm = class(TCustomEditorToolView, IEditorToolView)
    {$REGION 'designer controls'}
    aclMain               : TActionList;
    actFocusSearchText    : TAction;
    actReplace            : TAction;
    actReplaceAll         : TAction;
    actFind               : TAction;
    btnFind               : TBitBtn;
    btnReplace            : TBitBtn;
    btnReplaceAll         : TBitBtn;
    cbxReplaceWith        : TComboBox;
    cbxSearchText         : TComboBox;
    chkCaseSensitive      : TCheckBox;
    chkMultiLine          : TCheckBox;
    chkRegularExpressions : TCheckBox;
    chkWholeWordsOnly     : TCheckBox;
    grpDirection          : TGroupBox;
    grpOptions            : TGroupBox;
    grpOrigin             : TGroupBox;
    grpReplaceWith        : TGroupBox;
    grpScope              : TGroupBox;
    grpSearchText         : TGroupBox;
    imgF2Key              : TImage;
    pnlButtons            : TPanel;
    pnlOperations         : TPanel;
    pnlResultList         : TPanel;
    rbAllViews            : TRadioButton;
    rbBackward            : TRadioButton;
    rbEntireScope         : TRadioButton;
    pnlStatus             : TPanel;
    rbForward             : TRadioButton;
    rbFromCursor          : TRadioButton;
    rbActiveView          : TRadioButton;
    rbSelection           : TRadioButton;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actFocusSearchTextExecute(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    {$ENDREGION}

    procedure cbxSearchTextChange(Sender: TObject);
    procedure chkClick(Sender: TObject);
    procedure FTVPSelectionChanged(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbBackwardChange(Sender: TObject);
    procedure rbEntireScopeClick(Sender: TObject);
    procedure rbForwardClick(Sender: TObject);
    procedure rbFromCursorClick(Sender: TObject);
    procedure rbActiveViewClick(Sender: TObject);
    procedure rbSelectionClick(Sender: TObject);

  private
    FTVP : TTreeViewPresenter;
    FVST : TVirtualStringTree;

    function GetSearchEngine: IEditorSearchEngine;
    function GetOptions: TBCEditorSearchOptions;
    function GetSearchText: string;
    procedure SetOptions(AValue: TBCEditorSearchOptions);
    procedure SetSearchText(const AValue: string);
    function GetReplaceText: string;
    procedure SetReplaceText(const AValue: string);

    procedure SearchEngineExecute(Sender: TObject);
    procedure SearchEngineChange(Sender: TObject);
    procedure ActionExecute(
          Sender   : TObject;
          AAction  : TBasicAction;
      var AHandled : Boolean
    );

  protected
    procedure EditorSettingsChanged(Sender: TObject); override;

    procedure SettingsChanged; override;
    procedure Execute;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure UpdateActions; override;
    procedure UpdateView; override;

    property Options : TBCEditorSearchOptions
      read GetOptions write SetOptions;

    property SearchText : string
      read GetSearchText write SetSearchText;

    property ReplaceText : string
      read GetReplaceText write SetReplaceText;
  end;

implementation

{$R *.dfm}

uses
  Vcl.Graphics,

  Spring.Collections,

  DDuce.Components.Factories, DDuce.Factories,

  DDuce.Editor.Search.Engine, DDuce.Editor.Search.Data,
  DDuce.Editor.Search.Templates;

resourcestring
  SFileName     = 'FileName';
  SMatchFound   = '%d search match found.';
  SMatchesFound = '%d search matches found.';

{$REGION 'construction and destruction'}
procedure TfrmSearchForm.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := TFactories.CreateVirtualStringTree(Self, pnlResultList);
  FVST.TreeOptions.AutoOptions :=
    FVST.TreeOptions.AutoOptions + [toAutoSpanColumns] ;
  FVST.Header.MainColumn := 0;

  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.ShowHeader  := False;
  FTVP.ListMode    := False;
  FTVP.ColumnDefinitions.Add('Text', 150);
  FTVP.View.ItemsSource := SearchEngine.ItemGroups as IObjectList;
  FTVP.TreeView    := FVST;
  FTVP.View.ItemTemplate := TSearchResultGroupTemplate.Create;
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
  cbxSearchText.Text  := '';
  cbxReplaceWith.Text := '';

  SearchEngine.OnExecute.Add(SearchEngineExecute);
  SearchEngine.OnChange.Add(SearchEngineChange);
  Manager.Events.OnActionExecute.Add(ActionExecute);
end;

procedure TfrmSearchForm.BeforeDestruction;
begin
  if Assigned(SearchEngine) then
  begin
    SearchEngine.OnChange.Remove(SearchEngineChange);
    SearchEngine.OnExecute.Remove(SearchEngineExecute);
  end;
  if Assigned(Manager) then
    Manager.Events.OnActionExecute.Remove(ActionExecute);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmSearchForm.GetSearchEngine: IEditorSearchEngine;
begin
  Result := Owner as IEditorSearchEngine;
end;

function TfrmSearchForm.GetOptions: TBCEditorSearchOptions;
begin
  Result := [];
  if chkCaseSensitive.Checked then
    Include(Result, soCaseSensitive);
  if chkWholeWordsOnly.Checked then
    Include(Result, soWholeWordsOnly);
//  if chkRegularExpressions.Checked then
//    Include(Result, soRegExpr);
//  if chkMultiLine.Checked then
//    Include(Result, soRegExprMultiLine);
  if rbEntireScope.Checked then
    Include(Result, soEntireScope);
//  if rbSelection.Checked then
//    Include(Result, soSelectedOnly);
//  if rbBackward.Checked then
//    Include(Result, soBackwards);
end;

procedure TfrmSearchForm.SetOptions(AValue: TBCEditorSearchOptions);
begin
  chkCaseSensitive.Checked      := soCaseSensitive in AValue;
  chkWholeWordsOnly.Checked     := soWholeWordsOnly in AValue;
//  chkRegularExpressions.Checked := soRegExpr in AValue;
//  chkMultiLine.Checked          := soRegExprMultiLine in AValue;

  if soEntireScope in AValue then
    rbEntireScope.Checked := True
  else
    rbFromCursor.Checked  := True;
//  if soSelectedOnly in AValue then
//    rbSelection.Checked := True
//  else
//    rbActiveView.Checked   := True;
//  if soBackwards in AValue then
//    rbBackward.Checked := True
//  else
//    rbForward.Checked  := True;
  Modified;
end;

function TfrmSearchForm.GetSearchText: string;
begin
  Result := cbxSearchText.Text;
end;

procedure TfrmSearchForm.SetSearchText(const AValue: string);
begin
  if AValue <> SearchText then
  begin
    cbxSearchText.Text := AValue;
  end;
end;

function TfrmSearchForm.GetReplaceText: string;
begin
  Result := cbxReplaceWith.Text;
end;

procedure TfrmSearchForm.SetReplaceText(const AValue: string);
begin
  if AValue <> ReplaceText then
  begin
    cbxReplaceWith.Text := AValue;
  end;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSearchForm.actFocusSearchTextExecute(Sender: TObject);
begin
  cbxSearchText.SetFocus;
end;

procedure TfrmSearchForm.actReplaceAllExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    SearchEngine.ReplaceAll;
    Execute;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmSearchForm.actReplaceExecute(Sender: TObject);
begin
  SearchEngine.Replace;
  Execute;
end;

procedure TfrmSearchForm.actFindExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    Execute;
  finally
    Screen.Cursor := crDefault;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmSearchForm.cbxSearchTextChange(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.chkClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.FormHide(Sender: TObject);
begin
  if Assigned(SearchEngine) then
  begin
    SearchEngine.ItemList.Clear;
    SearchEngine.ItemGroups.Clear;
    FTVP.Refresh;
    SearchEngine.SearchText := '';
    SearchEngine.ReplaceText := '';
  end;
end;

procedure TfrmSearchForm.FormShow(Sender: TObject);
begin
  Options := SearchEngine.Options;
  rbAllViews.Checked  := SearchEngine.SearchAllViews;
  cbxSearchText.Text  := SearchEngine.SearchText;
  cbxReplaceWith.Text := SearchEngine.ReplaceText;
end;

procedure TfrmSearchForm.rbBackwardChange(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbEntireScopeClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbForwardClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbFromCursorClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbActiveViewClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbSelectionClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.EditorSettingsChanged(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.SearchEngineExecute(Sender: TObject);
var
  S : string;
begin
  FTVP.Refresh;
  FVST.Header.AutoFitColumns(False, smaAllColumns, 0);
  if SearchEngine.ItemList.Count = 1 then
    S := SMatchFound
  else
    S := SMatchesFound;
  pnlStatus.Caption := Format(S, [SearchEngine.ItemList.Count]);
  if Visible then
  begin
    if SearchEngine.ItemList.Count > 0 then
    begin
      pnlStatus.Font.Color := clGreen;
      FVST.SetFocus;
      FTVP.SelectedItem := FTVP.View.ItemsSource[0];
    end
    else
    begin
      pnlStatus.Font.Color := clRed;
      cbxSearchText.SetFocus;
      cbxSearchText.SelectAll;
    end;
  end;
end;

procedure TfrmSearchForm.SearchEngineChange(Sender: TObject);
begin
  SearchText  := SearchEngine.SearchText;
  ReplaceText := SearchEngine.ReplaceText;
  if Assigned(FTVP.View.CurrentItem)
    and (TSearchResult(FTVP.View.CurrentItem).Index <> SearchEngine.CurrentIndex + 1)
    then
  begin
    FTVP.View.CurrentItem := SearchEngine.ItemList[SearchEngine.CurrentIndex];
  end;
end;

procedure TfrmSearchForm.ActionExecute(Sender: TObject; AAction: TBasicAction;
  var AHandled: Boolean);
begin
  Logger.Send('Executed', AAction.Name);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSearchForm.Execute;
begin
  SearchEngine.SearchText := cbxSearchText.Text;
  //cbxSearchText.AddHistoryItem(SearchText, 30, True, True);
  SearchEngine.SearchAllViews := rbAllViews.Checked;
  SearchEngine.Options        := Options;
//  Logger.Send(
//    'SearchOptions',
//    SetToString(TypeInfo(TSynSearchOptions),
//    SearchEngine.Options)
//  );
  SearchEngine.Execute;
end;

procedure TfrmSearchForm.UpdateView;
begin
  if rbAllViews.Checked then
  begin
    View.BeginUpdate;
//    View.SetHighlightSearch(
//      SearchEngine.SearchText,
//      SearchEngine.Options
//    );
    View.EndUpdate;
  end;
end;

procedure TfrmSearchForm.SettingsChanged;
begin
  inherited SettingsChanged;
  Options            := SearchEngine.Options;
  rbAllViews.Checked := SearchEngine.SearchAllViews;
end;

{ Updates the active editorview and selects SearchText }

procedure TfrmSearchForm.FTVPSelectionChanged(Sender: TObject);
var
  SR  : TSearchResult;
  SRL : TSearchResultLine;
  SRG : TSearchResultGroup;
begin
  SR := nil;
  if FTVP.View.CurrentItem is TSearchResult then
  begin
    SR := FTVP.View.CurrentItem as TSearchResult;
  end
  else if FTVP.View.CurrentItem is TSearchResultLine then
  begin
    SRL := FTVP.View.CurrentItem as TSearchResultLine;
    SR  := SRL.List[0] as TSearchResult;
  end
  else if FTVP.View.CurrentItem is TSearchResultGroup then
  begin
    SRG := (FTVP.View.CurrentItem as TSearchResultGroup);
    SRL := SRG.Lines[0] as TSearchResultLine;
    SR  := SRL.List[0] as TSearchResult;
  end;
  if Assigned(SR) then
  begin
    Manager.ActivateView(SR.ViewName);
    View.Editor.GotoLineAndCenter(SR.BlockBegin.Y);
    View.Editor.SelectionBeginPosition := TBCEditorTextPosition(SR.BlockBegin);
    View.Editor.SelectionEndPosition   := TBCEditorTextPosition(SR.BlockEnd);
    Modified;
    SearchEngine.CurrentIndex := SearchEngine.ItemList.IndexOf(SR);
  end;
end;

procedure TfrmSearchForm.UpdateActions;
//var
//  B: Boolean;
begin
  inherited UpdateActions;
  { Focus the corresponding search result in the list when we do find next/
    find previous from the editor view. }
//  B := (SearchEngine.ItemList.Count > 0) and (ReplaceText <> '');
//  btnReplace.Visible     := B;
//  btnReplaceAll.Visible  := B;
//  B := not rbAllViews.Checked;
//  grpOrigin.Enabled    := B;
//  grpDirection.Enabled := B;
//  grpOrigin.Visible    := B;
//  grpDirection.Visible := B;
//  if Update then
//  begin
//    SearchEngine.ReplaceText := ReplaceText;
//    if (SearchEngine.SearchText <> SearchText)
//      or (SearchEngine.Options <> Options) then
//    begin
//      SearchEngine.Options    := Options;
//      SearchEngine.SearchText := SearchText;
//      pnlStatus.Caption       := '';
//      SearchEngine.ItemGroups.Clear;
//      SearchEngine.ItemList.Clear;
//      FTVP.Refresh;
//    end;
//    Updated;
//  end;
end;
{$ENDREGION}

end.
