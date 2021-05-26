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

unit DDuce.Editor.AlignLines.ToolView;

{ Tool for aligning selected lines to a common specified token. }

interface

uses
  System.Classes, System.SysUtils, System.Actions, System.Contnrs,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.ActnList,

  VirtualTrees,

  DSharp.Windows.TreeViewPresenter,

  DDuce.Editor.ToolView.Base, DDuce.Editor.Interfaces, DDuce.Editor.Types,
  DDuce.Editor.AlignLines.Settings,

  DDuce.Logger;

const
  DEFAULT_TOKENS: array[0..28] of string = (
    ':=',
    '=',
    '//',
    '{',
    '(*',
    ':',
    ',',
    ';',
    '@',
    '*',
    '|',
    '--',
    '<<',
    '>>',
    '*)',
    '}',
    '-',
    '.',
    '%',
    '''',
    '"',
    '#',
    '+',
    'read',
    'write',
    'in',
    'as',
    'and',
    'or'
  );

type
  TfrmAlignLines = class(TCustomEditorToolView, IEditorToolView)
    aclMain          : TActionList;
    actExecute       : TAction;
    btnOK            : TButton;
    pnlBottom        : TPanel;
    rgpAlignAt       : TRadioGroup;
    rgpSortDirection : TRadioGroup;
    sbrMain          : TScrollBox;

    procedure actExecuteExecute(Sender: TObject);

    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FTVPDoubleClick(Sender: TObject);
    procedure gbxInsertSpaceItemClick(Sender: TObject; Index: integer);
    procedure gbxOptionsItemClick(Sender: TObject; Index: integer);
    procedure mmoTokensChange(Sender: TObject);
    procedure pnlTokensResize(Sender: TObject);
    procedure rgpAlignAtClick(Sender: TObject);
    procedure rgpSortDirectionClick(Sender: TObject);

  strict private
    //FTVP    : TTreeViewPresenter;
    //FVST    : TVirtualStringTree;
    FTokens : TObjectList; // list of alignment tokens found in selection

  strict protected
    function GetSettings: TAlignLinesSettings;

    procedure AssignDefaultTokens;
    procedure UpdateTokenList;

    { Lets the view respond to changes. }
    procedure UpdateView; override;
    procedure Execute;

    property Settings: TAlignLinesSettings
      read GetSettings;

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Factories.TreeViewPresenter,
  DDuce.Editor.Utils;

resourcestring
  SToken = 'Token';

{$REGION 'TToken'}
type
  TToken = class(TPersistent)
  private
    FToken: string;

  public
    constructor Create(const AToken: string);

  published
    property Token: string
      read FToken write FToken;
  end;

constructor TToken.Create(const AToken: string);
begin
  inherited Create;
  FToken := AToken;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmAlignLines.AfterConstruction;
begin
  inherited AfterConstruction;
  FTokens := TObjectList.Create;
  //FVST := TFactories.CreateVirtualStringTree.Create(Self, pnlVST);
  //FVST.Font.Name := Manager.Settings.EditorFont.Name;
  //FTVP := TTreeViewPresenter.Create(Self);
//  with FTVP.ColumnDefinitions.AddColumn(SToken) do
//  begin
//    Alignment := taCenter;
//  end;
//  if Settings.Tokens.Count = 0 then
//    AssignDefaultTokens;
//  FTVP.ItemsSource   := FTokens;
//  FTVP.TreeView      := FVST;
//  FTVP.ShowHeader    := False;
//  FTVP.OnDoubleClick := FTVPDoubleClick;
//  mmoTokens.Font.Name := Manager.Settings.EditorFont.Name;
//  mmoTokens.Lines.Assign(Settings.Tokens);
//  ActiveControl := FVST;
//  Width := Settings.Width;
end;

procedure TfrmAlignLines.BeforeDestruction;
begin
  FTokens.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmAlignLines.actExecuteExecute(Sender: TObject);
begin
  Execute;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmAlignLines.FormShow(Sender: TObject);
begin
//  mmoTokens.Lines.Assign(Settings.Tokens);
  UpdateTokenList;
//  if View.SelAvail and (FTokens.Count > 0) then
//  begin
//    FVST.SetFocus;
//    FTVP.SelectedItem := FTokens[0];
//  end;
end;

procedure TfrmAlignLines.FTVPDoubleClick(Sender: TObject);
begin
  actExecute.Execute;
end;

procedure TfrmAlignLines.FormResize(Sender: TObject);
begin
  Settings.Width := Width;
end;

procedure TfrmAlignLines.gbxInsertSpaceItemClick(Sender: TObject; Index: integer);
//var
//  B : Boolean;
begin
//  B := (Sender as TCheckGroup).Checked[Index];
//  case Index of
//    0: Settings.KeepSpaceBeforeToken := B;
//    1: Settings.KeepSpaceAfterToken  := B;
//  end;
end;

procedure TfrmAlignLines.gbxOptionsItemClick(Sender: TObject; Index: integer);
//var
//  B : Boolean;
begin
//  B := (Sender as TCheckGroup).Checked[Index];
//  case Index of
//    0: Settings.RemoveWhiteSpace  := B;
//    1: Settings.AlignInParagraphs := B;
//    2: Settings.SortAfterAlign    := B;
//  end;
end;

procedure TfrmAlignLines.mmoTokensChange(Sender: TObject);
begin
//  Settings.Tokens := mmoTokens.Lines;
//  UpdateView;
end;

procedure TfrmAlignLines.pnlTokensResize(Sender: TObject);
begin
//  gbxTokenList.Width := (pnlTokens.ClientWidth + 4) div 2;
end;

procedure TfrmAlignLines.rgpAlignAtClick(Sender: TObject);
begin
//  Settings.AlignToToken := TAlignToToken((Sender as TRadioGroup).ItemIndex);
end;

procedure TfrmAlignLines.rgpSortDirectionClick(Sender: TObject);
begin
//  Settings.SortDirection := TSortDirection((Sender as TRadioGroup).ItemIndex);
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmAlignLines.GetSettings: TAlignLinesSettings;
begin
  Result := inherited Settings.ToolSettings.ItemsByClass[TAlignLinesSettings]
    as TAlignLinesSettings;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmAlignLines.UpdateTokenList;
//var
//  S  : string;
//  O  : string;
//  T  : TToken;
//  ST : TToken; // selected token
begin
//  FTVP.BeginUpdate;
//  ST := nil;
//  O  := '';
//  T  := nil;
//  if Assigned(FTVP.CurrentItem) then
//  begin
//    O := TToken(FTVP.CurrentItem).Token;
//  end;
//  FTokens.Clear;
//  for S in Settings.Tokens do
//  begin
//    if StrContains(S, Manager.ActiveView.SelText) then
//    begin
//      T := TToken.Create(S);
//      FTokens.Add(T);
//      if S = O then
//        ST := T;
//    end;
//  end;
//  FTVP.EndUpdate;
//  FTVP.Refresh;
//  if Assigned(ST) then
//    FTVP.CurrentItem := ST // restore focused item if possible
//  else if FTokens.Count > 0 then
//  begin
//    FTVP.CurrentItem := FTokens[0];
//  end;
end;

procedure TfrmAlignLines.AssignDefaultTokens;
var
  S  : string;
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    for S in DEFAULT_TOKENS do
      SL.Add(S);
    Settings.Tokens := SL;
  finally
    SL.Free;
  end;
end;

procedure TfrmAlignLines.UpdateView;
begin
  UpdateTokenList;
end;

procedure TfrmAlignLines.Execute;
//var
//  T : string;
begin
//  if Assigned(FTVP.CurrentItem) then
//  begin
//    T := TToken(FTVP.CurrentItem).Token;
//    Manager.Commands.AlignSelection(
//      T,
//      gbxOptions.Checked[0],          // Remove whitespace
//      gbxInsertSpace.Checked[0],      // Before token
//      gbxInsertSpace.Checked[1],      // After token
//      gbxOptions.Checked[1]           // Align in paragraphs
//    );
//    if gbxOptions.Checked[2] then // TODO: sort after align
//    begin
//      // TODO
//    end;
//  end;
//  View.Activate;
end;

procedure TfrmAlignLines.UpdateActions;
begin
  inherited UpdateActions;
//  gbxOptions.Checked[0]      := Settings.RemoveWhiteSpace;
//  gbxOptions.Checked[1]      := Settings.AlignInParagraphs;
//  gbxOptions.Checked[2]      := Settings.SortAfterAlign;
//  gbxInsertSpace.Checked[0]  := Settings.KeepSpaceBeforeToken;
//  gbxInsertSpace.Checked[1]  := Settings.KeepSpaceAfterToken;
//  rgpSortDirection.ItemIndex := Integer(Settings.SortDirection);
//  rgpAlignAt.ItemIndex       := Integer(Settings.AlignToToken);
end;
{$ENDREGION}

end.

