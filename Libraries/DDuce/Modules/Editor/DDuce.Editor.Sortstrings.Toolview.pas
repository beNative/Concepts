{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.SortStrings.ToolView;

interface

uses
  System.Classes, System.SysUtils, System.Actions,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ActnList,

  DDuce.Editor.SortStrings.Settings, DDuce.Editor.ToolView.Base;

type
  TfrmSortStrings = class(TCustomEditorToolView)
    {$REGION 'designer controls'}
    aclMain          : TActionList;
    actExecute       : TAction;
    btnOK            : TButton;
    pnlBottom        : TPanel;
    rgpSortDirection : TRadioGroup;
    rgpSortScope     : TRadioGroup;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actExecuteExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure FormResize(Sender: TObject);
    procedure gbxOptionsItemClick(Sender: TObject; Index: Integer);
    procedure rgpSortScopeClick(Sender: TObject);
    procedure rgpSortDirectionClick(Sender: TObject);
    {$ENDREGION}

  private
    {$REGION 'property access methods'}
    function GetSettings: TSortStringsSettings;
    {$ENDREGION}

  protected
    procedure UpdateActions; override;

    property Settings: TSortStringsSettings
      read GetSettings;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Editor.Types;

{$REGION 'construction and destruction'}
procedure TfrmSortStrings.AfterConstruction;
begin
  inherited AfterConstruction;
  Width := Settings.Width;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmSortStrings.GetSettings: TSortStringsSettings;
begin
  Result := inherited Settings
    .ToolSettings.ItemsByClass[TSortStringsSettings] as TSortStringsSettings;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSortStrings.actExecuteExecute(Sender: TObject);
begin
  Manager.Commands.SortSelectedLines;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmSortStrings.FormResize(Sender: TObject);
begin
  //Settings.Width := Width;
end;

procedure TfrmSortStrings.gbxOptionsItemClick(Sender: TObject; Index: Integer);
//var
//  B : Boolean;
begin
  //B := (Sender as TCheckGroup).Checked[Index];
//  case Index of
//    0: Settings.CaseSensitive := B;
//    1: Settings.IgnoreSpaces  := B;
//  end;
end;

procedure TfrmSortStrings.rgpSortScopeClick(Sender: TObject);
begin
  Settings.SortScope := TSortScope((Sender as TRadioGroup).ItemIndex);
end;

procedure TfrmSortStrings.rgpSortDirectionClick(Sender: TObject);
begin
  Settings.SortDirection := TSortDirection((Sender as TRadioGroup).ItemIndex);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSortStrings.UpdateActions;
begin
  inherited UpdateActions;
//  gbxOptions.Checked[0]      := Settings.CaseSensitive;
//  gbxOptions.Checked[1]      := Settings.IgnoreSpaces;
  rgpSortDirection.ItemIndex := Integer(Settings.SortDirection);
  rgpSortScope.ItemIndex     := Integer(Settings.SortScope);
end;
{$ENDREGION}

end.

