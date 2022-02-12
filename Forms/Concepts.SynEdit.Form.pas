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

{$I Concepts.inc}

unit Concepts.SynEdit.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,

  zObjInspector, zObjInspTypes,

  SynEdit, SynEditMiscClasses, SynEditSearch, SynHighlighterPas,
  SynHighlighterURI, SynEditHighlighter, SynHighlighterIni, SynEditOptionsDialog,
  SynURIOpener, SynCompletionProposal, SynEditCodeFolding;

type
  TfrmSynEdit = class(TForm)
    {$REGION 'designer controls'}
    sbrMain     : TStatusBar;
    pnlLeft     : TPanel;
    pnlMain     : TPanel;
    splVertical : TSplitter;
    seMain      : TSynEdit;
    scpMain     : TSynCompletionProposal;
    sacMain     : TSynAutoComplete;
    suoMain     : TSynURIOpener;
    sodMain     : TSynEditOptionsDialog;
    synINI      : TSynIniSyn;
    synURI      : TSynURISyn;
    synPAS      : TSynPasSyn;
    sesSearch   : TSynEditSearch;
    pnlHeader   : TPanel;
    {$ENDREGION}

  private
    FObjectInspector : TzObjectInspector;

   function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.TypInfo, System.Rtti,

  DDuce.Factories.zObjInspector, DDuce.Logger;

{$REGION 'construction and destruction'}
procedure TfrmSynEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlLeft);
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublic;
  FObjectInspector.Component := seMain;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmSynEdit.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
  Result := Result and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;
{$ENDREGION}

end.
