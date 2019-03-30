{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.SynMemoEx.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,

  zObjInspector, zObjInspTypes,

  SynMemoEx;

type
  TfrmSynMemoEx = class(TForm)
    {$REGION 'designer controls'}
    sbrMain     : TStatusBar;
    pnlLeft     : TPanel;
    pnlMain     : TPanel;
    splVertical : TSplitter;
    pnlHeader   : TPanel;
    {$ENDREGION}

  private
    FObjectInspector : TzObjectInspector;
    FMemoEx          : TMemoEx;

   function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  System.TypInfo, System.Rtti,

  DDuce.Components.Factories, DDuce.Factories.zObjInspector, DDuce.Logger,

  Concepts.Factories, Concepts.Utils;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmSynMemoEx.AfterConstruction;
begin
  inherited AfterConstruction;
  FMemoEx := TMemoEx.Create(Self);
  FMemoEx.Parent := pnlMain;
  FMemoEx.Align := alClient;
  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlLeft);
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublic;
  FObjectInspector.Component := FMemoEx;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmSynMemoEx.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
  Result := Result and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;
{$ENDREGION}

end.
