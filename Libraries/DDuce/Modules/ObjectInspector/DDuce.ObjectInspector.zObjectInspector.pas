{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.ObjectInspector.zObjectInspector;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Contnrs,
  System.Actions,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ActnList,

  zObjInspector, zObjInspTypes;

type
  TfrmComponentInspectorzObjectInspector = class(TForm)
    pnlMain        : TPanel;
    cbxInspector   : TComboBox;
    aclMain        : TActionList;
    actExpandAll   : TAction;
    actCollapseAll : TAction;

    procedure cbxInspectorChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure actExpandAllExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);

  private
    FObjectInspector : TzObjectInspector;
    FObjectHost      : TzObjectHost;

    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;

  public
    constructor Create(
      AOwner  : TComponent;
      AObject : TObject
    ); reintroduce;
    procedure BeforeDestruction; override;

    procedure CreateObjectInspector;

    procedure AddComponentToInspector(AComponent: TObject); virtual;
    procedure FocusComponentInInspector(AComponent: TObject); virtual;
  end;

procedure InspectComponent(AComponent : TComponent);

procedure InspectObject(AObject : TObject);

procedure InspectComponents(AComponent : TComponent); overload;

procedure InspectApplicationComponents;

procedure InspectComponents(AComponents : array of TComponent); overload;

procedure InspectComponents(AComponents : TComponentList); overload;

implementation

{$R *.dfm}

uses
  System.Rtti, System.TypInfo,

  DDuce.Factories.zObjInspector;

{$REGION 'interfaced routines'}
procedure InspectComponent(AComponent : TComponent);
var
  InspectorForm : TfrmComponentInspectorzObjectInspector;
begin
  if Assigned(AComponent) then
  begin
    InspectorForm := TfrmComponentInspectorzObjectInspector.Create(
      Application,
      AComponent
    );
    InspectorForm.Show;
  end
  else
    raise Exception.Create('No component Assigned');
end;

procedure InspectObject(AObject : TObject);
var
  InspectorForm : TfrmComponentInspectorzObjectInspector;
begin
  if Assigned(AObject) then
  begin
    InspectorForm := TfrmComponentInspectorzObjectInspector.Create(Application, AObject);
    InspectorForm.Show;
  end
  else
    raise Exception.Create('No component Assigned');
end;

procedure InspectComponents(AComponents : array of TComponent);
var
  InspectorForm : TfrmComponentInspectorzObjectInspector;
  I             : Integer;
begin
  if Length(AComponents) > 0 then
  begin
    InspectorForm := TfrmComponentInspectorzObjectInspector.Create(Application, AComponents[0]);
    for I := 1 to High(AComponents) do
      InspectorForm.AddComponentToInspector(AComponents[I]);
    InspectorForm.Show;
    InspectorForm.FocusComponentInInspector(AComponents[0]);
  end
  else
    raise Exception.Create('Component array is empty');
end;

procedure InspectComponents(AComponents : TComponentList);
var
  InspectorForm : TfrmComponentInspectorzObjectInspector;
  I             : Integer;
begin
  if Assigned(AComponents) then
  begin
    if AComponents.Count > 0 then
    begin
      InspectorForm := TfrmComponentInspectorzObjectInspector.Create(Application, AComponents[0]);
      for I := 1 to AComponents.Count - 1 do
        InspectorForm.AddComponentToInspector(AComponents[I]);
    InspectorForm.Show;
    InspectorForm.FocusComponentInInspector(AComponents[0]);
  end
  end
  else
    raise Exception.Create('Componentlist not assigned');
end;

procedure InspectComponents(AComponent : TComponent);
var
  CL : TComponentList;
  I  : Integer;
begin
  CL := TComponentList.Create(False);
  try
    for I := 0 to AComponent.ComponentCount - 1 do
      CL.Add(AComponent.Components[I]);
    InspectComponents(CL);
  finally
    CL.Free;
  end;
end;

procedure InspectApplicationComponents;
var
  CL    : TComponentList;
  I, J  : Integer;
begin
  CL := TComponentList.Create(False);
  try
    for I := 0 to Screen.FormCount - 1 do
      for J :=  0 to Screen.Forms[I].ComponentCount - 1 do
        CL.Add(Screen.Forms[I].Components[J]);

    for I := 0 to Screen.DataModuleCount - 1 do
      for J :=  0 to Screen.DataModules[I].ComponentCount - 1 do
        CL.Add(Screen.DataModules[I].Components[J]);

    InspectComponents(CL);
  finally
    CL.Free;
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TfrmComponentInspectorzObjectInspector.Create(AOwner: TComponent; AObject: TObject);
begin
  inherited Create(AOwner);
  CreateObjectInspector;
  AddComponentToInspector(AObject);
  FocusComponentInInspector(AObject);
end;

procedure TfrmComponentInspectorzObjectInspector.BeforeDestruction;
begin
  if Assigned(FObjectHost) then
    FObjectHost.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'message handlers'}
procedure TfrmComponentInspectorzObjectInspector.CMDialogKey(var Msg: TCMDialogKey);
begin
  if Msg.CharCode = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else
    inherited;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmComponentInspectorzObjectInspector.actCollapseAllExecute(
  Sender: TObject);
begin
  FObjectInspector.CollapseAll;
end;

procedure TfrmComponentInspectorzObjectInspector.actExpandAllExecute(
  Sender: TObject);
begin
  FObjectInspector.ExpandAll;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmComponentInspectorzObjectInspector.cbxInspectorChange(Sender: TObject);
var
  CBX : TComboBox;
begin
  CBX := Sender as TComboBox;
  FObjectInspector.BeginUpdate;
  try
    if CBX.ItemIndex > -1 then
    begin
      // this assignment will destroy the assigned ObjectHost object!!
      FObjectInspector.Component := CBX.Items.Objects[CBX.ItemIndex];
      FObjectHost := nil;
      FObjectInspector.SortByCategory := False;
    end;
  finally
    FObjectInspector.EndUpdate;
  end;
end;

procedure TfrmComponentInspectorzObjectInspector.FormActivate(Sender: TObject);
begin
  FObjectInspector.Refresh;
end;

procedure TfrmComponentInspectorzObjectInspector.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmComponentInspectorzObjectInspector.FormResize(Sender: TObject);
begin
  FObjectInspector.SplitterPos := FObjectInspector.ClientWidth div 2;
end;

procedure TfrmComponentInspectorzObjectInspector.FormShow(Sender: TObject);
begin
  Height := Screen.WorkAreaHeight;
  Width  := 400;
  Left   := Screen.WorkAreaLeft;
  Top    := Screen.WorkAreaTop;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmComponentInspectorzObjectInspector.CreateObjectInspector;
begin
  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlMain);
  FObjectHost      := TzObjectHost.Create;
  FObjectInspector.Component        := FObjectHost;
  FObjectInspector.SplitterPos      := FObjectInspector.Width div 2;
  FObjectInspector.SortByCategory   := False;
  FObjectInspector.ObjectVisibility := mvPublic;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmComponentInspectorzObjectInspector.AddComponentToInspector(
  AComponent: TObject);
var
  S     : string;
  sName : string;
  CI    : TCollectionItem;
begin
  if AComponent is TComponent then
  begin
    sName := TComponent(AComponent).Name;
    if sName = '' then
      sName := 'unnamed';
    S := Format('%s - %s', [sName, AComponent.ClassName]);
    cbxInspector.Items.AddObject(S, AComponent);
  end
  else
  if AComponent is TCollection then
  begin
    for CI in TCollection(AComponent) do
    begin
        S := Format('%s[%d]', [CI.ClassName,
          (CI as TCollectionItem).Index]);
      cbxInspector.Items.AddObject(S, CI);
    end;
  end
  else
  begin
    S := Format('%s', [AComponent.ClassName]);
    cbxInspector.Items.AddObject(S, AComponent);
  end;
end;

procedure TfrmComponentInspectorzObjectInspector.FocusComponentInInspector(
  AComponent: TObject);
begin
  cbxInspector.ItemIndex := cbxInspector.Items.IndexOfObject(AComponent);
  cbxInspectorChange(cbxInspector);
end;
{$ENDREGION}
end.
