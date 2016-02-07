{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.DSharp.TreeViewPresenter.Tree.Form;

{ Form demonstrating the usage of the DSharp TTreeViewPresenter which simplifies
  the process of representing data in a TVirtualStringTree control. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Samples.Spin, Vcl.ExtCtrls, Vcl.ActnList,

  VirtualTrees,

  zObjInspector,

  DSharp.Windows.ColumnDefinitions, DSharp.Windows.TreeViewPresenter,
  DSharp.Bindings, DSharp.Windows.CustomPresenter,

  Spring.Collections, Spring.Reflection,

  DDuce.Components.PropertyInspector, DDuce.Components.GridView,

  Concepts.RTTEye.Data, Concepts.Resources;

type
  TfrmTreeViewPresenterTree = class(TForm)
    pnlTop               : TPanel;
    pnlBottom            : TPanel;
    pnlLeft              : TPanel;
    aclMain              : TActionList;
    splVertical          : TSplitter;
    pnlTreeviewPresenter : TPanel;
    pnlLeftTop           : TPanel;
    pnlLeftBottom        : TPanel;
    splHorizontal        : TSplitter;
    pnlTreeView          : TPanel;
    edtFilter            : TLabeledEdit;
    actExecute           : TAction;
    btnExecute           : TButton;
    mmoDetails           : TMemo;
    pnlType              : TPanel;

    procedure actExecuteExecute(Sender: TObject);

  private
    FOI         : TzObjectInspector;
    FVST        : TVirtualStringTree;
    FTVP        : TTreeViewPresenter;
    FData       : TReflectionData;
    FObjectList : IObjectList;

    procedure FTVPSelectionChanged(Sender: TObject);

  public
    procedure AfterConstruction; override;

    procedure CreateRttiTreeview;

    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.Rtti,

  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  Spring.Collections.Adapters, Spring.Collections.Enumerable,

  DDuce.RandomData, DDuce.Components.Factories, DDuce.Reflect,

  Concepts.RTTEye.RttiTemplates, Concepts.Factories, Concepts.RTTEye.Templates,
  Concepts.Utils;

{$REGION 'construction and destruction'}
procedure TfrmTreeViewPresenterTree.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST  := TConceptFactories.CreateVirtualStringTree(Self, pnlTreeView);
  CreateRttiTreeview;
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
  FTVP.UseColumnDefinitions := True;
  FTVP.TreeView := FVST;
  FOI   := TConceptFactories.CreatezObjectInspector(Self, pnlLeftTop, FTVP);
end;

procedure TfrmTreeViewPresenterTree.BeforeDestruction;
begin
  FObjectList := nil;
  inherited BeforeDestruction;
end;

procedure TfrmTreeViewPresenterTree.CreateRttiTreeview;
begin
  FTVP := TTreeViewPresenter.Create(Self);
  FObjectList := TCollections.CreateObjectList<TObject>(True) as IObjectList;
  FTVP.View.ItemsSource := FObjectList;
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
  FTVP.View.ItemTemplate := TRttiTypeTemplate.Create;
end;
{$ENDREGION}

{$REGION 'action handlers'}

{ REMARK: When wrapping the body of thiss method in a HourGlass call this
  seems to introduce a memory leak.
}
procedure TfrmTreeViewPresenterTree.actExecuteExecute(Sender: TObject);
var
  ET : Enumerable<TRttiType>;
begin
  Screen.Cursor := crHourGlass;
  FTVP.BeginUpdate;
  try
    FObjectList.Clear;
    ET := TType.Types.Where( function(const AArg: TRttiType): Boolean
      begin
        Result := AArg.QualifiedName.Contains(edtFilter.Text);
      end
    );
    FObjectList.AddRange(ET.ToList as IObjectList);
  finally
    Screen.Cursor := crDefault;
    FTVP.EndUpdate;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmTreeViewPresenterTree.FTVPSelectionChanged(Sender: TObject);
begin
  if Assigned(FTVP.SelectedItem) then
  begin
    pnlType.Caption := FTVP.SelectedItem.ClassName;
    mmoDetails.Lines.Text := Reflect.Properties(FTVP.SelectedItem).ToString;
  end;
end;

{$ENDREGION}

end.


