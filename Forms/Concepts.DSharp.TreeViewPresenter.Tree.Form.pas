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

  DSharp.Windows.ColumnDefinitions, DSharp.Windows.TreeViewPresenter,
  DSharp.Bindings, DSharp.Windows.CustomPresenter,

  Spring.Collections,

  DDuce.Components.PropertyInspector, DDuce.Components.GridView,

  Concepts.RTTEye.Data,

  Concepts.Types.Contact;

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

  private
    FList       : IList<TContact>;
    FPI         : TPropertyInspector;
    FVST        : TVirtualStringTree;
    FTVP        : TTreeViewPresenter;
    FVSTColumns : TVirtualStringTree;
    FTVPColumns : TTreeViewPresenter;
    FObjectList : IList<TReflectionData>;

    procedure FTVPColumnsSelectionChanged(Sender: TObject);
    procedure FTVPSelectionChanged(Sender: TObject);

  public
    procedure AfterConstruction; override;

    procedure CreateColumnDefinitionsView;

  end;

implementation

{$R *.dfm}

uses
  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  DDuce.RandomData, DDuce.Components.Factories,

  Concepts.Factories, Concepts.ComponentInspector,
  Concepts.RTTEye.Templates;

{$REGION 'construction and destruction'}
procedure TfrmTreeViewPresenterTree.AfterConstruction;
var
  C : TColumnDefinition;
  I : Integer;
begin
  inherited AfterConstruction;

  //FList := TConceptFactories.CreateContactList(10000);
  FVST  := TConceptFactories.CreateVirtualStringTree(Self, pnlTop);
  //FTVP  := TConceptFactories.CreateTreeViewPresenter(Self, FVST, FList as IObjectList);

  FObjectList := TCollections.CreateObjectList<TReflectionData>;
  FObjectList.Add(TReflectionData.Create);
   FTVP := TTreeViewPresenter.Create(Self);

   with FTVP.ColumnDefinitions.Add('Name') do
  begin
    ValuePropertyName := 'Name';
    Alignment         := taLeftJustify;
    //OnCustomDraw      := FTVPColumnDefinitionsCustomDrawColumn;
    AutoSize          := True;
  end;

  //FTVP  := TConceptFactories.CreateTreeViewPresenter(Self, FVST, FObjectList as IObjectList);
  //FTVP.View.ItemTemplate := TReflectionTemplate.Create(FTVP.ColumnDefinitions);
  FTVP.View.ItemTemplate := TReflectionTemplate.Create(FTVP.ColumnDefinitions);
  //FTVP.View.ItemsSource :=

   FTVP.UseColumnDefinitions := True;
   FTVP.View.ItemsSource := FObjectList as IObjectList;
   FTVP.View.ItemTemplate := TReflectionTemplate.Create(FTVP.ColumnDefinitions);
   FTVP.TreeView := FVST;






//  APresenter.UseColumnDefinitions := True;
//  APresenter.View.ItemsSource     := ASource;
//  if not Assigned(ATemplate) then
//    APresenter.View.ItemTemplate :=
//      TColumnDefinitionsControlTemplate.Create(APresenter.ColumnDefinitions)
//  else
//    APresenter.View.ItemTemplate := ATemplate;
//  if Assigned(AFilter) then
//    APresenter.View.Filter.Add(AFilter);



  //
  FPI   := TDDuceComponents.CreatePropertyInspector(Self, pnlLeftTop, FTVP);

  //FTVP.View.ItemTemplate := TColumnDefinitionsControlTemplate.Create(FTVP.ColumnDefinitions);
  //FTVP.OnSelectionChanged := FTVPSelectionChanged;

  CreateColumnDefinitionsView;
//  InspectComponent(FTVP);
end;
{$ENDREGION}

procedure TfrmTreeViewPresenterTree.CreateColumnDefinitionsView;
var
  CDList : IList<TColumnDefinition>;
  C      : TColumnDefinition;
  I      : Integer;
begin
  CDList := TCollections.CreateObjectList<TColumnDefinition>;
  for I := 0 to FTVP.ColumnDefinitions.Count - 1 do
  begin
    C := FTVP.ColumnDefinitions[I];
    CDList.Add(C);
  end;
  FVSTColumns := TConceptFactories.CreateVirtualStringTree(Self, pnlLeftBottom);
  FTVPColumns := TConceptFactories.CreateTreeViewPresenter(Self, FVSTColumns, CDList as IObjectList);
  FTVPColumns.SelectionMode := smSingle;
  FTVPColumns.OnSelectionChanged := FTVPColumnsSelectionChanged;
end;

procedure TfrmTreeViewPresenterTree.FTVPColumnsSelectionChanged(Sender: TObject);
begin
  if Assigned(FTVPColumns.SelectedItem) then
    FPI.Objects[0] := FTVPColumns.SelectedItem;
end;

procedure TfrmTreeViewPresenterTree.FTVPSelectionChanged(Sender: TObject);
begin
  FPI.Objects[0] := FTVP;
end;

end.

(*
type
  TFolderTemplate = class(TDataTemplate)
  public
    function GetItem(const Item: TObject; const Index: Integer): TObject; override;
    function GetItemCount(const Item: TObject): Integer; override;
    function GetText(const Item: TObject; const ColumnIndex: Integer): string; override;
    function GetTemplateDataClass: TClass; override;
  end;

  TFileTemplate = class(TDataTemplate)
  public
    function GetText(const Item: TObject; const ColumnIndex: Integer): string; override;
    function GetTemplateDataClass: TClass; override;
  end;

implementation

{ TFolderTemplate }

function TFolderTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  Result := TFolder(Item).Files[Index];
end;

function TFolderTemplate.GetItemCount(const Item: TObject): Integer;
begin
  Result := TFolder(Item).Files.Count; // containing subfolders in that list as well
end;

function TFolderTemplate.GetTemplateDataClass: TClass;
begin
  Result := TFolder;
end;

function TFolderTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  case ColumnIndex of
    0: Result := TFolder(Item).Name;
  end;
end;

{ TFileTemplate }

function TFileTemplate.GetTemplateDataClass: TClass;
begin
  Result := TFile;
end;

function TFileTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  case ColumnIndex of
    0: Result := TFile(Item).Name;
    1: Result := DateTimeToStr(TFile(Item).ChangeDate);
    2: Result := IntToStr(TFile(Item).Size);
  end;
end;

*)
