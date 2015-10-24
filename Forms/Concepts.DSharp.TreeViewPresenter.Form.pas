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

unit Concepts.DSharp.TreeViewPresenter.Form;

{ Form demonstrating the usage of the DSharp TTreeViewPresenter which simplifies
  the process of representing data in a TVirtualStringTree control. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Samples.Spin, Vcl.ExtCtrls,

  VirtualTrees,

  DSharp.Windows.ColumnDefinitions, DSharp.Windows.TreeViewPresenter,
  DSharp.Bindings, DSharp.Windows.CustomPresenter,

  Spring.Collections,

  DDuce.Components.PropertyInspector,

  Concepts.Types.Contact;

type
  TfrmTreeViewPresenter = class(TForm)
    pnlTop    : TPanel;
    pnlBottom : TPanel;
    edtFilter : TEdit;
    btnFilter : TButton;
    lblChange : TLabel;
    edtIndex  : TSpinEdit;
    edtName   : TEdit;
    btnEvent  : TButton;
    spl1      : TSplitter;
    pnlLeft   : TPanel;

    procedure tvpMainSelectionChanged(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure btnEventClick(Sender: TObject);

  private
    FList      : IList<TContact>;
    FPI        : TPropertyInspector;
    FSelection : IList;
    FVST       : TVirtualStringTree;
    FTVP       : TTreeViewPresenter;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DSharp.Windows.ColumnDefinitions.RttiDataTemplate,

  Concepts.Factories,

  DDuce.RandomData;

{$REGION 'construction and destruction'}
procedure TfrmTreeViewPresenter.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TConceptFactories.CreateContactList(10000);
  FVST  := TConceptFactories.CreateVirtualStringTree(Self, pnlTop);
  FTVP  := TConceptFactories.CreateTreeViewPresenter(Self, FVST, FList as IObjectList);
  FPI   := TConceptFactories.CreatePropertyInspector(Self, pnlLeft, FTVP);

//  with FTVP.ColumnDefinitions.Add('Firstname') do
//  begin
//    ValuePropertyName := 'Firstname';
//    AutoSize          := True;
//    Alignment         := taCenter;
//  end;
//  with FTVP.ColumnDefinitions.Add('Lastname') do
//  begin
//    ValuePropertyName := 'Lastname';
//    AutoSize          := True;
//    Alignment         := taLeftJustify;
//  end;
//  with FTVP.ColumnDefinitions.Add('Email') do
//  begin
//    ValuePropertyName := 'Email';
//    AutoSize          := True;
//  end;
//  with FTVP.ColumnDefinitions.Add('CompanyName') do
//  begin
//    ValuePropertyName := 'CompanyName';
//    AutoSize          := True;
//  end;
  //FTVP.UseColumnDefinitions := True;
  //FTVP.View.ItemsSource := FList as IObjectList;
  //FTVP.TreeView := grdMain;
  //FTVP.View.ItemTemplate := TRttiDataTemplate.Create(FTVP.ColumnDefinitions);
   //FList3 := TObjectList<TObject>.Create(False);
   //FSelection := TObservableCollection<TObject>.Create(False);

  //tvpMainDetail.View.ItemsSource := FSelection;
  //grdMain.Header.AutoFitColumns(False);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmTreeViewPresenter.btnEventClick(Sender: TObject);
begin
  (FList[edtIndex.Value] as TContact).Firstname := edtName.Text;
end;

procedure TfrmTreeViewPresenter.btnFilterClick(Sender: TObject);
begin
 //tvpMain.ColumnDefinitions[0].CustomFilter := edtFilter.Text;
end;

procedure TfrmTreeViewPresenter.tvpMainSelectionChanged(Sender: TObject);
begin
  //grdMainDetail.BeginUpdate;
//  FSelection.Clear;
//  FSelection.AddRange(tvpMain.SelectedItems);
  //grdMainDetail.EndUpdate;
end;
{$ENDREGION}

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
