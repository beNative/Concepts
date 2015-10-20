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

  VirtualTrees,

  DSharp.Windows.ColumnDefinitions, DSharp.Windows.TreeViewPresenter,
  DSharp.Bindings, DSharp.Windows.CustomPresenter,

  Spring.Collections,

  Concepts.Types.Contact, Vcl.Samples.Spin;

type
  TfrmTreeViewPresenter = class(TForm)
    grdMain       : TVirtualStringTree;
    grdMainDetail : TVirtualStringTree;
    btnFilter     : TButton;
    edtFilter     : TEdit;
    btnEvent      : TButton;
    lblChange     : TLabel;
    edtName       : TEdit;
    edtIndex      : TSpinEdit;

    procedure tvpMainSelectionChanged(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure btnEventClick(Sender: TObject);

  private
    FList      : IList<TContact>;
    //FList      : IObjectList;
    //FList      : IList;
    FSelection : IList;
    FTVP       : TTreeViewPresenter;

  public
    procedure AfterConstruction; override;

    procedure FillList(AList: IList<TContact>; ACount: Integer);

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
  FList := TCollections.CreateObjectList<TContact>;
  FillList(FList, 1000);
  FTVP  := TConceptFactories.CreateTVP(Self, grdMain, FList as IObjectList);

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
  grdMainDetail.BeginUpdate;
//  FSelection.Clear;
//  FSelection.AddRange(tvpMain.SelectedItems);
  grdMainDetail.EndUpdate;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmTreeViewPresenter.FillList(AList: IList<TContact>;
  ACount: Integer);
var
  C: TContact;
  I: Integer;
begin
  if Assigned(AList) then
  begin
    AList.Clear;
    for I := 0 to ACount - 1 do
    begin
      C := TContact.Create;
      with C do
      begin
        Firstname   := RandomData.FirstName(gnMale);
        Lastname    := RandomData.LastName;
        CompanyName := RandomData.CompanyName;
        Email       := RandomData.Email(Firstname, Lastname);
        Address     := RandomData.Address;
        Number      := RandomData.Number(100);
      end;
      AList.Add(C);
    end;
  end;
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
