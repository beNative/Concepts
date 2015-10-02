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

unit Concepts.DSharp.VSTPresenter.Form;

{ Demonstrates the use of presenters to seperate UI from model data. }

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms,

  VirtualTrees,

  DDuce.Components.PropertyInspector,

  Spring.Collections,

  DSharp.Core.DataTemplates,
  DSharp.Windows.ColumnDefinitions, DSharp.Windows.TreeViewPresenter,
  DSharp.Bindings, DSharp.Windows.CustomPresenter;

type
  TfrmVSTPresenter = class(TForm)
    pnlVST: TPanel;

  private
    FIns      : TPropertyInspector;
//    FVST      : TVirtualStringTree;
//    FTVP      : TTreeViewPresenter;
//    FTemplate : IDataTemplate;
//    FList     : IList;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DSharp.Collections.ObservableCollection,
  DSharp.Windows.ColumnDefinitions.RttiDataTemplate,

  Concepts.Types.Contact,

  Concepts.Factories;

type
  TContactTemplate = class(TDataTemplate<TContact>)
//    function GetValue(const Item: TContact;
//      const ColumnIndex: Integer);
  end;

procedure TfrmVSTPresenter.AfterConstruction;
begin
  inherited;
  FIns := TConceptFactories.CreateInspector(Self, pnlVST, Self);
  //FVST := CreateVST(Self, pnlVST);
//  FList := TObservableCollection<TObject>.Create(True);
//  FillListWithContacts(FList, 10);
//  FTVP := CreateTVP(Self, FVST, FList);
//  FTVP.BeginUpdate;
//  with FTVP.ColumnDefinitions.Add('FirstName') do
//    ValuePropertyName := 'FirstName';
//
//  with FTVP.ColumnDefinitions.Add('LastName') do
//    ValuePropertyName := 'LastName';
//  FTVP.EndUpdate;

  //FTVP.View.

//  FTemplate := TRttiDataTemplate.Create(FTVP.ColumnDefinitions);
//  FTVP.View.ItemTemplate := FTemplate;
//  FTVP := TTreeViewPresenter.Create(Self);
//  FTVP.TreeView := FVST;
//  FTVP.View.ItemsSource :=
//  FTVP.View.ItemTemplate :=
end;

end.
