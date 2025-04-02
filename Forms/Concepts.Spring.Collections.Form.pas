{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.Spring.Collections.Form;

{ Demonstrates some of the features of Spring collections. }

{
  References:
    - Coding in Delphi - Nick Hodges
    - Generics and variance
        (http://delphisorcery.blogspot.be/search?q=Generics+and+variance)
    - Reimplementing LINQ to Objects
        (http://edulinq.googlecode.com/hg/posts/index.html)

  covariance <-> contravariance
  variance   <-> invariance
}

interface

uses
  System.Actions, System.Classes, System.SysUtils, System.Bindings.Outputs,
  System.Rtti,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls,
  Vcl.Bind.DBEngExt, Vcl.Bind.Editors, Vcl.ExtCtrls,
  Data.Bind.EngExt, Data.Bind.Components,

  Spring, Spring.Collections,

  Concepts.Types.Contact;

type
  TfrmCollections = class(TForm)
    {$REGION 'designer controls'}
    aclMain         : TActionList;
    actBoth         : TAction;
    actEnumerate    : TAction;
    actFirstNameIs  : TAction;
    actLastNameIs   : TAction;
    actPopulateList : TAction;
    BindExpression1 : TBindExpression;
    btnBoth         : TButton;
    btnCreateList   : TButton;
    btnFirstNameIs  : TButton;
    btnLastNameIs   : TButton;
    edtFirstName    : TEdit;
    edtLastName     : TEdit;
    edtRecordCount  : TEdit;
    lblHeader       : TLabel;
    lblRecordCount  : TLabel;
    lstBindings     : TBindingsList;
    mmoList         : TMemo;
    pnlHeader       : TPanel;
    sbrMain         : TStatusBar;
    trbRecordCount  : TTrackBar;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actPopulateListExecute(Sender: TObject);
    procedure actFirstNameIsExecute(Sender: TObject);
    procedure actLastNameIsExecute(Sender: TObject);
    procedure actBothExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure trbRecordCountChange(Sender: TObject);
    procedure lblRecordCountClick(Sender: TObject);
    {$ENDREGION}

  private
    FList        : IList<TContact>;
    FFirstNameIs : TPredicate<TContact>;
    FLastNameIs  : TPredicate<TContact>;
    FDict        : IDictionary<string,Int64>;

    procedure PopulateList;
    procedure DefinePredicates;

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Vcl.Dialogs,

  DDuce.Utils,

  Concepts.Factories;

resourcestring
  SFindContactsWithFirstName = 'Find all contacts with first name = %s';
  SFindContactsWithLastName  = 'Find all contacts with last name = %s';
  SListPopulated             = 'Contactlist has been populated.';

{$REGION 'construction and destruction'}
procedure TfrmCollections.AfterConstruction;
begin
  inherited AfterConstruction;
  // Collection classes in Spring4D should always be accessed through interface
  // references of the generic type.
  // Use the static methods of the TCollections class to create the collection
  // object of your choice and assign it to the corresponding interface variable.
  // IList<TContact>
  // If you are just using lists where T is a class then use the IObjectList
  // interface (which is in fact a IList<TObject>). Otherwise use the AsList
  // method to return an IList which wraps the original generic list and uses
  // TValue on its API.
  FList := TCollections.CreateObjectList<TContact>;

  FDict := TCollections.CreateDictionary<string, Int64>;


end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmCollections.actPopulateListExecute(Sender: TObject);
begin
  HourGlass(PopulateList);
  ShowMessage(SListPopulated);
end;

procedure TfrmCollections.actFirstNameIsExecute(Sender: TObject);
begin
  HourGlass(procedure
    var
      SL : IShared<TStringList>;
      C  : TContact;
    begin
      SL := Shared<TStringList>.Make;
      DefinePredicates;
      for C in FList.Where(FFirstNameIs) do
      begin
        SL.Add(C.Firstname + ' ' + C.Lastname + ' ' + C.Address);
      end;
      mmoList.Lines.Assign(SL);
    end
  );
end;

procedure TfrmCollections.actLastNameIsExecute(Sender: TObject);
begin
  HourGlass(procedure
    var
      SL : IShared<TStringList>;
      C  : TContact;
    begin
      SL := Shared<TStringList>.Make;
      DefinePredicates;
      for C in FList.Where(FLastNameIs) do
      begin
        SL.Add(C.Firstname + ' ' + C.Lastname + ' ' + C.Address);
      end;
      mmoList.Lines.Assign(SL);
    end
  );
end;

procedure TfrmCollections.actBothExecute(Sender: TObject);
begin
  HourGlass(procedure
    var
      SL : IShared<TStringList>;
      C  : TContact;
    begin
      SL := Shared<TStringList>.Make;
      DefinePredicates;
      for C in FList.Where(FLastNameIs).Where(FFirstNameIs) do
      begin
        SL.Add(C.Firstname + ' ' + C.Lastname + ' ' + C.Address);
      end;
      mmoList.Lines.Assign(SL);
    end
  );
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmCollections.PopulateList;
begin
  FList.Clear;
  TConceptFactories.FillListWithContacts(
    FList as IObjectList,
    trbRecordCount.Position
  );
end;

procedure TfrmCollections.trbRecordCountChange(Sender: TObject);
begin
  lstBindings.Notify(Sender, '');
end;

procedure TfrmCollections.DefinePredicates;
begin
  FFirstNameIs := function(const AC: TContact): Boolean
  begin
    Result := AC.Firstname = edtFirstName.Text;
  end;

  FLastNameIs := function(const AC: TContact): Boolean
  begin
    Result := AC.Lastname = edtLastName.Text;
  end;
end;

procedure TfrmCollections.lblRecordCountClick(Sender: TObject);
begin
  FDict['Test'] := 5;
  FDict['Test'] := 6;

  if FDict.ContainsKey('Test') then
    ShowMessage(FDict.Count.ToString);
end;

{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmCollections.UpdateActions;
begin
  actFirstNameIs.Enabled := not FList.IsEmpty;
  actLastNameIs.Enabled  := not FList.IsEmpty;
  actBoth.Enabled        := not FList.IsEmpty;
  actFirstNameIs.Caption := Format(SFindContactsWithFirstName, [edtFirstName.Text]);
  actLastNameIs.Caption  := Format(SFindContactsWithLastName, [edtLastName.Text]);
  inherited UpdateActions;
end;
{$ENDREGION}

end.
