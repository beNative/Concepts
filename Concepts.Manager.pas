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

unit Concepts.Manager;

{ Manages a list of concepts. A concept is nothing more than a standard form
  that demomstrates a user defined feature. }

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Graphics,

  Spring.Collections, Spring.Collections.Lists;

type
  TConcept = class(TPersistent)
  private
    FName        : string;
    FFormClass   : TComponentClass;
    FDescription : string;
    FCategory    : string;
    FColor       : TColor;

    function GetSourceFilename: string;

  published
    property Name: string
      read FName write FName;

    property Description: string
      read FDescription write FDescription;

    property Category: string
      read FCategory write FCategory;

    property Color: TColor
      read FColor write FColor;

    property FormClass: TComponentClass
      read FFormClass write FFormClass;

    property SourceFilename: string
      read GetSourceFilename;
  end;

type
  TConceptManager = record
    class var
      FList: IList<TConcept>;

    class constructor Create;

    class function Execute(
      AConcept   : TObject;
      AShowModal : Boolean = True
    ): Boolean; overload; static;

    class function Execute(
      const AName: string
    ): Boolean; overload; static;

    class function Register(
      AFormClass         : TComponentClass;
      const AName        : string = '';
      const ACategory    : string = '';
      const ADescription : string = '';
      AColor             : TColor = clWhite
    ): Boolean; static;

    class property ItemList: IList<TConcept>
      read FList;

  end;

  ConceptManager = TConceptManager;

implementation

uses
  System.SysUtils, System.StrUtils;

{$REGION 'construction and destruction'}
class constructor TConceptManager.Create;
begin
  FList := TCollections.CreateObjectList<TConcept>;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TConcept.GetSourceFilename: string;
begin
  Result := FFormClass.UnitName;
end;
{$ENDREGION}

{$REGION 'public methods'}
class function TConceptManager.Execute(AConcept: TObject; AShowModal: Boolean)
  : Boolean;
var
  F : TComponent;
  C : TConcept;
begin
  if Assigned(AConcept) then
  begin
    C := AConcept as TConcept;
    F := C.FormClass.Create(Application);
    if F is TForm then
    begin
      with TForm(F) do
      begin
        Caption := C.Name;
        Position := TPosition.poScreenCenter;
        if AShowModal then
          ShowModal
        else
          Show;
      end
    end
    else
      raise Exception.CreateFmt('Cannot create %s', [C.FormClass.ClassName]);
    Result := True;
  end
  else
    Result := False;
end;

class function TConceptManager.Execute(const AName: string): Boolean;
begin
  Result := Execute(
    FList.Where(function(const AItem: TConcept): Boolean
      begin
        Result := SameText(AItem.Name, AName);
      end
    ).FirstOrDefault
  );
end;

class function TConceptManager.Register(AFormClass: TComponentClass;
  const AName, ACategory, ADescription: string; AColor: TColor): Boolean;
var
  S : string;
  C : TConcept;
begin
  S := IfThen(AName = '', AFormClass.ClassName, AName);
  C             := TConcept.Create;
  C.Name        := S;
  C.Category    := ACategory;
  C.Description := ADescription;
  C.FormClass   := AFormClass;
  C.Color       := AColor;
  FList.Add(C);
  Result := True;
end;
{$ENDREGION}

end.



