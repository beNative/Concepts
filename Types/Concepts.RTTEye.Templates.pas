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

{ NOT USED!}

unit Concepts.RTTEye.Templates;

interface

uses
  DSharp.Windows.ColumnDefinitions.ControlTemplate,
  DSharp.Core.DataTemplates,

  Spring.Collections;

{
  Application name
    +- Unit name
           +- Type name
                 Type kind
                   +- tkUnknown,
                      tkInteger, tkChar, tkFloat,tkString, tkWChar, tkLString,
                        tkWString, tkVariant, tkInt64, tkUString,

                      tkEnumeration,
                      tkSet,
                      tkPointer,
                      tkClassRef,

                      tkProcedure
                      tkMethod,

                      tkArray,
                      tkDynArray,

                      tkClass,
                      tkInterface,
                      tkRecord,

}

type
//  TReflectionTemplate = class(TColumnDefinitionsControlTemplate, IDataTemplate)
//    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
//    function GetTemplateDataClass: TClass; override;
//
//    function GetItemCount(const Item: TObject): Integer; override;
//    function GetItems(const Item: TObject): IObjectList; override;
//    function GetItem(const Item: TObject; const Index: Integer): TObject; override;
//
//    procedure AfterConstruction; override;
//  end;

  TTypeTemplate = class(TColumnDefinitionsControlTemplate, IDataTemplate)
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;
    function GetItemCount(const Item: TObject): Integer; override;
    function GetItems(const Item: TObject): IObjectList; override;
    function GetItem(const Item: TObject; const Index: Integer): TObject; override;

    procedure AfterConstruction; override;
  end;

  TMemberTemplate = class(TColumnDefinitionsControlTemplate, IDataTemplate)
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

    function GetItemCount(const Item: TObject): Integer; override;
    function GetItems(const Item: TObject): IObjectList; override;
    function GetItem(const Item: TObject; const Index: Integer): TObject; override;

  end;

  TParameterTemplate = class(TColumnDefinitionsControlTemplate, IDataTemplate)
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;
  end;

implementation

uses
  System.Rtti, System.SysUtils,

  DDuce.Logger,

  Concepts.RttEye.Data;

{$REGION 'TReflectionTemplate'}
//procedure TReflectionTemplate.AfterConstruction;
//begin
//  RegisterDataTemplate(TTypeTemplate.Create(FColumnDefinitions));
//  RegisterDataTemplate(TParameterTemplate.Create(FColumnDefinitions));
//  inherited AfterConstruction;
//end;
//
//function TReflectionTemplate.GetItem(const Item: TObject;
//  const Index: Integer): TObject;
//begin
//  if Item is TReflectionData then
//    Result := TReflectionData(Item).Types[Index]
//  else
//    Result := inherited GetItem(Item, Index);
//end;
//
//function TReflectionTemplate.GetItemCount(const Item: TObject): Integer;
//begin
//  if Item is TReflectionData then
//    //Result := TReflectionData(Item).Types.Count
//    Result := TReflectionData(Item).Types.Count
//  else
//    Result := 1;
//    //inherited GetItemCount(Item);
//end;
//
//function TReflectionTemplate.GetItems(const Item: TObject): IObjectList;
//begin
//  if Item is TReflectionData then
//    Result := TReflectionData(Item).Types
//  else
//    Result := inherited GetItems(Item);
//end;
//
//function TReflectionTemplate.GetItemTemplate(
//  const Item: TObject): IDataTemplate;
//begin
//  if Item is TReflectionData then
//    Result := Self
//  else
//    Result := inherited GetItemTemplate(Item);
//end;
//
//function TReflectionTemplate.GetTemplateDataClass: TClass;
//begin
//  Result := TReflectionData;
//end;
{$ENDREGION}

{$REGION 'TTypeTemplate'}
procedure TTypeTemplate.AfterConstruction;
begin
  RegisterDataTemplate(TMemberTemplate.Create(FColumnDefinitions));
  inherited AfterConstruction;
end;

function TTypeTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  if Item is TTypeData then
    Result := TTypeData(Item).Members[Index]
  else
    Result := inherited GetItem(Item, Index);
end;

function TTypeTemplate.GetItemCount(const Item: TObject): Integer;
begin
  if Item is TTypeData then
    Result := TTypeData(Item).Members.Count
  else
    Result := inherited GetItemCount(Item);
end;

function TTypeTemplate.GetItems(const Item: TObject): IObjectList;
begin
  if Item is TTypeData then
     Result := TTypeData(Item).Members
   else
     Result := inherited GetItems(Item);
end;

function TTypeTemplate.GetItemTemplate(const Item: TObject): IDataTemplate;
begin
  if Item is TTypeData then
    Result := Self
  else
  begin
    Result := inherited GetItemTemplate(Item);
  end;
end;

function TTypeTemplate.GetTemplateDataClass: TClass;
begin
  Result := TTypeData;
end;
{$ENDREGION}

{$REGION 'TMemberTemplate'}
function TMemberTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  if Item is TMemberData then
    Result := TMemberData(Item).Parameters[Index]
  else
    Result := inherited GetItem(Item, Index);
end;

function TMemberTemplate.GetItemCount(const Item: TObject): Integer;
begin
  if Item is TMemberData then
    Result := TMemberData(Item).Parameters.Count
  else
  begin
    Result := inherited GetItemCount(Item);
  end;
end;

function TMemberTemplate.GetItems(const Item: TObject): IObjectList;
begin
  if Item is TMethodData then
    Result := TMethodData(Item).Parameters
  else
    Result := inherited GetItems(Item);
end;

function TMemberTemplate.GetItemTemplate(const Item: TObject): IDataTemplate;
begin
  if (Item is TMemberData) and not (Item is TMethodData) then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TMemberTemplate.GetTemplateDataClass: TClass;
begin
  Result := TMemberData;
end;
{$ENDREGION}

{ TParameterTemplate }

function TParameterTemplate.GetItemTemplate(const Item: TObject): IDataTemplate;
begin
  if Item is TParameter then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TParameterTemplate.GetTemplateDataClass: TClass;
begin
  Result := TParameter;
end;

end.
