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

unit Concepts.RTTEye.RttiTemplates;

interface

uses
  System.Rtti,

  DSharp.Windows.ColumnDefinitions.ControlTemplate,
  DSharp.Core.DataTemplates,

  Spring.Collections;

type
  TRttiTypeTemplate = class(TDataTemplate<TRttiType>)
    function GetItemCount(const Item: TRttiType): Integer; override;
    function GetItems(const Item: TRttiType): IObjectList; override;
    function GetItem(const Item: TRttiType; const Index: Integer): TObject; override;
    function GetValue(const Item: TRttiType;
      const ColumnIndex: Integer): TValue; override;
   procedure AfterConstruction; override;
  end;

  TRttiMemberTemplate = class(TDataTemplate<TRttiMember>)
    function GetValue(const Item: TRttiMember;
      const ColumnIndex: Integer): TValue; override;
    function GetItemCount(const Item: TRttiMember): Integer; override;
    function GetItems(const Item: TRttiMember): IObjectList; override;
    function GetItem(const Item: TRttiMember; const Index: Integer): TObject; override;

  public
    procedure AfterConstruction; override;
  end;

  TRttiParameterTemplate = class(TDataTemplate<TRttiParameter>)
    function GetValue(const Item: TRttiParameter;
      const ColumnIndex: Integer): TValue; override;
  end;

implementation

uses
  Spring.Reflection, Spring.Collections.Enumerable;

{$REGION 'TRttiTypeTemplate'}
procedure TRttiTypeTemplate.AfterConstruction;
begin
  RegisterDataTemplate(TRttiMemberTemplate.Create);
  inherited AfterConstruction;
end;

function TRttiTypeTemplate.GetItem(const Item: TRttiType;
  const Index: Integer): TObject;
begin
  if Item is TRttiInstanceType then
    Result := GetItems(Item)[Index]
  else
  begin
    Result := inherited GetItem(Item, Index);
  end;
end;

function TRttiTypeTemplate.GetItemCount(const Item: TRttiType): Integer;
begin
  if Item is TRttiInstanceType then
    Result := TRttiInstanceType(Item).Fields.Count
      + TRttiInstanceType(Item).Properties.Count
      + TRttiInstanceType(Item).Methods.Count
  else
    Result := inherited GetItemCount(Item);
end;

function TRttiTypeTemplate.GetItems(const Item: TRttiType): IObjectList;
var
  EF : Enumerable<TRttiField>;
  EP : Enumerable<TRttiProperty>;
  EM : Enumerable<TRttiMethod>;
  O  : IObjectList;
begin
  if Item is TRttiInstanceType then
  begin
    EF := TRttiInstanceType(Item).Fields;
    O := EF.ToList as IObjectList;
    EP := TRttiInstanceType(Item).Properties;
    O.AddRange((EP.ToList as IObjectList).ToArray);
    EM := TRttiInstanceType(Item).Methods;
    O.AddRange((EM.ToList as IObjectList).ToArray);
    //O := O.Concat(EF.ToList as IObjectList) as IObjectList;
    Result := O;
  end

  else
    Result := inherited GetItems(Item);
end;

function TRttiTypeTemplate.GetValue(const Item: TRttiType;
  const ColumnIndex: Integer): TValue;
begin
  Result := Item.ToString;
end;
{$ENDREGION}

{$REGION 'TRttiMemberTemplate'}
procedure TRttiMemberTemplate.AfterConstruction;
begin
  inherited;
  RegisterDataTemplate(TRttiParameterTemplate.Create);
end;

function TRttiMemberTemplate.GetItem(const Item: TRttiMember;
  const Index: Integer): TObject;
begin
  if Item is TRttiMethod then
    Result := TRttiMethod(Item).Parameters.ElementAt(Index)
  else
  begin
    Result := inherited GetItem(Item, Index);
  end;

end;

function TRttiMemberTemplate.GetItemCount(const Item: TRttiMember): Integer;
begin
  if Item is TRttiMethod then
    Result := TRttiMethod(Item).Parameters.Count
  else
  begin
    Result := inherited GetItemCount(Item);
  end;
end;

function TRttiMemberTemplate.GetItems(const Item: TRttiMember): IObjectList;
var
  EP : Enumerable<TRttiParameter>;
begin
  if Item is TRttiMethod then
  begin
    EP := TRttiMethod(Item).Parameters;
    Result :=  EP.ToList as IObjectList;
  end
  else
  begin
    Result := inherited GetItems(Item);
  end;
end;

function TRttiMemberTemplate.GetValue(const Item: TRttiMember;
  const ColumnIndex: Integer): TValue;
begin
  Result := Item.ToString;
end;
{$ENDREGION}

{$REGION 'TRttiParameterTemplate'}
function TRttiParameterTemplate.GetValue(const Item: TRttiParameter;
  const ColumnIndex: Integer): TValue;
begin
  Result := Item.ToString;
end;
{$ENDREGION}

end.
