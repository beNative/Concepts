{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.Search.Templates;

{ Data templates used to display the search results hierarchically in a
  TVirtualStringTree using a TTreeViewPresenter. }

interface

uses
  System.Classes, System.SysUtils, System.Contnrs, System.Rtti,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Windows.ControlTemplates,
  DSharp.Core.DataTemplates, DSharp.Windows.ColumnDefinitions.ControlTemplate,

  DDuce.Editor.Search.Data;

type
  TSearchResultGroupTemplate = class(TControlTemplate<TSearchResultGroup>)
  public
    function GetItemCount(const Item: TSearchResultGroup): Integer; override;
    function GetItems(const Item: TSearchResultGroup): IObjectList; override;
    function GetValue(
      const Item        : TSearchResultGroup;
      const ColumnIndex : Integer
    ) : TValue; override;
    procedure AfterConstruction; override;
  end;

  { TSearchResultLineTemplate }

  TSearchResultLineTemplate = class(TControlTemplate<TSearchResultLine>)
  public
    function GetItemCount(const Item: TSearchResultLine): Integer; override;
    function GetItems(const Item: TSearchResultLine): IObjectList; override;
    function GetValue(
      const Item        : TSearchResultLine;
      const ColumnIndex : Integer
    ) : TValue; override;
    procedure AfterConstruction; override;
  end;

  { TSearchResultTemplate }

  TSearchResultTemplate = class(TControlTemplate<TSearchResult>)
    function GetValue(
      const Item        : TSearchResult;
      const ColumnIndex : Integer
    ) : TValue; override;
  end;

implementation

{$REGION 'TSearchResultGroupTemplate'}
procedure TSearchResultGroupTemplate.AfterConstruction;
begin
  inherited AfterConstruction;
  RegisterDataTemplate(TSearchResultLineTemplate.Create);
end;

function TSearchResultGroupTemplate.GetItemCount(
  const Item: TSearchResultGroup): Integer;
begin
  Result := 1;
end;

function TSearchResultGroupTemplate.GetItems(
  const Item: TSearchResultGroup): IObjectList;
begin
  Result := Item.Lines;
end;

function TSearchResultGroupTemplate.GetValue(const Item: TSearchResultGroup;
  const ColumnIndex: Integer): TValue;
begin
  Result := Item.Text;
end;
{$ENDREGION}

{$REGION 'TSearchResultLineTemplate'}
procedure TSearchResultLineTemplate.AfterConstruction;
begin
  inherited AfterConstruction;
  RegisterDataTemplate(TSearchResultTemplate.Create);
end;

function TSearchResultLineTemplate.GetItemCount(
  const Item: TSearchResultLine): Integer;
begin
  Result := Item.List.Count;
end;

function TSearchResultLineTemplate.GetItems(
  const Item: TSearchResultLine): IObjectList;
begin
  Result := Item.List;
end;

function TSearchResultLineTemplate.GetValue(const Item: TSearchResultLine;
  const ColumnIndex: Integer): TValue;
begin
  Result := Item.Text;
end;
{$ENDREGION}

{$REGION 'TSearchResultTemplate'}
function TSearchResultTemplate.GetValue(const Item: TSearchResult;
  const ColumnIndex: Integer): TValue;
begin
  Result := Item.Text;
end;
{$ENDREGION}

end.

