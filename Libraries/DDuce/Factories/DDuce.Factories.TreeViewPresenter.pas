{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DDuce.inc}

unit DDuce.Factories.TreeViewPresenter;

interface

{
Missing properties in
  TTreeViewPresenter:
    - AutoScroll ?
    - Last: set focus to last item
    - First: set focus to first item

  IColumnDefinitions
    - Delete(index)
    - Clear

  TColumnDefinition
    -
}

uses
  System.Classes,
  Vcl.Controls,

  Spring, Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Core.DataTemplates,
  DSharp.Bindings.Collections, DSharp.Windows.ColumnDefinitions,

  VirtualTrees;

type
  TFactories = class sealed
  public
    class procedure InitializeTVP(
      ATVP              : TTreeViewPresenter;
      AVST              : TVirtualStringTree = nil;
      ASource           : IObjectList = nil;
      AColumDefinitions : IColumnDefinitions = nil;
      ATemplate         : IDataTemplate = nil;
      AFilter           : TFilterEvent = nil
    ); static;

    class function CreateColumnDefinitions(
      ATVP: TTreeViewPresenter = nil
    ): IColumnDefinitions; static;

    class function CreateTreeViewPresenter(
      AOwner            : TComponent;
      AVST              : TVirtualStringTree = nil;
      ASource           : IObjectList = nil;
      AColumDefinitions : IColumnDefinitions = nil;
      ATemplate         : IDataTemplate = nil;
      AFilter           : TFilterEvent = nil;
      const AName       : string = ''
    ): TTreeViewPresenter; overload; static;
  end;

implementation

uses
  System.Rtti,
  Vcl.Graphics,

  DSharp.Windows.ColumnDefinitions.ControlTemplate;

class procedure TFactories.InitializeTVP(ATVP: TTreeViewPresenter;
  AVST: TVirtualStringTree; ASource: IObjectList; AColumDefinitions
  : IColumnDefinitions; ATemplate: IDataTemplate; AFilter: TFilterEvent);
var
  P : TRttiProperty;
  C : TRttiContext;
begin
  if Assigned(ASource) then
  begin
    if Assigned(AColumDefinitions) then
    begin
      if ATVP.ColumnDefinitions <> AColumDefinitions then
        ATVP.ColumnDefinitions := AColumDefinitions
    end
    else // auto create column definitions
    begin
      ATVP.BeginUpdate;
      ATVP.ColumnDefinitions.Clear;
      for P in C.GetType(ASource.ElementType).GetProperties do
      begin
        with ATVP.ColumnDefinitions.Add(P.Name) do
        begin
          ValuePropertyName := P.Name;
          HintPropertyName  := P.Name;
          // required to automatically add hoAutoResize to the tree's header
          // options.
          AutoSize          := True;
        end;
      end;
      ATVP.EndUpdate;
    end;
  end;
  ATVP.TreeView := AVST;
  ATVP.SyncMode := True;
  ATVP.UseColumnDefinitions := True;
  ATVP.ListMode             := True;
  if Assigned(ASource) then
    ATVP.View.ItemsSource := ASource as IObjectList;
  if not Assigned(ATemplate) then
    ATVP.View.ItemTemplate :=
      TColumnDefinitionsControlTemplate.Create(ATVP.ColumnDefinitions)
  else
    ATVP.View.ItemTemplate := ATemplate;
  if Assigned(AFilter) then
    ATVP.View.Filter.Add(AFilter);
end;

class function TFactories.CreateColumnDefinitions(ATVP: TTreeViewPresenter):
  IColumnDefinitions;
begin
  Result := TColumnDefinitions.Create(ATVP);
end;

class function TFactories.CreateTreeViewPresenter(AOwner: TComponent;
  AVST: TVirtualStringTree; ASource: IObjectList;
  AColumDefinitions: IColumnDefinitions; ATemplate: IDataTemplate;
  AFilter: TFilterEvent; const AName: string): TTreeViewPresenter;
var
  TVP: TTreeViewPresenter;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  TVP := TTreeViewPresenter.Create(AOwner);
  InitializeTVP(TVP, AVST, ASource, AColumDefinitions, ATemplate, AFilter);
  Result := TVP;
end;

end.
