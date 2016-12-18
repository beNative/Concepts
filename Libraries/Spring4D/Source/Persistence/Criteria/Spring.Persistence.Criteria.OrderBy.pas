{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Persistence.Criteria.OrderBy;

interface

uses
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Implementation of <see cref="Spring.Persistence.Core.Interfaces|IOrderBy" />
  ///    interface.
  /// </summary>
  TOrderBy = class(TInterfacedObject, IOrderBy)
  private
    fEntityClass: TClass;
    fPropertyName: string;
    fSortingDirection: TSortingDirection;
    function GetEntityClass: TClass;
    function GetPropertyName: string;
    function GetSortingDirection: TSortingDirection;
    procedure SetEntityClass(value: TClass);
  protected
    constructor Create(const propertyName: string; sortingDirection: TSortingDirection);
  public
    class function Asc(const propertyName: string): IOrderBy;
    class function Desc(const propertyName: string): IOrderBy;
  end;

implementation


{$REGION 'TOrderBy'}

constructor TOrderBy.Create(const propertyName: string;
  sortingDirection: TSortingDirection);
begin
  inherited Create;
  fPropertyName := propertyName;
  fSortingDirection := sortingDirection;
end;

class function TOrderBy.Asc(const propertyName: string): IOrderBy;
begin
  Result := TOrderBy.Create(propertyName, stAscending);
end;

class function TOrderBy.Desc(const propertyName: string): IOrderBy;
begin
  Result := TOrderBy.Create(propertyName, stDescending);
end;

function TOrderBy.GetEntityClass: TClass;
begin
  Result := fEntityClass;
end;

function TOrderBy.GetPropertyName: string;
begin
  Result := fPropertyName;
end;

function TOrderBy.GetSortingDirection: TSortingDirection;
begin
  Result := fSortingDirection;
end;

procedure TOrderBy.SetEntityClass(value: TClass);
begin
  fEntityClass := value;
end;

{$ENDREGION}


end.
