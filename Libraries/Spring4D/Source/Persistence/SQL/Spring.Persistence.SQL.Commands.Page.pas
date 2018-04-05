{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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

unit Spring.Persistence.SQL.Commands.Page;

interface

uses
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract;

type
  /// <summary>
  ///   Responsible for building and executing paged statements.
  /// </summary>
  TPageExecutor = class(TAbstractCommandExecutor)
  private
    fPage: Integer;
    fItemsPerPage: Integer;
    fTotalItems: Int64;
    function GetLimit: Integer;
    function GetOffset: Integer;
  protected
    function GetCommand: TDMLCommand; override;
  public
    function BuildSQL(const sql: string): string;

    property Page: Integer read fPage write fPage;
    property ItemsPerPage: Integer read fItemsPerPage write fItemsPerPage;
    property TotalItems: Int64 read fTotalItems write fTotalItems;
    property Limit: Integer read GetLimit;
    property Offset: Integer read GetOffset;
  end;

implementation


{$REGION 'TPageExecutor'}

function TPageExecutor.BuildSQL(const sql: string): string;
begin
  Result := Generator.GeneratePagedQuery(sql, Limit, Offset);
end;

function TPageExecutor.GetCommand: TDMLCommand;
begin
  Result := nil;
end;

function TPageExecutor.GetLimit: Integer;
begin
  Result := ItemsPerPage;
end;

function TPageExecutor.GetOffset: Integer;
begin
  if Page <= 1 then
    Result := 0
  else
    Result := (Page * ItemsPerPage) - ItemsPerPage;
end;

{$ENDREGION}


end.
