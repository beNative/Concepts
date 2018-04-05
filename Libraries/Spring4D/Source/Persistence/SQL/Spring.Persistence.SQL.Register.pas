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

unit Spring.Persistence.SQL.Register;

interface

uses
  Spring.Collections,
  Spring.Persistence.SQL.Interfaces;

type
  /// <summary>
  ///   Represent factory for <c>ISQLGenerators.</c> Each custom SQL generator
  ///   must register itself with this factory class.
  /// </summary>
  TSQLGeneratorRegister = class
  strict private
    class var fGenerators: IDictionary<TQueryLanguage,ISQLGenerator>;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterGenerator(const generator: ISQLGenerator);
    class function GetGenerator(const queryLanguage: TQueryLanguage): ISQLGenerator;
  end;

implementation

uses
  Spring.Persistence.SQL.Generators.Ansi;


{$REGION 'TSQLGeneratorRegister'}

class constructor TSQLGeneratorRegister.Create;
begin
  fGenerators := TCollections.CreateDictionary<TQueryLanguage,ISQLGenerator>;
  RegisterGenerator(TAnsiSQLGenerator.Create as ISQLGenerator);
end;

class destructor TSQLGeneratorRegister.Destroy;
begin
  fGenerators := nil;
end;

class function TSQLGeneratorRegister.GetGenerator(const queryLanguage: TQueryLanguage): ISQLGenerator;
begin
  Result := fGenerators[queryLanguage];
end;

class procedure TSQLGeneratorRegister.RegisterGenerator(const generator: ISQLGenerator);
begin
  fGenerators.AddOrSetValue(generator.QueryLanguage, generator);
end;

{$ENDREGION}


end.
