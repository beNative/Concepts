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

unit Spring.Tests.Helpers;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  Spring,
  Spring.Helpers;

type
  TTestRttiTypeHelper = class(TTestCase)
  published
    procedure TestGetGenericArguments;
  end;

type
  IDict<TKey,TValue> = interface
  end;

var
  dummy1: IDict<string,TObject>;
  dummy2: IDict<string,IDict<string,TObject>>;

implementation

uses
  Rtti,
  Spring.Reflection;


{$REGION 'TTestRttiTypeHelper'}

procedure TTestRttiTypeHelper.TestGetGenericArguments;
var
  t: TRttiType;
  types: TArray<TRttiType>;
begin
  t := TType.GetType(TypeInfo(IDict<string,TObject>));
  types := t.GetGenericArguments;
  CheckEquals(2, Length(types));
  Check(TypeInfo(string) = types[0].Handle);
  Check(TypeInfo(TObject) = types[1].Handle);

  t := TType.GetType(TypeInfo(IDict<string,IDict<string,TObject>>));
  types := t.GetGenericArguments;
  CheckEquals(2, Length(types));
  Check(TypeInfo(string) = types[0].Handle);
  Check(TypeInfo(IDict<string,TObject>) = types[1].Handle);

  t := TType.GetType(TypeInfo(IDict<string,IDict<string,IDict<string,TObject>>>));
  types := t.GetGenericArguments;
  CheckEquals(2, Length(types));
  Check(TypeInfo(string) = types[0].Handle);
  Check(TypeInfo(IDict<string,IDict<string,TObject>>) = types[1].Handle);
end;

{$ENDREGION}


end.
