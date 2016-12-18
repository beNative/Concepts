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

unit Spring.Tests.Testing;

interface

{$I Spring.inc}
{$I Spring.Tests.inc}

uses
  Spring.Testing;

type
  TTestEnum = (zero, one, two, three);

  TSelfTest = class(TTestCase)
    [Sequential]
    procedure TestEnum(
      [Values]value: TTestEnum;
      [Range(Ord(Low(TTestEnum)), Ord(High(TTestEnum)))]ordValue: Integer);
  end;

implementation

uses
  TypInfo;


{$REGION 'TSelfTest'}

procedure TSelfTest.TestEnum(value: TTestEnum; ordValue: Integer);
begin
  CheckEquals(ordValue, Ord(value));
end;

{$ENDREGION}


initialization
  TSelfTest.Register('Spring.Testing');


end.
