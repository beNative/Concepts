{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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


  TestCaseAttribute = class(Spring.Testing.TestCaseAttribute)
  public
    constructor Create(x, y, expected: Integer); overload;
    constructor Create(c: TClass); overload;
  end;

  TSelfTest = class(TTestCase)
    [Sequential]
    procedure TestEnum(
      [Values]value: TTestEnum;
      [Range(Ord(Low(TTestEnum)), Ord(High(TTestEnum)))]ordValue: Integer);

    [TestCase('foo')]
    procedure TestParams(const s1, s2: string);

    [TestCase('foo;bar;foobar')]
    function TestResult(const s1, s2: string): string;

    [TestCase(1, 2, 3)]
    [TestCase(-1, 2, 1)]
    [TestCase(1, -2, -1)]
    [TestCase(-1, -2, -3)]
    function TestCustomAttribute(const x, y: Integer): Integer;

    [TestCase(TSelfTest)]
    procedure TestClass(c: TClass);
  end;

  TestData = class
    class function ConvertCases: TArray<TArray<TValue>>; static;
    class function EvenNumbers: TArray<Integer>; static;
    class function TestCases: TArray<TTestCaseData>; static;
  end;

  TDataDrivenTest = class(TTestCase)
    class function DivideCases: TArray<TArray<TValue>>; static;

    [TestCaseSource('DivideCases')]
    procedure DivideTest(n, d, q: Integer);

    [TestCaseSource(TestData, 'ConvertCases')]
    procedure ConvertTest(i: Integer; s: string);

    [TestCaseSource(TestData, 'EvenNumbers')]
    procedure TestMethod(i: Integer);

    [TestCaseSource(TestData, 'TestCases')]
    function DivideTest2(n, d: Integer): Integer;
  end;

  TSuiteSetUpTearDownTest = class(TTestCase)
  private
    class var fCount: Integer;
  protected
    class procedure SetUpFixture; override;
    class procedure TearDownFixture; override;

    procedure MethodRaisingException;
  published
    procedure CheckCount1;
    procedure CheckCount2;

    procedure TestNoStackoverflow;
  end;

implementation

uses
  Rtti,
  SysUtils,
  TypInfo;


{$REGION 'TSelfTest'}

procedure TSelfTest.TestClass(c: TClass);
begin
  CheckEquals(Self.ClassType, c);
end;

function TSelfTest.TestCustomAttribute(const x, y: Integer): Integer;
begin
  Result := x + y;
end;

procedure TSelfTest.TestEnum(value: TTestEnum; ordValue: Integer);
begin
  CheckEquals(ordValue, Ord(value));
end;

procedure TSelfTest.TestParams(const s1, s2: string);
begin
  CheckEqualsString('foo', s1);
  CheckEqualsString('', s2);
end;

function TSelfTest.TestResult(const s1, s2: string): string;
begin
  Result := s1 + s2;
end;

{$ENDREGION}


{$REGION 'TestCaseAttribute'}

constructor TestCaseAttribute.Create(x, y, expected: Integer);
begin
  fValues := TArray<TValue>.Create(x, y, expected);
end;

constructor TestCaseAttribute.Create(c: TClass);
begin
  fValues := TArray<TValue>.Create(c);
end;

{$ENDREGION}


{$REGION 'TDataDrivenTest'}

class function TestData.ConvertCases: TArray<TArray<TValue>>;
begin
//  Result := [[1,'1'],[22,'22']];
  Result := TArray<TArray<TValue>>.Create(
    TArray<TValue>.Create(1, '1'),
    TArray<TValue>.Create(22, '22')
  );
end;

class function TestData.EvenNumbers: TArray<Integer>;
begin
//  Result := [2, 4, 6, 8];
  Result :=  TArray<Integer>.Create(2, 4, 6, 8);
end;

class function TestData.TestCases: TArray<TTestCaseData>;
begin
//  Result := [
//    TTestCaseData.Create([12, 3]).Returns(4),
//    TTestCaseData.Create([12, 2]).Returns(6),
//    TTestCaseData.Create([12, 4]).Returns(3),
//    TTestCaseData.Create([0, 0]).Raises(EDivByZero).SetName('DivideByZero')
//  ];
  Result := TArray<TTestCaseData>.Create(
    TTestCaseData.Create([12, 3]).Returns(4),
    TTestCaseData.Create([12, 2]).Returns(6),
    TTestCaseData.Create([12, 4]).Returns(3),
    TTestCaseData.Create([0, 0]).Raises(EDivByZero).SetName('DivideByZero')
  );
end;

procedure TDataDrivenTest.ConvertTest(i: Integer; s: string);
begin
  CheckEquals(s, IntToStr(i));
end;

class function TDataDrivenTest.DivideCases: TArray<TArray<TValue>>;
begin
//  Result := [[12,3,4],[12,2,6],[12,4,3]];
  Result := TArray<TArray<TValue>>.Create(
    TArray<TValue>.Create(12, 3, 4),
    TArray<TValue>.Create(12, 2, 6),
    TArray<TValue>.Create(12, 4, 3)
  );
end;

procedure TDataDrivenTest.DivideTest(n, d, q: Integer);
begin
  CheckEquals(q, n / d);
end;

function TDataDrivenTest.DivideTest2(n, d: Integer): Integer;
begin
  Result := n div d;
end;

procedure TDataDrivenTest.TestMethod(i: Integer);
begin
  Check(i mod 2 = 0);
end;

{$ENDREGION}


{$REGION 'TSuiteSetUpTearDownTest'}

procedure TSuiteSetUpTearDownTest.CheckCount1;
begin
  CheckEquals(1, fCount);
end;

procedure TSuiteSetUpTearDownTest.CheckCount2;
begin
  CheckEquals(1, fCount);
end;

class procedure TSuiteSetUpTearDownTest.SetUpFixture;
begin
  Inc(fCount);
end;

procedure TSuiteSetUpTearDownTest.MethodRaisingException;
begin
  raise EProgrammerNotFound.Create('');
end;

class procedure TSuiteSetUpTearDownTest.TearDownFixture;
begin
  Dec(fCount);
end;

procedure TSuiteSetUpTearDownTest.TestNoStackoverflow;
begin
  CheckException(MethodRaisingException, EProgrammerNotFound);
end;

{$ENDREGION}


initialization
  TSelfTest.Register('Spring.Testing');
{$IFNDEF DELPHI2010}
  TDataDrivenTest.Register('Spring.Testing');
{$ENDIF}
  TSuiteSetUpTearDownTest.Register('Spring.Testing');

end.
