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

unit Spring.Tests.Reflection;

interface

{$I Spring.inc}
{$I Spring.Tests.inc}

uses
  TestFramework;

type
  TTestType = class(TTestCase)
  published
    procedure TestIsDelegateTypeWithTFunc;
    procedure TestIsDelegateTypeWithTProc;
    procedure TestIsDelegateTypeWithTPredicate;
    procedure TestIsDelegateTypeWithNil;
    procedure TestIsDelegateTypeWithIInterface;
    procedure TestIsDelegateTypeWithTObject;

    procedure TestIsAssignableClassAndInterfaceNotImplemented;
    procedure TestIsAssignableClassAndInterfaceImplemented;
  end;

  ITestInterface1 = interface
    ['{4A7F972F-3EED-44E2-983D-C052A2A1CCFA}']
  end;

  ITestInterface2 = interface
    ['{E14D2471-EBCC-4A9E-AE0D-5F88AED4AD39}']
  end;

  ITestInterface3 = interface
    ['{13473FD9-6092-4D66-933B-4CA96CF7A004}']
  end;

  ITestInterface4 = interface
    ['{59953D36-963C-4655-9B38-5C5B553662D5}']
  end;

  TTestObject = class(TInterfacedObject, ITestInterface1, ITestInterface2, ITestInterface3)

  end;

  TTestMethodHelper = class(TTestCase)
  published
    procedure ParameterParentEqualsOriginalMethodInstance;
  end;

implementation

uses
  Classes,
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Reflection;


{$REGION 'TTestType'}

procedure TTestType.TestIsAssignableClassAndInterfaceNotImplemented;
begin
  CheckFalse(TType.IsAssignable(TypeInfo(TTestObject), TypeInfo(ITestInterface4)));
end;

procedure TTestType.TestIsAssignableClassAndInterfaceImplemented;
begin
  CheckTrue(TType.IsAssignable(TypeInfo(TTestObject), TypeInfo(ITestInterface3)));
end;

procedure TTestType.TestIsDelegateTypeWithIInterface;
begin
  CheckFalse(TType.IsDelegate(TypeInfo(IInterface)));
end;

procedure TTestType.TestIsDelegateTypeWithNil;
begin
  CheckFalse(TType.IsDelegate(nil));
end;

procedure TTestType.TestIsDelegateTypeWithTFunc;
begin
  CheckTrue(TType.IsDelegate(TypeInfo(TFunc<Integer>)));
end;

procedure TTestType.TestIsDelegateTypeWithTObject;
begin
  CheckFalse(TType.IsDelegate(TypeInfo(TObject)));
end;

procedure TTestType.TestIsDelegateTypeWithTPredicate;
begin
  CheckTrue(TType.IsDelegate(TypeInfo(TPredicate<Integer>)));
end;

procedure TTestType.TestIsDelegateTypeWithTProc;
begin
  CheckTrue(TType.IsDelegate(TypeInfo(TProc<Integer>)));
end;

{$ENDREGION}


{$REGION 'TTestMethodHelper'}

procedure TTestMethodHelper.ParameterParentEqualsOriginalMethodInstance;
var
  t: TRttiType;
  m: TRttiMethod;
  p: TRttiParameter;
begin
  t := TType.GetType(TComponent);
  m := t.GetMethod('Create');

  CheckTrue(m.IsConstructor, 'TRttiMethod.IsConstructor');
  p := m.GetParameters[0];
  CheckIs(p.Parent, TRttiMethod, 'TRttiParameter.Parent');
  CheckTrue(TRttiMethod(p.Parent).IsConstructor, 'TRttiParameter.Parent.IsConstructor');
  CheckSame(m, p.Parent, 'TRttiParameter.Parent');
end;

{$ENDREGION}


end.
