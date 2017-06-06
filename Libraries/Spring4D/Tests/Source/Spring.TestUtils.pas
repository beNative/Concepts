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

unit Spring.TestUtils;

{$I Spring.Tests.inc}

interface

uses
  SysUtils,
  TestFramework;

type
  TAbstractTestHelper = class helper for TAbstractTest
  private
    function GetExpectedException: ExceptionClass; inline;
  protected
  {$IFNDEF DELPHIXE2_UP}
    procedure CheckEquals(expected, actual: UInt64; msg: string = ''); overload;
  {$ENDIF}
  public
    procedure CheckEqualsString(const expected, actual: string; msg: string = '');
    procedure CheckException(expected: ExceptionClass; const method: TProc; const msg: string = '');
    procedure Pass; inline;
    function RegisterExpectedMemoryLeak(p: Pointer): Boolean; inline;
    procedure StartExpectingException(e: ExceptionClass);
    property ExpectedException: ExceptionClass
      read GetExpectedException write StartExpectingException;
  end;

  TTestCase<T: class, constructor> = class(TTestCase)
  private type
    TInterfacedObjectAccess = class(TInterfacedObject);
  private
    fSUT: T;
  strict protected
    property SUT: T read fSUT;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

procedure ProcessTestResult(const ATestResult: TTestResult);

implementation

uses
  Math,
  StrUtils;


procedure ProcessTestResult(const ATestResult: TTestResult);
begin
{$IFNDEF AUTOREFCOUNT}
  ATestResult.Free;
{$ENDIF}
end;

{$IFNDEF DELPHIXE2_UP}
function ReturnAddress: Pointer; inline;
begin
  Result := CallerAddr;
end;
{$ENDIF}


{$REGION 'TAbstractTestHelper'}

{$IFNDEF DELPHIXE2_UP}
procedure TAbstractTestHelper.CheckEquals(expected, actual: UInt64; msg: string);
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(UIntToStr(expected), UIntToStr(actual), msg, ReturnAddress);
end;
{$ENDIF}

procedure TAbstractTestHelper.CheckEqualsString(const expected, actual: string; msg: string);

  procedure EqualsFail(index: Integer); overload;
  const
    ContextCharCount = 20;
  begin
    if (msg <> '') and not EndsText(sLineBreak, msg) then
      msg := msg + sLineBreak;
    msg :=
      'Strings differ at position ' + IntToStr(index) + sLineBreak +
      'Expected: ' + ReplaceStr(Copy(expected, Max(1, index - ContextCharCount), ContextCharCount * 2), sLineBreak, '  ') + sLineBreak +
      'But was:  ' + ReplaceStr(Copy(actual, Max(1, index - ContextCharCount), ContextCharCount * 2), sLineBreak, '  ') + sLineBreak +
      '----------' + DupeString('-', Min(ContextCharCount, index - 1)) + '^';
    Fail(msg);
  end;

var
  i: Integer;
begin
  FCheckCalled := True;
  for i := 1 to Min(Length(expected), Length(actual)) do
    if expected[i] <> actual[i] then
      EqualsFail(i);
  if Length(expected) <> Length(actual) then
    EqualsFail(Min(Length(expected), Length(actual)) + 1);
end;

procedure TAbstractTestHelper.CheckException(expected: ExceptionClass;
  const method: TProc; const msg: string);
begin
  FCheckCalled := True;
  try
    method;
  except
    on E: Exception do
    begin
      if not Assigned(expected) then
        raise
      else if not E.InheritsFrom(expected) then
        FailNotEquals(expected.ClassName, E.ClassName, msg, ReturnAddress)
      else
        expected := nil;
    end;
  end;
  if Assigned(expected) then
    FailNotEquals(expected.ClassName, 'nothing', msg, ReturnAddress);
end;

function TAbstractTestHelper.GetExpectedException: ExceptionClass;
begin
  Result := FExpectedException;
end;

procedure TAbstractTestHelper.Pass;
begin
  FCheckCalled := True;
end;

function TAbstractTestHelper.RegisterExpectedMemoryLeak(p: Pointer): Boolean;
{$IFNDEF MSWINDOWS}
var
  memMgrEx: TMemoryManagerEx;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := System.RegisterExpectedMemoryLeak(p);
{$ELSE}
  GetMemoryManager(memMgrEx);
  if Assigned(memMgrEx.RegisterExpectedMemoryLeak) then
    Result := memMgrEx.RegisterExpectedMemoryLeak(p)
  else
    Result := False;
{$ENDIF}
end;

procedure TAbstractTestHelper.StartExpectingException(e: ExceptionClass);
begin
  StopExpectingException;
  FExpectedException := e;
  FCheckCalled := True;
end;

{$ENDREGION}


{$REGION 'TTestCase<T>'}

procedure TTestCase<T>.SetUp;
begin
  inherited SetUp;
  fSUT := T.Create;
{$IFNDEF AUTOREFCOUNT}
  if fSUT.InheritsFrom(TInterfacedObject) then
    TInterfacedObjectAccess(fSUT)._AddRef;
{$ENDIF}
end;

procedure TTestCase<T>.TearDown;
begin
{$IFNDEF AUTOREFCOUNT}
  if fSUT.InheritsFrom(TInterfacedObject) then
    TInterfacedObjectAccess(fSUT)._Release
  else
{$ENDIF}
    fSUT.Free;
  inherited TearDown;
end;

{$ENDREGION}


end.
