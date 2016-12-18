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

unit TestExceptions;

interface

uses
  SysUtils,
  TestFramework,
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.TestUtils;

type
  TORMExceptionHandlerImpl = class(TORMExceptionHandler)
  protected
    fResult: Exception;
    fDefaultMsg: string;
    function GetAdapterException(const exc: Exception;
      const defaultMsg: string): Exception; override;
  end;

  TTestORMExceptionHandler = class(TTestCase<TORMExceptionHandlerImpl>)
  protected
    procedure RaiseTestException(const exc: TObject;
      const defaultMsg: string = '');
  published
    procedure TestHandleException_ReRaise;
    procedure TestHandleException_NotAnExceptionAndUndexpectedExceptionReRaise;
    procedure TestHandleException_DefaultMsg;
    procedure TestHandleException_NoDefaultMsg;
    procedure TestHandleException_Abort;
    procedure TestHandleException_AdapterException;
  end;

implementation

type
  EORMTestException = class(EORMAdapterException);

{$REGION 'TTestORMExceptionHandler'}

procedure TTestORMExceptionHandler.RaiseTestException(const exc: TObject;
  const defaultMsg: string = '');
begin
  try
    raise exc;
  except
    raise SUT.HandleException(defaultMsg);
  end;
end;

procedure TTestORMExceptionHandler.TestHandleException_Abort;
var
  exc: Exception;
begin
  exc := EAbort.Create('exception');
  try
    RaiseTestException(exc);
    Fail('Exception expected');
  except
    on E: EAbort do
      CheckSame(exc, E);
  end;
end;

procedure TTestORMExceptionHandler.TestHandleException_AdapterException;
var
  inner, outer: Exception;
begin
  inner := Exception.Create('inner');
  outer := Exception.Create('outer');
  SUT.fResult := outer;
  try
    RaiseTestException(inner);
    Fail('Exception expected');
  except
    on E: Exception do
    begin
      CheckSame(outer, E);
      CheckSame(inner, E.InnerException);
    end;
  end;
end;

procedure TTestORMExceptionHandler.TestHandleException_DefaultMsg;
var
  exc: Exception;
begin
  exc := Exception.Create('exception');
  try
    RaiseTestException(exc, 'default');
    Fail('Exception expected');
  except
    on E: Exception do ;
  end;
  CheckEqualsString('default', SUT.fDefaultMsg);
end;

procedure TTestORMExceptionHandler.TestHandleException_NoDefaultMsg;
var
  exc: Exception;
begin
  exc := Exception.Create('exception');
  try
    RaiseTestException(exc);
    Fail('Exception expected');
  except
    on E: Exception do ;
  end;
  CheckEqualsString('exception', SUT.fDefaultMsg);
end;

procedure TTestORMExceptionHandler.TestHandleException_NotAnExceptionAndUndexpectedExceptionReRaise;
var
  exc: TObject;
begin
  exc := TObject.Create;
  try
    RaiseTestException(exc);
    Fail('Exception expected');
  except
    on E: TObject do
      CheckSame(exc, E);
  end;
end;

procedure TTestORMExceptionHandler.TestHandleException_ReRaise;
var
  exc: Exception;
begin
  // Check ORM exception re-raise
  exc := EORMTestException.Create('exception');
  try
    RaiseTestException(exc);
    Fail('Exception expected');
  except
    on E: Exception do
    begin
      CheckSame(exc, E);
      CheckNull(exc.InnerException);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TORMExceptionHandlerImpl'}

function TORMExceptionHandlerImpl.GetAdapterException(const exc: Exception;
  const defaultMsg: string): Exception;
begin
  Result := fResult;
  fDefaultMsg := defaultMsg;
end;

{$ENDREGION}

initialization
  RegisterTest('Spring.Persistence.Core', TTestORMExceptionHandler.Suite);

end.
