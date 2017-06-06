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

{$I Spring.inc}

unit TestAdaptersADO;

interface

uses
  SysUtils,
  Variants,
  DB,
  ADOInt,
  ADODB,
  Classes,
  ComObj,
  TestFramework,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Mocking,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Adapters.ADO,
  Spring.Persistence.SQL.Interfaces,
  Spring.Reflection,
  Spring.TestUtils,
  TestMockADOConnection;

type
  IADOExecuteCall = interface
  end;

  TBaseADOAdapterTest = class(TTestCase)
  protected
    fMockConnectionObject: Mock<TMockADOConnection>;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure SetupOpen;
    function SetupExecute(const queries: array of string): IADOExecuteCall;
  end;

  TBaseADOConnectionAdapterTest = class(TBaseADOAdapterTest)
  protected
    fAdapter: TADOConnectionAdapter;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TADOExceptionHandlerAccess = class(TADOExceptionHandler);
  TADOExceptionHandlerTest = class(TTestCase<TADOExceptionHandlerAccess>)
  published
    procedure TestGetAdapterException_EDatabaseError;
    procedure TestGetAdapterException_ESafecallException;
    procedure TestGetAdapterException_EOleSysError;
    procedure TestGetAdapterException_Others_Return_Nil;
  end;

  TADOConnectionAdapterTest = class(TBaseADOConnectionAdapterTest)
  published
    procedure TestIsConnected;
    procedure TestConnect;
    procedure TestDisconnect;
    procedure TestCreateStatement;
    procedure TestBeginTransaction;
  end;

  TADOTransactionAdapterTest = class(TBaseADOConnectionAdapterTest)
  protected
    procedure SetUp; override;
  published
    procedure TestCommit;
    procedure TestRollback;
  end;

  // Test it here since we need some exception handler
  TDriverResultSetAdapterTest = class(TTestCase)
  private
    fDataSet: Mock<TDataSet>;
    fResultSet: IDBResultSet;
    procedure CreateResultSet;
    procedure FreeResultSet;
    function AddField(fieldClass: TFieldClass; const name: string): TField;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsEmpty;
    procedure TestNext;
    procedure TestFieldExists;
    procedure TestFieldCount;
    procedure TestFieldName;
    procedure TestNext_Exception;
    procedure TestGetFieldValue;
    procedure TestGetFieldValue_Exception;
  end;

implementation

uses
  TestEntities,
  TestExceptions;


{$REGION 'TADOExecuteCall'}

type
  TADOExecuteCall = class(TInterfacedObject, IADOExecuteCall)
  private
    {$IFDEF WEAKREF}[Unsafe]{$ENDIF}
    fTestCase: TTestCase;
    fQueries: IList<string>;
    fCurrentCallIndex: Integer;
  public
    constructor Create(const testCase: TTestCase;
      const queries: array of string);
    function Execute(const callInfo: TCallInfo): TValue;
  end;

constructor TADOExecuteCall.Create(const testCase: TTestCase;
  const queries: array of string);
begin
  inherited Create;
  fTestCase := testCase;
  fQueries := TCollections.CreateList<string>(queries);
  fCurrentCallIndex := -1
end;

function TADOExecuteCall.Execute(const callInfo: TCallInfo): TValue;
var
  query: string;
begin
  Inc(fCurrentCallIndex);
  query := callInfo[0].ToString;
  try
    if fCurrentCallIndex < fQueries.Count then
      fTestCase.CheckEqualsString(fQueries[fCurrentCallIndex], query,
        Format('Invalid query, index: %d', [fCurrentCallIndex]))
    else
      fTestCase.Fail(Format('Invalid query, no query expected index: %d query: %s',
        [fCurrentCallIndex, query]));
  except
    // Save status in case EOleException gets raised with undescriptive error
    on E: ETestFailure do
    begin
      fTestCase.Status(E.Message);
      raise;
    end
    else raise;
  end;
end;

{$ENDREGION}


{$REGION 'TBaseADOAdapterTest'}

procedure TBaseADOAdapterTest.SetUp;
begin
  inherited;
  fMockConnectionObject := Mock<TMockADOConnection>.Create(TMockBehavior.Strict);
end;

function TBaseADOAdapterTest.SetupExecute(const queries: array of string): IADOExecuteCall;
var
  lResult: TADOExecuteCall;
  o: OleVariant;
begin
  lResult := TADOExecuteCall.Create(Self, queries);
  Result := lResult;
  fMockConnectionObject.Setup.Executes(lResult.Execute).When(Args.Any)
    .Execute('', o, 0);
end;

procedure TBaseADOAdapterTest.SetupOpen;
begin
  with fMockConnectionObject.Setup.Executes do
  begin
    When(Args.Any).Get_ConnectionString;
    When(Args.Any).Get_State;
    When(Args.Any).Open('', '', '', 0);
  end;
end;

procedure TBaseADOAdapterTest.TearDown;
begin
  inherited;
  fMockConnectionObject.Free;
end;

{$ENDREGION}


{$REGION 'TBaseADOConnectionAdapterTest'}

procedure TBaseADOConnectionAdapterTest.SetUp;
begin
  inherited;
  fAdapter := TADOConnectionAdapter.Create(TADOConnection.Create(nil));
  fAdapter.AutoFreeConnection := True;
end;

procedure TBaseADOConnectionAdapterTest.TearDown;
begin
  // Ensure the mock alows the adapter to free properly
  SetupOpen;
  fAdapter.Free;
  inherited;
end;

{$ENDREGION}


{$REGION 'TADOConnectionAdapterTest'}

procedure TADOConnectionAdapterTest.TestBeginTransaction;
var
  transaction: IDBTransaction;
begin
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;

  // Test connect exception
  CheckException(EADOAdapterException,
    procedure begin fAdapter.BeginTransaction end);

  SetupOpen;
  // Test BeginTrans exception
  CheckException(EADOAdapterException,
    procedure begin fAdapter.BeginTransaction end);

  fMockConnectionObject.Setup.Executes.When(Args.Any).BeginTrans;
  transaction := fAdapter.BeginTransaction;
  CheckNotNull(transaction);
end;

procedure TADOConnectionAdapterTest.TestConnect;
begin
  CheckException(EADOAdapterException,
    procedure begin fAdapter.Connect end);

  SetupOpen;
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;
  fAdapter.Connect;

  Pass;
end;

procedure TADOConnectionAdapterTest.TestCreateStatement;
var
  statement: IDBStatement;
begin
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;

  statement := fAdapter.CreateStatement;
  CheckNotNull(statement);
end;

procedure TADOConnectionAdapterTest.TestDisconnect;
begin
  fMockConnectionObject.Setup.Returns<Integer>([adStateConnecting, adStateConnecting,
    adStateOpen]).When(Args.Any).Get_State;
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;

  CheckException(EADOAdapterException,
    procedure begin fAdapter.Disconnect end);

  SetupOpen;
  fAdapter.Disconnect;

  Pass;
end;

procedure TADOConnectionAdapterTest.TestIsConnected;
begin
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;
  CheckException(EADOAdapterException,
    procedure begin fAdapter.IsConnected end);

  fMockConnectionObject.Setup.Returns<Integer>([adStateConnecting, adStateConnecting,
    adStateOpen, adStateOpen]).When(Args.Any).Get_State;
  CheckTrue(fAdapter.IsConnected);

  fMockConnectionObject.Setup.Returns<Integer>([adStateConnecting, adStateConnecting,
    adStateClosed, adStateClosed]).When(Args.Any).Get_State;
  CheckFalse(fAdapter.IsConnected);
end;

{$ENDREGION}


{$REGION 'TADOTransactionAdapterTest'}

procedure TADOTransactionAdapterTest.SetUp;
begin
  inherited;
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;
end;

procedure TADOTransactionAdapterTest.TestCommit;
begin
  SetupOpen;
  with fMockConnectionObject.Setup.Executes do
  begin
    When.BeginTrans;
    When.CommitTrans;
  end;
  with fAdapter.BeginTransaction do
  begin
    Commit;
    fMockConnectionObject.Setup.Raises<EOleException>.When.CommitTrans;
    ExpectedException := EADOAdapterException;
    Commit;
  end;
end;

procedure TADOTransactionAdapterTest.TestRollback;
begin
  SetupOpen;
  with fMockConnectionObject.Setup.Executes do
  begin
    When.BeginTrans;
    When.RollbackTrans;
  end;
  with fAdapter.BeginTransaction do
  begin
    Rollback;
    fMockConnectionObject.Setup.Raises<EOleException>.When.RollbackTrans;
    ExpectedException := EADOAdapterException;
    Rollback;
  end;
end;

{$ENDREGION}


{$REGION 'TADOExceptionHandlerTest'}

procedure TADOExceptionHandlerTest.TestGetAdapterException_EDatabaseError;
var
  exc, result: Managed<Exception>;
begin
  exc := EDatabaseError.Create('');
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EADOAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckFalse(EADOAdapterException(result.Value).ErrorCode.HasValue);
end;

procedure TADOExceptionHandlerTest.TestGetAdapterException_EOleSysError;
var
  exc, result: Managed<Exception>;
begin
  exc := EOleException.Create('', -1, '', '', 0);
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EADOAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckEquals(-1, EADOAdapterException(result.Value).ErrorCode);
end;

procedure TADOExceptionHandlerTest.TestGetAdapterException_ESafecallException;
var
  exc, result: Managed<Exception>;
begin
  exc := ESafecallException.Create('');
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EADOAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckFalse(EADOAdapterException(result.Value).ErrorCode.HasValue);
end;

procedure TADOExceptionHandlerTest.TestGetAdapterException_Others_Return_Nil;
var
  exc, result: Managed<Exception>;
begin
  exc := Exception.Create('');
  result := SUT.GetAdapterException(exc, '');
  CheckNull(result);
end;

{$ENDREGION}


{$REGION 'TDriverResultSetAdapterTest'}

type
  TMockField = class(TField)
  public // get RTTI access for this originally protected method which we want to mock
    function GetAsVariant: Variant; override;
  end;

function TMockField.GetAsVariant: Variant;
begin
  Result := inherited;
end;

function TDriverResultSetAdapterTest.AddField(fieldClass: TFieldClass;
  const name: string): TField;
begin
  Result := fieldClass.Create(fDataSet);
  Result.FieldName := name;
  Result.DataSet := fDataSet;
end;

procedure TDriverResultSetAdapterTest.CreateResultSet;
begin
  if Assigned(fResultSet) then
    FreeResultSet;

  fResultSet := TDriverResultSetAdapter<TDataSet>.Create(fDataSet,
    TADOExceptionHandler.Create);
end;

procedure TDriverResultSetAdapterTest.FreeResultSet;
begin
  // Clear the internal value so mock is not freed and does not raise exception
  // during cleanup.
  TType.SetFieldValue(fResultSet as TObject, 'fDataSet', nil);
  fResultSet := nil;
end;

procedure TDriverResultSetAdapterTest.SetUp;
begin
  inherited;
  fDataSet := Mock<TDataSet>.Create(TMockBehavior.Strict,
    [TValue.From<TComponent>(nil)]);
  CreateResultSet;
end;

procedure TDriverResultSetAdapterTest.TearDown;
begin
  inherited;
  FreeResultSet;
  fDataSet.Free;
end;

procedure TDriverResultSetAdapterTest.TestFieldCount;
begin
  CheckEquals(0, fResultSet.GetFieldCount);

  AddField(TStringField, 'name');
  CreateResultSet;
  CheckEquals(1, fResultSet.GetFieldCount);

  AddField(TIntegerField, 'value');
  CreateResultSet;
  CheckEquals(2, fResultSet.GetFieldCount);
end;

procedure TDriverResultSetAdapterTest.TestFieldExists;
begin
  CheckFalse(fResultSet.FieldExists('name'));

  AddField(TStringField, 'name');
  CreateResultSet;
  CheckTrue(fResultSet.FieldExists('name'));
  CheckFalse(fResultSet.FieldExists('value'));
end;

procedure TDriverResultSetAdapterTest.TestFieldName;
begin
  AddField(TStringField, 'name');
  AddField(TIntegerField, 'value');
  CreateResultSet;

  CheckEqualsString('name', fResultSet.GetFieldName(0));
  CheckEqualsString('value', fResultSet.GetFieldName(1));
end;

procedure TDriverResultSetAdapterTest.TestGetFieldValue;
var
  stringField, integerField: Mock<TMockField>;
begin
  stringField := Mock<TMockField>.Create(TMockBehavior.Strict, [fDataSet.Instance]);
  stringField.Instance.FieldName := 'name';
  stringField.Instance.DataSet := fDataSet;
  stringField.Setup.Returns('test').When.Value;

  integerField := Mock<TMockField>.Create(TMockBehavior.Strict, [fDataSet.Instance]);
  integerField.Instance.FieldName := 'value';
  integerField.Instance.DataSet := fDataSet;
  integerField.Setup.Returns(42).When.Value;

  CreateResultSet;

  CheckEqualsString('test', fResultSet.GetFieldValue(0));
  CheckEqualsString('test', fResultSet.GetFieldValue('name'));
  CheckEquals(42, fResultSet.GetFieldValue(1));
  CheckEquals(42, fResultSet.GetFieldValue('value'));

  stringField.Setup.Returns(Null).When.Value;
  integerField.Setup.Returns(Null).When.Value;

  CheckTrue(VarIsNull(fResultSet.GetFieldValue(0)));
  CheckTrue(VarIsNull(fResultSet.GetFieldValue('name')));
  CheckTrue(VarIsNull(fResultSet.GetFieldValue(1)));
  CheckTrue(VarIsNull(fResultSet.GetFieldValue('value')));

  stringField.Received(4).Value;
  integerField.Received(4).Value;
end;

procedure TDriverResultSetAdapterTest.TestGetFieldValue_Exception;
var
  field: Mock<TMockField>;
begin
  field := Mock<TMockField>.Create(TMockBehavior.Strict, [fDataSet.Instance]);
  field.Instance.FieldName := 'name';
  field.Instance.DataSet := fDataSet;
  field.Setup.Raises<EDatabaseError>.When.Value;

  CreateResultSet;

  CheckException(EADOAdapterException,
    procedure begin fResultSet.GetFieldValue(0) end);
  CheckException(EADOAdapterException,
    procedure begin fResultSet.GetFieldValue('name') end);

  field.Received(2).Value;
end;

procedure TDriverResultSetAdapterTest.TestIsEmpty;
begin
  CheckTrue(fResultSet.IsEmpty);

  TType.SetFieldValue(fDataSet, 'fEOF', False);
  CheckFalse(fResultSet.IsEmpty);
end;

procedure TDriverResultSetAdapterTest.TestNext;
begin
  fDataSet.Setup.Executes.When.MoveBy(1);

  CheckFalse(fResultSet.Next);

  TType.SetFieldValue(fDataSet, 'fEOF', False);
  CheckTrue(fResultSet.Next);

  fDataSet.Received(2).MoveBy(1);
end;

procedure TDriverResultSetAdapterTest.TestNext_Exception;
begin
  fDataSet.Setup.Raises<EDatabaseError>.When.MoveBy(1);

  CheckException(EADOAdapterException,
    procedure begin fResultSet.Next end);
end;

{$ENDREGION}


initialization
  RegisterTests('Spring.Persistence.Adapters', [
    TADOConnectionAdapterTest.Suite,
    TADOTransactionAdapterTest.Suite,
    TADOExceptionHandlerTest.Suite,
    TDriverResultSetAdapterTest.Suite]);

end.
