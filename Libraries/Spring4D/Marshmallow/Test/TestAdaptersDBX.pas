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

unit TestAdaptersDBX;

interface

uses
  SysUtils,
  DB,
  DBXCommon,
  Classes,
  TestFramework,
  SqlExpr,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Reflection,
  Spring.Mocking,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Adapters.DBX,
  Spring.Persistence.SQL.Interfaces,
  Spring.TestUtils,
  TestSQLConnection;

type
  TDBXExceptionHandlerAccess = class(TDBXExceptionHandler);
  TDBXExceptionHandlerTest = class(TTestCase<TDBXExceptionHandlerAccess>)
  published
    procedure TestGetAdapterException_EDatabaseError;
    procedure TestGetAdapterException_TDBXError;
    procedure TestGetAdapterException_Others_Return_Nil;
  end;

  TBaseDBXAdapterTest = class(TTestCase)
  protected
    fMockConnection: Mock<TTestSQLConnection>;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TBaseDBXConnectionAdapterTest = class(TBaseDBXAdapterTest)
  protected
    fAdapter: TDBXConnectionAdapter;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TDBXConnectionAdapterTest = class(TBaseDBXConnectionAdapterTest)
  published
    procedure TestIsConnected;
    procedure TestConnectException;
    procedure TestConnect;
    procedure TestDisconnect;
    procedure TestCreateStatement;
    procedure TestBeginTransaction;
  end;

  TDBXTransactionAdapterTest = class(TTestCase)
  private
    fMockConnection: Mock<TTestDBXConnection>;
    fTransaction: IDBTransaction;
    fDBXTransaction: TDBXTransaction;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDestroy;
    procedure TestCommit;
    procedure TestRollback;
  end;

  TDBXStatementAdapterTest = class(TBaseDBXAdapterTest)
  private
    fStatement: IDBStatement;
    fDBXStatement: TSQLQuery;
    fParams: TStrings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecute;
    procedure TestExecuteQuery;
  end;

implementation

uses
  Spring.Persistence.Core.ResourceStrings,
  TestExceptions;

type
  TDBXTransactionAdapterAccess = class(TDBXTransactionAdapter);


{$REGION 'TDBXExceptionHandlerTest'}

procedure TDBXExceptionHandlerTest.TestGetAdapterException_EDatabaseError;
var
  exc, result: Shared<Exception>;
begin
  exc := EDatabaseError.Create('');
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EDBXAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckFalse(EDBXAdapterException(result.Value).ErrorCode.HasValue);
end;

procedure TDBXExceptionHandlerTest.TestGetAdapterException_Others_Return_Nil;
var
  exc, result: Shared<Exception>;
begin
  exc := Exception.Create('');
  result := SUT.GetAdapterException(exc, '');
  CheckNull(result);
end;

procedure TDBXExceptionHandlerTest.TestGetAdapterException_TDBXError;
var
  exc, result: Shared<Exception>;
begin
  exc := TDBXError.Create(-1, '');
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EDBXAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckEquals(-1, EDBXAdapterException(result.Value).ErrorCode);
end;

{$ENDREGION}


{$REGION 'TBaseDBXAdapterTest'}

procedure TBaseDBXAdapterTest.SetUp;
begin
  inherited;
  fMockConnection := Mock<TTestSQLConnection>.Create(TMockBehavior.Strict);
end;

procedure TBaseDBXAdapterTest.TearDown;
begin
  fMockConnection.Free;
  inherited;
end;

{$ENDREGION}


{$REGION 'TBaseDBXConnectionAdapterTest'}

procedure TBaseDBXConnectionAdapterTest.SetUp;
begin
  inherited;
  fAdapter := TDBXConnectionAdapter.Create(fMockConnection);
end;

procedure TBaseDBXConnectionAdapterTest.TearDown;
begin
  fAdapter.Free;
  inherited;
end;

{$ENDREGION}


{$REGION 'TDBXConnectionAdapterTest'}

procedure TDBXConnectionAdapterTest.TestBeginTransaction;
var
  transaction: IDBTransaction;
begin
  // Test connect exception
  fMockConnection.Setup.Raises<TDBXError>.When.SetConnected(True);
  CheckException(EDBXAdapterException,
    procedure begin fAdapter.BeginTransaction end);

  fMockConnection.Setup.Executes.When.SetConnected(True);
  transaction := fAdapter.BeginTransaction;
  CheckNotNull(transaction);
end;

procedure TDBXConnectionAdapterTest.TestConnect;
begin
  fMockConnection.Setup.Executes.When.SetConnected(True);
  fAdapter.Connect;
  fMockConnection.Received(1).SetConnected(True);

  Pass;
end;

procedure TDBXConnectionAdapterTest.TestConnectException;
var
  adapter: TDBXConnectionAdapter;
begin
  // Use native connection for this test
  adapter := TDBXConnectionAdapter.Create(TSQLConnection.Create(nil));
  try
    adapter.AutoFreeConnection := True;
    ExpectedException := EDBXAdapterException;
    adapter.Connect;
  finally
    adapter.Free;
  end;
end;

procedure TDBXConnectionAdapterTest.TestCreateStatement;
var
  params: Shared<TStrings>;
  statement: IDBStatement;
begin
  fMockConnection.Setup.Executes.When(Args.Any).RegisterClient(nil, nil);
  params := TStringList.Create;
  TType.SetFieldValue(fMockConnection, 'FParams', params.Value);
  statement := fAdapter.CreateStatement;
  CheckNotNull(statement);
  fMockConnection.Setup.Executes.When(Args.Any).UnRegisterClient(nil);
  statement := nil;
end;

procedure TDBXConnectionAdapterTest.TestDisconnect;
begin
  fMockConnection.Setup.Raises<TDBXError>.When.SetConnected(False);
  CheckException(EDBXAdapterException,
    procedure begin fAdapter.Disconnect end);

  fMockConnection.Setup.Executes.When.SetConnected(False);
  fAdapter.Disconnect;
  fMockConnection.Received(2).SetConnected(False);

  Pass;
end;

procedure TDBXConnectionAdapterTest.TestIsConnected;
begin
  fMockConnection.Setup.Returns<Boolean>(True).When(Args.Any).GetConnected;
  CheckTrue(fAdapter.IsConnected);

  fMockConnection.Setup.Returns<Boolean>(False).When(Args.Any).GetConnected;
  CheckFalse(fAdapter.IsConnected);
end;

{$ENDREGION}


{$REGION 'TDBXTransactionAdapterTest'}

procedure TDBXTransactionAdapterTest.SetUp;
begin
  inherited;
  fMockConnection := Mock<TTestDBXConnection>.Create(TMockBehavior.Strict);
  fDBXTransaction := TTestDBXTransaction.Create(fMockConnection);
  fTransaction := TDBXTransactionAdapterAccess.Create(fDBXTransaction,
    TDBXExceptionHandler.Create);
end;

procedure TDBXTransactionAdapterTest.TearDown;
begin
  // Used by destructor
  fMockConnection.Setup.Executes.When(Args.Any).RollbackFreeAndNil(
    fDBXTransaction);
  fTransaction := nil;
  FreeAndNil(fDBXTransaction);
  fMockConnection.Free;
  inherited;
end;

procedure TDBXTransactionAdapterTest.TestCommit;
begin
  fMockConnection.Setup.Raises<TDBXError>.When.CommitFreeAndNil(fDBXTransaction);
  CheckException(EDBXAdapterException,
    procedure begin fTransaction.Commit end);

  fMockConnection.Setup.Executes.When.CommitFreeAndNil(fDBXTransaction);
  fTransaction.Commit;

  TDBXTransactionAdapterAccess(fTransaction).fTransaction := nil;
  fTransaction.Commit;

  fMockConnection.Received(2).CommitFreeAndNil(fDBXTransaction);
end;

procedure TDBXTransactionAdapterTest.TestDestroy;
begin
  fMockConnection.Setup.Executes.When.RollbackFreeAndNil(fDBXTransaction);
  fTransaction := nil;
  fMockConnection.Received(1).RollbackFreeAndNil(fDBXTransaction);
  Pass;
end;

procedure TDBXTransactionAdapterTest.TestRollback;
begin
  fMockConnection.Setup.Raises<TDBXError>.When.RollbackFreeAndNil(fDBXTransaction);
  CheckException(EDBXAdapterException,
    procedure begin fTransaction.Rollback end);

  fMockConnection.Setup.Executes.When.RollbackFreeAndNil(fDBXTransaction);
  fTransaction.Rollback;

  TDBXTransactionAdapterAccess(fTransaction).fTransaction := nil;
  fTransaction.Rollback;

  fMockConnection.Received(2).RollbackFreeAndNil(fDBXTransaction);
end;

{$ENDREGION}


{$REGION 'TDBXStatementAdapterTest'}

procedure TDBXStatementAdapterTest.SetUp;
begin
  inherited;
  fParams := TStringList.Create;
  TType.SetFieldValue(fMockConnection, 'FParams', fParams);
  fDBXStatement := TSQLQuery.Create(nil);
  // TValue cannot compare events
  fMockConnection.Setup.Executes.When(Args.Any).RegisterClient(nil, nil);
  fDBXStatement.SQLConnection := fMockConnection;
  fStatement := TDBXStatementAdapter.Create(fDBXStatement,
    TDBXExceptionHandler.Create);
end;

procedure TDBXStatementAdapterTest.TearDown;
begin
  fMockConnection.Setup.Executes.When.UnRegisterClient(fDBXStatement);
  fStatement := nil;
  fDBXStatement := nil; // Freed by Statement
  inherited;
  FreeAndNil(fParams);
end;

procedure TDBXStatementAdapterTest.TestExecute;
begin
  fMockConnection.Setup.Executes.When.SetConnected(True);
  CheckException(EDBXAdapterException,
    procedure begin fStatement.Execute end);
end;

procedure TDBXStatementAdapterTest.TestExecuteQuery;
var
  resultSet: IDBResultSet;
begin
  with fMockConnection.Setup do
  begin
    Executes.When.SetConnected(True);
    Returns<Integer>(0).When.GetDataSetCount;
    Executes.When(Args.Any).UnRegisterClient(nil);
  end;

  try
    resultSet := fStatement.ExecuteQuery;
    Fail('Exception expected');
  except
    on E: Exception do
    begin
      CheckIs(E, EDBXAdapterException);
      CheckNotNull(E.InnerException);
      CheckEqualsString(Format(SCannotOpenQuery,
        [E.InnerException.Message]), E.Message);
      resultSet := nil;
    end;
  end;
end;

{$ENDREGION}


initialization
  RegisterTests('Spring.Persistence.Adapters', [
    TDBXExceptionHandlerTest.Suite,
    TDBXConnectionAdapterTest.Suite,
    TDBXTransactionAdapterTest.Suite,
    TDBXStatementAdapterTest.Suite
  ]);

end.
