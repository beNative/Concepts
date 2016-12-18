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

unit Spring.Persistence.Adapters.MSSQL;

interface

{$IFDEF MSWINDOWS}
uses
  Spring.Persistence.Adapters.ADO,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces;

{
  Must use OLEDB povider because ODBC providers are buggy with SQL SERVER
  see: http://stackoverflow.com/questions/4877576/how-to-make-ado-parameter-to-update-sql-server-datetime-column

  Connection string example: 'Provider=SQLOLEDB.1;Password=master;Persist Security Info=True;'+
    'User ID=VIKARINA;Initial Catalog=ViktorDemo;Data Source=FILE_SERVER';
}

type
  /// <summary>
  ///   Represents Miscrosoft SQL Server resultset.
  /// </summary>
  TMSSQLResultsetAdapter = class(TADOResultSetAdapter);

  /// <summary>
  ///   Represents Miscrosoft SQL Server statement.
  /// </summary>
  TMSSQLStatementAdapter = class(TADOStatementAdapter);

  /// <summary>
  ///   Represents Miscrosoft SQL Server connection.
  /// </summary>
  TMSSQLConnectionAdapter = class(TADOConnectionAdapter)
  public
    procedure AfterConstruction; override;
    function BeginTransaction: IDBTransaction; override;
  end;

  /// <summary>
  ///   Represents Miscrosoft SQL Server transaction.
  /// </summary>
  TMSSQLTransactionAdapter = class(TADOTransactionAdapter)
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.ResourceStrings,
  Spring.Persistence.SQL.Generators.MSSQL,
  Spring.Persistence.SQL.Interfaces;


{$REGION 'TMSSQLConnectionAdapter'}

procedure TMSSQLConnectionAdapter.AfterConstruction;
begin
  inherited;
  QueryLanguage := qlMSSQL;
end;

function TMSSQLConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
    GenerateNewID;
    Connection.Execute(SQL_BEGIN_TRAN + GetTransactionName);

    Result := TMSSQLTransactionAdapter.Create(Connection, ExceptionHandler);
    Result.TransactionName := GetTransactionName;
  except
    raise HandleException;
  end
  else
    Result := nil;
end;

{$ENDREGION}


{$REGION 'TMSSQLTransactionAdapter'}

procedure TMSSQLTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
  try
    Transaction.Execute(SQL_COMMIT_TRAN + TransactionName);
  except
    raise HandleException;
  end;
end;

procedure TMSSQLTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
  try
    Transaction.Execute(SQL_ROLLBACK_TRAN + TransactionName);
  except
    raise HandleException;
  end;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TMSSQLConnectionAdapter>(dtMSSQL);
{$ENDIF}

end.
