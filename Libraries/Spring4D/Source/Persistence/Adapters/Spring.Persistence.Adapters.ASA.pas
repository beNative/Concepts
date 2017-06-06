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

unit Spring.Persistence.Adapters.ASA;

interface

uses
  Spring.Persistence.Adapters.ADO,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces;

type
  /// <summary>
  ///   Represents Sybase ASA resultset.
  /// </summary>
  TASAResultsetAdapter = class(TADOResultSetAdapter);

  /// <summary>
  ///   Represent Sybase ASA statement.
  /// </summary>
  TASAStatementAdapter = class(TADOStatementAdapter);

  /// <summary>
  ///   Represent Sybase ASA connection.
  /// </summary>
  TASAConnectionAdapter = class(TADOConnectionAdapter)
  public
    procedure AfterConstruction; override;
    function BeginTransaction: IDBTransaction; override;
  end;

  /// <summary>
  ///   Represent Sybase ASA transaction.
  /// </summary>
  TASATransactionAdapter = class(TADOTransactionAdapter)
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.ResourceStrings,
  Spring.Persistence.SQL.Generators.ASA,
  Spring.Persistence.SQL.Interfaces;


{$REGION 'TASAConnectionAdapter'}

procedure TASAConnectionAdapter.AfterConstruction;
begin
  inherited;
  QueryLanguage := qlASA;
end;

function TASAConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
    GenerateNewID;
    Connection.Execute(SQL_BEGIN_TRAN + GetTransactionName);

    Result := TASATransactionAdapter.Create(Connection, ExceptionHandler);
    Result.TransactionName := GetTransactionName;
  except
    raise HandleException;
  end
  else
    Result := nil;
end;

{$ENDREGION}


{$REGION 'TASATransactionAdapter'}

procedure TASATransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
  try
    Transaction.Execute(SQL_COMMIT_TRAN + TransactionName);
  except
    raise HandleException;
  end;
end;

procedure TASATransactionAdapter.Rollback;
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
  TConnectionFactory.RegisterConnection<TASAConnectionAdapter>(dtASA);

end.
