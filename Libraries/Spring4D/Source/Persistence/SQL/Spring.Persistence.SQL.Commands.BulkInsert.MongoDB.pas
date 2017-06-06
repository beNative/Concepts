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

unit Spring.Persistence.SQL.Commands.BulkInsert.MongoDB;

interface

uses
  Spring.Collections,
  Spring.Persistence.SQL.Commands.Insert;

type
  TMongoDBBulkInsertExecutor = class(TInsertExecutor)
  public
    procedure BulkExecute<T: class, constructor>(const entities: IEnumerable<T>);
  end;

implementation

uses
  Rtti,
  MongoBson,
  Spring.Reflection,
  Spring.Persistence.SQL.Types,
  Spring.Persistence.Adapters.MongoDB;


{$REGION 'TMongoDBBulkInsertExecutor'}

procedure TMongoDBBulkInsertExecutor.BulkExecute<T>(const entities: IEnumerable<T>);
var
  LEntity: T;
  LQuery: string;
  LStatement: TMongoStatementAdapter;
  LConn: TMongoDBConnection;
  LDocs: TArray<IBSONDocument>;
  LCollection: string;
  i: Integer;
begin
  if not entities.Any then
    Exit;
  LConn := (Connection as TMongoConnectionAdapter).Connection;
  LStatement := TMongoStatementAdapter.Create(nil,
    TMongoDBExceptionHandler.Create);
  try
    SetLength(LDocs, entities.Count);
    i := 0;

    if CanClientAutogenerateValue then
      InsertCommand.InsertFields.Add(TSQLInsertField.Create(
        EntityData.PrimaryKeyColumn.ColumnName, Table, EntityData.PrimaryKeyColumn,
        InsertCommand.GetAndIncParameterName(EntityData.PrimaryKeyColumn.ColumnName)));

    for LEntity in entities do
    begin
      if CanClientAutogenerateValue then
        EntityData.PrimaryKeyColumn.Member.SetValue(LEntity, TValue.FromVariant(Generator.GenerateUniqueId));

      InsertCommand.Entity := LEntity;
      Command.Entity := LEntity;
      LQuery := Generator.GenerateInsert(InsertCommand);
      LStatement.SetSQLCommand(LQuery);
      if (LCollection = '') then
        LCollection := LStatement.GetFullCollectionName;
      LDocs[i] := JsonToBson(LStatement.GetQueryText);
      Inc(i);
    end;

    LConn.Insert(LCollection, LDocs);
  finally
    LStatement.Free;
  end;
end;

{$ENDREGION}


end.
