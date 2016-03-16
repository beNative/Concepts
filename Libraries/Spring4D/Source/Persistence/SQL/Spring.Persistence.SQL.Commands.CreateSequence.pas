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

unit Spring.Persistence.SQL.Commands.CreateSequence;

interface

uses
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract;

type
  /// <summary>
  ///   Responsible for building and executing statements which create
  ///   sequences in the database.
  /// </summary>
  TCreateSequenceExecutor = class(TAbstractCommandExecutor, IDDLCommand)
  private
    fSequence: TCreateSequenceCommand;
  protected
    function SequenceExists: Boolean; virtual;
    function GetCommand: TDMLCommand; override;
  public
    constructor Create(const connection: IDBConnection); override;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure Execute;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions;


{$REGION 'TSequenceCreateCommand'}

constructor TCreateSequenceExecutor.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fSequence := TCreateSequenceCommand.Create(nil);
end;

destructor TCreateSequenceExecutor.Destroy;
begin
  fSequence.Free;
  inherited Destroy;
end;

procedure TCreateSequenceExecutor.Build(entityClass: TClass);
begin
  inherited Build(entityClass);
  fSequence.Sequence := EntityData.Sequence;
  if Assigned(fSequence.Sequence) then
  begin
    fSequence.SequenceExists := SequenceExists;
    SQL := Generator.GenerateCreateSequence(fSequence);
  end
  else
    SQL := '';
end;

procedure TCreateSequenceExecutor.Execute;
var
  statement: IDBStatement;
begin
  if SQL = '' then
    Exit;

  statement := Connection.CreateStatement;
  statement.SetSQLCommand(SQL);
  statement.Execute;
end;

function TCreateSequenceExecutor.GetCommand: TDMLCommand;
begin
  Result := nil;
end;

function TCreateSequenceExecutor.SequenceExists: Boolean;
var
  sqlStatement: string;
  statement: IDBStatement;
  results: IDBResultSet;
begin
  Result := False;
  sqlStatement := Generator.GetSQLSequenceCount(fSequence.Sequence.SequenceName);
  if sqlStatement <> '' then
  try
    statement := Connection.CreateStatement;
    statement.SetSQLCommand(sqlStatement);
    results := statement.ExecuteQuery;
    Result := (not results.IsEmpty) and (results.GetFieldValue(0) > 0);
  except
    Result := False;
  end;
end;

{$ENDREGION}


end.
