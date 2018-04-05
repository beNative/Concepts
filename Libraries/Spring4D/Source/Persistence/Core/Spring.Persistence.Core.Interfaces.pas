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

unit Spring.Persistence.Core.Interfaces;

interface

uses
  Rtti,
  Variants,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.ValueConverters,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

const
  dtSQLite  = 'DRIVER_TYPE_SQLITE';
  dtADO     = 'DRIVER_TYPE_ADO';
  dtMSSQL   = 'DRIVER_TYPE_MSSQL';
  dtASA     = 'DRIVER_TYPE_ASA';
  dtOracle  = 'DRIVER_TYPE_ORACLE';
  dtDBX     = 'DRIVER_TYPE_DBX';
  dtUIB     = 'DRIVER_TYPE_UIB';
  dtZeos    = 'DRIVER_TYPE_ZEOS';
  dtMongo   = 'DRIVER_TYPE_MONGO';
  dtFireDAC = 'DRIVER_TYPE_FIREDAC';

type
  TDBDriverType = type string;

  TExecutionListenerProc = reference to procedure(const command: string;
    const params: IEnumerable<TDBParam>);

  IFieldCache = interface(IInvokable)
    ['{11B51ABB-0C29-40CA-A2C1-623CBFF86F4F}']
    function FieldExists(const fieldName: string): Boolean;
    function GetFieldValue(const fieldName: string): Variant;
  end;

  /// <summary>
  ///   Represents the result set to fetch data from the database.
  /// </summary>
  IDBResultSet = interface(IInvokable)
    ['{4FA97CFB-4992-4DAA-BB2A-B5CAF84B6B47}']
    function IsEmpty: Boolean;
    function Next: Boolean;
    function FieldExists(const fieldName: string): Boolean;
    function GetFieldValue(index: Integer): Variant; overload;
    function GetFieldValue(const fieldname: string): Variant; overload;
    function GetFieldCount: Integer;
    function GetFieldName(index: Integer): string;
  end;

  TQueryType = (qtQueryText, qtQueryEntity);

  TQueryMetadata = record
  public
    QueryOperation: TDMLCommandType;
    TableName: string;
  public
    class function GetQueryType(const query: Variant): TQueryType; inline; static;
  end;

  /// <summary>
  ///   Represents the executable database statement.
  /// </summary>
  IDBStatement = interface(IInvokable)
    ['{DA905CAA-0FC2-4570-9788-1DC206600171}']
    procedure SetSQLCommand(const commandText: string);
    procedure SetQuery(const metadata: TQueryMetadata; const query: Variant);
    procedure SetParams(const params: IEnumerable<TDBParam>);
    function Execute: NativeUInt;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet;
  end;

  /// <summary>
  ///   Represents the database transaction.
  /// </summary>
  /// <remarks>
  ///   If transaction was not committed, rollback will be performed when
  ///   interface goes out of scope.
  /// </remarks>
  IDBTransaction = interface(IInvokable)
    ['{AA35EE88-7271-4894-B6F0-06080C797BCF}']
    procedure Commit;
    procedure Rollback;
    function GetTransactionName: string;
    procedure SetTransactionName(const value: string);
    property TransactionName: string read GetTransactionName write SetTransactionName;
  end;

  /// <summary>
  ///   Represents the database connection.
  /// </summary>
  IDBConnection = interface(IInvokable)
    ['{256B8F14-7FF1-4442-A202-358B24756654}']
  {$REGION 'Property Accessors'}
    function GetAutoFreeConnection: Boolean;
    function GetExecutionListeners: IList<TExecutionListenerProc>;
    function GetQueryLanguage: TQueryLanguage;
    procedure SetAutoFreeConnection(value: Boolean);
    procedure SetQueryLanguage(const value: TQueryLanguage);
  {$ENDREGION}

    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;

    procedure AddExecutionListener(const listenerProc: TExecutionListenerProc);
    procedure ClearExecutionListeners;

    property AutoFreeConnection: Boolean read GetAutoFreeConnection write SetAutoFreeConnection;
    property ExecutionListeners: IList<TExecutionListenerProc> read GetExecutionListeners;
    property QueryLanguage: TQueryLanguage read GetQueryLanguage write SetQueryLanguage;
  end;

  /// <summary>
  ///   Represents interface for mapping rows of a ResultSet on a per-row
  ///   basis. Implementations of this interface perform the actual work of
  ///   mapping each row to a result object.
  /// </summary>
  IRowMapper<T: class, constructor> = interface(IInvokable)
    ['{557EE245-3542-40EC-86B5-16B1A3EA902A}']
    /// <summary>
    ///   Implementations must implement this method to map each row of data in
    ///   the ResultSet.
    /// </summary>
    /// <param name="resultSet">
    ///   The ResultSet to map (pre-initialized for the current row)
    /// </param>
    /// <returns>
    ///   New mapped entity
    /// </returns>
    function MapRow(const resultSet: IDBResultSet): T;
  end;

  IRowMapper = IRowMapper<TObject>;

  /// <summary>
  ///   Represents list session which can be used to sync changes in the list
  ///   with the database table. E.g. entities from the database can be fetched
  ///   by using a <see cref="Spring.Persistence.Core.Session|TSession" /> .
  ///   After List Session is started, changes can be made in the list (add new
  ///   entities, remove current entities, change entity properties, etc.). To
  ///   persist these changes <see cref="CommitListSession" /> must be called.
  ///   To clear changes history <see cref="RollbackListSession" /> must be
  ///   called.
  /// </summary>
  IListSession<T: class, constructor> = interface(IInvokable)
    ['{D3F67BC4-1F62-4C1F-8E1F-4CD3A414F79D}']
    procedure CommitListSession;
    procedure RollbackListSession;
  end;

  IRepository<T: class, constructor; TID> = interface(IInvokable)
    ['{849C6AB6-04F0-4C0F-B139-A08A3396525D}']
    /// <summary>
    ///   Executes sql statement which does not return resultset.
    /// </summary>
    function Execute(const query: string; const params: array of TValue): NativeUInt;

    /// <summary>
    ///   Retrieves multiple entities from the sql statement.
    /// </summary>
    function Query(const query: string; const params: array of TValue): IList<T>;
  end;

  ICrudRepository<T: class, constructor; TID> = interface(IRepository<T, TID>)
    ['{E490DD59-5466-4036-8CA5-852D8F7EF527}']
    function Count: Integer;

    /// <summary>
    ///   Retrieves single model from the database based on its primary key
    ///   value. If record not found, nil is returned.
    /// </summary>
    function FindOne(const id: TID): T;

    /// <summary>
    ///   Retrieves all models from PODO database table.
    /// </summary>
    function FindAll: IList<T>;

    /// <summary>
    ///   Checks if entity exists in the repository.
    /// </summary>
    function Exists(const id: TID): Boolean;

    /// <summary>
    ///   Inserts model to the database .
    /// </summary>
    procedure Insert(const entity: T); overload;

    /// <summary>
    ///   Inserts models to the database.
    /// </summary>
    procedure Insert(const entities: IEnumerable<T>); overload;

    /// <summary>
    ///   Saves the entity to the database. It will do update or the insert
    ///   based on the entity state.
    /// </summary>
    function Save(const entity: T): T; overload;

    /// <summary>
    ///   Saves entities to the database. It will do update or the insert based
    ///   on the entity state.
    /// </summary>
    function Save(const entities: IEnumerable<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Saves the entity and all entities it contains to the database. It
    ///   will do update or the insert based on the entity state.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     Use with caution when inserting new entities containing identity
    ///     primary keys. If both base (main) and sub entities are newly
    ///     created then framework won't be able to resolve their
    ///     relationships because their primary keys aren't known at save
    ///     time.
    ///   </para>
    ///   <para>
    ///     Works best when entities are updated.
    ///   </para>
    /// </remarks>
    procedure SaveCascade(const entity: T);

    /// <summary>
    ///   Removes model from the database.
    /// </summary>
    procedure Delete(const entity: T); overload;

    /// <summary>
    ///   Removes entities from the database.
    /// </summary>
    procedure Delete(const entities: IEnumerable<T>); overload;

    /// <summary>
    ///   Deletes all entities managed by the repository.
    /// </summary>
    procedure DeleteAll;
  end;

  IPagedRepository<T: class, constructor; TID> = interface(ICrudRepository<T, TID>)
    ['{46A40512-604A-4013-B0F0-693D81CAF5DF}']
    /// <summary>
    ///   Retrieves ICriteria
    /// </summary>
    function FindWhere: ICriteria<T>; overload;
    /// <summary>
    ///   Retrieves ICriteria, pre-constructed by given expression.
    /// </summary>
    /// <example>
    ///   <c>var Key: Prop; begin Key := GetProp('KEY');
    ///   fRepository.FindWhere(Key = 100).Page(1,10);</c>
    /// </example>
    function FindWhere(const expression: ICriterion): IList<T>; overload;
  end;

  IEntityWrapper = interface(IInvokable)
    ['{E6B12BC9-AA98-4F00-851B-8DDC0AADD36A}']
  {$REGION 'Property Accessors'}
    function GetEntity: TObject;
    function GetColumnsData: TColumnDataList;
    function GetOneToManyColumns: IEnumerable<OneToManyAttribute>;
    function GetManyToOneColumns: IEnumerable<ManyToOneAttribute>;
    function GetForeignKeyColumns: IEnumerable<ForeignJoinColumnAttribute>;
    function GetTableName: string;
    function GetPrimaryKeyValue: TValue; overload;
    procedure SetEntity(const value: TObject);
  {$ENDREGION}

    function GetValue(const member: TRttiMember): TValue;
    function GetPrimaryKeyValue(const resultSet: IDBResultSet): TValue; overload;

    procedure SetValue(const member: TRttiMember; const value: TValue);
    procedure SetPrimaryKeyValue(const value: TValue);

    function HasOneToManyRelations: Boolean;
    function HasManyToOneRelations: Boolean;

    property Entity: TObject read GetEntity write SetEntity;
    property ColumnsData: TColumnDataList read GetColumnsData;

    property TableName: string read GetTableName;
    property OneToManyColumns: IEnumerable<OneToManyAttribute> read GetOneToManyColumns;
    property ManyToOneColumns: IEnumerable<ManyToOneAttribute> read GetManyToOneColumns;
    property ForeignKeyColumns: IEnumerable<ForeignJoinColumnAttribute> read GetForeignKeyColumns;

    property PrimaryKeyValue: TValue read GetPrimaryKeyValue;
  end;

  IEntityMap = interface(IInvokable)
    ['{64F25680-A0F8-4A23-86D2-88A2F5F7D5EC}']
    procedure AddOrReplace(const instance: TObject);
    procedure Remove(const instance: TObject);
    procedure Clear;

    function IsMapped(const instance: TObject): Boolean;

    function GetChangedMembers(const instance: TObject;
      const entityData: TEntityData): IList<ColumnAttribute>;
    function GetMemberValue(const className, id: string;
      const member: TRttiMember): TValue;
  end;

  IDBCommand = interface
    ['{5E05151C-9D03-43A0-83A2-77442593F963}']
    procedure Build(entityClass: TClass);
    procedure BuildParams(const entity: TObject);
    function TableExists: Boolean; overload;
    function TableExists(const tableName: string): Boolean; overload;
  end;

  IDeleteCommand = interface(IDBCommand)
    ['{408EC739-B603-4001-AEB8-7909F1C0224A}']
    procedure Execute(const entity: TObject);
    procedure ExecuteById(const id: TValue);
  end;

  IInsertCommand = interface(IDBCommand)
    ['{1B20A79E-1E0A-4F87-B9CD-D404CFB433AC}']
    procedure Execute(const entity: TObject);
  end;

  IUpdateCommand = interface(IDBCommand)
    ['{3AAC8E1E-463A-4435-A0D3-EC12A9DEC9CB}']
    procedure Execute(const entity: TObject);
  end;

  ISelectCommand = interface(IDBCommand)
    ['{8D6D2229-8626-43C3-AD0A-091E4CB91050}']
    function Select: IDBResultSet;
    function SelectAll: IDBResultSet;
  end;

  IDDLCommand = interface(IDBCommand)
    ['{7D2046A5-F008-4F38-BD67-1248F2EC1658}']
    procedure Execute;
  end;

implementation


{$REGION 'TQueryMetadata'}

class function TQueryMetadata.GetQueryType(const query: Variant): TQueryType;
begin
  case VarType(query) of
    varUString, varString, varStrArg, varOleStr: Result := qtQueryText
  else
    Result := qtQueryEntity;
  end;
end;

{$ENDREGION}


end.
