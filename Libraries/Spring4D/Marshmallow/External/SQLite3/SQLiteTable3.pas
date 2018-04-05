unit SQLiteTable3;

{
  Simple classes for using SQLite's exec and get_table.

  TSQLiteDatabase wraps the calls to open and close an SQLite database.
  It also wraps SQLite_exec for queries that do not return a result set

  TSQLiteTable wraps execution of SQL query.
  It run query and read all returned rows to internal buffer.
  It allows accessing fields by name as well as index and can move through a
  result set forward and backwards, or randomly to any row.

  TSQLiteUniTable wraps execution of SQL query.
  It run query as TSQLiteTable, but reading just first row only!
  You can step to next row (until not EOF) by 'Next' method.
  You cannot step backwards! (So, it is called as UniDirectional result set.)
  It not using any internal buffering, this class is very close to Sqlite API.
  It allows accessing fields by name as well as index on actual row only.
  Very good and fast for sequentional scanning of large result sets with minimal
    memory footprint.

  Warning! Do not close TSQLiteDatabase before any TSQLiteUniTable,
    because query is closed on TSQLiteUniTable destructor and database connection
    is used during TSQLiteUniTable live!

  SQL parameter usage:
    You can add named parameter values by call set of AddParam* methods.
    Parameters will be used for first next SQL statement only.
    Parameter name must be prefixed by ':', '$' or '@' and same prefix must be
    used in SQL statement!
    Sample:
      table.AddParamText(':str', 'some value');
      s := table.GetTableString('SELECT value FROM sometable WHERE id=:str');

   Notes from Andrew Retmanski on prepared queries
   The changes are as follows:

   SQLiteTable3.pas
   - Added new boolean property Synchronised (this controls the SYNCHRONOUS pragma as I found that turning this OFF increased the write performance in my application)
   - Added new type TSQLiteQuery (this is just a simple record wrapper around the SQL string and a TSQLiteStmt pointer)
   - Added PrepareSQL method to prepare SQL query - returns TSQLiteQuery
   - Added ReleaseSQL method to release previously prepared query
   - Added overloaded BindSQL methods for Integer and String types - these set new values for the prepared query parameters
   - Added overloaded ExecSQL method to execute a prepared TSQLiteQuery
   - Added TSQLitePreparedStatement class for managing prepared queries. It is recommended to use it together with TSQliteUniTable for best performance

   Usage of the new methods should be self explanatory but the process is in essence:

   1. Call PrepareSQL to return TSQLiteQuery 2. Call BindSQL for each parameter in the prepared query 3. Call ExecSQL to run the prepared query 4. Repeat steps 2 & 3 as required 5. Call ReleaseSQL to free SQLite resources

   One other point - the Synchronised property throws an error if used inside a transaction.

   Acknowledments
   Adapted by Tim Anderson (tim@itwriting.com)
   Originally created by Pablo Pissanetzky (pablo@myhtpc.net)
   Modified and enhanced by Lukas Gebauer
   Modified and enhanced by Tobias Gunkel
   Modified and enhanced by Linas Naginionis (lnaginionis@gmail.com)
}
{$I SQLite3.inc}
interface

{$IFDEF FPC}
  {$MODE Delphi}{$H+}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$DEFINE USE_SYSTEM_SQLITE}
{$ENDIF}

uses
  {$IFNDEF USE_SYSTEM_SQLITE}
  SQLite3,
  {$ELSE}
  System.Sqlite,
  {$ENDIF}
  {$IFDEF DELPHI16_UP}
  {$IFDEF WIN32}
  Winapi.Windows,
  {$ENDIF}
  System.Classes, System.SysUtils, System.Generics.Collections, DB;
  {$ELSE}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Classes, SysUtils, Generics.Collections, DB;
  {$ENDIF}

const

  dtInt = 1;
  dtNumeric = 2;
  dtStr = 3;
  dtBlob = 4;
  dtNull = 5;
  //my types
  dtDate = 15;
  dtDateTime = 16;

{$IFDEF USE_SYSTEM_SQLITE}
type
  TSQLiteActionCode = type Integer;
  TSQLiteDB = Pointer;
  TSQLiteResult = type PMarshaledAString;
  TSQLiteStmt = Pointer;
  TSQLiteBackup = type pointer;
  PPsqlite3_value = ^Psqlite3_value;
  //function prototype for define own collate
  TCollateXCompare = function(UserData: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;
{$IFDEF NEXTGEN}
  PAnsiChar = MarshaledAString;
  PPAnsiChar = ^PAnsiChar;
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IF NOT Declared(AnsiString)}
  {$IF Declared(RawByteString)}
  AnsiString = RawByteString;
  {$ELSE}
  {$DEFINE USE_CUSTOM_ANSISTRING}
  AnsiString = record
  private
    FBytes: TBytes;
  public
    class operator Implicit(const s: string): AnsiString;
    class operator Implicit(const s: AnsiString): string;
    class operator Explicit(const s: AnsiString): PAnsiChar;
  end;
  {$IFEND}
{$IFEND}
const
  SQLITE_STATIC : TBindDestructor = TBindDestructor(System.Sqlite.SQLITE_STATIC);
  SQLITE_TRANSIENT : TBindDestructor = TBindDestructor(System.Sqlite.SQLITE_TRANSIENT);
{$ENDIF}

type
  TSQLiteDBEncoding = (seUTF8, seUTF16);
  //forward declarations
  ISQLitePreparedStatement = interface;
  ISQLiteTable = interface;
  TSQLiteDatabase = class;
  TSQLiteTable = class;
  TSQLiteUniTable = class;
  TSQLitePreparedStatement = class;
  TSQLiteFunctions = class;

  ESQLiteException = class(Exception)
  private
    FErrorCode: Integer;
  public
    property ErrorCode: Integer read FErrorCode;

    constructor Create(const Msg: string; ErrorCode: Integer = -1);
    constructor CreateFmt(const Msg: string; const Args: array of const;
      ErrorCode: Integer = -1);
  end;
  ESQLiteConstraintException = class(ESQLiteException);
  ESQLiteExceptionClass = class of ESQLiteException;

  TSQliteParam = class
  public
    name: string;
    index: Integer;
    valuetype: integer;
    valueinteger: int64;
    valuefloat: double;
    valuedata: string;
    valueptr: Pointer;
    blobsize: Int64;
    constructor Create(); virtual;
  end;

  THookQuery = procedure(Sender: TObject; const SQL: String) of object;
  TAuthEvent = procedure(Sender: TSQLiteDatabase; ActionCode: TSQLiteActionCode;
    const AArg1, AArg2, AArg3, AArg4: String; var AResult: Integer) of object;
  TUpdateHookEvent = procedure(Sender: TSQLiteDatabase; Operation: TSQLiteActionCode;
   const ADatabase, ATable: String; ARowID: Int64) of object;


  TSQLiteQuery = record
    SQL: String;
    Statement: TSQLiteStmt;
  end;

  TSQLiteUserFunc = reference to procedure(sqlite3_context: Psqlite3_context; ArgIndex: Integer; ArgValue: PPsqlite3_value);
  TSQLiteUserFuncFinal = reference to procedure(sqlite3_context: Psqlite3_context);

  TSQLiteFuncType = (sftScalar, sftAggregate);

  TSQLiteFuncs = record
    Funcs: TSQLiteFunctions;
    AFunc: TSQLiteUserFunc;
    AStepFunc: TSQLiteUserFunc;
    AFinalFunc: TSQLiteUserFuncFinal;
    FuncType: TSQLiteFuncType;
    FuncName: string;
  end;

  PSQLiteFuncs = ^TSQLiteFuncs;

  /// <summary>
  /// User defined SQLite functions
  /// </summary>
  TSQLiteFunctions = class
  private
    {$IFDEF WEAKREF}[Unsafe]{$ENDIF}
    FDB: TSQLiteDatabase;
    FVals: TList<PSQLiteFuncs>;
  public
    constructor Create(const DB: TSQLiteDatabase); virtual;
    destructor Destroy; override;

    class function GetValueType(ArgValue: Psqlite3_value): Integer;
    class function ValueAsInteger(ArgValue: Psqlite3_value): Int64;
    class function ValueAsString(ArgValue: Psqlite3_value): string;
    class function ValueAsFloat(ArgValue: Psqlite3_value): Double;

    class function GetArrayElement(Elements: PPsqlite3_value; Index: Integer): Psqlite3_value;

    class procedure ResultAsNull(sqlite3_context: Psqlite3_context);
    class procedure ResultAsInteger(sqlite3_context: Psqlite3_context; Val: Int64);
    class procedure ResultAsString(sqlite3_context: Psqlite3_context; Val: string);
    class procedure ResultAsFloat(sqlite3_context: Psqlite3_context; Val: Double);

{$IFNDEF NEXTGEN}
    procedure AddScalarFunction(const FuncName: AnsiString; ArgCount: Integer;
      AFunc: TSQLiteUserFunc);
    procedure AddAggregateFunction(const FuncName: AnsiString; ArgCount: Integer;
      AStepFunc: TSQLiteUserFunc; AFinalFunc: TSQLiteUserFuncFinal) ;
{$ENDIF}
    procedure Clear;
  end;


  /// <summary>
  /// SQLite database class
  /// </summary>
  TSQLiteDatabase = class(TComponent)
  class var
    FColumnTypes: TDictionary<string,Integer>;
  private
    fDB: TSQLiteDB;
    fInTrans: boolean;
    fSync: boolean;
    fParams: TObjectList<TSQliteParam>;
    FOnQuery: THookQuery;
    FFormatSett: TFormatSettings;
    FFilename: TFileName;
    FFunctions: TSQLiteFunctions;
    FEncoding: TSQLiteDBEncoding;
    FConnected: Boolean;
    FOnAuthorize: TAuthEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnUpdate: TUpdateHookEvent;
    FExtEnabled: Boolean;
    FReadUncommitted: Boolean;
    procedure RaiseError(const s: string; const SQL: string); overload;
    procedure RaiseError(const s: string); overload;
    procedure SetParams(Stmt: TSQLiteStmt);
    function GetRowsChanged: integer;
    class procedure InitDefaultColumnTypes;
    procedure SetFilename(const Value: TFileName);

    procedure SetConnected(const Value: Boolean);

    procedure SetOnAuthorize(const Value: TAuthEvent);

    procedure SetOnUpdate(const Value: TUpdateHookEvent);

    procedure SetExtEnabled(const Value: Boolean);

    procedure SetReadUncommitted(const Value: Boolean);
  protected
    procedure SetSynchronised(Value: boolean);
    procedure DoQuery(const value: string);
    procedure SetEvents();
    class procedure EnsureInitialized; static;
  public
    {$REGION 'Doc'}
      /// <summary>
      /// Creates (if file doesn't exist) or loads database from given file using default encoding
      /// </summary>
      /// <param name="AFileName">filename of sqlite database</param>
      /// <param name="DefaultEncoding">encoding to use when creating new database</param>
      /// <param name="Password">Password to encrypt database. Notes: default sqlite.dll does not support database encryption.
      /// To encrypt your database you must use http://system.data.sqlite.org/index.html/doc/trunk/www/index.wiki or
      /// http://sourceforge.net/projects/wxcode/files/Components/wxSQLite3/ library file (rename it to sqlite3.dll) </param>
    {$ENDREGION}
    constructor Create(); reintroduce; overload;
    constructor Create(const AFileName: string; DefaultEncoding: TSQLiteDBEncoding;
      const Password: AnsiString; AOwner: TComponent = nil); reintroduce; overload;
    constructor Create(const AFileName: string;
      DefaultEncoding: TSQLiteDBEncoding = seUTF8); reintroduce; overload;
    constructor Create(AOwner: TComponent); reintroduce; overload;
    destructor Destroy; override;
    //service methods
    procedure Analyze;
    procedure Reindex;
    procedure Vacuum;
    {$REGION 'Doc'}
       /// <summary>
       /// Adds new supported column type
       /// </summary>
       /// <param name="ColTypeName">Column Type Name</param>
       /// <param name="ColType">Column Type constant, e.g. dtStr</param>
    {$ENDREGION}
    class procedure AddNewSupportedColumnType(const ColTypeName: string; ColType: Integer = dtStr);
    {$REGION 'Doc'}
      /// <summary>
      /// Executes sql statement and returns table with results
      /// </summary>
      /// <param name="SQL">SQL statement which should return resultset</param>
      /// <returns>TSQLiteTable instance</returns>
    {$ENDREGION}
    function GetTable(const SQL: String): TSQLiteTable;
    /// <summary>
    /// Executes SQL statement
    /// </summary>
    /// <param name="SQL">SQL statement which should not return resultset</param>
    procedure ExecSQL(const SQL: String); overload;
    procedure ExecSQL(const SQL: String; var RowsAffected: Integer); overload;
    procedure ExecSQL(const SQL: String; const AParams: array of const); overload;
    procedure ExecSQL(const SQL: String; const AParams: array of const; var RowsAffected: Integer); overload;
    procedure ExecSQL(Query: TSQLiteQuery); overload; deprecated;

    function PrepareSQL(const SQL: String): TSQLiteQuery;
    procedure BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: Integer); overload;
    procedure BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: String); overload;
    procedure ReleaseSQL(Query: TSQLiteQuery);
    /// <summary>
    /// Gets very fast unidirectional resultset
    /// </summary>
    /// <param name="SQL">SQL statement</param>
    /// <returns>TSQLiteUniTable</returns>
    function GetUniTable(const SQL: String): TSQLiteUniTable; overload;
    function GetUniTable(const SQL: String; const AParams: array of const): TSQLiteUniTable; overload;
     /// <summary>
    /// Gets very fast unidirectional resultset as an interface
    /// </summary>
    /// <param name="SQL">SQL statement</param>
    /// <returns>ISQLiteTable interface</returns>
    function GetUniTableIntf(const SQL: string): ISQLiteTable; overload;
    function GetUniTableIntf(const SQL: string; const AParams: array of const): ISQLiteTable; overload;

    function GetTableValue(const SQL: String): int64;
    function GetTableString(const SQL: String): string;
    procedure GetTableStrings(const SQL: String; const Value: TStrings);
{$IFNDEF NEXTGEN}
    procedure GetTableColumnMetadata(const TableName, ColumnName: AnsiString; var Datatype, CollSeq: AnsiString;
      var bNotNull, bPrimKey, bAutoinc: Boolean; const DbName: AnsiString = '');
{$ENDIF}
    function GetPreparedStatement(const SQL: string): TSQLitePreparedStatement; overload;
    {$REGION 'Doc'}
      /// <summary>
      /// returns newly created prepared statment
      /// </summary>
      /// <param name="SQL">SQL statement string</param>
      /// <param name="Params">Parameter values for prepared query</param>
      /// <returns>TSQLitePreparedStatement instance</returns>
    {$ENDREGION}
    function GetPreparedStatement(const SQL: string; const Params: array of TVarRec): TSQLitePreparedStatement; overload;
    function GetPreparedStatementIntf(const SQL: string): ISQLitePreparedStatement; overload;
    function GetPreparedStatementIntf(const SQL: string; const Params: array of TVarRec): ISQLitePreparedStatement; overload;
    {$REGION 'Doc'}
      /// <summary>
      /// Update blob value. Expects SQL of the form 'UPDATE MYTABLE SET MYFIELD = ? WHERE MYKEY = 1'
      /// </summary>
      /// <param name="SQL">update statement</param>
      /// <param name="BlobData">BlobData stream</param>
    {$ENDREGION}
    procedure UpdateBlob(const SQL: String; BlobData: TStream); deprecated 'Use prepared statements to add blobs as parameters';
    /// <summary>
    /// Starts new transaction. Speeds up drastically data inserts, updates, deletes.
    /// </summary>
    procedure BeginTransaction;
    /// <summary>
    /// Commits transaction changes to database
    /// </summary>
    procedure Commit;
    /// <summary>
    /// Rollbacks transaction changes to database
    /// </summary>
    procedure Rollback;
    /// <summary>
    /// Checks if given table exists in the database
    /// </summary>
    /// <param name="TableName">Table name</param>
    /// <returns>Boolean</returns>
    function TableExists(const TableName: string): boolean;
    /// <summary>
    /// Returns last inserted rows ID (for autoincrement fields it is that field value)
    /// </summary>
    /// <returns></returns>
    function GetLastInsertRowID: int64;
    /// <summary>
    /// How many rows changes since the last database change
    /// </summary>
    /// <returns>Number of rows Integer</returns>
    function GetLastChangedRows: Integer;
    /// <summary>
    /// Returns sqlite memory usage
    /// </summary>
    /// <returns>Memory used in bytes</returns>
    function GetMemoryUsed: Int64;
    /// <summary>
    /// Change password for encrypted database. Empty password decrypts database.
    /// </summary>
    /// <param name="NewPassword">New password: Ansistring</param>
    procedure ChangePassword(const NewPassword: AnsiString);

    procedure Open(const Password: AnsiString{$IFNDEF NEXTGEN} = ''{$ENDIF});
    procedure Close;
    /// <summary>
    /// This routine sets a busy handler that sleeps for a specified amount of time when a table is locked.
    /// The handler will sleep multiple times until at least "ms" milliseconds of sleeping have accumulated.
    /// </summary>
    /// <param name="Value">Time is miliseconds</param>
    procedure SetTimeout(Value: integer);
    function Backup(const TargetDB: TSQLiteDatabase): integer; Overload;
    function Backup(const TargetDB: TSQLiteDatabase; const targetName: String; const sourceName: String): integer; Overload;
    function Version: string;
    procedure LoadExtension(const AFilename: string);
    procedure AddCustomCollate(const name: string; xCompare: TCollateXCompare);
    //adds collate named SYSTEM for correct data sorting by user's locale
    Procedure AddSystemCollate;
    procedure ParamsClear;
    procedure AddParamInt(const name: string; value: int64); deprecated 'Use prepared statements to add parameters';
    procedure AddParamFloat(const name: string; value: double); deprecated 'Use prepared statements to add parameters';
    procedure AddParamText(const name: string; const value: string); deprecated 'Use prepared statements to add parameters';
    procedure AddParamNull(const name: string); deprecated 'Use prepared statements to add parameters';
    {$REGION 'Doc'}
      /// <summary>
      /// Attached another database into the current one with the given alias (name)
      /// </summary>
      /// <param name="DBFilename">filename of the database to attach</param>
      /// <param name="AttachedDBName">name of the attached database</param>
      /// <returns>true; attached succesfully</returns>
    {$ENDREGION}
    function Attach(const DBFilename: string; const AttachedDBName: string): Boolean;
    /// <summary>
    /// Format settings to use for DateTime fields
    /// </summary>
    property FmtSett: TFormatSettings read FFormatSett;
    property DB: TSQLiteDB read fDB;
  published
    property Connected: Boolean read FConnected write SetConnected default False;
    property IsTransactionOpen: Boolean read fInTrans;
    property Encoding: TSQLiteDBEncoding read FEncoding write FEncoding default seUTF8;
    property ExtensionsEnabled: Boolean read FExtEnabled write SetExtEnabled default False;
    /// <summary>
    /// Database filename. Opens the database when filename is being set
    /// </summary>
    property Filename: TFileName read FFilename write SetFilename;
    property Functions: TSQLiteFunctions read FFunctions;
    property ReadUncommitted: Boolean read FReadUncommitted write SetReadUncommitted default False;
     ///	<summary>
    ///	  Database rows that were changed (or inserted or deleted) by the most
    ///	  recent SQL statement
    ///	</summary>
    property RowsChanged : Integer read getRowsChanged;
    property Synchronised: Boolean read FSync write SetSynchronised default False;


    //events
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnQuery: THookQuery read FOnQuery write FOnQuery;
    /// <summary>
    /// Assign authorization callback.
    /// AResult should return one of the values: SQLITE_OK, SQLITE_DENY, SQLITE_IGNORE.
    /// </summary>
    property OnAuthorize: TAuthEvent read FOnAuthorize write SetOnAuthorize;
    property OnUpdate: TUpdateHookEvent read FOnUpdate write SetOnUpdate;
  end;

  /// <summary>
  /// SQLite table which holds all the data in memory
  /// </summary>
  TSQLiteTable = class
  private
    fResults: TList<Pointer>;
    fRowCount: cardinal;
    fColCount: cardinal;
    fCols: TStringList;
    fColTypes: TList<Pointer>;
    fRow: cardinal;
    function GetFields(I: cardinal): string;
    function GetEOF: boolean;
    function GetBOF: boolean;
    function GetColumns(I: integer): string;
    function GetFieldValByName(const FieldName: string): string;
    function GetFieldIndex(const FieldName: string): integer;
    function GetCount: integer;
    function GetCountResult: integer;
    function GetIsLastRow: boolean;
  public
    constructor Create(const DB: TSQLiteDatabase; const SQL: String);
    destructor Destroy; override;

    function FieldAsInteger(I: cardinal): int64;
    function FieldAsBlob(I: cardinal): TMemoryStream;
    function FieldAsBlobText(I: cardinal): string;
    function FieldIsNull(I: cardinal): boolean;
    function FieldAsString(I: cardinal): string;
    function FieldAsDouble(I: cardinal): double;
    function Next: boolean;
    function Previous: boolean;
    property EOF: boolean read GetEOF;
    property BOF: boolean read GetBOF;
    property IsLastRow: boolean read GetIsLastRow;
    property Fields[I: cardinal]: string read GetFields;
    property FieldValByNameAsString[const FieldName: string]: string read GetFieldValByName;
    property FieldIndex[const FieldName: string]: integer read GetFieldIndex;
    property Columns[I: integer]: string read GetColumns;
    property ColCount: cardinal read fColCount;
    property RowCount: cardinal read fRowCount;
    property Row: cardinal read fRow;
    function MoveFirst: boolean;
    function MoveLast: boolean;
    function MoveTo(position: cardinal): boolean;
    property Count: integer read GetCount;
    // The property CountResult is used when you execute count(*) queries.
    // It returns 0 if the result set is empty or the value of the
    // first field as an integer.
    property CountResult: integer read GetCountResult;
  end;
  /// <summary>
  /// SQLite prepared statement interface - Linas Naginionis
  /// </summary>
  ISQLitePreparedStatement = interface
    function GetParams(index: Integer): TSQliteParam;
    function GetParamCount: Integer;

    procedure ClearParams;

    procedure SetParamInt(const name: string; value: int64); overload;
    procedure SetParamFloat(const name: string; value: double); overload;
    procedure SetParamText(const name: string; const value: string); overload;
    procedure SetParamNull(const name: string); overload;
    procedure SetParamInt(const I: Integer; value: int64); overload;
    procedure SetParamFloat(const I: Integer; value: double); overload;
    procedure SetParamText(const I: Integer; const value: string); overload;
    procedure SetParamNull(const I: Integer); overload;
    procedure SetParamDateTime(const I: Integer; const Value: TDateTime); overload;
    procedure SetParamDateTime(const name: string; const Value: TDateTime); overload;
    procedure SetParamDate(const I: Integer; const Value: TDate); overload;
    procedure SetParamDate(const name: string; const Value: TDate); overload;
    procedure SetParamTime(const I: Integer; const Value: TTime); overload;
    procedure SetParamTime(const name: string; const Value: TTime); overload;
    procedure SetParamVariant(const I: Integer; const Value: Variant); overload;
    procedure SetParamVariant(const name: string; const Value: Variant); overload;
    procedure SetParamBlob(const I: Integer; const Value: TStream); overload;
    procedure SetParamBlob(const name: string; const Value: TStream); overload;
    /// <summary>
    /// Executes query which returns unidirectional table
    /// </summary>
    /// <returns>TSQLiteUniTable instance</returns>
    function ExecQuery(): TSQLiteUniTable; overload;
    function ExecQuery(const SQL: string): TSQLiteUniTable; overload;
    function ExecQuery(const SQL: string; const Params: array of TVarRec): TSQLiteUniTable; overload;

    function ExecQueryIntf(): ISQLiteTable; overload;
    function ExecQueryIntf(const SQL: string): ISQLiteTable; overload;
    function ExecQueryIntf(const SQL: string; const Params: array of TVarRec): ISQLiteTable; overload;
    /// <summary>
    /// Executes SQL statement which should not return resultset
    /// </summary>
    /// <returns></returns>
    function ExecSQL(): Boolean; overload;
    function ExecSQL(const SQL: string): Boolean; overload;
    function ExecSQL(var RowsAffected: Integer): Boolean; overload;
    function ExecSQL(const SQL: string; var RowsAffected: Integer): Boolean; overload;
    function ExecSQL(const SQL: string; const Params: array of TVarRec): Boolean; overload;
    /// <summary>
    /// Prepares new SQL statement. Useful when using same TSQLitePreparedStatement with different queries
    /// </summary>
    /// <param name="SQL">SQL statement</param>
    procedure PrepareStatement(const SQL: string = ''); overload;
    procedure PrepareStatement(const SQL: string; const Params: array of TVarRec); overload;

    property Params[index: Integer]: TSQliteParam read GetParams;
    property ParamCount: Integer read GetParamCount;
    function BindParameterCount: Integer;
    function BindParameterName(const i: Integer): string;
  end;
  /// <summary>
  /// SQLite prepared statement - Linas Naginionis
  /// </summary>
  TSQLitePreparedStatement = class(TInterfacedObject, ISQLitePreparedStatement)
  private
    FSQL: string;
    {$IFDEF WEAKREF}[Unsafe]{$ENDIF}
    FDB: TSQLiteDatabase;
    fParams: TObjectList<TSQliteParam>;
    FStmt: TSQLiteStmt;
    FParamsBound: Boolean;
    FOwnedStmt: Boolean; //true = prepared statement should free sqlite stmt, false = table responsible for freeing sqlite stmt
    procedure BindParams;
    procedure SetParams(const Params: array of TVarRec);
   // function FindParam(const I: Integer): TSQliteParam; overload;
   // function FindParam(const name: string): TSQliteParam; overload;
    procedure SetSQL(const Value: string);
    function GetParamCount: Integer;
    function GetParams(index: Integer): TSQliteParam;
  protected
    procedure SetParamBlobInternal(const Value: TStream; const I: Integer = 0; const Name: string = '');
    procedure SetParamIntInternal(value: int64; const I: Integer = 0; const name: string = '');
    procedure SetParamFloatInternal(value: double; const I: Integer = 0; const name: string = '');
    procedure SetParamTextInternal(const value: string; const I: Integer = 0; const name: string = '');
    procedure SetParamNullInternal(const I: Integer = 0; const name: string = '');
    procedure SetParamVariantInternal(const Value: Variant; const I: Integer = 0; const name: string = '');
  public
    constructor Create(const DB: TSQLiteDatabase); overload;
    constructor Create(const DB: TSQLiteDatabase; const SQL: string); overload;
    constructor Create(const DB: TSQLiteDatabase; const SQL: string; const Params: array of TVarRec); overload;
    destructor Destroy; override;

    procedure ClearParams;

    procedure SetParamInt(const I: Integer; value: int64); overload;
    procedure SetParamInt(const name: string; value: int64); overload;
    procedure SetParamFloat(const I: Integer; value: double); overload;
    procedure SetParamFloat(const name: string; value: double); overload;
    procedure SetParamText(const name: string; const value: string); overload;
    procedure SetParamText(const I: Integer; const value: string); overload;
    procedure SetParamNull(const name: string); overload;
    procedure SetParamNull(const I: Integer); overload;
    procedure SetParamDateTime(const I: Integer; const Value: TDateTime); overload;
    procedure SetParamDateTime(const name: string; const Value: TDateTime); overload;
    procedure SetParamDate(const I: Integer; const Value: TDate); overload;
    procedure SetParamDate(const name: string; const Value: TDate); overload;
    procedure SetParamTime(const I: Integer; const Value: TTime); overload;
    procedure SetParamTime(const name: string; const Value: TTime); overload;
    procedure SetParamVariant(const I: Integer; const Value: Variant); overload;
    procedure SetParamVariant(const name: string; const Value: Variant); overload;
    procedure SetParamBlob(const I: Integer; const Value: TStream); overload;
    procedure SetParamBlob(const name: string; const Value: TStream); overload;
    {$IFDEF DELPHI14_UP}
    /// <summary>
    /// Executes previously set SQL statement and fetches results into given class type
    /// </summary>
    function ExecSQLAndMapData<T: constructor, class>(var DataList: TObjectList<T>): Boolean;
    {$ENDIF}
    /// <summary>
    /// Executes query which returns unidirectional table
    /// </summary>
    /// <returns>TSQLiteUniTable instance</returns>
    function ExecQuery(): TSQLiteUniTable; overload;
    function ExecQuery(const SQL: string): TSQLiteUniTable; overload;
    function ExecQuery(const SQL: string; const Params: array of TVarRec): TSQLiteUniTable; overload;

    function ExecQueryIntf(): ISQLiteTable; overload;
    function ExecQueryIntf(const SQL: string): ISQLiteTable; overload;
    function ExecQueryIntf(const SQL: string; const Params: array of TVarRec): ISQLiteTable; overload;
    /// <summary>
    /// Executes SQL statement which should not return resultset
    /// </summary>
    /// <returns></returns>
    function ExecSQL(): Boolean; overload;
    function ExecSQL(const SQL: string): Boolean; overload;
    function ExecSQL(var RowsAffected: Integer): Boolean; overload;
    function ExecSQL(const SQL: string; var RowsAffected: Integer): Boolean; overload;
    function ExecSQL(const SQL: string; const Params: array of TVarRec): Boolean; overload;
    /// <summary>
    /// Prepares new SQL statement. Useful when using same TSQLitePreparedStatement with different queries
    /// </summary>
    /// <param name="SQL">SQL statement</param>
    procedure PrepareStatement(const SQL: string = ''); overload;
    procedure PrepareStatement(const SQL: string; const Params: array of TVarRec); overload;

    property DB: TSQLiteDatabase read FDB;
    property Stmt: TSQLiteStmt read FStmt;
    property SQL: string read FSQL write SetSQL;
    property Params[index: Integer]: TSQliteParam read GetParams;
    function BindParameterCount: Integer;
    function BindParameterName(const i: Integer): string;
    property ParamCount: Integer read GetParamCount;
    property ParamsBound: Boolean read FParamsBound;
  end;

  /// <summary>
  /// Class responsible for getting data from SQLite columns
  /// </summary>
  TSQLiteField = class
  private
    {$IFDEF WEAKREF}[Unsafe]{$ENDIF}
    Table: TSQLiteUniTable;
  public
    Index: Integer;
    Name: string;
    FieldType: Integer;

    constructor Create(); virtual;
    destructor Destroy; override;

    function IsNull: Boolean;

    function AsBlob: TMemoryStream;
    function AsBlobPtr(out iNumBytes: integer): Pointer;
    function AsBlobText: string;
    function AsBlobTextDef(const Def: string = ''): string;
    function AsDateTime: TDateTime;  //datetime support slows down data retrieval
    function AsDateTimeDef(const Def: TDateTime = 0.0): TDateTime;
    function AsDouble: Double;
    function AsDoubleDef(const Def: Double = 0.0): Double;
    function AsInteger: Int64;
    function AsIntegerDef(const Def: Int64 = 0): Int64;
    function AsString: string;
    function AsStringDef(const Def: string = ''): string;
    function Value: Variant;
    function ValueDef(const Def: Variant): Variant;
  end;

  /// <summary>
  /// TSQLiteUniTable enumerator
  ///  for ARec in Dataset do...
  /// </summary>
  TUniTableEnumerator = class
  private
    FMoveToFirst: Boolean;
    {$IFDEF WEAKREF}[Unsafe]{$ENDIF}
    FDataSet: TSQLiteUniTable;
    function GetCurrent: Variant;
  public
    constructor Create(const ATable: TSQLiteUniTable);
    destructor Destroy; override;
    function MoveNext: Boolean;
    property Current: Variant read GetCurrent;
  end;

  /// <summary>
  /// Interface of SQLite table
  /// </summary>
  ISQLiteTable = interface
    function GetFieldCount: Cardinal;
    function GetRow: Cardinal;
    function GetEOF: Boolean;
    function GetFieldsAsString(I: Cardinal): string;
    function GetField(I: Integer): TSQLiteField;
    function GetFieldsVal(I: Cardinal): Variant;
    function GetColumns(I: integer): string;
    function GetFieldByNameAsString(const FieldName: string): string;
    function GetFieldIndex(const FieldName: string): integer;
    function GetFieldByName(const FieldName: string): TSQLiteField;
    function GetEnumerator: TUniTableEnumerator;
    function FindField(const AFieldName: string): TSQLiteField;
    function FieldAsInteger(I: Cardinal): int64;
    function FieldAsBlob(I: Cardinal): TMemoryStream;
    function FieldAsBlobPtr(I: Cardinal; out iNumBytes: integer): Pointer;
    function FieldAsBlobText(I: Cardinal): string;
    function FieldIsNull(I: Cardinal): Boolean;
    function FieldAsString(I: Cardinal): string;
    function FieldAsDouble(I: Cardinal): double;
    function Next: Boolean;

    property EOF: Boolean read GetEOF;
    property FieldCount: Cardinal read GetFieldCount;
    property FieldsAsString[I: Cardinal]: string read GetFieldsAsString;
    property Fields[I: Integer]: TSQLiteField read GetField;
    property FieldsVal[I: Cardinal]: Variant read GetFieldsVal;
    property FieldByName[const FieldName: string]: TSQLiteField read GetFieldByName; default;
    property FieldByNameAsString[const FieldName: string]: string read GetFieldByNameAsString;
    property FieldIndex[const FieldName: string]: integer read GetFieldIndex;
    property Columns[I: integer]: string read GetColumns;
    property ColCount: Cardinal read GetFieldCount;
    property Row: Cardinal read GetRow;
  end;

  /// <summary>
  /// Unidirectional SQLite table for fastest data access and very small memory footprint
  /// </summary>
  TSQLiteUniTable = class(TInterfacedObject, ISQLiteTable)
  private
    fColCount: Cardinal;
    fCols: TDictionary<string,Integer>;
    FColTypes: array of Integer;
    FColNames: array of string;
    FFields: TObjectList<TSQLiteField>;
    fRow: Cardinal;
    fEOF: Boolean;
    fStmt: TSQLiteStmt;
    {$IFDEF WEAKREF}[Unsafe]{$ENDIF}
    fDB: TSQLiteDatabase;
    fSQL: string;
    function GetFieldsAsString(I: Cardinal): string;
    function GetColumns(I: integer): string;
    function GetFieldByNameAsString(const FieldName: string): string;
    function GetFieldIndex(const FieldName: string): integer;
    function GetFieldByName(const FieldName: string): TSQLiteField;
    procedure GetDataTypes;
    function GetField(I: Integer): TSQLiteField;
    function GetFieldCount: Cardinal;
    function GetRow: Cardinal;
    function GetEOF: Boolean;
    function GetCurrentRec: Variant;
  protected
    function GetFieldsVal(I: Cardinal): Variant; virtual;
  public
    constructor Create(const DB: TSQLiteDatabase); overload; //use with caution!
    constructor Create(const DB: TSQLiteDatabase; hStmt: TSQLiteStmt); overload;
    constructor Create(const DB: TSQLiteDatabase; const SQL: string); overload;
    destructor Destroy; override;
    function GetEnumerator: TUniTableEnumerator;
    property CurrentRec: Variant read GetCurrentRec;
    /// <summary>
    /// Checks if fieldname exists
    /// </summary>
    /// <param name="AFieldName">name of the field</param>
    /// <returns>if field exists, returns TSQLiteField instance, else returns nil</returns>
    function FindField(const AFieldName: string): TSQLiteField;
    function FieldAsInteger(I: Cardinal): int64;
    function FieldAsBlob(I: Cardinal): TMemoryStream;
    function FieldAsBlobPtr(I: Cardinal; out iNumBytes: integer): Pointer;
    function FieldAsBlobText(I: Cardinal): string;
    function FieldAsBlobVariant(I: Cardinal): Variant;
    function FieldIsNull(I: Cardinal): Boolean;
    function FieldAsString(I: Cardinal): string;
    function FieldAsDouble(I: Cardinal): double;
    function Next: Boolean;

    property EOF: Boolean read GetEOF;
    property FieldCount: Cardinal read GetFieldCount;
    property FieldsAsString[I: Cardinal]: string read GetFieldsAsString;
    property Fields[I: Integer]: TSQLiteField read GetField;
    property FieldsVal[I: Cardinal]: Variant read GetFieldsVal;
    property FieldByName[const FieldName: string]: TSQLiteField read GetFieldByName; default;
    property FieldByNameAsString[const FieldName: string]: string read GetFieldByNameAsString;
    property FieldIndex[const FieldName: string]: integer read GetFieldIndex;
    property Columns[I: integer]: string read GetColumns;
    property ColCount: Cardinal read GetFieldCount;
    property Row: Cardinal read GetRow;
    property SQL: string read fSQL write fSQL;
    property Stmt: TSQLiteStmt read FStmt;
  end;

procedure DisposePointer(ptr: pointer); cdecl;

{$IFDEF WIN32}
function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;
{$ENDIF}
function SQLiteDataTypeToDelphiFieldType(SQLiteDataType: Integer): TFieldType;

implementation

uses
  Variants,
  Math,
  {$IFDEF DELPHI14_UP}
  Rtti,
  {$ENDIF}
  TypInfo;

type
  { A custom variant type that implements the mapping from the property names
    to the DataSet fields. }
  TVarDataRecordType = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string; const Value: TVarData): Boolean; override;
  end;

type
  { Our layout of the variants record data.
    We only hold a pointer to the DataSet. }
  TVarDataRecordData = record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    DataSet: TSQLiteUniTable;
    Reserved4: Pointer;
  end;

var
  { The global instance of the custom variant type. The data of the custom
    variant is stored in a TVarData record, but the methods and properties
    are implemented in this class instance. }
  VarDataRecordType: TVarDataRecordType = nil;

{ A global function the get our custom VarType value. This may vary and thus
  is determined at runtime. }
function VarDataRecord: TVarType;
begin
  result := VarDataRecordType.VarType;
end;

{ A global function that fills the VarData fields with the correct values. }
function VarDataRecordCreate(ADataSet: TSQLiteUniTable): Variant;
begin
  VarClear(result);
  TVarDataRecordData(result).VType := VarDataRecord;
  TVarDataRecordData(result).DataSet := ADataSet;
end;

procedure TVarDataRecordType.Clear(var V: TVarData);
begin
  { No fancy things to do here, we are only holding a pointer to a TDataSet and
    we are not supposed to destroy it here. }
  SimplisticClear(V);
end;

procedure TVarDataRecordType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  { No fancy things to do here, we are only holding a pointer to a TDataSet and
    that can simply be copied here. }
  SimplisticCopy(Dest, Source, Indirect);
end;

function TVarDataRecordType.GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean;
var
  fld: TSQLiteField;
begin
  { Find a field with the property's name. If there is one, return its current value. }
  fld := TVarDataRecordData(V).DataSet.FieldByName[Name];
  result := (fld <> nil);
  if result then
    Variant(dest) := fld.Value;
end;

function TVarDataRecordType.SetProperty(const V: TVarData; const Name: string; const Value: TVarData): Boolean;
{var
  fld: TField;}
begin
  Result := False;
  Assert(False, 'Cannot set field value for unidirectional table');
  { Find a field with the property's name. If there is one, set its value. }
 // fld := TVarDataRecordData(V).DataSet.FieldByName[Name];
 { result := (fld <> nil);
  if result then begin
    TVarDataRecordData(V).DataSet.Edit;
    fld.Value := Variant(Value);
  end;}
end;

const
  //default supported column types defined in sqlite db
  // each other type which isn't supported is bound as text column
  DEF_COLCOUNT = 10;
  DEF_COLTYPES_NAMES : array[0..DEF_COLCOUNT-1] of string =
    ('SMALLINT', 'INTEGER', 'BOOLEAN', 'NUMERIC', 'FLOAT', 'DOUBLE', 'REAL',
     'DATETIME', 'DATE', 'BLOB'
    );
  DEF_COLTYPES : array[0..DEF_COLCOUNT-1] of Integer =
    (dtInt, dtInt, dtInt, dtNumeric, dtNumeric, dtNumeric, dtNumeric,
     dtDateTime, dtDate, dtBlob
    );

procedure DisposePointer(ptr: pointer); cdecl;
begin
  if Assigned(ptr) then
    FreeMem(ptr);
end;


procedure AggFinalDefFunc(sqlite3_context: Psqlite3_context); cdecl;
var
  Funcs: PSQLiteFuncs;
  UserFunc: TSQLiteUserFuncFinal;
begin
  Funcs := PSQLiteFuncs(sqlite3_user_data(sqlite3_context));
  if Assigned(Funcs) then
  begin
    UserFunc := Funcs.AFinalFunc;
    if Assigned(UserFunc) then
      UserFunc(sqlite3_context);
  end;
end;

procedure AggStepDefFunc(sqlite3_context: Psqlite3_context; cArg: integer; ArgV: PPsqlite3_value); cdecl;
var
  Funcs: PSQLiteFuncs;
  UserFunc: TSQLiteUserFunc;
begin
  Funcs := PSQLiteFuncs(sqlite3_user_data(sqlite3_context));
  if Assigned(Funcs) then
  begin
    UserFunc := Funcs.AStepFunc;
    if Assigned(UserFunc) then
      UserFunc(sqlite3_context, cArg, ArgV);
  end;
end;

procedure ScalarDefFunc(sqlite3_context: Psqlite3_context; cArg: integer; ArgV: PPsqlite3_value); cdecl;
var
  Funcs: PSQLiteFuncs;
  UserFunc: TSQLiteUserFunc;
begin
  Funcs := PSQLiteFuncs(sqlite3_user_data(sqlite3_context));
  if Assigned(Funcs) then
  begin
    case Funcs.FuncType of
      sftScalar:
      begin
        UserFunc := Funcs.AFunc;
        if Assigned(UserFunc) then
          UserFunc(sqlite3_context, cArg, ArgV);
      end;
      sftAggregate: 
      begin
        UserFunc := Funcs.AFunc;
        if Assigned(UserFunc) then
          UserFunc(sqlite3_context, cArg, ArgV);
      end;
    end;
  end;
end;

function DoAuthorize(pUserData: Pointer; ActionCode: Integer; Det1, Det2, Det3, Det4: PAnsiChar): Integer; cdecl;
var
  DB: TSQLiteDatabase;
  iRes: Integer;
begin
  Result := SQLITE_OK;
  DB := TSQLiteDatabase(pUserData);

  if Assigned(DB.FOnAuthorize) then
  begin
    iRes := SQLITE_OK;

    DB.FOnAuthorize(DB, TSQLiteActionCode(ActionCode), UTF8ToString(Det1), UTF8ToString(Det2),
      UTF8ToString(Det3), UTF8ToString(Det4), iRes);

    Result := iRes;

  end;
end;

procedure DoUpdateHook(pUserData: Pointer; Operation: Integer; DbName: PAnsiChar;
    TableName: PAnsiChar; ARowID: Int64); cdecl;
var
  DB: TSQLiteDatabase;
begin
  DB := TSQLiteDatabase(pUserData);
  if Assigned(DB.FOnUpdate) then
  begin
    DB.FOnUpdate(DB, TSQLiteActionCode(Operation), UTF8ToString(DbName), UTF8ToString(TableName),
      ARowID);
  end;
end;




{$IFDEF WIN32}
function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(Buf1), Buf1Len,
    PWideChar(Buf2), Buf2Len) - 2;
end;
{$ENDIF}

function SQLiteDataTypeToDelphiFieldType(SQLiteDataType: Integer): TFieldType;
begin
  Result := ftString;
  case SQLiteDataType of
    dtInt: Result := ftInteger;
    dtNumeric: Result := ftFloat;
    dtStr: Result := ftWideString;
    dtBlob : Result := ftBlob;
    dtDate : Result := ftDate;
    dtDateTime: Result := ftDateTime;
  end;
end;

{$IFDEF USE_SYSTEM_SQLITE}
function Sqlite3_LastInsertRowID(db: TSQLiteDB): int64;
begin
  Result := sqlite3_last_insert_rowid(db);
end;

function SQLite3_TotalChanges(db: TSQLiteDB): integer;
begin
  Result := sqlite3_total_changes(db);
end;

procedure SQLite3_BusyTimeout(db: TSQLiteDB; TimeOut: integer);
begin
  sqlite3_busy_timeout(db, TimeOut);
end;

function SQLite3_Version: PAnsiChar;
begin
  Result := sqlite3_libversion;
end;

function Sqlite3_ColumnText(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar;
begin
  Result := sqlite3_column_text(hStmt, ColNum);
end;

function SQLite3_ColumnCount(hStmt: TSqliteStmt): integer;
begin
  Result := sqlite3_column_count(hStmt);
end;

function Sqlite3_ColumnName16(hStmt: TSqliteStmt; ColNum: integer): PWideChar;
begin
  Result := sqlite3_column_name16(hStmt, ColNum);
end;

function Sqlite3_ColumnDeclType16(hStmt: TSqliteStmt; ColNum: integer): PWideChar;
begin
  Result := sqlite3_column_decltype16(hStmt, ColNum);
end;

function Sqlite3_ColumnType(hStmt: TSqliteStmt; ColNum: integer): integer;
begin
  Result := sqlite3_column_type(hStmt, ColNum);
end;

function Sqlite3_ColumnInt64(hStmt: TSqliteStmt; ColNum: integer): Int64;
begin
  Result := sqlite3_column_int64(hStmt, ColNum);
end;

function Sqlite3_ColumnDouble(hStmt: TSqliteStmt; ColNum: integer): double;
begin
  Result := sqlite3_column_double(hStmt, ColNum);
end;

function Sqlite3_ColumnBytes(hStmt: TSqliteStmt; ColNum: integer): integer;
begin
  Result := sqlite3_column_bytes(hStmt, ColNum);
end;

function Sqlite3_ColumnBlob(hStmt: TSqliteStmt; ColNum: integer): pointer;
begin
  Result := sqlite3_column_blob(hStmt, ColNum);
end;

function Sqlite3_ColumnText16(hStmt: TSqliteStmt; ColNum: integer): PWideChar;
begin
  Result := sqlite3_column_text16(hStmt, ColNum);
end;

function SQLiteErrorStr(SQLiteErrorCode: Integer): String;
begin
  case SQLiteErrorCode of
    SQLITE_OK: Result := 'Successful result';
    SQLITE_ERROR: Result := 'SQL error or missing database';
    SQLITE_INTERNAL: Result := 'An internal logic error in SQLite';
    SQLITE_PERM: Result := 'Access permission denied';
    SQLITE_ABORT: Result := 'Callback routine requested an abort';
    SQLITE_BUSY: Result := 'The database file is locked';
    SQLITE_LOCKED: Result := 'A table in the database is locked';
    SQLITE_NOMEM: Result := 'A malloc() failed';
    SQLITE_READONLY: Result := 'Attempt to write a readonly database';
    SQLITE_INTERRUPT: Result := 'Operation terminated by sqlite3_interrupt()';
    SQLITE_IOERR: Result := 'Some kind of disk I/O error occurred';
    SQLITE_CORRUPT: Result := 'The database disk image is malformed';
    SQLITE_NOTFOUND: Result := '(Internal Only) Table or record not found';
    SQLITE_FULL: Result := 'Insertion failed because database is full';
    SQLITE_CANTOPEN: Result := 'Unable to open the database file';
    SQLITE_PROTOCOL: Result := 'Database lock protocol error';
    SQLITE_EMPTY: Result := 'Database is empty';
    SQLITE_SCHEMA: Result := 'The database schema changed';
    SQLITE_TOOBIG: Result := 'Too much data for one row of a table';
    SQLITE_CONSTRAINT: Result := 'Abort due to contraint violation';
    SQLITE_MISMATCH: Result := 'Data type mismatch';
    SQLITE_MISUSE: Result := 'Library used incorrectly';
    SQLITE_NOLFS: Result := 'Uses OS features not supported on host';
    SQLITE_AUTH: Result := 'Authorization denied';
    SQLITE_FORMAT: Result := 'Auxiliary database format error';
    SQLITE_RANGE: Result := '2nd parameter to sqlite3_bind out of range';
    SQLITE_NOTADB: Result := 'File opened that is not a database file';
    SQLITE_ROW: Result := 'sqlite3_step() has another row ready';
    SQLITE_DONE: Result := 'sqlite3_step() has finished executing';
  else
    Result := 'Unknown SQLite Error Code "' + IntToStr(SQLiteErrorCode) + '"';
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// TSQLiteDatabase
//------------------------------------------------------------------------------

constructor TSQLiteDatabase.Create(const AFileName: string; DefaultEncoding: TSQLiteDBEncoding;
  const Password: AnsiString; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnAuthorize := nil;
  fDB := nil;
  fParams := TObjectList<TSQliteParam>.Create;
  FFilename := AFileName;
  FFormatSett := TFormatSettings.Create();
  FFormatSett.ShortDateFormat := 'YYYY-MM-DD';
  FFormatSett.DateSeparator := '-';
  FFormatSett.LongDateFormat := FFormatSett.ShortDateFormat + ' HH:MM:SS';
  FFunctions := TSQLiteFunctions.Create(Self);
  self.fInTrans := False;
  FEncoding := DefaultEncoding;
  FOnAfterOpen := nil;
  FOnAfterClose := nil;
  FOnUpdate := nil;
  FConnected := False;
  if FFilename <> '' then
    Open(Password);
end;

constructor TSQLiteDatabase.Create(AOwner: TComponent);
begin
  //inherited Create(AOwner);
  Create('');
end;

constructor TSQLiteDatabase.Create(const AFileName: string;
  DefaultEncoding: TSQLiteDBEncoding);
begin
  Create(AFileName, DefaultEncoding, '');
end;

constructor TSQLiteDatabase.Create;
begin
  Create('');
end;

destructor TSQLiteDatabase.Destroy;
begin
  if FConnected then
    Close;
  fParams.Free;
  FFunctions.Free;
  inherited Destroy;
end;

function TSQLiteDatabase.GetLastInsertRowID: int64;
begin
  Result := Sqlite3_LastInsertRowID(self.fDB);
end;

function TSQLiteDatabase.GetMemoryUsed: Int64;
begin
  Result := sqlite3_memory_used;
end;

function TSQLiteDatabase.GetPreparedStatement(const SQL: string;
  const Params: array of TVarRec): TSQLitePreparedStatement;
begin
  Result := TSQLitePreparedStatement.Create(Self, SQL, Params);
end;

function TSQLiteDatabase.GetPreparedStatementIntf(const SQL: string): ISQLitePreparedStatement;
begin
  Result := GetPreparedStatement(SQL);
end;

function TSQLiteDatabase.GetPreparedStatementIntf(const SQL: string;
  const Params: array of TVarRec): ISQLitePreparedStatement;
begin
  Result := GetPreparedStatement(SQL, Params);
end;

function TSQLiteDatabase.GetPreparedStatement(const SQL: string): TSQLitePreparedStatement;
begin
  Result := TSQLitePreparedStatement.Create(Self, SQL);

end;

function TSQLiteDatabase.GetLastChangedRows: Integer;
begin
  Result := SQLite3_TotalChanges(self.fDB);
end;

//..............................................................................

procedure TSQLiteDatabase.RaiseError(const s: string; const SQL: string);
//look up last error and raise an exception with an appropriate message
var
  Msg: PAnsiChar;
  ret : integer;
  excClass: ESQLiteExceptionClass;
begin
  Msg := nil;
  ret := SQLite3_ErrCode(fDB);

  if ret <> SQLITE_OK then
    Msg := sqlite3_errmsg(fDB);
  case ret of
    SQLITE_CONSTRAINT:
      excClass := ESqliteConstraintException;
    else
      excClass := ESqliteException;
  end;

  if Msg <> nil then
    raise excClass.CreateFmt(s +'.'#13'Error [%d]: %s.'#13'"%s": %s',
    [ret, SQLiteErrorStr(ret),SQL, Msg], ret)
  else
    raise excClass.CreateFmt(s, [SQL, 'No message'], ret);
end;

procedure TSQLiteDatabase.RaiseError(const s: string);
var
  Msg: PAnsiChar;
  ret : integer;
begin
  Msg := nil;
  ret := SQLite3_ErrCode(fDB);

  if ret <> SQLITE_OK then
    Msg := sqlite3_errmsg(fDB);

  if Msg <> nil then
    raise ESqliteException.CreateFmt(s +'.'#13'Error [%d]: %s.'#13'%s',
      [ret, SQLiteErrorStr(ret), Msg], ret)
  else
    raise ESqliteException.Create(s, ret);
end;

procedure TSQLiteDatabase.SetSynchronised(Value: boolean);
begin
  if Value <> fSync then
  begin
    if FConnected then
    begin
      if Value then
        ExecSQL('PRAGMA synchronous = ON;')
      else
        ExecSQL('PRAGMA synchronous = OFF;');
      fSync := Value;
    end;
  end;
end;

procedure TSQLiteDatabase.SetReadUncommitted(const Value: Boolean);
var
  sVal: string;
begin
  if Value <> FReadUncommitted then
  begin
    if FConnected then
    begin
      if Value then
        sVal := 'True'
      else
        sVal := 'False';

      ExecSQL(Format('PRAGMA read_uncommitted = %s',[sVal]));
      FReadUncommitted := Value;
    end;
  end;
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: String);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PChar;
  iStepResult: integer;
begin
  try
    if Sqlite3_Prepare16_v2(self.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <>
      SQLITE_OK then
      RaiseError('Error executing SQL', SQL);
    if (Stmt = nil) then
      RaiseError('Could not prepare SQL statement', SQL);
    DoQuery(SQL);
    SetParams(Stmt);

    iStepResult := Sqlite3_step(Stmt);
    if (iStepResult <> SQLITE_DONE) then
    begin
      SQLite3_reset(stmt);
      RaiseError('Error executing SQL statement', SQL);
    end;
  finally
    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
  end;
end;

class procedure TSQLiteDatabase.EnsureInitialized;
begin
{$IF Declared(sqlite3_initialize)}
  try
    if sqlite3_initialize <> SQLITE_OK then
      raise ESqliteException.Create('SQLite library cannot be initialized');
  except on EExternalException do
    raise ESqliteException.Create('SQLite library cannot be found');
  end;
{$IFEND}
end;

{$WARNINGS OFF}
procedure TSQLiteDatabase.ExecSQL(Query: TSQLiteQuery);
var
  iStepResult: integer;
begin
  if Assigned(Query.Statement) then
  begin
    DoQuery(Query.SQL);
    iStepResult := Sqlite3_step(Query.Statement);

    if (iStepResult <> SQLITE_DONE) then
    begin
      SQLite3_reset(Query.Statement);
      RaiseError('Error executing prepared SQL statement', Query.SQL);
    end;
    Sqlite3_Reset(Query.Statement);
  end;
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: String; const AParams: array of const);
var
  iRows: Integer;
begin
  ExecSQL(SQL, AParams, iRows);
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: String; const AParams: array of const; var RowsAffected: Integer);
begin
  if not (GetPreparedStatementIntf(SQL, AParams).ExecSQL(RowsAffected)) then
    RowsAffected := 0;
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: String; var RowsAffected: Integer);
begin
  ExecSQL(SQL, [], RowsAffected);
end;

{$WARNINGS ON}

{$WARNINGS OFF}
function TSQLiteDatabase.PrepareSQL(const SQL: String): TSQLiteQuery;
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PChar;
begin
  Result.SQL := SQL;
  Result.Statement := nil;

  if Sqlite3_Prepare16(self.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <>  SQLITE_OK then
    RaiseError('Error executing SQL', SQL)
  else
    Result.Statement := Stmt;

  if (Result.Statement = nil) then
    RaiseError('Could not prepare SQL statement', SQL);
  DoQuery(SQL);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSQLiteDatabase.BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: Integer);
begin
  if Assigned(Query.Statement) then
    sqlite3_Bind_Int(Query.Statement, Index, Value)
  else
    RaiseError('Could not bind integer to prepared SQL statement', Query.SQL);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSQLiteDatabase.BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: String);
begin
  if Assigned(Query.Statement) then
    Sqlite3_Bind_Text16(Query.Statement, Index, PChar(Value), Length(Value) * SizeOf(char), SQLITE_STATIC)
  else
    RaiseError('Could not bind string to prepared SQL statement', Query.SQL);
end;
{$WARNINGS ON}

{$WARNINGS OFF}

procedure TSQLiteDatabase.Reindex;
begin
  ExecSQL('REINDEX');
end;

procedure TSQLiteDatabase.ReleaseSQL(Query: TSQLiteQuery);
begin
  if Assigned(Query.Statement) then
  begin
    Sqlite3_Finalize(Query.Statement);
    Query.Statement := nil;
  end
  else
    RaiseError('Could not release prepared SQL statement', Query.SQL);
end;
{$WARNINGS ON}

procedure TSQLiteDatabase.UpdateBlob(const SQL: String; BlobData: TStream);
var
  iSize: integer;
  ptr: pointer;
  Stmt: TSQLiteStmt;
  Msg: PChar;
  NextSQLStatement: PChar;
  iStepResult: integer;
  iBindResult: integer;

begin
  //expects SQL of the form 'UPDATE MYTABLE SET MYFIELD = ? WHERE MYKEY = 1'
  if pos('?', SQL) = 0 then
    RaiseError('SQL must include a ? parameter', SQL);

  Msg := nil;
  try

    if Sqlite3_Prepare16_v2(self.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <>
      SQLITE_OK then
      RaiseError('Could not prepare SQL statement', SQL);

    if (Stmt = nil) then
      RaiseError('Could not prepare SQL statement', SQL);
    DoQuery(SQL);

    //now bind the blob data
    iSize := BlobData.size;

    GetMem(ptr, iSize);

    if (ptr = nil) then
      raise ESqliteException.CreateFmt('Error getting memory to save blob',
        [SQL, 'Error']);

    BlobData.position := 0;
    BlobData.Read(ptr^, iSize);

    iBindResult := SQLite3_Bind_Blob(stmt, 1, ptr, iSize, @DisposePointer);

    if iBindResult <> SQLITE_OK then
      RaiseError('Error binding blob to database', SQL);

    iStepResult := Sqlite3_step(Stmt);

    if (iStepResult <> SQLITE_DONE) then
    begin
      SQLite3_reset(stmt);
      RaiseError('Error executing SQL statement', SQL);
    end;

  finally

    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);

    if Assigned(Msg) then
      SQLite3_Free(Msg);
  end;

end;

//..............................................................................

function TSQLiteDatabase.GetTable(const SQL: String): TSQLiteTable;
begin
  Result := TSQLiteTable.Create(Self, SQL);
end;

{$IFNDEF NEXTGEN}

procedure TSQLiteDatabase.GetTableColumnMetadata(const TableName, ColumnName: AnsiString; var Datatype,
  CollSeq: AnsiString; var bNotNull, bPrimKey, bAutoinc: Boolean; const DbName: AnsiString);
var
  ADbName, ADataType, ACollSeq: PAnsiChar;
  ANotNull, APrimKey, AAutoinc: Integer;
begin
  if DbName = '' then
    ADbName := nil
  else
    ADbName := PAnsiChar(DbName);

  ADataType := '';
  ACollSeq := '';

  if (sqlite3_table_column_metadata(fDB, ADbName, PAnsiChar(TableName), PAnsiChar(ColumnName), ADatatype,
    ACollSeq, ANotNull, APrimKey, AAutoinc) <> SQLITE_OK) then
  begin
    RaiseError(Format('Cannot get table ([%s]) column ([%s]) metadata.', [TableName, ColumnName]), '');
  end;

  bNotNull := Boolean(ANotNull);
  bPrimKey := Boolean(APrimKey);
  bAutoinc := Boolean(AAutoinc);
  Datatype := ADataType;
  CollSeq := ACollSeq;
end;

{$ENDIF}

{$HINTS OFF}
function TSQLiteDatabase.GetUniTable(const SQL: string): TSQLiteUniTable;
begin
  Result := GetUniTable(SQL, []);
end;

function TSQLiteDatabase.GetUniTable(const SQL: String; const AParams: array of const): TSQLiteUniTable;
var
  stmt: TSQLitePreparedStatement;
begin
  Result := nil;
  stmt := GetPreparedStatement(SQL, AParams);
  try
    Result := stmt.ExecQuery(SQL);
  finally
    stmt.Free;
  end;
end;

function TSQLiteDatabase.GetUniTableIntf(const SQL: string; const AParams: array of const): ISQLiteTable;
var
  stmt: ISQLitePreparedStatement;
begin
  Result := nil;
  stmt := GetPreparedStatementIntf(SQL, AParams);
  Result := stmt.ExecQueryIntf(SQL);
end;

function TSQLiteDatabase.GetUniTableIntf(const SQL: string): ISQLiteTable;
begin
  Result := GetUniTableIntf(SQL, []);
end;

{$HINTS ON}

class procedure TSQLiteDatabase.InitDefaultColumnTypes;
var
  i: Integer;
begin
  TSQLiteDatabase.FColumnTypes.Clear;

  for i := Low(DEF_COLTYPES_NAMES) to High(DEF_COLTYPES_NAMES) do
  begin
    TSQLiteDatabase.FColumnTypes.Add(DEF_COLTYPES_NAMES[i], DEF_COLTYPES[i]);
  end;
end;

procedure TSQLiteDatabase.LoadExtension(const AFilename: string);
var
{$IFNDEF USE_SYSTEM_SQLITE}
  zErr: PAnsiChar;
{$ELSE}
  zErr: Pointer;
{$ENDIF}
  lFileName: PAnsiChar;
{$IFDEF NEXTGEN}
  M: TMarshaller;
{$ENDIF}
begin
  zErr := nil;
{$IFNDEF NEXTGEN}
  lFileName := PAnsiChar(UTF8Encode(AFilename));
{$ELSE}
  lFileName := M.AsUtf8(AFileName).ToPointer;
{$ENDIF}
  if (sqlite3_load_extension(fDB, lFileName, '0', zErr) <> SQLITE_OK) then
  begin
    RaiseError('Cannot load extension: ' + AFilename);
  end;
end;

procedure TSQLiteDatabase.Open(const Password: AnsiString);
var
  Msg: PAnsiChar;
  iResult: Integer;
begin
  EnsureInitialized;

  FConnected := False;

  case FEncoding of
    seUTF8: iResult := sqlite3_open_v2(PAnsiChar(UTF8Encode(FFileName)), Fdb,
      SQLITE_OPEN_CREATE or SQLITE_OPEN_READWRITE or SQLITE_OPEN_URI, nil);
    seUTF16: iResult := SQLite3_Open16(PChar(FFileName), Fdb);
  else
    iResult := SQLITE_OK;
  end;

  if iResult <> SQLITE_OK then
    if Assigned(Fdb) then
    begin
      Msg := Sqlite3_ErrMsg(Fdb);
      raise ESqliteException.CreateFmt('Failed to open database "%s" : %s',
        [FFileName, Msg], iResult);
    end
    else
      raise ESqliteException.CreateFmt('Failed to open database "%s" : unknown error',
        [FFileName], iResult);


  if (Password <> '') then
  begin
    //db is encrypted

{$IFNDEF USE_SYSTEM_SQLITE}
    if not Assigned(sqlite3_key) then
      raise ESQLiteException.Create('Loaded SQLite library does not support database encryption');

    iResult := sqlite3_key(fDB, PAnsiChar(Password), Length(Password));
    if iResult <> SQLITE_OK then
    begin
      RaiseError('Cannot encrypt database', '');
    end;
{$ELSE}
    raise ESQLiteException.Create('Loaded SQLite library does not support database encryption');
{$ENDIF}
  end;

  FConnected := True;
//set a few configs
//L.G. Do not call it here. Because busy handler is not setted here,
// any share violation causing exception!

//    self.ExecSQL('PRAGMA SYNCHRONOUS=NORMAL;');
//    self.ExecSQL('PRAGMA temp_store = MEMORY;');
  if Assigned(FOnAfterOpen) then
    FOnAfterOpen(Self);
end;

function TSQLiteDatabase.GetTableValue(const SQL: string): int64;
var
  Table: TSQLiteUniTable;
  stmt: TSQLitePreparedStatement;
begin
  Result := 0;
  stmt := TSQLitePreparedStatement.Create(Self);
  Table := stmt.ExecQuery(SQL);
  try
    if not Table.EOF then
      Result := Table.Fields[0].AsInteger;
  finally
    Table.Free;
    stmt.Free;
  end;
end;

function TSQLiteDatabase.GetTableString(const SQL: string): String;
var
  Table: TSQLiteUniTable;
  stmt: TSQLitePreparedStatement;
begin
  Result := '';
  stmt := TSQLitePreparedStatement.Create(Self);
  Table := stmt.ExecQuery(SQL);
  try
    if not Table.EOF then
      Result := Table.Fields[0].AsString;
  finally
    Table.Free;
    stmt.Free;
  end;
end;

procedure TSQLiteDatabase.GetTableStrings(const SQL: string;
  const Value: TStrings);
var
  Table: TSQLiteUniTable;
  stmt: TSQLitePreparedStatement;
begin
  Value.Clear;
  stmt := TSQLitePreparedStatement.Create(Self, SQL);
  Table := stmt.ExecQuery(SQL);
  Value.BeginUpdate;
  try
    while not table.EOF do
    begin
      Value.Add(Table.FieldAsString(0));
      table.Next;
    end;
  finally
    Value.EndUpdate;
    Table.Free;
    stmt.Free;
  end;
end;

procedure TSQLiteDatabase.BeginTransaction;
begin
  if not self.fInTrans then
  begin
    self.ExecSQL('BEGIN TRANSACTION');
    self.fInTrans := True;
  end
  else
    raise ESqliteException.Create('Transaction already open');
end;

procedure TSQLiteDatabase.ChangePassword(const NewPassword: AnsiString);
{$IFNDEF USE_SYSTEM_SQLITE}
var
  pNewPass: PAnsiChar;
  iLen: Integer;
{$ENDIF}
begin
{$IFNDEF USE_SYSTEM_SQLITE}
  if Assigned(sqlite3_rekey) then
  begin
    if NewPassword = '' then
    begin
      pNewPass := nil;
      iLen := 0;
    end
    else
    begin
      pNewPass := PAnsiChar(NewPassword);
      iLen := Length(NewPassword);
    end;

    if (sqlite3_rekey(fDB, pNewPass, iLen) <> SQLITE_OK) then
    begin
      RaiseError('Cannot change database password', '');
    end;
  end
  else
{$ENDIF}
  begin
    raise ESQLiteException.Create('Loaded SQLite library does not support database encryption');
  end;
end;

procedure TSQLiteDatabase.Close;
begin
  if self.fInTrans then
    self.Rollback;  //assume rollback
  ParamsClear;
  if Assigned(fDB) then
    SQLite3_Close(fDB);
  FConnected := False;

  if Assigned(FOnAfterClose) then
    FOnAfterClose(Self);
end;

procedure TSQLiteDatabase.Commit;
begin
  self.ExecSQL('COMMIT');
  self.fInTrans := False;
end;

procedure TSQLiteDatabase.Rollback;
begin
  self.ExecSQL('ROLLBACK');
  self.fInTrans := False;
end;

function TSQLiteDatabase.TableExists(const TableName: string): boolean;
var
  sql: string;
  ds: TSqliteTable;
begin
  //returns true if table exists in the database
  sql := 'select [sql] from sqlite_master where [type] = ''table'' and lower(name) = ''' +
    lowercase(TableName) + ''' ';
  ds := self.GetTable(sql);
  try
    Result := (ds.Count > 0);
  finally
    ds.Free;
  end;
end;

procedure TSQLiteDatabase.SetTimeout(Value: integer);
begin
  SQLite3_BusyTimeout(self.fDB, Value);
end;

procedure TSQLiteDatabase.Vacuum;
begin
  ExecSQL('VACUUM');
end;

function TSQLiteDatabase.Version: string;
begin
  EnsureInitialized;
  Result := UTF8ToString(SQLite3_Version);
end;

procedure TSQLiteDatabase.AddCustomCollate(const name: string;
  xCompare: TCollateXCompare);
begin
  sqlite3_create_collation16(fdb, PChar(name), SQLITE_UTF8, nil, xCompare);
end;

procedure TSQLiteDatabase.AddSystemCollate;
begin
  {$IFDEF WIN32}
  sqlite3_create_collation16(fdb, 'SYSTEM', SQLITE_UTF16LE, nil, @SystemCollate);
  {$ENDIF}
end;

procedure TSQLiteDatabase.Analyze;
begin
  ExecSQL('ANALYZE');
end;

{$HINTS OFF}
function TSQLiteDatabase.Attach(const DBFilename, AttachedDBName: string): Boolean;
var
  stmt: TSQLitePreparedStatement;
begin
  Result := False;
  stmt := GetPreparedStatement('ATTACH DATABASE ? AS ?', [DBFilename, AttachedDBName]);
  try
    Result := stmt.ExecSQL;
  finally
    stmt.Free;
  end;
end;
{$HINTS ON}

procedure TSQLiteDatabase.ParamsClear;
begin
  if Assigned(fParams) then
  begin
    fParams.Clear;
  end;
end;

procedure TSQLiteDatabase.AddParamInt(const name: string; value: int64);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_INTEGER;
  par.valueinteger := value;
  fParams.Add(par);
end;

class procedure TSQLiteDatabase.AddNewSupportedColumnType(const ColTypeName: string; ColType: Integer);
var
  sColName: string;
begin
  if (ColTypeName <> '') and (ColType > 0) then
  begin
    sColName := UpperCase(ColTypeName);

    FColumnTypes.AddOrSetValue(sColName, ColType);
  end;
end;

procedure TSQLiteDatabase.AddParamFloat(const name: string; value: double);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_FLOAT;
  par.valuefloat := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamText(const name: string; const value: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_TEXT;
  par.valuedata := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamNull(const name: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_NULL;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.SetConnected(const Value: Boolean);
begin
  if Value <> FConnected then
  begin
    if FConnected and not Value then
      Close;

    if Value then
    begin
      Open('');
      //set callback events
      SetEvents;
    end;

    FConnected := Value;
  end;

end;

procedure TSQLiteDatabase.SetEvents;
begin
  SetOnAuthorize(FOnAuthorize);
  SetOnUpdate(FOnUpdate);
end;

procedure TSQLiteDatabase.SetExtEnabled(const Value: Boolean);
begin
  if Value <> FExtEnabled then
  begin
    if FConnected then
    begin
      sqlite3_enable_load_extension(fDB, Integer(Value));

      FExtEnabled := Value;
    end;
  end;
end;

procedure TSQLiteDatabase.SetFilename(const Value: TFileName);
begin
  if (Value <> FFilename) then
  begin
    FFilename := Value;
  end;
end;

procedure TSQLiteDatabase.SetOnAuthorize(const Value: TAuthEvent);
begin
  FOnAuthorize := Value;

  if FConnected then
  begin
    if Assigned(FOnAuthorize) then
    begin
      sqlite3_set_authorizer(fDB, @DoAuthorize, Self);
    end
    else
    begin
      sqlite3_set_authorizer(fDB, nil, nil);
    end;
  end;
end;

procedure TSQLiteDatabase.SetOnUpdate(const Value: TUpdateHookEvent);
begin
  FOnUpdate := Value;

  if FConnected then
  begin
    if Assigned(FOnUpdate) then
    begin
      sqlite3_update_hook(fDB, @DoUpdateHook, Self);
    end
    else
    begin
      sqlite3_update_hook(fDB, nil, nil);
    end;
  end;
end;

procedure TSQLiteDatabase.SetParams(Stmt: TSQLiteStmt);
var
  n: integer;
  i: integer;
  par: TSQliteParam;
begin
  try
    for n := 0 to fParams.Count - 1 do
    begin
      par := fParams[n];
      i := sqlite3_bind_parameter_index(Stmt, PAnsiChar(ansistring(par.name)));
      if i > 0 then
      begin
        case par.valuetype of
          SQLITE_INTEGER:
            sqlite3_bind_int64(Stmt, i, par.valueinteger);
          SQLITE_FLOAT:
            sqlite3_bind_double(Stmt, i, par.valuefloat);
          SQLITE_TEXT:
            sqlite3_bind_text16(Stmt, i, PChar(par.valuedata),
              length(par.valuedata), SQLITE_TRANSIENT);
          SQLITE_NULL:
            sqlite3_bind_null(Stmt, i);
        end;
      end;
    end;
  finally
    ParamsClear;
  end;
end;

//database rows that were changed (or inserted or deleted) by the most recent SQL statement
function TSQLiteDatabase.GetRowsChanged: integer;
begin
  Result := 0;
  if FConnected then
    Result := SQLite3_Changes(self.fDB);
end;

procedure TSQLiteDatabase.DoQuery(const value: string);
begin
  if assigned(OnQuery) then
    OnQuery(Self, Value);
end;

//returns result of SQLITE3_Backup_Step
function TSQLiteDatabase.Backup(const TargetDB: TSQLiteDatabase; const targetName: String; const sourceName: String): integer;
var
  pBackup: TSQLiteBackup;
begin
  pBackup := Sqlite3_backup_init(TargetDB.DB,PAnsiChar(ansistring(targetName)),self.DB,PAnsiChar(ansistring(sourceName)));

  if (pBackup = nil) then
    raise ESqliteException.Create('Could not initialize backup')
  else begin
      try

        result := SQLITE3_Backup_Step(pBackup,-1); //copies entire db
      finally
        SQLITE3_backup_finish(pBackup);
      end;
  end;
end;

function TSQliteDatabase.Backup(const TargetDB: TSQLiteDatabase): integer;
begin
  result := self.Backup(TargetDB,'main','main');
end;

//------------------------------------------------------------------------------
// TSQLiteTable
//------------------------------------------------------------------------------

constructor TSQLiteTable.Create(const DB: TSQLiteDatabase; const SQL: String);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PChar;
  iStepResult: integer;
  ptr: pointer;
  iNumBytes: integer;
  thisBlobValue: TMemoryStream;
  thisStringValue: pstring;
  thisDoubleValue: pDouble;
  thisIntValue: pInt64;
  thisColType: pInteger;
  i: integer;
  DeclaredColType: PChar;
  ActualColType: integer;
  ptrValue: PChar;
  sColType: string;
  iColType: Integer;
begin
  inherited create;
  try
    self.fRowCount := 0;
    self.fColCount := 0;
    //if there are several SQL statements in SQL, NextSQLStatment points to the
    //beginning of the next one. Prepare only prepares the first SQL statement.
    if Sqlite3_Prepare16_v2(DB.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <> SQLITE_OK then
      DB.RaiseError('Error executing SQL', SQL);
    if (Stmt = nil) then
      DB.RaiseError('Could not prepare SQL statement', SQL);
    DB.DoQuery(SQL);
    DB.SetParams(Stmt);

    iStepResult := Sqlite3_step(Stmt);
    while (iStepResult <> SQLITE_DONE) do
    begin
      case iStepResult of
        SQLITE_ROW:
          begin
            Inc(fRowCount);
            if (fRowCount = 1) then
            begin
            //get data types
              fCols := TStringList.Create;
              fColTypes := TList<Pointer>.Create;
              fColCount := SQLite3_ColumnCount(stmt);

              for i := 0 to Pred(fColCount) do
              begin
                fCols.Add(AnsiUpperCase(Sqlite3_ColumnName16(stmt, i)));
              end;

              for i := 0 to Pred(fColCount) do
              begin
                new(thisColType);
                DeclaredColType := Sqlite3_ColumnDeclType16(stmt, i);
                if DeclaredColType = nil then
                  thisColType^ := Sqlite3_ColumnType(stmt, i) //use the actual column type instead
                //seems to be needed for last_insert_rowid
                else
                begin
                  sColType :=  AnsiUpperCase(DeclaredColType);

                  if TSQLiteDatabase.FColumnTypes.TryGetValue(sColType, iColType) then
                  begin
                    thisColType^ := iColType;
                  end
                  else
                  begin
                    thisColType^ := dtStr;
                  end;

                end;
                 { if (DeclaredColType = 'INTEGER') or (DeclaredColType = 'BOOLEAN') then
                    thisColType^ := dtInt
                  else
                    if (DeclaredColType = 'NUMERIC') or
                      (DeclaredColType = 'FLOAT') or
                      (DeclaredColType = 'DOUBLE') or
                      (DeclaredColType = 'REAL') then
                      thisColType^ := dtNumeric
                    else
                      if DeclaredColType = 'BLOB' then
                        thisColType^ := dtBlob
                      else
                        thisColType^ := dtStr; }
                fColTypes.Add(thiscoltype);
              end;
              fResults := TList<Pointer>.Create;
            end;

          //get column values
            for i := 0 to Pred(ColCount) do
            begin
              ActualColType := Sqlite3_ColumnType(stmt, i);
              if (ActualColType = SQLITE_NULL) then
                fResults.Add(nil)
              else
                if pInteger(fColTypes[i])^ = dtInt then
                begin
                  new(thisintvalue);
                  thisintvalue^ := Sqlite3_ColumnInt64(stmt, i);
                  fResults.Add(thisintvalue);
                end
                else
                  if pInteger(fColTypes[i])^ = dtNumeric then
                  begin
                    new(thisdoublevalue);
                    thisdoublevalue^ := Sqlite3_ColumnDouble(stmt, i);
                    fResults.Add(thisdoublevalue);
                  end
                  else
                    if pInteger(fColTypes[i])^ = dtBlob then
                    begin
                      iNumBytes := Sqlite3_ColumnBytes(stmt, i);
                      if iNumBytes = 0 then
                        thisblobvalue := nil
                      else
                      begin
                        thisblobvalue := TMemoryStream.Create;
                        thisblobvalue.position := 0;
                        ptr := Sqlite3_ColumnBlob(stmt, i);
                        //call again, see sqlite docs
                        iNumBytes := Sqlite3_ColumnBytes(stmt, i);
                        thisblobvalue.writebuffer(ptr^, iNumBytes);
                      end;
{$IFDEF AUTOREFCOUNT}
                      // fResults hold pointer only so next assignment to
                      // thisblobvalue would clear the original value.
                      if Assigned(thisblobvalue) then
                        thisblobvalue.__ObjAddRef;
{$ENDIF}
                      fResults.Add(thisblobvalue);
                    end
                    else
                    begin
                      new(thisstringvalue);
                      ptrValue := Sqlite3_ColumnText16(stmt, i);
                      setstring(thisstringvalue^, ptrvalue, strlen(ptrvalue));
                      fResults.Add(thisstringvalue);
                    end;
            end;
          end;
        SQLITE_BUSY:
          raise ESqliteException.CreateFmt('Could not prepare SQL statement',
            [SQL, 'SQLite is Busy'], iStepResult);
      else
        begin
        SQLite3_reset(stmt);
        DB.RaiseError('Could not retrieve data', SQL);
        end;
      end;
      iStepResult := Sqlite3_step(Stmt);
    end;
    fRow := 0;
  finally
    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
  end;
end;

//..............................................................................

destructor TSQLiteTable.Destroy;
var
  i: cardinal;
  iColNo: integer;
begin
  if Assigned(fResults) then
  begin
    for i := 0 to fResults.Count - 1 do
    begin
      //check for blob type
      iColNo := (i mod fColCount);
      case pInteger(self.fColTypes[iColNo])^ of
        dtBlob:
{$IFNDEF AUTOREFCOUNT}
          TMemoryStream(fResults[i]).Free;
{$ELSE}
          // See TSQLiteTable.Create
          if Assigned(fResults[i]) then
            TObject(fResults[i]).__ObjRelease;
{$ENDIF}
        dtStr:
          if fResults[i] <> nil then
          begin
            setstring(string(fResults[i]^), nil, 0);
            dispose(fResults[i]);
          end;
      else
        dispose(fResults[i]);
      end;
    end;
    fResults.Free;
  end;
  if Assigned(fCols) then
    fCols.Free;
  if Assigned(fColTypes) then
    for i := 0 to fColTypes.Count - 1 do
      dispose(fColTypes[i]);
  fColTypes.Free;
  inherited;
end;

//..............................................................................

function TSQLiteTable.GetColumns(I: integer): string;
begin
  Result := fCols[I];
end;

//..............................................................................

function TSQLiteTable.GetCountResult: integer;
begin
  if not EOF then
    Result := StrToInt(Fields[0])
  else
    Result := 0;
end;

function TSQLiteTable.GetCount: integer;
begin
  Result := FRowCount;
end;

//..............................................................................

function TSQLiteTable.GetEOF: boolean;
begin
  Result := fRow >= fRowCount;
end;

function TSqliteTable.GetIsLastRow: boolean;
begin
Result := fRow = (fRowCount -1);
end;

function TSQLiteTable.GetBOF: boolean;
begin
  Result := fRow <= 0;
end;

//..............................................................................

function TSQLiteTable.GetFieldValByName(const FieldName: string): string;
begin
  Result := GetFields(self.GetFieldIndex(FieldName));
end;

function TSQLiteTable.GetFieldIndex(const FieldName: string): integer;
begin
  if (fCols = nil) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  if (fCols.count = 0) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  Result := fCols.IndexOf(UpperCase(FieldName));

  if (result < 0) then
  begin
    raise ESqliteException.Create('Field not found in dataset: ' + fieldname)
  end;
end;

//..............................................................................

function TSQLiteTable.GetFields(I: cardinal): string;
var
  thisvalue: pstring;
  thistype: integer;
begin
  Result := '';
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  //integer types are not stored in the resultset
  //as strings, so they should be retrieved using the type-specific
  //methods
  thistype := pInteger(self.fColTypes[I])^;

  case thistype of
    dtStr:
      begin
        thisvalue := self.fResults[(self.frow * self.fColCount) + I];
        if (thisvalue <> nil) then
          Result := thisvalue^
        else
          Result := '';
      end;
    dtInt:
      Result := IntToStr(self.FieldAsInteger(I));
    dtNumeric:
      Result := FloatToStr(self.FieldAsDouble(I));
    dtBlob:
      Result := self.FieldAsBlobText(I);
  else
    Result := '';
  end;
end;

function TSqliteTable.FieldAsBlob(I: cardinal): TMemoryStream;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := nil
  else
    if pInteger(self.fColTypes[I])^ = dtBlob then
      Result := TMemoryStream(self.fResults[(self.frow * self.fColCount) + I])
    else
      raise ESqliteException.Create('Not a Blob field');
end;

function TSqliteTable.FieldAsBlobText(I: cardinal): string;
var
  MemStream: TMemoryStream;
  ts: TStringStream;
begin
  Result := '';
  MemStream := self.FieldAsBlob(I);

  if MemStream <> nil then
    if MemStream.Size > 0 then
      begin
         ts := TStringStream.Create('',TEncoding.UTF8);
         try
         ts.LoadFromStream(MemStream);
         ts.Position := 0;
         result := ts.ReadString(ts.size);
         finally
         ts.Free;
         end;
      end;
     //do not free the TMemoryStream here; it is freed when
     //TSqliteTable is destroyed

end;


function TSqliteTable.FieldAsInteger(I: cardinal): int64;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := 0
  else
    if pInteger(self.fColTypes[I])^ = dtInt then
      Result := pInt64(self.fResults[(self.frow * self.fColCount) + I])^
    else
      if pInteger(self.fColTypes[I])^ = dtNumeric then
        Result := trunc(strtofloat(pString(self.fResults[(self.frow * self.fColCount) + I])^))
      else
        raise ESqliteException.Create('Not an integer or numeric field');
end;

function TSqliteTable.FieldAsDouble(I: cardinal): double;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := 0
  else
    if pInteger(self.fColTypes[I])^ = dtInt then
      Result := pInt64(self.fResults[(self.frow * self.fColCount) + I])^
    else
      if pInteger(self.fColTypes[I])^ = dtNumeric then
        Result := pDouble(self.fResults[(self.frow * self.fColCount) + I])^
      else
        raise ESqliteException.Create('Not an integer or numeric field');
end;

function TSqliteTable.FieldAsString(I: cardinal): string;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := ''
  else
    Result := self.GetFields(I);
end;

function TSqliteTable.FieldIsNull(I: cardinal): boolean;
var
  thisvalue: pointer;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  thisvalue := self.fResults[(self.frow * self.fColCount) + I];
  Result := (thisvalue = nil);
end;

//..............................................................................

function TSQLiteTable.Next: boolean;
begin
  Result := False;
  if not EOF then
  begin
    Inc(fRow);
    Result := True;
  end;
end;

function TSQLiteTable.Previous: boolean;
begin
  Result := False;
  if not BOF then
  begin
    Dec(fRow);
    Result := True;
  end;
end;

function TSQLiteTable.MoveFirst: boolean;
begin
  Result := False;
  if self.fRowCount > 0 then
  begin
    fRow := 0;
    Result := True;
  end;
end;

function TSQLiteTable.MoveLast: boolean;
begin
  Result := False;
  if self.fRowCount > 0 then
  begin
    fRow := fRowCount - 1;
    Result := True;
  end;
end;

{$WARNINGS OFF}
function TSQLiteTable.MoveTo(position: cardinal): boolean;
begin
  Result := False;
  if (self.fRowCount > 0) and (self.fRowCount > position) then
  begin
    fRow := position;
    Result := True;
  end;
end;
{$WARNINGS ON}

{ TSQLiteUniTable }

constructor TSQLiteUniTable.Create(const DB: TSQLiteDatabase; const SQL: string);
begin
  Create(DB);

  DB.DoQuery(SQL);

  GetDataTypes;
end;

constructor TSQLiteUniTable.Create(const DB: TSQLiteDatabase);
begin
  inherited Create;
  self.fDB := db;
  self.fEOF := false;
  self.fRow := 0;
  self.fColCount := 0;
  self.fSQL := SQL;
  fStmt := nil;
  FFields := TObjectList<TSQLiteField>.Create(True);
end;

constructor TSQLiteUniTable.Create(const DB: TSQLiteDatabase; hStmt: TSQLiteStmt);
begin
  Create(DB);
  fStmt := hStmt;
  GetDataTypes;
end;

destructor TSQLiteUniTable.Destroy;
begin
  if Assigned(fStmt) then
    Sqlite3_Finalize(fstmt);

  FFields.Free;
  if Assigned(fCols) then
  begin
    fCols.Free;
  end;
  inherited;
end;

procedure TSQLiteUniTable.GetDataTypes;
var
  i: Integer;
  sColName, sColType: string;
  aField: TSQLiteField;
  DeclaredColType: PChar;
  iColType: Integer;
begin
  //get data types
  FFields.Clear;
  fColCount := SQLite3_ColumnCount(fstmt);
  SetLength(FColTypes, fColCount);
  SetLength(FColNames, fColCount);
  fCols := TDictionary<string, Integer>.Create(fColCount);

  Next;

  for i := 0 to Pred(fColCount) do
  begin
    sColName := AnsiUpperCase(Sqlite3_ColumnName16(fstmt, i));
    DeclaredColType := Sqlite3_ColumnDeclType16(stmt, i);
    if DeclaredColType = nil then
      FColTypes[i] := SQLite3_ColumnType(fStmt, i)
    else
    begin
      sColType := UpperCase(DeclaredColType);

      if TSQLiteDatabase.FColumnTypes.TryGetValue(sColType, iColType) then
      begin
        FColTypes[i] := iColType;
      end
      else
      begin
        FColTypes[i] := dtStr;
      end;


     { if (sColType = 'INTEGER') or (sColType = 'BOOLEAN') then
        FColTypes[i] := dtInt
      else
        if (sColType = 'NUMERIC') or
          (sColType = 'FLOAT') or
          (sColType = 'DOUBLE') or
          (sColType = 'REAL') then
          FColTypes[i] := dtNumeric
        else
        if (sColType = 'DATETIME') then
          FColTypes[i] := dtDateTime
        else if (sColType = 'DATE') then
          FColTypes[i] := dtDate
        else
        if sColType = 'BLOB' then
          FColTypes[i] := dtBlob
          else
            FColTypes[i] := dtStr;  }
    end;
    aField := TSQLiteField.Create;
    aField.Table := Self;
    aField.Index := i;
    aField.Name := sColName;
    aField.FieldType := FColTypes[i];
    FFields.Add(aField);
    fCols.Add(sColName, i);
    FColNames[i] := sColName;
  end;

end;

function TSQLiteUniTable.GetEnumerator: TUniTableEnumerator;
begin
  Result := TUniTableEnumerator.Create(Self);
end;

function TSQLiteUniTable.GetEOF: Boolean;
begin
  Result := fEOF;
end;

/// <summary>
/// Creates new TMemoryStream object on succesful retrieval. Do not forget to free it afterwards!
/// </summary>
function TSQLiteUniTable.FieldAsBlob(I: cardinal): TMemoryStream;
var
  iNumBytes: integer;
  ptr: pointer;
begin

  iNumBytes := Sqlite3_ColumnBytes(fstmt, i);
  if iNumBytes > 0 then
  begin
    Result := TMemoryStream.Create; // potential memory leak if result not freed after this function call
    ptr := Sqlite3_ColumnBlob(fstmt, i);
    //call it again - see sqlite docs
    iNumBytes := Sqlite3_ColumnBytes(fstmt, i);
    Result.writebuffer(ptr^, iNumBytes);
    Result.Position := 0;
  end
  else
    Result := nil;

end;

function TSQLiteUniTable.FieldAsBlobPtr(I: cardinal; out iNumBytes: integer): Pointer;
begin
  Result := Sqlite3_ColumnBlob(fstmt, i);
  iNumBytes := Sqlite3_ColumnBytes(fstmt, i);
end;

function TSQLiteUniTable.FieldAsBlobText(I: cardinal): string;
var
  MemStream: TMemoryStream;
  ts: TStringStream;
begin
  Result := '';
  MemStream := self.FieldAsBlob(I);
  try

  if MemStream <> nil then
    if MemStream.Size > 0 then
      begin
         ts := TStringStream.Create('',TEncoding.UTF8);
         try
         ts.LoadFromStream(MemStream);
         ts.Position := 0;
         result := ts.ReadString(ts.size);
         finally
         ts.Free;
         end;
      end;

  finally
  MemStream.Free;
  end;


end;

function TSQLiteUniTable.FieldAsBlobVariant(I: Cardinal): Variant;
var
  LStream: TMemoryStream;
  MyBuffer: Pointer;
begin
  Result := Null;
  LStream := FieldAsBlob(I);
  if Assigned(LStream) then
  begin
    try
      LStream.Position := 0;
      Result := VarArrayCreate([0, LStream.Size - 1], VarByte);
      MyBuffer := VarArrayLock(Result);
      LStream.ReadBuffer(MyBuffer^, LStream.Size);
    finally
      VarArrayUnlock(Result);
      LStream.Free;
    end;
  end;
end;

function TSQLiteUniTable.FieldAsDouble(I: cardinal): double;
begin
  Result := Sqlite3_ColumnDouble(fstmt, i);
end;

function TSQLiteUniTable.FieldAsInteger(I: cardinal): int64;
begin
  Result := Sqlite3_ColumnInt64(fstmt, i);
end;

function TSQLiteUniTable.FieldAsString(I: cardinal): string;
begin
  Result := self.GetFieldsAsString(I);
end;

function TSQLiteUniTable.FieldIsNull(I: cardinal): boolean;
begin
  Result := Sqlite3_ColumnText(fstmt, i) = nil;
end;

function TSQLiteUniTable.FindField(const AFieldName: string): TSQLiteField;
var
  ix: Integer;
begin
  Result := nil;
  if (fCols.TryGetValue(AnsiUpperCase(AFieldName), ix)) then
  begin
    Result := FFields[ix];
  end;
end;

function TSQLiteUniTable.GetColumns(I: integer): string;
begin
  Result := FColNames[I];
end;

function TSQLiteUniTable.GetCurrentRec: Variant;
begin
  Result := VarDataRecordCreate(Self);
end;

function TSQLiteUniTable.GetField(I: Integer): TSQLiteField;
begin
  Result := FFields[I];
end;

function TSQLiteUniTable.GetFieldByName(const FieldName: string): TSQLiteField;
begin
  Result := GetField(GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.GetFieldByNameAsString(const FieldName: string): string;
begin
  Result := GetFieldsAsString(self.GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.GetFieldCount: Cardinal;
begin
  Result := fColCount;
end;

function TSQLiteUniTable.GetFieldIndex(const FieldName: string): integer;
begin
  if (fCols = nil) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  if (fCols.count = 0) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  Result := -1;

  if not (fCols.TryGetValue(AnsiUpperCase(FieldName), Result)) then
  begin
    raise ESqliteException.Create('Field not found in dataset: ' + fieldname)
  end;
end;

function TSQLiteUniTable.GetFieldsAsString(I: cardinal): string;
begin
  Result := Sqlite3_ColumnText16(fstmt, i);
end;

function TSQLiteUniTable.GetFieldsVal(I: Cardinal): Variant;
var
  //thisvalue: pstring;
  thistype: integer;
  dt1: TDateTime;
begin
  Result := Unassigned;
  if EOF then
    raise ESqliteException.Create('Table is at End of File')
  else if FieldIsNull(I) then
    Exit;
  //integer types are not stored in the resultset
  //as strings, so they should be retrieved using the type-specific
  //methods
  thistype := FColTypes[I];

  case thistype of
    dtStr:
      begin
        Result := FieldAsString(I);
      end;
    dtInt:
      Result := FieldAsInteger(I);
    dtNumeric:
      Result := FieldAsDouble(I);
    dtBlob:
      Result := FieldAsBlobVariant(I); //  FieldAsBlobText(I);
    dtDate:
    begin
      if not TryStrToDate(FieldAsString(I), dt1, fDB.FmtSett) then
        Result := Unassigned
      else
        Result := dt1;
    end;

    dtDateTime:
      if not TryStrToDateTime(FieldAsString(I), dt1, fDB.FmtSett) then
        Result := Unassigned
      else
        Result := dt1;
  else
    Result := Unassigned;
  end;
end;


function TSQLiteUniTable.GetRow: cardinal;
begin
  Result := fRow;
end;

function TSQLiteUniTable.Next: boolean;
var
  iStepResult: integer;
begin
  fEOF := true;
  iStepResult := Sqlite3_step(fStmt);
  case iStepResult of
    SQLITE_ROW:
      begin
        fEOF := false;
        inc(fRow);
      end;
    SQLITE_DONE:
      // we are on the end of dataset
      // return EOF=true only
      ;
  else
    begin
      SQLite3_reset(fStmt);
      fDB.RaiseError('Cannot execute SQL statement', fSQL);
    end;
  end;
  Result := not fEOF;
end;

{SQliteParam }

constructor TSQliteParam.Create;
begin
  inherited Create;
  valueptr := nil;
  index := -1;
  blobsize := 0;
end;

{ TSQLitePreparedStatement }

procedure TSQLitePreparedStatement.SetParamDateTime(const I: Integer; const Value: TDateTime);
begin
  SetParamText(I, DateTimeToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamDate(const I: Integer; const Value: TDate);
begin
  SetParamText(I, DateToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamTime(const I: Integer; const Value: TTime);
begin
  SetParamText(I, TimeToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamBlob(const I: Integer; const Value: TStream);
begin
  SetParamBlobInternal(Value, I);
end;

procedure TSQLitePreparedStatement.SetParamBlob(const name: string; const Value: TStream);
begin
  SetParamBlobInternal(Value, 0, name);
end;

procedure TSQLitePreparedStatement.SetParamBlobInternal(const Value: TStream; const I: Integer; const Name: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.index := I;
  par.valuetype := SQLITE_BLOB;
  par.blobsize := Value.Size;
  if par.blobsize > 0 then
  begin
    GetMem(par.valueptr, par.blobsize);
    if (par.valueptr = nil) then
      raise ESqliteException.CreateFmt('Error getting memory to set blob as param',
        ['', 'Error']);

    Value.Position := 0;
    Value.ReadBuffer(par.valueptr^, par.blobsize);
  end;
  fParams.Add(par);
end;

procedure TSQLitePreparedStatement.SetParamDate(const name: string; const Value: TDate);
begin
  SetParamText(name, DateToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamDateTime(const name: string; const Value: TDateTime);
begin
  SetParamText(name, DateTimeToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamTime(const name: string; const Value: TTime);
begin
  SetParamText(name, TimeToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamFloat(const I: Integer; value: double);
begin
  SetParamFloatInternal(value, I);
end;

procedure TSQLitePreparedStatement.SetParamFloatInternal(value: double; const I: Integer; const name: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.index := I;
  par.valuetype := SQLITE_FLOAT;
  fParams.Add(par);

  par.valuefloat := value;
end;

procedure TSQLitePreparedStatement.SetParamFloat(const name: string; value: double);
begin
  SetParamFloatInternal(value, 0, name);
end;

procedure TSQLitePreparedStatement.SetParamInt(const name: string; value: int64);
begin
  SetParamIntInternal(value, 0, name);
end;

procedure TSQLitePreparedStatement.SetParamInt(const I: Integer; value: int64);
begin
  SetParamIntInternal(value, I);
end;

procedure TSQLitePreparedStatement.SetParamIntInternal(value: int64; const I: Integer; const name: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.index := I;
  par.valuetype := SQLITE_INTEGER;
  fParams.Add(par);

  par.valueinteger := value;
end;

procedure TSQLitePreparedStatement.SetParamNull(const I: Integer);
begin
  SetParamNullInternal(I);
end;

procedure TSQLitePreparedStatement.SetParamNull(const name: string);
begin
  SetParamNullInternal(0, name);
end;

procedure TSQLitePreparedStatement.SetParamNullInternal(const I: Integer; const name: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.index := I;
  par.name := name;
  par.valuetype := SQLITE_NULL;
  fParams.Add(par);
end;

procedure TSQLitePreparedStatement.SetParams(const Params: array of TVarRec);
var
  i: Integer;
  ext: Extended;
  d: Double;
  v: Variant;
begin
  for i := Low(Params) to High(Params) do
  begin
    case Params[i].VType of
{$IFNDEF NEXTGEN}
      vtAnsiString:
      begin
        SetParamText(i + 1, String(Params[i].VAnsiString));
      end;
      vtString:
      begin
        SetParamText(i + 1, String(Params[i].VString));
      end;
{$ENDIF}
      vtWideString:
      begin
        SetParamText(i + 1, String(Params[i].VWideString));
      end;
      vtUnicodeString:
      begin
        SetParamText(i + 1, String(Params[i].VUnicodeString));
      end;
      vtInt64:
      begin
        SetParamInt(i+1, Int64(Params[i].VInt64^));
      end;
      vtInteger:
      begin
        SetParamInt(i+1, Params[i].VInteger);
      end;
      vtExtended:
      begin
        ext := Params[i].VExtended^;
        d := ext;
        SetParamFloat(i+1, d);
      end;
      vtCurrency:
      begin
        ext := Params[i].VCurrency^;
        d := ext;
        SetParamFloat(i+1, d);
      end;
      vtBoolean:
      begin
        SetParamInt(i+1, Integer(Params[i].VBoolean));
      end;
      vtObject:
      begin
        if not Assigned(Params[i].VObject) then
          SetParamNull(i+1)
        else
        begin
          if (TObject(Params[i].VObject) is TStream) then
          begin
            SetParamBlob(i+1, TStream(Params[i].VObject));
          end
          else
            raise ESQLiteException.Create('Unsupported param object type');
        end;
      end;
      vtVariant:
      begin
        v := Params[i].VVariant^;
        SetParamVariant(i+1, v);
      end;
      vtPointer:
      begin
        if Params[i].VPointer = nil then
        begin
          SetParamNull(i+1);
        end;
      end;
      else
      begin
        SetParamNull(i+1);
      end;
    end;
  end;
  BindParams;
end;

procedure TSQLitePreparedStatement.SetParamText(const name, value: string);
begin
  SetParamTextInternal(value, 0, name);
end;

procedure TSQLitePreparedStatement.SetParamText(const I: Integer; const value: string);
begin
  SetParamTextInternal(value, I);
end;

procedure TSQLitePreparedStatement.SetParamTextInternal(const value: string; const I: Integer; const name: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.index := I;
  par.name := name;
  par.valuetype := SQLITE_TEXT;
  fParams.Add(par);

  par.valuedata := value;
end;

procedure TSQLitePreparedStatement.SetParamVariant(const I: Integer; const Value: Variant);
begin
  SetParamVariantInternal(Value, I);
end;

procedure TSQLitePreparedStatement.SetParamVariant(const name: string; const Value: Variant);
begin
  SetParamVariantInternal(Value, 0, name);
end;

procedure TSQLitePreparedStatement.SetParamVariantInternal(const Value: Variant; const I: Integer; const name: string);
var
  vType: TVarType;
  bStream: TMemoryStream;
  ptr: Pointer;
  iDim: Integer;
  len: Integer;
begin
  vType := VarType(Value);
  case vType of
    varInteger, varSmallint, varByte, varInt64, varUInt64, varWord, varLongWord, varShortInt,
    varBoolean:
    begin
      SetParamIntInternal(Value, I, name);
    end;
    varString, varUString, varOleStr:
    begin
      SetParamTextInternal(Value, I, name);
    end;
    varSingle, varDouble, varCurrency:
    begin
      SetParamFloatInternal(Value, I, name);
    end;
    varDate:
    begin
      SetParamTextInternal(DateTimeToStr(Value, FDB.FmtSett), I, name);
    end;
    varNull, varEmpty:
    begin
      SetParamNullInternal(I, name);
    end
    else
    begin
      if VarIsArray(Value) then
      begin
        //blob type
        bStream := TMemoryStream.Create();
        iDim := VarArrayDimCount(Value);
        ptr := VarArrayLock(Value);
        try
          len := VarArrayHighBound(Value, iDim) - VarArrayLowBound(Value, iDim) + 1;
          bStream.WriteBuffer(ptr^, len);
          SetParamBlobInternal(bStream, I, name);
        finally
          VarArrayUnlock(Value);
          bStream.Free;
        end;
      end;
    end;
  end;

end;

procedure TSQLitePreparedStatement.SetSQL(const Value: string);
begin
  if Value <> FSQL then
  begin
    if (FSQL <> '') and (Value = '') then
      Exit;

    FSQL := Value;
    if BindParameterCount > 0 then
    begin
      ClearParams;
    end;

    if Assigned(FStmt) and (FOwnedStmt) then
    begin
      Sqlite3_Finalize(FStmt);
      FStmt := nil;
    end;
  end;
end;

procedure TSQLitePreparedStatement.ClearParams;
begin
  FParamsBound := False;
  if (FStmt <> nil) and (FOwnedStmt) then
    sqlite3_clear_bindings(FStmt);
  fParams.Clear;
end;

constructor TSQLitePreparedStatement.Create(const DB: TSQLiteDatabase; const SQL: string;
  const Params: array of TVarRec);
begin
  Create(DB);

  Assert(SQL <> '', 'SQL cannot be empty');

  FSQL := SQL;

  PrepareStatement(SQL);

  SetParams(Params);
end;

constructor TSQLitePreparedStatement.Create(const DB: TSQLiteDatabase; const SQL: string);
begin
  Create(DB, SQL, []);
end;

constructor TSQLitePreparedStatement.Create(const DB: TSQLiteDatabase);
begin
  inherited Create();
  FDB := DB;
  FSQL := '';
  FStmt := nil;
  FParamsBound := False;
  FOwnedStmt := True;
  fParams := TObjectList<TSQliteParam>.Create(True);
end;

destructor TSQLitePreparedStatement.Destroy;
begin
  FDB := nil;
  ClearParams;
  fParams.Free;
  if Assigned(fStmt) and (FOwnedStmt) then
    Sqlite3_Finalize(fstmt);
  inherited Destroy;
end;

function TSQLitePreparedStatement.ExecQuery: TSQLiteUniTable;
begin
  Result := ExecQuery('', []);
end;

function TSQLitePreparedStatement.ExecQuery(const SQL: string): TSQLiteUniTable;
begin
  Result := ExecQuery(SQL, []);
end;

{$HINTS OFF}
function TSQLitePreparedStatement.ExecQuery(const SQL: string; const Params: array of TVarRec): TSQLiteUniTable;
begin
  Result := nil;

  if FStmt = nil then
  begin
    PrepareStatement(SQL);
  end;

  SetParams(Params);

  // Even if the SQLitTable raises an exception, it will free the statement in
  // either case.
  FOwnedStmt := False;
  {DONE -oLinas -cGeneral : exec query and get resultset}
  Result := TSQLiteUniTable.Create(FDB, FStmt);
end;
{$HINTS ON}

function TSQLitePreparedStatement.ExecQueryIntf(const SQL: string; const Params: array of TVarRec): ISQLiteTable;
begin
  Result := ExecQuery(SQL, Params);
end;

function TSQLitePreparedStatement.ExecQueryIntf(): ISQLiteTable;
begin
  Result := ExecQuery();
end;

function TSQLitePreparedStatement.ExecQueryIntf(const SQL: string): ISQLiteTable;
begin
  Result := ExecQuery(SQL);
end;

{$HINTS OFF}
function TSQLitePreparedStatement.ExecSQL(const SQL: string; const Params: array of TVarRec): Boolean;
var
  iStepResult: Integer;
  LErrMsg: {$IFNDEF USE_SYSTEM_SQLITE}PAnsiChar{$ELSE}PPChar{$ENDIF};
  lSQL: PAnsiChar;
{$IFDEF NEXTGEN}
  M: TMarshaller;
{$ENDIF}
begin
  Result := False;
  try
    Self.SQL := SQL;

    if FStmt = nil then
    begin
      PrepareStatement(SQL);
    end;

    SetParams(Params);

    if fParams.Count > 0 then
    begin
      iStepResult := Sqlite3_step(FStmt);
      Result := (iStepResult = SQLITE_DONE);
      if not Result then
      begin
        SQLite3_reset(fstmt);
        FDB.RaiseError('Error executing SQL statement', FSQL);
      end;
    end
    else
    begin
      LErrMsg := nil;
{$IFNDEF NEXTGEN}
      lSQL := PAnsiChar(UTF8String(FSQL));
{$ELSE}
      lSQL := M.AsUtf8(FSQL).ToPointer;
{$ENDIF}
      // There is a bug in the RTL that states the parameter is wide char but it
      // is not!
      iStepResult := SQLite3_Exec(FDB.DB, Pointer(lSQL), nil, nil, LErrMsg);
      try
        Result := (iStepResult = SQLITE_OK);
        if not Result then
        begin
          FDB.RaiseError('Error executing SQL statement', FSQL);
        end;
      finally
        if LErrMsg <> nil then
          SQlite3_Free(LErrMsg);
      end;
    end;
  finally
    if Assigned(FStmt) then
    begin
      Sqlite3_Finalize(FStmt);
      FStmt := nil;
      ClearParams;
    end;
  end;
end;
{$HINTS ON}

function TSQLitePreparedStatement.ExecSQL(var RowsAffected: Integer): Boolean;
begin
  Result := ExecSQL('', RowsAffected);
end;

function TSQLitePreparedStatement.ExecSQL(const SQL: string; var RowsAffected: Integer): Boolean;
begin
  RowsAffected := 0;
  Result := ExecSQL(SQL);
  if Result then
  begin
    RowsAffected := FDB.GetRowsChanged;
  end;
end;

function TSQLitePreparedStatement.ExecSQL(const SQL: string): Boolean;
begin
  Result := ExecSQL(SQL, []);
end;

function TSQLitePreparedStatement.ExecSQL: Boolean;
begin
  Result := ExecSQL('', []);
end;

{$IFDEF DELPHI14_UP}
function TSQLitePreparedStatement.ExecSQLAndMapData<T>(var DataList: TObjectList<T>): Boolean;
var
  dst: TSQLiteUniTable;
  obj: TObject;
  ctx: TRttiContext;
  rtype: TRttiType;
  mType: TRTTIMethod;
  props: TArray<TRttiProperty>;
  AProp: TRttiProperty;
  AVal: TValue;
  fld: TSQLiteField;
  metaClass: TClass;
begin
  Result := False;
  if Assigned(DataList) then
  begin
    dst := ExecQuery;
    if Assigned(dst) then
    begin
      try
        ctx := TRttiContext.Create;
        rtype := ctx.GetType(TypeInfo(T));

        props := rtype.GetProperties;
        if Length(props) > 0 then
        begin
          for mType in rType.GetMethods do
          begin
            if mType.HasExtendedInfo and mType.IsConstructor then
            begin
              if Length(mType.GetParameters) = 0 then
              begin
                // invoke
                metaClass := rType.AsInstance.MetaclassType;
                Break;
              end;
            end;
          end;

          while not dst.EOF do
          begin
            obj := mType.Invoke(metaClass, []).AsObject;


            for AProp in props do
            begin
              if (AProp.IsWritable) and
                ( (mvPublic = AProp.Visibility) or (mvPublished = AProp.Visibility)) then
              begin
                fld := dst.FindField(AProp.Name);
                if Assigned(fld) then
                begin
                  AVal := TValue.FromVariant(fld.Value);

                  AProp.SetValue(obj, AVal);
                end;
              end;

            end;

            DataList.Add(obj);

            dst.Next;
          end;
          Result := True;
        end;
      finally
        dst.Free;
      end;
    end;
  end;
end;
{$ENDIF}

(*
function TSQLitePreparedStatement.FindParam(const name: string): TSQliteParam;
var
  par: TSQliteParam;
begin
  Result := nil;
  {TODO -oLinas -cGeneral : don't know if params should be case sensitive}
  for par in fParams do
  begin
    if (par.name = name) then
    begin
      Result := par;
      Break;
    end;
  end;
end;
*)

{
function TSQLitePreparedStatement.FindParam(const I: Integer): TSQliteParam;
begin
  Result := nil;

  if (I > 0) and (I <= fParams.Count) then
  begin
    Result := fParams[I-1];
  end;
end;
 }

function TSQLitePreparedStatement.GetParamCount: Integer;
begin
  Result := fParams.Count;
end;

function TSQLitePreparedStatement.GetParams(index: Integer): TSQliteParam;
begin
  Result := fParams[index];
end;

procedure TSQLitePreparedStatement.PrepareStatement(const SQL: string; const Params: array of TVarRec);
begin
  PrepareStatement(SQL);

  SetParams(Params);
end;

procedure TSQLitePreparedStatement.PrepareStatement(const SQL: string);
var
  NextSQLStatement: PChar;
begin
  if (SQL <> '') and (SQL <> FSQL) then
    Self.SQL := SQL;

 // if ParamCount > 0 then
 // begin
    if Sqlite3_Prepare16_v2(FDB.fDB, PChar(FSQL), -1, fStmt, NextSQLStatement) <> SQLITE_OK then
      FDB.RaiseError('Error executing SQL', FSQL);
    if (fStmt = nil) then
      FDB.RaiseError('Could not prepare SQL statement', FSQL);
 // end;
 // BindParams;
  DB.DoQuery(SQL);
end;

function TSQLitePreparedStatement.BindParameterCount: Integer;
begin
  if FStmt <> nil then
    Result := sqlite3_bind_parameter_count(FStmt)
  else
    Result := 0;
end;

function TSQLitePreparedStatement.BindParameterName(const i: Integer): string;
begin
  Result := '';
  if FStmt <> nil then
  begin
    Result := UTF8ToString(sqlite3_bind_parameter_name(FStmt, i));
  end;
end;

procedure TSQLitePreparedStatement.BindParams;
var
  n: integer;
  i, iBindResult: integer;
  par: TSQliteParam;
begin
  if (BindParameterCount = fParams.Count) and not (FParamsBound) and (FStmt <> nil) then
  begin
    for n := 0 to fParams.Count - 1 do
    begin
      par := fParams[n];
      if par.index < 1 then
      begin
        i := sqlite3_bind_parameter_index(fStmt, PAnsiChar(Ansistring(par.name)));
      end
      else
      begin
        i := par.index;
      end;


      if i > 0 then
      begin
        case par.valuetype of
          SQLITE_INTEGER:
            sqlite3_bind_int64(fStmt, i, par.valueinteger);
          SQLITE_FLOAT:
            sqlite3_bind_double(fStmt, i, par.valuefloat);
          SQLITE_TEXT:
          begin
           // sqlite3_bind_text(fStmt, i, PAnsiChar(par.valuedata),
           //   -1, SQLITE_TRANSIENT);
            // sqlite3_bind_text16 not working, don't know why
            //I guess it was incorrectly specified sqlite function name, corrected now
            //but our old method works ok so we don't change it now
            sqlite3_bind_text16(fStmt, i, PChar(par.valuedata),
              -1, SQLITE_TRANSIENT);
          end;
          SQLITE_BLOB:
          begin
            iBindResult := SQLite3_Bind_Blob(FStmt, i, par.valueptr, par.blobsize, @DisposePointer);
            if iBindResult <> SQLITE_OK then
                FDB.RaiseError('Error binding blob to database. Param index: ' + IntToStr(i), '');
          end;
          SQLITE_NULL:
            sqlite3_bind_null(fStmt, i);
        end;
      end;
    end;

    FParamsBound := True;
  end;
end;

{ TSQLiteField }

function TSQLiteField.AsBlob: TMemoryStream;
begin
  Result := Table.FieldAsBlob(Index);
end;

function TSQLiteField.AsBlobPtr(out iNumBytes: integer): Pointer;
begin
  Result := Table.FieldAsBlobPtr(Index, iNumBytes);
end;

function TSQLiteField.AsBlobText: string;
begin
  Result := Table.FieldAsBlobText(Index);
end;

function TSQLiteField.AsBlobTextDef(const Def: string): string;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsBlobText;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

function TSQLiteField.AsDateTime: TDateTime;
begin
  Result := VarToDateTime(Value);
end;

function TSQLiteField.AsDateTimeDef(const Def: TDateTime): TDateTime;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsDateTime;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

function TSQLiteField.AsDouble: Double;
begin
  Result := Table.FieldAsDouble(Index);
end;

function TSQLiteField.AsDoubleDef(const Def: Double): Double;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsDouble;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

function TSQLiteField.AsInteger: Int64;
begin
  Result := Table.FieldAsInteger(Index);
end;

function TSQLiteField.AsIntegerDef(const Def: Int64): Int64;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsInteger;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

function TSQLiteField.AsString: string;
begin
  Result := Table.FieldsAsString[Index];
end;

function TSQLiteField.AsStringDef(const Def: string): string;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsString;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

constructor TSQLiteField.Create;
begin
  inherited Create();

end;

destructor TSQLiteField.Destroy;
begin
  Name := '';
  inherited Destroy;
end;

function TSQLiteField.IsNull: Boolean;
begin
  Result := Table.FieldIsNull(Index);
end;


function TSQLiteField.Value: Variant;
begin
  Result := Table.FieldsVal[Index];
end;

function TSQLiteField.ValueDef(const Def: Variant): Variant;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := Value;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

{ TSQLiteFunctions }

{$IFNDEF NEXTGEN}

procedure TSQLiteFunctions.AddAggregateFunction(const FuncName: AnsiString; ArgCount: Integer;
  AStepFunc: TSQLiteUserFunc; AFinalFunc: TSQLiteUserFuncFinal);
var
  iRes: Integer;
  Val: PSQLiteFuncs;
begin
  New(Val);
  Val.FuncName := UpperCase(UTF8ToString(FuncName));
  Val.Funcs := Self;
  Val.AFunc := nil;
  Val.AStepFunc := AStepFunc;
  Val.AFinalFunc := AFinalFunc;
  Val.FuncType := sftScalar;
  FVals.Add(Val);

  iRes := sqlite3_create_function(FDB.fDB, PAnsiChar(FuncName), ArgCount, SQLITE_ANY,
    Val, nil, @AggStepDefFunc, @AggFinalDefFunc);

  if iRes <> SQLITE_OK then
  begin
    FDB.RaiseError('Cannot add aggregate function ' + UTF8ToString(FuncName),'');
  end;
end;

procedure TSQLiteFunctions.AddScalarFunction(const FuncName: AnsiString; ArgCount: Integer;
  AFunc: TSQLiteUserFunc);
var
  iRes: Integer;
  Val: PSQLiteFuncs;
begin
  New(Val);
  Val.FuncName := UpperCase(UTF8ToString(FuncName));
  Val.Funcs := Self;
  Val.AFunc := AFunc;
  Val.AStepFunc := nil;
  Val.AFinalFunc := nil;
  Val.FuncType := sftScalar;
  FVals.Add(Val);

  iRes := sqlite3_create_function(FDB.fDB, PAnsiChar(FuncName), ArgCount, SQLITE_ANY,
    Val, @ScalarDefFunc, nil, nil);
  if iRes <> SQLITE_OK then
  begin
    FDB.RaiseError('Cannot add scalar function ' + UTF8ToString(FuncName),'');
  end;
end;

{$ENDIF}

procedure TSQLiteFunctions.Clear;
var
  Val: PSQLiteFuncs;
begin
  for Val in FVals do
  begin
   // sqlite3_create_function(FDB.fDB, PAnsiChar(Val.FuncName), 0, SQLITE_ANY,
  //    nil, nil, nil, nil);
    Dispose(Val);
  end;
  FVals.Clear;
end;

constructor TSQLiteFunctions.Create(const DB: TSQLiteDatabase);
begin
  inherited Create();
  FDB := DB;
  FVals := TList<PSQLiteFuncs>.Create;
end;

destructor TSQLiteFunctions.Destroy;
begin
  Clear;
  FVals.Free;
  FDB := nil;
  inherited;
end;

class function TSQLiteFunctions.GetArrayElement(Elements: PPsqlite3_value;
  Index: Integer): Psqlite3_value;
begin
  Result := Psqlite3_value(PByte(Elements) + Index * SizeOf(Elements^))^
end;

class function TSQLiteFunctions.GetValueType(ArgValue: Psqlite3_value): Integer;
begin
  Result := sqlite3_value_type(ArgValue);
end;

class procedure TSQLiteFunctions.ResultAsFloat(sqlite3_context: Psqlite3_context; Val: Double);
begin
  sqlite3_result_double(sqlite3_context, Val);
end;

class procedure TSQLiteFunctions.ResultAsInteger(sqlite3_context: Psqlite3_context; Val: Int64);
begin
  sqlite3_result_int64(sqlite3_context, Val);
end;

class procedure TSQLiteFunctions.ResultAsNull(sqlite3_context: Psqlite3_context);
begin
  sqlite3_result_null(sqlite3_context);
end;

class procedure TSQLiteFunctions.ResultAsString(sqlite3_context: Psqlite3_context; Val: string);
begin
  sqlite3_result_text16(sqlite3_context, PChar(Val), -1, nil);
end;

class function TSQLiteFunctions.ValueAsFloat(ArgValue: Psqlite3_value): Double;
begin
  Result := sqlite3_value_double(ArgValue);
end;

class function TSQLiteFunctions.ValueAsInteger(ArgValue: Psqlite3_value): Int64;
begin
  Result := sqlite3_value_int64(ArgValue);
end;

class function TSQLiteFunctions.ValueAsString(ArgValue: Psqlite3_value): string;
begin
  Result := sqlite3_value_text16(ArgValue);
end;

{ TSQLiteUniTable.TUniTableEnumerator }

constructor TUniTableEnumerator.Create(const ATable: TSQLiteUniTable);
begin
  inherited Create();
  FDataSet := ATable;
  FMoveToFirst := True;
end;

destructor TUniTableEnumerator.Destroy;
begin
  FDataSet := nil;
  inherited Destroy;
end;

function TUniTableEnumerator.GetCurrent: Variant;
begin
  Result := FDataSet.CurrentRec;
end;

function TUniTableEnumerator.MoveNext: Boolean;
begin
  if FMoveToFirst then
  begin
    FMoveToFirst := False;
    Result := not FDataSet.EOF;
  end
  else
    Result := FDataSet.Next;
end;

{ AnsiString }

{$IFDEF USE_CUSTOM_ANSISTRING}

class operator AnsiString.Explicit(const s: AnsiString): PAnsiChar;
begin
  if Length(s.FBytes) = 0 then
    Result := nil
  else Result := @s.FBytes[0];
end;

class operator AnsiString.Implicit(const s: string): AnsiString;
var
  Enc: TEncoding;
  Len: Integer;
begin
  if s = '' then
  begin
    SetLength(Result.FBytes, 0);
    Exit;
  end;

  Enc := TEncoding.ANSI;
  Len := Enc.GetByteCount(s);
  // Add trailing zero
  SetLength(Result.FBytes, Len + 1);
  Enc.GetBytes(s, Low(string), Length(s), Result.FBytes, 0);
  Result.FBytes[Len] := 0;
end;

class operator AnsiString.Implicit(const s: AnsiString): string;
begin
  if Length(s.FBytes) = 0 then
    Result := ''
  else Result := TEncoding.ANSI.GetString(s.FBytes);
end;

{$ENDIF}

{ ESQLiteException }

constructor ESQLiteException.Create(const Msg: string; ErrorCode: Integer);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

constructor ESQLiteException.CreateFmt(const Msg: string;
  const Args: array of const; ErrorCode: Integer);
begin
  inherited CreateFmt(Msg, Args);
  FErrorCode := ErrorCode;
end;

initialization
  TSQLiteDatabase.FColumnTypes := TDictionary<string,Integer>.Create(DEF_COLCOUNT);
  TSQLiteDatabase.InitDefaultColumnTypes;
  VarDataRecordType := TVarDataRecordType.Create;
finalization
  TSQLiteDatabase.FColumnTypes.Free;
  FreeAndNil(VarDataRecordType);
end.

