unit SQLite3;
{$I SQLite3.inc}
{
  Simplified interface for SQLite.
  Updated for Sqlite 3 by Tim Anderson (tim@itwriting.com)
  Note: NOT COMPLETE for version 3, just minimal functionality
  Adapted from file created by Pablo Pissanetzky (pablo@myhtpc.net)
  which was based on SQLite.pas by Ben Hochstrasser (bhoc@surfeu.ch)
  Modified, adopted to MacOSX and enhanced by Linas Naginionis (lnaginionis@gmail.com)
}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

const
{$IFDEF MSWINDOWS}
  SQLiteDLL = 'sqlite3.dll';
{$ELSE}
 // SQLiteDLL = 'sqlite3.so';
  SQLiteDLL = 'libsqlite3.dylib';
  //{$LINK libsqlite3}
{$ENDIF}
// Return values for sqlite3_exec() and sqlite3_step()
type
  TSQLite3Destructor = procedure(Ptr: Pointer); cdecl;

const
  SQLITE_OK          =  0; // Successful result
  (* beginning-of-error-codes *)
  SQLITE_ERROR       =  1; // SQL error or missing database
  SQLITE_INTERNAL    =  2; // An internal logic error in SQLite
  SQLITE_PERM        =  3; // Access permission denied
  SQLITE_ABORT       =  4; // Callback routine requested an abort
  SQLITE_BUSY        =  5; // The database file is locked
  SQLITE_LOCKED      =  6; // A table in the database is locked
  SQLITE_NOMEM       =  7; // A malloc() failed
  SQLITE_READONLY    =  8; // Attempt to write a readonly database
  SQLITE_INTERRUPT   =  9; // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR       = 10; // Some kind of disk I/O error occurred
  SQLITE_CORRUPT     = 11; // The database disk image is malformed
  SQLITE_NOTFOUND    = 12; // (Internal Only) Table or record not found
  SQLITE_FULL        = 13; // Insertion failed because database is full
  SQLITE_CANTOPEN    = 14; // Unable to open the database file
  SQLITE_PROTOCOL    = 15; // Database lock protocol error
  SQLITE_EMPTY       = 16; // Database is empty
  SQLITE_SCHEMA      = 17; // The database schema changed
  SQLITE_TOOBIG      = 18; // Too much data for one row of a table
  SQLITE_CONSTRAINT  = 19; // Abort due to contraint violation
  SQLITE_MISMATCH    = 20; // Data type mismatch
  SQLITE_MISUSE      = 21; // Library used incorrectly
  SQLITE_NOLFS       = 22; // Uses OS features not supported on host
  SQLITE_AUTH        = 23; // Authorization denied
  SQLITE_FORMAT      = 24; // Auxiliary database format error
  SQLITE_RANGE       = 25; // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB      = 26; // File opened that is not a database file
  SQLITE_ROW         = 100; // sqlite3_step() has another row ready
  SQLITE_DONE        = 101; // sqlite3_step() has finished executing

  {authorizer returns}
  SQLITE_DENY  = 1;
  SQLITE_IGNORE = 2;

  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_TEXT    = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

  SQLITE_UTF8     = 1;
  SQLITE_UTF16    = 2;
  SQLITE_UTF16BE  = 3;
  SQLITE_UTF16LE  = 4;
  SQLITE_ANY      = 5;

  SQLITE_OPEN_READONLY = $00000001;
  /// The database is opened for reading and writing if possible, or reading
  // only if the file is write protected by the operating system
  // - In either case the database must already exist, otherwise an error is
  // returned
  // - Ok for sqlite3.open_v2()
  SQLITE_OPEN_READWRITE = $00000002;
  /// In conjunction with SQLITE_OPEN_READWRITE, optionally create the database
  // file if it does not exist
  // - The database is opened for reading and writing if possible, or reading
  // only if the file is write protected by the operating system
  // - In either case the database must already exist, otherwise an error is returned
  SQLITE_OPEN_CREATE = $00000004;
  /// URI filename interpretation is enabled if the SQLITE_OPEN_URI flag is set
  // in the fourth argument to sqlite3.open_v2(), or if it has been enabled
  // globally using the SQLITE_CONFIG_URI option with the sqlite3.config() method
  // or by the SQLITE_USE_URI compile-time option.
  // - As of SQLite version 3.7.7, URI filename interpretation is turned off by
  // default, but future releases of SQLite might enable URI filename
  // interpretation by default
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_URI = $00000040;  // Ok for sqlite3_open_v2()
  /// If the SQLITE_OPEN_NOMUTEX flag is set, then the database will remain in
  // memory
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_MEMORY = $00000080;  // Ok for sqlite3_open_v2()
  /// If the SQLITE_OPEN_NOMUTEX flag is set, then the database connection opens
  // in the multi-thread threading mode as long as the single-thread mode has
  // not been set at compile-time or start-time
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_NOMUTEX = $00008000;  // Ok for sqlite3_open_v2()
  /// If the SQLITE_OPEN_FULLMUTEX flag is set then the database connection opens
  // in the serialized threading mode unless single-thread was previously selected
  // at compile-time or start-time
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_FULLMUTEX = $00010000;  // Ok for sqlite3_open_v2()
  /// The SQLITE_OPEN_SHAREDCACHE flag causes the database connection to be
  // eligible to use shared cache mode, regardless of whether or not shared
  // cache is enabled using sqlite3.enable_shared_cache()
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_SHAREDCACHE = $00020000;  // Ok for sqlite3_open_v2()
  /// The SQLITE_OPEN_PRIVATECACHE flag causes the database connection to not
  // participate in shared cache mode even if it is enabled
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_PRIVATECACHE = $00040000;

  SQLITE_STATIC    : TSQLite3Destructor = TSQLite3Destructor(0);
  SQLITE_TRANSIENT : TSQLite3Destructor = TSQLite3Destructor(-1);

type
  TSQLiteDB = type Pointer;
  TSQLiteResult = type PPAnsiChar;
  TSQLiteStmt = type Pointer;
  TSQLiteBackup = type pointer;

  Psqlite3_context = type pointer;
  Psqlite3_value = ^sqlite3_value;
  PPsqlite3_value = ^Psqlite3_value;
  sqlite3_value = type pointer;
  sqlite3_int64 = Int64;

  {Authorizer Action Codes}
  TSQLiteActionCode = (
    SQLITE_CREATE_INDEX          =1,   { Index Name      Table Name      }
    SQLITE_CREATE_TABLE          =2,   { Table Name      NULL            }
    SQLITE_CREATE_TEMP_INDEX     =3,   { Index Name      Table Name      }
    SQLITE_CREATE_TEMP_TABLE     =4,   { Table Name      NULL            }
    SQLITE_CREATE_TEMP_TRIGGER   =5,   { Trigger Name    Table Name      }
    SQLITE_CREATE_TEMP_VIEW      =6,   { View Name       NULL            }
    SQLITE_CREATE_TRIGGER        =7,   { Trigger Name    Table Name      }
    SQLITE_CREATE_VIEW           =8,   { View Name       NULL            }
    SQLITE_DELETE                =9,   { Table Name      NULL            }
    SQLITE_DROP_INDEX           =10,   { Index Name      Table Name      }
    SQLITE_DROP_TABLE           =11,   { Table Name      NULL            }
    SQLITE_DROP_TEMP_INDEX      =12,   { Index Name      Table Name      }
    SQLITE_DROP_TEMP_TABLE      =13,   { Table Name      NULL            }
    SQLITE_DROP_TEMP_TRIGGER    =14,   { Trigger Name    Table Name      }
    SQLITE_DROP_TEMP_VIEW       =15,   { View Name       NULL            }
    SQLITE_DROP_TRIGGER         =16,   { Trigger Name    Table Name      }
    SQLITE_DROP_VIEW            =17,   { View Name       NULL            }
    SQLITE_INSERT               =18,   { Table Name      NULL            }
    SQLITE_PRAGMA               =19,   { Pragma Name     1st arg or NULL }
    SQLITE_READ                 =20,   { Table Name      Column Name     }
    SQLITE_SELECT               =21,   { NULL            NULL            }
    SQLITE_TRANSACTION          =22,   { Operation       NULL            }
    SQLITE_UPDATE               =23,   { Table Name      Column Name     }
    SQLITE_ATTACH               =24,   { Filename        NULL            }
    SQLITE_DETACH               =25,   { Database Name   NULL            }
    SQLITE_ALTER_TABLE          =26,   { Database Name   Table Name      }
    SQLITE_REINDEX              =27,   { Index Name      NULL            }
    SQLITE_ANALYZE              =28,   { Table Name      NULL            }
    SQLITE_CREATE_VTABLE        =29,   { Table Name      Module Name     }
    SQLITE_DROP_VTABLE          =30,   { Table Name      Module Name     }
    SQLITE_FUNCTION             =31,   { NULL            Function Name   }
    SQLITE_SAVEPOINT            =32,   { Operation       Savepoint Name  }
    SQLITE_COPY                 = 0);   { No longer used }


  TxFinal = procedure(sqlite3_context: Psqlite3_context);
  TxFunc = procedure(sqlite3_context: Psqlite3_context; cArg: integer; ArgV: PPsqlite3_value);
  TxStep = procedure(sqlite3_context: Psqlite3_context; cArg: integer; ArgV: PPsqlite3_value);
  TxAuth = function(pUserData: Pointer; ActionCode: Integer; Det1, Det2, Det3, Det4: PAnsiChar): Integer;
  {void(*)(void *,int ,char const *,char const *,sqlite3_int64),}
  TxUpdHook = procedure(pUserData: Pointer; Operation: Integer; DbName: PAnsiChar;
    TableName: PAnsiChar; ARowID: Int64);

  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = array[0 .. (MaxInt div SizeOf(PAnsiChar))-1] of PAnsiChar;


  TSQLiteExecCallback = function(UserData: Pointer; NumCols: integer; ColValues:
    PPAnsiCharArray; ColNames: PPAnsiCharArray): integer; cdecl;
  TSQLiteBusyHandlerCallback = function(UserData: Pointer; P2: integer): integer; cdecl;

  //function prototype for define own collate
  TCollateXCompare = function(UserData: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;

var
  sqlite3_open: function(filename: PAnsiChar; var db: TSQLiteDB): integer; cdecl; //'sqlite3_open';
  sqlite3_open16: function(filename: PWideChar; var db: TSQLiteDB): integer; cdecl; //sqlite3_open16
  sqlite3_open_v2: function(filename: PAnsiChar; var db: TSQLiteDB; flags: integer; zVfs: PAnsiChar): integer; cdecl; //sqlite3_open_v2
  SQLite3_Close: function(db: TSQLiteDB): integer; cdecl; // 'sqlite3_close';
  SQLite3_Exec: function(db: TSQLiteDB; SQLStatement: PAnsiChar; CallbackPtr: TSQLiteExecCallback; UserData: Pointer; var ErrMsg: PAnsiChar): integer; cdecl; // 'sqlite3_exec';
  SQLite3_Version: function(): PAnsiChar; cdecl; // 'sqlite3_libversion';
  SQLite3_ErrMsg: function(db: TSQLiteDB): PAnsiChar; cdecl; // 'sqlite3_errmsg';
  SQLite3_ErrMsg16: function(db: TSQLiteDB): PWideChar; cdecl; // 'sqlite3_errmsg16';
  SQLite3_ErrCode: function(db: TSQLiteDB): integer; cdecl; // 'sqlite3_errcode';
  SQlite3_Free: procedure(P: pointer); cdecl; // 'sqlite3_free';
  sqlite3_mprintf: function (format: PAnsiChar): PAnsiChar varargs; cdecl; // 'sqlite3_mprintf'
  SQLite3_GetTable: function(db: TSQLiteDB; SQLStatement: PAnsiChar; var ResultPtr: TSQLiteResult; var RowCount: Cardinal; var ColCount: Cardinal; var ErrMsg: PAnsiChar): integer; cdecl; // 'sqlite3_get_table';
  SQLite3_FreeTable: procedure(Table: TSQLiteResult); cdecl; // 'sqlite3_free_table';
  SQLite3_Complete: function(P: PAnsiChar): boolean; cdecl; // 'sqlite3_complete';
  SQLite3_LastInsertRowID: function(db: TSQLiteDB): int64; cdecl; // 'sqlite3_last_insert_rowid';
  SQLite3_Interrupt: procedure(db: TSQLiteDB); cdecl; // 'sqlite3_interrupt';
  SQLite3_BusyHandler: procedure(db: TSQLiteDB; CallbackPtr: TSQLiteBusyHandlerCallback; UserData: Pointer); cdecl; // 'sqlite3_busy_handler';
  SQLite3_BusyTimeout: procedure(db: TSQLiteDB; TimeOut: integer); cdecl; // 'sqlite3_busy_timeout';
  SQLite3_Changes: function(db: TSQLiteDB): integer; cdecl; // 'sqlite3_changes';
  SQLite3_TotalChanges: function(db: TSQLiteDB): integer; cdecl; // 'sqlite3_total_changes';
  SQLite3_Prepare: function(db: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PAnsiChar): integer; cdecl; // 'sqlite3_prepare';
  SQLite3_Prepare_v2: function(db: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PAnsiChar): integer; cdecl; // 'sqlite3_prepare_v2';
  SQLite3_Prepare16: function(db: TSQLiteDB; SQLStatement: PWideChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PWideChar): integer; cdecl; // 'sqlite3_prepare16';
  SQLite3_Prepare16_v2: function(db: TSQLiteDB; SQLStatement: PWideChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PWideChar): integer; cdecl; // 'sqlite3_prepare16_v2';
  SQLite3_ColumnCount: function(hStmt: TSqliteStmt): integer; cdecl; // 'sqlite3_column_count';
  SQLite3_ColumnName: function(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl; // 'sqlite3_column_name';
  SQLite3_ColumnName16: function(hStmt: TSqliteStmt; ColNum: integer): PWideChar; cdecl; // 'sqlite3_column_name16';
  SQLite3_ColumnDeclType: function(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl; // 'sqlite3_column_decltype';
  SQLite3_ColumnDeclType16: function(hStmt: TSqliteStmt; ColNum: integer): PWideChar; cdecl; // 'sqlite3_column_decltype16';
  SQLite3_Step: function(hStmt: TSqliteStmt): integer; cdecl; // 'sqlite3_step';
  SQLite3_DataCount: function(hStmt: TSqliteStmt): integer; cdecl; // 'sqlite3_data_count';
  sqlite3_memory_used: function(): Int64; cdecl; // sqlite3_memory_used
  sqlite3_key: function(db: TSQLiteDB; Key: PAnsiChar; Len: Integer): Integer; cdecl; //sqlite3_key
  sqlite3_rekey: function(db: TSQLiteDB; Key: PAnsiChar; Len: Integer): Integer; cdecl; //sqlite3_rekey
  sqlite3_set_authorizer: function(db: TSQLiteDB; xAuth: TxAuth; pUserData: Pointer): Integer; cdecl; //sqlite3_set_authorizer
  sqlite3_update_hook: procedure(db: TSQLiteDB; UpdHookProc: TxUpdHook; pUserData: Pointer); cdecl; //sqlite3_update_hook
  sqlite3_table_column_metadata: function(db: TSQLiteDB; zDbName: PAnsiChar; zTableName: PAnsiChar; zColumnName: PAnsiChar;
    var pzDataType: PAnsiChar; var pzCollSeq: PAnsiChar; var pNotNull: Integer; var pPrimaryKey: Integer; var pAutoinc: Integer): Integer; cdecl;
  sqlite3_enable_load_extension: function(db: TSQLiteDB; onoff: Integer): Integer; cdecl;
  sqlite3_load_extension: function(db: TSQLiteDB; zFile: PAnsiChar; zProc: PAnsiChar; var pzErrMsg: PAnsiChar): Integer; cdecl;

  SQLite3_ColumnBlob: function(hStmt: TSqliteStmt; ColNum: integer): pointer; cdecl; // 'sqlite3_column_blob';
  SQLite3_ColumnBytes: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl; // 'sqlite3_column_bytes';
  SQLite3_ColumnBytes16: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl; // 'sqlite3_column_bytes16';
  SQLite3_ColumnDouble: function(hStmt: TSqliteStmt; ColNum: integer): double; cdecl; // 'sqlite3_column_double';
  SQLite3_ColumnInt: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl; // 'sqlite3_column_int';
  SQLite3_ColumnText: function(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl; // 'sqlite3_column_text';
  SQLite3_ColumnText16: function(hStmt: TSqliteStmt; ColNum: integer): PWideChar; cdecl; // 'sqlite3_column_text16';
  SQLite3_ColumnType: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl; // 'sqlite3_column_type';
  SQLite3_ColumnInt64: function(hStmt: TSqliteStmt; ColNum: integer): Int64; cdecl; // 'sqlite3_column_int64';
  SQLite3_Finalize: function(hStmt: TSqliteStmt): integer; cdecl; // 'sqlite3_finalize';
  SQLite3_Reset: function(hStmt: TSqliteStmt): integer; cdecl; // 'sqlite3_reset';

//The database name is "main" for the main database, "temp" for the temporary database, or the name specified after the AS keyword in an ATTACH statement for an attached database.
  SQLite3_Backup_Init: function(DestDb: TSQLiteDB; DestDbName: PAnsiChar; SourceDb: TSQLiteDB; SourceDbName: PAnsiChar): TSqliteBackup; cdecl; // 'sqlite3_backup_init';
  SQLite3_Backup_Step: function(hBackup: TSQLiteBackup; nPage: integer): integer; cdecl; // 'sqlite3_backup_step';
  SQLite3_Backup_Finish: function(hBackup: TSQLiteBackup): integer; cdecl; // 'sqlite3_backup_finish';
  SQLite3_Backup_Remaining: function(hBackup: TSQLiteBackup): integer; cdecl; // 'sqlite3_backup_remaining';
  SQLite3_Backup_Pagecount: function(hBackup: TSQLiteBackup): integer; cdecl; // 'sqlite3_backup_pagecount';

//
// In the SQL strings input to sqlite3_prepare() and sqlite3_prepare16(),
// one or more literals can be replace by a wildcard "?" or ":N:" where
// N is an integer.  These value of these wildcard literals can be set
// using the routines listed below.
//
// In every case, the first parameter is a pointer to the sqlite3_stmt
// structure returned from sqlite3_prepare().  The second parameter is the
// index of the wildcard.  The first "?" has an index of 1.  ":N:" wildcards
// use the index N.
// 
// The fifth parameter to sqlite3_bind_blob(), sqlite3_bind_text(), and
//sqlite3_bind_text16() is a destructor used to dispose of the BLOB or
//text after SQLite has finished with it.  If the fifth argument is the
// special value SQLITE_STATIC, then the library assumes that the information
// is in static, unmanaged space and does not need to be freed.  If the
// fifth argument has the value SQLITE_TRANSIENT, then SQLite makes its
// own private copy of the data.
// 
// The sqlite3_bind_* routine must be called before sqlite3_step() after
// an sqlite3_prepare() or sqlite3_reset().  Unbound wildcards are interpreted
// as NULL.
// 

var
 sqlite3_bind_blob: function(hStmt: TSqliteStmt; ParamNum: integer;
  ptrData: pointer; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer;
cdecl; // 'sqlite3_bind_blob';
 sqlite3_bind_text: function(hStmt: TSqliteStmt; ParamNum: integer;
  Text: PAnsiChar; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer;
cdecl; // 'sqlite3_bind_text';
 sqlite3_bind_text16: function(hStmt: TSqliteStmt; ParamNum: integer;
  Text: PWideChar; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer;
cdecl; // 'sqlite3_bind_text';
 sqlite3_bind_double: function(hStmt: TSqliteStmt; ParamNum: integer; Data: Double): integer;
  cdecl; // 'sqlite3_bind_double';
 sqlite3_bind_int: function(hStmt: TSqLiteStmt; ParamNum: integer; Data: integer): integer;
  cdecl; // 'sqlite3_bind_int';
 sqlite3_bind_int64: function(hStmt: TSqliteStmt; ParamNum: integer; Data: int64): integer;
  cdecl; // 'sqlite3_bind_int64';
 sqlite3_bind_null: function(hStmt: TSqliteStmt; ParamNum: integer): integer;
  cdecl; // 'sqlite3_bind_null';

 sqlite3_clear_bindings: function(hStmt: TSqliteStmt): integer;
  cdecl; // 'sqlite3_clear_bindings';
 //int sqlite3_clear_bindings(sqlite3_stmt*);
 sqlite3_bind_parameter_index: function(hStmt: TSqliteStmt; zName: PAnsiChar): integer;
  cdecl; // 'sqlite3_bind_parameter_index';
 sqlite3_bind_parameter_name: function(hStmt: TSqliteStmt; zIndex: integer): PAnsiChar;
  cdecl; // 'sqlite3_bind_parameter_name';
 sqlite3_bind_parameter_count: function(hStmt: TSqliteStmt): integer;
  cdecl; // 'sqlite3_bind_parameter_count';

  sqlite3_create_function: function(db: TSQLiteDB; functionName: PAnsiChar; nArg: integer;
    eTextRep: integer; pUserdata: pointer; xFunc: TxFunc; xStep: TxStep; xFinal: TxFinal
    ): integer; cdecl; // 'sqlite3_create_function';

  sqlite3_result_blob: procedure(sqlite3_context: Psqlite3_context; value: Pointer;
    n: integer; destroy: TSQLite3Destructor); cdecl; // 'sqlite3_result_blob';
  sqlite3_result_double: procedure(sqlite3_context: Psqlite3_context; value: Double);
    cdecl; // 'sqlite3_result_double';
  sqlite3_result_error: procedure(sqlite3_context: Psqlite3_context; value: PAnsiChar;
    n: integer); cdecl; // 'sqlite3_result_error';
  sqlite3_result_error16: procedure(sqlite3_context: Psqlite3_context; value: PWideChar;
    n: integer); cdecl; // 'sqlite3_result_error16';
  sqlite3_result_int: procedure(sqlite3_context: Psqlite3_context; value: integer);
    cdecl; // 'sqlite3_result_int';
  sqlite3_result_int64: procedure(sqlite3_context: Psqlite3_context; value: int64);
    cdecl; // 'sqlite3_result_int64';
  sqlite3_result_null: procedure(sqlite3_context: Psqlite3_context);
    cdecl; // 'sqlite3_result_null';
  sqlite3_result_text: procedure(sqlite3_context: Psqlite3_context; value: PAnsiChar;
    n: integer; destroy: TSQLite3Destructor); cdecl; // 'sqlite3_result_text';
  sqlite3_result_text16: procedure(sqlite3_context: Psqlite3_context; value: PWideChar;
    n: integer; destroy: TSQLite3Destructor); cdecl; // 'sqlite3_result_text16';
  sqlite3_result_text16be: procedure(sqlite3_context: Psqlite3_context; value: PWideChar;
    n: integer; destroy: TSQLite3Destructor); cdecl; // 'sqlite3_result_text16be';
  sqlite3_result_text16le: procedure(sqlite3_context: Psqlite3_context; value: PWideChar;
    n: integer; destroy: TSQLite3Destructor); cdecl; // 'sqlite3_result_text16le';
  sqlite3_result_value: procedure(sqlite3_context: Psqlite3_context; value: Psqlite3_value);
    cdecl; // 'sqlite3_result_value';
  sqlite3_user_data: function(sqlite3_context: Psqlite3_context): Pointer; cdecl;
  //void *sqlite3_user_data(sqlite3_context*);
  sqlite3_aggregate_context: function(sqlite3_context: Psqlite3_context; nBytes: Integer): Pointer; cdecl;
  //void *sqlite3_aggregate_context(sqlite3_context*, int nBytes);

  sqlite3_value_blob: function(value: Psqlite3_value): Pointer;
    cdecl; // 'sqlite3_value_blob';
  sqlite3_value_bytes: function(value: Psqlite3_value): integer;
    cdecl; // 'sqlite3_value_bytes';
  sqlite3_value_bytes16: function(value: Psqlite3_value): integer;
    cdecl; // 'sqlite3_value_bytes16';
  sqlite3_value_double: function(value: Psqlite3_value): double;
    cdecl; // 'sqlite3_value_double';
  sqlite3_value_int: function(value: Psqlite3_value): integer;
    cdecl; // 'sqlite3_value_int';
  sqlite3_value_int64: function(value: Psqlite3_value): int64;
    cdecl; // 'sqlite3_value_int64';
  sqlite3_value_text: function(value: Psqlite3_value): PAnsiChar;
    cdecl; // 'sqlite3_value_text';
  sqlite3_value_text16: function(value: Psqlite3_value): PWideChar;
    cdecl; // 'sqlite3_value_text16';
  sqlite3_value_text16be: function(value: Psqlite3_value): PWideChar;
    cdecl; // 'sqlite3_value_text16be';
  sqlite3_value_text16le: function(value: Psqlite3_value): PWideChar;
    cdecl; // 'sqlite3_value_text16le';
  sqlite3_value_type: function(value: Psqlite3_value): integer;
    cdecl; // 'sqlite3_value_type';
  {
    //Sample of usage:
    PROCEDURE fn(ctx:pointer;n:integer;args:ppansichar);cdecl;
    VAR     p : ppansichar; theString : string; res:integer;
    BEGIN
    p         := args;
    theString := trim(sqlite3_value_text(p^));

    ...do something with theString...

    sqlite3_result_int(ctx,res);  // < return a number based on string
    END;
    ...
    var i:integer;
    begin
    i := sqlite3_create_function(db3,'myfn',1,SQLITE_UTF8,nil,@fn,nil,nil);
    s := 'select myfn(thestring) from theTable;'
    ...execute statement...
    end;
    }


 SQLite_Handle: THandle = 0;

 sqlite3_enable_shared_cache: function(Value: integer): integer; cdecl; // 'sqlite3_enable_shared_cache';

//user collate definiton
 SQLite3_create_collation: function(db: TSQLiteDB; Name: PAnsiChar; eTextRep: integer;
  UserData: pointer; xCompare: TCollateXCompare): integer; cdecl; // 'sqlite3_create_collation';
 SQLite3_create_collation16: function(db: TSQLiteDB; Name: PWideChar; eTextRep: integer;
  UserData: pointer; xCompare: TCollateXCompare): integer; cdecl; // 'sqlite3_create_collation';

type
  Psqlite3_module = ^sqlite3_module;

  Psqlite3_vtab = ^sqlite3_vtab;
  sqlite3_vtab = record
    pModule: Psqlite3_module;            // The module for this virtual table
    nRef: Integer;                       // NO LONGER USED
    zErrMsg: PAnsiChar;                  // Error message from sqlite3_mprintf()
    // Virtual table implementations will typically add additional fields
  end;

  Psqlite3_index_constraint = ^sqlite3_index_constraint;
  sqlite3_index_constraint = record
    iColumn: Integer;              // Column on left-hand side of constraint
    op: Byte;                      // Constraint operator
    usable: ByteBool;              // True if this constraint is usable
    iTermOffset: Integer;          // Used internally - xBestIndex should ignore
  end;

  Psqlite3_index_orderby = ^sqlite3_index_orderby;
  sqlite3_index_orderby = record
    iColumn: Integer;     // Column number
    desc: ByteBool;       // True for DESC.  False for ASC.
  end;

  Psqlite3_index_constraint_usage = ^sqlite3_index_constraint_usage;
  sqlite3_index_constraint_usage = record
    argvIndex: Integer;  // if >0, constraint is part of argv to xFilter
    omit: ByteBool;      // Do not code a test for this constraint
  end;

  Psqlite3_index_info = ^sqlite3_index_info;
  sqlite3_index_info = record
    // Inputs
    nConstraint: Integer;     // Number of entries in aConstraint
    aConstraint: Psqlite3_index_constraint; // Table of WHERE clause constraints
    nOrderBy: Integer;        // Number of terms in the ORDER BY clause
    aOrderBy: Psqlite3_index_orderby;         // The ORDER BY clause

    // Outputs
    aConstraintUsage: Psqlite3_index_constraint_usage;
    idxNum: Integer;                // Number used to identify the index
    idxStr: PAnsiChar;              // String, possibly obtained from sqlite3_malloc
    needToFreeIdxStr: LongBool;      // Free idxStr using sqlite3_free() if true
    orderByConsumed: LongBool;       // True if output is already ordered
    estimatedCost: Double;      // Estimated cost of using this index
  end;

  Psqlite3_vtab_cursor = ^sqlite3_vtab_cursor;
  sqlite3_vtab_cursor = record
    pVtab: Psqlite3_vtab;      // Virtual table of this cursor
    // Virtual table implementations will typically add additional fields
  end;

  sqlite3_module = record
    iVersion: Integer;
    xCreate: function (db: TSQLiteDB; pAux: Pointer;
                 argc: Integer; argv: PPAnsiChar;
                 out ppVTab: Psqlite3_vtab; out pzErr: PAnsiChar): Integer; cdecl;
    xConnect: function (db: TSQLiteDB; pAux: Pointer;
                 argc: Integer; argv: PPAnsiChar;
                 var ppVTab: Psqlite3_vtab; out pzErr: PAnsiChar): Integer; cdecl;
    xBestIndex: function(pVTab: Psqlite3_vtab; pIndexInfo: Psqlite3_index_info): Integer; cdecl;
    xDisconnect: function(pVTab: Psqlite3_vtab): Integer; cdecl;
    xDestroy: function(pVTab: Psqlite3_vtab): Integer; cdecl;
    xOpen: function(pVTab: Psqlite3_vtab; var ppCursor: Psqlite3_vtab_cursor): Integer; cdecl;
    xClose: function(pCursor: Psqlite3_vtab_cursor): Integer; cdecl;
    xFilter: function(pCursor: Psqlite3_vtab_cursor; idxNum: Integer; idxStr: PAnsiChar;
                  argc: Integer; argv: PPsqlite3_value): Integer; cdecl;
    xNext: function(pCursor: Psqlite3_vtab_cursor): Integer; cdecl;
    xEof: function(pCursor: Psqlite3_vtab_cursor): LongBool; cdecl;
    xColumn: function(pCursor: Psqlite3_vtab_cursor; pContext: Psqlite3_context; N: Integer): Integer; cdecl;
    xRowid: function(pCursor: Psqlite3_vtab_cursor; out pRowid: sqlite3_int64): Integer; cdecl;
    xUpdate: function(pVTab: Psqlite3_vtab; argc: Integer; argv: PPsqlite3_value; var pRowId: sqlite3_int64): Integer; cdecl;
    xBegin: function(pVTab: Psqlite3_vtab): Integer; cdecl;
    xSync: function(pVTab: Psqlite3_vtab): Integer; cdecl;
    xCommit: function(pVTab: Psqlite3_vtab): Integer; cdecl;
    xRollback: function(pVTab: Psqlite3_vtab): Integer; cdecl;
    xFindFunction: function(pVTab: Psqlite3_vtab; nArg: Integer; zName: PAnsiChar;
                         out pxFunc: TxFunc;
                         out ppArg: Pointer): LongBool; cdecl;
    xRename: function(pVTab: Psqlite3_vtab; zNew: PAnsiChar): Integer; cdecl;
    // The methods above are in version 1 of the sqlite_module object.
    // Those ** below are for version 2 and greater.
    xSavepoint: function(pVTab: Psqlite3_vtab; savepoint: Integer): Integer; cdecl;
    xRelease: function(pVTab: Psqlite3_vtab; savepoint: Integer): Integer; cdecl;
    xRollbackTo: function(pVTab: Psqlite3_vtab; savepoint: Integer): Integer; cdecl;
  end;

const
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;

var
  sqlite3_create_module: function(
        db: TSQLiteDB;               // SQLite connection to register module with
        zName: PAnsiChar;         // Name of the module
        out p: sqlite3_module;   // Methods for the module
        pClientData: Pointer          // Client data for xCreate/xConnect
      ): Integer; cdecl;

  sqlite3_create_module_v2: function(
        db: TSQLiteDB;               // SQLite connection to register module with
        zName: PAnsiChar;         // Name of the module
        out p: sqlite3_module;   // Methods for the module
        pClientData: Pointer;         // Client data for xCreate/xConnect
        xDestroy: TSQLite3Destructor     // Module destructor function
      ): Integer; cdecl;
  sqlite3_declare_vtab: function (db: TSQLiteDB; zSQL: PAnsiChar): Integer; cdecl;

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): String;
function SQLiteErrorStr(SQLiteErrorCode: Integer): String;

function LoadSQLite(const AFilename: string): Boolean;
procedure UnloadSQLite;

procedure SQLiteCheck(SQLiteResult: Integer; ErrorMessage: PAnsiChar); overload;
procedure SQLiteCheck(SQLiteResult: Integer); overload;

implementation

uses
  {$IFDEF DELPHI16_UP}

  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils;
  {$ELSE}

  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}

  SysUtils;
  {$ENDIF}


function SQLiteFieldType(SQLiteFieldTypeCode: Integer): String;
begin
  case SQLiteFieldTypeCode of
    SQLITE_INTEGER: Result := 'Integer';
    SQLITE_FLOAT: Result := 'Float';
    SQLITE_TEXT: Result := 'Text';
    SQLITE_BLOB: Result := 'Blob';
    SQLITE_NULL: Result := 'Null';
  else
    Result := 'Unknown SQLite Field Type Code "' + IntToStr(SQLiteFieldTypeCode) + '"';
  end;
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

function LoadProc(const ProcName: string): Pointer;
begin
  Result := GetProcAddress(SQLite_Handle, PChar(ProcName));
  if Result = nil then
  begin
    raise Exception.Create('Cannot get procedure "' + ProcName + '" address.');
  end;
end;

function LoadProcSilent(const ProcName: string): Pointer;
begin
  Result := GetProcAddress(SQLite_Handle, PChar(ProcName));
end;

function ColValueToStr(Value: PChar): String;
begin
  if (Value = nil) then
    Result := 'NULL'
  else
    Result := Value;
end;

procedure SQLiteCheck(SQLiteResult: Integer; ErrorMessage: PAnsiChar);
var
  Tmp: string;
begin
  if (SQLiteResult <> SQLITE_OK) and (SQLiteResult <> SQLITE_DONE) then
  begin
    if ErrorMessage <> nil then
    begin
      Tmp := string(AnsiString(ErrorMessage));
      SQlite3_Free(ErrorMessage);
    end
    else
    begin
      Tmp := SQLiteErrorStr(SQLiteResult);
    end;

    raise Exception.Create(Tmp);
  end;
end;

procedure SQLiteCheck(SQLiteResult: Integer);
begin
  SQLiteCheck(SQLiteResult, nil);
end;

function LoadSQLite(const AFilename: string): Boolean;
begin
  Result := (SQLite_Handle <> 0);
  if not Result then
  begin
    SQLite_Handle := LoadLibrary(PChar(AFilename));
    if (SQLite_Handle <> 0) then
    begin
      sqlite3_open := LoadProc('sqlite3_open');
      sqlite3_open16 := LoadProc('sqlite3_open16');
      sqlite3_open_v2 := LoadProc('sqlite3_open_v2');
      SQLite3_Close := LoadProc('sqlite3_close');
      SQLite3_Exec := LoadProc('sqlite3_exec');
      SQLite3_Version := LoadProc('sqlite3_libversion');
      SQLite3_ErrMsg := LoadProc('sqlite3_errmsg');
      SQLite3_ErrMsg16 := LoadProc('sqlite3_errmsg16');
      SQLite3_ErrCode := LoadProc('sqlite3_errcode');
      SQlite3_Free := LoadProc('sqlite3_free');
      sqlite3_mprintf := LoadProc('sqlite3_mprintf');
      SQLite3_GetTable := LoadProc('sqlite3_get_table');
      SQLite3_FreeTable := LoadProc('sqlite3_free_table');
      SQLite3_Complete := LoadProc('sqlite3_complete');
      SQLite3_LastInsertRowID := LoadProc('sqlite3_last_insert_rowid');
      SQLite3_Interrupt := LoadProc('sqlite3_interrupt');
      SQLite3_BusyHandler := LoadProc('sqlite3_busy_handler');
      SQLite3_BusyTimeout := LoadProc('sqlite3_busy_timeout');
      SQLite3_Changes := LoadProc('sqlite3_changes');
      SQLite3_TotalChanges := LoadProc('sqlite3_total_changes');
      SQLite3_Prepare := LoadProc('sqlite3_prepare');
      SQLite3_Prepare_v2 := LoadProc('sqlite3_prepare_v2');
      SQLite3_Prepare16 := LoadProc('sqlite3_prepare16');
      SQLite3_Prepare16_v2 := LoadProc('sqlite3_prepare16_v2');

      sqlite3_bind_blob := LoadProc('sqlite3_bind_blob');
      sqlite3_bind_text := LoadProc('sqlite3_bind_text');
      sqlite3_bind_text16 := LoadProc('sqlite3_bind_text16');
      sqlite3_bind_double := LoadProc('sqlite3_bind_double');
      sqlite3_bind_int := LoadProc('sqlite3_bind_int');
      sqlite3_bind_int64 := LoadProc('sqlite3_bind_int64');
      sqlite3_bind_null := LoadProc('sqlite3_bind_null');
      sqlite3_clear_bindings := LoadProc('sqlite3_clear_bindings');
      sqlite3_bind_parameter_index := LoadProc('sqlite3_bind_parameter_index');
      sqlite3_bind_parameter_name := LoadProc('sqlite3_bind_parameter_name');
      sqlite3_bind_parameter_count := LoadProc('sqlite3_bind_parameter_count');


      SQLite3_ColumnCount := LoadProc('sqlite3_column_count');
      SQLite3_ColumnName := LoadProc('sqlite3_column_name');
      SQLite3_ColumnName16 := LoadProc('sqlite3_column_name16');
      SQLite3_ColumnDeclType := LoadProc('sqlite3_column_decltype');
      SQLite3_ColumnDeclType16 := LoadProc('sqlite3_column_decltype16');
      SQLite3_Step := LoadProc('sqlite3_step');
      SQLite3_DataCount := LoadProc('sqlite3_data_count');
      sqlite3_memory_used := LoadProc('sqlite3_memory_used');
      SQLite3_ColumnBlob := LoadProc('sqlite3_column_blob');
      SQLite3_ColumnBytes := LoadProc('sqlite3_column_bytes');
      SQLite3_ColumnBytes16 := LoadProc('sqlite3_column_bytes16');
      SQLite3_ColumnDouble := LoadProc('sqlite3_column_double');
      SQLite3_ColumnInt := LoadProc('sqlite3_column_int');
      SQLite3_ColumnText := LoadProc('sqlite3_column_text');
      SQLite3_ColumnText16 := LoadProc('sqlite3_column_text16');
      SQLite3_ColumnType := LoadProc('sqlite3_column_type');
      SQLite3_ColumnInt64 := LoadProc('sqlite3_column_int64');
      SQLite3_Finalize := LoadProc('sqlite3_finalize');
      SQLite3_Reset := LoadProc('sqlite3_reset');

      SQLite3_Backup_Init := LoadProc('sqlite3_backup_init');
      SQLite3_Backup_Step := LoadProc('sqlite3_backup_step');
      SQLite3_Backup_Finish := LoadProc('sqlite3_backup_finish');
      SQLite3_Backup_Remaining := LoadProc('sqlite3_backup_remaining');
      SQLite3_Backup_Pagecount := LoadProc('sqlite3_backup_pagecount');


      sqlite3_enable_shared_cache := LoadProc('sqlite3_enable_shared_cache');
      SQLite3_create_collation := LoadProc('sqlite3_create_collation');
      SQLite3_create_collation16 := LoadProc('sqlite3_create_collation16');

      sqlite3_create_function := LoadProc('sqlite3_create_function');
      sqlite3_result_blob := LoadProc('sqlite3_result_blob');
      sqlite3_result_double := LoadProc('sqlite3_result_double');
      sqlite3_result_error := LoadProc('sqlite3_result_error');
      sqlite3_result_error16 := LoadProc('sqlite3_result_error16');
      sqlite3_result_int := LoadProc('sqlite3_result_int');
      sqlite3_result_int64 := LoadProc('sqlite3_result_int64');
      sqlite3_result_null := LoadProc('sqlite3_result_null');
      sqlite3_result_text := LoadProc('sqlite3_result_text');
      sqlite3_result_text16 := LoadProc('sqlite3_result_text16');
      sqlite3_result_text16be := LoadProc('sqlite3_result_text16be');
      sqlite3_result_text16le := LoadProc('sqlite3_result_text16le');
      sqlite3_result_value := LoadProc('sqlite3_result_value');
      sqlite3_user_data := LoadProc('sqlite3_user_data');
      sqlite3_aggregate_context := LoadProc('sqlite3_aggregate_context');

      sqlite3_value_blob := LoadProc('sqlite3_value_blob');
      sqlite3_value_bytes := LoadProc('sqlite3_value_bytes');
      sqlite3_value_bytes16 := LoadProc('sqlite3_value_bytes16');
      sqlite3_value_double := LoadProc('sqlite3_value_double');
      sqlite3_value_int := LoadProc('sqlite3_value_int');
      sqlite3_value_int64 := LoadProc('sqlite3_value_int64');
      sqlite3_value_text := LoadProc('sqlite3_value_text');
      sqlite3_value_text16 := LoadProc('sqlite3_value_text16');
      sqlite3_value_text16be := LoadProc('sqlite3_value_text16be');
      sqlite3_value_text16le := LoadProc('sqlite3_value_text16le');
      sqlite3_value_type := LoadProc('sqlite3_value_type');
      sqlite3_set_authorizer := LoadProc('sqlite3_set_authorizer');
      sqlite3_update_hook := LoadProc('sqlite3_update_hook');
      sqlite3_table_column_metadata := LoadProcSilent('sqlite3_table_column_metadata');
      sqlite3_enable_load_extension := LoadProcSilent('sqlite3_enable_load_extension');
      sqlite3_load_extension := LoadProcSilent('sqlite3_load_extension');

      sqlite3_key := LoadProcSilent('sqlite3_key');
      sqlite3_rekey := LoadProcSilent('sqlite3_rekey');

      sqlite3_create_module := LoadProc('sqlite3_create_module');
      sqlite3_create_module_v2 := LoadProc('sqlite3_create_module_v2');
      sqlite3_declare_vtab := LoadProc('sqlite3_declare_vtab');

      Result := (SQLite_Handle <> 0);
    end;
  end;
end;

procedure UnloadSQLite;
begin
  if SQLite_Handle <> 0 then
  begin
    FreeLibrary(SQLite_Handle);
  end;
  SQLite_Handle := 0;
end;

initialization
  {$IFNDEF LOAD_DYNAMICALLY}
  LoadSQLite(SQLiteDLL);
  {$ENDIF}

finalization
  UnloadSQLite;

end.

