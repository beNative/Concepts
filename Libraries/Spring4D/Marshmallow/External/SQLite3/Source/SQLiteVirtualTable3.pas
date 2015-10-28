unit SQLiteVirtualTable3;

interface

{$I sv.inc}
uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils, System.Generics.Collections,
  {$ELSE}
  Classes, SysUtils, Generics.Collections,
  {$ENDIF}
  SQLite3, SQLiteTable3;

type
  TSQLiteModule = class;
  TSQLiteVirtualTable = class;
  TSQLiteVirtualTableCursor = class;
  TSQLiteVirtualTableList = class (TObjectList<TSQLiteVirtualTable>);
  TSQLiteVirtualTableFunctionOverload = class;
  TSQLiteVirtualTableFunctionOverloadList = class(TObjectList<TSQLiteVirtualTableFunctionOverload>);

  TSQLiteModule = class
  private
    FModule: sqlite3_module;
    FVirtualTables: TSQLiteVirtualTableList;
    FDanglingVirtualTables: TSQLiteVirtualTableList;
    FDatabase: TSQLiteDatabase;
    FName: string;
    FLastErrorMessage: string;

    procedure RegisterVirtualTable(Table: TSQLiteVirtualTable);
    procedure UnregisterVirtualTable(Table: TSQLiteVirtualTable);
  protected
    procedure SetLastErrorMessage(const LastErrorMessage: string);

    function GetNewTable(const Name: string; Arguments: TStrings; APVTab: Psqlite3_vtab): TSQLiteVirtualTable; virtual; abstract;
    function GetColumnsDeclaration(Arguments: TStrings; out Declaration: string): Boolean; virtual; abstract;
  public
    constructor Create(ADatabase: TSQLiteDatabase; AName: string);
    destructor Destroy; override;

    property Name: string read FName;
    property LastErrorMessage: string read FLastErrorMessage;
    property VirtualTables: TSQLiteVirtualTableList read FVirtualTables;
  end;

  TPsqlite3_valueList = TList<Psqlite3_value>;

  TSQLiteVirtualTableConstraintOperator = (vtcoUnknown = 0, vtcoEqual, vtcoGreater, vtcoLowerOrEqual, vtcoLower, vtcoGreaterOrEqual, vtcoMatch);

  TSQLiteVirtualTableConstraint = class
  private
    FColumn: Integer;
    FOp: TSQLiteVirtualTableConstraintOperator;
  public
    constructor Create(AColumn: Integer; AOp: TSQLiteVirtualTableConstraintOperator);

    property Column: Integer read FColumn;
    property Op: TSQLiteVirtualTableConstraintOperator read FOp;
  end;
  TSQLiteVirtualTableConstraintList = TObjectList<TSQLiteVirtualTableConstraint>;

  TSQLiteVirtualTableOrderByKind = (vtokAsc, vtokDesc);

  TSQLiteVirtualTableOrderBy = class
  private
    FColumn: Integer;
    FKind: TSQLiteVirtualTableOrderByKind;
  public
    constructor Create(AColumn: Integer; AKind: TSQLiteVirtualTableOrderByKind);

    property Column: Integer read FColumn;
    property Kind: TSQLiteVirtualTableOrderByKind read FKind;
  end;
  TSQLiteVirtualTableOrderByList = TObjectList<TSQLiteVirtualTableOrderBy>;

  TSQLiteVirtualTableConstraintUsage = class
  private
    FGiveValueToFilter: Boolean;
    FDoNotDoubleCheck: Boolean;
  public
    constructor Create(AGiveValueToFilter: Boolean; ADoNotDoubleCheck: Boolean);

    property GiveValueToFilter: Boolean read FGiveValueToFilter;
    property DoNotDoubleCheck: Boolean read FDoNotDoubleCheck;
  end;
  TSQLiteVirtualTableConstraintUsageList = TObjectList<TSQLiteVirtualTableConstraintUsage>;

  TSQLiteVirtualTable = class
  private
    FModule: TSQLiteModule;
    FName: string;
    FLastErrorMessage: string;
    FCursors: TObjectList<TSQLiteVirtualTableCursor>;
    FFunctionOverloads: TSQLiteVirtualTableFunctionOverloadList;
    FPVTab: Psqlite3_vtab;

    procedure RegisterCursor(Cursor: TSQLiteVirtualTableCursor);
    procedure UnregisterCursor(Cursor: TSQLiteVirtualTableCursor);
    function SetName(const NewName: string): Boolean;
  protected
    procedure SetLastErrorMessage(const LastErrorMessage: string);

    function DoDelete(RowId: Int64): Boolean; virtual; abstract;
    function DoInsert(RowId: Int64; Values: TPsqlite3_valueList): Boolean; virtual; abstract;
    function DoUpdate(ExistingRowId: Int64; NewRowId: Int64; Values: TPsqlite3_valueList): Boolean; virtual; abstract;
    function DoGetBestIndex(Constraints: TSQLiteVirtualTableConstraintList; OrderBys: TSQLiteVirtualTableOrderByList;
                            out IndexId: Integer; out IndexParameters: Pointer;
                            out OutputInOrder: Boolean; out EstimatedCost: Double;
                            ConstraintsUsage: TSQLiteVirtualTableConstraintUsageList): Boolean; virtual; abstract;
    function DoRename(const NewName: string): Boolean; virtual; abstract;
    function DoConnect: Boolean; virtual; abstract;
    function DoDisconnect: Boolean; virtual; abstract;
    function DoGetFunctionOverload(const FunctionName: string; FunctionArgumentCount: Integer): TSQLiteVirtualTableFunctionOverload; virtual; abstract;

    function GetNewCursor: TSQLiteVirtualTableCursor; virtual; abstract;
  public
    constructor Create(AModule: TSQLiteModule; const AName: string; APVTab: Psqlite3_vtab);
    destructor Destroy; override;

    property Name: string read FName;
    property Module: TSQLiteModule read FModule;
    property LastErrorMessage: string read FLastErrorMessage;
  end;

  TOnSQLiteVirtualTableFunctionOverload =  procedure (pContext: Psqlite3_context; Values: TPsqlite3_valueList) of object;

  TSQLiteVirtualTableFunctionOverload = class
  private
    FOnOverload: TOnSQLiteVirtualTableFunctionOverload;
  public
    constructor Create(AOnOverload: TOnSQLiteVirtualTableFunctionOverload);

    procedure DoOverload(pContext: Psqlite3_context; Values: TPsqlite3_valueList); dynamic;
  end;

  TSQLiteVirtualTableCursor = class
  private
    FTable: TSQLiteVirtualTable;
  protected
    procedure SetLastErrorMessage(const LastErrorMessage: string);

    function DoInitialize(IndexId: Integer; IndexParameters: Pointer; Values: TPsqlite3_valueList): Boolean; virtual; abstract;
    function DoNext: Boolean; virtual; abstract;
    function GetRowId(out RowId: Int64): Boolean; virtual; abstract;
    function GetEof: Boolean; virtual; abstract;
    function GetColumnValue(SQLiteContext: Psqlite3_context; ColumnNumber: Integer): Boolean; virtual; abstract;
  public
    constructor Create(ATable: TSQLiteVirtualTable);
    destructor Destroy; override;

    property Table: TSQLiteVirtualTable read FTable;
    property Eof: Boolean read GetEof;
  end;

  ESQLiteModule = class(Exception)
  end;

implementation

type
  PSQLiteModuleVTab = ^TSQLiteModuleVTab;
  TSQLiteModuleVTab = record
    Base: sqlite3_vtab;
    Instance: TSQLiteVirtualTable;
  end;

type
  PSQLiteModuleVTabCursor = ^TSQLiteModuleVTabCursor;
  TSQLiteModuleVTabCursor = record
    Base: sqlite3_vtab_cursor;
    Instance: TSQLiteVirtualTableCursor;
  end;

{ TSQLiteModule }

procedure SetSQLiteVTabErrorMessage(pVTab: Psqlite3_vtab; const ErrorMessage: string); overload;
begin
  sqlite3_free(pVTab.zErrMsg);
  pVTab.zErrMsg := sqlite3_mprintf('%s', UTF8encode(ErrorMessage));
end;

procedure SetSQLiteVTabErrorMessage(pVTab: Psqlite3_vtab; const ErrorMessage: string; Args: array of const); overload;
begin
  SetSQLiteVTabErrorMessage(pVTab, Format(ErrorMessage, Args));
end;

function GetSQLiteResult(Success: Boolean; pVTab: Psqlite3_vtab): Integer; overload;
var
  Table: TSQLiteVirtualTable;
begin
  if Success then
  begin
    Result := SQLITE_OK
  end
  else
  begin
    Table := PSQLiteModuleVTab(pVTab).Instance;
    if Table.LastErrorMessage <> '' then
      SetSQLiteVTabErrorMessage(pvTab, Table.LastErrorMessage);

    Result := SQLITE_ERROR;
  end;
end;

function GetSQLiteResult(Success: Boolean; pCursor: Psqlite3_vtab_cursor): Integer; inline; overload;
begin
  Result := GetSQLiteResult(Success, pCursor.pVtab);
end;

function GetArrayElement(Elements: PPAnsiChar; Index: Integer): PAnsiChar;
begin
  Result := PPAnsiChar(PByte(Elements) + Index * SizeOf(Elements^))^;
end;

function SQLiteDeclareVTab(db: TSQLiteDB; argc: Integer; argv: PPAnsiChar; Module: TSQLiteModule; Arguments: TStringList; out pzErr: PAnsiChar): Integer;
var
  CreateStatement: string;
  I: Integer;
  ColumnDeclaration: string;
begin
  for I := 3 to argc - 1 do
    Arguments.Add(UTF8ToString(GetArrayElement(argv, I)));

  if not Module.GetColumnsDeclaration(Arguments, ColumnDeclaration) then
  begin
    pzErr := sqlite3_mprintf('%s', UTF8Encode(Module.LastErrorMessage));
    Exit(SQLITE_ERROR);
  end;

  CreateStatement := 'CREATE TABLE x(' + ColumnDeclaration + ')';
  Result := sqlite3_declare_vtab(db, PAnsiChar(UTF8Encode(CreateStatement)));
end;

function xCreate(db: TSQLiteDB; pAux: Pointer;
             argc: Integer; argv: PPAnsiChar;
             out ppVTab: Psqlite3_vtab; out pzErr: PAnsiChar): Integer; cdecl;
var
  VTab: PSQLiteModuleVTab;
  Module: TSQLiteModule;
  Arguments: TStringList;
begin
  Module := TSQLiteModule(pAux);
  VTab := nil;

  Arguments := TStringList.Create;
  try
    Result := SQLiteDeclareVTab(db, argc, argv, Module, Arguments, pzErr);

    if Result = SQLITE_OK then
    begin
      GetMem(VTab, SizeOf(TSQLiteModuleVTab));
      FillChar(VTab^, SizeOf(TSQLiteModuleVTab), 0);
      VTab.Instance := Module.GetNewTable(UTF8ToString(GetArrayElement(argv, 2)), Arguments, PSQlite3_vtab(VTab));
    end;
  finally
    Arguments.Free;
  end;

  ppVTab := Psqlite3_vtab(VTab);
end;

function xConnect(db: TSQLiteDB; pAux: Pointer;
             argc: Integer; argv: PPAnsiChar;
             var pVTab: Psqlite3_vtab; out pzErr: PAnsiChar): Integer; cdecl;
var
  Module: TSQLiteModule;
  Found: Boolean;
  I: Integer;
  Table: TSQLiteVirtualTable;
  Arguments: TStringList;
begin
  if Assigned(pVTab) then
  begin
    pzErr := sqlite3_mprintf('xConnect is not implemented for when pVTab is not nil');
    Result := SQLITE_ERROR;
  end
  else
  begin
    Module := TSQLiteModule(pAux);

    Found := False;
    I := 0;
    while not Found and (I < Module.FDanglingVirtualTables.Count) do
      Found := Module.FDanglingVirtualTables[I].Name = UTF8ToString(GetArrayElement(argv, 2));

    if Found then
    begin
      Arguments := TStringList.Create;
      try
        Result := SQLiteDeclareVTab(db, argc, argv, Module, Arguments, pzErr);
      finally
        Arguments.Free;
      end;

      if Result = SQLITE_OK then
      begin
        Table := Module.FDanglingVirtualTables[I];

        Result := GetSQLiteResult(Table.DoConnect, Table.FPVTab);
        if Result = SQLITE_OK then
        begin
          Module.FVirtualTables.Add(Module.FDanglingVirtualTables.Extract(Table));
          pVTab := Table.FPVTab;
        end;
      end;
    end
    else
    begin
      pzErr := sqlite3_mprintf('xConnect does not know how to handle the case when no dangling table is available (name = %s)', GetArrayElement(argv, 2));
      Result := SQLITE_ERROR;
    end;
  end;
end;

function xBestIndex(pVTab: Psqlite3_vtab; pIndexInfo: Psqlite3_index_info): Integer; cdecl;
var
  Table: TSQLiteVirtualTable;
  OperationSuccessful: Boolean;
  Constraints: TSQLiteVirtualTableConstraintList;
  IndexId: Integer;
  IndexParameters: Pointer;
  I: Integer;
  curConstraint: Psqlite3_index_constraint;
  Op: TSQLiteVirtualTableConstraintOperator;
  OrderBys: TSQLiteVirtualTableOrderByList;
  curOrderBy: Psqlite3_index_orderby;
  OrderByKind: TSQLiteVirtualTableOrderByKind;
  OutputInOrder: Boolean;
  EstimatedCost: Double;
  ConstraintsUsage: TSQLiteVirtualTableConstraintUsageList;
  curConstraintUsage: Psqlite3_index_constraint_usage;
begin
  Table := PSQLiteModuleVTab(pVTab).Instance;

  Constraints := TSQLiteVirtualTableConstraintList.Create(True);
  OrderBys := TSQLiteVirtualTableOrderByList.Create(True);
  ConstraintsUsage := TSQLiteVirtualTableConstraintUsageList.Create(True);
  try
    for I := 0 to pIndexInfo.nConstraint - 1 do
    begin
      curConstraint := Psqlite3_index_constraint(PByte(pIndexInfo.aConstraint) + I * SizeOf(pIndexInfo.aConstraint^));
      if curConstraint.usable then
      begin
        case curConstraint.op of
          SQLITE_INDEX_CONSTRAINT_EQ:
            Op := vtcoEqual;
          SQLITE_INDEX_CONSTRAINT_GT:
            Op := vtcoGreater;
          SQLITE_INDEX_CONSTRAINT_LE:
            Op := vtcoLowerOrEqual;
          SQLITE_INDEX_CONSTRAINT_LT:
            Op := vtcoLower;
          SQLITE_INDEX_CONSTRAINT_GE:
            Op := vtcoGreaterOrEqual;
          SQLITE_INDEX_CONSTRAINT_MATCH:
            Op := vtcoMatch;
          else
          begin
            SetSQLiteVTabErrorMessage(pVTab, 'Unknown operator: %d', [curConstraint.op]);
            Exit(SQLITE_ERROR);
            Op := vtcoUnknown;  // avoid warning on the line just below
          end;
        end;
        Constraints.Add(TSQLiteVirtualTableConstraint.Create(curConstraint.iColumn, Op));
        ConstraintsUsage.Add(TSQLiteVirtualTableConstraintUsage.Create(True, False));
      end;
    end;

    for I := 0 to pIndexInfo.nOrderBy - 1 do
    begin
      curOrderBy := Psqlite3_index_orderby(PByte(pIndexInfo.aOrderBy) + I * SizeOf(pIndexInfo.aOrderBy^));
      if curOrderBy.desc then
        OrderByKind := vtokDesc
      else
        OrderByKind := vtokAsc;

      OrderBys.Add(TSQLiteVirtualTableOrderBy.Create(curOrderBy.iColumn, OrderByKind));
    end;

    OperationSuccessful := Table.DoGetBestIndex(Constraints, OrderBys,
                                                IndexId, IndexParameters,
                                                OutputInOrder, EstimatedCost,
                                                ConstraintsUsage);

    pIndexInfo.idxNum := IndexId;
    pIndexInfo.idxStr := IndexParameters;
    pIndexInfo.needToFreeIdxStr := True;
    pIndexInfo.orderByConsumed := OutputInOrder;
    pIndexInfo.estimatedCost := EstimatedCost;

    for I := 0 to pIndexInfo.nConstraint - 1 do
    begin
      curConstraintUsage := Psqlite3_index_constraint_usage(PByte(pIndexInfo.aConstraintUsage) + I * SizeOf(pIndexInfo.aConstraintUsage^));

      curConstraintUsage.argvIndex := 0;
      curConstraintUsage.omit := False;

      if Psqlite3_index_constraint(PByte(pIndexInfo.aConstraint) + I * SizeOf(pIndexInfo.aConstraint^)).usable then
      begin
        if ConstraintsUsage[I].GiveValueToFilter then
          curConstraintUsage.argvIndex := I + 1;
        curConstraintUsage.omit := ConstraintsUsage[I].DoNotDoubleCheck;
      end;
    end;
  finally
    OrderBys.Free;
    Constraints.Free;
    ConstraintsUsage.Free;
  end;

  Result := GetSQLiteResult(OperationSuccessful, pVTab);
end;

function xDisconnect(pVTab: Psqlite3_vtab): Integer; cdecl;
var
  Table: TSQLiteVirtualTable;
  Module: TSQLiteModule;
begin
  Table := PSQLiteModuleVTab(pVTab).Instance;
  Module := Table.Module;

  Result := GetSQLiteResult(Table.DoDisconnect, pVTab);

  if Result = SQLITE_OK then
    Module.FDanglingVirtualTables.Add(Module.FVirtualTables.Extract(Table));
end;

function xDestroy(pVTab: Psqlite3_vtab): Integer; cdecl;
begin
  PSQLiteModuleVTab(pVTab).Instance.Free;
  FreeMem(pVTab);
  Result := SQLITE_OK;
end;

function xOpen(pVTab: Psqlite3_vtab; var ppCursor: Psqlite3_vtab_cursor): Integer; cdecl;
var
  Table: TSQLiteVirtualTable;
  VTabCursor: PSQLiteModuleVTabCursor;
begin
  Table := PSQLiteModuleVTab(pVTab).Instance;

  GetMem(VTabCursor, SizeOf(TSQLiteModuleVTabCursor));
  FillChar(VTabCursor^, SizeOf(TSQLiteModuleVTabCursor), 0);
  VTabCursor.Instance := Table.GetNewCursor;

  ppCursor := Psqlite3_vtab_cursor(VTabCursor);

  Result := SQLITE_OK;
end;

function xClose(pCursor: Psqlite3_vtab_cursor): Integer; cdecl;
begin
  PSQLiteModuleVTabCursor(pCursor).Instance.Free;
  FreeMem(pCursor);
  Result := SQLITE_OK;
end;

function xFilter(pCursor: Psqlite3_vtab_cursor; idxNum: Integer; idxStr: PAnsiChar;
              argc: Integer; argv: PPsqlite3_value): Integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
  Values: TPsqlite3_valueList;
  I: Integer;
  OperationSuccessful: Boolean;
begin
  Cursor := PSQLiteModuleVTabCursor(pCursor).Instance;

  Values := TPsqlite3_valueList.Create;
  try
    for I := 0 to argc - 1 do
      Values.Add(TSQLiteFunctions.GetArrayElement(argv, I));

    OperationSuccessful := Cursor.DoInitialize(idxNum, idxStr, Values);
  finally
    Values.Free;
  end;

  Result := GetSQLiteResult(OperationSuccessful, pCursor);
end;

function xNext(pCursor: Psqlite3_vtab_cursor): Integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
  OperationSuccessful: Boolean;
begin
  Cursor := PSQLiteModuleVTabCursor(pCursor).Instance;

  OperationSuccessful := Cursor.DoNext;

  Result := GetSQLiteResult(OperationSuccessful, pCursor);
end;

function xEof(pCursor: Psqlite3_vtab_cursor): LongBool; cdecl;
begin
  Result := PSQLiteModuleVTabCursor(pCursor).Instance.Eof;
end;

function xColumn(pCursor: Psqlite3_vtab_cursor; pContext: Psqlite3_context; N: Integer): Integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
  OperationSuccessful: Boolean;
begin
  Cursor := PSQLiteModuleVTabCursor(pCursor).Instance;

  OperationSuccessful := Cursor.GetColumnValue(pContext, N);

  Result := GetSQLiteResult(OperationSuccessful, pCursor);
end;

function xRowid(pCursor: Psqlite3_vtab_cursor; out pRowid: sqlite3_int64): Integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
  OperationSuccessful: Boolean;
begin
  Cursor := PSQLiteModuleVTabCursor(pCursor).Instance;

  OperationSuccessful := Cursor.GetRowId(pRowId);

  Result := GetSQLiteResult(OperationSuccessful, pCursor);
end;

function xUpdate(pVTab: Psqlite3_vtab; argc: Integer; argv: PPsqlite3_value; var pRowId: sqlite3_int64): Integer; cdecl;
var
  Table: TSQLiteVirtualTable;
  OperationSuccessful: Boolean;
  Values: TPsqlite3_valueList;
  I: Integer;
  ExistingRowId: Int64;
  NewRowId: Int64;
begin
  Table := PSQLiteModuleVTab(pVTab).Instance;

  ExistingRowId := sqlite3_value_int64(argv^);

  if argc = 1 then
  begin
    OperationSuccessful := Table.DoDelete(ExistingRowId);
  end
  else
  begin
    NewRowId := sqlite3_value_int64(TSQLiteFunctions.GetArrayElement(argv, 1));
    Values := TPsqlite3_valueList.Create;
    try
      for I := 2 to argc - 1 do
        Values.Add(TSQLiteFunctions.GetArrayElement(argv, I));

      if sqlite3_value_type(argv^) = SQLITE_NULL then
        OperationSuccessful := Table.DoInsert(NewRowId, Values)
      else
        OperationSuccessful := Table.DoUpdate(ExistingRowId, NewRowId, Values);
    finally
      Values.Free;
    end;
  end;

  Result := GetSQLiteResult(OperationSuccessful, pVTab);
end;

function xBegin(pVTab: Psqlite3_vtab): Integer; cdecl;
begin
  SetSQLiteVTabErrorMessage(pVTab, 'xBegin is not implemented');
  Result := SQLITE_ERROR;
end;

function xSync(pVTab: Psqlite3_vtab): Integer; cdecl;
begin
  SetSQLiteVTabErrorMessage(pVTab, 'xSync is not implemented');
  Result := SQLITE_ERROR;
end;

function xCommit(pVTab: Psqlite3_vtab): Integer; cdecl;
begin
  SetSQLiteVTabErrorMessage(pVTab, 'xCommit is not implemented');
  Result := SQLITE_ERROR;
end;

function xRollback(pVTab: Psqlite3_vtab): Integer; cdecl;
begin
  SetSQLiteVTabErrorMessage(pVTab, 'xRollback is not implemented');
  Result := SQLITE_ERROR;
end;

procedure xFindFunctionXFunc(pContext: Psqlite3_context; argc: Integer; argv: PPsqlite3_value); cdecl;
var
  FunctionOverload: TSQLiteVirtualTableFunctionOverload;
  Values: TPsqlite3_valueList;
  I: Integer;
begin
  FunctionOverload := TSQLiteVirtualTableFunctionOverload(sqlite3_user_data(pContext));
  Values := TPsqlite3_valueList.Create;
  try
    for I := 0 to argc - 1 do
      Values.Add(TSQLiteFunctions.GetArrayElement(argv, I));

    FunctionOverload.DoOverload(pContext, Values);
  finally
    Values.Free;
  end;
end;

function xFindFunction(pVTab: Psqlite3_vtab; nArg: Integer; zName: PAnsiChar;
                     out pxFunc: TxFunc;
                     out ppArg: Pointer): LongBool; cdecl;
var
  Table: TSQLiteVirtualTable;
  FunctionOverload: TSQLiteVirtualTableFunctionOverload;
begin
  Table := PSQLiteModuleVTab(pVTab).Instance;
  FunctionOverload := Table.DoGetFunctionOverload(UTF8ToString(zName), nArg);
  if Assigned(FunctionOverload) then
  begin
    Table.FFunctionOverloads.Add(FunctionOverload);
    pxFunc := @xFindFunctionXFunc;
    ppArg := FunctionOverload;
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function xRename(pVTab: Psqlite3_vtab; zNew: PAnsiChar): Integer; cdecl;
begin
  Result := GetSQLiteResult(PSQLiteModuleVTab(pVTab).Instance.SetName(UTF8ToString(zNew)), pVTab);
end;

function xSavepoint(pVTab: Psqlite3_vtab; savepoint: Integer): Integer; cdecl;
begin
  SetSQLiteVTabErrorMessage(pVTab, 'xSavepoint is not implemented');
  Result := SQLITE_ERROR;
end;

function xRelease(pVTab: Psqlite3_vtab; savepoint: Integer): Integer; cdecl;
begin
  SetSQLiteVTabErrorMessage(pVTab, 'xRelease is not implemented');
  Result := SQLITE_ERROR;
end;

function xRollbackTo(pVTab: Psqlite3_vtab; savepoint: Integer): Integer; cdecl;
begin
  SetSQLiteVTabErrorMessage(pVTab, 'xRollbackTo is not implemented');
  Result := SQLITE_ERROR;
end;

constructor TSQLiteModule.Create(ADatabase: TSQLiteDatabase; AName: string);
begin
  if not ADatabase.Connected then
    raise ESQLiteModule.Create('The database must be connected before creating the module');

  inherited Create;

  FName := AName;
  FDatabase := ADatabase;
  FVirtualTables := TSQLiteVirtualTableList.Create(True);
  FDanglingVirtualTables := TSQLiteVirtualTableList.Create(True);

  FillChar(FModule, SizeOf(FModule), 0);
  FModule.iVersion := 2;
  FModule.xCreate := xCreate;
  FModule.xConnect := xConnect;
  FModule.xBestIndex := xBestIndex;
  FModule.xDisconnect := xDisconnect;
  FModule.xDestroy := xDestroy;
  FModule.xOpen := xOpen;
  FModule.xClose := xClose;
  FModule.xFilter := xFilter;
  FModule.xNext := xNext;
  FModule.xEof := xEof;
  FModule.xColumn := xColumn;
  FModule.xRowid := xRowid;
  FModule.xUpdate := xUpdate;
//  FModule.xBegin := xBegin;
//  FModule.xSync := xSync;
//  FModule.xCommit := xCommit;
//  FModule.xRollback := xRollback;
  FModule.xFindFunction := xFindFunction;
  FModule.xRename := xRename;
//  FModule.xSavepoint := xSavepoint;
//  FModule.xRelease := xRelease;
//  FModule.xRollbackTo := xRollbackTo;

  SQLiteCheck(sqlite3_create_module(FDatabase.DB, PAnsiChar(UTF8Encode(FName)), FModule, Self));
end;

destructor TSQLiteModule.Destroy;
var
  Table: TSQLiteVirtualTable;
  ErrMsg: PAnsiChar;
begin
  try
    for Table in FVirtualTables do
      SQLiteCheck(SQLite3_Exec(FDatabase.DB, PAnsiChar(UTF8Encode('DROP TABLE ' + Table.Name + ';')), nil, nil, ErrMsg), ErrMsg);
  finally
    FVirtualTables.Free;

    try
      for Table in FDanglingVirtualTables do
        FreeMem(Table.FPVTab);
    finally
      FDanglingVirtualTables.Free;

      inherited Destroy;
    end;
  end;
end;

procedure TSQLiteModule.RegisterVirtualTable(Table: TSQLiteVirtualTable);
begin
  FVirtualTables.Add(Table);
end;

procedure TSQLiteModule.SetLastErrorMessage(const LastErrorMessage: string);
begin
  FLastErrorMessage := LastErrorMessage;
end;

procedure TSQLiteModule.UnregisterVirtualTable(Table: TSQLiteVirtualTable);
begin
  FVirtualTables.Extract(Table);
end;

{ TSQLiteVirtualTable }

constructor TSQLiteVirtualTable.Create(AModule: TSQLiteModule; const AName: string; APVTab: Psqlite3_vtab);
begin
  inherited Create;

  FCursors := TObjectList<TSQLiteVirtualTableCursor>.Create(True);
  FFunctionOverloads := TSQLiteVirtualTableFunctionOverloadList.Create(True);
  FModule := AModule;
  FName := AName;
  FPVTab := APVTab;

  FModule.RegisterVirtualTable(Self);
end;

destructor TSQLiteVirtualTable.Destroy;
begin
  FModule.UnregisterVirtualTable(Self);
  FCursors.Free;
  FFunctionOverloads.Free;

  inherited Destroy;
end;

procedure TSQLiteVirtualTable.RegisterCursor(Cursor: TSQLiteVirtualTableCursor);
begin
  FCursors.Add(Cursor);
end;

procedure TSQLiteVirtualTable.SetLastErrorMessage(
  const LastErrorMessage: string);
begin
  FLastErrorMessage := LastErrorMessage;
end;

function TSQLiteVirtualTable.SetName(const NewName: string): Boolean;
begin
  FName := NewName;
  Result := DoRename(NewName);
end;

procedure TSQLiteVirtualTable.UnregisterCursor(
  Cursor: TSQLiteVirtualTableCursor);
begin
  FCursors.Extract(Cursor);
end;

{ TSQLiteVirtualTableConstraint }

constructor TSQLiteVirtualTableConstraint.Create(AColumn: Integer;
  AOp: TSQLiteVirtualTableConstraintOperator);
begin
  inherited Create;

  FColumn := AColumn;
  FOp := AOp;
end;

{ TSQLiteVirtualTableOrderBy }

constructor TSQLiteVirtualTableOrderBy.Create(AColumn: Integer;
  AKind: TSQLiteVirtualTableOrderByKind);
begin
  inherited Create;

  FColumn := AColumn;
  FKind := AKind;
end;

{ TSQLiteVirtualTableConstraintUsage }

constructor TSQLiteVirtualTableConstraintUsage.Create(AGiveValueToFilter,
  ADoNotDoubleCheck: Boolean);
begin
  inherited Create;

  FGiveValueToFilter := AGiveValueToFilter;
  FDoNotDoubleCheck := ADoNotDoubleCheck;
end;

{ TSQLiteVirtualTableCursor }

constructor TSQLiteVirtualTableCursor.Create(ATable: TSQLiteVirtualTable);
begin
  inherited Create;

  FTable := ATable;

  FTable.RegisterCursor(Self);
end;

destructor TSQLiteVirtualTableCursor.Destroy;
begin
  FTable.UnregisterCursor(Self);

  inherited Destroy;
end;

procedure TSQLiteVirtualTableCursor.SetLastErrorMessage(
  const LastErrorMessage: string);
begin
  Table.SetLastErrorMessage(LastErrorMessage);
end;

{ TSQLiteVirtualTableFunctionOverload }

constructor TSQLiteVirtualTableFunctionOverload.Create(
  AOnOverload: TOnSQLiteVirtualTableFunctionOverload);
begin
  inherited Create;

  FOnOverload := AOnOverload;
end;

procedure TSQLiteVirtualTableFunctionOverload.DoOverload(
  pContext: Psqlite3_context; Values: TPsqlite3_valueList);
begin
  FOnOverload(pContext, Values);
end;

end.
