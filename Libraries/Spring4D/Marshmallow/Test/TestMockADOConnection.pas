unit TestMockADOConnection;

interface

uses
  SysUtils,
  ActiveX,
  ADOInt,
  ADODB,
  Rtti,
  TestFramework,
  Spring.Interception.CustomProxy;

type
  // Cannot create Spring.Mock since safecall is not supported. Add a helper class.

  TMockADOConnection = class(TCustomProxy, _Connection,
    IConnectionPointContainer, IConnectionPoint)
  public
    //IDispatch
    function GetTypeInfoCount(out Count: Integer): HResult; virtual; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; virtual; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; virtual; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; virtual; stdcall;
    // _ADO
    function Get_Properties: Properties; virtual; safecall;
    // Connection15
    function Get_ConnectionString: WideString; virtual; safecall;
    procedure Set_ConnectionString(const pbstr: WideString); virtual; safecall;
    function Get_CommandTimeout: Integer; virtual; safecall;
    procedure Set_CommandTimeout(plTimeout: Integer); virtual; safecall;
    function Get_ConnectionTimeout: Integer; virtual; safecall;
    procedure Set_ConnectionTimeout(plTimeout: Integer); virtual; safecall;
    function Get_Version: WideString; virtual; safecall;
    procedure Close; virtual; safecall;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant;
      Options: Integer): _Recordset; virtual; safecall;
    function BeginTrans: Integer; virtual; safecall;
    procedure CommitTrans; virtual; safecall;
    procedure RollbackTrans; virtual; safecall;
    procedure Open(const ConnectionString: WideString; const UserID: WideString;
      const Password: WideString; Options: Integer); virtual; safecall;
    function Get_Errors: Errors; virtual; safecall;
    function Get_DefaultDatabase: WideString; virtual; safecall;
    procedure Set_DefaultDatabase(const pbstr: WideString); virtual; safecall;
    function Get_IsolationLevel: IsolationLevelEnum; virtual; safecall;
    procedure Set_IsolationLevel(Level: IsolationLevelEnum); virtual; safecall;
    function Get_Attributes: Integer; virtual; safecall;
    procedure Set_Attributes(plAttr: Integer); virtual; safecall;
    function Get_CursorLocation: CursorLocationEnum; virtual; safecall;
    procedure Set_CursorLocation(plCursorLoc: CursorLocationEnum); virtual; safecall;
    function Get_Mode: ConnectModeEnum; virtual; safecall;
    procedure Set_Mode(plMode: ConnectModeEnum); virtual; safecall;
    function Get_Provider: WideString; virtual; safecall;
    procedure Set_Provider(const pbstr: WideString); virtual; safecall;
    function Get_State: Integer; virtual; safecall;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): _Recordset; virtual; safecall;
    // _Connection
    procedure Cancel; virtual; safecall;

    // IConnectionPointContainer
    function EnumConnectionPoints(out Enum: IEnumConnectionPoints): HResult; virtual; stdcall;
    function FindConnectionPoint(const iid: TIID; out cp: IConnectionPoint): HResult; virtual; stdcall;

    // IConnectionPoint
    function GetConnectionInterface(out iid: TIID): HResult; virtual; stdcall;
    function GetConnectionPointContainer(out cpc: IConnectionPointContainer): HResult; virtual; stdcall;
    function Advise(const unkSink: IUnknown; out dwCookie: Longint): HResult; virtual; stdcall;
    function Unadvise(dwCookie: Longint): HResult; virtual; stdcall;
    function EnumConnections(out Enum: IEnumConnections): HResult; virtual; stdcall;
  end;

implementation

{ TMockADOConnection }

function TMockADOConnection.Advise(const unkSink: IInterface;
  out dwCookie: Integer): HResult;
begin
  {Result := Intercept([TValue.From<IInterface>(unkSink), @dwCookie]).AsInteger;}
  Result := 0;
end;

function TMockADOConnection.BeginTrans: Integer;
begin
  Result := Intercept.AsInteger;
end;

procedure TMockADOConnection.Cancel;
begin
  Intercept;
end;

procedure TMockADOConnection.Close;
begin
  Intercept;
end;

procedure TMockADOConnection.CommitTrans;
begin
  Intercept;
end;

function TMockADOConnection.EnumConnectionPoints(
  out Enum: IEnumConnectionPoints): HResult;
begin
  Result := Intercept([@Enum]).AsInteger;
end;

function TMockADOConnection.EnumConnections(
  out Enum: IEnumConnections): HResult;
begin
  Result := Intercept([@Enum]).AsInteger;
end;

function TMockADOConnection.Execute(const CommandText: WideString;
  out RecordsAffected: OleVariant; Options: Integer): _Recordset;
begin
  Result := Intercept([CommandText, @RecordsAffected, Options])
    .AsType<_Recordset>;
end;

function TMockADOConnection.FindConnectionPoint(const iid: TIID;
  out cp: IConnectionPoint): HResult;
begin
  {Result := Intercept([TValue.From<TIID>(iid), @cp]).AsInteger;}
  Result := 0;
  cp := Self;
end;

function TMockADOConnection.GetConnectionInterface(out iid: TIID): HResult;
begin
  Result := Intercept([@iid]).AsInteger;
end;

function TMockADOConnection.GetConnectionPointContainer(
  out cpc: IConnectionPointContainer): HResult;
begin
  Result := Intercept([@cpc]).AsInteger;
end;

function TMockADOConnection.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := Intercept([TValue.From<TIID>(IID), Names, NameCount, LocaleID,
    DispIDs]).AsInteger;
end;

function TMockADOConnection.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := Intercept([Index, LocaleID, @TypeInfo]).AsInteger;
end;

function TMockADOConnection.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := Intercept([@Count]).AsInteger;
end;

function TMockADOConnection.Get_Attributes: Integer;
begin
  Result := Intercept.AsInteger;
end;

function TMockADOConnection.Get_CommandTimeout: Integer;
begin
  Result := Intercept.AsInteger;
end;

function TMockADOConnection.Get_ConnectionString: WideString;
begin
  Result := Intercept.AsString;
end;

function TMockADOConnection.Get_ConnectionTimeout: Integer;
begin
  Result := Intercept.AsInteger;
end;

function TMockADOConnection.Get_CursorLocation: CursorLocationEnum;
begin
  Result := Intercept.AsType<CursorLocationEnum>;
end;

function TMockADOConnection.Get_DefaultDatabase: WideString;
begin
  Result := Intercept.AsString;
end;

function TMockADOConnection.Get_Errors: Errors;
begin
  Result := Intercept.AsType<Errors>;
end;

function TMockADOConnection.Get_IsolationLevel: IsolationLevelEnum;
begin
  Result := Intercept.AsType<IsolationLevelEnum>;
end;

function TMockADOConnection.Get_Mode: ConnectModeEnum;
begin
  Result := Intercept.AsType<ConnectModeEnum>;
end;

function TMockADOConnection.Get_Properties: Properties;
begin
  Result := Intercept.AsType<Properties>;
end;

function TMockADOConnection.Get_Provider: WideString;
begin
  Result := Intercept.AsString;
end;

function TMockADOConnection.Get_State: Integer;
begin
  Result := Intercept.AsInteger;
end;

function TMockADOConnection.Get_Version: WideString;
begin
  Result := Intercept.AsString;
end;

function TMockADOConnection.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := Intercept([DispID, TValue.From<TGUID>(IID), LocaleID, Flags,
    @Params, VarResult, ExcepInfo, ArgErr]).AsInteger;
end;

procedure TMockADOConnection.Open(const ConnectionString, UserID,
  Password: WideString; Options: Integer);
begin
  Intercept([ConnectionString, UserID, Password, Options]);
end;

function TMockADOConnection.OpenSchema(Schema: SchemaEnum; Restrictions,
  SchemaID: OleVariant): _Recordset;
begin
  Result := Intercept([Schema, TValue.From<OleVariant>(Restrictions),
    TValue.From<OleVariant>(SchemaID)]).AsType<_Recordset>;
end;

procedure TMockADOConnection.RollbackTrans;
begin
  Intercept;
end;

procedure TMockADOConnection.Set_Attributes(plAttr: Integer);
begin
  Intercept([plAttr]);
end;

procedure TMockADOConnection.Set_CommandTimeout(plTimeout: Integer);
begin
  Intercept([plTimeout]);
end;

procedure TMockADOConnection.Set_ConnectionString(const pbstr: WideString);
begin
  Intercept([pbstr]);
end;

procedure TMockADOConnection.Set_ConnectionTimeout(plTimeout: Integer);
begin
  Intercept([plTimeout]);
end;

procedure TMockADOConnection.Set_CursorLocation(
  plCursorLoc: CursorLocationEnum);
begin
  Intercept([plCursorLoc]);
end;

procedure TMockADOConnection.Set_DefaultDatabase(const pbstr: WideString);
begin
  Intercept([pbstr]);
end;

procedure TMockADOConnection.Set_IsolationLevel(Level: IsolationLevelEnum);
begin
  Intercept([Level]);
end;

procedure TMockADOConnection.Set_Mode(plMode: ConnectModeEnum);
begin
  Intercept([plMode]);
end;

procedure TMockADOConnection.Set_Provider(const pbstr: WideString);
begin
  Intercept([pbstr]);
end;

function TMockADOConnection.Unadvise(dwCookie: Integer): HResult;
begin
  //Result := Intercept([dwCookie]).AsInteger;
  Result := 0;
end;

end.
