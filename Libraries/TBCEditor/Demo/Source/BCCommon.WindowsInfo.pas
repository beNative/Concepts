{ Ripped from JEDI: JclSysInfo, JclFileUtils, JclSysUtils, JclRegistry, JclWideStrings, and JclResources.
  TODO: Remove obsolete code. }
unit BCCommon.WindowsInfo;

interface

function GetWindowsVersionString: string;
function GetWindowsEditionString: string;

implementation

uses
  Winapi.Windows, System.SysUtils, System.Character;

type
  TRootKey = record
    Key: HKEY;
    AnsiName: AnsiString;
    WideName: WideString;
  end;

const
  RegKeyDelimiter = '\';

  HKEY_CLASSES_ROOTLongName = 'HKEY_CLASSES_ROOT';
  HKEY_CURRENT_USERLongName = 'HKEY_CURRENT_USER';
  HKEY_LOCAL_MACHINELongName = 'HKEY_LOCAL_MACHINE';
  HKEY_USERSLongName = 'HKEY_USERS';
  HKEY_PERFORMANCE_DATALongName = 'HKEY_PERFORMANCE_DATA';
  HKEY_CURRENT_CONFIGLongName = 'HKEY_CURRENT_CONFIG';
  HKEY_DYN_DATALongName = 'HKEY_DYN_DATA';
  HKEY_CLASSES_ROOTShortName = 'HKEY_CLASSES_ROOT';
  HKEY_CURRENT_USERShortName = 'HKEY_CURRENT_USER';
  HKEY_LOCAL_MACHINEShortName = 'HKEY_LOCAL_MACHINE';
  HKEY_USERSShortName = 'HKEY_USERS';
  HKEY_PERFORMANCE_DATAShortName = 'HKEY_PERFORMANCE_DATA';
  HKEY_CURRENT_CONFIGShortName = 'HKEY_CURRENT_CONFIG';
  HKEY_DYN_DATAShortName = 'HKEY_DYN_DATA';

  RootKeys: array [0..13] of TRootKey =
   (
    (Key: HKEY_CLASSES_ROOT; AnsiName: HKEY_CLASSES_ROOTLongName; WideName: HKEY_CLASSES_ROOTLongName),
    (Key: HKEY_CURRENT_USER; AnsiName: HKEY_CURRENT_USERLongName; WideName: HKEY_CURRENT_USERLongName),
    (Key: HKEY_LOCAL_MACHINE; AnsiName: HKEY_LOCAL_MACHINELongName; WideName: HKEY_LOCAL_MACHINELongName),
    (Key: HKEY_USERS; AnsiName: HKEY_USERSLongName; WideName: HKEY_USERSLongName),
    (Key: HKEY_PERFORMANCE_DATA; AnsiName: HKEY_PERFORMANCE_DATALongName; WideName: HKEY_PERFORMANCE_DATALongName),
    (Key: HKEY_CURRENT_CONFIG; AnsiName: HKEY_CURRENT_CONFIGLongName; WideName: HKEY_CURRENT_CONFIGLongName),
    (Key: HKEY_DYN_DATA; AnsiName: HKEY_DYN_DATALongName; WideName: HKEY_DYN_DATALongName),
    (Key: HKEY_CLASSES_ROOT; AnsiName: HKEY_CLASSES_ROOTShortName; WideName: HKEY_CLASSES_ROOTShortName),
    (Key: HKEY_CURRENT_USER; AnsiName: HKEY_CURRENT_USERShortName; WideName: HKEY_CURRENT_USERShortName),
    (Key: HKEY_LOCAL_MACHINE; AnsiName: HKEY_LOCAL_MACHINEShortName; WideName: HKEY_LOCAL_MACHINEShortName),
    (Key: HKEY_USERS; AnsiName: HKEY_USERSShortName; WideName: HKEY_USERSShortName),
    (Key: HKEY_PERFORMANCE_DATA; AnsiName: HKEY_PERFORMANCE_DATAShortName; WideName: HKEY_PERFORMANCE_DATAShortName),
    (Key: HKEY_CURRENT_CONFIG; AnsiName: HKEY_CURRENT_CONFIGShortName; WideName: HKEY_CURRENT_CONFIGShortName),
    (Key: HKEY_DYN_DATA; AnsiName: HKEY_DYN_DATAShortName; WideName: HKEY_DYN_DATAShortName)
   );

  HexPrefixPascal = string('$');
  HexPrefixC      = string('0x');
  HexDigitFmt32   = string('%.8x');
  HexDigitFmt64   = string('%.16x');

  HexPrefix = HexPrefixPascal;

  {$IFDEF CPU64}
  HexDigitFmt = HexDigitFmt64;
  {$ELSE}
  HexDigitFmt = HexDigitFmt32;
  {$ENDIF CPU64}

  HexFmt = HexPrefix + HexDigitFmt;

  DirDelimiter = '\';

type
  TWindowsVersion =
   (wvUnknown, wvWin95, wvWin95OSR2, wvWin98, wvWin98SE, wvWinME,
    wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4, wvWin2000, wvWinXP,
    wvWin2003, wvWinXP64, wvWin2003R2, wvWinVista, wvWinServer2008,
    wvWin7, wvWinServer2008R2, wvWin8, wvWin8RT, wvWinServer2012,
    wvWin81, wvWin81RT, wvWinServer2012R2, wvWin10, wvWinServer2016);
  TWindowsEdition =
   (weUnknown, weWinXPHome, weWinXPPro, weWinXPHomeN, weWinXPProN, weWinXPHomeK,
    weWinXPProK, weWinXPHomeKN, weWinXPProKN, weWinXPStarter, weWinXPMediaCenter,
    weWinXPTablet, weWinVistaStarter, weWinVistaHomeBasic, weWinVistaHomeBasicN,
    weWinVistaHomePremium, weWinVistaBusiness, weWinVistaBusinessN,
    weWinVistaEnterprise, weWinVistaUltimate, weWin7Starter, weWin7HomeBasic,
    weWin7HomePremium, weWin7Professional, weWin7Enterprise, weWin7Ultimate,
    weWin8, weWin8Pro, weWin8Enterprise, weWin8RT, weWin81, weWin81Pro,
    weWin81Enterprise, weWin81RT, weWin10, weWin10Home, weWin10Pro,
    weWin10Enterprise, weWin10Education);

  TRegKind = REG_NONE..REG_QWORD;
  TRegKinds = set of TRegKind;

  TJclRegWOW64Access = (raDefault, raNative, ra32Key, ra64Key);

resourcestring
  RsUnableToOpenKeyRead  = 'Unable to open key "%s\%s" for read';
  RsUnableToOpenKeyWrite = 'Unable to open key "%s\%s" for write';
  RsUnableToAccessValue  = 'Unable to open key "%s\%s" and access value "%s"';
  RsWrongDataType        = '"%s\%s\%s" is of wrong kind or size';
  RsInconsistentPath     = '"%s" does not match RootKey';

  RsOSVersionWin95              = 'Windows 95';
  RsOSVersionWin95OSR2          = 'Windows 95 OSR2';
  RsOSVersionWin98              = 'Windows 98';
  RsOSVersionWin98SE            = 'Windows 98 SE';
  RsOSVersionWinME              = 'Windows ME';
  RsOSVersionWinNT3             = 'Windows NT 3.%u';
  RsOSVersionWinNT4             = 'Windows NT 4.%u';
  RsOSVersionWin2000            = 'Windows 2000';
  RsOSVersionWinXP              = 'Windows XP';
  RsOSVersionWin2003            = 'Windows Server 2003';
  RsOSVersionWin2003R2          = 'Windows Server 2003 R2';
  RsOSVersionWinXP64            = 'Windows XP x64';
  RsOSVersionWinVista           = 'Windows Vista';
  RsOSVersionWinServer2008      = 'Windows Server 2008';
  RsOSVersionWin7               = 'Windows 7';
  RsOSVersionWinServer2008R2    = 'Windows Server 2008 R2';
  RsOSVersionWin8               = 'Windows 8';
  RsOSVersionWin8RT             = 'Windows RT';
  RsOSVersionWinServer2012      = 'Windows Server 2012';
  RsOSVersionWin81              = 'Windows 8.1';
  RsOSVersionWin81RT            = 'Windows RT 8.1';
  RsOSVersionWinServer2012R2    = 'Windows Server 2012 R2';
  RsOSVersionWin10              = 'Windows 10';
  RsOSVersionWinServer2016      = 'Windows Server 2016';

  RsEditionWinXPHome            = 'Home Edition';
  RsEditionWinXPPro             = 'Professional';
  RsEditionWinXPHomeN           = 'Home Edition N';
  RsEditionWinXPProN            = 'Professional N';
  RsEditionWinXPHomeK           = 'Home Edition K';
  RsEditionWinXPProK            = 'Professional K';
  RsEditionWinXPHomeKN          = 'Home Edition KN';
  RsEditionWinXPProKN           = 'Professional KN';
  RsEditionWinXPStarter         = 'Starter Edition';
  RsEditionWinXPMediaCenter     = 'Media Center Edition';
  RsEditionWinXPTablet          = 'Tablet PC Edition';
  RsEditionWinVistaStarter      = 'Starter';
  RsEditionWinVistaHomeBasic    = 'Home Basic';
  RsEditionWinVistaHomeBasicN   = 'Home Basic N';
  RsEditionWinVistaHomePremium  = 'Home Premium';
  RsEditionWinVistaBusiness     = 'Business';
  RsEditionWinVistaBusinessN    = 'Business N';
  RsEditionWinVistaEnterprise   = 'Enterprise';
  RsEditionWinVistaUltimate     = 'Ultimate';
  RsEditionWin7Starter          = 'Starter';
  RsEditionWin7HomeBasic        = 'Home Basic';
  RsEditionWin7HomePremium      = 'Home Premium';
  RsEditionWin7Professional     = 'Professional';
  RsEditionWin7Enterprise       = 'Enterprise';
  RsEditionWin7Ultimate         = 'Ultimate';
  RsEditionWin8Pro              = 'Pro';
  RsEditionWin8Enterprise       = 'Enterprise';
  RsEditionWin8RT               = 'RT';
  RsEditionWin81Pro             = 'Pro';
  RsEditionWin81Enterprise      = 'Enterprise';
  RsEditionWin81RT              = 'RT';
  RsEditionWin10Home            = 'Home';
  RsEditionWin10Pro             = 'Pro';
  RsEditionWin10Enterprise      = 'Enterprise';
  RsEditionWin10Education       = 'Education';

var
  KernelVersionHi: DWORD;
  CachedIsWindows64: Integer = -1;

  ProcessorCount: Cardinal = 0;
  AllocGranularity: Cardinal = 0;
  PageSize: Cardinal = 0;

   IsWin95: Boolean = False;
  IsWin95OSR2: Boolean = False;
  IsWin98: Boolean = False;
  IsWin98SE: Boolean = False;
  IsWinME: Boolean = False;
  IsWinNT: Boolean = False;
  IsWinNT3: Boolean = False;
  IsWinNT31: Boolean = False;
  IsWinNT35: Boolean = False;
  IsWinNT351: Boolean = False;
  IsWinNT4: Boolean = False;
  IsWin2K: Boolean = False;
  IsWinXP: Boolean = False;
  IsWin2003: Boolean = False;
  IsWinXP64: Boolean = False;
  IsWin2003R2: Boolean = False;
  IsWinVista: Boolean = False;
  IsWinServer2008: Boolean = False;
  IsWin7: Boolean = False;
  IsWinServer2008R2: Boolean = False;
  IsWin8: Boolean = False;
  IsWin8RT: Boolean = False;
  IsWinServer2012: Boolean = False;
  IsWin81: Boolean = False;
  IsWin81RT: Boolean = False;
  IsWinServer2012R2: Boolean = False;
  IsWin10: Boolean = False;
  IsWinServer2016: Boolean = False;

threadvar
  JclRegWOW64Access: TJclRegWOW64Access {= raDefault};

function StrPosW(const Str, SubStr: PWideChar): PWideChar;
var
  P: PWideChar;
  I: Integer;
begin
  Result := nil;
  if (Str = nil) or (SubStr = nil) or (Str^ = #0) or (SubStr^ = #0) then
    Exit;
  Result := Str;
  while Result^ <> #0 do
  begin
    if Result^ <> SubStr^ then
      Inc(Result)
    else
    begin
      P := Result + 1;
      I := 1;
      while (P^ <> #0) and (P^ = SubStr[I]) do
      begin
        Inc(I);
        Inc(P);
      end;
      if SubStr[I] = #0 then
        Exit
      else
        Inc(Result);
    end;
  end;
  Result := nil;
end;

function RelativeKey(const RootKey: HKEY; Key: PWideChar): PWideChar; overload;
var
  I: Integer;
begin
  Result := Key;
  if Result^ = RegKeyDelimiter then
    Inc(Result);
  for I := Low(RootKeys) to High(RootKeys) do
    if StrPosW(Key, PWideChar(RootKeys[I].WideName + RegKeyDelimiter)) = Result then
    begin
      if RootKey <> RootKeys[I].Key then
        raise Exception.CreateResFmt(@RsInconsistentPath, [Key])
      else
        Inc(Result, Length(RootKeys[I].WideName));
      Break;
    end;
end;

function IsWindows64: Boolean;
var
  ASystemInfo: TSystemInfo;
begin
  ASystemInfo.dwOemId := 0;
  GetNativeSystemInfo(ASystemInfo);
  Result := ASystemInfo.wProcessorArchitecture in [PROCESSOR_ARCHITECTURE_IA64,PROCESSOR_ARCHITECTURE_AMD64];
end;

function GetWOW64AccessMode(samDesired: REGSAM): REGSAM;
const
  KEY_WOW64_32KEY = $0200;
  KEY_WOW64_64KEY = $0100;
  KEY_WOW64_RES = $0300;
  RegWOW64Accesses: array[Boolean, TJclRegWOW64Access] of HKEY = (
    (HKEY(0), HKEY(0), HKEY(0), HKEY(0)),
    (HKEY(0), KEY_WOW64_64KEY, KEY_WOW64_32KEY, KEY_WOW64_64KEY)
  );
begin
  Result := samDesired;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (samDesired and KEY_WOW64_RES = 0) then
  begin
    if CachedIsWindows64 = -1 then
      if IsWindows64 then
        CachedIsWindows64 := 1
      else
        CachedIsWindows64 := 0;

    Result := Result or RegWOW64Accesses[CachedIsWindows64 = 1, JclRegWOW64Access];
  end;
end;

function InternalRegOpenKeyEx(Key: HKEY; SubKey: PWideChar;
  ulOptions: DWORD; samDesired: REGSAM; var RegKey: HKEY): Longint; overload;
var
  RelKey: AnsiString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := RegOpenKeyExW(Key, RelativeKey(Key, SubKey), ulOptions, GetWOW64AccessMode(samDesired), RegKey)
  else
  begin
    RelKey := AnsiString(WideString(RelativeKey(Key, SubKey)));
    Result := RegOpenKeyExA(Key, PAnsiChar(RelKey), ulOptions, samDesired, RegKey);
  end;
end;

function InternalRegQueryValueEx(Key: HKEY; ValueName: PWideChar;
  lpReserved: Pointer; lpType: PDWORD; lpData: Pointer; lpcbData: PDWORD): Longint;
var
  ValName: AnsiString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := RegQueryValueExW(Key, ValueName, lpReserved, lpType, lpData, lpcbData)
  else
  begin
    ValName := AnsiString(WideString(ValueName));
    Result := RegQueryValueExA(Key, PAnsiChar(ValName), lpReserved, lpType, lpData, lpcbData);
  end;
end;

function RootKeyName(const RootKey: HKEY): string;
begin
  if RootKey = HKEY_CLASSES_ROOT then
    Result := HKEY_CLASSES_ROOTLongName
  else
  if RootKey = HKEY_CURRENT_USER then
    Result := HKEY_CURRENT_USERLongName
  else
  if RootKey = HKEY_LOCAL_MACHINE then
    Result := HKEY_LOCAL_MACHINELongName
  else
  if RootKey = HKEY_USERS then
    Result := HKEY_USERSLongName
  else
  if RootKey = HKEY_PERFORMANCE_DATA then
    Result := HKEY_PERFORMANCE_DATALongName
  else
  if RootKey = HKEY_CURRENT_CONFIG then
    Result := HKEY_CURRENT_CONFIGLongName
  else
  if RootKey = HKEY_DYN_DATA then
    Result := HKEY_DYN_DATALongName
  else
    Result := Format(HexFmt, [RootKey]);
end;

procedure ValueError(const RootKey: THandle; const Key, Name: string);
begin
  raise Exception.CreateResFmt(@RsUnableToAccessValue, [RootKeyName(RootKey), Key, Name]);
end;

procedure DataError(const RootKey: THandle; const Key, Name: string);
begin
  raise Exception.CreateResFmt(@RsWrongDataType, [RootKeyName(RootKey), Key, Name]);
end;

procedure ReadError(const RootKey: THandle; const Key: string);
begin
  raise Exception.CreateResFmt(@RsUnableToOpenKeyRead, [RootKeyName(RootKey), Key]);
end;

function InternalGetWideString(const RootKey: HKEY; const Key, Name: WideString; MultiFlag: Boolean;
  out RetValue: WideString; RaiseException: Boolean): Boolean;
var
  RegKey: HKEY;
  DataType, DataSize: DWORD;
  RegKinds: TRegKinds;
  DataLength: Integer;
begin
  Result := True;
  DataType := REG_NONE;
  DataSize := 0;
  RetValue := '';
  RegKey := 0;
  if InternalRegOpenKeyEx(RootKey, PWideChar(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    try
      if InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
          RegKinds := [REG_BINARY]
        else
        if MultiFlag then
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ]
        else
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ];
        if DataType in RegKinds then
        begin
          DataLength := DataSize div SizeOf(WideChar);
          SetLength(RetValue, DataLength);
          if InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, nil, PWideChar(RetValue), @DataSize) = ERROR_SUCCESS then
            SetLength(RetValue, DataLength - 1)
          else
          begin
            RetValue := '';
            if RaiseException then
              ValueError(RootKey, Key, Name)
            else
              Result := False;
          end;
        end
        else
        begin
          RetValue := '';
          if RaiseException then
            DataError(RootKey, Key, Name)
          else
            Result := False;
        end;
      end
      else
        if RaiseException then
          ValueError(RootKey, Key, Name)
        else
          Result := False;
    finally
      RegCloseKey(RegKey);
    end
  else
    if RaiseException then
      ReadError(RootKey, Key)
    else
      Result := False;
end;


function RegReadStringEx(const RootKey: HKEY; const Key, Name: string;
  out RetValue: string; RaiseException: Boolean): Boolean;
var
  TmpRet: WideString;
begin
  Result := InternalGetWideString(RootKey, Key, Name, False, TmpRet, RaiseException);
  RetValue := string(TmpRet);
end;

function RegReadStringDef(const RootKey: HKEY; const Key, Name: string; Def: string): string;
begin
  try
    if not RegReadStringEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegGetDataType(const RootKey: HKEY; const Key, Name: string;
  out DataType: DWORD): Boolean;
var
  RegKey: HKEY;
begin
  DataType := REG_NONE;
  RegKey := 0;
  Result := InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, @DataType, nil, nil) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

function RegReadString(const RootKey: HKEY; const Key, Name: string): string;
begin
  RegReadStringEx(RootKey, Key, Name, Result, True);
end;

function InternalGetData(const RootKey: HKEY; const Key, Name: WideString;
  RegKinds: TRegKinds; ExpectedSize: DWORD;
  out DataType: DWORD; Data: Pointer; out DataSize: DWORD; RaiseException: Boolean): Boolean;
var
  RegKey: HKEY;
begin
  Result := True;
  DataType := REG_NONE;
  DataSize := 0;
  RegKey := 0;
  if InternalRegOpenKeyEx(RootKey, PWideChar(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    try
      if InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        if not (DataType in RegKinds) or (DataSize > ExpectedSize) then
          if RaiseException then
            DataError(RootKey, Key, Name)
          else
            Result := False;
        if InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, nil, Data, @DataSize) <> ERROR_SUCCESS then
          if RaiseException then
            ValueError(RootKey, Key, Name)
          else
            Result := False;
      end
      else
        if RaiseException then
          ValueError(RootKey, Key, Name)
        else
          Result := False;
    finally
      RegCloseKey(RegKey);
    end
  else
    if RaiseException then
      ReadError(RootKey, Key)
    else
      Result := False;
end;

function RegReadIntegerEx(const RootKey: HKEY; const Key, Name: string;
  out RetValue: Integer; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  Ret := 0;
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    if RaiseException then
    begin
      Ret := StrToInt64(RegReadString(RootKey, Key, Name));
      Result := True;
    end
    else
      Result := TryStrToInt64(RegReadString(RootKey, Key, Name), Ret)
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(Ret), DataType, @Ret, DataSize, RaiseException);
  RetValue := Integer(Ret and $FFFFFFFF);
end;

function RegReadIntegerDef(const RootKey: HKEY; const Key, Name: string; Def: Integer): Integer;
begin
  try
    if not RegReadIntegerEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

procedure StrUpperInPlace(var S: string);
var
  P: PChar;
  I, L: Integer;
begin
  L := Length(S);
  if L > 0 then
  begin
    UniqueString(S);
    P := PChar(S);
    for I := 1 to L do
    begin
      P^ := P^.ToUpper;
      Inc(P);
    end;
  end;
end;

function StrUpper(const S: string): string;
begin
  Result := S;
  StrUpperInPlace(Result);
end;

function StrIPos(const SubStr, S: string): Integer;
begin
  Result := Pos(StrUpper(SubStr), StrUpper(S));
end;

function StrFind(const Substr, S: string; const Index: Integer): Integer;
var
  pos: Integer;
begin
  if (SubStr <> '') and (S <> '') then
  begin
    pos := StrIPos(Substr, Copy(S, Index, Length(S) - Index + 1));
    if pos = 0 then
      Result := 0
    else
      Result := Index + Pos - 1;
  end
  else
    Result := 0;
end;

function StrLeft(const S: string; Count: Integer): string;
begin
  Result := Copy(S, 1, Count);
end;

function StrBefore(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;

function GetWindowsMajorVersionNumber: Integer;
begin
  // Starting with Windows 8.1, the GetVersion(Ex) API is deprecated and will detect the
  // application as Windows 8 (kernel version 6.2) until an application manifest is included
  // See https://msdn.microsoft.com/en-us/library/windows/desktop/dn302074.aspx
  if (Win32MajorVersion = 6) and (Win32MinorVersion = 2) then
  begin
    // CurrentMajorVersionNumber present in registry starting with Windows 10
    // If CurrentMajorVersionNumber not present in registry then use CurrentVersion
    Result := RegReadIntegerDef(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows NT\CurrentVersion', 'CurrentMajorVersionNumber', -1);
    if Result = -1 then
      Result := strToInt(StrBefore('.', RegReadStringDef(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows NT\CurrentVersion', 'CurrentVersion', intToStr(Win32MajorVersion) + '.' + intToStr(Win32MinorVersion))));
  end
  else
    Result := Win32MajorVersion;
end;

function StrRestOf(const S: string; N: Integer): string;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrAfter(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

function GetWindowsMinorVersionNumber: Integer;
begin
  // Starting with Windows 8.1, the GetVersion(Ex) API is deprecated and will detect the
  // application as Windows 8 (kernel version 6.2) until an application manifest is included
  // See https://msdn.microsoft.com/en-us/library/windows/desktop/dn302074.aspx
  if (Win32MajorVersion = 6) and (Win32MinorVersion = 2) then
  begin
    // CurrentMinorVersionNumber present in registry starting with Windows 10
    // If CurrentMinorVersionNumber not present then use CurrentVersion
    Result := RegReadIntegerDef(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows NT\CurrentVersion', 'CurrentMinorVersionNumber', -1);
    if Result = -1 then
      Result := strToInt(StrAfter('.', RegReadStringDef(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows NT\CurrentVersion', 'CurrentVersion', intToStr(Win32MajorVersion) + '.' + intToStr(Win32MinorVersion))));
  end
  else
    Result := Win32MinorVersion;
end;

function GetWindowsVersion: TWindowsVersion;
var
  TrimmedWin32CSDVersion: string;
  SystemInfo: TSystemInfo;
  OSVersionInfoEx: TOSVersionInfoEx;
  Win32MajorVersionEx, Win32MinorVersionEx: integer;
  ProductName: string;
const
  SM_SERVERR2 = 89;
begin
  Win32MajorVersionEx := -1;
  Win32MinorVersionEx := -1;
  Result := wvUnknown;
  TrimmedWin32CSDVersion := Trim(Win32CSDVersion);

  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      case Win32MinorVersion of
        0..9:
          if (TrimmedWin32CSDVersion = 'B') or (TrimmedWin32CSDVersion = 'C') then
            Result := wvWin95OSR2
          else
            Result := wvWin95;
        10..89:
          // On Windows ME Win32MinorVersion can be 10 (indicating Windows 98
          // under certain circumstances (image name is setup.exe). Checking
          // the kernel version is one way of working around that.
          if KernelVersionHi = $0004005A then // 4.90.x.x
            Result := wvWinME
          else
          if (TrimmedWin32CSDVersion = 'A') or (TrimmedWin32CSDVersion = 'B') then
            Result := wvWin98SE
          else
            Result := wvWin98;
        90:
          Result := wvWinME;
      end;
    VER_PLATFORM_WIN32_NT:
      case Win32MajorVersion of
        3:
          case Win32MinorVersion of
            1:
              Result := wvWinNT31;
            5:
              Result := wvWinNT35;
            51:
              Result := wvWinNT351;
          end;
        4:
          Result := wvWinNT4;
        5:
          case Win32MinorVersion of
            0:
              Result := wvWin2000;
            1:
              Result := wvWinXP;
            2:
              begin
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                SystemInfo.dwOemId := 0;
                GetNativeSystemInfo(SystemInfo);
                if GetSystemMetrics(SM_SERVERR2) <> 0 then
                  Result := wvWin2003R2
                else
                if (SystemInfo.wProcessorArchitecture <> PROCESSOR_ARCHITECTURE_INTEL) and
                  GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWinXP64
                else
                  Result := wvWin2003;
              end;
          end;
        6:
        begin
          // Starting with Windows 8.1, the GetVersion(Ex) API is deprecated and will detect the
          // application as Windows 8 (kernel version 6.2) until an application manifest is included
          // See https://msdn.microsoft.com/en-us/library/windows/desktop/dn302074.aspx

          if Win32MinorVersion = 2 then
          begin
            ProductName := RegReadStringDef(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows NT\CurrentVersion', 'ProductName', '');
            if (pos(RsOSVersionWin81, ProductName) = 1) or (pos(RsOSVersionWinServer2012R2, ProductName) = 1) then
              Win32MinorVersionEx := 3 // Windows 8.1 and Windows Server 2012R2
            else
            if (pos(RsOSVersionWin8, ProductName) = 1) or (pos(RsOSVersionWinServer2012, ProductName) = 1) then
              Win32MinorVersionEx := 2 // Windows 8 and Windows Server 2012
            else
            begin
              Win32MajorVersionEx := GetWindowsMajorVersionNumber;
              if Win32MajorVersionEx = 6 then
                 Win32MinorVersionEx := 4 // Windows 10 (builds < 9926) and Windows Server 2016 (builds < 10074)
              else
              if Win32MajorVersionEx = 10 then
                 Win32MinorVersionEx := -1 // Windows 10 (builds >= 9926) and Windows Server 2016 (builds >= 10074), set to -1 to escape case block
              else
                 Win32MinorVersionEx := Win32MinorVersion;
            end;
          end
          else
            Win32MinorVersionEx := Win32MinorVersion;

          case Win32MinorVersionEx of
            0:
              begin
                // Windows Vista and Windows Server 2008
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                if GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWinVista
                else
                  Result := wvWinServer2008;
              end;
            1:
              begin
                // Windows 7 and Windows Server 2008 R2
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                if GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWin7
                else
                  Result := wvWinServer2008R2;
              end;
            2:
              begin
                // Windows 8 and Windows Server 2012
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                if GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWin8
                else
                  Result := wvWinServer2012;
              end;
            3:
              begin
                // Windows 8.1 and Windows Server 2012 R2
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                if GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWin81
                else
                  Result := wvWinServer2012R2;
              end;
            4:
              begin
                // Windows 10 (builds < 9926) and Windows Server 2016 (builds < 10074)
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                if GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWin10
                else
                  Result := wvWinServer2016;
              end;
          end;
        end;
        10:
        begin
          // Windows 10 if manifest is present
          Win32MajorVersionEx := Win32MajorVersion;
          Win32MinorVersionEx := Win32MinorVersion;
        end;
      end;
  end;

  // This part will only be hit with Windows 10 and Windows Server 2016 (and newer) where an application manifest is not included
  if (Win32MajorVersionEx >= 10) then
  begin
    case Win32MajorVersionEx of
      10:
      begin
        if (Win32MinorVersionEx = -1) then
          Win32MinorVersionEx := GetWindowsMinorVersionNumber;
        case Win32MinorVersionEx of
          0:
            begin
              // Windows 10 (builds >= 9926) and Windows Server 2016 (builds >= 10074)
              OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
              if GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                Result := wvWin10
              else
                Result := wvWinServer2016;
            end;
        end;
      end;
    end;
  end;
end;

function GetWindowsVersionString: string;
begin
  case GetWindowsVersion of
    wvWin95:
      Result := LoadResString(@RsOSVersionWin95);
    wvWin95OSR2:
      Result := LoadResString(@RsOSVersionWin95OSR2);
    wvWin98:
      Result := LoadResString(@RsOSVersionWin98);
    wvWin98SE:
      Result := LoadResString(@RsOSVersionWin98SE);
    wvWinME:
      Result := LoadResString(@RsOSVersionWinME);
    wvWinNT31, wvWinNT35, wvWinNT351:
      Result := Format(LoadResString(@RsOSVersionWinNT3), [Win32MinorVersion]);
    wvWinNT4:
      Result := Format(LoadResString(@RsOSVersionWinNT4), [Win32MinorVersion]);
    wvWin2000:
      Result := LoadResString(@RsOSVersionWin2000);
    wvWinXP:
      Result := LoadResString(@RsOSVersionWinXP);
    wvWin2003:
      Result := LoadResString(@RsOSVersionWin2003);
    wvWin2003R2:
      Result := LoadResString(@RsOSVersionWin2003R2);
    wvWinXP64:
      Result := LoadResString(@RsOSVersionWinXP64);
    wvWinVista:
      Result := LoadResString(@RsOSVersionWinVista);
    wvWinServer2008:
      Result := LoadResString(@RsOSVersionWinServer2008);
    wvWin7:
      Result := LoadResString(@RsOSVersionWin7);
    wvWinServer2008R2:
      Result := LoadResString(@RsOSVersionWinServer2008R2);
    wvWin8:
      Result := LoadResString(@RsOSVersionWin8);
    wvWin8RT:
      Result := LoadResString(@RsOSVersionWin8RT);
    wvWinServer2012:
      Result := LoadResString(@RsOSVersionWinServer2012);
    wvWin81:
      Result := LoadResString(@RsOSVersionWin81);
    wvWin81RT:
      Result := LoadResString(@RsOSVersionWin81RT);
    wvWinServer2012R2:
      Result := LoadResString(@RsOSVersionWinServer2012R2);
    wvWin10:
      Result := LoadResString(@RsOSVersionWin10);
    wvWinServer2016:
      Result := LoadResString(@RsOSVersionWinServer2016);
  else
    Result := '';
  end;
end;

function GetWindowsEdition: TWindowsEdition;
const
  ProductName = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
var
  Edition: string;
begin
  Result := weUnknown;
  Edition := RegReadStringDef(HKEY_LOCAL_MACHINE, ProductName, 'ProductName', '');

  // Remove (tm) in 'Windows (TM) Vista Ultimate'
  Edition := StringReplace(Edition, '(TM) ', '', [rfReplaceAll, rfIgnoreCase]);

  if Pos('Windows XP', Edition) = 1 then
  begin
   // Windows XP Editions
   if Pos('Home Edition N', Edition) > 0 then
      Result := weWinXPHomeN
   else
   if Pos('Professional N', Edition) > 0 then
      Result := weWinXPProN
   else
   if Pos('Home Edition K', Edition) > 0 then
      Result := weWinXPHomeK
   else
   if Pos('Professional K', Edition) > 0 then
      Result := weWinXPProK
   else
   if Pos('Home Edition KN', Edition) > 0 then
      Result := weWinXPHomeKN
   else
   if Pos('Professional KN', Edition) > 0 then
      Result := weWinXPProKN
   else
   if Pos('Home', Edition) > 0 then
      Result := weWinXPHome
   else
   if Pos('Professional', Edition) > 0 then
      Result := weWinXPPro
   else
   if Pos('Starter', Edition) > 0 then
      Result := weWinXPStarter
   else
   if Pos('Media Center', Edition) > 0 then
      Result := weWinXPMediaCenter
   else
   if Pos('Tablet', Edition) > 0 then
      Result := weWinXPTablet;
  end
  else
  if (Pos('Windows Vista', Edition) = 1) then
  begin
   // Windows Vista Editions
   if Pos('Starter', Edition) > 0 then
      Result := weWinVistaStarter
   else
   if Pos('Home Basic N', Edition) > 0 then
      Result := weWinVistaHomeBasicN
   else
   if Pos('Home Basic', Edition) > 0 then
      Result := weWinVistaHomeBasic
   else
   if Pos('Home Premium', Edition) > 0 then
      Result := weWinVistaHomePremium
   else
   if Pos('Business N', Edition) > 0 then
      Result := weWinVistaBusinessN
   else
   if Pos('Business', Edition) > 0 then
      Result := weWinVistaBusiness
   else
   if Pos('Enterprise', Edition) > 0 then
      Result := weWinVistaEnterprise
   else
   if Pos('Ultimate', Edition) > 0 then
      Result := weWinVistaUltimate;
  end
  else
  if Pos('Windows 7', Edition) = 1 then
  begin
   // Windows 7 Editions
   if Pos('Starter', Edition) > 0 then
      Result := weWin7Starter
   else
   if Pos('Home Basic', Edition) > 0 then
      Result := weWin7HomeBasic
   else
   if Pos('Home Premium', Edition) > 0 then
      Result := weWin7HomePremium
   else
   if Pos('Professional', Edition) > 0 then
      Result := weWin7Professional
   else
   if Pos('Enterprise', Edition) > 0 then
      Result := weWin7Enterprise
   else
   if Pos('Ultimate', Edition) > 0 then
      Result := weWin7Ultimate;
  end
  else
  if Pos('Windows 8.1', Edition) = 1 then
  begin
   // Windows 8.1 Editions
   if Pos('Pro', Edition) > 0 then
      Result := weWin81Pro
   else
   if Pos('Enterprise', Edition) > 0 then
      Result := weWin81Enterprise
   else
      Result := weWin81;
  end
  else
  if Pos('Windows 8', Edition) = 1 then
  begin
   // Windows 8 Editions
   if Pos('Pro', Edition) > 0 then
      Result := weWin8Pro
   else
   if Pos('Enterprise', Edition) > 0 then
      Result := weWin8Enterprise
   else
      Result := weWin8;
  end
  else
  if Pos('Windows RT 8.1', Edition) = 1 then
    Result := weWin81RT
  else
  if Pos('Windows RT', Edition) = 1 then
    Result := weWin8RT
  else
  if Pos('Windows 10', Edition) = 1 then
  begin
   // Windows 10 Editions
   if Pos('Home', Edition) > 0 then
      Result := weWin10Home
   else
   if Pos('Pro', Edition) > 0 then
      Result := weWin10Pro
   else
   if Pos('Enterprise', Edition) > 0 then
      Result := weWin10Enterprise
   else
   if Pos('Education', Edition) > 0 then
      Result := weWin10Education
   else
      Result := weWin10;
  end
end;


function GetWindowsEditionString: string;
begin
  case GetWindowsEdition of
    weWinXPHome:
      Result := LoadResString(@RsEditionWinXPHome);
    weWinXPPro:
      Result := LoadResString(@RsEditionWinXPPro);
    weWinXPHomeN:
      Result := LoadResString(@RsEditionWinXPHomeN);
    weWinXPProN:
      Result := LoadResString(@RsEditionWinXPProN);
    weWinXPHomeK:
      Result := LoadResString(@RsEditionWinXPHomeK);
    weWinXPProK:
      Result := LoadResString(@RsEditionWinXPProK);
    weWinXPHomeKN:
      Result := LoadResString(@RsEditionWinXPHomeKN);
    weWinXPProKN:
      Result := LoadResString(@RsEditionWinXPProKN);
    weWinXPStarter:
      Result := LoadResString(@RsEditionWinXPStarter);
    weWinXPMediaCenter:
      Result := LoadResString(@RsEditionWinXPMediaCenter);
    weWinXPTablet:
      Result := LoadResString(@RsEditionWinXPTablet);
    weWinVistaStarter:
      Result := LoadResString(@RsEditionWinVistaStarter);
    weWinVistaHomeBasic:
      Result := LoadResString(@RsEditionWinVistaHomeBasic);
    weWinVistaHomeBasicN:
      Result := LoadResString(@RsEditionWinVistaHomeBasicN);
    weWinVistaHomePremium:
      Result := LoadResString(@RsEditionWinVistaHomePremium);
    weWinVistaBusiness:
      Result := LoadResString(@RsEditionWinVistaBusiness);
    weWinVistaBusinessN:
      Result := LoadResString(@RsEditionWinVistaBusinessN);
    weWinVistaEnterprise:
      Result := LoadResString(@RsEditionWinVistaEnterprise);
    weWinVistaUltimate:
      Result := LoadResString(@RsEditionWinVistaUltimate);
    weWin7Starter:
      Result := LoadResString(@RsEditionWin7Starter);
    weWin7HomeBasic:
      Result := LoadResString(@RsEditionWin7HomeBasic);
    weWin7HomePremium:
      Result := LoadResString(@RsEditionWin7HomePremium);
    weWin7Professional:
      Result := LoadResString(@RsEditionWin7Professional);
    weWin7Enterprise:
      Result := LoadResString(@RsEditionWin7Enterprise);
    weWin7Ultimate:
      Result := LoadResString(@RsEditionWin7Ultimate);
    weWin8Pro:
      Result := LoadResString(@RsEditionWin8Pro);
    weWin8Enterprise:
      Result := LoadResString(@RsEditionWin8Enterprise);
    weWin8RT:
      Result := LoadResString(@RsEditionWin8RT);
    weWin81Pro:
      Result := LoadResString(@RsEditionWin81Pro);
    weWin81Enterprise:
      Result := LoadResString(@RsEditionWin81Enterprise);
    weWin81RT:
      Result := LoadResString(@RsEditionWin81RT);
    weWin10Home:
      Result := LoadResString(@RsEditionWin10Home);
    weWin10Pro:
      Result := LoadResString(@RsEditionWin10Pro);
    weWin10Enterprise:
      Result := LoadResString(@RsEditionWin10Enterprise);
    weWin10Education:
      Result := LoadResString(@RsEditionWin10Education);
  else
    Result := '';
  end;
end;

procedure ResetMemory(out P; Size: Longint);
begin
  if Size > 0 then
  begin
    Byte(P) := 0;
    FillChar(P, Size, 0);
  end;
end;

function GetModulePath(const Module: HMODULE): string;
var
  L: Integer;
begin
  L := MAX_PATH + 1;
  SetLength(Result, L);
  L := Winapi.Windows.GetModuleFileName(Module, Pointer(Result), L);
  SetLength(Result, L);
end;

function VersionFixedFileInfo(const FileName: string; var FixedInfo: TVSFixedFileInfo): Boolean;
var
  Size, FixInfoLen: DWORD;
  Handle: DWORD;
  Buffer: string;
  FixInfoBuf: PVSFixedFileInfo;
begin
  Result := False;
  Handle := 0;
  Size := GetFileVersionInfoSize(PChar(FileName), Handle);
  if Size > 0 then
  begin
    SetLength(Buffer, Size);
    FixInfoLen := 0;
    FixInfoBuf := nil;
    if GetFileVersionInfo(PChar(FileName), Handle, Size, Pointer(Buffer)) and
      VerQueryValue(Pointer(Buffer), DirDelimiter, Pointer(FixInfoBuf), FixInfoLen) and
      (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
    begin
      Result := True;
      FixedInfo := FixInfoBuf^;
    end;
  end;
end;

procedure InitSysInfo;
var
  SystemInfo: TSystemInfo;
  Kernel32FileName: string;
  VerFixedFileInfo: TVSFixedFileInfo;
begin
  { processor information related initialization }

  ResetMemory(SystemInfo, SizeOf(SystemInfo));
  GetSystemInfo(SystemInfo);
  ProcessorCount := SystemInfo.dwNumberOfProcessors;
  AllocGranularity := SystemInfo.dwAllocationGranularity;
  PageSize := SystemInfo.dwPageSize;

  { Windows version information }

  IsWinNT := Win32Platform = VER_PLATFORM_WIN32_NT;

  Kernel32FileName := GetModulePath(GetModuleHandle(kernel32));
  VerFixedFileInfo.dwFileDateLS := 0;
  if (not IsWinNT) and VersionFixedFileInfo(Kernel32FileName, VerFixedFileInfo) then
    KernelVersionHi := VerFixedFileInfo.dwProductVersionMS
  else
    KernelVersionHi := 0;

  case GetWindowsVersion of
    wvUnknown:
      ;
    wvWin95:
      IsWin95 := True;
    wvWin95OSR2:
      IsWin95OSR2 := True;
    wvWin98:
      IsWin98 := True;
    wvWin98SE:
      IsWin98SE := True;
    wvWinME:
      IsWinME := True;
    wvWinNT31:
      begin
        IsWinNT3 := True;
        IsWinNT31 := True;
      end;
    wvWinNT35:
      begin
        IsWinNT3 := True;
        IsWinNT35 := True;
      end;
    wvWinNT351:
      begin
        IsWinNT3 := True;
        IsWinNT35 := True;
        IsWinNT351 := True;
      end;
    wvWinNT4:
      IsWinNT4 := True;
    wvWin2000:
      IsWin2K := True;
    wvWinXP:
      IsWinXP := True;
    wvWin2003:
      IsWin2003 := True;
    wvWinXP64:
      IsWinXP64 := True;
    wvWin2003R2:
      IsWin2003R2 := True;
    wvWinVista:
      IsWinVista := True;
    wvWinServer2008:
      IsWinServer2008 := True;
    wvWin7:
      IsWin7 := True;
    wvWinServer2008R2:
      IsWinServer2008R2 := True;
    wvWin8:
      IsWin8 := True;
    wvWin8RT:
      IsWin8RT := True;
    wvWinServer2012:
      IsWinServer2012 := True;
    wvWin81:
      IsWin81 := True;
    wvWin81RT:
      IsWin81RT := True;
    wvWinServer2012R2:
      IsWinServer2012R2 := True;
    wvWin10:
      IsWin10 := True;
    wvWinServer2016:
      IsWinServer2016 := True;
  end;
end;

var
  ResmeterLibHandle: THandle;
  MyGetFreeSystemResources: function(ResType: UINT): UINT; stdcall;

procedure UnloadSystemResourcesMeterLib;
begin
  if ResmeterLibHandle <> 0 then
  begin
    FreeLibrary(ResmeterLibHandle);
    ResmeterLibHandle := 0;
    @MyGetFreeSystemResources := nil;
  end;
end;

procedure FinalizeSysInfo;
begin
  UnloadSystemResourcesMeterLib;
end;

initialization

  InitSysInfo;

finalization

  FinalizeSysInfo;

end.
