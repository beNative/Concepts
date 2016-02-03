unit BCCommon.FileUtils;

interface

uses
  Winapi.Windows, System.Classes, System.Types;

function CountFilesInFolder(AFolderText: string; AFileTypeText: string; AFileExtensions: string): Integer;
function DisplayContextMenu(const Handle: THandle; const FileName: string; Pos: TPoint): Boolean;
function FormatFileName(FileName: string; Modified: Boolean = False): string;
function GetFileDateTime(FileName: string): TDateTime;
function GetFileNamesFromFolder(Folder: string; FileType: string = ''): TStrings;
function GetFiles(const APath, AMasks: string; ALookInSubfolders: Boolean): TStringDynArray;
function GetFileVersion(FileName: string): string;
function GetHighlighters: TStringList;
function GetHighlighterColors: TStringList;
function GetIniFilename: string;
function GetOutFilename: string;
function GetSQLFormatterDLLFilename: string;
function IsVirtualDrive(Drive: Char): Boolean;
function SystemDir: string;
function VirtualDrivePath(Drive: Char): string;
procedure CreateVirtualDrive(const Drive: Char; const Path: string);
procedure DeleteVirtualDrive(const Drive: Char);
procedure FilePropertiesDialog(FileName: string);

implementation

uses
  Winapi.ShellAPI, Winapi.ShlObj, Winapi.ActiveX, Winapi.Messages, System.SysUtils, System.AnsiStrings, Vcl.Forms,
  BCCommon.Language.Strings, BCControl.Utils, System.IOUtils, System.StrUtils, System.Masks;

const
  DirDelimiter = '/';

function PathAddSeparator(const Path: string): string;
begin
  Result := Path;
  if (Path = '') or (Path[Length(Path)] <> DirDelimiter) then
    Result := Path + DirDelimiter;
end;

function DriveToPidlBind(const DriveName: string; out Folder: IShellFolder): PItemIdList;
var
  Attr: ULONG;
  Eaten: ULONG;
  DesktopFolder: IShellFolder;
  Drives: PItemIdList;
  Path: PWideChar;
begin
  Result := nil;
  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
  begin
    if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_DRIVES, Drives)) then
    begin
      if Succeeded(DesktopFolder.BindToObject(Drives, nil, IID_IShellFolder, Pointer(Folder))) then
      begin
        Path := PChar(PathAddSeparator(DriveName));
        Attr := 0;
        if Failed(Folder.ParseDisplayName(0, nil, Path, Eaten, Result, Attr)) then
        begin
          Folder := nil;
          // Failure probably means that this is not a drive. However, do not
          // call PathToPidlBind() because it may cause infinite recursion.
        end;
      end;
    end;
    CoTaskMemFree(Drives);
  end;
end;

function PidlFree(var IdList: PItemIdList): Boolean;
var
  Malloc: IMalloc;
begin
  Result := False;
  if IdList = nil then
    Result := True
  else
  begin
    Malloc := nil;
    if Succeeded(SHGetMalloc(Malloc)) and (Malloc.DidAlloc(IdList) > 0) then
    begin
      Malloc.Free(IdList);
      IdList := nil;
      Result := True;
    end;
  end;
end;

function PathToPidlBind(const FileName: string; out Folder: IShellFolder): PItemIdList;
var
  Attr, Eaten: ULONG;
  PathIdList: PItemIdList;
  DesktopFolder: IShellFolder;
  Path, ItemName: PWideChar;
begin
  Result := nil;
  Path := PChar(ExtractFilePath(FileName));
  ItemName := PChar(ExtractFileName(FileName));

  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
  begin
    Attr := 0;
    if Succeeded(DesktopFolder.ParseDisplayName(0, nil, Path, Eaten, PathIdList, Attr)) then
    begin
      if Succeeded(DesktopFolder.BindToObject(PathIdList, nil, IID_IShellFolder, Pointer(Folder))) then
        if Failed(Folder.ParseDisplayName(0, nil, ItemName, Eaten, Result, Attr)) then
        begin
          Folder := nil;
          Result := DriveToPidlBind(FileName, Folder);
        end;
      PidlFree(PathIdList);
    end
    else
      Result := DriveToPidlBind(FileName, Folder);
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

function MenuCallback(Wnd: THandle; Msg: UINT; wParam: wParam; lParam: lParam): LRESULT; stdcall;
var
  ContextMenu2: IContextMenu2;
begin
  case Msg of
    WM_CREATE:
      begin
        ContextMenu2 := IContextMenu2(PCreateStruct(lParam).lpCreateParams);
        SetWindowLongPtr(Wnd, GWLP_USERDATA, LONG_PTR(ContextMenu2));
        Result := DefWindowProc(Wnd, Msg, wParam, lParam);
      end;
    WM_INITMENUPOPUP:
      begin
        ContextMenu2 := IContextMenu2(GetWindowLongPtr(Wnd, GWLP_USERDATA));
        ContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
        Result := 0;
      end;
    WM_DRAWITEM, WM_MEASUREITEM:
      begin
        ContextMenu2 := IContextMenu2(GetWindowLongPtr(Wnd, GWLP_USERDATA));
        ContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
        Result := 1;
      end;
  else
    Result := DefWindowProc(Wnd, Msg, wParam, lParam);
  end;
end;

function CreateMenuCallbackWnd(const ContextMenu: IContextMenu2): THandle;
const
  IcmCallbackWnd = 'ICMCALLBACKWND';
var
  WndClass: TWndClass;
begin
  ResetMemory(WndClass, SizeOf(WndClass));
  WndClass.lpszClassName := PChar(IcmCallbackWnd);
  WndClass.lpfnWndProc := @MenuCallback;
  WndClass.hInstance := hInstance;
  Winapi.Windows.RegisterClass(WndClass);
  Result := CreateWindow(IcmCallbackWnd, IcmCallbackWnd, WS_POPUPWINDOW, 0, 0, 0, 0, 0, 0, hInstance,
    Pointer(ContextMenu));
end;

function DisplayContextMenuPidl(const Handle: THandle; const Folder: IShellFolder; Item: PItemIdList;
  Pos: TPoint): Boolean;
var
  Cmd: Cardinal;
  ContextMenu: IContextMenu;
  ContextMenu2: IContextMenu2;
  Menu: HMENU;
  CommandInfo: TCMInvokeCommandInfo;
  CallbackWindow: THandle;
begin
  Result := False;
  if (Item = nil) or (Folder = nil) then
    Exit;
  Folder.GetUIObjectOf(Handle, 1, Item, IID_IContextMenu, nil, Pointer(ContextMenu));
  if ContextMenu <> nil then
  begin
    Menu := CreatePopupMenu;
    if Menu <> 0 then
    begin
      if Succeeded(ContextMenu.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_EXPLORE)) then
      begin
        CallbackWindow := 0;
        if Succeeded(ContextMenu.QueryInterface(IContextMenu2, ContextMenu2)) then
        begin
          CallbackWindow := CreateMenuCallbackWnd(ContextMenu2);
        end;
        ClientToScreen(Handle, Pos);
        Cmd := Cardinal(TrackPopupMenu(Menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD, Pos.X,
          Pos.Y, 0, CallbackWindow, nil));
        if Cmd <> 0 then
        begin
          ResetMemory(CommandInfo, SizeOf(CommandInfo));
          CommandInfo.cbSize := SizeOf(TCMInvokeCommandInfo);
          CommandInfo.hwnd := Handle;
          CommandInfo.lpVerb := MakeIntResourceA(Cmd - 1);
          CommandInfo.nShow := SW_SHOWNORMAL;
          Result := Succeeded(ContextMenu.InvokeCommand(CommandInfo));
        end;
        if CallbackWindow <> 0 then
          DestroyWindow(CallbackWindow);
      end;
      DestroyMenu(Menu);
    end;
  end;
end;

function DisplayContextMenu(const Handle: THandle; const FileName: string; Pos: TPoint): Boolean;
var
  ItemIdList: PItemIdList;
  Folder: IShellFolder;
begin
  Result := False;
  ItemIdList := PathToPidlBind(FileName, Folder);
  if ItemIdList <> nil then
  begin
    Result := DisplayContextMenuPidl(Handle, Folder, ItemIdList, Pos);
    PidlFree(ItemIdList);
  end;
end;

function GetFiles(const APath, AMasks: string; ALookInSubfolders: Boolean): TStringDynArray;
var
  MaskArray: TStringDynArray;
  Predicate: TDirectory.TFilterPredicate;
  LSearchOption: TSearchOption;
begin
  if ALookInSubfolders then
    LSearchOption := System.IOUtils.TSearchOption.soAllDirectories
  else
    LSearchOption := System.IOUtils.TSearchOption.soTopDirectoryOnly;

  MaskArray := SplitString(AMasks, ';');
  Predicate := function(const Path: string; const SearchRec: TSearchRec): Boolean
    var
      Mask: string;
    begin
      for Mask in MaskArray do
        if MatchesMask(SearchRec.Name, Mask) then
          Exit(True);
      Exit(False);
    end;
  Result := TDirectory.GetFiles(APath, LSearchOption, Predicate);
end;

function CountFilesInFolder(AFolderText: string; AFileTypeText: string; AFileExtensions: string): Integer;
var
  FName: string;
begin
  Result := 0;
  for FName in GetFiles(AFolderText, AFileTypeText, True) do
  if (AFileExtensions = '*.*') or
    (AFileTypeText = '*.*') and IsExtInFileType(ExtractFileExt(FName), AFileExtensions) or
    IsExtInFileType(ExtractFileExt(FName), AFileTypeText) then
    Inc(Result);
end;

function GetFileDateTime(FileName: string): TDateTime;
var
  SearchRec: TSearchRec;
begin
  FindFirst(FileName, faAnyFile, SearchRec);
  Result := SearchRec.TimeStamp;
end;

function GetFileVersion(FileName: string): string;
var
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
  InfoSize: Cardinal;
  ValueSize: Cardinal;
  Dummy: Cardinal;
  TempPath: PChar;
begin
  if Trim(FileName) = EmptyStr then
    TempPath := PChar(ParamStr(0))
  else
    TempPath := PChar(FileName);

  InfoSize := GetFileVersionInfoSize(TempPath, Dummy);

  if InfoSize = 0 then
    Exit(LanguageDataModule.GetConstant('VersionInfoNotFound'));

  GetMem(VerInfo, InfoSize);
  GetFileVersionInfo(TempPath, 0, InfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), ValueSize);

  with VerValue^ do
    Result := Format('%d.%d.%d', [dwFileVersionMS shr 16, dwFileVersionMS and $FFFF, dwFileVersionLS shr 16]);
  FreeMem(VerInfo, InfoSize);
end;

function GetFileNamesFromFolder(Folder: string; FileType: string): TStrings;
var
  SearchRec: TSearchRec;
begin
  Result := TStringList.Create;
{$WARNINGS OFF} { faHidden is specific to a platform }
  if FindFirst(IncludeTrailingBackslash(Folder) + '*.*', faAnyFile - faHidden, SearchRec) = 0 then
{$WARNINGS ON}
  begin
    repeat
      if SearchRec.Attr <> faDirectory then
        if (FileType = '') or (FileType = '*.*') or
          ((FileType <> '') and IsExtInFileType(ExtractFileExt(SearchRec.Name), FileType)) then
{$WARNINGS OFF} { IncludeTrailingBackslash is specific to a platform }
          Result.Add(IncludeTrailingBackslash(Folder) + SearchRec.Name);
{$WARNINGS ON}
    until FindNext(SearchRec) <> 0;
    System.SysUtils.FindClose(SearchRec);
  end;
end;

function GetIniFilename: string;
begin
  Result := ChangeFileExt(Application.EXEName, '.ini');
end;

function GetOutFilename: string;
begin
  Result := ChangeFileExt(Application.EXEName, '.out');
end;

function GetSQLFormatterDLLFilename: string;
begin
{$WARNINGS OFF} { IncludeTrailingPathDelimiter is specific to a platform }
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.EXEName)) + 'SQLFormatter.dll';
{$WARNINGS ON}
end;

function FormatFileName(FileName: string; Modified: Boolean): string;
begin
  Result := Trim(FileName);
  if Pos('~', Result) = Length(Result) then
    Result := System.Copy(Result, 0, Length(Result) - 1);
  if Modified then
    Result := Format('%s~', [Result]);
end;

function IsVirtualDrive(Drive: Char): Boolean;
var
  DeviceName, TargetPath: string;
begin
  TargetPath := Drive + ':';
  SetLength(DeviceName, Max_Path + 1);
  SetLength(DeviceName, QueryDosDevice(PChar(TargetPath), PChar(DeviceName), Length(DeviceName)));
  Result := Pos('\??\', DeviceName) = 1;
end;

function VirtualDrivePath(Drive: Char): string;
var
  DeviceName, TargetPath: string;
begin
  TargetPath := Drive + ':';
  SetLength(DeviceName, Max_Path + 1);
  SetLength(DeviceName, QueryDosDevice(PWideChar(TargetPath), PWideChar(DeviceName), Length(DeviceName)));
  if Pos('\??\', DeviceName) = 1 then
    Result := Trim(Copy(DeviceName, 5, Length(DeviceName)))
  else
    Result := '';
end;

function SystemDir: string;
begin
  SetLength(Result, Max_Path);
  GetSystemDirectory(@Result[1], Max_Path);
end;

procedure CreateVirtualDrive(const Drive: Char; const Path: string);
var
  Param: string;
begin
  Param := Format('%s: "%s"', [Drive, Path]);
  ShellExecute(1, 'open', 'subst', PChar(Param), PChar(SystemDir), SW_HIDE);
end;

procedure DeleteVirtualDrive(const Drive: Char);
var
  Param: string;
begin
  Param := Format('%s: /d', [Drive]);
  ShellExecute(1, 'open', 'subst', PChar(Param), PChar(SystemDir), SW_HIDE);
end;

procedure FilePropertiesDialog(FileName: string);
var
  ShellExecuteInfo: TShellExecuteInfo;
begin
  if FileName = '' then
    Exit;
  FillChar(ShellExecuteInfo, SizeOf(ShellExecuteInfo), 0);
  ShellExecuteInfo.cbSize := SizeOf(ShellExecuteInfo);
  ShellExecuteInfo.lpFile := PChar(FileName);
  ShellExecuteInfo.lpVerb := 'properties';
  ShellExecuteInfo.fMask  := SEE_MASK_INVOKEIDLIST;
  ShellExecuteEx(@ShellExecuteInfo);
end;

function GetHighlighters: TStringList;
var
  LFileName, LName: string;
begin
  Result := TStringList.Create;
  for LFileName in GetFiles(ExtractFilePath(Application.ExeName) + '\Highlighters\', '*.json', False) do
  begin
    LName := ChangeFileExt(ExtractFileName(LFileName), '');
    Result.Add(LName);
  end;
end;

function GetHighlighterColors: TStringList;
var
  LFileName, LName: string;
begin
  Result := TStringList.Create;
  for LFileName in GetFiles(ExtractFilePath(Application.ExeName) + '\Colors\', '*.json', False) do
  begin
    LName := ChangeFileExt(ExtractFileName(LFileName), '');
    Result.Add(LName);
  end;
end;

end.
