{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kdialogs; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  Classes, Controls, Forms, SysUtils, KFunctions, KControls, KPrintPreview, KPrintSetup;

type
  { Specifies the root folder for TKBrowseFolderDialog dialog - RootFolder property. }
  TKRootFolder = (brAdminTools, brAltStartUp, brAppData,
    brBitBucket, brCommonAdminTools, brCommonAltStartUp,
    brCommonAppData, brCommonDesktopDirectory, brCommonDocuments,
    brCommonFavorites, brCommonPrograms, brCommonStartMenu,
    brCommonStartUp, brCommonTemplates, brControls, brCookies,
    brDesktop, brDesktopDirectory, brDrives, brFavorites, brFonts,
    brHistory, brInternet, brInternetCache, brLocalAppData,
    brMyMusic, brMyPictures, brNetHood, brNetWork, brPersonal,
    brPrinters, brPrintHood, brProfile, brProgramFiles,
    brProgramFilesCommon, brPrograms, brRecent, brSendTo,
    brStartMenu, brStartUp, brSystem, brTemplates, brWindows,
    brCustom);

  TFolder = type string;

  { Specifies the folder options for TKBrowseFolderDialog dialog - Options property. }
  TKBrowseFolderOption = (bfSetFolder, bfBrowseForComputer, bfBrowseForPrinter,
    bfBrowseIncludeFiles, bfBrowseIncludeURLs, bfDontGoBelowDomain,
    bfEditBox, bfNewDialogStyle, bfReturnFSAncestors, bfReturnOnlyFSDirs,
    bfShareAble, bfStatusText, bfUseNewUI, bfValidate);

  TKBrowseFolderOptions = set of TKBrowseFolderOption;

  { Specifies the initial position for TKBrowseFolderDialog dialog - Position property. }
  TKBrowseFolderPosition = (poDefault, poScreenCenter, poCustom);

  { @abstract(Encapsulates the print preview dialog) }
  TKPrintPreviewDialog = class(TComponent)
  private
    FControl: TKCustomControl;
    FPrintPreviewForm: TKPrintPreviewForm;
    function GetPrintPreviewForm: TKPrintPreviewForm;
  public
    { Creates the instance. Assigns default values to properties. }
    constructor Create(AOwner: TComponent); override;
    { Shows the dialog. }
    procedure Show;
    { Shows the dialog as modal dialog. }
    function Execute: Boolean;
    { Specifies the associated preview form. }
    property PrintPreviewForm: TKPrintPreviewForm read GetPrintPreviewForm;
  published
    { Specifies the associated control. }
    property Control: TKCustomControl read FControl write FControl;
  end;

  { @abstract(Encapsulates the print preview dialog) }
  TKPrintSetupDialog = class(TComponent)
  private
    FControl: TKCustomControl;
    FPreviewDialog: TKPrintPreviewDialog;
    FSelAvail: Boolean;
  protected
    FPrintSetupForm: TKPrintSetupForm;
    procedure SetupForm(AfterCreation: Boolean); virtual;
  public
    { Creates the instance. Assigns default values to properties. }
    constructor Create(AOwner: TComponent); override;
    { Shows the dialog as modal dialog. }
    function Execute: Boolean;
  published
    { Specifies the associated control. }
    property Control: TKCustomControl read FControl write FControl;
    { Specifies the preview dialog for the Preview... button.
      If not specified, the print setup dialog creates a new one. }
    property PreviewDialog: TKPrintPreviewDialog read FPreviewDialog write FPreviewDialog;
    { If True, the Selection Only option will be checked (if selection is available
      for the control). }
    property SelAvail: Boolean read FSelAvail write FSelAvail default True;
  end;

  { @abstract(Encapsulates the browse for folder dialog - Windows only) }
  TKBrowseFolderDialog = class(TComponent)
  private
    FParentWindow: TWinControl;
    FFolder: TFolder;
    FLabelText: string;
    FPosition: TKBrowseFolderPosition;
    FCustomRootFolder: TFolder;
    FOptions: TKBrowseFolderOptions;
    FRootFolder: TKRootFolder;
    FCustomLeft,
    FCustomTop: Integer;
    procedure SetFolder(const Value: TFolder);
  public
    { Creates the instance. Assigns default values to properties. }
    constructor Create(AOwner: TComponent); override;
    { Shows the dialog as modal dialog. }
    function Execute: Boolean;
    property LabelText: string read FLabelText write FLabelText;
  published
    property CustomRootFolder: TFolder read FCustomRootFolder write FCustomRootFolder;
    property Folder: TFolder read FFolder write SetFolder;
    property Options: TKBrowseFolderOptions read FOptions write FOptions;
    property ParentWindow: TWinControl read FParentWindow write FParentWindow;
    property Position: TKBrowseFolderPosition read FPosition write FPosition default poScreenCenter;
    property RootFolder: TKRootFolder read FRootFolder write FRootFolder;
    property CustomLeft: Integer index 1 read FCustomLeft write FCustomLeft default 100;
    property CustomTop: Integer index 2 read FCustomTop write FCustomTop default 100;
  end;

implementation

uses
  KRes
{$IFDEF MSWINDOWS}
  , ActiveX, ShlObj, Windows, Messages
{$ENDIF}
  ;

{$IFDEF MSWINDOWS}
const
  {Common Controls version 5.O extensions}
  BIF_BROWSEINCLUDEURLS = $0080;
  BIF_NEWDIALOGSTYLE    = $0040;
  BIF_SHAREABLE         = $8000;
  BIF_USENEWUI          = BIF_NEWDIALOGSTYLE + BIF_EDITBOX;

  CSIDL_FLAG_CREATE       = $8000;

  CSIDL_ADMINTOOLS        = $0030;
  CSIDL_COMMON_ADMINTOOLS = $002F;
  CSIDL_COMMON_APPDATA    = $0023;
  CSIDL_COMMON_DOCUMENTS  = $002E;
  CSIDL_LOCAL_APPDATA     = $001C;
  CSIDL_MYMUSIC           = $0028;
  CSIDL_MYPICTURES        = $0027;
  CSIDL_PROGRAM_FILES     = $0026;
  CSIDL_PROGRAM_FILES_COMMON = $002B;
  CSIDL_SYSTEM            = $0025;
  CSIDL_WINDOWS           = $0024;

{$IFDEF FPC}
// message from browser
  BFFM_INITIALIZED     = 1;
  BFFM_SELCHANGED      = 2;
  BFFM_VALIDATEFAILEDA = 3;     // lParam:szPath ret:1(cont),0(EndDialog)
  BFFM_VALIDATEFAILEDW = 4;     // lParam:wzPath ret:1(cont),0(EndDialog)
  BFFM_IUNKNOWN        = 5;    // provides IUnknown to client. lParam: IUnknown*

// messages to browser
  BFFM_SETSTATUSTEXTA = WM_USER + 100;
  BFFM_ENABLEOK       = WM_USER + 101;
  BFFM_SETSELECTIONA  = WM_USER + 102;
  BFFM_SETSELECTIONW  = WM_USER + 103;
  BFFM_SETSTATUSTEXTW = WM_USER + 104;
  BFFM_SETOKTEXT      = WM_USER + 105; // Unicode only
  BFFM_SETEXPANDED    = WM_USER + 106; // Unicode only

 {$IFDEF UNICODE}
  BFFM_VALIDATEFAILED     = BFFM_VALIDATEFAILEDW;
  BFFM_SETSTATUSTEXT      = BFFM_SETSTATUSTEXTW;
  BFFM_SETSELECTION       = BFFM_SETSELECTIONW;
 {$ELSE}
  BFFM_VALIDATEFAILED     = BFFM_VALIDATEFAILEDA;
  BFFM_SETSTATUSTEXT      = BFFM_SETSTATUSTEXTA;
  BFFM_SETSELECTION       = BFFM_SETSELECTIONA;
 {$ENDIF}
{$ENDIF}

{$ENDIF}

{ TKPrintPreviewDialog }

constructor TKPrintPreviewDialog.Create(AOwner: TComponent);
begin
  inherited;
  FPrintPreviewForm := nil;
  FControl := nil;
end;

function TKPrintPreviewDialog.Execute;
begin
  PrintPreviewForm.Preview.Control := FControl;
  PrintPreviewForm.ShowModal;
  Result := True;
end;

function TKPrintPreviewDialog.GetPrintPreviewForm: TKPrintPreviewForm;
begin
  if not Assigned(FPrintPreviewForm) then
    FPrintPreviewForm := TKPrintPreviewForm.Create(Self);
  Result := FPrintPreviewForm;
end;

procedure TKPrintPreviewDialog.Show;
begin
  PrintPreviewForm.Preview.Control := FControl;
  PrintPreviewForm.Show;
end;

{ TKPrintSetupDialog }

constructor TKPrintSetupDialog.Create(AOwner: TComponent);
begin
  inherited;
  FControl := nil;
  FPrintSetupForm := nil;
  FPreviewDialog := nil;
  FSelAvail := True;
end;

function TKPrintSetupDialog.Execute: Boolean;
begin
  if Assigned(FControl) then
  begin
    if not Assigned(FPrintSetupForm) then
    begin
      FPrintSetupForm := TKPrintSetupForm.Create(Self);
      SetupForm(True);
    end;
    FPrintSetupForm.PageSetup := FControl.PageSetup;
    if Assigned(FPreviewDialog) then
      FPrintSetupForm.PreviewForm := FPreviewDialog.PrintPreviewForm;
    SetupForm(False);
    Result := FPrintSetupForm.ShowModal = mrOk;
  end else
    Result := False;
end;

procedure TKPrintSetupDialog.SetupForm(AfterCreation: Boolean);
begin
  if not AfterCreation then
    FPrintSetupForm.SelAvail := FSelAvail;
end;

{ TKBrowseFolderDialog }

{$IFDEF MSWINDOWS}
function BFCallBack(Wnd: HWND; uMsg: UINT; lPar, lpData: LPARAM): Integer stdcall;
var
  Allocator: IMalloc;
  Location: PItemIDList;
begin
  with TKBrowseFolderDialog(lpData) do
  try
    if uMsg = BFFM_INITIALIZED then
    begin
      if (bfSetFolder in Options) and (Folder <> '') then
        SendMessage(Wnd, BFFM_SETSELECTION, Integer(TRUE), LPARAM(PChar(Folder)))
      else
      begin
        SHGetMAlloc(Allocator);
        try
          SHGetSpecialFolderLocation(Application.MainForm.Handle, CSIDL_DRIVES, Location);
          SendMessage(Wnd, BFFM_SETSELECTION, Integer(FALSE), LPARAM(Location));
        finally
          Allocator.Free(Location);
        end;
      end;
      case Position of
        poScreenCenter: CenterWindowOnScreen(Wnd);
        poCustom: SetWindowPos(Wnd, 0, CustomLeft, CustomTop, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
      end;
    end;
  except
  end;
  Result := 0;
end;
{$ENDIF}

constructor TKBrowseFolderDialog.Create(AOwner: TComponent);
begin
  inherited;
  FParentWindow := nil;
  FFolder := '';
  FLabelText := sBrowseDirectory;
  FOptions := [bfSetFolder, bfReturnOnlyFSDirs, bfDontGoBelowDomain, bfUseNewUI];
  FRootFolder := brDesktop;
  FPosition := poScreenCenter;
  FCustomLeft := 100;
  FCustomTop := 100;
end;

function TKBrowseFolderDialog.Execute: Boolean;
{$IFDEF MSWINDOWS}
var
  BI: TBrowseInfo;
  Buf: PChar;
  List, Root: PItemIDList;
  Allocator: IMalloc;
  DesktopFolder: IShellFolder;
  I: Integer;
  Eaten, Flags: LongWord;
 {$IFDEF FPC}
  DisabledList: TList;
 {$ELSE}
  P: Pointer;
 {$ENDIF}
begin
  Result := False;
  SHGetMAlloc(Allocator);
  if Allocator <> nil then
  begin
    GetMem(Buf, MAX_PATH);
  {$IFDEF FPC}
    DisabledList := Screen.DisableForms(nil, nil);
  {$ELSE}
    P := DisableTaskWindows(0);
  {$ENDIF}
    try
      if FParentWindow <> nil then
        BI.hwndOwner := FParentWindow.Handle
      else
        BI.hwndOwner := Application.MainForm.Handle;
      case FRootFolder of
        brAdminTools: I := CSIDL_ADMINTOOLS;
        brAltStartUp: I := CSIDL_ALTSTARTUP;
        brAppData: I := CSIDL_APPDATA;
        brBitBucket: I := CSIDL_BITBUCKET;
        brCommonAdminTools: I := CSIDL_COMMON_ADMINTOOLS;
        brCommonAltStartUp: I := CSIDL_COMMON_ALTSTARTUP;
        brCommonAppData: I := CSIDL_COMMON_APPDATA;
        brCommonDesktopDirectory: I := CSIDL_COMMON_DESKTOPDIRECTORY;
        brCommonDocuments: I := CSIDL_COMMON_DOCUMENTS;
        brCommonFavorites: I := CSIDL_COMMON_FAVORITES;
        brCommonPrograms: I := CSIDL_COMMON_PROGRAMS;
        brCommonStartMenu: I := CSIDL_COMMON_STARTMENU;
        brCommonStartUp: I := CSIDL_COMMON_STARTUP;
        brControls: I := CSIDL_CONTROLS;
        brCookies: I := CSIDL_COOKIES;
        brDesktop: I := CSIDL_DESKTOP;
        brDesktopDirectory: I := CSIDL_DESKTOPDIRECTORY;
        brDrives: I := CSIDL_DRIVES;
        brFavorites: I := CSIDL_FAVORITES;
        brFonts: I := CSIDL_FONTS;
        brHistory: I := CSIDL_HISTORY;
        brInternet: I := CSIDL_INTERNET;
        brInternetCache: I := CSIDL_INTERNET_CACHE;
        brLocalAppData: I := CSIDL_LOCAL_APPDATA;
        brMyMusic: I := CSIDL_MYMUSIC;
        brMyPictures: I := CSIDL_MYPICTURES;
        brNetHood: I := CSIDL_NETHOOD;
        brNetwork: I := CSIDL_NETWORK;
        brPersonal: I := CSIDL_PERSONAL;
        brPrinters: I := CSIDL_PRINTERS;
        brPrintHood: I := CSIDL_PRINTHOOD;
        brProgramFiles: I := CSIDL_PROGRAM_FILES;
        brProgramFilesCommon: I := CSIDL_PROGRAM_FILES_COMMON;
        brPrograms: I := CSIDL_PROGRAMS;
        brRecent: I := CSIDL_RECENT;
        brSendTo: I := CSIDL_SENDTO;
        brStartMenu: I := CSIDL_STARTMENU;
        brStartUp: I := CSIDL_STARTUP;
        brSystem: I := CSIDL_SYSTEM;
        brTemplates: I := CSIDL_TEMPLATES;
        brWindows: I := CSIDL_WINDOWS;
      else
        I := CSIDL_DESKTOP;
      end;
      if FRootFolder <> brCustom then
        SHGetSpecialFolderLocation(Application.MainForm.Handle, I, Root)
      else
      begin
        SHGetDesktopFolder(DesktopFolder);
        try
          Eaten := 0; Flags := 0;
          DesktopFolder.ParseDisplayName(Application.MainForm.Handle, nil,
            PWideChar(WideString(FCustomRootFolder)), Eaten, Root, Flags);
        except
          Root := nil;
        end;
      end;
      BI.pidlRoot := Root;
      BI.pszDisplayName := Buf;
      BI.lpszTitle := PChar(FLabelText);
      BI.ulFlags := 0;
      if bfBrowseForComputer in FOptions then BI.ulFlags := BIF_BROWSEFORCOMPUTER;
      if bfBrowseForPrinter in FOptions then BI.ulFlags := BI.ulFlags or BIF_BROWSEFORPRINTER;
      if bfBrowseIncludeFiles in FOptions then BI.ulFlags := BI.ulFlags or BIF_BROWSEINCLUDEFILES;
      if bfBrowseIncludeURLs in FOptions then BI.ulFlags := BI.ulFlags or BIF_BROWSEINCLUDEURLS or BIF_USENEWUI or BIF_BROWSEINCLUDEFILES;
      if bfEditBox in FOptions then BI.ulFlags := BI.ulFlags or BIF_EDITBOX;
      if bfNewDialogStyle in FOptions then BI.ulFlags := BI.ulFlags or BIF_NEWDIALOGSTYLE;
      if bfReturnFSAncestors in FOptions then BI.ulFlags := BI.ulFlags or BIF_RETURNFSANCESTORS;
      if bfReturnOnlyFSDirs in FOptions then BI.ulFlags := BIF_RETURNONLYFSDIRS;
      if bfShareAble in FOptions then BI.ulFlags := BIF_SHAREABLE or BIF_USENEWUI;
      if bfStatusText in FOptions then BI.ulFlags := BI.ulFlags or BIF_STATUSTEXT;
      if bfUseNewUI in FOptions then BI.ulFlags := BI.ulFlags or BIF_USENEWUI;
      if bfValidate in FOptions then BI.ulFlags := BI.ulFlags or BIF_VALIDATE;
      BI.lpfn := BFCallBack;
      BI.lParam := Integer(Self);
      List := SHBrowseForFolder({$IFDEF FPC}@BI{$ELSE}BI{$ENDIF});
      if List <> nil then
      begin
        SHGetPathFromIDList(List, Buf);
        Allocator.Free(List);
        FFolder := Buf;
        Result := True;
      end;
    finally
    {$IFDEF FPC}
      Screen.EnableForms(DisabledList);
    {$ELSE}
      EnableTaskWindows(P);
    {$ENDIF}
      Allocator.Free(Root);
      FreeMem(Buf);
    end;
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

procedure TKBrowseFolderDialog.SetFolder(const Value: TFolder);
begin
  FFolder := Value;
end;

end.
