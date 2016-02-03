unit BCCommon.Language.Strings;

interface

uses
  System.SysUtils, System.Classes, BCComponent.MultiStringHolder;

type
  TLanguageDataModule = class(TDataModule)
    MultiStringHolderConstant: TBCMultiStringHolder;
    MultiStringHolderConvertConstant: TBCMultiStringHolder;
    MultiStringHolderErrorMessage: TBCMultiStringHolder;
    MultiStringHolderMessage: TBCMultiStringHolder;
    MultiStringHolderSQLFormatter: TBCMultiStringHolder;
    MultiStringHolderWarningMessage: TBCMultiStringHolder;
    MultiStringHolderYesOrNo: TBCMultiStringHolder;
    MultiStringHolderFileTypes: TBCMultiStringHolder;
    MultiStringHolderColorConstant: TBCMultiStringHolder;
    MultiStringHolderDialogConstant: TBCMultiStringHolder;
  private
    FColorComboBoxStrings: TStrings;
    FHookedConstantsList: TList;
  protected
    procedure FreeHookedConstraintsList;
  public
    function GetConstant(Name: string): string;
    function GetConvertConstant(Name: string): string;
    function GetErrorMessage(Name: string): string;
    function GetFileTypes(Name: string): string;
    function GetMessage(Name: string): string;
    function GetPAskYesOrNo(Name: string): PWideChar;
    function GetPColor(AIndex: Integer): PWideChar;
    function GetPConstant(Name: string): PWideChar;
    function GetPDialog(AIndex: Integer): PWideChar;
    function GetSQLFormatter(Name: string): string;
    function GetWarningMessage(Name: string): string;
    function GetYesOrNoMessage(Name: string): string;
    //property ColorComboBoxStrings: TStrings read FColorComboBoxStrings;
  end;

procedure ReadLanguageFile(Language: string);

var
  LanguageDataModule: TLanguageDataModule;

implementation

{$R *.dfm}

uses
  Winapi.Windows, Vcl.Consts, System.IniFiles, BCControl.Language, BCEditor.Language;

procedure HookResourceString(aResStringRec: PResStringRec; aNewStr: PChar);
var
  OldProtect: DWORD;
begin
  VirtualProtect(aResStringRec, SizeOf(aResStringRec^), PAGE_EXECUTE_READWRITE, @OldProtect);
  aResStringRec^.Identifier := Integer(aNewStr);
  VirtualProtect(aResStringRec, SizeOf(aResStringRec^), OldProtect, @OldProtect);
end;

procedure ReadLanguageFile(Language: string);
var
  LanguagePath: string;
  BigIniFile: TMemIniFile;

  procedure SetStringHolder(MultiStringHolder: TBCMultiStringHolder; Section: string);
  var
    i: Integer;
    StringName: string;
    s: string;
  begin
    for i := 0 to MultiStringHolder.MultipleStrings.Count - 1 do
    begin
      StringName := MultiStringHolder.MultipleStrings.Items[i].Name;
      s := BigIniFile.ReadString(Section, StringName, '');
      if s <> '' then
        MultiStringHolder.MultipleStrings.Items[i].Strings.Text := s;
    end;
  end;
begin
  if Language = '' then
    Exit;

  LanguagePath := IncludeTrailingPathDelimiter(Format('%s%s', [ExtractFilePath(ParamStr(0)), 'Languages']));
  if not DirectoryExists(LanguagePath) then
    Exit;

  BigIniFile := TMemIniFile.Create(Format('%s%s.%s', [LanguagePath, Language, 'lng']), TEncoding.Unicode);
  try
    SetStringHolder(LanguageDataModule.MultiStringHolderYesOrNo, 'AskYesOrNo');
    SetStringHolder(LanguageDataModule.MultiStringHolderMessage, 'Message');
    SetStringHolder(LanguageDataModule.MultiStringHolderErrorMessage, 'ErrorMessage');
    SetStringHolder(LanguageDataModule.MultiStringHolderWarningMessage, 'WarningMessage');
    SetStringHolder(LanguageDataModule.MultiStringHolderConstant, 'Constant');
    SetStringHolder(LanguageDataModule.MultiStringHolderFileTypes, 'FileTypes');
    SetStringHolder(LanguageDataModule.MultiStringHolderConvertConstant, 'ConvertConstant');
    SetStringHolder(LanguageDataModule.MultiStringHolderSQLFormatter, 'SQLFormatterConstant');
    SetStringHolder(LanguageDataModule.MultiStringHolderColorConstant, 'ColorConstant');
    SetStringHolder(LanguageDataModule.MultiStringHolderDialogConstant, 'DialogConstant');
    //BigIniFile.ReadSectionValues('ColorComboBox', LanguageDataModule.FColorComboBoxStrings);
  finally
    BigIniFile.Free;
  end;

  { colors }
  HookResourceString(@SNameBlack, LanguageDataModule.GetPColor(0));
  HookResourceString(@SNameMaroon, LanguageDataModule.GetPColor(1));
  HookResourceString(@SNameGreen, LanguageDataModule.GetPColor(2));
  HookResourceString(@SNameOlive, LanguageDataModule.GetPColor(3));
  HookResourceString(@SNameNavy, LanguageDataModule.GetPColor(4));
  HookResourceString(@SNamePurple, LanguageDataModule.GetPColor(5));
  HookResourceString(@SNameTeal, LanguageDataModule.GetPColor(6));
  HookResourceString(@SNameGray, LanguageDataModule.GetPColor(7));
  HookResourceString(@SNameSilver, LanguageDataModule.GetPColor(8));
  HookResourceString(@SNameRed, LanguageDataModule.GetPColor(9));
  HookResourceString(@SNameLime, LanguageDataModule.GetPColor(10));
  HookResourceString(@SNameYellow, LanguageDataModule.GetPColor(11));
  HookResourceString(@SNameBlue, LanguageDataModule.GetPColor(12));
  HookResourceString(@SNameFuchsia, LanguageDataModule.GetPColor(13));
  HookResourceString(@SNameAqua, LanguageDataModule.GetPColor(14));
  HookResourceString(@SNameWhite, LanguageDataModule.GetPColor(15));
  HookResourceString(@SNameMoneyGreen, LanguageDataModule.GetPColor(16));
  HookResourceString(@SNameSkyBlue, LanguageDataModule.GetPColor(17));
  HookResourceString(@SNameCream, LanguageDataModule.GetPColor(18));
  HookResourceString(@SNameMedGray, LanguageDataModule.GetPColor(19));
  HookResourceString(@SNameActiveBorder, LanguageDataModule.GetPColor(20));
  HookResourceString(@SNameActiveCaption, LanguageDataModule.GetPColor(21));
  HookResourceString(@SNameAppWorkSpace, LanguageDataModule.GetPColor(22));
  HookResourceString(@SNameBackground, LanguageDataModule.GetPColor(23));
  HookResourceString(@SNameBtnFace, LanguageDataModule.GetPColor(24));
  HookResourceString(@SNameBtnHighlight, LanguageDataModule.GetPColor(25));
  HookResourceString(@SNameBtnShadow, LanguageDataModule.GetPColor(26));
  HookResourceString(@SNameBtnText, LanguageDataModule.GetPColor(27));
  HookResourceString(@SNameCaptionText, LanguageDataModule.GetPColor(28));
  HookResourceString(@SNameDefault, LanguageDataModule.GetPColor(29));
  HookResourceString(@SNameGradientActiveCaption, LanguageDataModule.GetPColor(30));
  HookResourceString(@SNameGradientInactiveCaption, LanguageDataModule.GetPColor(31));
  HookResourceString(@SNameGrayText, LanguageDataModule.GetPColor(32));
  HookResourceString(@SNameHighlight, LanguageDataModule.GetPColor(33));
  HookResourceString(@SNameHighlightText, LanguageDataModule.GetPColor(34));
  HookResourceString(@SNameHotLight, LanguageDataModule.GetPColor(35));
  HookResourceString(@SNameInactiveBorder, LanguageDataModule.GetPColor(36));
  HookResourceString(@SNameInactiveCaption, LanguageDataModule.GetPColor(37));
  HookResourceString(@SNameInactiveCaptionText, LanguageDataModule.GetPColor(38));
  HookResourceString(@SNameInfoBk, LanguageDataModule.GetPColor(39));
  HookResourceString(@SNameInfoText, LanguageDataModule.GetPColor(40));
  HookResourceString(@SNameMenu, LanguageDataModule.GetPColor(41));
  HookResourceString(@SNameMenuBar, LanguageDataModule.GetPColor(42));
  HookResourceString(@SNameMenuHighlight, LanguageDataModule.GetPColor(43));
  HookResourceString(@SNameMenuText, LanguageDataModule.GetPColor(44));
  HookResourceString(@SNameNone, LanguageDataModule.GetPColor(45));
  HookResourceString(@SNameScrollBar, LanguageDataModule.GetPColor(46));
  HookResourceString(@SName3DDkShadow, LanguageDataModule.GetPColor(47));
  HookResourceString(@SName3DLight, LanguageDataModule.GetPColor(48));
  HookResourceString(@SNameWindow, LanguageDataModule.GetPColor(49));
  HookResourceString(@SNameWindowFrame, LanguageDataModule.GetPColor(50));
  HookResourceString(@SNameWindowText, LanguageDataModule.GetPColor(51));
  HookResourceString(@SColorBoxCustomCaption, LanguageDataModule.GetPColor(52));
  { message dialog captions and buttons }
  HookResourceString(@SMsgDlgWarning, LanguageDataModule.GetPDialog(0));
  HookResourceString(@SMsgDlgError, LanguageDataModule.GetPDialog(1));
  HookResourceString(@SMsgDlgInformation, LanguageDataModule.GetPDialog(2));
  HookResourceString(@SMsgDlgConfirm, LanguageDataModule.GetPDialog(3));
  HookResourceString(@SMsgDlgYes, LanguageDataModule.GetPDialog(4));
  HookResourceString(@SMsgDlgNo, LanguageDataModule.GetPDialog(5));
  HookResourceString(@SMsgDlgOK, LanguageDataModule.GetPDialog(6));
  HookResourceString(@SMsgDlgCancel, LanguageDataModule.GetPDialog(7));
  HookResourceString(@SMsgDlgHelp, LanguageDataModule.GetPDialog(8));
  HookResourceString(@SMsgDlgHelpNone, LanguageDataModule.GetPDialog(9));
  HookResourceString(@SMsgDlgHelpHelp, LanguageDataModule.GetPDialog(10));
  HookResourceString(@SMsgDlgAbort, LanguageDataModule.GetPDialog(11));
  HookResourceString(@SMsgDlgRetry, LanguageDataModule.GetPDialog(12));
  HookResourceString(@SMsgDlgIgnore, LanguageDataModule.GetPDialog(13));
  HookResourceString(@SMsgDlgAll, LanguageDataModule.GetPDialog(14));
  HookResourceString(@SMsgDlgNoToAll, LanguageDataModule.GetPDialog(15));
  HookResourceString(@SMsgDlgYesToAll, LanguageDataModule.GetPDialog(16));
  HookResourceString(@SMsgDlgClose, LanguageDataModule.GetPDialog(17));

  HookResourceString(@SBCControlFileControlEndEditInvalidName, LanguageDataModule.GetPConstant('InvalidName'));
  HookResourceString(@SBCControlFileControlEndEditRename, LanguageDataModule.GetPConstant('Rename'));
  HookResourceString(@SBCControlFileControlEndEditRenameFailed, LanguageDataModule.GetPConstant('RenameFailed'));
  HookResourceString(@SBCEditorScrollInfoTopLine, LanguageDataModule.GetPConstant('TopLine'));
  HookResourceString(@SBCEditorSearchStringNotFound, LanguageDataModule.GetPAskYesOrNo('SearchStringNotFound'));
  HookResourceString(@SBCEditorSearchMatchNotFound, LanguageDataModule.GetPAskYesOrNo('SearchMatchNotFound'));
  HookResourceString(@SBCEditorRightMarginPosition, LanguageDataModule.GetPConstant('Position'));

end;

function TLanguageDataModule.GetYesOrNoMessage(Name: string): string;
begin
  Result := Trim(MultiStringHolderYesOrNo.StringsByName[Name].Text);
end;

function TLanguageDataModule.GetMessage(Name: string): string;
begin
  Result := Trim(MultiStringHolderMessage.StringsByName[Name].Text);
end;

function TLanguageDataModule.GetErrorMessage(Name: string): string;
begin
  Result := Trim(MultiStringHolderErrorMessage.StringsByName[Name].Text);
end;

function TLanguageDataModule.GetWarningMessage(Name: string): string;
begin
  Result := Trim(MultiStringHolderWarningMessage.StringsByName[Name].Text);
end;

function TLanguageDataModule.GetFileTypes(Name: string): string;
begin
  Result := Trim(MultiStringHolderFileTypes.StringsByName[Name].Text);
end;

function TLanguageDataModule.GetConstant(Name: string): string;
begin
  Result := Trim(MultiStringHolderConstant.StringsByName[Name].Text);
end;

function TLanguageDataModule.GetConvertConstant(Name: string): string;
begin
  Result := Trim(MultiStringHolderConvertConstant.StringsByName[Name].Text);
end;

function TLanguageDataModule.GetSQLFormatter(Name: string): string;
begin
  Result := Trim(MultiStringHolderSQLFormatter.StringsByName[Name].Text);
end;

function TLanguageDataModule.GetPAskYesOrNo(Name: string): PWideChar;
var
  LConstant: string;
begin
  LConstant := GetYesOrNoMessage(Name);
  GetMem(Result, SizeOf(WideChar) * (Length(LConstant) + 1));
  StringToWideChar(LConstant, Result, Length(LConstant) + 1);
  FHookedConstantsList.Add(Result);
end;

function TLanguageDataModule.GetPColor(AIndex: Integer): PWideChar;
var
  LColor: string;
begin
  LColor := Trim(MultiStringHolderColorConstant.MultipleStrings.Items[AIndex].Strings.Text);
  GetMem(Result, SizeOf(WideChar) * (Length(LColor) + 1));
  StringToWideChar(LColor, Result, Length(LColor) + 1);
  FHookedConstantsList.Add(Result);
end;

function TLanguageDataModule.GetPConstant(Name: string): PWideChar;
var
  LConstant: string;
begin
  LConstant := GetConstant(Name);
  GetMem(Result, SizeOf(WideChar) * (Length(LConstant)+ 1));
  StringToWideChar(LConstant, Result, Length(LConstant) + 1);
  FHookedConstantsList.Add(Result);
end;

function TLanguageDataModule.GetPDialog(AIndex: Integer): PWideChar;
var
  LText: string;
begin
  LText := Trim(MultiStringHolderDialogConstant.MultipleStrings.Items[AIndex].Strings.Text);
  GetMem(Result, SizeOf(WideChar) * (Length(LText) + 1));
  StringToWideChar(LText, Result, Length(LText) + 1);
  FHookedConstantsList.Add(Result);
end;

procedure TLanguageDataModule.FreeHookedConstraintsList;
var
  i: Integer;
begin
  for i := 0 to FHookedConstantsList.Count - 1 do
    Dispose(PWideChar(FHookedConstantsList.Items[i]));
  FHookedConstantsList.Free;
end;

initialization

  LanguageDataModule := TLanguageDataModule.Create(nil);
  LanguageDataModule.FHookedConstantsList := TList.Create;
  LanguageDataModule.FColorComboBoxStrings := TStringList.Create;

finalization

  LanguageDataModule.FreeHookedConstraintsList;
  LanguageDataModule.FColorComboBoxStrings.Free;
  LanguageDataModule.Free;

end.
