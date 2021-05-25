{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kres; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

{ Because the resourcestring concept used in Delphi is not always the best way to localize an application,
  I decided to implement routines which allow to modify the resourcestrings dynamically at runtime.
  They allow for direct localization e.g. from XML file without the need of the standard localization scheme
  for resourcestrings. Especially if you need to translate only some of the strings it is much easier approach. }

{ Standard resourcestrings localized to english by default }
resourcestring
  // KGraphics texts
  sGrAlphaBitmap = 'KControls alpha bitmap';

  // KDialogs texts
  sBrowseDirectory = 'Choose directory:';

  // KEdits texts
  sEDBadSubDirName = 'The invalid subdirectory "%s" has been replaced with "%s".';
  sEDCurrentDirAdded = 'Current path added to path "%s".';
  sEDBadDir = 'The directory "%s" can be invalid.';
  sEDBadDirCorr = 'Invalid or incomplete directory "%s" has been replaced with "%s".';
  sEDBadPath = 'The path or file name "%s" can be incomplete or invalid.';
  sEDBadPathCorr = 'Invalid path or file name "%s" has been replaced with "%s".';
  sEDMissingFileName = 'Missing file name.';
  sEDNoExistingDir = 'The directory "%s" doesn''t exist.';
  sEDNoExistingPath = 'The file "%s" doesn''t exist.';
  sEDFormatNotAccepted = 'The text either doesn''t represent a numeral or the numeric format cannot be accepted.';
  sEDBadFloatValueAsStr = 'The text is not a float value in the range from %s to %s. Value corrected to %s.';
  sEDBadIntValueAsStr = 'The text is not a decimal value in the range from %s to %s. Value corrected to %s.';
  sEDBadHexValueAsStr = 'The text is not a hexadecimal value in the range from %s to %s. Value corrected to %s.';
  sEDClipboardFmtNotAccepted = 'The current clipboard text cannot be accepted.';
  sEDBrowse = 'Browse ...';
  sEDAllFiles = 'All files (*.*)|*.*';

  // KGraphics texts
  sGDIError = 'GDI object could not be created.';
  sErrGraphicsLoadFromResource = 'Graphics could not be loaded from resource.';

  // KHexEditor texts
  sHEAddressText = 'Address area text';
  sHEAddressBkGnd = 'Address area background';
  sHEBkGnd = 'Editor background';
  sHEDigitTextEven = 'Digit area even column';
  sHEDigitTextOdd = 'Digit area odd column';
  sHEDigitBkgnd = 'Digit area background';
  sHEHorzLines = 'Horizontal lines';
  sHEInactiveCaretBkGnd = 'Inactive caret background';
  sHEInactiveCaretSelBkGnd = 'Selected inactive caret background';
  sHEInactiveCaretSelText = 'Selected inactive caret text';
  sHEInactiveCaretText = 'Inactive caret text';
  sHELinesHighLight = 'Lines highlight';
  sHESelBkGnd = 'Selection background';
  sHESelBkGndFocused = 'Focused selection background';
  sHESelText = 'Selection text';
  sHESelTextFocused = 'Focused selection text';
  sHESeparators = 'Area separating lines';
  sHETextText = 'Text area text';
  sHETextBkGnd = 'Text area background';
  sHEVertLines = 'Vertical lines';

  // KIcons texts
  sIconIcons = 'Icons';
  sIconCursors = 'Cursors';
  sIconAllocationError = 'Error while allocating icon data';
  sIconBitmapError = 'Invalid icon bitmap handles';
  sIconFormatError = 'Invalid icon format';
  sIconResourceError = 'Invalid icon resource';
  sIconIndexError = 'Invalid icon resource index';
  sIconInvalidModule = 'Invalid module or no icon resources';
  sIconResizingError = 'Error while resizing icon';
  sIconAssocResolveError = 'Error while resolving associated icon';

  // KLog texts
  sLogError = 'Error';
  sLogWarning = 'Warning';
  sLogNote = 'Note';
  sLogHint = 'Hint';
  sLogInfo = 'Info';
  sLogInputError = 'Input error';
  sLogIOError = 'IO error';

  // KMessagebox texts
  sMsgBoxYes = '&Yes';
  sMsgBoxNo = '&No';
  sMsgBoxOK = '&OK';
  sMsgBoxCancel = 'Cancel';
  sMsgBoxClose = '&Close';
  sMsgBoxAbort = 'A&bort';
  sMsgBoxRetry = '&Retry';
  sMsgBoxIgnore = '&Ignore';
  sMsgBoxAll = '&All';
  sMsgBoxNoToAll = 'Non&e';
  sMsgBoxYesToAll = 'Ye&s to all';
  sMsgBoxHelp = '&Help';

  // KPrinterSetup texts
  sPSPrinterSetup = 'Printer setup';
  sPSAllPages = 'All pages (%d)';
  sPSErrPrintSetup = 'Print setup error';
  sPSErrNoPrinterInstalled = 'No printer is installed on this computer.';
  sPSErrNoDefaultPrinter = 'No default printer selected, cannot continue. Please select default printer.';
  sPSErrPrinterUnknown = 'Unknown error in printer interface. Please restart application and try again.';
  sPSErrPrinterConfiguration = 'Printer configuration not supported.';

  // KControlsDesign texts
  sInvalidGraphicFormat = 'Invalid graphic format.';

  // KDBGrids texts
  sDataSetUnidirectional = 'Cannot use KDBGrid with a unidirectional dataset.';

  // KMemoRTF texts
  sErrMemoLoadFromRTF = 'Error while reading RTF file.';
  sErrMemoLoadImageFromRTF = 'Error while loading image from RTF file.';
  sErrMemoSaveToRTF = 'Error while saving RTF file.';

  // KMemoFrame texts
  sAppError = 'Application error';
  sAppQuery = 'Application query';
  sMemoDefaultFileName = 'document';
  sQueryFileSave = 'File "%s" has been changed. Do you want to save it?';
  sErrMemoLoadFromFile = 'Error while loading file "%s".';
  sErrMemoSaveToFile = 'Error while saving file "%s".';
  sMemoSampleTextBox = 'Enter the text box contents. The textbox can be placed anywhere in the document.';


{ Localize given resourcestring directly.
  Usage: ResMod(@sYourResourceString, 'New text');
  Note: Text passed to NewValue must persist through the entire
  application lifetime under Delphi, as only its pointer is taken! }
procedure ResMod(Res: PResStringRec; const NewValue: string);

{ Localize all resourcestrings to Czech language. }
procedure LocalizeToCzech;

implementation

{$IFnDEF FPC}
uses
  Windows, KFunctions;
{$ENDIF}

{$IFDEF FPC}
type
  PResModRec = ^TResModRec;
  TResModRec = record
    DefStr: string;
    NewStr: string;
  end;

function ResModIterator(Name, Value: AnsiString; Hash: Longint; arg:pointer): AnsiString;
begin
  if Value = PResModRec(arg).DefStr then
    Result := PResModRec(arg).NewStr
  else
    Result := '';
end;

procedure ResMod(Res: PResStringRec; const NewValue: string);
var
  RM: TResModRec;
begin
  if (Res <> nil) and (Res^ <> '') then
  begin
    RM.DefStr := Res^;
    RM.NewStr := NewValue;
    SetResourceStrings(ResModIterator, @RM);
  end;
end;
{$ELSE}
procedure ResMod(Res: PResStringRec; const NewValue: string);
var
  OldProtect: LongWord;
  OK: Boolean;
begin
  if (Res <> nil) and (Res.Module <> nil) then
  begin
    OK := VirtualProtect(Res, Sizeof(TResStringRec), PAGE_EXECUTE_READWRITE, @oldProtect);
    if OK then
    begin
    {$IFDEF COMPILER16_UP} // new code for Delphi XE2 and later
      Res.Identifier := NativeUInt(NewValue);
    {$ELSE}
      Res.Identifier := LongInt(NewValue);
    {$ENDIF}
      VirtualProtect(Res, SizeOf(TResStringRec), oldProtect, @oldProtect);
    end;
  end;
end;
{$ENDIF}

procedure LocalizeToCzech;
begin
  // KGraphics texts
  ResMod(@sGrAlphaBitmap, 'Alpha bitmap KControls');

  // KDialogs texts
  ResMod(@sBrowseDirectory, 'Vyberte složku:');

  // KEdits texts
  ResMod(@sEDBadSubDirName, 'Neplatná podsložka "%s" byla nahrazena "%s".');
  ResMod(@sEDCurrentDirAdded, 'Aktuální cesta byla pøidána k cestì "%s".');
  ResMod(@sEDBadDir, 'Složka "%s" mùže být neplatná.');
  ResMod(@sEDBadDirCorr, 'Neplatná nebo nekompletní složka "%s" byla nahrazena "%s".');
  ResMod(@sEDBadPath, 'Cesta nebo soubor "%s" nemusí být kompletní nebo platná.');
  ResMod(@sEDBadPathCorr, 'Neplatná cesta nebo soubor "%s" byl(a) nahrazen(a) "%s".');
  ResMod(@sEDMissingFileName, 'Chybí název souboru.');
  ResMod(@sEDNoExistingDir, 'Složka "%s" neexistuje.');
  ResMod(@sEDNoExistingPath, 'Soubor "%s" neexistuje.');
  ResMod(@sEDFormatNotAccepted, 'Text není èíslem nebo èíselný formát nelze pøijmout.');
  ResMod(@sEDBadFloatValueAsStr, 'Text není reálným èíslem v rozsahu od %s do %s. Hodnota opravena na %s.');
  ResMod(@sEDBadIntValueAsStr, 'Text není celým èíslem v rozsahu od %s do %s. Hodnota opravena na %s.');
  ResMod(@sEDBadHexValueAsStr, 'Text není hexadecimálním èíslem od %s do %s. Hodnota opravena na %s.');
  ResMod(@sEDClipboardFmtNotAccepted, 'Text ze schránky nelze pøijmout.');
  ResMod(@sEDBrowse, 'Procházet...');
  ResMod(@sEDAllFiles, 'Všechny soubory (*.*)|*.*');

  // KGraphics texts
  ResMod(@sGDIError, 'Objekt GDI nelze vytvoøit.');

  // KHexEditor texts
  ResMod(@sHEAddressText, 'Pás adresy - text');
  ResMod(@sHEAddressBkGnd, 'Pás adresy - pozadí');
  ResMod(@sHEBkGnd, 'Pozadí editoru');
  ResMod(@sHEDigitTextEven, 'Pás èíslic sudý sloupec - text');
  ResMod(@sHEDigitTextOdd, 'Pás èíslic lichý sloupec - text');
  ResMod(@sHEDigitBkgnd, 'Pás èíslic - pozadí');
  ResMod(@sHEHorzLines, 'Vodorovné linky');
  ResMod(@sHEInactiveCaretBkGnd, 'Neaktivní kurzor - pozadí');
  ResMod(@sHEInactiveCaretSelBkGnd, 'Neaktivní kurzor pozadí výbìru');
  ResMod(@sHEInactiveCaretSelText, 'Neaktivní kurzor text výbìru');
  ResMod(@sHEInactiveCaretText, 'Neaktivní kurzor - text');
  ResMod(@sHELinesHighLight, 'Zvýraznìní øádkù');
  ResMod(@sHESelBkGnd, 'Pozadí výbìru');
  ResMod(@sHESelBkGndFocused, 'Pozadí aktivního výbìru');
  ResMod(@sHESelText, 'Text výbìru');
  ResMod(@sHESelTextFocused, 'Text aktivního výbìru');
  ResMod(@sHESeparators, 'Oddìlovací linky pásù');
  ResMod(@sHETextText, 'Pás textu - text');
  ResMod(@sHETextBkGnd, 'Pás textu - pozadí');
  ResMod(@sHEVertLines, 'Svislé linky');

  // KIcons texts
  ResMod(@sIconIcons, 'Ikony');
  ResMod(@sIconCursors, 'Kurzory');
  ResMod(@sIconAllocationError, 'Chyba pøi alokování dat ikony');
  ResMod(@sIconBitmapError, 'Neplatné popisovaèe bitmap ikony');
  ResMod(@sIconFormatError, 'Neplatný formát ikony');
  ResMod(@sIconResourceError, 'Neplatný zdroj ikony');
  ResMod(@sIconIndexError, 'Neplatný index zdroje ikony');
  ResMod(@sIconInvalidModule, 'Neplatný modul nebo chybí zdroje ikon');
  ResMod(@sIconResizingError, 'Chyba pøi zmìnì velikosti ikony');
  ResMod(@sIconAssocResolveError, 'Chyba pøi nahrávání asociované ikony');

  // KLog texts
  ResMod(@sLogError, 'Chyba');
  ResMod(@sLogWarning, 'Varování');
  ResMod(@sLogNote, 'Poznámka');
  ResMod(@sLogHint, 'Nápovìda');
  ResMod(@sLogInfo, 'Informace');
  ResMod(@sLogInputError, 'Chyba zadání');
  ResMod(@sLogIOError, 'Chyba IO operace');

  // KMessagebox texts
  ResMod(@sMsgBoxYes, '&Ano');
  ResMod(@sMsgBoxNo, '&Ne');
  ResMod(@sMsgBoxOK, '&OK');
  ResMod(@sMsgBoxCancel, 'Storno');
  ResMod(@sMsgBoxClose, 'Za&vøít');
  ResMod(@sMsgBoxAbort, '&Pøerušit');
  ResMod(@sMsgBoxRetry, '&Znovu');
  ResMod(@sMsgBoxIgnore, '&Ignorovat');
  ResMod(@sMsgBoxAll, 'Vš&e');
  ResMod(@sMsgBoxNoToAll, 'Ni&c');
  ResMod(@sMsgBoxYesToAll, 'Ano pro vše');
  ResMod(@sMsgBoxHelp, 'Nápovìda');

  // KPrinterSetup texts
  ResMod(@sPSPrinterSetup, 'Nastevení tiskárny');
  ResMod(@sPSAllPages, 'Všechny stránky (%d)');
  ResMod(@sPSErrPrintSetup, 'Chybné nastavení tisku');
  ResMod(@sPSErrNoPrinterInstalled, 'Na poèítaèi není instalována žádná tiskárna.');
  ResMod(@sPSErrNoDefaultPrinter, 'Není zvolena výchozí tiskárna, nelze pokraèovat. Zvolte výchozí tiskárnu.');
  ResMod(@sPSErrPrinterUnknown, 'Neznámá chyba v tiskovém rozhraní. Prosím restartujte aplikaci a zkuste to znovu.');
  ResMod(@sPSErrPrinterConfiguration, 'Konfigurace tiskárny není podporována.');

  // KControlsDesign texts
  ResMod(@sInvalidGraphicFormat, 'Neplatný grafický formát.');

  // KDBGrids texts
  ResMod(@sDataSetUnidirectional, 'Nelze použít KDBGrid s jednosmìrným datasetem.');

  // KMemoRTF texts
  ResMod(@sErrMemoLoadFromRTF, 'Chyba pøi ètení souboru RTF.');
  ResMod(@sErrMemoLoadImageFromRTF, 'Chyba pøi ètení obrázku z RTF souboru.');
  ResMod(@sErrMemoSaveToRTF, 'Chyba pøi zápisu souboru RTF.');

  // KMemoFrame texts
  ResMod(@sAppError, 'Chyba aplikace');
  ResMod(@sAppQuery, 'Dotaz aplikace');
  ResMod(@sMemoDefaultFileName, 'dokument');
  ResMod(@sQueryFileSave, 'Soubor "%s" byl zmìnìn. Pøejete si jej uložit?');
  ResMod(@sErrMemoLoadFromFile, 'Chyba pøi ètení souboru "%s".');
  ResMod(@sErrMemoSaveToFile, 'Chyba pøi zápisu souboru "%s".');
  ResMod(@sMemoSampleTextBox, 'Vyplòte obsah textového pole. Pole lze umístit kdekoli v dokumentu.');

end;

end.
