unit BCEditor.Editor.Utils;

interface

uses
  System.Classes, System.SysUtils, BCEditor.Types;

function GetTextPosition(const AChar, ALine: Integer): TBCEditorTextPosition;
function IsUTF8Buffer(const ABuffer: TBytes; out AWithBOM: Boolean): Boolean;
function GetClipboardText: string;
procedure SetClipboardText(const AText: string);

implementation

uses
  System.Math, BCEditor.Consts, Winapi.Windows, vcl.ClipBrd;

function GetTextPosition(const AChar, ALine: Integer): TBCEditorTextPosition;
begin
  with Result do
  begin
    Char := AChar;
    Line := ALine;
  end;
end;

// checks for a BOM in UTF-8 format or searches the Buffer for typical UTF-8 octet sequences
function IsUTF8Buffer(const ABuffer: TBytes; out AWithBOM: Boolean): Boolean;
const
  MinimumCountOfUTF8Strings = 1;
var
  i, LBufferSize, LFoundUTF8Strings: Integer;

  { 3 trailing bytes are the maximum in valid UTF-8 streams,
    so a count of 4 trailing bytes is enough to detect invalid UTF-8 streams }
  function CountOfTrailingBytes: Integer;
  begin
    Result := 0;
    Inc(i);
    while (i < LBufferSize) and (Result < 4) do
    begin
      if ABuffer[i] in [$80 .. $BF] then
        Inc(Result)
      else
        Break;
      Inc(i);
    end;
  end;

begin
  { if Stream is nil, let Delphi raise the exception, by accessing Stream,
    to signal an invalid result }

  // start analysis at actual Stream.Position
  LBufferSize := Length(ABuffer);

  // if no special characteristics are found it is not UTF-8
  Result := False;
  AWithBOM := False;

  if LBufferSize > 0 then
  begin
    { first search for BOM }
    if (LBufferSize >= Length(BCEDITOR_UTF8BOM)) and CompareMem(@ABuffer[0], @BCEDITOR_UTF8BOM[0], Length(BCEDITOR_UTF8BOM)) then
    begin
      AWithBOM := True;
      Result := True;
      Exit;
    end;

    { If no BOM was found, check for leading/trailing byte sequences,
      which are uncommon in usual non UTF-8 encoded text.

      NOTE: There is no 100% save way to detect UTF-8 streams. The bigger
      MinimumCountOfUTF8Strings, the lower is the probability of
      a false positive. On the other hand, a big MinimumCountOfUTF8Strings
      makes it unlikely to detect files with only little usage of non
      US-ASCII chars, like usual in European languages. }
    LFoundUTF8Strings := 0;
    i := 0;
    while i < LBufferSize do
    begin
      case ABuffer[i] of
        $00 .. $7F: // skip US-ASCII characters as they could belong to various charsets
          ;
        $C2 .. $DF:
          if CountOfTrailingBytes = 1 then
            Inc(LFoundUTF8Strings)
          else
            Break;
        $E0:
          begin
            Inc(i);
            if (i < LBufferSize) and (ABuffer[i] in [$A0 .. $BF]) and (CountOfTrailingBytes = 1) then
              Inc(LFoundUTF8Strings)
            else
              Break;
          end;
        $E1 .. $EC, $EE .. $EF:
          if CountOfTrailingBytes = 2 then
            Inc(LFoundUTF8Strings)
          else
            Break;
        $ED:
          begin
            Inc(i);
            if (i < LBufferSize) and (ABuffer[i] in [$80 .. $9F]) and (CountOfTrailingBytes = 1) then
              Inc(LFoundUTF8Strings)
            else
              Break;
          end;
        $F0:
          begin
            Inc(i);
            if (i < LBufferSize) and (ABuffer[i] in [$90 .. $BF]) and (CountOfTrailingBytes = 2) then
              Inc(LFoundUTF8Strings)
            else
              Break;
          end;
        $F1 .. $F3:
          if CountOfTrailingBytes = 3 then
            Inc(LFoundUTF8Strings)
          else
            Break;
        $F4:
          begin
            Inc(i);
            if (i < LBufferSize) and (ABuffer[i] in [$80 .. $8F]) and (CountOfTrailingBytes = 2) then
              Inc(LFoundUTF8Strings)
            else
              Break;
          end;
        $C0, $C1, $F5 .. $FF: // invalid UTF-8 bytes
          Break;
        $80 .. $BF: // trailing bytes are consumed when handling leading bytes,
          // any occurence of "orphaned" trailing bytes is invalid UTF-8
          Break;
      end;

      if LFoundUTF8Strings = MinimumCountOfUTF8Strings then
      begin
        Result := True;
        Break;
      end;

      Inc(i);
    end;
  end;
end;

function OpenClipboard: Boolean;
var
  LRetryCount: Integer;
  LDelayStepMs: Integer;
begin
  LDelayStepMs := BCEDITOR_CLIPBOARD_DELAY_STEP_MS;
  Result := False;
  for LRetryCount := 1 to BCEDITOR_CLIPBOARD_MAX_RETRIES do
  try
    Clipboard.Open;
    Exit(True);
  except
    on Exception do
    if LRetryCount = BCEDITOR_CLIPBOARD_MAX_RETRIES then
      raise
    else
    begin
      Sleep(LDelayStepMs);
      Inc(LDelayStepMs, BCEDITOR_CLIPBOARD_DELAY_STEP_MS);
    end;
  end;
end;

function GetClipboardText: string;
var
  LGlobalMem: HGlobal;
  LLocaleID: LCID;
  LBytePointer: PByte;

  function AnsiStringToString(const AValue: AnsiString; ACodePage: Word): string;
  var
    LInputLength, LOutputLength: Integer;
  begin
    LInputLength := Length(AValue);
    LOutputLength := MultiByteToWideChar(ACodePage, 0, PAnsiChar(AValue), LInputLength, nil, 0);
    SetLength(Result, LOutputLength);
    MultiByteToWideChar(ACodePage, 0, PAnsiChar(AValue), LInputLength, PChar(Result), LOutputLength);
  end;

  function CodePageFromLocale(ALanguage: LCID): Integer;
  var
    LBuffer: array [0 .. 6] of Char;
  begin
    GetLocaleInfo(ALanguage, LOCALE_IDEFAULTANSICODEPAGE, LBuffer, 6);
    Result := StrToIntDef(LBuffer, GetACP);
  end;

begin
  Result := '';
  if not OpenClipboard then
    Exit;
  try
    if Clipboard.HasFormat(CF_UNICODETEXT) then
    begin
      LGlobalMem := Clipboard.GetAsHandle(CF_UNICODETEXT);
      if LGlobalMem <> 0 then
        try
          Result := PChar(GlobalLock(LGlobalMem));
        finally
          GlobalUnlock(LGlobalMem);
        end;
    end
    else
    begin
      LLocaleID := 0;
      LGlobalMem := Clipboard.GetAsHandle(CF_LOCALE);
      if LGlobalMem <> 0 then
        try
          LLocaleID := PInteger(GlobalLock(LGlobalMem))^;
        finally
          GlobalUnlock(LGlobalMem);
        end;

      LGlobalMem := Clipboard.GetAsHandle(CF_TEXT);
      if LGlobalMem <> 0 then
        try
          LBytePointer := GlobalLock(LGlobalMem);
          Result := AnsiStringToString(PAnsiChar(LBytePointer), CodePageFromLocale(LLocaleID));
        finally
          GlobalUnlock(LGlobalMem);
        end;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure SetClipboardText(const AText: string);
var
  LGlobalMem: HGlobal;
  LPGlobalLock: PByte;
  LLength: Integer;
begin
  if AText = '' then
    Exit;
  LLength := Length(AText);

  if not OpenClipboard then
    Exit;
  try
    Clipboard.Clear;
    { Set ANSI text only on Win9X, WinNT automatically creates ANSI from Unicode }
    if Win32Platform <> VER_PLATFORM_WIN32_NT then
    begin
      LGlobalMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, LLength + 1);
      if LGlobalMem <> 0 then
      begin
        LPGlobalLock := GlobalLock(LGlobalMem);
        try
          if Assigned(LPGlobalLock) then
          begin
            Move(PAnsiChar(AnsiString(AText))^, LPGlobalLock^, LLength + 1);
            Clipboard.SetAsHandle(CF_TEXT, LGlobalMem);
          end;
        finally
          GlobalUnlock(LGlobalMem);
        end;
      end;
    end;
    { Set unicode text, this also works on Win9X, even if the clipboard-viewer
      can't show it, Word 2000+ can paste it including the unicode only characters }
    LGlobalMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, (LLength + 1) * SizeOf(Char));
    if LGlobalMem <> 0 then
    begin
      LPGlobalLock := GlobalLock(LGlobalMem);
      try
        if Assigned(LPGlobalLock) then
        begin
          Move(PChar(AText)^, LPGlobalLock^, (LLength + 1) * SizeOf(Char));
          Clipboard.SetAsHandle(CF_UNICODETEXT, LGlobalMem);
        end;
      finally
        GlobalUnlock(LGlobalMem);
      end;
    end;
  finally
    Clipboard.Close;
  end;
end;

end.
