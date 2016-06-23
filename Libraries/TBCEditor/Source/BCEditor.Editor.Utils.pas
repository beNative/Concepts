unit BCEditor.Editor.Utils;

interface

uses
  System.Classes, BCEditor.Types;

function GetTextPosition(AChar, ALine: Integer): TBCEditorTextPosition;
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean;

implementation

uses
  System.Math, System.SysUtils, BCEditor.Consts;

function GetTextPosition(AChar, ALine: Integer): TBCEditorTextPosition;
begin
  with Result do
  begin
    Char := AChar;
    Line := ALine;
  end;
end;

// checks for a BOM in UTF-8 format or searches the first 4096 bytes for typical UTF-8 octet sequences
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean;
const
  MinimumCountOfUTF8Strings = 1;
  MaxBufferSize = $4000;
var
  Buffer: array of Byte;
  BufferSize, i, FoundUTF8Strings: Integer;

  { 3 trailing bytes are the maximum in valid UTF-8 streams,
    so a count of 4 trailing bytes is enough to detect invalid UTF-8 streams }
  function CountOfTrailingBytes: Integer;
  begin
    Result := 0;
    Inc(i);
    while (i < BufferSize) and (Result < 4) do
    begin
      if Buffer[i] in [$80 .. $BF] then
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
  BufferSize := Min(MaxBufferSize, Stream.Size - Stream.Position);

  // if no special characteristics are found it is not UTF-8
  Result := False;
  WithBOM := False;

  if BufferSize > 0 then
  begin
    SetLength(Buffer, BufferSize);
    Stream.ReadBuffer(Buffer[0], BufferSize);
    Stream.Seek(-BufferSize, soFromCurrent);

    { first search for BOM }
    if (BufferSize >= Length(UTF8BOM)) and CompareMem(@Buffer[0], @UTF8BOM[0], Length(UTF8BOM)) then
    begin
      WithBOM := True;
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
    FoundUTF8Strings := 0;
    i := 0;
    while i < BufferSize do
    begin
      case Buffer[i] of
        $00 .. $7F: // skip US-ASCII characters as they could belong to various charsets
          ;
        $C2 .. $DF:
          if CountOfTrailingBytes = 1 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $E0:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$A0 .. $BF]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $E1 .. $EC, $EE .. $EF:
          if CountOfTrailingBytes = 2 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $ED:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$80 .. $9F]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F0:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$90 .. $BF]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F1 .. $F3:
          if CountOfTrailingBytes = 3 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $F4:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$80 .. $8F]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $C0, $C1, $F5 .. $FF: // invalid UTF-8 bytes
          Break;
        $80 .. $BF: // trailing bytes are consumed when handling leading bytes,
          // any occurence of "orphaned" trailing bytes is invalid UTF-8
          Break;
      end;

      if FoundUTF8Strings = MinimumCountOfUTF8Strings then
      begin
        Result := True;
        Break;
      end;

      Inc(i);
    end;
  end;
end;

end.
