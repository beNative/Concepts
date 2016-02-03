unit BCCommon.Hash;

interface

uses
  System.SysUtils;

function HashLine(const Line: string; IgnoreCase, IgnoreBlanks: Boolean): PLongWord; overload;
function HashLine(const Line: string): LongWord; overload;

implementation

uses
  BCCommon.StringUtils;

function HashLine(const Line: string): LongWord;
var
  i: Integer;
const
  FNV_offset_basis = 2166136261;
  FNV_prime = 16777619;
begin
  Result := FNV_offset_basis;
  for i := 1 to Length(Line) do
    Result := (Result xor Byte(Line[i])) * FNV_prime;
end;

function HashLine(const Line: string; IgnoreCase, IgnoreBlanks: Boolean): PLongWord;
var
  s: string;
begin
  s := Line;
  if IgnoreBlanks then
    s := DeleteWhiteSpace(s);
  if IgnoreCase then
    s := LowerCase(s);

  Result := PLongWord(HashLine(s));
end;

end.
