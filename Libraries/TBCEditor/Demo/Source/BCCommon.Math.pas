unit BCCommon.Math;

interface

function BinToInt(Value: string): LongInt;
function IntToBin(Value: LongInt; Digits: Integer): string;
function MinMax(x, mi, ma: Integer): Integer;

implementation

uses
  System.Math;

function BinToInt(Value: string): LongInt;
var
  i: Integer;
begin
  Result:=0;
  while Copy(Value,1,1) = '0' do
    Value := Copy(Value, 2, Length(Value) - 1);
  for i := Length(Value) downto 1 do
    if Copy(Value, i, 1) = '1' then
      Result := Result + (1 shl (Length(Value) - i));
end;

function IntToBin(Value: LongInt; Digits: Integer): string;
begin
  Result := StringOfChar('0', Digits);
  while Value > 0 do
  begin
    if (Value and 1) = 1 then
      Result[Digits] := '1';
    Dec(Digits);
    Value := Value shr 1;
  end;
end;

function MinMax(x, mi, ma: Integer): Integer;
begin
  x := Min(x, ma);
  Result := Max(x, mi);
end;

end.
