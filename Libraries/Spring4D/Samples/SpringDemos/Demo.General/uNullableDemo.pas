unit uNullableDemo;

interface

uses
  Spring;

procedure RunNullableDemo;

implementation

procedure ShowThreeStates(const value: Nullable<Boolean>);
begin
  if value.HasValue then
  begin
    Write('Value set to: ');
    if value.Value then
      Writeln('True')
    else
      Writeln('False');
  end
  else
    Writeln('Null')
end;

procedure RunNullableDemo;
var
  NullableBoolean: Nullable<Boolean>;
begin
  try
    WriteLn(NullableBoolean.Value);
  except
    on E: EInvalidOperationException do
      Writeln('Value has not been set, EInvalidOperationException properly thrown');
  end;

  ShowThreeStates(NullableBoolean);

  NullableBoolean := True;
  ShowThreeStates(NullableBoolean);

  NullableBoolean := False;
  ShowThreeStates(NullableBoolean);
end;

end.
