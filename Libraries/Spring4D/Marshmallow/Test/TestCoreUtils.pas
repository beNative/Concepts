unit TestCoreUtils;

{.$DEFINE PERFORMANCE_TESTS}

interface

uses
  TestFramework;

type
  TTestCoreUtils = class(TTestCase)
  published
    procedure TryConvert_Nullable;
    {$IFDEF PERFORMANCE_TESTS}
    procedure TryConvert_Nullable_Speed;
    {$ENDIF}
  end;

implementation

uses
  TypInfo,
  SysUtils,
  Spring;

{ TTestCoreUtils }

procedure TTestCoreUtils.TryConvert_Nullable;
var
  LFrom, LResult: TValue;
  bOK: Boolean;
  LValue: Nullable<Double>;
  LSpringValue: Nullable<string>;
begin
  LFrom := 'Bob';
  bOK := LFrom.TryConvert(TypeInfo(Nullable<string>), LResult);
  CheckTrue(bOK);
  CheckEquals('Nullable<System.string>', LResult.TypeInfo.TypeName);
  CheckTrue(LResult.TryAsType<Nullable<string>>(LSpringValue));
  CheckTrue(LSpringValue.HasValue);

  CheckTrue(LResult.TryGetNullableValue(LFrom));
  CheckEquals('Bob', LFrom.AsString);

  LFrom := 256.12;
  bOK := LFrom.TryConvert(TypeInfo(Nullable<Double>), LResult);
  CheckTrue(bOK);
  CheckEquals('Nullable<System.Double>', LResult.TypeInfo.TypeName);
  CheckTrue(LResult.TryAsType<Nullable<Double>>(LValue));
  CheckTrue(LValue.HasValue);
  CheckEquals(256.12, LValue.Value, 0.001);

  CheckTrue(LResult.TryGetNullableValue(LFrom));
  CheckEquals(256.12, LFrom.AsExtended, 0.001);
end;

{$IFDEF PERFORMANCE_TESTS}
procedure TTestCoreUtils.TryConvert_Nullable_Speed;
var
  LFrom, LResult: TValue;
  bOK: Boolean;
  LCount, i: Integer;
  sw: TStopwatch;
begin
  bOK := False;
  LFrom := 'Bob';
  LCount := 100000;

  sw := TStopwatch.StartNew;
  for i := 1 to LCount do
    bOK := LFrom.TryConvert(TypeInfo(Nullable<string>), LResult);

  sw.Stop;
  CheckTrue(bOK);
  CheckEquals('Nullable<System.string>', string(LResult.TypeInfo.Name));

  Status(Format('Set %d Spring Nullable<string> values in %d ms', [LCount, sw.ElapsedMilliseconds]));

  sw := TStopwatch.StartNew;
  for i := 1 to LCount do
    bOK := LFrom.TryConvert(TypeInfo(Nullable<string>), LResult);
  sw.Stop;
  CheckTrue(bOK);
  Status(Format('Set %d simple string values in %d ms', [LCount, sw.ElapsedMilliseconds]));

  sw := TStopwatch.StartNew;
  for i := 1 to LCount do
    bOK := LFrom.TryConvert(TypeInfo(Nullable<Double>), LResult);
  sw.Stop;
  CheckTrue(bOK);
  CheckEquals('Nullable<System.Double>', string(LResult.TypeInfo.Name));

  Status(Format('Set %d Marshmallow Nullable<double> values in %d ms', [LCount, sw.ElapsedMilliseconds]));
end;
{$ENDIF}

initialization
  RegisterTest('Spring.Persistence.Core', TTestCoreUtils.Suite);

end.
