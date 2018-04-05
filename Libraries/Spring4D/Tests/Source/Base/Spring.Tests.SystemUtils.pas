{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.Tests.SystemUtils;

interface

uses
  SysUtils,
  TestFramework,
  Spring,
  Spring.SystemUtils;

type
  TTestSplitString = class(TTestCase)
  private
    fStrings: TStringDynArray;
  protected
    procedure TearDown; override;
  published
    procedure TestEmptyString;
    procedure TestOneEntry;
    procedure TestEmptyEntry;
    procedure TestMoreEntries;
    procedure TestRemoveEmptyEntries;
  end;

  TTestTryStrToDateTimeFmt = class(TTestCase)
  published
    procedure TestParseDate;
    procedure TestParseTime;
    procedure TestParseDateTime;
    procedure TestFailedCases;
  end;

  TTestSplitNullTerminatedStrings = class(TTestCase)
  private
    fStrings: TStringDynArray;
    fBuffer: TCharArray;
  protected
    procedure TearDown; override;
  published
    procedure TestNil;
    procedure TestEmpty;
    procedure TestOneEntry;
    procedure TestThreeEntries;
    procedure TestVariousStrings;
  end;

implementation

uses
  Classes,
  DateUtils,
  TypInfo;


{$REGION 'TTestSplitString'}

procedure TTestSplitString.TestEmptyString;
begin
  fStrings := SplitString('', []);
  CheckEquals(0, Length(fStrings));
end;

procedure TTestSplitString.TestOneEntry;
begin
  fStrings := SplitString('word', [' ']);
  CheckEquals(1, Length(fStrings));
  CheckEquals('word', fStrings[0]);

  fStrings := SplitString('2', [' ']);
  CheckEquals(1, Length(fStrings));
  CheckEquals('2', fStrings[0]);
end;

procedure TTestSplitString.TestMoreEntries;
begin
  fStrings := SplitString('one word', [' ']);
  CheckEquals(2, Length(fStrings));
  CheckEquals('one', fStrings[0]);
  CheckEquals('word', fStrings[1]);

  fStrings := SplitString('one two three four', [' ']);
  CheckEquals(4, Length(fStrings));
  CheckEquals('one', fStrings[0]);
  CheckEquals('two', fStrings[1]);
  CheckEquals('three', fStrings[2]);
  CheckEquals('four', fStrings[3]);

  fStrings := SplitString('2.0', ['.']);
  CheckEquals(2, Length(fStrings));
  CheckEquals('2', fStrings[0]);
  CheckEquals('0', fStrings[1]);
end;

procedure TTestSplitString.TearDown;
begin
  inherited;
  SetLength(fStrings, 0);
end;

procedure TTestSplitString.TestEmptyEntry;
begin
  fStrings := SplitString('one  word', [' ']);
  CheckEquals(3, Length(fStrings));
  CheckEquals('one', fStrings[0]);
  CheckEquals('', fStrings[1]);
  CheckEquals('word', fStrings[2]);

  fStrings := SplitString('1..2', ['.']);
  CheckEquals(3, Length(fStrings));
  CheckEquals('1', fStrings[0]);
  CheckEquals('', fStrings[1]);
  CheckEquals('2', fStrings[2]);

  fStrings := SplitString('12..3..456', ['.']);
  CheckEquals(5, Length(fStrings));
  CheckEquals('12', fStrings[0]);
  CheckEquals('', fStrings[1]);
  CheckEquals('3', fStrings[2]);
  CheckEquals('', fStrings[3]);
  CheckEquals('456', fStrings[4]);

  fStrings := SplitString('.', ['.']);
  CheckEquals(2, Length(fStrings));
  CheckEquals('', fStrings[0]);
  CheckEquals('', fStrings[1]);

  fStrings := SplitString('.1.', ['.']);
  CheckEquals(3, Length(fStrings));
  CheckEquals('', fStrings[0]);
  CheckEquals('1', fStrings[1]);
  CheckEquals('', fStrings[2]);
end;

procedure TTestSplitString.TestRemoveEmptyEntries;
begin
  fStrings := SplitString('1..2', ['.'], True);
  CheckEquals(2, Length(fStrings));
  CheckEquals('1', fStrings[0]);
  CheckEquals('2', fStrings[1]);

  fStrings := SplitString('.', ['.'], True);
  CheckEquals(0, Length(fStrings));
end;

{$ENDREGION}


{$REGION 'TTestSplitNullTerminatedStrings'}

procedure TTestSplitNullTerminatedStrings.TearDown;
begin
  inherited;
  SetLength(fStrings, 0);
  SetLength(fBuffer, 0);
end;

procedure TTestSplitNullTerminatedStrings.TestEmpty;
begin
  fBuffer := TCharArray.Create(#0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(0, Length(fStrings));
end;

procedure TTestSplitNullTerminatedStrings.TestNil;
begin
  fStrings := SplitString(nil);
  CheckEquals(0, Length(fStrings));
end;

procedure TTestSplitNullTerminatedStrings.TestOneEntry;
begin
  fBuffer := TCharArray.Create('C', ':', #0, #0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(1, Length(fStrings));
  CheckEquals('C:', fStrings[0]);
end;

procedure TTestSplitNullTerminatedStrings.TestThreeEntries;
begin
  fBuffer := TCharArray.Create('C', ':', #0, 'D', ':', #0, 'E', ':', #0, #0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(3, Length(fStrings));
  CheckEquals('C:', fStrings[0]);
  CheckEquals('D:', fStrings[1]);
  CheckEquals('E:', fStrings[2]);
end;

procedure TTestSplitNullTerminatedStrings.TestVariousStrings;
begin
  fBuffer := TCharArray.Create('A', 'B', 'C', #0, 'D', 'E', #0, 'F', #0, #0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(3, Length(fStrings));
  CheckEquals('ABC', fStrings[0]);
  CheckEquals('DE', fStrings[1]);
  CheckEquals('F', fStrings[2]);
end;

{$ENDREGION}


{$REGION 'TTestTryStrToDateTimeFmt'}

procedure TTestTryStrToDateTimeFmt.TestParseDate;
var
  actual, expected: TDateTime;
begin
  expected := EncodeDate(2009, 10, 18);

  CheckTrue(TryStrToDateTimeFmt('20091018', 'YYYYMMDD', actual));
  CheckTrue(SameDateTime(actual, expected));

  CheckTrue(TryStrToDateTimeFmt('091018', 'YYMMDD', actual));
  CheckTrue(SameDateTime(actual, expected));

  CheckTrue(TryStrToDateTimeFmt('10-18-2009', 'MM-DD-YYYY', actual));
  CheckTrue(SameDateTime(actual, expected));

  CheckTrue(TryStrToDateTimeFmt(' 2009-10-18 ', 'YYYY-MM-DD', actual));
  CheckTrue(SameDateTime(actual, expected));
end;

procedure TTestTryStrToDateTimeFmt.TestParseTime;
var
  actual, expected: TDateTime;
begin
  expected := EncodeTime(12, 10, 18, 35);
  CheckTrue(TryStrToDateTimeFmt('12:10:18.035', 'hh:nn:ss.zzz', actual));
  CheckTrue(SameDateTime(actual, expected));

  expected := EncodeTime(12, 10, 0, 0);
  CheckTrue(TryStrToDateTimeFmt('12:10 ', 'hh:nn', actual));
  CheckTrue(SameDateTime(actual, expected));
end;

procedure TTestTryStrToDateTimeFmt.TestParseDateTime;
var
  actual, expected: TDateTime;
begin
  expected := EncodeDateTime(2009, 10, 18, 12, 30, 59, 200);
  CheckTrue(TryStrToDateTimeFmt('2009-10-18 12:30:59.200', 'YYYY-MM-DD HH:NN:SS.ZZZ', actual));
  CheckTrue(SameDateTime(actual, expected));

  expected := EncodeDateTime(2009, 10, 18, 12, 30, 59, 200);
  CheckTrue(TryStrToDateTimeFmt('20091018123059200', 'YYYYMMDDHHNNSSZZZ', actual));
  CheckTrue(SameDateTime(actual, expected));
end;

procedure TTestTryStrToDateTimeFmt.TestFailedCases;
var
  value: TDateTime;
begin
  CheckFalse(TryStrToDateTimeFmt('', 'YYYYMMDD', value));
  CheckFalse(TryStrToDateTimeFmt(' ', 'YYYYMMDD', value));
  CheckFalse(TryStrToDateTimeFmt('2009', 'YYYYMMDD', value));
  CheckFalse(TryStrToDateTimeFmt('2009080', 'YYYYMMDD', value));
  CheckFalse(TryStrToDateTimeFmt('2009080A', 'YYYYMMDD', value));
  CheckFalse(TryStrToDateTimeFmt('200908011230', 'YYYYMMDDHHNNSS', value));
  CheckFalse(TryStrToDateTimeFmt('20090801123007', 'YYYYMMDDHHNNSSZZZ', value));
end;

{$ENDREGION}

end.
