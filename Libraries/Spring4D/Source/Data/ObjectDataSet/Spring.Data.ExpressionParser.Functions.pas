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

{$I Spring.inc}

unit Spring.Data.ExpressionParser.Functions;

interface

uses
  Spring.Collections,
  SysUtils;

type
  TGetValueFunc = reference to function(const args: Variant): Variant;

  TFilterFunctions = record
  private
    class var
      fFunctions: IDictionary<string,TGetValueFunc>;
      fIsoFormatSettings: TFormatSettings;
  private
    class procedure Initialize; static;
    class constructor Create;
  public
    class procedure RegisterFunction(const name: string;
      const func: TGetValueFunc); static;
    class procedure UnregisterFunction(const name: string); static;
    class function TryGetFunction(const name: string;
      out func: TGetValueFunc): Boolean; static;
  end;

implementation

uses
  DateUtils,
  Math,
  StrUtils,
  Variants;

function VarArrayLength(AValue: Variant): Integer;
begin
  Result := VarArrayHighBound(AValue, 1);
  if Result >= 0 then
    Inc(Result)
  else
    Result := 0;
end;


{$REGION 'TFilterFunctions'}

class constructor TFilterFunctions.Create;
begin
  fIsoFormatSettings := TFormatSettings.Create;
  fIsoFormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  fIsoFormatSettings.DateSeparator := '-';
  fIsoFormatSettings.DecimalSeparator := '.';

  fFunctions := TCollections.CreateDictionary<string, TGetValueFunc>(
    50, TStringComparer.OrdinalIgnoreCase);
end;

class procedure TFilterFunctions.RegisterFunction(const name: string;
  const func: TGetValueFunc);
begin
  fFunctions.AddOrSetValue(name, func);
end;

class function TFilterFunctions.TryGetFunction(const name: string;
  out func: TGetValueFunc): Boolean;
begin
  Result := fFunctions.TryGetValue(name, func);
end;

class procedure TFilterFunctions.UnregisterFunction(const name: string);
begin
  fFunctions.Remove(name);
end;

class procedure TFilterFunctions.Initialize;
begin
  RegisterFunction('Today',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 0, 'Today does not require any argument');
      Result := Today;
    end);
  RegisterFunction('GetDate',
    function(const args: Variant): Variant
    begin
      Result := Now;
    end);
  RegisterFunction('IsNull',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 2, 'IsNull requires 2 arguments');
      if VarIsNull(Args[0]) then
        Result := Args[1]
      else
        Result := Args[0];
    end);
  RegisterFunction('Abs',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Abs requires 1 argument');
      Result := Abs(Args[0]);
    end);
  RegisterFunction('IsNotNull',
    function(const args: Variant): Variant
    begin
      Result := 1;
    end);
  RegisterFunction('Lower',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Lower requires 1 argument');
      Result := AnsiLowerCase(Args[0]);
    end);
  RegisterFunction('Upper',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Upper requires 1 argument');
      Result := AnsiUpperCase(Args[0]);
    end);
  RegisterFunction('Coalesce',
    function(const args: Variant): Variant
    var
      i: Integer;
    begin
      Assert(VarArrayLength(Args) >= 2, 'Coalesce requires at least 2 arguments');
      for i := VarArrayLowBound(Args, 1) to VarArrayHighBound(Args, 1) do
        if not VarIsNull(Args[i]) then
          Exit(Args[i]);
      Result := Null;
    end);
  RegisterFunction('Random',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Random requires 1 argument');
      Result := Random(Args[0]);
    end);
  RegisterFunction('Round',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Round requires 1 argument');
      Result := Round(Args[0]);
    end);
  RegisterFunction('Trim',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Trim requires 1 argument');
      Result := Trim(Args[0]);
    end);
  RegisterFunction('SubStr',
    function(const args: Variant): Variant
    const
      NumericVarTypes = [varSmallint, varInteger, varShortInt, varByte,
        varWord, varLongWord, varInt64, varUInt64];
    begin
      Assert(VarArrayLength(Args) = 3, 'SubStr requires 3 arguments');
      Assert(VarType(Args[1]) in NumericVarTypes, 'Index argument must be Integer') ;
      Assert(VarType(Args[2]) in NumericVarTypes, 'Length argument must be Integer') ;
      Result := Copy(Args[0], Integer(Args[1]), Integer(Args[2]));
    end);
  RegisterFunction('Quote',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Quote requires 1 argument');
      Result := QuotedStr(Args[0]);
    end);
  RegisterFunction('Max',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 2, 'Max requires 2 arguments');
      Result := Max(Args[0], Args[1]);
    end);
  RegisterFunction('Min',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 2, 'Min requires 2 arguments');
      Result := Min(Args[0], Args[1]);
    end);
  RegisterFunction('Length',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Length requires 1 argument');
      Result := Length(Args[0]);
    end);
  RegisterFunction('InStr',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 2, 'InStr requires 2 arguments');
      Result := Pos(string(Args[0]), string(Args[1]));
    end);
  RegisterFunction('Replace',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 3, 'Replace requires 3 arguments');
      Result := ReplaceText(Args[0], Args[1], Args[2]);
    end);
  RegisterFunction('Date',
    function(const args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Date requires 1 argument');
      Result := StrToDate(Args[0], fIsoFormatSettings);
    end);
end;

{$ENDREGION}


initialization
  TFilterFunctions.Initialize;

end.
