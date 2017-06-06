{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

unit Spring.Persistence.SQL.Params;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  DB,
  Spring,
  Spring.Collections;

type
  TDBParamClass = class of TDBParam;

  /// <summary>
  ///   Represents query parameter.
  /// </summary>
  TDBParam = class
  protected
    fName: string;
    fValue: TValue;
    fParamType: TFieldType;
    function TypeInfoToFieldType(typeInfo: PTypeInfo): TFieldType; virtual;
  public
    constructor Create(const name: string; const value: TValue); virtual;

    function GetNormalizedParamName(const prefix: string = ':'): string;
    function ToVariant: Variant; overload; virtual;
    function ToVariant(dataType: TFieldType): Variant; overload; virtual;

    property Name: string read fName;
    property ParamType: TFieldType read fParamType;
    property Value: TValue read fValue;
  end;

  TDBParams = record
  public
    class function Create(const values: array of TValue): IEnumerable<TDBParam>; static;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  TypInfo,
  Spring.Persistence.Core.Exceptions;


{$REGION 'TDBParam'}

constructor TDBParam.Create(const name: string; const value: TValue);
begin
  inherited Create;
  fName := AnsiUpperCase(name);
  if not StartsStr(':', fName) then
    fName := ':' + fName;
  fValue := value;
  fParamType := TypeInfoToFieldType(fValue.TypeInfo);
end;

function TDBParam.GetNormalizedParamName(const prefix: string): string;
begin
  if StartsStr(prefix, fName) then
    Result := Copy(fName, Length(prefix) + 1)
  else
    Result := fName;
end;

function TDBParam.ToVariant: Variant;
begin
  Result := fValue.ToVariant;
end;

function TDBParam.ToVariant(dataType: TFieldType): Variant;
begin
  case dataType of
    ftVarBytes: Result := fValue.ConvertTo<TArray<Byte>>;
    ftGuid: Result := fValue.ConvertTo<string>;
  else
    Result := ToVariant;
  end;
end;

function TDBParam.TypeInfoToFieldType(typeInfo: PTypeInfo): TFieldType;
begin
  if not Assigned(typeInfo) then
    Exit(ftUnknown);

  case typeInfo.Kind of
    tkInteger, tkSet: Result := ftInteger;
    tkInt64: Result := ftLargeint;
    tkChar, tkLString, tkString: Result := ftString;
    tkWChar, tkWString, tkUString: Result := ftWideString;
    tkEnumeration:
      if typeInfo = System.TypeInfo(Boolean) then
        Result := ftBoolean
      else
        Result := ftInteger;
    tkFloat:
      if typeInfo = System.TypeInfo(TDateTime) then
        Result := ftDateTime
      else if typeInfo = System.TypeInfo(TDate) then
        Result := ftDate
      else if typeInfo = System.TypeInfo(TTime) then
        Result := ftTime
      else if typeInfo = System.TypeInfo(Currency) then
        Result := ftCurrency
      else if typeInfo = System.TypeInfo(Extended) then
        Result := TFieldType.ftExtended
      else
        Result := ftFloat;
    tkClass, tkArray, tkInterface, tkDynArray: Result := ftBlob;
    tkVariant: Result := ftVariant;
    tkRecord:
      if IsNullable(typeInfo) then
        Result := TypeInfoToFieldType(GetUnderlyingType(typeInfo))
      else if typeInfo = System.TypeInfo(TGUID) then
        Result := ftGuid
      else
        Result := ftBlob;
    tkClassRef, tkPointer: Result := ftReference;
  else
    Result := ftUnknown;
  end;
end;

{$ENDREGION}


{$REGION 'TDBParams'}

class function TDBParams.Create(const values: array of TValue): IEnumerable<TDBParam>;
var
  i: Integer;
  param: TDBParam;
  params: IList<TDBParam>;
begin
  if Length(values) > 0 then
  begin
    params := TCollections.CreateObjectList<TDBParam>;
    for i := Low(values) to High(values) do
    begin
      param := TDBParam.Create(Format(':%d', [i]), values[i]);
      params.Add(param);
    end;
    Result := params;
  end
  else
    Result := TEnumerable.Empty<TDBParam>;
end;

{$ENDREGION}


end.
