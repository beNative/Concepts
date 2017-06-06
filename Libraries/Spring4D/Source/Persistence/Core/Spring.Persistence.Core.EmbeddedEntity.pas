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

unit Spring.Persistence.Core.EmbeddedEntity;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Interfaces;

type
  TEmbeddedEntity = class(TInterfacedObject, IDBResultSet)
  protected
    function IsObject: Boolean; virtual; abstract;
    function IsArray: Boolean; virtual; abstract;

    function IsEmpty: Boolean; virtual; abstract;
    function Next: Boolean;
    function FieldExists(const fieldName: string): Boolean; virtual; abstract;
    function GetFieldValue(index: Integer): Variant; overload; virtual; abstract;
    function GetFieldValue(const fieldName: string): Variant; overload; virtual; abstract;
    function GetFieldCount: Integer; virtual; abstract;
    function GetFieldName(index: Integer): string; virtual; abstract;
  public
    constructor Create; virtual;

    function GetValue(const fieldName: string): Variant; virtual; abstract;
    procedure AddValue(const fieldName: string; const value: Variant); virtual; abstract;
  end;

  TFieldKey = string;

  TEmbeddedObjectEntity = class(TEmbeddedEntity)
  private
    fValues: IDictionary<string, Variant>;
    fIndexedValues: IList<string>;
    fCurrent: Integer;
  protected
    function GetKey(const fieldName: string): TFieldKey; virtual;

    function IsEmpty: Boolean; override;
    function FieldExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; override;
    function GetFieldValue(const fieldName: string): Variant; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  public
    constructor Create; override;

    function IsArray: Boolean; override;
    function IsObject: Boolean; override;

    function GetValue(const fieldName: string): Variant; override;
    procedure AddValue(const fieldName: string; const value: Variant); override;
  end;

  TEmbeddedArrayEntity = class(TEmbeddedEntity)
  private
    fValues: IList<Variant>;
    fCurrent: Integer;
  protected
    function IsEmpty: Boolean; override;
    function FieldExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; override;
    function GetFieldValue(const fieldname: string): Variant; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  public
    constructor Create; override;

    function IsArray: Boolean; override;
    function IsObject: Boolean; override;

    function GetValue(const fieldName: string): Variant; override;
    procedure AddValue(const fieldName: string; const value: Variant); override;
  end;

implementation

uses
  SysUtils,
  Variants;


{$REGION 'TEmbeddedEntity'}

constructor TEmbeddedEntity.Create;
begin
  inherited Create;
end;

function TEmbeddedEntity.Next: Boolean;
begin
  Result := True;
end;

{$ENDREGION}


{$REGION 'TEmbeddedObjectEntity'}

constructor TEmbeddedObjectEntity.Create;
begin
  inherited Create;
  fValues := TCollections.CreateDictionary<string, Variant>;
  fIndexedValues := TCollections.CreateList<string>;
  fCurrent := -1;
end;

procedure TEmbeddedObjectEntity.AddValue(const fieldName: string;
  const value: Variant);
begin
  fValues.AddOrSetValue(GetKey(fieldName), value);
  fIndexedValues.Add(GetKey(fieldName));
end;

function TEmbeddedObjectEntity.FieldExists(
  const fieldName: string): Boolean;
begin
  Result := fValues.ContainsKey(GetKey(fieldName));
end;

function TEmbeddedObjectEntity.GetFieldCount: Integer;
begin
  Result := fIndexedValues.Count;
end;

function TEmbeddedObjectEntity.GetFieldName(index: Integer): string;
begin
  Result := fIndexedValues[index];
end;

function TEmbeddedObjectEntity.GetFieldValue(index: Integer): Variant;
begin
  Result := GetValue(GetFieldName(index));
end;

function TEmbeddedObjectEntity.GetFieldValue(const fieldName: string): Variant;
begin
  Result := GetValue(fieldName);
end;

function TEmbeddedObjectEntity.GetKey(const fieldName: string): TFieldKey;
begin
  Result := AnsiUpperCase(fieldName);
end;

function TEmbeddedObjectEntity.GetValue(const fieldName: string): Variant;
begin
  Result := fValues[GetKey(fieldName)];
end;

function TEmbeddedObjectEntity.IsArray: Boolean;
begin
  Result := False;
end;

function TEmbeddedObjectEntity.IsEmpty: Boolean;
begin
  Result := fCurrent >= fIndexedValues.Count;
  fCurrent := fIndexedValues.Count;
end;

function TEmbeddedObjectEntity.IsObject: Boolean;
begin
  Result := True;
end;

{$ENDREGION}


{$REGION 'TEmbeddedArrayEntity'}

constructor TEmbeddedArrayEntity.Create;
begin
  inherited Create;
  fValues := TCollections.CreateList<Variant>;
  fCurrent := -1;
end;

procedure TEmbeddedArrayEntity.AddValue(const fieldName: string;
  const value: Variant);
begin
  fValues.Add(value);
end;

function TEmbeddedArrayEntity.FieldExists(
  const fieldName: string): Boolean;
begin
  Result := False;
end;

function TEmbeddedArrayEntity.GetFieldCount: Integer;
begin
  Result := 0;
end;

function TEmbeddedArrayEntity.GetFieldName(index: Integer): string;
begin
  Result := '';
end;

function TEmbeddedArrayEntity.GetFieldValue(index: Integer): Variant;
begin
  Result := fValues[index];
end;

function TEmbeddedArrayEntity.GetFieldValue(const fieldname: string): Variant;
begin
  Result := GetValue(fieldname);
end;

function TEmbeddedArrayEntity.GetValue(const fieldName: string): Variant;
var
  results: IDBResultSet;
begin
  Result := fValues[fCurrent];
  if VarType(Result) = varUnknown then
  begin
    results := IInterface(Result) as IDBResultSet;
    Result := results.GetFieldValue(fieldName);
  end;
end;

function TEmbeddedArrayEntity.IsArray: Boolean;
begin
  Result := True;
end;

function TEmbeddedArrayEntity.IsEmpty: Boolean;
begin
  Inc(fCurrent);
  Result := fCurrent >= fValues.Count;
end;

function TEmbeddedArrayEntity.IsObject: Boolean;
begin
  Result := False;
end;

{$ENDREGION}


end.
