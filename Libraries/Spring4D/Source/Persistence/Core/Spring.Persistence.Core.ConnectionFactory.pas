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

unit Spring.Persistence.Core.ConnectionFactory;

interface

uses
  {$IFDEF DELPHIXE6_UP}
  JSON,
  {$ELSE}
  DBXJSON,
  {$ENDIF}
  Spring.Collections,
  Spring.Persistence.Core.Interfaces;

type
  /// <summary>
  ///   Static class which acts as factory for <c>IDBConnection</c>s.
  /// </summary>
  TConnectionFactory = class sealed
  private
    class var fRegistered: IDictionary<TDBDriverType,TClass>;

    class function CreateConnection(connectionClass: TClass;
      const externalConnection: TObject): IDBConnection; overload;

    class procedure SetConnectionProperties(
      const externalConnection: TObject; const jsonObject: TJSONObject);
    class procedure SetConnectionConnected(
      const qualifiedName: string; const externalConnection: TObject);
  public
    class constructor Create;
    class destructor Destroy;

    class function GetInstance(const key: TDBDriverType;
      const externalConnection: TObject): IDBConnection; overload;
    class function GetInstance(const key: TDBDriverType;
      const jsonString: string): IDBConnection; overload;
    class function GetInstanceFromFile(const key: TDBDriverType;
      const jsonFilename: string): IDBConnection;
    class procedure RegisterConnection<T: class, IDBConnection>(const key: TDBDriverType);
  end;

implementation

uses
  Classes,
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Reflection;


function GetJsonPair(const jsonObject: TJSONObject; index: Integer): TJSONPair;
begin
  {$IFDEF DELPHIXE6_UP}
  Result := jsonObject.Pairs[index];
  {$ELSE}
  Result := jsonObject.Get(index);
  {$ENDIF}
end;

function GetJsonObjectCount(const jsonObject: TJSONObject): Integer;
begin
  {$IFDEF DELPHIXE6_UP}
  Result := jsonObject.Count;
  {$ELSE}
  Result := jsonObject.Size;
  {$ENDIF}
end;


{$REGION 'TConnectionFactory'}

class constructor TConnectionFactory.Create;
begin
  fRegistered := TCollections.CreateDictionary<TDBDriverType,TClass>;
end;

class destructor TConnectionFactory.Destroy;
begin
  fRegistered := nil;
end;

class function TConnectionFactory.CreateConnection(connectionClass: TClass;
  const externalConnection: TObject): IDBConnection;
begin
  TActivator.CreateInstance(connectionClass, [externalConnection])
    .GetInterface(IDBConnection, Result);
end;

class function TConnectionFactory.GetInstance(const key: TDBDriverType;
  const externalConnection: TObject): IDBConnection;
var
  connectionClass: TClass;
begin
  if not fRegistered.TryGetValue(key, connectionClass) then
    raise EORMConnectionNotRegistered.Create('Connection not registered');

  Result := CreateConnection(connectionClass, externalConnection);
  if not Assigned(Result) then
    raise EORMUnsupportedType.Create('Connection type not supported');
end;

class function TConnectionFactory.GetInstance(const key: TDBDriverType;
  const jsonString: string): IDBConnection;
var
  externalConnection: TObject;
  jsonObject: TJSONObject;
  qualifiedName: string;
begin
  externalConnection := nil;
  jsonObject := TJSONObject.ParseJSONValue(jsonString) as TJSONObject;
  if Assigned(jsonObject) then
  try
    qualifiedName := GetJsonPair(jsonObject, 0).JsonString.Value;
    externalConnection := TActivator.CreateInstance(qualifiedName, [nil]);
    try
      SetConnectionProperties(externalConnection, GetJsonPair(jsonObject, 0).JsonValue as TJSONObject);
      SetConnectionConnected(qualifiedName, externalConnection);
    except
      externalConnection.Free;
      raise;
    end;
  finally
    jsonObject.Free;
  end;
  Result := GetInstance(key, externalConnection);
  Result.AutoFreeConnection := True;
end;

class function TConnectionFactory.GetInstanceFromFile(const key: TDBDriverType;
  const jsonFilename: string): IDBConnection;
var
  fileStream: TStringStream;
begin
  fileStream := TStringStream.Create;
  try
    fileStream.LoadFromFile(jsonFilename);
    fileStream.Position := 0;
    Result := GetInstance(key, fileStream.DataString);
  finally
    fileStream.Free;
  end;
end;

class procedure TConnectionFactory.RegisterConnection<T>(const key: TDBDriverType);
begin
  if fRegistered.ContainsKey(key) then
    raise EORMConnectionAlreadyRegistered.Create('Connection already registered');

  fRegistered.Add(key, TClass(T));
end;

class procedure TConnectionFactory.SetConnectionConnected(
  const qualifiedName: string; const externalConnection: TObject);
begin
  TType.SetPropertyValue(externalConnection, 'Connected', True);
end;

class procedure TConnectionFactory.SetConnectionProperties(
  const externalConnection: TObject; const jsonObject: TJSONObject);
var
  connectionType: TRttiType;
  i: Integer;
  jsonPair: TJSONPair;
  name: string;
  value: TValue;
  prop: TRttiProperty;
begin
  connectionType := TType.GetType(externalConnection.ClassInfo);
  for i := 0 to GetJsonObjectCount(jsonObject) - 1 do
  begin
    jsonPair := GetJsonPair(jsonObject, i);
    name := jsonPair.JsonString.Value;
    if connectionType.TryGetProperty(name, prop) then
    begin
      value := jsonPair.JsonValue.Value;
      // do only simplest conversions
      case prop.PropertyType.TypeKind of
        tkEnumeration:
          if prop.PropertyType.IsType<Boolean> then
            value := StrToBool(jsonPair.JsonValue.Value);
        tkInteger: value := StrToInt(jsonPair.JsonValue.Value);
        tkInt64: value := StrToInt64(jsonPair.JsonValue.Value);
      end;
      prop.SetValue(externalConnection, value);
    end;
  end;
end;

{$ENDREGION}


end.
