{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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

unit Spring.Persistence.Core.Repository.Proxy;

interface

{$IFNDEF DELPHIXE_UP}
  {$MESSAGE FATAL 'Proxy repository only supported on XE or higher'}
{$ENDIF}

uses
  Rtti,
  TypInfo,
  Spring.Collections,
{$IFDEF DELPHIXE}
  Spring.VirtualInterface,
{$ENDIF}
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.Core.Session,
  Spring.Persistence.Core.Repository.Simple;

type
  TMethodReference = reference to function(const Args: TArray<TValue>): TValue;

  TProxyRepository<T: class, constructor; TID> = class(TVirtualInterface)
  private
    fRepository: IPagedRepository<T,TID>;
    {$IFDEF WEAKREF}[Weak]{$ENDIF}
    fSession: TSession;
    fDefaultMethods: IDictionary<string, TMethodReference>;
    fTypeName: string;
    fQualifiedTypeName: string;
    fIdTypeName: string;
    fQualifiedIdTypeName: string;
  protected
    function DoOnInvoke(const Method: TRttiMethod; const Args: TArray<TValue>): TValue;
    procedure RegisterDefaultMethods;
    procedure RegisterMethod(const methodSignature: string; const methodRef: TMethodReference);
  public
    constructor Create(const session: TSession; typeInfo: PTypeInfo;
      repositoryClass: TClass = nil); reintroduce;
  end;

function FromArgsToConstArray(const args: TArray<TValue>): TArray<TVarRec>;
procedure FinalizeVarRec(var item: TVarRec);
procedure FinalizeVarRecArray(var values: TArray<TVarRec>);
function GetPageArgs(const args: TArray<TValue>; out page: Integer; out pageSize: Integer): TArray<TValue>;
function GetQueryTextFromMethod(const method: TRttiMethod): string;

implementation

uses
  Math,
  SysUtils,
  Variants,
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.Attributes,
  Spring.Reflection;

function FromArgsToConstArray(const args: TArray<TValue>): TArray<TVarRec>;
var
  i: Integer;
  item: TVarRec;
  value: TValue;
begin
  SetLength(Result, Max(0, Length(args) - 1));
  for i := Low(args) + 1 to High(args) do
  begin
    value := args[i];
    item := Default(TVarRec);
    case value.Kind of
      tkInteger:
      begin
        item.VInteger := value.AsInteger;
        item.VType := vtInteger;
      end;
      tkEnumeration:
      begin
        item.VBoolean := value.AsBoolean;
        item.VType := vtBoolean;
      end;
      tkString, tkUString, tkLString, tkWString:
      begin
        item.VUnicodeString := nil;
        string(item.VUnicodeString) := value.AsString;
        item.VType := vtUnicodeString;
      end;
      tkFloat:
      begin
        New(item.VExtended);
        item.VExtended^ := value.AsExtended;
        item.VType := vtExtended;
      end;
      tkRecord: ;
      tkInt64:
      begin
        New(item.VInt64);
        item.VInt64^ := value.AsInt64;
        item.VType := vtInt64;
      end;
    else
      raise EORMUnsupportedType.CreateFmt('Unknown open argument type (%s)', [value.ToString]);
    end;
    Result[i - 1] := item;
  end;
end;

procedure FinalizeVarRec(var item: TVarRec);
begin
  case item.VType of
    vtExtended: Dispose(item.VExtended);
{$IFNDEF NEXTGEN}
    vtString: Dispose(item.VString);
    vtAnsiString: string(item.VAnsiString) := '';
{$ENDIF}
    vtPWideChar: FreeMem(item.VPWideChar);
{$IF Declared(WideString)}
    vtWideString: WideString(item.VWideString) := '';
{$IFEND}
    vtUnicodeString: string(item.VUnicodeString) := '';
    vtCurrency: Dispose(item.VCurrency);
    vtVariant: Dispose(item.VVariant);
    vtInterface: IInterface(item.VInterface) := nil;
{$IFDEF AUTOREFCOUNT}
    vtObject: TObject(item.VObject) := nil;
{$ENDIF}
    vtInt64: Dispose(item.VInt64);
  end;
  item.VInteger := 0;
end;

procedure FinalizeVarRecArray(var values: TArray<TVarRec>);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    FinalizeVarRec(values[i]);
  values := nil;
end;

function GetPageArgs(const args: TArray<TValue>; out page: Integer; out pageSize: Integer): TArray<TValue>;
var
  i: Integer;
begin
  try
    pageSize := args[High(args)].AsInteger;
    page := args[High(args) - 1].AsInteger;
  except
    raise EORMInvalidArguments.Create('Last 2 arguments for paged requests should be page(Integer) and pageSize(Integer).');
  end;
  SetLength(Result, Length(args) - 2);
  for i := Low(Result) to High(Result) do
    Result[i] := args[i];
end;

function GetQueryTextFromMethod(const method: TRttiMethod): string;
var
  attribute: QueryAttribute;
begin
  if method.TryGetCustomAttribute<QueryAttribute>(attribute) then
    Exit(attribute.QueryText);
  Result := '';
end;


{$REGION 'TProxyRepository<T, TID>'}

constructor TProxyRepository<T, TID>.Create(const session: TSession;
  typeInfo: PTypeInfo; repositoryClass: TClass);
begin
  inherited Create(typeInfo);
  fSession := session;
  fDefaultMethods := TCollections.CreateDictionary<string, TMethodReference>(
    TStringComparer.OrdinalIgnoreCase);
  if not Assigned(repositoryClass) then
    fRepository := TSimpleRepository<T,TID>.Create(session)
  else
    fRepository := TActivator.CreateInstance(repositoryClass, [session]) as TSimpleRepository<T,TID>;
  fTypeName := PTypeInfo(System.TypeInfo(T)).TypeName;
  fIdTypeName := PTypeInfo(System.TypeInfo(TID)).TypeName;
  fQualifiedTypeName := TType.GetType(System.TypeInfo(T)).QualifiedName;
  fQualifiedIdTypeName := TType.GetType(System.TypeInfo(TID)).QualifiedName;
  RegisterDefaultMethods;
  OnInvoke :=
    procedure(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue)
    begin
      Result := DoOnInvoke(Method, Args);
    end;
{$IFDEF AUTOREFCOUNT}
  // Release reference held by ancestor RawCallBack (bypass RSP-10177)
  __ObjRelease;
  // Release reference held by OnInvoke (RSP-10176)
  __ObjRelease;
  // Release reference held by RegisterDefaultMethods (RSP-10176)
  __ObjRelease;
{$ENDIF}
end;

function TProxyRepository<T, TID>.DoOnInvoke(const Method: TRttiMethod;
  const Args: TArray<TValue>): TValue;
var
  methodRef: TMethodReference;
  constArray: TArray<TVarRec>;
  items: IList<T>;
  pageArgs: TArray<TValue>;
  page, pageSize: Integer;
begin
  if fDefaultMethods.TryGetValue(Method.ToString, methodRef) then
    Result := methodRef(Args)
  else
  begin
    case Method.ReturnType.TypeKind of
      tkInteger, tkInt64: Result := fRepository.Count;
      tkClass, tkClassRef, tkPointer:
      begin
        constArray := FromArgsToConstArray(Args);
        try
          items := fRepository.Query(GetQueryTextFromMethod(Method), constArray);
          (items as ICollectionOwnership).OwnsObjects := False;
          Result := TValue.From<T>(items.FirstOrDefault);
        finally
          FinalizeVarRecArray(constArray);
        end;
      end;
      tkInterface:
      begin
        if Method.ReturnType.IsGenericTypeOf<IDBPage<TObject>> then
        begin
          // last two arguments should be page and pagesize
          pageArgs := GetPageArgs(Args, page, pageSize);
          constArray := FromArgsToConstArray(pageArgs);
          try
            Result := TValue.From<IDBPage<T>>(fSession.Page<T>(page, pageSize,
              GetQueryTextFromMethod(Method), constArray));
          finally
            FinalizeVarRecArray(constArray);
          end;
        end
        else
        begin
          constArray := FromArgsToConstArray(Args);
          try
            Result := TValue.From<IList<T>>(fRepository.Query(
              GetQueryTextFromMethod(Method), constArray));
          finally
            FinalizeVarRecArray(constArray);
          end;
        end;
      end
      else
        raise EORMUnsupportedType.CreateFmt('Unknown method (%s) return type: %s',
          [Method.ToString, Method.ReturnType.ToString]);
    end;
  end;
end;

procedure TProxyRepository<T, TID>.RegisterDefaultMethods;
begin
  RegisterMethod('function Count: Int64',
    function(const Args: TArray<TValue>): TValue
    begin
      Result := fRepository.Count;
    end);
  RegisterMethod(Format('function FindWhere: ICriteria<%s>', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := TValue.From<ICriteria<T>>(fRepository.FindWhere);
    end);
  RegisterMethod(Format('function FindWhere(const expression: ICriterion): IList<%s>', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := TValue.From<IList<T>>(fRepository.FindWhere(Args[1].AsInterface as ICriterion));
    end);
  RegisterMethod(Format('function FindOne(const id: %s): %s', [fIdTypeName, fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := fRepository.FindOne(Args[1].AsType<TID>);
    end);
  RegisterMethod(Format('function FindAll: IList<%s>', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := TValue.From<IList<T>>(fRepository.FindAll);
    end);
  RegisterMethod(Format('function Exists(const id: %s): Boolean', [fIdTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := fRepository.Exists(Args[1].AsType<TID>);
    end);
  RegisterMethod(Format('procedure Insert(const entity: %s)', [fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.Insert(Args[1].AsType<T>);
    end);
  RegisterMethod(Format('procedure Insert(const entities: IEnumerable<%s>)', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.Insert(Args[1].AsInterface as IEnumerable<T>);
    end);
  RegisterMethod(Format('function Save(const entity: %0:s): %0:s', [fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := fRepository.Save(Args[1].AsType<T>);
    end);
  RegisterMethod(Format('function Save(const entities: IEnumerable<%0:s>): IEnumerable<%0:s>', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := TValue.From<IEnumerable<T>>(fRepository.Save(Args[1].AsInterface as IEnumerable<T>));
    end);
  RegisterMethod(Format('procedure SaveCascade(const entity: %s)', [fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.SaveCascade(Args[1].AsType<T>);
    end);
  RegisterMethod(Format('procedure Delete(const entity: %s)', [fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.Delete(Args[1].AsType<T>);
    end);
  RegisterMethod(Format('procedure Delete(const entities: IEnumerable<%s>)', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.Delete(Args[1].AsInterface as IEnumerable<T>);
    end);
  RegisterMethod('procedure DeleteAll',
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.DeleteAll;
    end);
  RegisterMethod(Format('function Query(const query: string; const params: TVarRec): IList<%s>', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    var
      LConstArray: TArray<TVarRec>;
    begin
      LConstArray := FromArgsToConstArray(Copy(Args, 1));
      try
        Result := TValue.From<IList<T>>(fRepository.Query(Args[1].AsString, LConstArray));
      finally
        FinalizeVarRecArray(LConstArray);
      end;
    end);
  RegisterMethod('function Execute(const query: string; const params: TVarRec): NativeUInt',
    function(const Args: TArray<TValue>): TValue
    var
      LConstArray: TArray<TVarRec>;
    begin
      LConstArray := FromArgsToConstArray(Copy(Args, 1));
      try
        Result := fRepository.Execute(Args[1].AsString, LConstArray);
      finally
        FinalizeVarRecArray(LConstArray);
      end;
    end);
end;

procedure TProxyRepository<T, TID>.RegisterMethod(
  const methodSignature: string; const methodRef: TMethodReference);
begin
  fDefaultMethods.AddOrSetValue(methodSignature, methodRef);
end;

{$ENDREGION}


end.
