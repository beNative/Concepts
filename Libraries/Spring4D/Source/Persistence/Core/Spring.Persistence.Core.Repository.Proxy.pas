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

unit Spring.Persistence.Core.Repository.Proxy;

interface

{$IFDEF DELPHI2010}
  {$MESSAGE FATAL 'Proxy repository only supported on XE or higher'}
{$ENDIF}

uses
  Rtti,
  TypInfo,
  Spring.Collections,
  Spring.VirtualInterface,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.Core.Session,
  Spring.Persistence.Core.Repository.Simple;

type
  TMethodReference = reference to function(const Args: TArray<TValue>): TValue;

  TProxyRepository = class(TVirtualInterface)
  protected
    class function GetPageArgs(const args: TArray<TValue>;
      out page: Integer; out pageSize: Integer): TArray<TValue>;
    class function GetQueryTextFromMethod(const method: TRttiMethod): string;
  end;

  TProxyRepository<T: class, constructor; TID> = class(TProxyRepository)
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

implementation

uses
  Math,
  SysUtils,
  Variants,
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.Attributes,
  Spring.Reflection;


{$REGION 'TProxyRepository'}

class function TProxyRepository.GetPageArgs(const args: TArray<TValue>;
  out page: Integer; out pageSize: Integer): TArray<TValue>;
var
  i: Integer;
begin
  try
    i := High(args);
    pageSize := args[i].AsInteger;
    page := args[i - 1].AsInteger;
  except
    raise EORMInvalidArguments.Create('Last 2 arguments for paged requests should be page(Integer) and pageSize(Integer).');
  end;
  SetLength(Result, Length(args) - 2);
  for i := Low(Result) to High(Result) do
    Result[i] := args[i];
end;

class function TProxyRepository.GetQueryTextFromMethod(
  const method: TRttiMethod): string;
var
  attribute: QueryAttribute;
begin
  if method.TryGetCustomAttribute<QueryAttribute>(attribute) then
    Exit(attribute.QueryText);
  Result := '';
end;

{$ENDREGION}


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
      Result := DoOnInvoke(Method, Copy(Args, 1));
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
        items := fRepository.Query(GetQueryTextFromMethod(Method), Args);
        (items as ICollectionOwnership).OwnsObjects := False;
        Result := TValue.From<T>(items.FirstOrDefault);
      end;
      tkInterface:
      begin
        if Method.ReturnType.IsGenericTypeOf<IDBPage<TObject>> then
        begin
          // last two arguments should be page and pagesize
          pageArgs := GetPageArgs(Args, page, pageSize);
          Result := TValue.From<IDBPage<T>>(fSession.Page<T>(page, pageSize,
            GetQueryTextFromMethod(Method), pageArgs));
        end
        else
        begin
          Result := TValue.From<IList<T>>(fRepository.Query(
            GetQueryTextFromMethod(Method), Args));
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
      Result := TValue.From<IList<T>>(fRepository.FindWhere(Args[0].AsInterface as ICriterion));
    end);
  RegisterMethod(Format('function FindOne(const id: %s): %s', [fIdTypeName, fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := fRepository.FindOne(Args[0].AsType<TID>);
    end);
  RegisterMethod(Format('function FindAll: IList<%s>', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := TValue.From<IList<T>>(fRepository.FindAll);
    end);
  RegisterMethod(Format('function Exists(const id: %s): Boolean', [fIdTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := fRepository.Exists(Args[0].AsType<TID>);
    end);
  RegisterMethod(Format('procedure Insert(const entity: %s)', [fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.Insert(Args[0].AsType<T>);
    end);
  RegisterMethod(Format('procedure Insert(const entities: IEnumerable<%s>)', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.Insert(Args[0].AsInterface as IEnumerable<T>);
    end);
  RegisterMethod(Format('function Save(const entity: %0:s): %0:s', [fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := fRepository.Save(Args[0].AsType<T>);
    end);
  RegisterMethod(Format('function Save(const entities: IEnumerable<%0:s>): IEnumerable<%0:s>', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      Result := TValue.From<IEnumerable<T>>(fRepository.Save(Args[0].AsInterface as IEnumerable<T>));
    end);
  RegisterMethod(Format('procedure SaveCascade(const entity: %s)', [fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.SaveCascade(Args[0].AsType<T>);
    end);
  RegisterMethod(Format('procedure Delete(const entity: %s)', [fTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.Delete(Args[0].AsType<T>);
    end);
  RegisterMethod(Format('procedure Delete(const entities: IEnumerable<%s>)', [fQualifiedTypeName]),
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.Delete(Args[0].AsInterface as IEnumerable<T>);
    end);
  RegisterMethod('procedure DeleteAll',
    function(const Args: TArray<TValue>): TValue
    begin
      fRepository.DeleteAll;
    end);
  // because RTTI does not properly support open arrays this is not possible
//  RegisterMethod(Format('function Query(const query: string; const params: TValue): IList<%s>', [fQualifiedTypeName]),
//    function(const Args: TArray<TValue>): TValue
//    begin
//      Result := TValue.From<IList<T>>(fRepository.Query(Args[0].AsString, Args[1]));
//    end);
//  RegisterMethod('function Execute(const query: string; const params: TValue): NativeUInt',
//    function(const Args: TArray<TValue>): TValue
//    begin
//      Result := fRepository.Execute(Args[0].AsString, Copy(Args, 1));
//    end);
end;

procedure TProxyRepository<T, TID>.RegisterMethod(
  const methodSignature: string; const methodRef: TMethodReference);
begin
  fDefaultMethods.AddOrSetValue(methodSignature, methodRef);
end;

{$ENDREGION}


end.
