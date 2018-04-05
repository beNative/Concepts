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

unit Spring.Container.Pool;

interface

uses
  SyncObjs,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  IObjectPool = interface
    ['{E5842280-3750-46C0-8C91-0888EFFB0ED5}']
    procedure Initialize(const context: ICreationContext);
    function GetInstance(const context: ICreationContext): TObject;
    procedure ReleaseInstance(const instance: TObject);
  end;

  IObjectPool<T> = interface(IObjectPool)
    function GetInstance(const context: ICreationContext): T;
    procedure ReleaseInstance(const instance: T);
  end;

  IPoolableObjectFactory = interface(IComponentActivator)
    ['{56F9E805-A115-4E3A-8583-8D0B5462D98A}']
    function Validate(const instance: TObject): Boolean;
    procedure Activate(const instance: TObject);
    procedure Passivate(const instance: TObject);
  end;

  IPoolableObjectFactory<T> = interface(IPoolableObjectFactory)
    function Validate(const instance: T): Boolean;
    procedure Activate(const instance: T);
    procedure Passivate(const instance: T);
  end;

  TSimpleObjectPool = class(TInterfacedObject, IObjectPool)
  private
    fLock: TCriticalSection;
    fActivator: IComponentActivator;
    fMinPoolsize: Nullable<Integer>;
    fMaxPoolsize: Nullable<Integer>;
    // Use pointers to eliminate ARC on NextGen and just use our own mechanism
    // for keeping the references alive. (The inactive object collection can
    // also work the same way on all platforms this way.)
    fAvailableList: IQueue<Pointer>;
    fActiveList: IList<Pointer>;
    fInstances: IList<Pointer>;
    fInitialized: Boolean;
  protected
    function AddNewInstance(const context: ICreationContext): TObject;
    procedure CollectInactiveInstances;
    function GetAvailableObject: TObject;
    procedure InstancesChanged(Sender: TObject; const item: Pointer;
      action: TCollectionChangedAction);
    property MinPoolsize: Nullable<Integer> read fMinPoolsize;
    property MaxPoolsize: Nullable<Integer> read fMaxPoolsize;
  public
    constructor Create(const activator: IComponentActivator; minPoolSize, maxPoolSize: Integer);
    destructor Destroy; override;
    procedure Initialize(const context: ICreationContext); virtual;
    function GetInstance(const context: ICreationContext): TObject; virtual;
    procedure ReleaseInstance(const instance: TObject); virtual;
  end;

implementation

uses
  SysUtils,
  Spring.Container.Common;

type
  TInterfacedObjectAccess = class(TInterfacedObject);


{$REGION 'TSimpleObjectPool'}

constructor TSimpleObjectPool.Create(const activator: IComponentActivator;
  minPoolSize, maxPoolSize: Integer);
begin
  inherited Create;
  fActivator := activator;
  if minPoolSize > 0 then
    fMinPoolsize := minPoolSize;
  if maxPoolSize > 0 then
    fMaxPoolsize := maxPoolSize;
  fInstances := TCollections.CreateList<Pointer>;
  fInstances.OnChanged.Add(InstancesChanged);
  fAvailableList := TCollections.CreateQueue<Pointer>;
  fActiveList := TCollections.CreateList<Pointer>;
  fLock := TCriticalSection.Create;
end;

destructor TSimpleObjectPool.Destroy;
begin
  // Needs to be freed prior our weakrefs get cleared
  fInstances := nil;
  fLock.Free;
  inherited Destroy;
end;

function TSimpleObjectPool.AddNewInstance(const context: ICreationContext): TObject;
var
  refCounted: IRefCounted;
begin
  Result := fActivator.CreateInstance(context).AsObject;
  if Result.InheritsFrom(TInterfacedObject) then
    TInterfacedObjectAccess(Result)._AddRef
  else if Supports(Result, IRefCounted, refCounted) then
    refCounted._AddRef
{$IFDEF AUTOREFCOUNT}
  else Result.__ObjAddRef
{$ENDIF}
  ;
  fInstances.Add(Result);
end;

procedure TSimpleObjectPool.CollectInactiveInstances;
var
  instance: Pointer;
  refCounted: IRefCounted;
begin
  for instance in fInstances.ToArray do
  begin
    if TObject(instance).InheritsFrom(TInterfacedObject)
      and (TInterfacedObject(instance).RefCount = 1)
      and fActiveList.Contains(instance) then
    begin
      ReleaseInstance(instance);
    end else
    if Supports(TObject(instance), IRefCounted, refCounted)
      and (refCounted.RefCount = 2)
      and fActiveList.Contains(instance) then
    begin
      refCounted := nil;
      ReleaseInstance(instance);
    end;
  end;
end;

procedure TSimpleObjectPool.Initialize(const context: ICreationContext);
var
  i: Integer;
  instance: TObject;
begin
  if not fMinPoolsize.HasValue then
    Exit;
  for i := 0 to fMinPoolsize.Value - 1 do //FI:W528
  begin
    instance := AddNewInstance(context);
    fAvailableList.Enqueue(instance);
  end;
  fInitialized := True;
end;

procedure TSimpleObjectPool.InstancesChanged(Sender: TObject;
  const item: Pointer; action: TCollectionChangedAction);
var
  refCounted: IRefCounted;
begin
  if action = caRemoved then
  begin
    if TObject(item).InheritsFrom(TInterfacedObject) then
      TInterfacedObjectAccess(item)._Release
    else if Supports(TObject(item), IRefCounted, refCounted) then
      refCounted._Release
    else
{$IFNDEF AUTOREFCOUNT}
      TObject(item).Free;
{$ELSE}
    begin
      if TObject(item).__ObjRelease > 0 then
        TObject(item).DisposeOf;
    end;
{$ENDIF}
  end;
end;

function TSimpleObjectPool.GetAvailableObject: TObject;
begin
  Result := fAvailableList.Dequeue;
end;

function TSimpleObjectPool.GetInstance(const context: ICreationContext): TObject;
begin
  fLock.Acquire;
  try
    if not fInitialized then
      Initialize(context);

    if not fAvailableList.Any then
      CollectInactiveInstances;

    if fAvailableList.Any then
      Result := GetAvailableObject
    else
      Result := AddNewInstance(context);

    fActiveList.Add(Result);
  finally
    fLock.Release;
  end;
end;

procedure TSimpleObjectPool.ReleaseInstance(const instance: TObject);
var
  recyclable: IRecyclable;
begin
  Guard.CheckNotNull(instance, 'instance');

  fLock.Acquire;
  try
    fActiveList.Remove(instance);

    if Supports(instance, IRecyclable, recyclable) then
      recyclable.Recycle;

    if not fMaxPoolsize.HasValue or (fAvailableList.Count < fMaxPoolsize.Value) then
      fAvailableList.Enqueue(instance)
    else
      fInstances.Remove(instance);
  finally
    fLock.Release;
  end;
end;

{$ENDREGION}


end.
