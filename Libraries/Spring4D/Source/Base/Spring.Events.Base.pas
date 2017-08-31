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

unit Spring.Events.Base;

interface

uses
  Classes,
  Generics.Collections,
  SyncObjs,
  Spring;

type
  PMethodPointer = ^TMethodPointer;

  /// <summary>
  ///   Base class for multicast event implementation
  /// </summary>
  TEventBase = class(TInterfacedObject, IEvent)
  private
    fEnabled: Boolean;
    fHandlers: TList<TMethodPointer>;
    fLock: TCriticalSection;
    fOnChanged: TNotifyEvent;
    fNotificationHandler: TNotificationHandler;

    {$REGION 'Property Accessors'}
    function GetCanInvoke: Boolean;
    function GetEnabled: Boolean;
    function GetHandlers: TArray<TMethodPointer>;
    function GetInvoke: TMethodPointer;
    function GetOnChanged: TNotifyEvent;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
  {$ENDREGION}
  protected
    fInvoke: TMethodPointer;
    procedure EventsChanged; virtual;
    procedure HandleNotification(Component: TComponent;
      Operation: TOperation);
    procedure Notify(Sender: TObject; const Item: TMethodPointer;
      Action: TCollectionNotification); virtual;
    property Handlers: TArray<TMethodPointer> read GetHandlers;
  public
    constructor Create;
    destructor Destroy; override;

    {$REGION 'IEvent Methods'}
    procedure Add(const handler: TMethodPointer);
    procedure Remove(const handler: TMethodPointer);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
    {$ENDREGION}

    property CanInvoke: Boolean read GetCanInvoke;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: TMethodPointer read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

  TEventBase<T> = class(TEventBase, IEvent<T>)
  {$REGION 'Property Accessors'}
    function GetInvoke: T;
  {$ENDREGION}
    procedure Add(handler: T);
    procedure Remove(handler: T);
    procedure ForEach(const action: TAction<T>);
  end;

implementation

uses
  Generics.Defaults,
  TypInfo;

function IsClassPtr(p: Pointer): Boolean;
begin
  try
    Result := PPointer(NativeInt(p) + vmtSelfPtr)^ = p;
  except
    Result := False;
  end;
end;

function IsValidObj(p: Pointer): Boolean;
begin
  Result := False;
  if Assigned(p) then
  try
    if not IsClassPtr(p) then
      if PNativeInt(p)^ > $FFFF then
        Result := PPointer(p)^ = PPointer(PNativeInt(p)^ + vmtSelfPtr)^;
  except
  end; //FI:W501
end;

function SafeIsClass(p: Pointer; cls: TClass): Boolean; inline;
begin
  Result := IsValidObj(p) and (TObject(p) is cls);
end;


{$REGION 'Method comparer'}

function NopAddref(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopRelease(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function Compare_Method(Inst: Pointer; const Left, Right: TMethodPointer): Integer;
type
  UIntPtr = NativeUInt;
  TMethod = record
    Code, Data: UIntPtr;
  end;
var
  LMethod, RMethod: TMethod;
begin
  LMethod := TMethod(Left);
  RMethod := TMethod(Right);

  if (LMethod.Data < RMethod.Data)
    or ((LMethod.Data = RMethod.Data)
    and (LMethod.Code < RMethod.Code)) then
    Result := -1
  else if (LMethod.Data > RMethod.Data)
    or ((LMethod.Data = RMethod.Data)
    and (LMethod.Code > RMethod.Code)) then
    Result := 1
  else
    Result := 0;
end;

const
  Comparer_Vtable_Method: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Method
  );
  Comparer_Instance_Method: Pointer = @Comparer_Vtable_Method;

{$ENDREGION}


{$REGION 'TEventBase'}

constructor TEventBase.Create;
begin
  inherited Create;
  fEnabled := True;
  // some Delphi versions have a broken comparer for tkMethod
  fHandlers := TList<TMethodPointer>.Create(
    IComparer<TMethodPointer>(@Comparer_Instance_Method));
  fHandlers.OnNotify := Notify;
  fLock := TCriticalSection.Create;
end;

destructor TEventBase.Destroy;
begin
  fNotificationHandler.Free;
  fNotificationHandler := nil;
  fHandlers.Free;
  fLock.Free;
  inherited Destroy;
end;

procedure TEventBase.EventsChanged;
begin
  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

procedure TEventBase.Add(const handler: TMethodPointer);
begin
  fLock.Enter;
  try
    fHandlers.Add(handler);
  finally
    fLock.Leave;
  end;
end;

procedure TEventBase.Clear;
begin
  fLock.Enter;
  try
    fHandlers.Clear;
  finally
    fLock.Leave;
  end;
end;

function TEventBase.GetCanInvoke: Boolean;
begin
  Result := fEnabled and (fHandlers.Count <> 0);
end;

function TEventBase.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TEventBase.GetHandlers: TArray<TMethodPointer>;
var
  i: Integer;
begin
  fLock.Enter;
  try
    SetLength(Result, fHandlers.Count);
    for i := 0 to fHandlers.Count - 1 do
      Result[i] := fHandlers[i];
  finally
    fLock.Leave;
  end;
end;

function TEventBase.GetInvoke: TMethodPointer;
begin
  Result := fInvoke;
end;

function TEventBase.GetOnChanged: TNotifyEvent;
begin
  Result := fOnChanged;
end;

procedure TEventBase.HandleNotification(Component: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
    RemoveAll(Component);
end;

procedure TEventBase.Notify(Sender: TObject; const Item: TMethodPointer;
  Action: TCollectionNotification);
var
  data: Pointer;
begin
  data := TMethod(Item).Data;
  if SafeIsClass(data, TComponent) then
    case Action of
      cnAdded:
      begin
        if fNotificationHandler = nil then
        begin
          fNotificationHandler := TNotificationHandler.Create(nil);
          fNotificationHandler.OnNotification := HandleNotification;
        end;
        fNotificationHandler.FreeNotification(TComponent(data));
      end;
      cnRemoved:
        if fNotificationHandler <> nil then
          fNotificationHandler.RemoveFreeNotification(TComponent(data));
    end;

  EventsChanged;
end;

procedure TEventBase.Remove(const handler: TMethodPointer);
begin
  fLock.Enter;
  try
    fHandlers.Remove(handler);
  finally
    fLock.Leave;
  end;
end;

procedure TEventBase.RemoveAll(instance: Pointer);
var
  i: Integer;
begin
  fLock.Enter;
  try
    for i := fHandlers.Count - 1 downto 0 do
      if TMethod(fHandlers[i]).Data = instance then
        fHandlers.Delete(i);
  finally
    fLock.Leave;
  end;
end;

procedure TEventBase.SetEnabled(const value: Boolean);
begin
  fEnabled := value;
end;

procedure TEventBase.SetOnChanged(const value: TNotifyEvent);
begin
  fOnChanged := value;
end;

{$ENDREGION}


{$REGION 'TEventBase<T>'}

procedure TEventBase<T>.Add(handler: T);
begin
{$IFDEF DELPHI2010}
  if PTypeInfo(TypeInfo(T)).Kind = tkInterface then
{$ELSE}
  if TType.Kind<T> = tkInterface then
{$ENDIF}
    inherited Add(MethodReferenceToMethodPointer(handler))
  else
    inherited Add(PMethodPointer(@handler)^);
end;

procedure TEventBase<T>.ForEach(const action: TAction<T>);
var
  handler: TMethodPointer;
begin
  for handler in Handlers do
{$IFDEF DELPHI2010}
    if PTypeInfo(TypeInfo(T)).Kind = tkInterface then
{$ELSE}
    if TType.Kind<T> = tkInterface then
{$ENDIF}
      TAction<IInterface>(action)(MethodPointerToMethodReference(handler))
    else
      TAction<TMethodPointer>(action)(handler);
end;

function TEventBase<T>.GetInvoke: T;
begin
{$IFDEF DELPHI2010}
  if PTypeInfo(TypeInfo(T)).Kind = tkInterface then
{$ELSE}
  if TType.Kind<T> = tkInterface then
{$ENDIF}
    IInterface(PPointer(@Result)^) := MethodPointerToMethodReference(inherited Invoke)
  else
    PMethodPointer(@Result)^ := inherited Invoke;
end;

procedure TEventBase<T>.Remove(handler: T);
begin
{$IFDEF DELPHI2010}
  if PTypeInfo(TypeInfo(T)).Kind = tkInterface then
{$ELSE}
  if TType.Kind<T> = tkInterface then
{$ENDIF}
    inherited Remove(MethodReferenceToMethodPointer(handler))
  else
    inherited Remove(PMethodPointer(@handler)^);
end;

{$ENDREGION}


end.
