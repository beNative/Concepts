{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
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
  Spring,
  Spring.HazardEra;

type
  TEventBaseClass = class of TEventBase;

  /// <summary>
  ///   Base class for multicast event implementation
  /// </summary>
  TEventBase = class(TRefCountedObject)
  public type
    TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  private
    function GetRefCount: Integer; inline;
  strict protected
    fHandlers: PMethodArray;
    fNotificationHandler: TNotificationHandler;
    fOnChanged: TNotifyEvent;
  const
    objDestroyingFlag = Integer($80000000);
    DisabledFlag = Integer($40000000);
    RefCountMask = Integer($3FFFFFFF);
  {$REGION 'Property Accessors'}
    function GetCanInvoke: Boolean; inline;
    function GetEnabled: Boolean; inline;
    function GetHandlers: GuardedPointer;
    function GetOnChanged: TNotifyEvent;
    function GetUseFreeNotification: Boolean; inline;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
    procedure SetUseFreeNotification(const value: Boolean);
  {$ENDREGION}
    procedure EnsureNotificationHandler;
    procedure HandleNotification(component: TComponent; operation: TOperation);
  protected
    fInvoke: TMethodPointer;
    procedure Notify(sender: TObject; const item: TMethod;
      action: TCollectionNotification); virtual;
    function _Release: Integer; stdcall;
  public
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    property RefCount: Integer read GetRefCount;

  {$REGION 'Implements IEvent'}
    procedure Add(const handler: TMethod);
    procedure Remove(const handler: TMethod);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
  {$ENDREGION}

    property CanInvoke: Boolean read GetCanInvoke;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: TMethodPointer read fInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
    property UseFreeNotification: Boolean read GetUseFreeNotification write SetUseFreeNotification;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{$IFOPT O+}
function IsValidObject(p: Pointer): Boolean;

  function IsClassRef(p: Pointer): Boolean;
  begin
    try
      Result := PPointer(NativeInt(p) + vmtSelfPtr)^ = p;
    except
      Result := False;
    end;
  end;

begin
  Result := False;
  if Assigned(p) then
  try
    if not IsClassRef(p) then
      if PNativeInt(p)^ > $FFFF then
        Result := PPointer(p)^ = PPointer(PNativeInt(p)^ + vmtSelfPtr)^;
  except
  end; //FI:W501
end;
{$ELSE}
function IsValidObject(p: PPointer): Boolean;
{$IFDEF MSWINDOWS}
var
  memInfo: TMemoryBasicInformation;
{$ENDIF}

  function IsValidAddress(address: Pointer): Boolean;
  begin
    // Must be above 64k and 4 byte aligned
    if (UIntPtr(address) > $FFFF) and (UIntPtr(address) and 3 = 0) then
    begin
{$IFDEF MSWINDOWS}
      // do we need to recheck the virtual memory?
      if (UIntPtr(memInfo.BaseAddress) > UIntPtr(address))
        or ((UIntPtr(memInfo.BaseAddress) + memInfo.RegionSize) < (UIntPtr(address) + SizeOf(Pointer))) then
      begin
        // retrieve the status for the pointer
        memInfo.RegionSize := 0;
        VirtualQuery(address, memInfo, SizeOf(memInfo));
      end;
      // check the readability of the memory address
     if (memInfo.RegionSize >= SizeOf(Pointer))
        and (memInfo.State = MEM_COMMIT)
        and (memInfo.Protect and (PAGE_READONLY or PAGE_READWRITE
          or PAGE_WRITECOPY or PAGE_EXECUTE or PAGE_EXECUTE_READ
          or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY) <> 0)
        and (memInfo.Protect and PAGE_GUARD = 0) then
{$ENDIF}
      Exit(True);
    end;
    Result := False;
  end;

begin
  Result := False;
  if Assigned(p) then
  try
{$IFDEF MSWINDOWS}
    memInfo.RegionSize := 0;
{$ENDIF}
    if IsValidAddress(p)
      // not a class pointer - they point to themselves in the vmtSelfPtr slot
      and not (IsValidAddress(PByte(p) + vmtSelfPtr)
      and (p = PPointer(PByte(p) + vmtSelfPtr)^)) then
      if IsValidAddress(p^) and IsValidAddress(PByte(p^) + vmtSelfPtr)
        // looks to be an object, it points to a valid class pointer
        and (p^ = PPointer(PByte(p^) + vmtSelfPtr)^) then
        Result := True;
  except
  end; //FI:W501
end;
{$ENDIF}


{$REGION 'TEventBase'}

destructor TEventBase.Destroy; //FI:W504
begin
  if NativeUInt(fNotificationHandler) > 1 then
    fNotificationHandler.Free;
  NativeInt(fNotificationHandler) := 0;
  if Assigned(fHandlers) then
    Clear;
end;

procedure TEventBase.EnsureNotificationHandler;
var
  notificationHandler: TNotificationHandler;
begin
  if fNotificationHandler = nil then
  begin
    notificationHandler := TNotificationHandler.Create(nil);
    notificationHandler.OnNotification := HandleNotification;
    if AtomicCmpExchange(Pointer(fNotificationHandler), Pointer(notificationHandler), nil) <> nil then
      notificationHandler.Free;
  end;
end;

function TEventBase.GetEnabled: Boolean;
begin
  Result := fRefCount and DisabledFlag = 0;
end;

function TEventBase.GetCanInvoke: Boolean;
begin
  Result := Enabled and Assigned(fHandlers);
end;

function TEventBase.GetOnChanged: TNotifyEvent;
begin
  Result := fOnChanged;
end;

function TEventBase.GetRefCount: Integer;
begin
  Result := fRefCount and RefCountMask;
end;

function TEventBase.GetUseFreeNotification: Boolean;
begin
  Result := NativeInt(fNotificationHandler) <> 1;
end;

procedure TEventBase.Add(const handler: TMethod);
var
  guard: GuardedPointer;
  handlers, new: PMethodArray;
  count: NativeInt;
begin
  if not Assigned(handler.Code) then
    Exit;

  new := nil;
  repeat
    guard := AcquireGuard(fHandlers, new = nil);
    handlers := guard;
    count := DynArrayLength(handlers);
    EraArraySetLength(new, count + 1, TypeInfo(TMethod));
    EraArrayCopy(new, handlers);
    new[count] := handler;
  until AtomicCmpExchange(Pointer(fHandlers), new, handlers) = handlers;

  guard.Release;
  EraArrayClear(handlers);
  Notify(Self, handler, cnAdded);
end;

procedure TEventBase.BeforeDestruction;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
end;

procedure TEventBase.Clear;
var
  guard: GuardedPointer;
  handlers: PMethodArray;
  i: NativeInt;
begin
  if fHandlers = nil then Exit;

  handlers := nil;
  repeat
    guard := AcquireGuard(fHandlers, handlers = nil);
    handlers := guard;
  until AtomicCmpExchange(Pointer(fHandlers), nil, handlers) = handlers;

  try
    for i := 0 to DynArrayHigh(handlers) do
      Notify(Self, handlers[i], cnRemoved);
  finally
    guard.Release;
    EraArrayClear(handlers);
  end;
end;

function TEventBase.GetHandlers: GuardedPointer;
begin
  Result := AcquireGuard(fHandlers);
end;

procedure TEventBase.HandleNotification(component: TComponent;
  operation: TOperation);
begin
  if operation = opRemove then
    RemoveAll(component);
end;

procedure TEventBase.Notify(sender: TObject; const item: TMethod;
  action: TCollectionNotification);
var
  data: Pointer;
begin
  data := item.Data;
  if UseFreeNotification
    and IsValidObject(data) and TObject(data).InheritsFrom(TComponent) then
    case action of
      cnAdded:
      begin
        EnsureNotificationHandler;
        fNotificationHandler.FreeNotification(TComponent(data));
      end;
      cnRemoved:
        if fNotificationHandler <> nil then
          fNotificationHandler.RemoveFreeNotification(TComponent(data));
    end;

  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

procedure TEventBase.Remove(const handler: TMethod);
var
  guard: GuardedPointer;
  handlers, new: PMethodArray;
  count, index, i: NativeInt;
begin
  if not Assigned(handler.Code) then
    Exit;

  new := nil;
  repeat
    guard := AcquireGuard(fHandlers, new = nil);
    handlers := guard;
    count := DynArrayLength(handlers);
    index := -1;
    for i := 0 to count - 1 do
      if TMethod(handlers[i]) = TMethod(handler) then
      begin
        index := i;
        Break;
      end;
    if index = -1 then
    begin
      guard.Release;
      Exit;
    end;
    if count > 1 then
    begin
      EraArraySetLength(new, count, TypeInfo(TMethod));
      EraArrayCopy(new, handlers);
      EraArrayDelete(new, index);
    end;
  until AtomicCmpExchange(Pointer(fHandlers), new, handlers) = handlers;

  guard.Release;
  EraArrayClear(handlers);
  Notify(Self, handler, cnRemoved);
end;

procedure TEventBase.RemoveAll(instance: Pointer);
var
  guard: GuardedPointer;
  handlers, new: PMethodArray;
  oldItems: TArray<TMethod>;
  count, i, index: NativeInt;
begin
  new := nil;
  repeat
    guard := AcquireGuard(fHandlers, new = nil);
    handlers := guard;
    count := DynArrayLength(handlers);

    EraArraySetLength(new, count, TypeInfo(TMethod));
    EraArrayCopy(new, handlers);
    SetLength(oldItems, count);

    index := 0;
    for i := count - 1 downto 0 do
      if TMethod(handlers[i]).Data = instance then
      begin
        oldItems[index] := handlers[i];
        Inc(index);
        EraArrayDelete(new, i);
      end;

    if index = 0 then
    begin
      guard.Release;
      EraArrayClear(new);
      Exit;
    end;
  until AtomicCmpExchange(Pointer(fHandlers), new, handlers) = handlers;
  guard.Release;
  EraArrayClear(handlers);
  for i := index - 1 downto 0 do
    Notify(Self, oldItems[i], cnRemoved);
end;

procedure TEventBase.SetEnabled(const value: Boolean);
var
  bitMask: Integer;
  oldRefCount: Integer;
begin
  bitMask := Integer(value) shl 30;
  repeat
    oldRefCount := fRefCount;
  until AtomicCmpExchange(fRefCount, (oldRefCount or DisabledFlag) xor bitMask, oldRefCount) = oldRefCount;

  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

procedure TEventBase.SetOnChanged(const value: TNotifyEvent);
begin
  fOnChanged := value;
end;

procedure TEventBase.SetUseFreeNotification(const value: Boolean);
var
  guard: GuardedPointer;
  data: Pointer;
  handler: PMethodArray;
begin
  MonitorEnter(Self);
  try
    case NativeInt(fNotificationHandler) of
      0: // UseFreeNotification is True but no handler assigned yet ...
        if not value then // ... it can only be turned False
          NativeInt(fNotificationHandler) := 1;
      1: // UseFreeNotification is False ...
        if value then // ... it can only be turned True
        begin
          NativeInt(fNotificationHandler) := 0;
          guard := AcquireGuard(fHandlers);
          handler := guard;
          try
            if Assigned(handler) and Assigned(handler.Code) then
            repeat
              data := handler.Data;
              if IsValidObject(data) and TObject(data).InheritsFrom(TComponent) then
              begin
                EnsureNotificationHandler;
                fNotificationHandler.FreeNotification(TComponent(data));
              end;
              Inc(handler);
            until not Assigned(handler.Code);
          finally
            guard.Release;
          end;
        end;
    else // UseFreeNotification is True and handler is already assigned ...
      if not value then // ... it can only be turned False
      begin
        fNotificationHandler.Free;
        NativeInt(fNotificationHandler) := 1;
      end;
    end;
  finally
    MonitorExit(Self);
  end;
end;

function TEventBase._Release: Integer;
begin
  Result := AtomicDecrement(fRefCount) and not DisabledFlag;
  if Result = 0 then
  begin
    fRefCount := objDestroyingFlag;
    Destroy;
  end;
end;

{$ENDREGION}


end.
