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

unit Spring.Events.Base;

interface

uses
  Classes,
  Generics.Collections,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Spring;

type
  PMethodPointer = ^TMethodPointer;

  /// <summary>
  ///   Base class for multicast event implementation
  /// </summary>
  TEventBase = class(TInterfacedObject, IEvent)
  strict private
    fHandlers: TArray<TMethodPointer>;
    fCount: Integer;
    fNotificationHandler: TNotificationHandler;
    fOnChanged: TNotifyEvent;
  {$IFDEF MSWINDOWS}
    fLock: TRTLCriticalSection;
  {$ENDIF MSWINDOWS}
    const DisabledFlag = Integer($80000000);
  {$REGION 'Property Accessors'}
    function GetCanInvoke: Boolean; inline;
    function GetCount: Integer; inline;
    function GetEnabled: Boolean; inline;
    function GetHandlers: TArray<TMethodPointer>;
    function GetInvoke: TMethodPointer; inline;
    function GetOnChanged: TNotifyEvent;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
  {$ENDREGION}
    procedure Delete(index: Integer);
    procedure HandleNotification(component: TComponent; operation: TOperation);
    procedure LockEnter; inline;
    procedure LockLeave; inline;
    property Count: Integer read GetCount;
  protected
    fInvoke: TMethodPointer;
    procedure Notify(sender: TObject; const item: TMethodPointer;
      action: TCollectionNotification); virtual;
    property Handlers: TArray<TMethodPointer> read GetHandlers;
  public
    constructor Create;
    destructor Destroy; override;

  {$REGION 'Implements IEvent'}
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
  protected
  {$REGION 'Property Accessors'}
    function GetInvoke: T;
  {$ENDREGION}
  public
  {$REGION 'Implements IEvent<T>'}
    procedure Add(handler: T);
    procedure Remove(handler: T);
    procedure ForEach(const action: TAction<T>);
  {$ENDREGION}
  end;

implementation

uses
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


{$REGION 'TEventBase'}

constructor TEventBase.Create;
begin
  inherited Create;
{$IFDEF MSWINDOWS}
  InitializeCriticalSection(fLock);
{$ENDIF}
end;

destructor TEventBase.Destroy;
begin
  fNotificationHandler.Free;
  fNotificationHandler := nil;
  Clear;
{$IFDEF MSWINDOWS}
  DeleteCriticalSection(fLock);
{$ENDIF}
  inherited Destroy;
end;

function TEventBase.GetCanInvoke: Boolean;
begin
  Result := fCount > 0;
end;

function TEventBase.GetCount: Integer;
begin
  Result := fCount and not DisabledFlag;
end;

function TEventBase.GetEnabled: Boolean;
begin
  Result := fCount >= 0;
end;

function TEventBase.GetInvoke: TMethodPointer;
begin
  Result := fInvoke;
end;

function TEventBase.GetOnChanged: TNotifyEvent;
begin
  Result := fOnChanged;
end;

procedure TEventBase.LockEnter;
begin
{$IFDEF MSWINDOWS}
  EnterCriticalSection(fLock);
{$ELSE}
  TMonitor.Enter(Self);
{$ENDIF}
end;

procedure TEventBase.LockLeave;
begin
{$IFDEF MSWINDOWS}
  LeaveCriticalSection(fLock);
{$ELSE}
  TMonitor.Exit(Self);
{$ENDIF}
end;

procedure TEventBase.Add(const handler: TMethodPointer);
var
  i: Integer;
begin
  LockEnter;
  try
    i := Count;
    if Length(fHandlers) < i + 1 then
      if i = 0 then
        SetLength(fHandlers, 1)
      else
        SetLength(fHandlers, i * 2);
    fHandlers[i] := handler;
    AtomicIncrement(fCount);
    Notify(Self, handler, cnAdded);
  finally
    LockLeave;
  end;
end;

procedure TEventBase.Clear;
var
  i: Integer;
begin
  LockEnter;
  try
    for i := 0 to Count - 1 do
      Notify(Self, fHandlers[i], cnRemoved);
    fHandlers := nil;
  finally
    LockLeave;
  end;
end;

procedure TEventBase.Delete(index: Integer);
var
  oldItem: TMethodPointer;
  i: Integer;
begin
  oldItem := fHandlers[index];
  AtomicDecrement(fCount);
  for i := index to Count - 1 do
    fHandlers[i] := fHandlers[i + 1];
  fHandlers[Count] := nil;
  Notify(Self, oldItem, cnRemoved);
end;

function TEventBase.GetHandlers: TArray<TMethodPointer>;
begin
  LockEnter;
  try
    Result := Copy(fHandlers, 0, Count);
  finally
    LockLeave;
  end;
end;

procedure TEventBase.HandleNotification(component: TComponent;
  operation: TOperation);
begin
  if operation = opRemove then
    RemoveAll(component);
end;

procedure TEventBase.Notify(sender: TObject; const item: TMethodPointer;
  action: TCollectionNotification);
var
  data: Pointer;
begin
  data := TMethod(item).Data;
  if SafeIsClass(data, TComponent) then
    case action of
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

  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

procedure TEventBase.Remove(const handler: TMethodPointer);
var
  i: Integer;
begin
  LockEnter;
  try
    for i := 0 to Count - 1 do
      if TMethod(fHandlers[i]) = TMethod(handler) then
      begin
        Delete(i);
        Break;
      end;
  finally
    LockLeave;
  end;
end;

procedure TEventBase.RemoveAll(instance: Pointer);
var
  i: Integer;
begin
  LockEnter;
  try
    for i := Count - 1 downto 0 do
      if TMethod(fHandlers[i]).Data = instance then
        Delete(i);
  finally
    LockLeave;
  end;
end;

procedure TEventBase.SetEnabled(const value: Boolean);
var
  bitMask: Integer;
  oldCount: Integer;
begin
  bitMask := Integer(value) shl 31;
  repeat
    oldCount := fCount;
  until AtomicCmpExchange(fCount, (oldCount or DisabledFlag) xor bitMask, oldCount) = oldCount;
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
