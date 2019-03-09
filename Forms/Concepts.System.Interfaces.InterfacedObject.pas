{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{$I Concepts.inc}

unit Concepts.System.Interfaces.InterfacedObject;

interface

uses
  System.Classes,

  Concepts.System.Interfaces.Interfaces;

type
  TMyInterfacedObject = class(TObject, ITrackRefCount, IInterface)
  private
    FIsRefCounted     : Boolean;
    FOnRelease        : TRefCountEvent;
    FOnAddRef         : TRefCountEvent;
    FOnQueryInterface : TNotifyEvent;
    FOnDestroy        : TNotifyEvent;
    FRefCount         : Integer;

    {$REGION 'property access methods'}
    function GetOnAddRef: TRefCountEvent;
    function GetOnRelease: TRefCountEvent;
    procedure SetOnAddRef(const Value: TRefCountEvent);
    procedure SetOnRelease(const Value: TRefCountEvent);
    function GetRefCount: Integer;
    function GetIsRefCounted: Boolean;
    procedure SetIsRefCounted(const Value: Boolean);
    {$ENDREGION}

  protected
    { IRunnable }
    procedure Run(ARaiseException: Boolean = False);

    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    {$REGION 'event dispatch methods'}
    procedure DoAddRef; virtual;
    procedure DoRelease; virtual;
    procedure DoQueryInterface; virtual;
    {$ENDREGION}

  public
    constructor Create(
      AIsRefCounted     : Boolean;
      AOnAddRef         : TRefCountEvent = nil;
      AOnRelease        : TRefCountEvent = nil;
      AOnQueryInterface : TNotifyEvent = nil;
      AOnDestroy        : TNotifyEvent = nil
    ); virtual;

    destructor Destroy; override;

    property RefCount: Integer
      read GetRefCount;

    property IsRefCounted : Boolean
      read GetIsRefCounted write SetIsRefCounted;

    property OnAddRef: TRefCountEvent
      read GetOnAddRef write SetOnAddRef;

    property OnRelease: TRefCountEvent
      read GetOnRelease write SetOnRelease;

    property OnQueryInterface: TNotifyEvent
      read FOnQueryInterface write FOnQueryInterface;
  end;

  TRunnable = class(TMyInterfacedObject, IRunnable)
  protected
    procedure Run(ARaiseException: Boolean = False);
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Dialogs;

{$REGION 'construction and destruction'}
constructor TMyInterfacedObject.Create(AIsRefCounted: Boolean;
  AOnAddRef: TRefCountEvent; AOnRelease: TRefCountEvent;
  AOnQueryInterface, AOnDestroy : TNotifyEvent);
begin
  inherited Create;
  FOnAddRef         := AOnAddRef;
  FOnRelease        := AOnRelease;
  FOnQueryInterface := AOnQueryInterface;
  FOnDestroy        := AOnDestroy;
  FIsRefCounted     := AIsRefCounted;
end;

destructor TMyInterfacedObject.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  FOnAddRef         := nil;
  FOnRelease        := nil;
  FOnQueryInterface := nil;
  FOnDestroy        := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TMyInterfacedObject.GetOnAddRef: TRefCountEvent;
begin
  Result := FOnAddRef;
end;

procedure TMyInterfacedObject.SetOnAddRef(const Value: TRefCountEvent);
begin
  FOnAddRef := Value;
end;

function TMyInterfacedObject.GetOnRelease: TRefCountEvent;
begin
  Result := FOnRelease;
end;

procedure TMyInterfacedObject.SetOnRelease(const Value: TRefCountEvent);
begin
  FOnRelease := Value;
end;

function TMyInterfacedObject.GetRefCount: Integer;
begin
  Result := FRefCount;
end;

function TMyInterfacedObject.GetIsRefCounted: Boolean;
begin
  Result := FIsRefCounted;
end;

procedure TMyInterfacedObject.SetIsRefCounted(const Value: Boolean);
begin
  FIsRefCounted := Value;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TMyInterfacedObject.DoAddRef;
begin
  if Assigned(FOnAddRef) then
    FOnAddRef(Self, FRefCount);
end;

procedure TMyInterfacedObject.DoQueryInterface;
begin
  if Assigned(FOnQueryInterface) then
    FOnQueryInterface(Self);
end;

procedure TMyInterfacedObject.DoRelease;
begin
  if Assigned(FOnRelease) then
    FOnRelease(Self, FRefCount);
end;

procedure TMyInterfacedObject.Run(ARaiseException: Boolean);
begin
  if ARaiseException then
    raise Exception.Create('Exception raised during Execute')
  else
    ShowMessage('Execute');
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TMyInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
  DoQueryInterface;
end;

function TMyInterfacedObject._AddRef: Integer;
begin
  if FIsRefCounted then
    Result := AtomicIncrement(FRefCount)
  else
    Result := -1;
  DoAddRef;
end;

function TMyInterfacedObject._Release: Integer;
begin
  if FIsRefCounted then
  begin
    Result := AtomicDecrement(FRefCount);
    DoRelease;
    if Result = 0 then
    begin
      Destroy;
    end;
  end
  else
  begin
    Result := -1;
    DoRelease;
  end;
end;
{$ENDREGION}

{$REGION 'TRunnable'}
procedure TRunnable.Run(ARaiseException: Boolean);
begin
  if ARaiseException then
    raise Exception.Create('Exception raised during Run')
  else
    ShowMessage('Run');
end;
{$ENDREGION}

end.
