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

unit Concepts.System.Interfaces.WeakReferences;

interface

uses
  System.Classes,

  Spring,

  Concepts.System.Interfaces.Interfaces,
  Concepts.System.Interfaces.InterfacedObject;

type
  TInterfacedObject =
    Concepts.System.Interfaces.InterfacedObject.TMyInterfacedObject;

  TObject1 = class(TInterfacedObject, IInterface1)
  private
    FWeakInterface2 : Weak<IInterface2>;
    FInterface2     : IInterface2;
    FUseWeak        : Boolean;

    function GetInterfaceReference: IInterface2;
    procedure SetInterfaceReference(const AValue: IInterface2);

  protected
    property InterfaceReference : IInterface2
      read GetInterfaceReference write SetInterfaceReference;

  public
    constructor Create(
      AIsRefCounted     : Boolean;
      AUseWeakReference : Boolean = False;
      AOnAddRef         : TRefCountEvent = nil;
      AOnRelease        : TRefCountEvent = nil;
      AOnQueryInterface : TNotifyEvent = nil;
      AOnDestroy        : TNotifyEvent = nil
    ); reintroduce; virtual;

  end;

  TObject2 = class(TInterfacedObject, IInterface2)
  private
    FWeakInterface1 : Weak<IInterface1>;
    FInterface1     : IInterface1;
    FUseWeak        : Boolean;

    function GetInterfaceReference: IInterface1;
    procedure SetInterfaceReference(const AValue: IInterface1);

  protected
    property InterfaceReference : IInterface1
      read GetInterfaceReference write SetInterfaceReference;

  public
    constructor Create(
      AIsRefCounted     : Boolean;
      AUseWeakReference : Boolean = False;
      AOnAddRef         : TRefCountEvent = nil;
      AOnRelease        : TRefCountEvent = nil;
      AOnQueryInterface : TNotifyEvent = nil;
      AOnDestroy        : TNotifyEvent = nil
    ); reintroduce; virtual;

  end;

implementation

{ TObject1 }

constructor TObject1.Create(AIsRefCounted, AUseWeakReference: Boolean;
  AOnAddRef, AOnRelease: TRefCountEvent; AOnQueryInterface,
  AOnDestroy: TNotifyEvent);
begin
  inherited Create(
    AIsRefCounted, AOnAddRef, AOnRelease, AOnQueryInterface, AOnDestroy
  );
  FUseWeak := AUseWeakReference;
end;

function TObject1.GetInterfaceReference: IInterface2;
begin
  if FUseWeak then
    Result := FWeakInterface2.Target
  else
    Result := FInterface2;
end;

procedure TObject1.SetInterfaceReference(const AValue: IInterface2);
begin
  if FUseWeak then
    FWeakInterface2 := AValue
  else
    FInterface2 := AValue;
end;

{ TObject2 }

constructor TObject2.Create(AIsRefCounted, AUseWeakReference: Boolean;
  AOnAddRef, AOnRelease: TRefCountEvent; AOnQueryInterface,
  AOnDestroy: TNotifyEvent);
begin
  inherited Create(
    AIsRefCounted, AOnAddRef, AOnRelease, AOnQueryInterface, AOnDestroy
  );
  FUseWeak := AUseWeakReference;
end;

function TObject2.GetInterfaceReference: IInterface1;
begin
  if FUseWeak then
    Result := FWeakInterface1.Target
  else
    Result := FInterface1;
end;

procedure TObject2.SetInterfaceReference(const AValue: IInterface1);
begin
  if FUseWeak then
    FWeakInterface1 := AValue
  else
    FInterface1 := AValue;
end;

end.
