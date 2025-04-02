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

unit Spring.Collections.Events;

interface

uses
  Classes,
  Spring,
  Spring.Collections,
  Spring.Events.Base;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  TCollectionChangedEventImpl<T> = class(TEventBase, IEvent, ICollectionChangedEvent<T>)
{$IFDEF INVOKABLE_EVENT}
  private
    function GetInvoke: TCollectionChangedEvent<T>;
{$ENDIF}
  public
    procedure AfterConstruction; override;
    procedure Free;
    procedure Add(handler: TCollectionChangedEvent<T>); overload; inline;
    procedure Remove(handler: TCollectionChangedEvent<T>); overload; inline;
    procedure Invoke(Sender: TObject; const Item: T; Action: TCollectionChangedAction);
    property OnChanged: TNotifyEvent write fOnChanged;
  end;

implementation

uses
  Spring.HazardEra;


{$REGION 'TCollectionChangedEventImpl<T>'}

procedure TCollectionChangedEventImpl<T>.AfterConstruction;
begin
  TCollectionChangedEvent<T>(fInvoke) := Invoke;
end;

procedure TCollectionChangedEventImpl<T>.Free;
begin
  if Assigned(Self) then
    _Release;
end;

procedure TCollectionChangedEventImpl<T>.Add(
  handler: TCollectionChangedEvent<T>);
begin
  inherited Add(TMethod(handler));
end;

{$IFDEF INVOKABLE_EVENT}
function TCollectionChangedEventImpl<T>.GetInvoke: TCollectionChangedEvent<T>;
begin
  Result := TCollectionChangedEvent<T>(fInvoke);
end;
{$ENDIF}

procedure TCollectionChangedEventImpl<T>.Invoke(Sender: TObject;
  const Item: T; Action: TCollectionChangedAction);
var
  guard: GuardedPointer;
  handlers: PMethodArray;
  i: Integer;
begin
  if CanInvoke then
  begin
    guard := GetHandlers;
    handlers := guard;
    try
      for i := 0 to DynArrayHigh(handlers) do
        TCollectionChangedEvent<T>(handlers[i])(Sender, Item, Action);
    finally
      guard.Release;
    end;
  end;
end;

procedure TCollectionChangedEventImpl<T>.Remove(
  handler: TCollectionChangedEvent<T>);
begin
  inherited Remove(TMethod(handler));
end;

{$ENDREGION}


end.
