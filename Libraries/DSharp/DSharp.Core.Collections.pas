(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Core.Collections;

interface

uses
  Classes,
  Spring;

type
  TCollectionNotifyEvent<T> = procedure(Sender: TObject; const Item: T;
    Action: TCollectionNotification) of object;

  TCollectionEnumerator<T: TCollectionItem> = class(TCollectionEnumerator)
  public
    function GetCurrent: T; inline;
    property Current: T read GetCurrent;
  end;

  TCollection<T: TCollectionItem> = class(TCollection)
  private
    FOnNotify: Event<TCollectionNotifyEvent<T>>;
  protected
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    function GetOnNotify: IEvent<TCollectionNotifyEvent<T>>;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure SetItem(Index: Integer; Value: T);
  public
    constructor Create;
    function Add: T;
    function GetEnumerator: TCollectionEnumerator<T>;
    function Insert(Index: Integer): T;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property OnNotify: IEvent<TCollectionNotifyEvent<T>> read GetOnNotify;
  end;

  TOwnedCollection<T: TCollectionItem> = class(TCollection<T>)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
  end;

implementation

{ TCollectionEnumerator<T> }

function TCollectionEnumerator<T>.GetCurrent: T;
begin
  Result := T(inherited GetCurrent);
end;

{ TCollection<T> }

function TCollection<T>.Add: T;
begin
  Result := T(inherited Add());
end;

constructor TCollection<T>.Create;
begin
  inherited Create(T);
end;

function TCollection<T>.GetCount: Integer;
begin
  Result := inherited Count;
end;

function TCollection<T>.GetEnumerator: TCollectionEnumerator<T>;
begin
  Result := TCollectionEnumerator<T>.Create(Self);
end;

function TCollection<T>.GetItem(Index: Integer): T;
begin
  Result := T(inherited GetItem(Index));
end;

function TCollection<T>.GetOnNotify: IEvent<TCollectionNotifyEvent<T>>;
begin
  Result := FOnNotify;
end;

function TCollection<T>.Insert(Index: Integer): T;
begin
  Result := T(inherited Insert(Index));
end;

procedure TCollection<T>.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  FOnNotify.Invoke(Self, Item, Action);
end;

procedure TCollection<T>.SetItem(Index: Integer; Value: T);
begin
  inherited SetItem(Index, Value);
end;

{ TOwnedCollection<T> }

constructor TOwnedCollection<T>.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create();
end;

function TOwnedCollection<T>.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

end.
