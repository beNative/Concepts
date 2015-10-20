(*
  Copyright (c) 2011-2012, Stefan Glienke
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

unit DSharp.Collections.ObservableCollection;

interface

uses
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  DSharp.Core.Utils,
  Spring,
  Spring.Collections,
  Spring.Collections.Lists;

type
  TObservableCollection<T: class> = class(TObjectList<T>,
    INotifyCollectionChanged, INotifyPropertyChanged)
  private
    FOnPropertyChanged: Event<TPropertyChangedEvent>;
    function GetOnCollectionChanged: IEvent<TCollectionChangedEvent>;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
  protected
    procedure DoItemPropertyChanged(ASender: TObject;
      const AEventArgs: IPropertyChangedEventArgs);
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure Changed(const Value: T; Action: TCollectionChangedAction); override;
  public
    property OnPropertyChanged: IEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

implementation

{ TObservableCollection<T> }

procedure TObservableCollection<T>.DoItemPropertyChanged(ASender: TObject;
  const AEventArgs: IPropertyChangedEventArgs);
begin
  inherited Changed(T(ASender), caReplaced);
end;

procedure TObservableCollection<T>.DoPropertyChanged(
  const APropertyName: string; AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self,
    TPropertyChangedEventArgs.Create(APropertyName) as IPropertyChangedEventArgs);
end;

function TObservableCollection<T>.GetOnCollectionChanged: IEvent<TCollectionChangedEvent>;
begin
  ICollectionChangedEvent<T>(Result) := OnChanged;
end;

function TObservableCollection<T>.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged;
end;

procedure TObservableCollection<T>.Changed(const Value: T; Action: TCollectionChangedAction);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  if Supports(TObject(Value), INotifyPropertyChanged, LNotifyPropertyChanged) then
  begin
    LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
    case Action of
      caAdded: LPropertyChanged.Add(DoItemPropertyChanged);
      caRemoved, caExtracted: LPropertyChanged.Remove(DoItemPropertyChanged);
    end;
  end;

  inherited;
  DoPropertyChanged('Count');
end;

end.
