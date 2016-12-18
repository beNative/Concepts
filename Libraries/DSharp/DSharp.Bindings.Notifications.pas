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

unit DSharp.Bindings.Notifications;

interface

uses
  Classes,
  Spring;

type
  TUpdateTrigger = (utPropertyChanged, utLostFocus, utExplicit);

const
  UpdateTriggerDefault = utPropertyChanged;

type
  TPropertyChangedEventArgsEx = class(TPropertyChangedEventArgs)
  private
    fUpdateTrigger: TUpdateTrigger;
    function GetUpdateTrigger: TUpdateTrigger;
  public
    constructor Create(const propertyName: string;
      updateTrigger: TUpdateTrigger = UpdateTriggerDefault);

    property UpdateTrigger: TUpdateTrigger read GetUpdateTrigger;
  end;

  INotifyPropertyChangedEx = interface(INotifyPropertyChanged)
    ['{6627279B-8112-4A92-BBD3-795185A41966}']
    procedure NotifyOfPropertyChange(const APropertyName: string);
  end;

  TNotifyPropertyChanged = class sealed(TComponent, INotifyPropertyChanged)
  private
    FOnPropertyChanged: Event<TPropertyChangedEvent>;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
  public
    procedure NotifyOfPropertyChange(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  end;

procedure NotifyPropertyChanged(AObject, ASender: TObject; const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger = utPropertyChanged); overload;

implementation

uses
  DSharp.Core.Utils;

procedure NotifyPropertyChanged(AObject, ASender: TObject; const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  if Supports(AObject, INotifyPropertyChanged, LNotifyPropertyChanged) then
  begin
    LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
    LPropertyChanged.Invoke(ASender, TPropertyChangedEventArgsEx.Create(
      APropertyName, AUpdateTrigger) as IPropertyChangedEventArgs);
  end;
end;

{ TNotifiyPropertyChanged }

function TNotifyPropertyChanged.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged;
end;

procedure TNotifyPropertyChanged.NotifyOfPropertyChange(
  const APropertyName: string; AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Owner, TPropertyChangedEventArgsEx.Create(
    APropertyName, AUpdateTrigger) as IPropertyChangedEventArgs);
end;

{ TPropertyChangedEventArgsEx }

constructor TPropertyChangedEventArgsEx.Create(const propertyName: string;
  updateTrigger: TUpdateTrigger);
begin
  inherited Create(propertyName);
  fUpdateTrigger := updateTrigger;
end;

function TPropertyChangedEventArgsEx.GetUpdateTrigger: TUpdateTrigger;
begin
  Result := fUpdateTrigger;
end;

end.
