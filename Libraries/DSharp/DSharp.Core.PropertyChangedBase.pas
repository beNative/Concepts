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

unit DSharp.Core.PropertyChangedBase;

interface

uses
  Classes,
  DSharp.Bindings.Notifications,
  Spring;

type
  TPropertyChangedBase = class abstract(TPersistent, IInterface,
    INotifyPropertyChanged, INotifyPropertyChangedEx)
  private
    FPropertyChanged: Event<TPropertyChangedEvent>;
    function GetOnPropertyChanged: IPropertyChangedEvent;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    procedure NotifyOfPropertyChange(const APropertyName: string);
  end;

implementation

{ TPropertyChangedBase }

function TPropertyChangedBase.GetOnPropertyChanged: IPropertyChangedEvent;
begin
  Result := FPropertyChanged;
end;

procedure TPropertyChangedBase.NotifyOfPropertyChange(const APropertyName: string);
begin
  FPropertyChanged.Invoke(Self, TPropertyChangedEventArgsEx.Create(APropertyName,
    utPropertyChanged) as IPropertyChangedEventArgs);
end;

function TPropertyChangedBase.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TPropertyChangedBase._AddRef: Integer;
begin
  Result := -1;
end;

function TPropertyChangedBase._Release: Integer;
begin
  Result := -1;
end;

end.
