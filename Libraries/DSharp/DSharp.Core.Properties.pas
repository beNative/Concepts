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

unit DSharp.Core.Properties;

interface

uses
  Classes,
  DSharp.Core.CopyOperator,
  Spring,
  SysUtils;

type
  TProperty<T> = record
  private
    FOnChange: IEvent<TNotifyEvent>;
    FOwner: TObject;
    FValue: T;
    // this needs to be the last field so the other values
    // are already copied when the CopyOperator mechanic fires
    FCopyOperator: ICopyOperator;
    procedure DoChange;
    function GetValue: T;
    procedure SetValue(const AValue: T);
  public
    class operator Implicit(AValue: T): TProperty<T>;
    class operator Implicit(AValue: TProperty<T>): T;

    function ToString: string;
    property OnChange: IEvent<TNotifyEvent> read FOnChange;
    property Value: T read GetValue write SetValue;
  end;

  TField<T> = record
  private
    FEvent: Event<TNotifyEvent>;
  public
    Value: TProperty<T>;
    procedure Initialize(Owner: TObject);
    procedure Finalize;
    class operator Implicit(AValue: T): TField<T>;
    class operator Implicit(AValue: TField<T>): T;
  end;

  TPropertyCopyOperator<T> = class(TCopyOperator<TProperty<T>>)
  private
    FEventHandler: IEvent<TNotifyEvent>;
    FOwner: TObject;
  protected
    procedure Copy; override;
    procedure Initialize; override;
  end;

implementation

uses
  Rtti;

{ TProperty<T> }

procedure TProperty<T>.DoChange;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange.Invoke(FOwner);
  end;
end;

function TProperty<T>.GetValue: T;
begin
  if Assigned(FCopyOperator) and FCopyOperator.IsValid then
  begin
    Result := TProperty<T>(FCopyOperator.GetInstance^).FValue;
  end
  else
  begin
    raise EInvalidOperation.Create('property reference is invalid');
  end;
end;

procedure TProperty<T>.SetValue(const AValue: T);
begin
  FValue := AValue;
  if Assigned(FCopyOperator) then
  begin
    TProperty<T>(FCopyOperator.GetInstance^).FValue := AValue;
  end;
  DoChange();
end;

function TProperty<T>.ToString: string;
begin
  Result := TValue.From<T>(FValue).ToString();
end;

class operator TProperty<T>.Implicit(AValue: T): TProperty<T>;
begin
  Result.Value := AValue;
end;

class operator TProperty<T>.Implicit(AValue: TProperty<T>): T;
begin
  Result := AValue.Value;
end;

{ TField<T> }

procedure TField<T>.Finalize;
begin
  if Assigned(Value.FCopyOperator) then
  begin
    Value.FCopyOperator.Finalize();
  end;
end;

procedure TField<T>.Initialize(Owner: TObject);
begin
  Value.FOnChange := FEvent;
  Value.FOwner := Owner;
  Value.FCopyOperator := TPropertyCopyOperator<T>.Create(@Value);
end;

class operator TField<T>.Implicit(AValue: T): TField<T>;
begin
  Result.Value.Value := AValue
end;

class operator TField<T>.Implicit(AValue: TField<T>): T;
begin
  Result := AValue.Value;
end;

{ TPropertyCopyOperator<T> }

procedure TPropertyCopyOperator<T>.Copy;
begin
  if FInstance.FCopyOperator = nil then
  begin
    FInstance.FCopyOperator := Self;
    FInstance.FOnChange := FEventHandler;
    FInstance.FOwner := FOwner;
    FInstance.DoChange();
  end
  else
  begin
    if (FInstance.FCopyOperator as TObject) <> Self then
    begin
      FInstance.FOwner := FOwner;
      FInstance.DoChange();
      FInstance.FCopyOperator := TPropertyCopyOperator<T>.Create(FInstance);
    end;
  end;
end;

procedure TPropertyCopyOperator<T>.Initialize;
begin
  FEventHandler := FInstance.FOnChange;
  FOwner := FInstance.FOwner;
end;

end.
