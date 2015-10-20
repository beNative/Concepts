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

unit DSharp.Core.CopyOperator;

interface

type
  ICopyOperator = interface
    ['{4BDF3273-9E71-4C1E-929D-0673A353BEE0}']
    function GetInstance: Pointer;
    procedure Finalize;
    function IsValid: Boolean;
  end;

  TCopyOperator<T: record> = class abstract(TInterfacedObject, IInterface, ICopyOperator)
  protected
    FFinalized: Boolean;
    FInstance: ^T;

    function _Release: Integer; stdcall;

    function GetInstance: Pointer;

    procedure Copy; virtual; abstract;
    procedure Finalize; virtual;
    procedure Initialize; virtual; abstract;
    function IsValid: Boolean;
  public
    constructor Create(AValue: Pointer);
  end;

implementation

{ TCopyOperator<T> }

constructor TCopyOperator<T>.Create(AValue: Pointer);
begin
  FInstance := AValue;
  Initialize();
end;

procedure TCopyOperator<T>.Finalize;
begin
  FFinalized := True;
  FInstance^ := Default(T);
end;

function TCopyOperator<T>.GetInstance: Pointer;
begin
  Result := FInstance;
end;

function TCopyOperator<T>.IsValid: Boolean;
begin
  Result := not FFinalized;
end;

function TCopyOperator<T>._Release: Integer;
begin
  if not FFinalized then
  begin
    Copy();
  end;

  Result := inherited;
end;

end.
