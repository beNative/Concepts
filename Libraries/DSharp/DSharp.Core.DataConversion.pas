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

unit DSharp.Core.DataConversion;

interface

uses
  Rtti,
  TypInfo;

type
  TValue = Rtti.TValue;

  IValueConverter = interface
    ['{20006CE1-6C5A-41AF-9E2C-5D625C2BC07D}']
    function Convert(const Value: TValue): TValue;
    function ConvertBack(const Value: TValue): TValue;
  end;

  TValueConverter = class(TInterfacedObject, IValueConverter)
  public
    function Convert(const Value: TValue): TValue; virtual;
    function ConvertBack(const Value: TValue): TValue; virtual;
  end;

  ValueConversionAttribute = class(TCustomAttribute)
  private
    FSourceType: PTypeInfo;
    FTargetType: PTypeInfo;
  public
    constructor Create(ASourceType, ATargetType: PTypeInfo);
    property SourceType: PTypeInfo read FSourceType;
    property TargetType: PTypeInfo read FTargetType;
  end;

implementation

{ TValueConverter }

function TValueConverter.Convert(const Value: TValue): TValue;
begin
  Result := Value;
end;

function TValueConverter.ConvertBack(const Value: TValue): TValue;
begin
  Result := Value;
end;

{ ValueConversionAttribute }

constructor ValueConversionAttribute.Create(ASourceType,
  ATargetType: PTypeInfo);
begin
  FSourceType := ASourceType;
  FTargetType := ATargetType;
end;

end.
