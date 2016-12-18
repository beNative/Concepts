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

unit DSharp.ComponentModel.DataAnnotations;

interface

uses
  DSharp.Core.Validations,
  Rtti;

type
  TValidationContext = record
  private
    FMemberName: string;
    FObjectInstance: TObject;
    FObjectType: TRttiType;
  public
    constructor Create(Instance: TObject);

    property MemberName: string read FMemberName write FMemberName;
    property ObjectInstance: TObject read FObjectInstance;
    property ObjectType: TRttiType read FObjectType;
  end;

  ValidationAttribute = class(TCustomAttribute)
  private
    FErrorMessage: string;
  public
    constructor Create(const ErrorMessage: string); overload;
    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; virtual;
    property ErrorMessage: string read FErrorMessage;
  end;

  RangeAttribute = class(ValidationAttribute)
  private
    FMinimum: TValue;
    FMaximum: TValue;
  public
    constructor Create(Minimum, Maximum: Double); overload;
    constructor Create(Minimum, Maximum: Integer); overload;
    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; override;
  end;

  RequiredAttribute = class(ValidationAttribute)
  public
    constructor Create; overload;
    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; override;
  end;

implementation

uses
  DSharp.Core.Reflection,
  Math,
  SysUtils;

resourcestring
  SRangeAttribute_TypeNotNumeric = 'The type %s must be numeric.';
  SRangeAttribute_ValidationError = 'The field %s must be between %s and %s.';
  SRequiredAttribute_ValidationError = 'The %s field is required.';

{ TValidationContext }

constructor TValidationContext.Create(Instance: TObject);
begin
  FObjectInstance := Instance;
  FObjectType := GetRttiType(Instance.ClassInfo);
end;

{ ValidationAttribute }

constructor ValidationAttribute.Create(const ErrorMessage: string);
begin
  FErrorMessage := ErrorMessage;
end;

function ValidationAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
begin
  Result := TValidationResult.ValidResult;
end;

{ RangeAttribute }

constructor RangeAttribute.Create(Minimum, Maximum: Double);
begin
  FMinimum := Minimum;
  FMaximum := Maximum;
end;

constructor RangeAttribute.Create(Minimum, Maximum: Integer);
begin
  FMinimum := Minimum;
  FMaximum := Maximum;
end;

function RangeAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
begin
  Result := TValidationResult.ValidResult;

  if Value.IsOrdinal then
  begin
    if not InRange(Value.AsOrdinal, FMinimum.AsOrdinal, FMaximum.AsOrdinal) then
    begin
      Result := TValidationResult.Create(False,
        Format(SRangeAttribute_ValidationError, [ValidationContext.MemberName,
        FMinimum.ToString, FMaximum.ToString]));
    end;
  end else
  if Value.IsFloat then
  begin
    if not InRange(Value.AsFloat, FMinimum.AsFloat, FMaximum.AsFloat) then
    begin
      Result := TValidationResult.Create(False,
        Format(SRangeAttribute_ValidationError, [ValidationContext.MemberName,
        FMinimum.ToString, FMaximum.ToString]));
    end;
  end else
  begin
    Result := TValidationResult.Create(False,
      Format(SRangeAttribute_TypeNotNumeric, [Value.TypeInfo.Name]));
  end;
end;

{ RequiredAttribute }

constructor RequiredAttribute.Create;
begin
  inherited Create(SRequiredAttribute_ValidationError);
end;

function RequiredAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
begin
  if Value.IsString and (Value.AsString = '') then
  begin
    Result := TValidationResult.Create(False, Format(ErrorMessage, [ValidationContext.MemberName]));
  end
  else
  begin
    Result := TValidationResult.ValidResult;
  end;
end;

end.
