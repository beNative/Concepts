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

unit DSharp.Core.Validations;

interface

uses
  Spring.Collections,
  Rtti,
  SysUtils;

type
  TValidationStep = (
    vsRawProposedValue,
    vsConvertedProposedValue,
    vsUpdatedValue,
    vsCommittedValue
  );

  IValidationResult = interface
    function GetErrorContent: string;
    function GetIsValid: Boolean;
    property ErrorContent: string read GetErrorContent;
    property IsValid: Boolean read GetIsValid;
  end;

  TValidationResult = class(TInterfacedObject, IValidationResult)
  strict private
    FErrorContent: string;
    FIsValid: Boolean;
  private
    function GetErrorContent: string;
    function GetIsValid: Boolean;
  public
    constructor Create(AIsValid: Boolean; AErrorContent: string);
    class function ValidResult: TValidationResult; static;
    property ErrorContent: string read GetErrorContent;
    property IsValid: Boolean read GetIsValid;
  end;

  IValidationRule = interface
    function GetValidatesOnTargetUpdated: Boolean;
    function GetValidationStep: TValidationStep;
    procedure SetValidatesOnTargetUpdated(const Value: Boolean);
    procedure SetValidationStep(const Value: TValidationStep);
    function Validate(const Value: TValue): IValidationResult;
    property ValidatesOnTargetUpdated: Boolean
      read GetValidatesOnTargetUpdated write SetValidatesOnTargetUpdated;
    property ValidationStep: TValidationStep
      read GetValidationStep write SetValidationStep;
  end;

  TValidationRule = class abstract(TInterfacedObject, IValidationRule)
  private
    FValidatesOnTargetUpdated: Boolean;
    FValidationStep: TValidationStep;
    function GetValidatesOnTargetUpdated: Boolean;
    function GetValidationStep: TValidationStep;
    procedure SetValidatesOnTargetUpdated(const Value: Boolean);
    procedure SetValidationStep(const Value: TValidationStep);
  public
    constructor Create; virtual;
    function Validate(const Value: TValue): IValidationResult; virtual;

    property ValidatesOnTargetUpdated: Boolean
      read GetValidatesOnTargetUpdated write SetValidatesOnTargetUpdated;
    property ValidationStep: TValidationStep
      read GetValidationStep write SetValidationStep;
  end;

  TValidationRuleClass = class of TValidationRule;

  TValidationEvent = procedure(Sender: TObject; AValidationRule: IValidationRule;
    AValidationResult: IValidationResult) of object;

  IValidatable = interface
    ['{9D8703D8-CC3A-476A-B6C7-8693B05F2C6E}']
    function GetValidationErrors: IList<IValidationResult>;
    function Validate: Boolean;
    property ValidationErrors: IList<IValidationResult> read GetValidationErrors;
  end;

  IDataErrorInfo = interface
    ['{E8216DF2-CFF7-4C61-9A82-20AAB177D204}']
    function GetError: string;
    function GetItem(const Name: string): string;
    property Error: string read GetError;
    property Item[const Name: string]: string read GetItem; default;
  end;

implementation

{ TValidationResult }

constructor TValidationResult.Create(AIsValid: Boolean; AErrorContent: string);
begin
  FIsValid := AIsValid;
  FErrorContent := AErrorContent;
end;

function TValidationResult.GetErrorContent: string;
begin
  Result := FErrorContent;
end;

function TValidationResult.GetIsValid: Boolean;
begin
  Result := FIsValid;
end;

class function TValidationResult.ValidResult: TValidationResult;
begin
  Result := TValidationResult.Create(True, '');
end;

{ TValidationRule }

constructor TValidationRule.Create;
begin

end;

function TValidationRule.GetValidatesOnTargetUpdated: Boolean;
begin
  Result := FValidatesOnTargetUpdated;
end;

function TValidationRule.GetValidationStep: TValidationStep;
begin
  Result := FValidationStep;
end;

procedure TValidationRule.SetValidatesOnTargetUpdated(const Value: Boolean);
begin
  FValidatesOnTargetUpdated := Value;
end;

procedure TValidationRule.SetValidationStep(const Value: TValidationStep);
begin
  FValidationStep := Value;
end;

function TValidationRule.Validate(const Value: TValue): IValidationResult;
begin
  Result := TValidationResult.ValidResult();
end;

end.
