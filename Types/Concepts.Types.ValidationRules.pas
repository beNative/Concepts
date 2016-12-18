{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit Concepts.Types.ValidationRules;

interface

uses
  System.Rtti, System.SysUtils,

  DSharp.Core.Validations;

type
  TRequiredRule = class(TValidationRule)
  public
    function Validate(const Value: TValue): IValidationResult; override;
  end;

  TDelegateRule = class(TValidationRule)
  private
    FDelegate: TFunc<TValue, IValidationResult>;
  public
    constructor Create(ADelegate: TFunc<TValue, IValidationResult>); reintroduce;
    function Validate(const Value: TValue): IValidationResult; override;
  end;

implementation

{ TRequiredRule }

function TRequiredRule.Validate(const Value: TValue): IValidationResult;
begin
  if Value.AsString = '' then
    Exit(TValidationResult.Create(False, 'Value Required!'));

  Result := TValidationResult.ValidResult;
end;

{ TDelegateRule }

constructor TDelegateRule.Create(ADelegate: TFunc<TValue, IValidationResult>);
begin
  FDelegate := ADelegate;
end;

function TDelegateRule.Validate(const Value: TValue): IValidationResult;
begin
  Result := FDelegate(Value);
end;

end.
