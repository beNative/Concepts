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

unit DSharp.Bindings.Validations;

interface

uses
  DSharp.Core.DependencyProperty,
  DSharp.Core.Validations,
  Spring.Collections,
  Rtti;

type
  TDataErrorValidationRule = class(TValidationRule)
  public
    constructor Create; override;
    function Validate(const Value: TValue): IValidationResult; override;
  end;

  Validation = class
  private
    class var
      FErrors: DependencyProperty<IList<IValidationResult>>;
    class function GetHasError(Instance: TObject): Boolean; static;
  public
    class constructor Create;

    class property Errors: DependencyProperty<IList<IValidationResult>> read FErrors;
    class property HasError[Instance: TObject]: Boolean read GetHasError;
  end;

implementation

uses
  Classes,
  DSharp.Bindings,
  DSharp.ComponentModel.DataAnnotations,
  DSharp.Core.Reflection,
  DSharp.Core.Utils;

{ TDataErrorValidationRule }

constructor TDataErrorValidationRule.Create;
begin
  ValidationStep := vsCommittedValue;
end;

function TDataErrorValidationRule.Validate(const Value: TValue): IValidationResult;
var
  LBinding: TBinding;
  LBindingGroup: TBindingGroup;
  LItems: IList<TObject>;
  LItem: TObject;
  i: Integer;
  LInfo: IDataErrorInfo;
  LError: string;
  LName: string;
  LMember: TRttiMember;
  LAttribute: ValidationAttribute;
  LValidationContext: TValidationContext;
begin
  Result := TValidationResult.ValidResult;

  if Value.IsType<TBindingGroup> then
  begin
    LBindingGroup := Value.AsType<TBindingGroup>;
    if Assigned(LBindingGroup) then
    begin
      LItems := LBindingGroup.Items;

      for i := 0 to Pred(LItems.Count) do
      begin
        if Supports(LItems[i], IDataErrorInfo, LInfo) then
        begin
          LError := LInfo.Error;
          if LError <> '' then
          begin
            Result := TValidationResult.Create(False, LError);
          end;
        end;
      end;
    end;
  end
  else if Value.IsType<TBinding> then
  begin
    LBinding := Value.AsType<TBinding>;
    if Assigned(LBinding) then
    begin
      if Supports(LBinding.Source, IDataErrorInfo, LInfo) then
      begin
        LName := LBinding.SourcePropertyName;
        if LName <> '' then
        begin
          LError := LInfo[LName];
          if LError <> '' then
          begin
            Result := TValidationResult.Create(False, LError);
          end;
        end;
      end
      else
      begin
        LName := LBinding.SourcePropertyName;
        if Assigned(LBinding.Source) and Assigned(LBinding.SourceProperty)
          and LBinding.Source.TryGetMember(LName, LMember) then
        begin
          LValidationContext := TValidationContext.Create(LBinding.Source);
          LValidationContext.MemberName := LMember.Name;
          for LAttribute in LMember.GetCustomAttributes<ValidationAttribute> do
          begin
            Result := LAttribute.IsValid(LBinding.SourceProperty.Value, LValidationContext);
            if not Result.IsValid then
              Break;
          end;
        end;
      end;
    end;
  end
  else if Value.IsObject then
  begin
    LItem := Value.AsObject();
    if Assigned(LItem) then
    begin
      if Supports(LItem, IDataErrorInfo, LInfo) then
      begin
        LError := LInfo.Error;
        if LError <> '' then
        begin
          Result := TValidationResult.Create(False, LError);
        end;
      end;
    end;
  end;
end;

{ Validation }

class constructor Validation.Create;
begin
  TRttiPropertyExtension.Create(Validation.ClassInfo, 'HasError', TypeInfo(Boolean)).Getter :=
    function(Instance: Pointer): TValue
    begin
      Result := Validation.GetHasError(Instance);
    end;
  FErrors := TDependencyProperty.RegisterAttached('Errors', TypeInfo(IList<IValidationResult>), Validation,
    TPropertyMetadata.Create(
    procedure(Sender: TComponent; EventArgs: IDependencyPropertyChangedEventArgs)
    begin
      NotifyPropertyChanged(Sender, 'Validation.Errors');
      NotifyPropertyChanged(Sender, 'Validation.HasError');
    end));
end;

class function Validation.GetHasError(Instance: TObject): Boolean;
var
  LErrors: IList<IValidationResult>;
begin
  LErrors := Validation.Errors.GetValue(Instance as TComponent);
  Result := Assigned(LErrors) and (LErrors.Count > 0);
end;

end.
