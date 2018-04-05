{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Interception.AbstractInvocation;

interface

uses
  Rtti,
  Spring,
  Spring.Interception;

type
  TAbstractInvocation = class(TInterfacedObject, IInvocation)
  private
  {$REGION 'Property Accessors'}
    function GetArguments: TArray<TValue>;
    function GetMethod: TRttiMethod;
    function GetResult: TValue;
    function GetTarget: TValue;
    procedure SetResult(const value: TValue);
  {$ENDREGION}
  protected
    fArguments: TArray<TValue>;
    fCurrentInterceptorIndex: Integer;
    fInterceptors: TArray<IInterceptor>;
    fMethod: TRttiMethod;
    fResult: TValue;
    fTarget: TValue;
    procedure InvokeMethodOnTarget; virtual; abstract;
  public
    constructor Create(const target: TValue;
      const interceptors: TArray<IInterceptor>; const method: TRttiMethod;
      const arguments: TArray<TValue>);

    procedure Proceed;

    property Arguments: TArray<TValue> read GetArguments;
    property Method: TRttiMethod read GetMethod;
    property Result: TValue read GetResult;
    property Target: TValue read GetTarget;
  end;

implementation

uses
  Spring.Interception.ResourceStrings;


{$REGION 'TAbstractInvocation'}

constructor TAbstractInvocation.Create(const target: TValue;
  const interceptors: TArray<IInterceptor>; const method: TRttiMethod;
  const arguments: TArray<TValue>);
begin
  inherited Create;
  fArguments := arguments;
  fCurrentInterceptorIndex := -1;
  fInterceptors := interceptors;
  fMethod := method;
  fTarget := target;
end;

function TAbstractInvocation.GetArguments: TArray<TValue>;
begin
  Result := fArguments;
end;

function TAbstractInvocation.GetMethod: TRttiMethod;
begin
  Result := fMethod;
end;

function TAbstractInvocation.GetResult: TValue;
begin
  Result := fResult;
end;

function TAbstractInvocation.GetTarget: TValue;
begin
  Result := fTarget;
end;

procedure TAbstractInvocation.Proceed;
begin
  if not Assigned(fInterceptors) then
    InvokeMethodOnTarget
  else
  begin
    Inc(fCurrentInterceptorIndex);
    try
      if fCurrentInterceptorIndex = Length(fInterceptors) then
        InvokeMethodOnTarget
      else if fCurrentInterceptorIndex > Length(fInterceptors) then
        raise EInvalidOperationException.CreateRes(@SProceedCalledMoreTimesThanExpected)
      else
        fInterceptors[fCurrentInterceptorIndex].Intercept(Self);
    finally
      Dec(fCurrentInterceptorIndex);
    end;
  end;
end;

procedure TAbstractInvocation.SetResult(const value: TValue);
begin
  fResult := value;
end;

{$ENDREGION}


end.
