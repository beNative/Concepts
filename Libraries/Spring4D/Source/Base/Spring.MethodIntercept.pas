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

unit Spring.MethodIntercept;

interface

uses
  Rtti;

type
  TMethodIntercept = class
  private
    fImplementation: TMethodImplementation;
    fMethod: TRttiMethod;
    function GetCodeAddress: Pointer;
    function GetVirtualIndex: SmallInt;
  public
    constructor Create(const method: TRttiMethod;
      const callback: TMethodImplementationCallback);
    destructor Destroy; override;
    property CodeAddress: Pointer read GetCodeAddress;
    property Method: TRttiMethod read fMethod;
    property VirtualIndex: SmallInt read GetVirtualIndex;
  end;

implementation


{$REGION 'TMethodIntercept'}

constructor TMethodIntercept.Create(const method: TRttiMethod;
  const callback: TMethodImplementationCallback);
begin
  fImplementation := method.CreateImplementation(Self, callback);
  fMethod := method;
end;

destructor TMethodIntercept.Destroy;
begin
  fImplementation.Free;
  inherited Destroy;
end;

function TMethodIntercept.GetCodeAddress: Pointer;
begin
  Result := fImplementation.CodeAddress;
end;

function TMethodIntercept.GetVirtualIndex: SmallInt;
begin
  Result := fMethod.VirtualIndex;
end;

{$ENDREGION}


end.
