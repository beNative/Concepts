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

unit Spring.Data.ValueConverters;

interface

uses
  Spring,
  Spring.ValueConverters;

type
  TBcdToVariantConverter = class(TValueConverter)
  public
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  TVariantToBcdConverter = class(TValueConverter)
  public
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

implementation

uses
  FmtBcd;


{$REGION 'TBcdToVariantConverter'}

function TBcdToVariantConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Variant>(VarFMTBcdCreate(value.AsType<TBcd>));
end;

{$ENDREGION}


{$REGION 'TVariantToBcdConverter'}

function TVariantToBcdConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TBcd>(VarToBcd(value.AsVariant));

end;

{$ENDREGION}


procedure RegisterConverters;
begin
  TValueConverterFactory.RegisterConverter(TypeInfo(TBcd), TypeInfo(Variant), TBcdToVariantConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(Variant), TypeInfo(TBcd), TVariantToBcdConverter);
end;

initialization
  RegisterConverters;

end.
