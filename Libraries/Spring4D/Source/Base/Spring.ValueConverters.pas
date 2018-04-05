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

unit Spring.ValueConverters;

interface

uses
  Generics.Collections,
  Rtti,
  TypInfo;

type

  {$REGION 'IValueConverter'}

  /// <summary>
  ///   Base value converter interface
  /// </summary>
  IValueConverter = interface
    ['{048EF3F0-41B5-4019-9BD6-00B88CAA7275}']

    /// <param name="value">
    ///   Rtti.TValue to convert
    /// </param>
    /// <param name="targetTypeInfo">
    ///   Target Rtti.PTypeInfo structure
    /// </param>
    /// <returns>
    ///   Returns <paramref name="value">converted</paramref> to type pointing
    ///   by <paramref name="targetTypeInfo" /> parameter
    /// </returns>
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue; overload;

    /// <param name="value">
    ///   Rtti.TValue to convert
    /// </param>
    /// <param name="targetTypeInfo">
    ///   Target Rtti.PTypeInfo structure
    /// </param>
    /// <param name="parameter">
    ///   Additional Rtti.TValue formatting parameter, use when possible
    /// </param>
    /// <returns>
    ///   Returns <paramref name="value" /> converted to type pointing by <paramref name="targetTypeInfo" />
    ///    parameter
    /// </returns>
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; overload;

    /// <param name="value">
    ///   Rtti.TValue to convert
    /// </param>
    /// <param name="targetTypeInfo">
    ///   Target Rtti.PTypeInfo structure
    /// </param>
    /// <param name="targetValue">
    ///   Target Rtti.TValue out parameter
    /// </param>
    /// <returns>
    ///   Returns System.Boolean, True if converting with success
    /// </returns>
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; overload;

    /// <param name="value">
    ///   Rtti.TValue to convert
    /// </param>
    /// <param name="targetTypeInfo">
    ///   Target Rtti.PTypeInfo structure
    /// </param>
    /// <param name="targetValue">
    ///   Target Rtti.TValue out parameter
    /// </param>
    /// <param name="parameter">
    ///   Additional Rtti.TValue formatting parameter, use when possible
    /// </param>
    /// <returns>
    ///   Returns System.Boolean, True if converting with success
    /// </returns>
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; overload;
  end;

  {$ENDREGION}


  {$REGION 'TValueConverter'}

  /// <summary>
  ///   Base abstract class provides DefaultConverter as an entry point to the
  ///   user side
  /// </summary>
  TValueConverter = class abstract(TInterfacedObject, IValueConverter)
  private
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue; overload;
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; overload;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; overload;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; overload;
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; virtual; abstract;
  public
    class function Default: IValueConverter; static;
  end;

  TConverterClass = class of TValueConverter;

  {$ENDREGION}


  {$REGION 'TIntegerToStringConverter'}

  /// <summary>
  ///   Simply provides conversion routine between Integer and
  ///   string/UnicodeString
  /// </summary>
  TIntegerToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToIntegerConverter'}

  /// <summary>
  ///   Simply provides conversion routine between string and Integer
  /// </summary>
  TStringToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TIntegerToBooleanConverter'}

  /// <summary>
  ///   Simply provides conversion routine between Integer and Boolean
  /// </summary>
  TIntegerToBooleanConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TBooleanToIntegerConverter'}

  /// <summary>
  ///   Simply provides conversion routine between Boolean and Integer
  /// </summary>
  TBooleanToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TBooleanToStringConverter'}

  /// <summary>
  ///   Simply provides conversion routine between Boolean and string
  /// </summary>
  TBooleanToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToBooleanConverter'}

  /// <summary>
  ///   Simply provides conversion routine between string and Boolean
  /// </summary>
  TStringToBooleanConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TNullableToTypeConverter'}

  /// <summary>
  ///   Provides conversion routine between Nullable(T) and T
  /// </summary>
  /// <remarks>
  ///   Internally it use another Converter to delegate conversion routine if
  ///   necessary
  /// </remarks>
  TNullableToTypeConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TTypeToNullableConverter'}

  /// <summary>
  ///   Provides conversion routine between T and Nullable(T)
  /// </summary>
  /// <remarks>
  ///   Internally it use another Converter to delegate conversion routine if
  ///   necessary
  /// </remarks>
  TTypeToNullableConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TEnumToIntegerConverter'}

  /// <summary>
  ///   Provides conversion routine between enumeration and Integer
  /// </summary>
  TEnumToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TIntegerToEnumConverter'}

  /// <summary>
  ///   Provides conversion routine between Integer and enumeration
  /// </summary>
  TIntegerToEnumConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TEnumToStringConverter'}

  /// <summary>
  ///   Provides conversion routine between enumeration and string
  /// </summary>
  TEnumToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToEnumConverter'}

  /// <summary>
  ///   Provides conversion routine between string and enumeration
  /// </summary>
  TStringToEnumConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TSetToStringConverter'}

  /// <summary>
  ///   Provides conversion routine between set and string
  /// </summary>
  TSetToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToSetConverter'}

  /// <summary>
  ///   Provides conversion routine between string and enumeration
  /// </summary>
  TStringToSetConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TFloatToStringConverter'}

  /// <summary>
  ///   Provides conversion routine between float and string
  /// </summary>
  TFloatToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TFloatToIntegerConverter'}

  /// <summary>
  ///   Provides conversion routine between float and Integer
  /// </summary>
  TFloatToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToFloatConverter'}

  /// <summary>
  ///   Provides conversion routine between string and float
  /// </summary>
  TStringToFloatConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TColorToStringConverter'}

{$IFNDEF SPRING_DISABLE_GRAPHICS}
  /// <summary>
  ///   Provides conversion routine between TColor and string
  /// </summary>
  TColorToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TStringToColorConverter'}

{$IFNDEF SPRING_DISABLE_GRAPHICS}
  /// <summary>
  ///   Provides conversion routine between string and TColor
  /// </summary>
  TStringToColorConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TCurrencyToStringConverter'}

  /// <summary>
  ///   Provides conversion routine between Currency and string
  /// </summary>
  TCurrencyToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToCurrencyConverter'}

  /// <summary>
  ///   Provides conversion routine between string and Currency
  /// </summary>
  TStringToCurrencyConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToDateTimeConverter'}

  /// <summary>
  ///   Provides conversion routine between string and TDateTime
  /// </summary>
  TStringToDateTimeConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToGUIDConverter'}

  /// <summary>
  ///   Provides conversion routine between string and TGUID
  /// </summary>
  TStringToGUIDConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TDateTimeToStringConverter'}

  /// <summary>
  ///   Provides conversion routine between TDateTime and string
  /// </summary>
  TDateTimeToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TDateToStringConverter'}

  /// <summary>
  ///   Provides conversion routine between TDate and string
  /// </summary>
  TDateToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToDateConverter'}

  /// <summary>
  ///   Provides conversion routine between string and TDate
  /// </summary>
  TStringToDateConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TTimeToStringConverter'}

  /// <summary>
  ///   Provides conversion routine between TTime and string
  /// </summary>
  TTimeToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToTimeConverter'}

  /// <summary>
  ///   Provides conversion routine between string and TTime
  /// </summary>
  TStringToTimeConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TObjectToStringConverter'}

  /// <summary>
  ///   Provides conversion routine between TObject and string
  /// </summary>
  TObjectToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TObjectToInterfaceConverter'}

  /// <summary>
  ///   Provides conversion routine between TObject and IInterface
  /// </summary>
  /// <remarks>
  ///   acc. to #82433 TValue.TryAsType(T) raised an AV because ConvClass2Intf
  ///   is wrong
  /// </remarks>
  TObjectToInterfaceConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TInterfaceToObjectConverter'}

  /// <summary>
  ///   Provides conversion routine between TObject and IInterface
  /// </summary>
  TInterfaceToObjectConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TInterfaceToInterfaceConverter'}

  /// <summary>
  ///   Provides conversion routine between Interface and Interface
  /// </summary>
  TInterfaceToInterfaceConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TObjectToClassConverter'}

  /// <summary>
  ///   Provides conversion routine between TObject and TClass
  /// </summary>
  TObjectToClassConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToWStringConverter'}

{$IFNDEF NEXTGEN}
  /// <summary>
  ///   Provides conversion routine between UnicodeString and WideString
  /// </summary>
  /// <remarks>
  ///   acc. to #82487 Rtti.ConvStr2Str is wrong (when cast a unicode string to
  ///   WideString)
  /// </remarks>
  TStringToWStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TWStringToStringConverter'}

{$IFNDEF NEXTGEN}
  /// <summary>
  ///   Provides conversion routine between UnicodeString and WideString
  /// </summary>
  TWStringToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TGUIDToStringConverter}

  /// <summary>
  ///   Provides conversion routine between TGUID and string
  /// </summary>
  TGUIDToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TVariantToGUIDConverter}

  /// <summary>
  ///   Provides conversion routine between Variant and TGUID
  /// </summary>
  TVariantToGUIDConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TByteArrayToGUIDConverter}

  /// <summary>
  ///   Provides conversion routine between TArray<Byte> and TGUID
  /// </summary>
  TByteArrayToGUIDConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TGUIDToByteArrayConverter}

  /// <summary>
  ///   Provides conversion routine between TGUID and TArray<Byte>
  /// </summary>
  TGUIDToByteArrayConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TValueConverterFactory'}

  /// <summary>
  ///   Factory class that brings to live converter which are registered within
  ///   global converter registry scope
  /// </summary>
  TValueConverterFactory = class
  strict private
    type
      /// <summary>
      ///   TypeInfo, TypeKinds registry structure
      /// </summary>
      TConvertedTypeInfo = record
        SourceTypeInfo: PTypeInfo;
        SourceTypeKinds: TTypeKinds;
        TargetTypeInfo: PTypeInfo;
        TargetTypeKinds: TTypeKinds;
      end;

      TTypeMapping<TSource,TTarget> = record
        SourceType: TSource;
        TargetType: TTarget;
{$IFDEF NEXTGEN}
        // Fixes incorrect register argument passing
        Padding: Pointer;
{$ENDIF}
      end;

    class var fTypeInfoToTypeInfoRegistry: TDictionary<TTypeMapping<PTypeInfo,PTypeInfo>, IValueConverter>;
    class var fTypeInfoToTypeKindsRegistry: TDictionary<TTypeMapping<PTypeInfo,TTypeKind>, IValueConverter>;
    class var fTypeKindsToTypeInfoRegistry: TDictionary<TTypeMapping<TTypeKind,PTypeInfo>, IValueConverter>;
    class var fTypeKindsToTypeKindsRegistry: TDictionary<TTypeMapping<TTypeKind,TTypeKind>, IValueConverter>;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure RegisterConverter(
      sourceTypeInfo, targetTypeInfo: PTypeInfo;
      converterClass: TConverterClass); overload; static;
    class procedure RegisterConverter(
      sourceTypeKinds, targetTypeKinds: TTypeKinds;
      converterClass: TConverterClass); overload; static;
    class procedure RegisterConverter(
      sourceTypeKinds: TTypeKinds; targetTypeInfo: PTypeInfo;
      converterClass: TConverterClass); overload; static;
    class procedure RegisterConverter(
      sourceTypeInfo: PTypeInfo; targetTypeKinds: TTypeKinds;
      converterClass: TConverterClass); overload; static;

    class procedure RegisterConverter(
      sourceTypeInfo, targetTypeInfo: PTypeInfo;
      const converter: IValueConverter); overload; static;
    class procedure RegisterConverter(
      sourceTypeKinds, targetTypeKinds: TTypeKinds;
      const converter: IValueConverter); overload; static;
    class procedure RegisterConverter(
      sourceTypeKinds: TTypeKinds; targetTypeInfo: PTypeInfo;
      const converter: IValueConverter); overload; static;
    class procedure RegisterConverter(
      sourceTypeInfo: PTypeInfo; targetTypeKinds: TTypeKinds;
      const converter: IValueConverter); overload; static;

    class function GetConverter(
      sourceTypeInfo, targetTypeInfo: PTypeInfo): IValueConverter; static;
  end;

  {$ENDREGION}


implementation

uses
{$IFNDEF SPRING_DISABLE_GRAPHICS}
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UIConsts,
  System.UITypes,
  {$ELSE}
  Graphics,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
{$ENDIF SPRING_DISABLE_GRAPHICS}
  Math,
  StrUtils,
  SysUtils,
  Types,
  Variants,
  Spring,
  Spring.ResourceStrings,
  Spring.SystemUtils;


function SameTypeInfo(const left, right: PTypeInfo): Boolean;
begin
  Result := left = right;
  if Assigned(left) and Assigned(right) then
    Result := Result or ((left.Kind = right.Kind)
      and (left.TypeName = right.TypeName));
end;

procedure RaiseConvertError(sourceType, targetType: PTypeInfo);
begin
  raise EConvertError.CreateFmt('Trying to convert %s to %s',
    [sourceType.TypeName, targetType.TypeName]);
end;

function BytesToGuidBigEndian(const bytes: TBytes): TGUID;
begin
  Result.D1 := (Swap(Word(PGUID(bytes).D1)) shl 16) or Swap(Word(PGUID(bytes).D1 shr 16));
  Result.D2 := Swap(PGUID(bytes).D2);
  Result.D3 := Swap(PGUID(bytes).D3);
  Result.D4 := PGUID(bytes).D4;
end;

function GuidToBytesBigEndian(const guid: TGUID): TBytes;
begin
  SetLength(Result, 16);
  PGUID(Result).D1 := (Swap(Word(guid.D1)) shl 16) or Swap(Word(guid.D1 shr 16));
  PGUID(Result).D2 := Swap(guid.D2);
  PGUID(Result).D3 := Swap(guid.D3);
  PGUID(Result).D4 := guid.D4;
end;


{$REGION 'DefaultConverter'}

type
  TDefaultConverter = class
  private
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue;
    function ConvertToParam(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean;
    function TryConvertToParam(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean;
  private const
    Vtable: array[0..6] of Pointer =
    (
      @TDefaultConverter.QueryInterface,
      @TDefaultConverter._AddRef,
      @TDefaultConverter._Release,
      @TDefaultConverter.ConvertTo,
      @TDefaultConverter.ConvertToParam,
      @TDefaultConverter.TryConvertTo,
      @TDefaultConverter.TryConvertToParam
    );
    Instance: Pointer = @TDefaultConverter.Vtable;
  end;

function TDefaultConverter.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
end;

function TDefaultConverter._AddRef: Integer;
begin
  Result := -1;
end;

function TDefaultConverter._Release: Integer;
begin
  Result := -1;
end;

function TDefaultConverter.ConvertToParam(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;

  procedure RaiseException;
  begin
    raise Exception.CreateResFmt(@SCouldNotConvertValue,
      [value.TypeInfo.TypeName, targetTypeInfo.TypeName]);
  end;

var
  converter: IValueConverter;
begin
  converter := TValueConverterFactory.GetConverter(value.TypeInfo, targetTypeInfo);
  if Assigned(converter) then
    Result := converter.ConvertTo(value, targetTypeInfo, parameter)
  else
  begin
    // prevent object to Variant cast
    if (value.Kind = tkClass) and (targetTypeInfo.Kind = tkVariant) then
      RaiseException;
    Result := value.Cast(targetTypeInfo);
  end;
end;

function TDefaultConverter.ConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo): TValue;
begin
  Result := ConvertToParam(value, targetTypeInfo, EmptyValue);
end;

function TDefaultConverter.TryConvertToParam(const value: TValue;
  const targetTypeInfo: PTypeInfo;
  out targetValue: TValue;
  const parameter: TValue): Boolean;
var
  converter: IValueConverter;
begin
  converter := TValueConverterFactory.GetConverter(value.TypeInfo, targetTypeInfo);
  Result := Assigned(converter)
    and converter.TryConvertTo(value, targetTypeInfo, targetValue, parameter);
  if not Result then
{$IFNDEF DELPHIXE2_UP}
    // workaround for wrong TValue.TryCast for string to float (it calls ConvStr2Str by mistake)
    if not ((value.Kind in [tkString, tkLString, tkWString, tkUString])
      and (targetTypeInfo.Kind = tkFloat)) then
{$ENDIF}
    Result := value.TryCast(targetTypeInfo, targetValue);
end;

function TDefaultConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo;
  out targetValue: TValue): Boolean;
begin
  Result := TryConvertToParam(value, targetTypeInfo, targetValue, EmptyValue);
end;

{$ENDREGION}


{$REGION 'TValueConverter'}

class function TValueConverter.Default: IValueConverter;
begin
  Pointer(Result) := @TDefaultConverter.Instance;
end;

function TValueConverter.ConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo): TValue;
begin
  Result := DoConvertTo(value, targetTypeInfo, EmptyValue);
end;

function TValueConverter.ConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo;
  const parameter: TValue): TValue;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(value.TypeInfo, 'value.TypeInfo');
  Guard.CheckNotNull(targetTypeInfo, 'targetTypeInfo');
{$ENDIF}

  Result := DoConvertTo(value, targetTypeInfo, parameter);
end;

function TValueConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Result := TryConvertTo(value, targetTypeInfo, targetValue, EmptyValue);
end;

function TValueConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  try
    targetValue := DoConvertTo(value, targetTypeInfo, parameter);
    Result := True;
  except
    Result := False;
  end;
end;

{$ENDREGION}


{$REGION 'TIntegerToStringConverter'}

function TIntegerToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(IntToStr(value.AsInteger));
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(IntToStr(value.AsInteger)));
    tkWString:
      Result := TValue.From<WideString>(IntToStr(value.AsInteger));
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TStringToIntegerConverter'}

function TStringToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  if targetTypeInfo = TypeInfo(Integer) then
    Result := TValue.From<Integer>(StrToInt(value.AsString))
  else if targetTypeInfo = TypeInfo(Cardinal) then
    Result := TValue.From<Cardinal>(StrToInt64(value.AsString))
  else if targetTypeInfo = TypeInfo(Int64) then
    Result := TValue.From<Int64>(StrToInt64(value.AsString))
  else if targetTypeInfo = TypeInfo(UInt64) then
    Result := TValue.From<UInt64>(StrToInt64(value.AsString))
  else if targetTypeInfo = TypeInfo(SmallInt) then
    Result := TValue.From<SmallInt>(StrToInt(value.AsString))
  else if targetTypeInfo = TypeInfo(Word) then
    Result := TValue.From<Word>(StrToInt(value.AsString))
  else if targetTypeInfo = TypeInfo(ShortInt) then
    Result := TValue.From<ShortInt>(StrToInt(value.AsString))
  else if targetTypeInfo = TypeInfo(Byte) then
    Result := TValue.From<Byte>(StrToInt(value.AsString))
  else
    RaiseConvertError(value.TypeInfo, targetTypeInfo);
end;

{$ENDREGION}


{$REGION 'TBooleanToStringConverter'}

function TBooleanToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(BoolToStr(value.AsBoolean, True));
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(BoolToStr(value.AsBoolean, True)));
    tkWString:
      Result := TValue.From<WideString>(BoolToStr(value.AsBoolean, True));
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TStringToBooleanConverter'}

function TStringToBooleanConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Boolean>(StrToBool(value.AsString));
end;

{$ENDREGION}


{$REGION 'TBooleanToIntegerConverter'}

function TBooleanToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  if targetTypeInfo = TypeInfo(Integer) then
    Result := TValue.From<Integer>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(Cardinal) then
    Result := TValue.From<Cardinal>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(Int64) then
    Result := TValue.From<Int64>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(UInt64) then
    Result := TValue.From<UInt64>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(SmallInt) then
    Result := TValue.From<SmallInt>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(Word) then
    Result := TValue.From<Word>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(ShortInt) then
    Result := TValue.From<ShortInt>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(Byte) then
    Result := TValue.From<Byte>(Integer(value.AsBoolean))
  else
    RaiseConvertError(value.TypeInfo, targetTypeInfo);
end;

{$ENDREGION}


{$REGION 'TIntegerToBooleanConverter'}

function TIntegerToBooleanConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Boolean>(Boolean(value.AsInteger));
end;

{$ENDREGION}


{$REGION 'TNullableToTypeConverter'}

function TNullableToTypeConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  innerValue: TValue;
begin
  innerValue := TValue.Empty;
  if value.TryGetNullableValue(innerValue)
    and (innerValue.TypeInfo <> targetTypeInfo) then
    Result := TValueConverter.Default.ConvertTo(innerValue,
      targetTypeInfo, parameter)
  else
    Result := innerValue;
end;

{$ENDREGION}


{$REGION 'TTypeToNullableConverter'}

function TTypeToNullableConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  innerTypeInfo: PTypeInfo;
  innerValue: TValue;
begin
  innerTypeInfo := GetUnderlyingType(targetTypeInfo);
  if Assigned(innerTypeInfo) then
  begin
    innerValue := value;
    if innerTypeInfo.TypeName <> value.TypeInfo.TypeName then
      Result := TValueConverter.Default.TryConvertTo(value, innerTypeInfo,
        innerValue, parameter);

    TValue.Make(nil, targetTypeInfo, Result);
    Result.SetNullableValue(innerValue);
  end
  else
    RaiseConvertError(value.TypeInfo, targetTypeInfo);
end;

{$ENDREGION}


{$REGION 'TEnumToStringConverter'}

function TEnumToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  enumValue: Integer;
  enumName: string;
begin
  enumValue := PInteger(value.GetReferenceToRawData)^;
  enumName := GetEnumName(value.TypeInfo, enumValue);
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(enumName);
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(enumName));
    tkWString:
      Result := TValue.From<WideString>(enumName);
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TStringToEnumConverter'}

function TStringToEnumConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  enumValue: Integer;
begin
  enumValue := GetEnumValue(targetTypeInfo, value.AsString);
  TValue.Make(enumValue, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TSetToStringConverter'}

function TSetToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  setAsString: string;
begin
  setAsString := SetToString(value.TypeInfo,
    PInteger(value.GetReferenceToRawData)^, True);
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(setAsString);
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(setAsString));
    tkWString:
      Result := TValue.From<WideString>(setAsString);
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TStringToSetConverter'}

function TStringToSetConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  TValue.Make(StringToSet(targetTypeInfo, value.AsString),
    targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TEnumToIntegerConverter'}

function TEnumToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  enumValue: Integer;
begin
  enumValue := PInteger(value.GetReferenceToRawData)^;
  TValue.Make(enumValue, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TIntegerToEnumConverter'}

function TIntegerToEnumConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  i: Integer;
begin
  i := value.AsInteger;
  with targetTypeInfo.TypeData^ do
    Guard.CheckRangeInclusive(i, MinValue, MaxValue);
  TValue.Make(i, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TFloatToStringConverter'}

function TFloatToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and parameter.TryAsType<string>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FormatFloat(format, value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FormatFloat(format, value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(FormatFloat(format, value.AsExtended));
{$ENDIF}
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FloatToStr(value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FloatToStr(value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(FloatToStr(value.AsExtended));
{$ENDIF}
    end;
end;

{$ENDREGION}


{$REGION 'TFloatToIntegerConverter'}

function TFloatToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  f: Extended;
begin
  f := value.AsExtended;
  if Frac(f) <> 0 then
    RaiseConvertError(value.TypeInfo, targetTypeInfo);
  if targetTypeInfo = TypeInfo(Integer) then
    Result := TValue.From<Integer>(Floor(f))
  else if targetTypeInfo = TypeInfo(Cardinal) then
    Result := TValue.From<Cardinal>(Floor(f))
  else if targetTypeInfo = TypeInfo(Int64) then
    Result := TValue.From<Int64>(Floor(f))
  else if targetTypeInfo = TypeInfo(UInt64) then
    Result := TValue.From<UInt64>(Floor(f))
  else if targetTypeInfo = TypeInfo(SmallInt) then
    Result := TValue.From<SmallInt>(Floor(f))
  else if targetTypeInfo = TypeInfo(Word) then
    Result := TValue.From<Word>(Floor(f))
  else if targetTypeInfo = TypeInfo(ShortInt) then
    Result := TValue.From<ShortInt>(Floor(f))
  else if targetTypeInfo = TypeInfo(Byte) then
    Result := TValue.From<Byte>(Floor(f))
  else
    RaiseConvertError(value.TypeInfo, targetTypeInfo);
end;

{$ENDREGION}


{$REGION 'TStringToFloatConverter'}

function TStringToFloatConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  targetTypeData: PTypeData;
begin
  if targetTypeInfo.Kind = tkFloat then
  begin
    targetTypeData := targetTypeInfo.TypeData;
    case targetTypeData.FloatType of
      ftExtended:
        Result := TValue.From<Extended>(StrToFloat(value.AsString));
      ftDouble:
        Result := TValue.From<Double>(StrToFloat(value.AsString));
      ftSingle:
        Result := TValue.From<Single>(StrToFloat(value.AsString));
    else
      RaiseConvertError(value.TypeInfo, targetTypeInfo);
    end;
  end
  else
    RaiseConvertError(value.TypeInfo, targetTypeInfo);
end;

{$ENDREGION}


{$REGION 'TColorToStringConverter'}

{$IFNDEF SPRING_DISABLE_GRAPHICS}
function TColorToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(ColorToString(value.AsType<TColor>));
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(ColorToString(value.AsType<TColor>)));
    tkWString:
      Result := TValue.From<WideString>(ColorToString(value.AsType<TColor>));
{$ENDIF}
  end;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TStringToColorConverter'}

{$IFNDEF SPRING_DISABLE_GRAPHICS}
function TStringToColorConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TColor>(StringToColor(value.AsString));
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TCurrencyToStringConverter'}

function TCurrencyToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and parameter.TryAsType<string>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FormatCurr(format, value.AsType<Currency>));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FormatCurr(format, value.AsType<Currency>)));
      tkWString:
        Result := TValue.From<WideString>(FormatCurr(format, value.AsType<Currency>));
{$ENDIF}
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(CurrToStr(value.AsType<Currency>));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(CurrToStr(value.AsType<Currency>)));
      tkWString:
        Result := TValue.From<WideString>(CurrToStr(value.AsType<Currency>));
{$ENDIF}
    end;
end;

{$ENDREGION}


{$REGION 'TStringToCurrencyConverter'}

function TStringToCurrencyConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Currency>(StrToCurr(value.AsString));
end;

{$ENDREGION}


{$REGION 'TDateTimeToStringConverter'}

function TDateTimeToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and parameter.TryAsType<string>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FormatDateTime(format, value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FormatDateTime(format, value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(FormatDateTime(format, value.AsExtended));
{$ENDIF}
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(DateTimeToStr(value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(DateTimeToStr(value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(DateTimeToStr(value.AsExtended));
{$ENDIF}
    end;
end;

{$ENDREGION}


{$REGION 'TStringToDateTimeConverter'}

function TStringToDateTimeConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and parameter.TryAsType<string>(format) then
    Result := TValue.From<TDateTime>(StrToDateTimeFmt(value.AsString, format))
  else
    Result := TValue.From<TDateTime>(StrToDateTime(value.AsString));
end;

{$ENDREGION}


{$REGION 'TStringToGUIDConverter'}

function TStringToGUIDConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TGUID>(StringToGUID(value.AsString));
end;

{$ENDREGION}


{$REGION 'TDateToStringConverter'}

function TDateToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: TFormatSettings;
begin
  if not parameter.IsEmpty and parameter.TryAsType<TFormatSettings>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(DateToStr(value.AsExtended, format));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(DateToStr(value.AsExtended, format)));
      tkWString:
        Result := TValue.From<WideString>(DateToStr(value.AsExtended, format));
{$ENDIF}
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(DateToStr(value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(DateToStr(value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(DateToStr(value.AsExtended));
{$ENDIF}
    end;
end;

{$ENDREGION}


{$REGION 'TStringToDateConverter'}

function TStringToDateConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: TFormatSettings;
begin
  if not parameter.IsEmpty and parameter.TryAsType<TFormatSettings>(format) then
    Result := TValue.From<TDate>(StrToDate(value.AsString, format))
  else
    Result := TValue.From<TDate>(StrToDate(value.AsString));
end;

{$ENDREGION}


{$REGION 'TTimeToStringConverter'}

function TTimeToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: TFormatSettings;
begin
  if not parameter.IsEmpty and parameter.TryAsType<TFormatSettings>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(TimeToStr(value.AsExtended, format));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(TimeToStr(value.AsExtended, format)));
      tkWString:
        Result := TValue.From<WideString>(TimeToStr(value.AsExtended, format));
{$ENDIF}
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(TimeToStr(value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(TimeToStr(value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(TimeToStr(value.AsExtended));
{$ENDIF}
    end;
end;

{$ENDREGION}


{$REGION 'TStringToTimeConverter'}

function TStringToTimeConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: TFormatSettings;
begin
  if not parameter.IsEmpty and parameter.TryAsType<TFormatSettings>(format) then
    Result := TValue.From<TTime>(StrToTime(value.AsString, format))
  else
    Result := TValue.From<TTime>(StrToTime(value.AsString));
end;

{$ENDREGION}


{$REGION 'TObjectToStringConverter'}

function TObjectToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(value.AsObject.ToString);
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(value.AsObject.ToString));
    tkWString:
      Result := TValue.From<WideString>(value.AsObject.ToString);
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TObjectToInterfaceConverter'}

function TObjectToInterfaceConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  guid: TGUID;
  p: Pointer;
begin
  guid := targetTypeInfo.TypeData.Guid;
  if value.AsObject.GetInterface(guid, p) then
    TValue.MakeWithoutCopy(@p, targetTypeInfo, Result)
  else
    RaiseConvertError(value.TypeInfo, targetTypeInfo);
end;

{$ENDREGION}


{$REGION 'TInterfaceToObjectConverter'}

function TInterfaceToObjectConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TObject>(TObject(value.AsInterface));
end;

{$ENDREGION}


{$REGION 'TObjectToClassConverter'}

function TObjectToClassConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TClass>(value.AsObject.ClassType);
end;

{$ENDREGION}


{$REGION 'TStringToWStringConverter'}

{$IFNDEF NEXTGEN}
function TStringToWStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<WideString>(value.AsString);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TWStringToStringConverter'}

{$IFNDEF NEXTGEN}
function TWStringToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(value.AsString);
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(value.AsString));
    tkWString:
      Result := TValue.From<WideString>(value.AsString);
  end;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TInterfaceToInterfaceConverter'}

function TInterfaceToInterfaceConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;

  /// <summary>
  ///   Uses this function to get an interface instance from a TValue.
  /// </summary>
  /// <remarks>
  ///   <note type="warning">
  ///     Rtti bugs: QC #82433 if
  ///     value.TryAsType&lt;IPropertyNotification&gt;(propertyNotification)
  ///     then
  ///   </note>
  /// </remarks>
  function TryGetInterface(const instance: TValue; const guid: TGuid; out intf): Boolean;
  var
    localInterface: IInterface;
  begin
    if instance.IsEmpty then Exit(False);
    if instance.IsObject then
      Result := instance.AsObject.GetInterface(guid, intf)
    else if instance.TryAsType<IInterface>(localInterface) then
      Result := localInterface.QueryInterface(guid, intf) = S_OK
    else
      Result := False;
  end;

var
  intf: IInterface;
begin
  TryGetInterface(value, targetTypeInfo.TypeData.Guid, intf);
  TValue.Make(@intf, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TGUIDToStringConverter'}

function TGUIDToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := value.AsType<TGUID>.ToString;
end;

{$ENDREGION}


{$REGION 'TVariantToGUIDConverter'}

function TVariantToGUIDConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  v: Variant;
begin
  v := value.AsVariant;
  case VarType(v) of
    varString, varUString:
      Result := TValue.From<TGUID>(TGUID.Create(string(v)));
  else
    raise EInvalidOperationException.Create('unsupported vartype');
  end;
end;

{$ENDREGION}


{$REGION 'TByteArrayToGUIDConverter'}

function TByteArrayToGUIDConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TGUID>(BytesToGuidBigEndian(value.AsType<TBytes>));
end;

{$ENDREGION}


{$REGION 'TGUIDToByteArrayConverter'}

function TGUIDToByteArrayConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TBytes>(GuidToBytesBigEndian(value.AsType<TGUID>));
end;

{$ENDREGION}


{$REGION 'TValueConverterFactory'}

class constructor TValueConverterFactory.Create;
begin
  fTypeInfoToTypeInfoRegistry := TDictionary<TTypeMapping<PTypeInfo,PTypeInfo>, IValueConverter>.Create;
  fTypeInfoToTypeKindsRegistry := TDictionary<TTypeMapping<PTypeInfo,TTypeKind>, IValueConverter>.Create;
  fTypeKindsToTypeInfoRegistry := TDictionary<TTypeMapping<TTypeKind,PTypeInfo>, IValueConverter>.Create;
  fTypeKindsToTypeKindsRegistry := TDictionary<TTypeMapping<TTypeKind,TTypeKind>, IValueConverter>.Create;

  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Cardinal>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Int64>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.UInt64>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Word>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Byte>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkClass, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.string>), TTypeToNullableConverter);
{$IFNDEF NEXTGEN}
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkClass, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkClass, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.WideString>), TTypeToNullableConverter);
{$ENDIF}
  RegisterConverter([tkInteger, tkInt64, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Double>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Single>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64], TypeInfo(Boolean), TIntegerToBooleanConverter);
  RegisterConverter([tkInteger, tkInt64], [tkEnumeration], TIntegerToEnumConverter);
  RegisterConverter([tkInteger, tkInt64], [tkString, tkUString, tkLString, tkWString], TIntegerToStringConverter);

  RegisterConverter([tkFloat], [tkString, tkUString, tkLString, tkWString], TFloatToStringConverter);
  RegisterConverter([tkFloat], [tkInteger, tkInt64], TFloatToIntegerConverter);

{$IFNDEF SPRING_DISABLE_GRAPHICS}
  RegisterConverter(TypeInfo(TColor), TypeInfo(Nullable<TColor>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TColor), [tkString, tkUString, tkLString, tkWString], TColorToStringConverter);
{$ENDIF}

  RegisterConverter(TypeInfo(Currency), [tkString, tkUString, tkLString, tkWString], TCurrencyToStringConverter);

  RegisterConverter(TypeInfo(TDateTime), [tkString, tkUString, tkLString, tkWString], TDateTimeToStringConverter);
  RegisterConverter(TypeInfo(TDate), [tkString, tkUString, tkLString, tkWString], TDateToStringConverter);
  RegisterConverter(TypeInfo(TTime), [tkString, tkUString, tkLString, tkWString], TTimeToStringConverter);

  RegisterConverter(TypeInfo(Boolean), TypeInfo(Nullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Boolean), [tkString, tkUString, tkLString, tkWString], TBooleanToStringConverter);
  RegisterConverter(TypeInfo(Boolean), [tkInteger], TBooleanToIntegerConverter);

  RegisterConverter(TypeInfo(Nullable<System.Integer>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.SmallInt>), [
    tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.ShortInt>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.LongInt>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Cardinal>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Int64>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.UInt64>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Word>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Byte>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.string>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

{$IFNDEF NEXTGEN}
  RegisterConverter(TypeInfo(Nullable<System.AnsiString>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.WideString>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);
{$ENDIF}

  RegisterConverter(TypeInfo(Nullable<System.Single>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Double>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Extended>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Boolean>), TypeInfo(Boolean), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.Boolean>), [tkInteger, tkInt64], TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.Boolean>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

{$IFNDEF SPRING_DISABLE_GRAPHICS}
  RegisterConverter(TypeInfo(Nullable<TColor>), [tkInteger, tkInt64], TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<TColor>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);
{$ENDIF}

  RegisterConverter(TypeInfo(Nullable<System.TDateTime>), TypeInfo(TDate), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.TDateTime>), TypeInfo(TDateTime), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.TDateTime>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.TDate>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.TTime>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter([tkEnumeration], [tkString, tkUString, tkLString, tkWString], TEnumToStringConverter);
  RegisterConverter([tkEnumeration], [tkInteger, tkInt64], TEnumToIntegerConverter);

  RegisterConverter([tkSet], [tkString, tkUString, tkLString, tkWString], TSetToStringConverter);

  RegisterConverter([tkClass], [tkString, tkUString, tkLString, tkWString], TObjectToStringConverter);
  RegisterConverter([tkClass], [tkInterface], TObjectToInterfaceConverter);
  RegisterConverter([tkClass], [tkClassRef], TObjectToClassConverter);

  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Boolean), TStringToBooleanConverter);
{$IFNDEF SPRING_DISABLE_GRAPHICS}
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TColor), TStringToColorConverter);
{$ENDIF}
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Currency), TStringToCurrencyConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TDateTime), TStringToDateTimeConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TDate), TStringToDateConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TTime), TStringToTimeConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TGUID), TStringToGUIDConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Nullable<TGUID>), TTypeToNullableConverter);

  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Nullable<System.Currency>), TTypeToNullableConverter);
{$IFNDEF SPRING_DISABLE_GRAPHICS}
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Nullable<TColor>), TTypeToNullableConverter);
{$ENDIF}
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Nullable<System.TDateTime>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Nullable<System.TDate>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Nullable<System.TTime>), TTypeToNullableConverter);

  RegisterConverter([tkString, tkUString, tkLString, tkWString], [tkInteger, tkInt64], TStringToIntegerConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], [tkFloat], TStringToFloatConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], [tkEnumeration], TStringToEnumConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], [tkSet], TStringToSetConverter);

{$IFNDEF NEXTGEN}
  RegisterConverter([tkString, tkUString, tkLString], [tkWString], TStringToWStringConverter);
  RegisterConverter([tkWString], [tkString, tkUString, tkLString], TWStringToStringConverter);
{$ENDIF}

  RegisterConverter([tkInterface], [tkInterface], TInterfaceToInterfaceConverter);
  RegisterConverter([tkInterface], [tkClass], TInterfaceToObjectConverter);

  RegisterConverter(TypeInfo(TGUID), [tkString, tkUString, tkLString, tkWString], TGUIDToStringConverter);
  RegisterConverter(TypeInfo(Nullable<TGUID>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Variant), TypeInfo(TGUID), TVariantToGUIDConverter);

  RegisterConverter(TypeInfo(TBytes), TypeInfo(TGUID), TByteArrayToGUIDConverter);
  RegisterConverter(TypeInfo(TGUID), TypeInfo(TBytes), TGUIDToByteArrayConverter);
end;

class destructor TValueConverterFactory.Destroy;
begin
  fTypeInfoToTypeInfoRegistry.Free;
  fTypeInfoToTypeKindsRegistry.Free;
  fTypeKindsToTypeInfoRegistry.Free;
  fTypeKindsToTypeKindsRegistry.Free;
end;

class function TValueConverterFactory.GetConverter(
  sourceTypeInfo, targetTypeInfo: PTypeInfo): IValueConverter;
var
  typeToTypeMapping: TTypeMapping<PTypeInfo,PTypeInfo>;
  typeToKindMapping: TTypeMapping<PTypeInfo,TTypeKind>;
  kindToTypeMapping: TTypeMapping<TTypeKind,PTypeInfo>;
  kindToKindMapping: TTypeMapping<TTypeKind,TTypeKind>;
  sourceTypeData: PTypeData;
  cls: TClass;
  converter: IValueConverter;
begin
  System.MonitorEnter(fTypeInfoToTypeInfoRegistry);
  try
{$IFDEF NEXTGEN}
    typeToTypeMapping := Default(TTypeMapping<PTypeInfo,PTypeInfo>);
{$ENDIF}
    typeToTypeMapping.SourceType := sourceTypeInfo;
    typeToTypeMapping.TargetType := targetTypeInfo;
    if fTypeInfoToTypeInfoRegistry.TryGetValue(typeToTypeMapping, converter) then
      Exit(converter);
  finally
    System.MonitorExit(fTypeInfoToTypeInfoRegistry);
  end;

  System.MonitorEnter(fTypeInfoToTypeKindsRegistry);
  try
    typeToKindMapping := Default(TTypeMapping<PTypeInfo,TTypeKind>);
    typeToKindMapping.SourceType := sourceTypeInfo;
    typeToKindMapping.TargetType := targetTypeInfo.Kind;
    if fTypeInfoToTypeKindsRegistry.TryGetValue(typeToKindMapping, converter) then
      Exit(converter);
  finally
    System.MonitorExit(fTypeInfoToTypeKindsRegistry);
  end;

  System.MonitorEnter(fTypeKindsToTypeInfoRegistry);
  try
    kindToTypeMapping := Default(TTypeMapping<TTypeKind,PTypeInfo>);
    kindToTypeMapping.SourceType := sourceTypeInfo.Kind;
    kindToTypeMapping.TargetType := targetTypeInfo;
    if fTypeKindsToTypeInfoRegistry.TryGetValue(kindToTypeMapping, converter) then
      Exit(converter);
  finally
    System.MonitorExit(fTypeKindsToTypeInfoRegistry);
  end;

  System.MonitorEnter(fTypeKindsToTypeKindsRegistry);
  try
    kindToKindMapping := Default(TTypeMapping<TTypeKind,TTypeKind>);
    kindToKindMapping.SourceType := sourceTypeInfo.Kind;
    kindToKindMapping.TargetType := targetTypeInfo.Kind;
    if fTypeKindsToTypeKindsRegistry.TryGetValue(kindToKindMapping, converter) then
    begin
      typeToTypeMapping.SourceType := sourceTypeInfo;
      typeToTypeMapping.TargetType := targetTypeInfo;
      System.MonitorEnter(fTypeInfoToTypeInfoRegistry);
      try
        fTypeInfoToTypeInfoRegistry.Add(typeToTypeMapping, converter);
      finally
        System.MonitorExit(fTypeInfoToTypeInfoRegistry);
      end;
      Exit(converter);
    end;
  finally
    System.MonitorExit(fTypeKindsToTypeKindsRegistry);
  end;

  if sourceTypeInfo.Kind = tkClass then
  begin
    sourceTypeData := sourceTypeInfo.TypeData;
    cls := sourceTypeData.ClassType.ClassParent;
    typeToTypeMapping.TargetType := targetTypeInfo;
    System.MonitorEnter(fTypeInfoToTypeInfoRegistry);
    try
      while Assigned(cls) do
      begin
        typeToTypeMapping.SourceType := cls.ClassInfo;
        if fTypeInfoToTypeInfoRegistry.TryGetValue(typeToTypeMapping, converter) then
          Exit(converter);
        cls := cls.ClassParent;
      end;
    finally
      System.MonitorExit(fTypeInfoToTypeInfoRegistry);
    end;
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  sourceTypeKinds, targetTypeKinds: TTypeKinds;
  const converter: IValueConverter);
var
  mapping: TTypeMapping<TTypeKind,TTypeKind>;
  sourceTypeKind: TTypeKind;
  targetTypeKind: TTypeKind;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckFalse(SizeOf(sourceTypeKinds) = 0, SEmptySourceTypeKind);
  Guard.CheckFalse(SizeOf(targetTypeKinds) = 0, SEmptyTargetTypeKind);
{$ENDIF}

  System.MonitorEnter(fTypeKindsToTypeKindsRegistry);
  try
    mapping := Default(TTypeMapping<TTypeKind,TTypeKind>);
    for sourceTypeKind in sourceTypeKinds do
      for targetTypeKind in targetTypeKinds do
      begin
        mapping.SourceType := sourceTypeKind;
        mapping.TargetType := targetTypeKind;
        fTypeKindsToTypeKindsRegistry.AddOrSetValue(mapping, converter);
      end;
  finally
    System.MonitorExit(fTypeKindsToTypeKindsRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  sourceTypeInfo, targetTypeInfo: PTypeInfo;
  const converter: IValueConverter);
var
  mapping: TTypeMapping<PTypeInfo,PTypeInfo>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(sourceTypeInfo, 'sourceTypeInfo');
  Guard.CheckNotNull(targetTypeInfo, 'targetTypeInfo');
{$ENDIF}

  System.MonitorEnter(fTypeInfoToTypeInfoRegistry);
  try
{$IFDEF NEXTGEN}
    mapping := Default(TTypeMapping<PTypeInfo,PTypeInfo>);
{$ENDIF}
    mapping.SourceType := sourceTypeInfo;
    mapping.TargetType := targetTypeInfo;
    fTypeInfoToTypeInfoRegistry.AddOrSetValue(mapping, converter);
  finally
    System.MonitorExit(fTypeInfoToTypeInfoRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  sourceTypeInfo: PTypeInfo; targetTypeKinds: TTypeKinds;
  const converter: IValueConverter);
var
  mapping: TTypeMapping<PTypeInfo,TTypeKind>;
  targetTypeKind: TTypeKind;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(sourceTypeInfo, 'sourceTypeInfo');
  Guard.CheckFalse(SizeOf(targetTypeKinds) = 0, SEmptyTargetTypeKind);
{$ENDIF}

  System.MonitorEnter(fTypeInfoToTypeKindsRegistry);
  try
    mapping := Default(TTypeMapping<PTypeInfo,TTypeKind>);
    mapping.SourceType := sourceTypeInfo;
    for targetTypeKind in targetTypeKinds do
    begin
      mapping.TargetType := targetTypeKind;
      fTypeInfoToTypeKindsRegistry.AddOrSetValue(mapping, converter);
    end;
  finally
    System.MonitorExit(fTypeInfoToTypeKindsRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  sourceTypeKinds: TTypeKinds; targetTypeInfo: PTypeInfo;
  const converter: IValueConverter);
var
  mapping: TTypeMapping<TTypeKind,PTypeInfo>;
  sourceTypeKind: TTypeKind;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckFalse(SizeOf(sourceTypeKinds) = 0, SEmptySourceTypeKind);
  Guard.CheckNotNull(targetTypeInfo, 'targetTypeInfo');
{$ENDIF}

  System.MonitorEnter(fTypeKindsToTypeInfoRegistry);
  try
    mapping := Default(TTypeMapping<TTypeKind,PTypeInfo>);
    mapping.TargetType := targetTypeInfo;
    for sourceTypeKind in sourceTypeKinds do
    begin
      mapping.SourceType := sourceTypeKind;
      fTypeKindsToTypeInfoRegistry.AddOrSetValue(mapping, converter);
    end;
  finally
    System.MonitorExit(fTypeKindsToTypeInfoRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  sourceTypeInfo, targetTypeInfo: PTypeInfo;
  converterClass: TConverterClass);
begin
  RegisterConverter(sourceTypeInfo, targetTypeInfo, converterClass.Create);
end;

class procedure TValueConverterFactory.RegisterConverter(
  sourceTypeInfo: PTypeInfo; targetTypeKinds: TTypeKinds;
  converterClass: TConverterClass);
begin
  RegisterConverter(sourceTypeInfo, targetTypeKinds, converterClass.Create);
end;

class procedure TValueConverterFactory.RegisterConverter(
  sourceTypeKinds: TTypeKinds; targetTypeInfo: PTypeInfo;
  converterClass: TConverterClass);
begin
  RegisterConverter(sourceTypeKinds, targetTypeInfo, converterClass.Create);
end;

class procedure TValueConverterFactory.RegisterConverter(
  sourceTypeKinds, targetTypeKinds: TTypeKinds;
  converterClass: TConverterClass);
begin
  RegisterConverter(sourceTypeKinds, targetTypeKinds, converterClass.Create);
end;

{$ENDREGION}


end.
