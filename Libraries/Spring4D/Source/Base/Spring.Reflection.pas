{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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
{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

unit Spring.Reflection;

interface

uses
  Rtti,
  SyncObjs,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Extensions,
  Spring.DesignPatterns;

type

  {$REGION 'TType'}

  /// <summary>
  ///   Provides static methods to get RTTI information of a type.
  /// </summary>
  /// <remarks>
  ///   <note type="caller">
  ///     When using this class, a shared instance of the TRttiContext class
  ///     will be kept, which will make all instances of RTTI types live
  ///     during the lifetime.
  ///   </note>
  /// </remarks>
  TType = class(Spring.TType)
  strict private
    class var fSection: TCriticalSection;
    class var fInterfaceTypes: IDictionary<TGuid, TRttiInterfaceType>;
    class constructor Create;
  {$HINTS OFF}
    class destructor Destroy;
  {$HINTS ON}
  public
    class function GetTypes: IEnumerable<TRttiType>; static;
//    class function GetTypes: IEnumerable<TRttiType>;
    class function GetFullName(typeInfo: PTypeInfo): string; overload; static;
    class function GetFullName<T>: string; overload; static;
    class function FindType(const qualifiedName: string): TRttiType; static;
    class function TryGetType(typeInfo: PTypeInfo; out rttiType: TRttiType): Boolean; static;

    /// <summary>
    ///   Returns true if the typeFrom is assignable to the typeTo.
    /// </summary>
    class function IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean; static; inline;

    /// <summary>
    ///   Returns <c>True</c> if the typeInfo is a delegate type.
    /// </summary>
    class function IsDelegate(typeInfo: PTypeInfo): Boolean; overload; static;
    class function TryGetInterfaceType(const guid: TGUID; out intfType: TRttiInterfaceType): Boolean; static;

    class procedure SetFieldValue(const instance: TObject;
      const fieldName: string; const value: TValue); static;

    class procedure SetPropertyValue(const instance: TObject;
      const propertyName: string; const value: TValue); static;

    class procedure SetMemberValue(const instance: TObject;
      const name: string; const value: TValue); static;

    class property Types: IEnumerable<TRttiType> read GetTypes;
  end;

//  IRttiPackage = interface
//    ['{7365872F-36E1-424F-96F4-522357F0A9A4}']
//    {$REGION 'Property Accessors
//      function GetHandle: HINST;
//      function GetTypes: IEnumerable<TRttiType>;
//    {$ENDREGION}
//
//    property Handle: HINST read GetHandle;
//    function FindType(const qualifiedName: string): TRttiType;
//    property Types: IEnumerable<TRttiType> read GetTypes;
//  end;

  IReflection = interface
    ['{E3B66C0B-4827-44C4-BDD9-27F1A856FDDD}']
  {$REGION 'Property Accessors'}
    function GetClasses: IEnumerable<TRttiInstanceType>;
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;
    function GetTypes: IEnumerable<TRttiType>;
//    function GetPackages: IEnumerable<TRttiPackage>;
  {$ENDREGION}

    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
//    function GetType(const interfaceGuid: TGuid): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;

    function FindType(const qualifiedName: string): TRttiType;

//    function FindAllWhere(): IEnumerable<TRttiType>;

    property Classes: IEnumerable<TRttiInstanceType> read GetClasses;
    property Interfaces: IEnumerable<TRttiInterfaceType> read GetInterfaces;
    property Types: IEnumerable<TRttiType> read GetTypes;
//    property Packages: IEnumerable<TRttiPackage> read GetPackages;
  end;

  TReflection = class(TInterfacedObject, IReflection)
  strict private
    class var fContext: TRttiContext;
    function GetClasses: IEnumerable<TRttiInstanceType>;
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;
    function GetTypes: IEnumerable<TRttiType>;
//    function GetPackages: IEnumerable<TRttiPackage>;
    class constructor Create;
  {$HINTS OFF}
    class destructor Destroy;
  {$HINTS ON}
  public
    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;
    function FindType(const qualifiedName: string): TRttiType;

    property Classes: IEnumerable<TRttiInstanceType> read GetClasses;
    property Interfaces: IEnumerable<TRttiInterfaceType> read GetInterfaces;
    property Types: IEnumerable<TRttiType> read GetTypes;
//    property Packages: IEnumerable<TRttiPackage> read GetPackages;
  end;

  {$ENDREGION}


  {$REGION 'TRttiTypeIterator<T>'}

  TRttiTypeIterator<T: TRttiType> = class(TIterator<T>)
  private
    fContext: TRttiContext;
    fIndex: Integer;
    fTypes: TArray<TRttiType>;
  public
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TRttiObjectHelper'}

  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetCustomAttributes(attributeClass: TAttributeClass;
      inherit: Boolean = False): TArray<TCustomAttribute>; overload;

    /// <summary>
    ///   Gets an array which contains all custom attribute types which the
    ///   type applies.
    /// </summary>
    function GetCustomAttributes<T: TCustomAttribute>(
      inherit: Boolean = False): TArray<T>; overload;

    function GetCustomAttribute(attributeClass: TAttributeClass;
      inherit: Boolean = False): TCustomAttribute; overload;

    /// <summary>
    ///   Enumerates all applied custom attributes and returns the first one
    ///   which is/inherits the specified type.
    /// </summary>
    function GetCustomAttribute<T: TCustomAttribute>(inherit: Boolean = False): T; overload;

    function TryGetCustomAttribute(attributeClass: TAttributeClass;
      out attribute: TCustomAttribute; inherit: Boolean = False): Boolean; overload;

    /// <summary>
    ///   Try getting a custom attribute class which is applied by the type.
    /// </summary>
    function TryGetCustomAttribute<T: TCustomAttribute>(out attribute: T;
      inherit: Boolean = False): Boolean; overload;

    function HasCustomAttribute(attributeClass: TAttributeClass;
      inherit: Boolean = False): Boolean; overload;

    /// <summary>
    ///   Determines whether the type applies the specified custom attribute
    ///   class.
    /// </summary>
    function HasCustomAttribute<T: TCustomAttribute>(
      inherit: Boolean = False): Boolean; overload;
  end;

  {$ENDREGION}


  {$REGION 'TRttiTypeHelper'}

  TRttiTypeHelper =  class helper for TRttiType
  private
    function GetAsInterface: TRttiInterfaceType;
    function GetIsClass: Boolean;
    function GetIsInterface: Boolean;
    function GetIsClassOrInterface: Boolean;
    function GetAsClass: TRttiInstanceType;
    function GetIsGenericType: Boolean;
    function GetAsDynamicArray: TRttiDynamicArrayType;
    function GetIsDynamicArray: Boolean;
    function GetIsString: Boolean;
    function GetMethodsEnumerable: IEnumerable<TRttiMethod>;
    function GetPropertiesEnumerable: IEnumerable<TRttiProperty>;
    function GetFieldsEnumerable: IEnumerable<TRttiField>;
    function GetBaseTypes: IReadOnlyList<TRttiType>;
    function GetConstructorsEnumerable: IEnumerable<TRttiMethod>;
    function GetDefaultName: string;
    function GetAncestorCount: Integer;
  public

    /// <summary>
    ///   Returns all constructors
    /// </summary>
    function GetConstructors: TArray<TRttiMethod>;

    /// <summary>
    ///   Returns an enumerable collection which contains all the interface
    ///   Rtti types that the target type implements.
    ///   <note type="note">
    ///     Only Guid interfaces will be enumerated.
    ///   </note>
    /// </summary>
    /// <seealso cref="Spring.Collections|IEnumerable&lt;T&gt;" />
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;

    /// <summary>
    ///   Gets an array of types which contains all generic arguments.
    /// </summary>
    /// <remarks>
    ///   This method extracts generic arguments from the name of the generic
    ///   type. Invoking the method on the type <c>
    ///   TDictionary&lt;Integer,string&gt;</c> for example will return an
    ///   array which contains two types: <c>System.Integer</c> and <c>
    ///   System.string</c>.
    /// </remarks>
    function GetGenericArguments: TArray<TRttiType>;

    /// <summary>
    ///   Returns a string that represents a generic type definition.
    /// </summary>
    function GetGenericTypeDefinition: string;

    function HasField(const name: string): Boolean;
    function HasMethod(const name: string): Boolean;
    function HasProperty(const name: string): Boolean;

    /// <summary>
    ///   Determines whether an instance of the current TRttiType can be
    ///   assigned from an instance of the specified TRttiType.
    /// </summary>
    /// <param name="rttiType">
    ///   The type to compare with the current type.
    /// </param>
    function IsAssignableFrom(const rttiType: TRttiType): Boolean;

    /// <summary>
    ///   Determines whether the current type is of the specified generic type.
    /// </summary>
    /// <param name="genericType">
    ///   The generic type definition of the type to check for
    /// </param>
    function IsGenericTypeOf(const genericType: string): Boolean; overload;
    function IsGenericTypeOf(genericType: PTypeInfo): Boolean; overload;
    function IsGenericTypeOf<T>: Boolean; overload; inline;
    function IsType<T>: Boolean; overload;
    function IsType(typeInfo: PTypeInfo): Boolean; overload; inline;

    function TryGetField(const name: string; out field: TRttiField): Boolean;
    function TryGetProperty(const name: string; out prop: TRttiProperty): Boolean;
    function TryGetMethod(const name: string; out method: TRttiMethod): Boolean;

    function GetMember(const name: string): TRttiMember;

    function TryGetMember(const name: string; out member: TRttiMember): Boolean;

    property BaseTypes: IReadOnlyList<TRttiType> read GetBaseTypes;

    /// <summary>
    ///   Gets an enumerable collection which contains all constructor methods
    ///   of the type, including inherited.
    /// </summary>
    /// <seealso cref="Methods" />
    /// <seealso cref="Properties" />
    /// <seealso cref="Fields" />
    property Constructors: IEnumerable<TRttiMethod> read GetConstructorsEnumerable;

    /// <summary>
    ///   Gets a enumerable collection which contains all methods that the type
    ///   contains, including inherited.
    /// </summary>
    /// <seealso cref="Constructors" />
    /// <seealso cref="Properties" />
    /// <seealso cref="Fields" />
    property Methods: IEnumerable<TRttiMethod> read GetMethodsEnumerable;

    /// <summary>
    ///   Gets a enumerable collection which contains all properties that the
    ///   type contains, including inherited.
    /// </summary>
    /// <seealso cref="Constructors" />
    /// <seealso cref="Methods" />
    /// <seealso cref="Fields" />
    property Properties: IEnumerable<TRttiProperty> read GetPropertiesEnumerable;

    /// <summary>
    ///   Gets a enumerable collection which contains all fields that the type
    ///   contains, including inherited.
    /// </summary>
    /// <seealso cref="Constructors" />
    /// <seealso cref="Methods" />
    /// <seealso cref="Properties" />
    property Fields: IEnumerable<TRttiField> read GetFieldsEnumerable;

    property AsClass: TRttiInstanceType read GetAsClass;
    property AsInterface: TRttiInterfaceType read GetAsInterface;
    property AsDynamicArray: TRttiDynamicArrayType read GetAsDynamicArray;
    property IsClass: Boolean read GetIsClass;
    property IsInterface: Boolean read GetIsInterface;
    property IsClassOrInterface: Boolean read GetIsClassOrInterface;
    property IsDynamicArray: Boolean read GetIsDynamicArray;
    property IsString: Boolean read GetIsString;

    /// <summary>
    ///   Gets a value indicates whether the current type is generic.
    /// </summary>
    property IsGenericType: Boolean read GetIsGenericType;

    property DefaultName: string read GetDefaultName;
    property AncestorCount: Integer read GetAncestorCount;
  end;

  TRttiClassType = TRttiInstanceType;

  {$ENDREGION}


  {$REGION 'TRttiInterfaceTypeHelper'}

  TRttiInterfaceTypeHelper = class helper for TRttiInterfaceType
  private
    function GetHasGuid: Boolean;
  public
    /// <summary>
    ///   Determines whether this interface type has a guid.
    /// </summary>
    property HasGuid: Boolean read GetHasGuid;
  end;

  {$ENDREGION}


  {$REGION 'TRttiMemberHelper'}

  TRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsPrivate: Boolean;
    function GetIsProtected: Boolean;
    function GetIsPublic: Boolean;
    function GetIsPublished: Boolean;
    function GetIsConstructor: Boolean;
    function GetIsProperty: Boolean;
    function GetIsMethod: Boolean;
    function GetIsField: Boolean;
    function GetAsMethod: TRttiMethod;
    function GetAsProperty: TRttiProperty;
    function GetAsField: TRttiField;
    function GetMemberType: TRttiType;
    function GetIsReadable: Boolean;
    function GetIsWritable: Boolean;
  public
//    procedure InvokeMember(instance: TValue; const arguments: array of TValue);
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
    property AsMethod: TRttiMethod read GetAsMethod;
    property AsProperty: TRttiProperty read GetAsProperty;
    property AsField: TRttiField read GetAsField;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsProperty: Boolean read GetIsProperty;
    property IsMethod: Boolean read GetIsMethod;
    property IsField: Boolean read GetIsField;
    property IsPrivate: Boolean read GetIsPrivate;
    property IsProtected: Boolean read GetIsProtected;
    property IsPublic: Boolean read GetIsPublic;
    property IsPublished: Boolean read GetIsPublished;
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
    property MemberType: TRttiType read GetMemberType;
  end;

  {$ENDREGION}


  {$REGION 'TRttiFieldHelper'}

  TRttiFieldHelper = class helper for TRttiField
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  {$ENDREGION}


  {$REGION 'TRttiPropertyHelper'}

  TRttiPropertyHelper = class helper for TRttiProperty
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  {$ENDREGION}


  {$REGION 'TRttiMethodHelper'}

  TRttiMethodHelper = class helper(Spring.TRttiMethodHelper) for TRttiMethod
  private
    function GetIsGetter: Boolean;
    function GetIsSetter: Boolean;
    function GetParameterCount: Integer;
    function GetParametersList: IReadOnlyList<TRttiParameter>;
  public
    property IsGetter: Boolean read GetIsGetter;
    property IsSetter: Boolean read GetIsSetter;
    property ParameterCount: Integer read GetParameterCount;
    property Parameters: IReadOnlyList<TRttiParameter> read GetParametersList;
  end;

  {$ENDREGION}


  {$REGION 'TFiltersNamed<T>'}

  TFiltersNamed<T: TRttiNamedObject> = class
  public
    class function IsNamed(const name: string): TSpecification<T>;
    class function HasAttribute(attributeClass: TAttributeClass;
      inherit: Boolean = False): TSpecification<T>;
  end;

  {$ENDREGION}


  {$REGION 'TFiltersBase<T>'}

  TMethodKinds = set of TMethodKind;

  /// <summary>
  ///   Provides static methods to create specifications to filter TRttiMember
  ///   objects.
  /// </summary>
  TFiltersBase<T: TRttiMember> = class(TFiltersNamed<T>)
  public
    class function ContainsParameterType(typeInfo: PTypeInfo): TSpecification<T>;
    class function HasParameterTypes(const types: array of PTypeInfo): TSpecification<T>;
    class function HasParameterFlags(const flags: TParamFlags): TSpecification<T>;
    class function IsTypeOf<TType>: TSpecification<T>; overload;
    class function IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>; overload;
    class function IsConstructor: TSpecification<T>;
    class function IsInstanceMethod: TSpecification<T>;
    class function IsClassMethod: TSpecification<T>;
    class function IsMethodKind(const kinds : TMethodKinds): TSpecification<T>;
    class function IsInvokable: TSpecification<T>;
  end;

  {$ENDREGION}


  {$REGION 'Filters'}

  TPackageFilters = class(TFiltersNamed<TRttiPackage>);
  TMemberFilters = class(TFiltersBase<TRttiMember>);
  TMethodFilters = class(TFiltersBase<TRttiMethod>);
  TPropertyFilters = class(TFiltersBase<TRttiProperty>);
  TFieldFilters = class(TFiltersBase<TRttiField>);
  TTypeFilters = class(TFiltersNamed<TRttiType>)
  public
    class function IsClass : TSpecification<TRttiType>;
    class function IsInterface : TSpecification<TRttiType>;
  end;
  TParameterFilters = class(TFiltersNamed<TRttiParameter>)
  public
    class function HasFlags(flags: TParamFlags): TSpecification<TRttiParameter>;
  end;

  {$ENDREGION}


  {$REGION 'TNameFilter<T>'}

  TNameFilter<T: TRttiNamedObject> = class(TSpecificationBase<T>)
  private
    fName: string;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const name: string);
  end;

  {$ENDREGION}


  {$REGION 'TInvokableFilter<T>'}

  TInvokableFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasAttributeFilter<T>'}

  THasAttributeFilter<T: TRttiObject> = class(TSpecificationBase<T>)
  private
    fAttributeClass: TAttributeClass;
    fInherit: Boolean;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(attributeClass: TAttributeClass; inherit: Boolean = False);
  end;

  {$ENDREGION}


  {$REGION 'TTypeFilter<T>'}

  TTypeFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'THasParameterTypesFilter<T>'}

  THasParameterTypesFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fTypes: TArray<PTypeInfo>;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const types: array of PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'TContainsParameterTypeFilter<T>'}

  TContainsParameterTypeFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  TRttiMemberClass = class of TRttiMember;


  {$REGION 'TMemberTypeFilter<T>'}

  TMemberTypeFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fMemberClass: TRttiMemberClass;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(memberClass: TRttiMemberClass);
  end;

  {$ENDREGION}


  {$REGION 'TConstructorFilter<T>'}

  TConstructorFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TInstanceMethodFilter<T>'}

  TInstanceMethodFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TClassMethodFilter<T>'}

  TClassMethodFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasParameterFlagsFilter<T>'}

  THasParameterFlagsFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fFlags: TParamFlags;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const flags: TParamFlags);
  end;

  {$ENDREGION}


  {$REGION 'TMethodKindFilter<T>'}

  TMethodKindFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fFlags: TMethodKinds;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const flags: TMethodKinds);
  end;

  {$ENDREGION}


  {$REGION 'TIsClassFilter'}

  TIsClassFilter = class(TSpecificationBase<TRttiType>)
  protected
    function IsSatisfiedBy(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TIsInterfaceFilter'}

  TIsInterfaceFilter = class(TSpecificationBase<TRttiType>)
  protected
    function IsSatisfiedBy(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasFlagsFilter'}

  THasFlagsFilter = class(TSpecificationBase<TRttiParameter>)
  private
    fFlags: TParamFlags;
  protected
    function IsSatisfiedBy(const parameter: TRttiParameter): Boolean; override;
  public
    constructor Create(flags: TParamFlags);
  end;

  {$ENDREGION}


  {$REGION 'Routines'}

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv;
  IsConst: Boolean = False): Boolean;
procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue;
  var ArgDest: TValue; CC: TCallConv);

  {$ENDREGION}


implementation

uses
  Math,
  RTLConsts,
  StrUtils,
  SysConst,
  Spring.ResourceStrings;

const
  EmptyGuid: TGUID = ();


{$REGION 'Routines'}

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv; IsConst: Boolean = False): Boolean;
begin
  if TypeInfo = nil then
    Exit(False);
  case TypeInfo^.Kind of
    tkArray:
      Result := GetTypeData(TypeInfo)^.ArrayData.Size > SizeOf(Pointer);
{$IF Defined(CPUX86)}
    tkRecord:
      if (CC in [ccCdecl, ccStdCall, ccSafeCall]) and not IsConst then
        Result := False
      else
        Result := GetTypeData(TypeInfo)^.RecSize > SizeOf(Pointer);
    tkVariant:
      Result := IsConst or not (CC in [ccCdecl, ccStdCall, ccSafeCall]);
{$ELSEIF Defined(CPUX64)}
    tkRecord:
      Result := not (GetTypeData(TypeInfo)^.RecSize in [1,2,4,8]);
    tkMethod,
    tkVariant:
      Result := True;
{$ELSEIF Defined(CPUARM)}
    tkRecord:
      Result := (CC = ccReg) or (CC = ccPascal);
    tkMethod,
    tkVariant:
      Result := True;
{$IFEND}
{$IFNDEF NEXTGEN}
    tkString:
      Result := GetTypeData(TypeInfo)^.MaxLength > SizeOf(Pointer);
{$ENDIF}
  else
    Result := False;
  end;
end;

procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue;
  var ArgDest: TValue; CC: TCallConv);
begin
  if Par.ParamType = nil then
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData) // untyped var or const
  else if Par.Flags * [pfVar, pfOut] <> [] then
  begin
    if Par.ParamType.Handle <> ArgSrc.TypeInfo then
      raise EInvalidCast.CreateRes(@SByRefArgMismatch);
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData);
  end
  else if (pfConst in Par.Flags) and
    PassByRef(Par.ParamType.Handle, CC, True) then
  begin
    if TypeInfo(TValue) = Par.ParamType.Handle then
      ArgDest := TValue.From(ArgSrc)
    else
    begin
      if Par.ParamType.Handle <> ArgSrc.TypeInfo then
        raise EInvalidCast.CreateRes(@SByRefArgMismatch);
      ArgDest := TValue.From(ArgSrc.GetReferenceToRawData);
    end
  end
  else
    ArgDest := ArgSrc.Cast(Par.ParamType.Handle);
end;

{$ENDREGION}


{$REGION 'TType'}

class constructor TType.Create;
begin
  fSection := TCriticalSection.Create;
end;

class destructor TType.Destroy;
begin
  fSection.Free;
end;

class function TType.GetTypes: IEnumerable<TRttiType>;
begin
  Result := TRttiTypeIterator<TRttiType>.Create;
end;

class function TType.GetFullName(typeInfo: PTypeInfo): string;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(typeInfo, 'typeInfo');
{$ENDIF}

  Result := Context.GetType(typeInfo).QualifiedName;
end;

class function TType.GetFullName<T>: string;
var
  typeInfo: PTypeInfo;
begin
  typeInfo := System.TypeInfo(T);
  Result := TType.GetFullName(typeInfo);
end;

class function TType.FindType(const qualifiedName: string): TRttiType;
var
  item: TRttiType;
begin
  Result := Context.FindType(qualifiedName);
  if not Assigned(Result) then
    for item in Context.GetTypes do
      if SameText(item.Name, qualifiedName) then
        Exit(item);
end;

class function TType.IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean;
begin
  Result := IsAssignableFrom(typeTo, typeFrom);
end;

class function TType.IsDelegate(typeInfo: PTypeInfo): Boolean;
const
  DelegatePrefixStrings: array[0..2] of string = (
    'TFunc<', 'TProc<', 'TPredicate<');
  DelegatePrefixNonGenericStrings: array[0..1] of string = (
    'TProc', 'TPredicate');
var
  name: string;
  prefix: string;
  rttiType: TRttiType;
  method: TRttiMethod;
  typeData: PTypeData;
begin
  while Assigned(typeInfo) and (typeInfo.Kind = tkInterface) do
  begin
    name := typeInfo.TypeName;
    for prefix in DelegatePrefixNonGenericStrings do
      if SameText(prefix, name) then
        Exit(True);
    for prefix in DelegatePrefixStrings do
      if StartsText(prefix, name) then
        Exit(True);
    rttiType := TType.GetType(typeInfo);
    if rttiType.Methods.TryGetSingle(method) and (method.Name = 'Invoke') then
      Exit(True);
    typeData := GetTypeData(typeInfo);
    if Assigned(typeData) and Assigned(typeData.IntfParent) then
      typeInfo := typeData.IntfParent^
    else
      typeInfo := nil;
  end;
  Result := False;
end;

class procedure TType.SetFieldValue(const instance: TObject;
  const fieldName: string; const value: TValue);
var
  rttiType: TRttiType;
  field: TRttiField;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(instance, 'instance');
{$ENDIF}

  if TryGetType(instance.ClassInfo, rttiType)
    and rttiType.TryGetField(fieldName, field) then
    field.SetValue(instance, value);
end;

class procedure TType.SetMemberValue(const instance: TObject;
  const name: string; const value: TValue);
var
  rttiType: TRttiType;
  field: TRttiField;
  prop: TRttiProperty;
begin
  // TODO: TValue conversion ?
  if TryGetType(instance.ClassInfo, rttiType) then
    if rttiType.TryGetField(name, field) then
      field.SetValue(instance, value)
    else if rttiType.TryGetProperty(name, prop) then
      prop.SetValue(instance, value);
end;

class procedure TType.SetPropertyValue(const instance: TObject;
  const propertyName: string; const value: TValue);
var
  rttiType: TRttiType;
  prop: TRttiProperty;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(instance, 'instance');
{$ENDIF}

  if TryGetType(instance.ClassInfo, rttiType)
    and rttiType.TryGetProperty(propertyName, prop) then
    prop.SetValue(instance, value);
end;

class function TType.TryGetInterfaceType(const guid: TGUID;
  out intfType: TRttiInterfaceType): Boolean;
var
  item: TRttiType;
begin
  if fInterfaceTypes = nil then
  begin
    fSection.Enter;
    try
      MemoryBarrier;
      if fInterfaceTypes = nil then
      begin
        fInterfaceTypes := TCollections.CreateDictionary<TGuid, TRttiInterfaceType>;
        for item in Context.GetTypes do
          if item.IsInterface and TRttiInterfaceType(item).HasGuid
            and not fInterfaceTypes.ContainsKey(TRttiInterfaceType(item).GUID) then
            fInterfaceTypes.Add(TRttiInterfaceType(item).GUID, TRttiInterfaceType(item));
      end;
    finally
      fSection.Leave;
    end;
  end;
  Result := fInterfaceTypes.TryGetValue(guid, intfType);
end;

class function TType.TryGetType(typeInfo: PTypeInfo;
  out rttiType: TRttiType): Boolean;
begin
  rttiType := Context.GetType(typeInfo);
  Result := Assigned(rttiType);
end;

{$ENDREGION}


{$REGION 'TRttiTypeIterator<T>'}

function TRttiTypeIterator<T>.Clone: TIterator<T>;
begin
  Result := TRttiTypeIterator<T>.Create;
end;

function TRttiTypeIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fTypes := fContext.GetTypes;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fIndex < High(fTypes) do
    begin
      Inc(fIndex);
      if fTypes[fIndex].InheritsFrom(T) then
      begin
        fCurrent := T(fTypes[fIndex]);
        Exit(True);
      end;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TRttiObjectHelper'}

function TRttiObjectHelper.TryGetCustomAttribute(
  attributeClass: TAttributeClass; out attribute: TCustomAttribute;
  inherit: Boolean): Boolean;
begin
  attribute := GetCustomAttribute(attributeClass, inherit);
  Result := Assigned(attribute);
end;

function TRttiObjectHelper.TryGetCustomAttribute<T>(out attribute: T;
  inherit: Boolean): Boolean;
begin
  attribute := GetCustomAttribute<T>(inherit);
  Result := Assigned(attribute);
end;

function TRttiObjectHelper.GetCustomAttribute(
  attributeClass: TAttributeClass; inherit: Boolean): TCustomAttribute;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
    if attribute.InheritsFrom(attributeClass) then
      Exit(attribute);
  if inherit and (Self is TRttiType) and Assigned(TRttiType(Self).BaseType) then
    Result := TRttiType(Self).BaseType.GetCustomAttribute(attributeClass, inherit)
  else
    Result := nil;
end;

function TRttiObjectHelper.GetCustomAttribute<T>(inherit: Boolean): T;
begin
  Result := T(GetCustomAttribute(TAttributeClass(T), inherit));
end;

function TRttiObjectHelper.GetCustomAttributes(
  attributeClass: TAttributeClass; inherit: Boolean): TArray<TCustomAttribute>;
var
  attribute: TCustomAttribute;
  attributes: TArray<TCustomAttribute>;
begin
  Result := nil;
  for attribute in GetAttributes do
    if attribute.InheritsFrom(attributeClass) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := attribute;
    end;
  if inherit and (Self is TRttiType) and Assigned(TRttiType(Self).BaseType) then
  begin
    attributes := TRttiType(Self).BaseType.GetCustomAttributes(attributeClass, inherit);
    Result := TArray.Concat<TCustomAttribute>([Result, attributes]);
  end;
end;

function TRttiObjectHelper.GetCustomAttributes<T>(inherit: Boolean): TArray<T>;
begin
  TArray<TCustomAttribute>(Result) := GetCustomAttributes(TAttributeClass(T), inherit);
end;

function TRttiObjectHelper.HasCustomAttribute(
  attributeClass: TAttributeClass; inherit: Boolean): Boolean;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
    if attribute.InheritsFrom(attributeClass) then
      Exit(True);
  if inherit and (Self is TRttiType) and Assigned(TRttiType(Self).BaseType) then
    Result := TRttiType(Self).BaseType.HasCustomAttribute(attributeClass, inherit)
  else
    Result := False;
end;

function TRttiObjectHelper.HasCustomAttribute<T>(inherit: Boolean): Boolean;
begin
  Result := HasCustomAttribute(TAttributeClass(T), inherit);
end;

{$ENDREGION}


{$REGION 'TRttiTypeHelper'}

function TRttiTypeHelper.GetConstructors: TArray<TRttiMethod>;
var
  i, n: Integer;
begin
  n := 0;
  Result := GetMethods;
  for i := 0 to High(Result) do
    if Result[i].IsConstructor then
    begin
      if i > n then
        Result[n] := Result[i];
      Inc(n);
    end;
  SetLength(Result, n);
end;

function TRttiTypeHelper.GetConstructorsEnumerable: IEnumerable<TRttiMethod>;
begin
  Result := TArrayIterator<TRttiMethod>.Create(GetConstructors());
end;

function TRttiTypeHelper.GetMethodsEnumerable: IEnumerable<TRttiMethod>;
begin
  Result := TArrayIterator<TRttiMethod>.Create(GetMethods());
end;

function TRttiTypeHelper.GetPropertiesEnumerable: IEnumerable<TRttiProperty>;
begin
  Result := TArrayIterator<TRttiProperty>.Create(GetProperties());
end;

function TRttiTypeHelper.GetFieldsEnumerable: IEnumerable<TRttiField>;
begin
  Result := TArrayIterator<TRttiField>.Create(GetFields());
end;

function TRttiTypeHelper.GetDefaultName: string;
begin
  if IsPublicType then
    Result := QualifiedName
  else
    case TypeKind of
      tkClass: Result := TRttiInstanceType(Self).DeclaringUnitName + '.' + Name;
      tkInterface: Result := TRttiInterfaceType(Self).DeclaringUnitName + '.' + Name;
      tkDynArray: Result := TRttiDynamicArrayType(Self).DeclaringUnitName + '.' + Name;
    else
      Result := Name;
    end;
end;

function TRttiTypeHelper.GetMember(const name: string): TRttiMember;
begin
  if not TryGetProperty(name, TRttiProperty(Result))
    and not TryGetField(name, TRttiField(Result))
    and not TryGetMethod(name, TRttiMethod(Result)) then
    Result := nil;
end;

function TRttiTypeHelper.HasField(const name: string): Boolean;
begin
  Result := Assigned(GetField(name));
end;

function TRttiTypeHelper.HasMethod(const name: string): Boolean;
begin
  Result := Assigned(GetMethod(name));
end;

function TRttiTypeHelper.HasProperty(const name: string): Boolean;
begin
  Result := Assigned(GetProperty(name));
end;

// Nullable<TDateTime>
// TDictionary<string, TObject>
// TDictionary<string, IDictionary<string, TObject>>
function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>;
var
  names: TArray<string>;
  i: Integer;
begin
  names := GetGenericTypeParameters(Name);
  SetLength(Result, Length(names));
  for i := Low(names) to High(names) do
    Result[i] := TType.FindType(names[i]);
end;

function TRttiTypeHelper.GetGenericTypeDefinition: string;
begin
  if not IsGenericType then
    raise EInvalidOperationException.CreateResFmt(@SNotGenericType, [Name]);
  Result := Copy(Name, 0, Pos('<', Name)) + DupeString(',', High(GetGenericArguments)) + '>';
end;

function TRttiTypeHelper.GetAncestorCount: Integer;
var
  baseType: TRttiType;
begin
  Result := 0;
  baseType := Self;
  while Assigned(baseType.BaseType) do
  begin
    Inc(Result);
    baseType := baseType.BaseType;
  end;
end;

function TRttiTypeHelper.GetAsClass: TRttiInstanceType;
begin
  Result := Self as TRttiInstanceType;
end;

function TRttiTypeHelper.GetAsDynamicArray: TRttiDynamicArrayType;
begin
  Result := Self as TRttiDynamicArrayType;
end;

function TRttiTypeHelper.GetAsInterface: TRttiInterfaceType;
begin
  Result := Self as TRttiInterfaceType;
end;

function TRttiTypeHelper.GetBaseTypes: IReadOnlyList<TRttiType>;
var
  count: Integer;
  t: TRttiType;
  types: TArray<TRttiType>;
begin
  count := 0;
  t := Self;
  while Assigned(t) do
  begin
    Inc(count);
    t := t.BaseType;
  end;

  SetLength(types, count);
  count := 0;
  t := Self;
  while Assigned(t) do
  begin
    types[count] := t;
    Inc(count);
    t := t.BaseType;
  end;

  Result := TArrayIterator<TRttiType>.Create(types);
end;

function TRttiTypeHelper.GetInterfaces: IEnumerable<TRttiInterfaceType>;
var
  list: IDictionary<TGUID, TRttiInterfaceType>;
  classType: TClass;
  table: PInterfaceTable;
  entry: TInterfaceEntry;
  intfType: TRttiInterfaceType;
  i: Integer;
begin
  if IsClass then
  begin
    list := TCollections.CreateDictionary<TGUID, TRttiInterfaceType>;
    classType := AsInstance.MetaclassType;
    while Assigned(classType) do
    begin
      table := classType.GetInterfaceTable;
      if Assigned(table) then
      begin
        for i := 0 to table.EntryCount - 1 do
        begin
          entry := table.Entries[i];
          if not list.ContainsKey(entry.IID)
            and (entry.IID <> EmptyGuid)
            and TType.TryGetInterfaceType(entry.IID, intfType) then
            list[entry.IID] := intfType;
        end;
      end;
      classType := classType.ClassParent;
    end;
    Result := list.Values;
  end
  else
  if IsInterface then
  begin
    list := TCollections.CreateDictionary<TGUID, TRttiInterfaceType>;
    intfType := AsInterface;
    while Assigned(intfType) do
    begin
      if intfType.HasGuid and not list.ContainsKey(intfType.GUID)
        and (intfType.GUID <> EmptyGuid) then
        list[intfType.GUID] := intfType;
      intfType := intfType.BaseType;
    end;
    Result := list.Values;
  end
  else
    Result := TEnumerable.Empty<TRttiInterfaceType>;
end;

function TRttiTypeHelper.GetIsClass: Boolean;
begin
  Result := Self is TRttiInstanceType;
end;

function TRttiTypeHelper.GetIsClassOrInterface: Boolean;
begin
  Result := IsClass or IsInterface;
end;

function TRttiTypeHelper.GetIsDynamicArray: Boolean;
begin
  Result := Self is TRttiDynamicArrayType;
end;

function TRttiTypeHelper.GetIsGenericType: Boolean;
begin
  Result := (Pos('<', Name) > 0) and (Pos('>', Name) > 0);
end;

function TRttiTypeHelper.GetIsInterface: Boolean;
begin
  Result := Self is TRttiInterfaceType;
end;

function TRttiTypeHelper.GetIsString: Boolean;
begin
  Result := TypeKind in [tkString, tkLString, tkWString, tkUString, tkChar, tkWChar];
end;

function TRttiTypeHelper.IsAssignableFrom(const rttiType: TRttiType): Boolean;
begin
  Result := Spring.IsAssignableFrom(Handle, rttiType.Handle);
end;

function TRttiTypeHelper.IsGenericTypeOf(genericType: PTypeInfo): Boolean;
var
  t: TRttiType;
begin
  t := TType.GetType(genericType);
  Result := Assigned(t) and t.IsGenericType
    and IsGenericTypeOf(t.GetGenericTypeDefinition);
end;

function TRttiTypeHelper.IsGenericTypeOf<T>: Boolean;
begin
  Result := IsGenericTypeOf(TypeInfo(T));
end;

function TRttiTypeHelper.IsGenericTypeOf(const genericType: string): Boolean;
var
  baseType: TRttiType;
begin
  if not IsGenericType then
    Exit(False);
  if SameText(GetGenericTypeDefinition, genericType)  then
    Result := True
  else
  begin
    baseType := Self.BaseType;
    Result := Assigned(baseType) and baseType.IsGenericTypeOf(genericType);
  end;
end;

function TRttiTypeHelper.IsType(typeInfo: PTypeInfo): Boolean;
begin
  Result := Handle = typeInfo;
end;

function TRttiTypeHelper.IsType<T>: Boolean;
begin
  Result := Handle = TypeInfo(T);
end;

function TRttiTypeHelper.TryGetField(const name: string;
  out field: TRttiField): Boolean;
begin
  field := GetField(name);
  Result := Assigned(field);
end;

function TRttiTypeHelper.TryGetMethod(const name: string;
  out method: TRttiMethod): Boolean;
begin
  method := GetMethod(name);
  Result := Assigned(method);
end;

function TRttiTypeHelper.TryGetProperty(const name: string;
  out prop: TRttiProperty): Boolean;
begin
  prop := GetProperty(name);
  Result := Assigned(prop);
end;

function TRttiTypeHelper.TryGetMember(const name: string;
  out member: TRttiMember): Boolean;
begin
  member := GetMember(name);
  Result := Assigned(member);
end;

{$ENDREGION}


{$REGION 'TRttiInterfaceTypeHelper'}

function TRttiInterfaceTypeHelper.GetHasGuid: Boolean;
begin
  Result := ifHasGuid in IntfFlags;
end;

{$ENDREGION}


{$REGION 'TRttiMemberHelper'}

function TRttiMemberHelper.GetValue(const instance: TValue): TValue;
begin
  if IsProperty then
    Result := AsProperty.GetValue(instance)
  else if IsField then
    Result := AsField.GetValue(instance)
  else
    raise EInvalidOperationException.CreateRes(@SInvalidOperation_GetValue);
end;

procedure TRttiMemberHelper.SetValue(const instance, value: TValue);
begin
  if IsProperty then
    AsProperty.SetValue(instance, value)
  else if IsField then
    AsField.SetValue(instance, value)
  else
    raise EInvalidOperationException.CreateRes(@SInvalidOperation_SetValue);
end;

function TRttiMemberHelper.GetIsPrivate: Boolean;
begin
  Result := Visibility = mvPrivate;
end;

function TRttiMemberHelper.GetIsProtected: Boolean;
begin
  Result := Visibility = mvProtected;
end;

function TRttiMemberHelper.GetIsPublic: Boolean;
begin
  Result := Visibility = mvPublic;
end;

function TRttiMemberHelper.GetIsPublished: Boolean;
begin
  Result := Visibility = mvPublished;
end;

function TRttiMemberHelper.GetIsReadable: Boolean;
begin
  Result := IsField or (IsProperty and TRttiProperty(Self).IsReadable)
    or (IsMethod and Assigned(TRttiMethod(Self).ReturnType));
end;

function TRttiMemberHelper.GetIsWritable: Boolean;
begin
  Result := IsField or (IsProperty and TRttiProperty(Self).IsWritable);
end;

function TRttiMemberHelper.GetMemberType: TRttiType;
begin
  if IsProperty then
    Result := AsProperty.PropertyType
  else if IsField then
    Result := AsField.FieldType
  else
    Result := AsMethod.ReturnType;
end;

function TRttiMemberHelper.GetIsConstructor: Boolean;
begin
  Result := (Self is TRttiMethod) and TRttiMethod(Self).IsConstructor;
end;

function TRttiMemberHelper.GetIsProperty: Boolean;
begin
  Result := Self is TRttiProperty;
end;

function TRttiMemberHelper.GetIsMethod: Boolean;
begin
  Result := Self is TRttiMethod;
end;

function TRttiMemberHelper.GetIsField: Boolean;
begin
  Result := Self is TRttiField;
end;

function TRttiMemberHelper.GetAsMethod: TRttiMethod;
begin
  Result := Self as TRttiMethod;
end;

function TRttiMemberHelper.GetAsProperty: TRttiProperty;
begin
  Result := Self as TRttiProperty;
end;

function TRttiMemberHelper.GetAsField: TRttiField;
begin
  Result := Self as TRttiField;
end;

{$ENDREGION}


{$REGION 'TRttiFieldHelper'}

function TRttiFieldHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := AsField.GetValue(instance.AsObject)
  else
    Result := AsField.GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiFieldHelper.SetValue(const instance, value: TValue);
var
  temp: TValue;
begin
  temp := value.Cast(FieldType.Handle);
  if instance.IsObject then
    SetValue(instance.AsObject, temp)
  else
    SetValue(instance.GetReferenceToRawData, temp);
end;

{$ENDREGION}


{$REGION 'TRttiPropertyHelper'}

function TRttiPropertyHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := GetValue(instance.AsObject)
  else
    Result := GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiPropertyHelper.SetValue(const instance, value: TValue);
var
  temp: TValue;
begin
  temp := value.Cast(PropertyType.Handle);
  if instance.IsObject then
    SetValue(instance.AsObject, temp)
  else
    SetValue(instance.GetReferenceToRawData, temp);
end;

{$ENDREGION}


{$REGION 'TRttiMethodHelper'}

function GetCodeAddress(const classType: TClass; const proc: Pointer): Pointer;
begin
  if (Integer(proc) and $FF000000) = $FF000000 then
    Exit(nil);
  if (Integer(proc) and $FF000000) = $FE000000 then
    Result := PPointer(Integer(classType) + SmallInt(proc))^
  else
    Result := proc;
end;

function TRttiMethodHelper.GetIsGetter: Boolean;
var
  prop: TRttiProperty;
  code: Pointer;
begin
  for prop in Parent.GetProperties do
    if prop is TRttiInstanceProperty then
    begin
      code := GetCodeAddress(prop.Parent.AsInstance.MetaclassType,
        TRttiInstanceProperty(prop).PropInfo.GetProc);
      if code = CodeAddress then
        Exit(True);
    end;
  Result := False;
end;

function TRttiMethodHelper.GetIsSetter: Boolean;
var
  prop: TRttiProperty;
  code: Pointer;
begin
  for prop in Parent.GetProperties do
    if prop is TRttiInstanceProperty then
    begin
      code := GetCodeAddress(prop.Parent.AsInstance.MetaclassType,
        TRttiInstanceProperty(prop).PropInfo.SetProc);
      if code = CodeAddress then
        Exit(True);
    end;
  Result := False;
end;

function TRttiMethodHelper.GetParameterCount: Integer;
begin
  Result := Length(GetParameters);
end;

function TRttiMethodHelper.GetParametersList: IReadOnlyList<TRttiParameter>;
begin
  Result := TArrayIterator<TRttiParameter>.Create(GetParameters());
end;

{$ENDREGION}


{$REGION 'TFiltersNamed<T>'}

class function TFiltersNamed<T>.IsNamed(const name: string): TSpecification<T>;
begin
  Result := TNameFilter<T>.Create(name);
end;

class function TFiltersNamed<T>.HasAttribute(attributeClass: TAttributeClass;
  inherit: Boolean = False): TSpecification<T>;
begin
  Result := THasAttributeFilter<T>.Create(attributeClass, inherit);
end;

{$ENDREGION}


{$REGION 'TFiltersBase<T>'}

class function TFiltersBase<T>.ContainsParameterType(
  typeInfo: PTypeInfo): TSpecification<T>;
begin
  Result := TContainsParameterTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.HasParameterTypes(
  const types: array of PTypeInfo): TSpecification<T>;
begin
  Result := THasParameterTypesFilter<T>.Create(types);
end;

class function TFiltersBase<T>.HasParameterFlags(
  const flags: TParamFlags): TSpecification<T>;
begin
  Result := THasParameterFlagsFilter<T>.Create(flags);
end;

class function TFiltersBase<T>.IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>;
begin
  Result := TTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.IsTypeOf<TType>: TSpecification<T>;
begin
  Result := IsTypeOf(TypeInfo(TType));
end;

class function TFiltersBase<T>.IsClassMethod: TSpecification<T>;
begin
  Result := TClassMethodFilter<T>.Create;
end;

class function TFiltersBase<T>.IsConstructor: TSpecification<T>;
begin
  Result := TConstructorFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInstanceMethod: TSpecification<T>;
begin
  Result := TInstanceMethodFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInvokable: TSpecification<T>;
begin
  Result := TInvokableFilter<T>.Create;
end;

class function TFiltersBase<T>.IsMethodKind(
  const kinds: TMethodKinds): TSpecification<T>;
begin
  Result := TMethodKindFilter<T>.Create(kinds);
end;

{$ENDREGION}


{$REGION 'TTypeFilters'}

class function TTypeFilters.IsClass: TSpecification<TRttiType>;
begin
  Result := TIsClassFilter.Create;
end;

class function TTypeFilters.IsInterface: TSpecification<TRttiType>;
begin
  Result := TIsInterfaceFilter.Create;
end;

{$ENDREGION}


{$REGION 'TParameterFilters'}

class function TParameterFilters.HasFlags(
  flags: TParamFlags): TSpecification<TRttiParameter>;
begin
  Result := THasFlagsFilter.Create(flags);
end;

{$ENDREGION}


{$REGION 'Filters'}

{ THasAttributeFilter<T> }

constructor THasAttributeFilter<T>.Create(attributeClass: TAttributeClass;
  inherit: Boolean);
begin
  inherited Create;
  fAttributeClass := attributeClass;
  fInherit := inherit;
end;

function THasAttributeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.HasCustomAttribute(fAttributeClass, fInherit);
end;

{ TNameFilter<T> }

constructor TNameFilter<T>.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

function TNameFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := SameText(TRttiNamedObject(member).Name, fName);
end;

{ TTypeFilter<T> }

constructor TTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  if member.IsProperty then
    Result := member.AsProperty.PropertyType.Handle = fTypeInfo
  else if member.IsField then
    Result := member.AsField.FieldType.Handle = fTypeInfo
  else
    Result := False;
end;

{ THasParameterTypesFilter<T> }

constructor THasParameterTypesFilter<T>.Create(const types: array of PTypeInfo);
var
  i: Integer;
begin
  inherited Create;
  SetLength(fTypes, Length(types));
  for i := Low(types) to High(types) do
    fTypes[i] := types[i];
end;

function THasParameterTypesFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := member.AsMethod.GetParameters;
  Result := Length(parameters) = Length(fTypes);
  if Result then
    for i := Low(parameters) to High(parameters) do
      if Assigned(fTypes[i])
        and not IsAssignableFrom(parameters[i].ParamType.Handle, fTypes[i]) then
        Exit(False);
end;

{ TContainsParameterTypeFilter<T> }

constructor TContainsParameterTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TContainsParameterTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
      if parameter.ParamType.Handle = fTypeInfo then
        Exit(True);
  end;
end;

{ THasParameterFlagsFilter<T> }

constructor THasParameterFlagsFilter<T>.Create(const flags: TParamFlags);
begin
  inherited Create;
  fFlags := flags;
end;

function THasParameterFlagsFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
      if parameter.Flags * fFlags <> [] then
        Exit(True);
  end;
end;

{ TMethodKindFilter<T> }

constructor TMethodKindFilter<T>.Create(const flags: TMethodKinds);
begin
  inherited Create;
  fFlags := flags;
end;

function TMethodKindFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
{$IFDEF DELPHI2010}
  // explicit cast to prevent the compiler from choking
  Result := TRttiMember(member).IsMethod and (TRttiMember(member).AsMethod.MethodKind in fFlags);
{$ELSE}
  Result := member.IsMethod and (member.AsMethod.MethodKind in fFlags);
{$ENDIF}
end;

{ TInvokableFilter<T> }

function TInvokableFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  if member.IsProperty then
    Result := member.AsProperty.IsWritable
  else if member.IsMethod then
    Result := not (member.AsMethod.MethodKind in [mkClassConstructor, mkClassDestructor])
  else
    Result := True;
end;

{ TMemberTypeFilter<T> }

constructor TMemberTypeFilter<T>.Create(memberClass: TRttiMemberClass);
begin
  inherited Create;
  fMemberClass := memberClass;
end;

function TMemberTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.InheritsFrom(fMemberClass);
end;

{ TConstructorFilter<T> }

function TConstructorFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsConstructor;
end;

{ TInstanceMethodFilter<T> }

function TInstanceMethodFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsMethod and not member.AsMethod.IsClassMethod;
end;

{ TClassMethodFilter<T> }

function TClassMethodFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsMethod and member.AsMethod.IsClassMethod;
end;

{ TIsClassFilter }

function TIsClassFilter.IsSatisfiedBy(const member: TRttiType): Boolean;
begin
  Result := member.IsInstance;
end;

{ TIsInterfaceFilter }

function TIsInterfaceFilter.IsSatisfiedBy(const member: TRttiType): Boolean;
begin
  Result := member is TRttiInterfaceType;
end;

{ THasFlagsFilter }

constructor THasFlagsFilter.Create(flags: TParamFlags);
begin
  inherited Create;
  fFlags := flags;
end;

function THasFlagsFilter.IsSatisfiedBy(
  const parameter: TRttiParameter): Boolean;
begin
  Result := parameter.Flags * fFlags = fFlags;
end;

{$ENDREGION}


{$REGION 'TReflection'}

class constructor TReflection.Create;
begin
  fContext := TRttiContext.Create;
end;

class destructor TReflection.Destroy;
begin
  fContext.Free;
end;

function TReflection.FindType(const qualifiedName: string): TRttiType;
begin
  Result := fContext.FindType(qualifiedName);
end;

function TReflection.GetClasses: IEnumerable<TRttiInstanceType>;
begin
  Result := TRttiTypeIterator<TRttiInstanceType>.Create;
end;

function TReflection.GetFullName(const typeInfo: PTypeInfo): string;
var
  t: TRttiType;
begin
  t := fContext.GetType(typeInfo);
  if t = nil then
    Exit('');
  if t.IsPublicType then
    Result := t.QualifiedName
  else
    Result := t.Name;
end;

function TReflection.GetInterfaces: IEnumerable<TRttiInterfaceType>;
begin
  Result := TRttiTypeIterator<TRttiInterfaceType>.Create;
end;

function TReflection.GetType(const typeInfo: PTypeInfo): TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(typeInfo, 'typeInfo');
{$ENDIF}

  Result := fContext.GetType(typeInfo);
end;

function TReflection.GetType(const classType: TClass): TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(classType, 'classType');
{$ENDIF}

  Result := fContext.GetType(classType.ClassInfo);
end;

function TReflection.GetType(const instance: TObject): TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(instance, 'instance');
{$ENDIF}

  Result := fContext.GetType(instance.ClassInfo);
end;

function TReflection.GetType(const instance: TValue): TRttiType;
begin
  Result := fContext.GetType(instance.TypeInfo);
end;

function TReflection.GetTypes: IEnumerable<TRttiType>;
begin
  Result := TRttiTypeIterator<TRttiType>.Create;
end;

{$ENDREGION}


end.
