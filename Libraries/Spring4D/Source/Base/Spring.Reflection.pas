{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
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
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Extensions,
  Spring.Patterns.Specification;

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
    class function GetClasses: IEnumerable<TRttiInstanceType>; static;
    class function GetInterfaces: IEnumerable<TRttiInterfaceType>; static;
    class function GetTypes: IEnumerable<TRttiType>; static;

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

    class property Classes: IEnumerable<TRttiInstanceType> read GetClasses;
    class property Interfaces: IEnumerable<TRttiInterfaceType> read GetInterfaces;
    class property Types: IEnumerable<TRttiType> read GetTypes;
  end;

  {$ENDREGION}


  {$REGION 'TRttiTypeIterator<T>'}

  TRttiTypeIterator<T: TRttiType> = class(TIterator<T>, IEnumerable<T>)
  private
    fContext: TRttiContext;
    fIndex: Integer;
    fTypes: TArray<TRttiType>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
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

  TRttiTypeHelper = class helper for TRttiType
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
    function GetMethodsInternal: IReadOnlyList<TRttiMethod>;
    function GetPropertiesInternal: IReadOnlyList<TRttiProperty>;
    function GetFieldsInternal: IReadOnlyList<TRttiField>;
    function GetBaseTypes: IReadOnlyList<TRttiType>;
    function GetConstructorsInternal: IReadOnlyList<TRttiMethod>;
    function GetDefaultName: string;
    function GetAncestorCount: Integer;
    function GetDeclaringUnitName: string;
  public

    /// <summary>
    ///   Returns all attributes specified on the type and if specified also of
    ///   its ancestor types (for classes and interfaces).
    /// </summary>
    function GetAttributes(inherit: Boolean = False): TArray<TCustomAttribute>;

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
    function GetInterfaces: IReadOnlyList<TRttiInterfaceType>;

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
    property Constructors: IReadOnlyList<TRttiMethod> read GetConstructorsInternal;

    /// <summary>
    ///   Gets a enumerable collection which contains all methods that the type
    ///   contains, including inherited.
    /// </summary>
    /// <seealso cref="Constructors" />
    /// <seealso cref="Properties" />
    /// <seealso cref="Fields" />
    property Methods: IReadOnlyList<TRttiMethod> read GetMethodsInternal;

    /// <summary>
    ///   Gets a enumerable collection which contains all properties that the
    ///   type contains, including inherited.
    /// </summary>
    /// <seealso cref="Constructors" />
    /// <seealso cref="Methods" />
    /// <seealso cref="Fields" />
    property Properties: IReadOnlyList<TRttiProperty> read GetPropertiesInternal;

    /// <summary>
    ///   Gets a enumerable collection which contains all fields that the type
    ///   contains, including inherited.
    /// </summary>
    /// <seealso cref="Constructors" />
    /// <seealso cref="Methods" />
    /// <seealso cref="Properties" />
    property Fields: IReadOnlyList<TRttiField> read GetFieldsInternal;

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
    function GetIsPrivate: Boolean; inline;
    function GetIsProtected: Boolean; inline;
    function GetIsPublic: Boolean; inline;
    function GetIsPublished: Boolean; inline;
    function GetIsConstructor: Boolean; inline;
    function GetIsProperty: Boolean; inline;
    function GetIsMethod: Boolean; inline;
    function GetIsField: Boolean; inline;
    function GetAsMethod: TRttiMethod; inline;
    function GetAsProperty: TRttiProperty; inline;
    function GetAsField: TRttiField; inline;
    function GetMemberType: TRttiType;
    function GetIsReadable: Boolean;
    function GetIsWritable: Boolean;
  public
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
    class function IsNamed(const name: string): Specification<T>;
    class function HasAttribute(attributeClass: TAttributeClass;
      inherit: Boolean = False): Specification<T>;
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
    class function ContainsParameterType(typeInfo: PTypeInfo): Specification<T>;
    class function HasParameterTypes(const types: array of PTypeInfo): Specification<T>;
    class function HasParameterFlags(const flags: TParamFlags): Specification<T>;
    class function IsTypeOf<TType>: Specification<T>; overload;
    class function IsTypeOf(typeInfo: PTypeInfo): Specification<T>; overload;
    class function IsConstructor: Specification<T>;
    class function IsInstanceMethod: Specification<T>;
    class function IsClassMethod: Specification<T>;
    class function IsMethodKind(const kinds: TMethodKinds): Specification<T>;
    class function IsInvokable: Specification<T>;
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
    class function IsClass: Specification<TRttiType>;
    class function IsInterface: Specification<TRttiType>;
  end;
  TParameterFilters = class(TFiltersNamed<TRttiParameter>)
  public
    class function HasFlags(flags: TParamFlags): Specification<TRttiParameter>;
  end;

  {$ENDREGION}


  {$REGION 'TNameFilter<T>'}

  TNameFilter<T: TRttiNamedObject> = class(TSpecification<T>)
  private
    fName: string;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const name: string);
  end;

  {$ENDREGION}


  {$REGION 'TInvokableFilter<T>'}

  TInvokableFilter<T: TRttiMember> = class(TSpecification<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasAttributeFilter<T>'}

  THasAttributeFilter<T: TRttiObject> = class(TSpecification<T>)
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

  TTypeFilter<T: TRttiMember> = class(TSpecification<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'THasParameterTypesFilter<T>'}

  THasParameterTypesFilter<T: TRttiMember> = class(TSpecification<T>)
  private
    fTypes: TArray<PTypeInfo>;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const types: array of PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'TContainsParameterTypeFilter<T>'}

  TContainsParameterTypeFilter<T: TRttiMember> = class(TSpecification<T>)
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

  TMemberTypeFilter<T: TRttiMember> = class(TSpecification<T>)
  private
    fMemberClass: TRttiMemberClass;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(memberClass: TRttiMemberClass);
  end;

  {$ENDREGION}


  {$REGION 'TConstructorFilter<T>'}

  TConstructorFilter<T: TRttiMember> = class(TSpecification<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TInstanceMethodFilter<T>'}

  TInstanceMethodFilter<T: TRttiMember> = class(TSpecification<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TClassMethodFilter<T>'}

  TClassMethodFilter<T: TRttiMember> = class(TSpecification<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasParameterFlagsFilter<T>'}

  THasParameterFlagsFilter<T: TRttiMember> = class(TSpecification<T>)
  private
    fFlags: TParamFlags;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const flags: TParamFlags);
  end;

  {$ENDREGION}


  {$REGION 'TMethodKindFilter<T>'}

  TMethodKindFilter<T: TRttiMember> = class(TSpecification<T>)
  private
    fFlags: TMethodKinds;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const flags: TMethodKinds);
  end;

  {$ENDREGION}


  {$REGION 'TIsClassFilter'}

  TIsClassFilter = class(TSpecification<TRttiType>)
  protected
    function IsSatisfiedBy(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TIsInterfaceFilter'}

  TIsInterfaceFilter = class(TSpecification<TRttiType>)
  protected
    function IsSatisfiedBy(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasFlagsFilter'}

  THasFlagsFilter = class(TSpecification<TRttiParameter>)
  private
    fFlags: TParamFlags;
  protected
    function IsSatisfiedBy(const parameter: TRttiParameter): Boolean; override;
  public
    constructor Create(flags: TParamFlags);
  end;

  {$ENDREGION}


  {$REGION 'Routines'}

procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue;
  var ArgDest: TValue; CC: TCallConv);

  {$ENDREGION}


implementation

uses
  RTLConsts,
  StrUtils,
  SysUtils,
  Spring.ResourceStrings;

const
  EmptyGuid: TGUID = ();


{$REGION 'Routines'}

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

class function TType.GetClasses: IEnumerable<TRttiInstanceType>;
begin
  Result := TRttiTypeIterator<TRttiInstanceType>.Create;
end;

class function TType.GetInterfaces: IEnumerable<TRttiInterfaceType>;
begin
  Result := TRttiTypeIterator<TRttiInterfaceType>.Create;
end;

class function TType.GetTypes: IEnumerable<TRttiType>;
begin
  Result := TRttiTypeIterator<TRttiType>.Create;
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
begin
  Result := IsMethodReference(typeInfo);
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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(instance, 'instance');
{$ENDIF}

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
          if item.IsInterface and TRttiInterfaceType(item).HasGuid then
            fInterfaceTypes.TryAdd(TRttiInterfaceType(item).GUID, TRttiInterfaceType(item));
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

procedure TRttiTypeIterator<T>.Dispose;
begin
  fTypes := nil;
end;

procedure TRttiTypeIterator<T>.Start;
begin
  fTypes := fContext.GetTypes;
end;

function TRttiTypeIterator<T>.TryMoveNext(var current: T): Boolean;
var
  typ: TRttiType;
begin
  while fIndex < Length(fTypes) do
  begin
    typ := fTypes[fIndex];
    Inc(fIndex);
    if typ.InheritsFrom(T) then
    begin
      current := T(typ);
      Exit(True);
    end;
  end;
  Result := False;
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

function TRttiTypeHelper.GetAttributes(
  inherit: Boolean): TArray<TCustomAttribute>;
var
  flat: TArray<TArray<TCustomAttribute>>;
  t: TRttiType;
  depth: Integer;
begin
  if inherit then
  begin
    t := Self;
    depth := 0;
    while t <> nil do
    begin
      Inc(depth);
      t := t.BaseType;
    end;
  end
  else
    depth := 1;

  SetLength(flat, depth);
  t := Self;
  depth := 0;
  while t <> nil do
  begin
    flat[depth] := TRttiObject(t).GetAttributes;
    if not inherit then
      Break;
    Inc(depth);
    t := t.BaseType;
  end;

  Result := TArray.Concat<TCustomAttribute>(flat);
end;

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

function TRttiTypeHelper.GetConstructorsInternal: IReadOnlyList<TRttiMethod>;
begin
  Result := TEnumerable.From<TRttiMethod>(GetConstructors);
end;

function TRttiTypeHelper.GetMethodsInternal: IReadOnlyList<TRttiMethod>;
begin
  Result := TEnumerable.From<TRttiMethod>(GetMethods);
end;

function TRttiTypeHelper.GetPropertiesInternal: IReadOnlyList<TRttiProperty>;
begin
  Result := TEnumerable.From<TRttiProperty>(GetProperties);
end;

function TRttiTypeHelper.GetFieldsInternal: IReadOnlyList<TRttiField>;
begin
  Result := TEnumerable.From<TRttiField>(GetFields);
end;

function TRttiTypeHelper.GetDeclaringUnitName: string;
begin
  case TypeKind of
    tkClass: Result := TRttiInstanceType(Self).DeclaringUnitName;
    tkInterface: Result := TRttiInterfaceType(Self).DeclaringUnitName;
    tkDynArray: Result := TRttiDynamicArrayType(Self).DeclaringUnitName;
  else
    Result := '';
  end;
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

  Result := TEnumerable.From<TRttiType>(types);
end;

function TRttiTypeHelper.GetInterfaces: IReadOnlyList<TRttiInterfaceType>;
var
  guids: ISet<TGUID>;
  list: IList<TRttiInterfaceType>;
  classType: TClass;
  table: PInterfaceTable;
  p: PPPTypeInfo;
  intfType: TRttiInterfaceType;
  i: Integer;
begin
  if IsClass then
  begin
    guids := TCollections.CreateSet<TGUID>;
    list := TCollections.CreateList<TRttiInterfaceType>;
    classType := AsInstance.MetaclassType;
    while Assigned(classType) do
    begin
      table := classType.GetInterfaceTable;
      if Assigned(table) then
      begin
        p := @table.Entries[table.EntryCount];
        for i := 0 to table.EntryCount - 1 do //FI:W528
        begin
          intfType := TType.GetType(p^^).AsInterface;
          list.Add(intfType);
          Inc(p);
        end;
      end;
      classType := classType.ClassParent;
    end;
    Result := list.AsReadOnly;
  end
  else
  if IsInterface then
  begin
    guids := TCollections.CreateSet<TGUID>;
    list := TCollections.CreateList<TRttiInterfaceType>;
    intfType := AsInterface;
    while Assigned(intfType) do
    begin
      if intfType.HasGuid and (intfType.GUID <> EmptyGuid)
        and guids.Add(intfType.GUID) then
        list.Add(intfType);
      intfType := intfType.BaseType;
    end;
    Result := list.AsReadOnly;
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
  genericTypeDefinition, declaringUnitName: string;
  declaringUnitNameLength: Integer;
  baseType: TRttiType;
begin
  if not IsGenericType then
    Exit(False);
  genericTypeDefinition := GetGenericTypeDefinition;
  if SameText(genericTypeDefinition, genericType) then
    Exit(True);

  if TypeKind in [tkClass, tkInterface, tkDynArray] then
  begin
    declaringUnitName := GetDeclaringUnitName;
    declaringUnitNameLength := Length(declaringUnitName);
    if (Length(genericType) - declaringUnitNameLength - Length(genericTypeDefinition) = 1)
      and (genericType[declaringUnitNameLength + 1] = '.')
      and StartsText(declaringUnitName, genericType)
      and EndsText(genericTypeDefinition, genericType) then
      Exit(True);
  end;

  baseType := Self.BaseType;
  Result := Assigned(baseType) and baseType.IsGenericTypeOf(genericType);
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
      code := GetCodeAddress(TRttiInstanceType(prop.Parent).MetaclassType,
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
      code := GetCodeAddress(TRttiInstanceType(prop.Parent).MetaclassType,
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
  Result := TEnumerable.From<TRttiParameter>(GetParameters);
end;

{$ENDREGION}


{$REGION 'TFiltersNamed<T>'}

class function TFiltersNamed<T>.IsNamed(const name: string): Specification<T>;
begin
  Result := TNameFilter<T>.Create(name);
end;

class function TFiltersNamed<T>.HasAttribute(attributeClass: TAttributeClass;
  inherit: Boolean = False): Specification<T>;
begin
  Result := THasAttributeFilter<T>.Create(attributeClass, inherit);
end;

{$ENDREGION}


{$REGION 'TFiltersBase<T>'}

class function TFiltersBase<T>.ContainsParameterType(
  typeInfo: PTypeInfo): Specification<T>;
begin
  Result := TContainsParameterTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.HasParameterTypes(
  const types: array of PTypeInfo): Specification<T>;
begin
  Result := THasParameterTypesFilter<T>.Create(types);
end;

class function TFiltersBase<T>.HasParameterFlags(
  const flags: TParamFlags): Specification<T>;
begin
  Result := THasParameterFlagsFilter<T>.Create(flags);
end;

class function TFiltersBase<T>.IsTypeOf(typeInfo: PTypeInfo): Specification<T>;
begin
  Result := TTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.IsTypeOf<TType>: Specification<T>;
begin
  Result := IsTypeOf(TypeInfo(TType));
end;

class function TFiltersBase<T>.IsClassMethod: Specification<T>;
begin
  Result := TClassMethodFilter<T>.Create;
end;

class function TFiltersBase<T>.IsConstructor: Specification<T>;
begin
  Result := TConstructorFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInstanceMethod: Specification<T>;
begin
  Result := TInstanceMethodFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInvokable: Specification<T>;
begin
  Result := TInvokableFilter<T>.Create;
end;

class function TFiltersBase<T>.IsMethodKind(
  const kinds: TMethodKinds): Specification<T>;
begin
  Result := TMethodKindFilter<T>.Create(kinds);
end;

{$ENDREGION}


{$REGION 'TTypeFilters'}

class function TTypeFilters.IsClass: Specification<TRttiType>;
begin
  Result := TIsClassFilter.Create;
end;

class function TTypeFilters.IsInterface: Specification<TRttiType>;
begin
  Result := TIsInterfaceFilter.Create;
end;

{$ENDREGION}


{$REGION 'TParameterFilters'}

class function TParameterFilters.HasFlags(
  flags: TParamFlags): Specification<TRttiParameter>;
begin
  Result := THasFlagsFilter.Create(flags);
end;

{$ENDREGION}


{$REGION 'Filters'}

{ THasAttributeFilter<T> }

constructor THasAttributeFilter<T>.Create(attributeClass: TAttributeClass;
  inherit: Boolean);
begin
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
  fName := name;
end;

function TNameFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := SameText(TRttiNamedObject(member).Name, fName);
end;

{ TTypeFilter<T> }

constructor TTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  fTypeInfo := typeInfo;
end;

function TTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  if member.IsProperty then
    Result := TRttiProperty(member).PropertyType.Handle = fTypeInfo
  else if member.IsField then
    Result := TRttiField(member).FieldType.Handle = fTypeInfo
  else
    Result := False;
end;

{ THasParameterTypesFilter<T> }

constructor THasParameterTypesFilter<T>.Create(const types: array of PTypeInfo);
var
  i: Integer;
begin
  SetLength(fTypes, Length(types));
  for i := Low(types) to High(types) do
    fTypes[i] := types[i];
end;

function THasParameterTypesFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  if not member.IsMethod then
    Exit(False);

  parameters := TRttiMethod(member).GetParameters;
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
  fTypeInfo := typeInfo;
end;

function TContainsParameterTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
    for parameter in TRttiMethod(member).GetParameters do
      if parameter.ParamType.Handle = fTypeInfo then
        Exit(True);
end;

{ THasParameterFlagsFilter<T> }

constructor THasParameterFlagsFilter<T>.Create(const flags: TParamFlags);
begin
  fFlags := flags;
end;

function THasParameterFlagsFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameter: TRttiParameter;
begin
  if member.IsMethod then
    for parameter in TRttiMethod(member).GetParameters do
      if parameter.Flags * fFlags <> [] then
        Exit(True);
  Result := False;
end;

{ TMethodKindFilter<T> }

constructor TMethodKindFilter<T>.Create(const flags: TMethodKinds);
begin
  fFlags := flags;
end;

function TMethodKindFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsMethod and (TRttiMethod(member).MethodKind in fFlags);
end;

{ TInvokableFilter<T> }

function TInvokableFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  if member.IsProperty then
    Result := TRttiProperty(member).IsWritable
  else if member.IsMethod then
    Result := not (TRttiMethod(member).MethodKind in [mkClassConstructor, mkClassDestructor])
  else
    Result := True;
end;

{ TMemberTypeFilter<T> }

constructor TMemberTypeFilter<T>.Create(memberClass: TRttiMemberClass);
begin
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
  Result := member.IsMethod and not TRttiMethod(member).IsClassMethod;
end;

{ TClassMethodFilter<T> }

function TClassMethodFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsMethod and TRttiMethod(member).IsClassMethod;
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
  fFlags := flags;
end;

function THasFlagsFilter.IsSatisfiedBy(
  const parameter: TRttiParameter): Boolean;
begin
  Result := parameter.Flags * fFlags = fFlags;
end;

{$ENDREGION}


end.
