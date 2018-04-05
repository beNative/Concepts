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

/// <summary>
///   Declares the fundamental types for the <see href="http://www.spring4d.org">
///   Spring4D</see> Framework.
/// </summary>
unit Spring;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes,
  Diagnostics,
  Generics.Collections,
  Generics.Defaults,
  Rtti,
  SyncObjs,
  SysUtils,
  TimeSpan,
  Types,
  TypInfo,
  Variants;

type

  {$REGION 'Type redefinitions'}

  TBytes = SysUtils.TBytes;
  TByteSet = set of Byte;

  TStringDynArray = Types.TStringDynArray;

  TTimeSpan = TimeSpan.TTimeSpan;

  TStopwatch = Diagnostics.TStopwatch;

  PTypeInfo = TypInfo.PTypeInfo;
  PPPTypeInfo = ^PPTypeInfo;

  PInterface = ^IInterface;

  TValue = Rtti.TValue;
  PValue = ^TValue;

  TAttributeClass = class of TCustomAttribute;

{$IFDEF DELPHI2010}
  TThreadID = LongWord;

  PNativeInt = ^NativeInt;
  PNativeUInt = ^NativeUInt;
{$ENDIF}

{$IFNDEF DELPHIXE2_UP}
  IntPtr = NativeInt;
  UIntPtr = NativeUInt;
{$ENDIF}

  PObject = ^TObject;

  {$ENDREGION}


  {$REGION 'TGuidHelper'}

{$IFDEF DELPHI2010}
  TGuidHelper = record helper for TGUID
  public
    class function Create(const B: TBytes): TGUID; overload; static;
    class function Create(const S: string): TGUID; overload; static;
    class function Create(A: Integer; B: SmallInt; C: SmallInt; const D: TBytes): TGUID; overload; static;

    class function &&op_Equality(const left, right: TGUID): Boolean; static;
    class function &&op_Inequality(const left, right: TGUID): Boolean; static; inline;
    class function Empty: TGuid; static;
    class function NewGuid: TGuid; static;

    function ToByteArray: TBytes;
    function ToString: string;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TMethodHelper'}

{$IFNDEF DELPHIXE3_UP}
  TMethodHelper = record helper for TMethod
  public
    class function &&op_Equality(const left, right: TMethod): Boolean; static; inline;
    class function &&op_Inequality(const left, right: TMethod): Boolean; static; inline;
    class function &&op_GreaterThan(const left, right: TMethod): Boolean; static; inline;
    class function &&op_LessThan(const left, right: TMethod): Boolean; static; inline;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TType'}

  TType = class
  private
    class var fContext: TRttiContext;
  public
    class constructor Create;
    class destructor Destroy;

    class function HasWeakRef<T>: Boolean; inline; static;
    class function IsManaged<T>: Boolean; inline; static;
    class function Kind<T>: TTypeKind; inline; static;

    class function GetType<T>: TRttiType; overload; static; inline;
    class function GetType(typeInfo: PTypeInfo): TRttiType; overload; static;
    class function GetType(classType: TClass): TRttiInstanceType; overload; static;

    class property Context: TRttiContext read fContext;
  end;

  {$ENDREGION}


  {$REGION 'TEnum'}

  /// <summary>
  ///   Provides static methods to manipulate an enumeration type.
  /// </summary>
  /// <remarks>
  ///   This does only work for enum types that have type info. Discontiguous
  ///   enumerations and enumerations which don't start at zero have no type
  ///   info. See: <see href="http://stackoverflow.com/questions/1420562/why-do-i-get-type-has-no-typeinfo-error-with-an-enum-type" />
  /// </remarks>
  TEnum = class
  public
    class function ToInteger<T>(const value: T): Integer; static; inline;
    class function IsValid<T>(const value: Integer): Boolean; overload; static;
    class function IsValid<T>(const value: T): Boolean; overload; static;
    class function GetName<T>(const value: Integer): string; overload; static;
    class function GetName<T>(const value: T): string; overload; static;
    class function GetNames<T>: TStringDynArray; static;
    class function GetValue<T>(const value: string): Integer; overload; static;
    class function GetValue<T>(const value: T): Integer; overload; static;
    class function GetValues<T>: TIntegerDynArray; static;
    class function TryParse<T>(const value: Integer; out enum: T): Boolean; overload; static;
    class function TryParse<T>(const value: string; out enum: T): Boolean; overload; static;
    class function Parse<T>(const value: Integer): T; overload; static;
    class function Parse<T>(const value: string): T; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'TActivator'}

  IObjectActivator = interface
    ['{CE05FB89-3467-449E-81EA-A5AEECAB7BB8}']
    function CreateInstance: TValue;
  end;

  TConstructor = function(InstanceOrVMT: Pointer; Alloc: ShortInt = 1): Pointer;

  TActivator = record
  private
    class var ConstructorCache: TDictionary<PTypeInfo,TConstructor>;
    class function FindConstructor(const classType: TRttiInstanceType;
      const arguments: array of TValue): TRttiMethod; overload; static;
    class procedure RaiseNoConstructorFound(classType: TClass); static;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure ClearCache; static;

    class function CreateInstance(const classType: TRttiInstanceType): TValue; overload; static;
    class function CreateInstance(const classType: TRttiInstanceType;
      const arguments: array of TValue): TValue; overload; static;
    class function CreateInstance(const classType: TRttiInstanceType;
      const constructorMethod: TRttiMethod; const arguments: array of TValue): TValue; overload; static;

    class function CreateInstance(typeInfo: PTypeInfo): TObject; overload; static; inline;
    class function CreateInstance(const typeName: string): TObject; overload; static; inline;
    class function CreateInstance(const typeName: string;
      const arguments: array of TValue): TObject; overload; static;

    class function CreateInstance(classType: TClass): TObject; overload; static;
    class function CreateInstance(classType: TClass;
      const arguments: array of TValue): TObject; overload; static;

    class function CreateInstance<T: class>: T; overload; static; inline;
    class function CreateInstance<T: class>(
      const arguments: array of TValue): T; overload; static;

    class function FindConstructor(classType: TClass): TConstructor; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'Attributes'}

  TBaseAttribute = class(TCustomAttribute)
  strict protected
    constructor Create;
  end;

{$IFDEF DELPHIXE3_UP}
  DefaultAttribute = Classes.DefaultAttribute;
{$ELSE}
  DefaultAttribute = class(TBaseAttribute)
  strict protected
    fValue: Variant;
  public
    constructor Create(const defaultValue: Boolean); overload;
    constructor Create(const defaultValue: Integer); overload;
    constructor Create(const defaultValue: Cardinal); overload;
    constructor Create(const defaultValue: Int64); overload;
    constructor Create(const defaultValue: UInt64); overload;
    constructor Create(const defaultValue: string); overload;
    constructor Create(const defaultValue: Extended); overload;
    property Value: Variant read fValue;
  end;
{$ENDIF}

  /// <summary>
  ///   This attribute marks automatically initialized interface or object
  ///   fields inside of classes that inherit from TManagedObject or are using
  ///   the mechanism provided by TInitTable.
  /// </summary>
  /// <remarks>
  ///   Because of limited RTTI in Delphi 2010 interface fields are only
  ///   supported when the interface type has a GUID.
  /// </remarks>
  ManagedAttribute = class(TBaseAttribute)
  strict private
    fCreateInstance: Boolean;
    fInstanceClass: TClass;
    fFactory: TFunc<PTypeInfo,Pointer>;
  strict protected
    constructor Create(const factory: TFunc<PTypeInfo,Pointer>); overload;
  public
    constructor Create(createInstance: Boolean = True); overload;
    constructor Create(instanceClass: TClass) overload;

    property CreateInstance: Boolean read fCreateInstance;
    property InstanceClass: TClass read fInstanceClass;
    property Factory: TFunc<PTypeInfo,Pointer> read fFactory;
  end;

  {$ENDREGION}


  {$REGION 'TInitTable'}

  TInitTable = class
  strict private type
    TInitializableField = class abstract
    public
      procedure InitializeValue(instance: Pointer); virtual; abstract;
    end;

    TDefaultField<T> = class(TInitializableField)
    strict private type
      PT = ^T;
    private
      fOffset: Integer;
      fValue: T;
    public
      constructor Create(offset: Integer; const value: Variant);
      procedure InitializeValue(instance: Pointer); override; final;
    end;

    TDefaultProperty<T> = class(TInitializableField)
    strict private type
      TGetter = function: T of object;
      TIndexedGetter = function(index: Integer): T of object;
      TSetter = procedure(const value: T) of object;
      TIndexedSetter = procedure(index: Integer; const value: T) of object;
    var
      fPropInfo: PPropInfo;
      fValue: T;
    public
      constructor Create(propInfo: PPropInfo; const value: Variant);
      procedure InitializeValue(instance: Pointer); override; final;
    end;

    TFinalizableField = class abstract(TInitializableField)
    public
      procedure FinalizeValue(instance: Pointer); virtual; abstract;
    end;

    TManagedObjectField = class(TFinalizableField)
    private
      fOffset: Integer;
      fFieldType: PTypeInfo;
      fCls: TClass;
      fCtor: TConstructor;
      fFactory: TFunc<PTypeInfo,Pointer>;
    public
      constructor Create(offset: Integer; fieldType: PTypeInfo; cls: TClass;
        const factory: TFunc<PTypeInfo,Pointer>);
      procedure InitializeValue(instance: Pointer); override;
      procedure FinalizeValue(instance: Pointer); override;
    end;

    TManagedInterfaceField = class(TManagedObjectField)
    private
      fEntry: PInterfaceEntry;
      function CreateInstance: Pointer;
    public
      constructor Create(offset: Integer; fieldType: PTypeInfo; cls: TClass;
        const factory: TFunc<PTypeInfo,Pointer>; entry: PInterfaceEntry);
      procedure InitializeValue(instance: Pointer); override;
      procedure FinalizeValue(instance: Pointer); override;
    end;

  const
  {$IF SizeOf(Pointer) = 4}
    PROPSLOT_MASK    = $FF000000;
    PROPSLOT_FIELD   = $FF000000;
    PROPSLOT_VIRTUAL = $FE000000;
  {$ELSEIF SizeOf(Pointer) = 8}
    PROPSLOT_MASK    = $FF00000000000000;
    PROPSLOT_FIELD   = $FF00000000000000;
    PROPSLOT_VIRTUAL = $FE00000000000000;
  {$ELSE OTHER_PTR_SIZE}
  {$MESSAGE Fatal 'Unrecognized pointer size'}
  {$IFEND OTHER_PTR_SIZE}
  strict private
    DefaultFields: TArray<TInitializableField>;
    ManagedFields: TArray<TFinalizableField>;
    DefaultFieldCount: Integer;
    ManagedFieldCount: Integer;
  private class var
{$IFDEF USE_VMTAUTOTABLE}
    InitTables: TObjectList<TInitTable>;
{$ELSE}
    InitTables: TDictionary<TClass,TInitTable>;
{$ENDIF}
    FormatSettings: TFormatSettings;
    procedure AddDefaultField(fieldType: PTypeInfo; const value: Variant;
      offset: Integer);
    procedure AddDefaultProperty(fieldType: PTypeInfo; const value: Variant;
      propInfo: PPropInfo);
    procedure AddManagedField(const field: TRttiField; const attribute: ManagedAttribute);
    class function GetCodePointer(instance: TObject; p: Pointer): Pointer; static; inline;
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create(classType: TClass);
    destructor Destroy; override;

    procedure InitInstance(instance: Pointer);
  {$IFNDEF AUTOREFCOUNT}
    procedure CleanupInstance(instance: Pointer);
  {$ENDIF}
  end;

  {$ENDREGION}


  {$REGION 'TManagedObject'}

  TManagedObject = class(TObject)
  public
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
  {$IFNDEF AUTOREFCOUNT}
    procedure FreeInstance; override;
  {$ENDIF}
  end;

  TManagedInterfacedObject = class(TInterfacedObject)
  public
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
  {$IFNDEF AUTOREFCOUNT}
    procedure FreeInstance; override;
  {$ENDIF}
  end;

  {$ENDREGION}


  {$REGION 'TCollectionChangedAction'}

  /// <summary>
  ///   Describes the action that caused a CollectionChanged event.
  /// </summary>
  TCollectionChangedAction = (
    /// <summary>
    ///   An item was added to the collection.
    /// </summary>
    caAdded,

    /// <summary>
    ///   An item was removed from the collection.
    /// </summary>
    caRemoved,

    /// <summary>
    ///   An item was removed from the collection without considering
    ///   ownership.
    /// </summary>
    caExtracted,

    /// <summary>
    ///   An item was replaced in the collection.
    /// </summary>
    caReplaced,

    /// <summary>
    ///   An item was moved within the collection.
    /// </summary>
    caMoved,

    /// <summary>
    ///   The content of the collection changed dramatically.
    /// </summary>
    caReseted,

    /// <summary>
    ///   An item in the collection was changed.
    /// </summary>
    caChanged
  );

  {$ENDREGION}


  {$REGION 'TValueHelper'}

  TValueHelper = record helper for TValue
  private
    procedure Init(typeInfo: Pointer);
{$IFNDEF DELPHIXE8_UP}
    function GetTypeKind: TTypeKind; inline;
{$ENDIF}
    function GetValueType: TRttiType;
    function TryAsInterface(typeInfo: PTypeInfo; out Intf): Boolean;
    class procedure RaiseConversionError(source, target: PTypeInfo); static;
  public
    class function &&op_Equality(const left, right: TValue): Boolean; static; inline;
    class function &&op_Inequality(const left, right: TValue): Boolean; static; inline;

{$IFNDEF DELPHIXE4_UP}
    class function &&op_Implicit(value: Single): TValue; overload; static; inline;
    class function &&op_Implicit(value: Double): TValue; overload; static; inline;
    class function &&op_Implicit(value: Currency): TValue; overload; static; inline;
    class function &&op_Implicit(value: UInt64): TValue; overload; static; inline;
{$ENDIF}

{$IFNDEF DELPHIXE8_UP}
    class function &&op_Implicit(const value: TVarRec): TValue; overload; static; inline;
{$ENDIF}

    class function &&op_Implicit(value: TDateTime): TValue; overload; static; inline;
    class function &&op_Implicit(value: TDate): TValue; overload; static; inline;
    class function &&op_Implicit(value: TTime): TValue; overload; static; inline;

    class function From(buffer: Pointer; typeInfo: PTypeInfo): TValue; overload; static;
    class function From(instance: TObject; classType: TClass): TValue; overload; static;
    class function FromFloat(typeInfo: PTypeInfo; value: Extended): TValue; overload; static;
    class function FromVariant(const value: Variant): TValue; static;

    /// <summary>
    ///   Returns a TValue that holds the value that was passed in a TVarRec.
    ///   The TypeInfo of the returned TValue depends on the VType of the
    ///   passed TVarRec.
    /// </summary>
    class function FromVarRec(const value: TVarRec): TValue; static;

    function AsPointer: Pointer;

{$IFDEF DELPHI2010}
    function AsString: string;
{$ENDIF}

    /// <summary>
    ///   Casts the currently stored value to another type.
    /// </summary>
    /// <remarks>
    ///   This method fixes the missing interface cast support of
    ///   TValue.AsType&lt;T&gt;.
    /// </remarks>
    function AsType<T>: T;

    /// <summary>
    ///   Casts the currently stored value to another type.
    /// </summary>
    /// <remarks>
    ///   This method fixes the missing interface cast support of TValue.Cast.
    /// </remarks>
    function Cast(typeInfo: PTypeInfo): TValue;

    /// <summary>
    ///   Compares to another TValue.
    /// </summary>
    function CompareTo(const value: TValue): Integer;

    /// <summary>
    ///   Converts the stored value to another type.
    /// </summary>
    function Convert<T>: TValue; overload;

    /// <summary>
    ///   Converts the stored value to another type using the specified format
    ///   settings.
    /// </summary>
    function Convert<T>(const formatSettings: TFormatSettings): TValue; overload;

    /// <summary>
    ///   Converts the stored value to another type.
    /// </summary>
    function Convert(targetType: PTypeInfo): TValue; overload;

    /// <summary>
    ///   Converts the stored value to another type using the specified format
    ///   settings.
    /// </summary>
    function Convert(targetType: PTypeInfo; const formatSettings: TFormatSettings): TValue; overload;

    /// <summary>
    ///   Checks for equality with another TValue.
    /// </summary>
    function Equals(const value: TValue): Boolean;

    /// <summary>
    ///   Returns the array content.
    /// </summary>
    function GetArray: TArray<TValue>;

    /// <summary>
    ///   Returns the stored nullable value or <c>TValue.Empty</c> when it is
    ///   null.
    /// </summary>
    /// <exception cref="EInvalidOperationException">
    ///   When the stored value is not a nullable value.
    /// </exception>
    function GetNullableValue: TValue;

    /// <summary>
    ///   Checks whether the stored value is an object or interface reference.
    /// </summary>
    function IsInstance: Boolean;

    /// <summary>
    ///   Checks whether the stored value is an interface reference.
    /// </summary>
    function IsInterface: Boolean;

    /// <summary>
    ///   Checks whether the stored value is a float type.
    /// </summary>
    function IsFloat: Boolean;

    /// <summary>
    ///   Checks whether the stored value is a numeric type.
    /// </summary>
    function IsNumeric: Boolean;

    /// <summary>
    ///   Checks whether the stored value is a <c>string</c>.
    /// </summary>
    function IsString: Boolean;

    /// <summary>
    ///   Checks whether the stored value is a <c>Variant</c>.
    /// </summary>
    function IsVariant: Boolean;

{$IFDEF DELPHI2010}
    function IsType<T>: Boolean; overload;
    function IsType(ATypeInfo: PTypeInfo): Boolean; overload;
{$ENDIF}

    /// <summary>
    ///   Sets the stored value of a nullable.
    /// </summary>
    procedure SetNullableValue(const value: TValue);

    /// <summary>
    ///   Tries to convert the stored value. Returns false when the conversion
    ///   is not possible.
    /// </summary>
    function TryConvert(targetType: PTypeInfo; out targetValue: TValue): Boolean; overload;

    /// <summary>
    ///   Tries to convert the stored value using the specified format
    ///   settings. Returns false when the conversion is not possible.
    /// </summary>
    function TryConvert(targetType: PTypeInfo; out targetValue: TValue;
      const formatSettings: TFormatSettings): Boolean; overload;

    /// <summary>
    ///   Tries to get the stored value of a nullable. Returns false when the
    ///   nullable is null.
    /// </summary>
    function TryGetNullableValue(out value: TValue): Boolean;

    /// <summary>
    ///   Tries to get the stored value of a lazy. Returns false when the lazy
    ///   was not assigned.
    /// </summary>
    function TryGetLazyValue(out value: TValue): Boolean;

    /// <summary>
    ///   Tries to convert the stored value. Returns false when the conversion
    ///   is not possible.
    /// </summary>
    function TryToType<T>(out targetValue: T): Boolean; overload;

    /// <summary>
    ///   Tries to convert the stored value using the specified format
    ///   settings. Returns false when the conversion is not possible.
    /// </summary>
    function TryToType<T>(out targetValue: T;
      const formatSettings: TFormatSettings): Boolean; overload;

    /// <summary>
    ///   Returns the stored value as TObject.
    /// </summary>
    function ToObject: TObject;

    /// <summary>
    ///   Returns the string representation of the stored value.
    /// </summary>
    function ToString: string;

    /// <summary>
    ///   Converts stored value to the specified type.
    /// </summary>
    function ToType<T>: T; overload;

    /// <summary>
    ///   Converts stored value to the specified type using the specified
    ///   format settings.
    /// </summary>
    function ToType<T>(const formatSettings: TFormatSettings): T; overload;

    /// <summary>
    ///   Returns the stored value as Variant.
    /// </summary>
    function ToVariant: Variant;

    /// <summary>
    ///   If the stored value is an object it will get destroyed/disposed.
    /// </summary>
    procedure Free;

    /// <summary>
    ///   Specifies the type kind of the stored value.
    /// </summary>
    /// <remarks>
    ///   This fixes the issue with returning <c>tkUnknown</c> when the stored
    ///   value is an empty reference type (RSP-10071).
    /// </remarks>
{$IFNDEF DELPHIXE8_UP}
    property Kind: TTypeKind read GetTypeKind;
{$ENDIF}

    /// <summary>
    ///   Returns the TRttiType of the stored value.
    /// </summary>
    property ValueType: TRttiType read GetValueType;
  end;

  {$ENDREGION}


  {$REGION 'TRttiMethodHelper'}

{$IF CompilerVersion < 31}
  {$HINTS OFF}
  TRttiMethodHack = class(TRttiMethod)
  private
    function GetParameters: TArray<TRttiParameter>; override;
  end;
  {$HINTS ON}
{$IFEND}

  TRttiMethodHelper = class helper for TRttiMethod
  private
    function GetIsAbstract: Boolean;
    function GetReturnTypeHandle: PTypeInfo;
{$IF CompilerVersion < 31}
    procedure DispatchValue(const value: TValue; typeInfo: PTypeInfo);
    procedure FixParameters(const parameters: TArray<TRttiParameter>);
  public
    /// <summary>
    ///   Returns the parameters of the method
    /// </summary>
    /// <remarks>
    ///   This fixes the incorrect Parent property of TRttiParameter (RSP-9824).
    /// </remarks>
    function GetParameters: TArray<TRttiParameter>; inline;

    /// <summary>
    ///   Invokes the method.
    /// </summary>
    /// <remarks>
    ///   This fixes the missing interface cast support in TValue (QC#123729).
    /// </remarks>
    function Invoke(Instance: TObject; const Args: array of TValue): TValue; overload;

    /// <summary>
    ///   Invokes the method.
    /// </summary>
    /// <remarks>
    ///   This fixes the missing interface cast support in TValue (QC#123729).
    /// </remarks>
    function Invoke(Instance: TClass; const Args: array of TValue): TValue; overload;

    /// <summary>
    ///   Invokes the method.
    /// </summary>
    /// <remarks>
    ///   This fixes the missing interface cast support in TValue (QC#123729).
    /// </remarks>
    function Invoke(Instance: TValue; const Args: array of TValue): TValue; overload;
{$IFEND}
  public

    /// <summary>
    ///   Returns if the method is dynamic or virtual abstract.
    /// </summary>
    property IsAbstract: Boolean read GetIsAbstract;

    /// <summary>
    ///   Returns the PTypeInfo of the ReturnType if assigned; otherwise
    ///   returns nil.
    /// </summary>
    property ReturnTypeHandle: PTypeInfo read GetReturnTypeHandle;
  end;

  {$ENDREGION}


  {$REGION 'TRttiInvokableTypeHelper'}

  {$IFDEF DELPHIXE2_UP}
  TRttiInvokableTypeHelper = class helper for TRttiInvokableType
  public
    function CreateImplementation(AUserData: Pointer;
      const ACallback: TMethodImplementationCallback): TMethodImplementation;
  end;
  {$ENDIF}

  {$ENDREGION}


  {$REGION 'TMethodImplementationHelper'}

  {$IFNDEF DELPHI2010}
  TMethodImplementationHelper = class helper for TMethodImplementation
  public
    function AsMethod: TMethod;
  end;
  {$ENDIF}

  {$ENDREGION}


  {$REGION 'Procedure types'}

  /// <summary>
  ///   Represents a logical predicate.
  /// </summary>
  /// <param name="arg">
  ///   the value needs to be determined.
  /// </param>
  /// <returns>
  ///   Returns <c>True</c> if the value was accepted, otherwise, returns <c>
  ///   False</c>.
  /// </returns>
  /// <remarks>
  ///   <note type="tip">
  ///     This type redefined the <see cref="SysUtils|TPredicate`1">
  ///     SysUtils.TPredicate&lt;T&gt;</see> type with a const parameter.
  ///   </note>
  /// </remarks>
  /// <seealso cref="Spring.DesignPatterns|ISpecification&lt;T&gt;" />
  {$M+}
  TPredicate<T> = reference to function(const arg: T): Boolean;
  {$M-}

  /// <summary>
  ///   Represents an anonymous method that has a single parameter and does not
  ///   return a value.
  /// </summary>
  /// <seealso cref="TActionProc&lt;T&gt;" />
  /// <seealso cref="TActionMethod&lt;T&gt;" />
  {$M+}
  TAction<T> = reference to procedure(const arg: T);

  TAction<T1, T2> = reference to procedure(const arg1: T1; const arg2: T2);

  TAction<T1, T2, T3> = reference to procedure(const arg1: T1; const arg2: T2; const arg3: T3);

  TAction<T1, T2, T3, T4> = reference to procedure(const arg1: T1; const arg2: T2; const arg3: T3; const arg4: T4);
  {$M-}

  /// <summary>
  ///   Represents a procedure that has a single parameter and does not return
  ///   a value.
  /// </summary>
  /// <seealso cref="TAction&lt;T&gt;" />
  /// <seealso cref="TActionMethod&lt;T&gt;" />
  TActionProc<T> = procedure(const arg: T);

  /// <summary>
  ///   Represents a instance method that has a single parameter and does not
  ///   return a value.
  /// </summary>
  /// <seealso cref="TAction&lt;T&gt;" />
  /// <seealso cref="TActionProc&lt;T&gt;" />
  TActionMethod<T> = procedure(const arg: T) of object;

  /// <summary>
  ///   Represents a anonymous method that has the same signature as
  ///   TNotifyEvent.
  /// </summary>
  {$M+}
  TNotifyProc = reference to procedure(Sender: TObject);
  {$M-}

  /// <summary>
  ///   An event type like TNotifyEvent that also has a generic item parameter.
  /// </summary>
  TNotifyEvent<T> = procedure(Sender: TObject; const item: T) of object;

  {$ENDREGION}


  {$REGION 'Multicast Event'}

  TMethodPointer = procedure of object;

  IEvent = interface
    ['{CFC14C4D-F559-4A46-A5B1-3145E9B182D8}']
  {$REGION 'Property Accessors'}
    function GetCanInvoke: Boolean;
    function GetInvoke: TMethodPointer;
    function GetEnabled: Boolean;
    function GetOnChanged: TNotifyEvent;
    function GetUseFreeNotification: Boolean;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
    procedure SetUseFreeNotification(const value: Boolean);
  {$ENDREGION}

    procedure Add(const handler: TMethodPointer);
    procedure Remove(const handler: TMethodPointer);

    /// <summary>
    ///   Removes all event handlers which were registered by an instance.
    /// </summary>
    procedure RemoveAll(instance: Pointer);

    /// <summary>
    ///   Clears all event handlers.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Returns <b>True</b> when the event will do anything because it is <see cref="Spring|IEvent.Enabled">
    ///   Enabled</see> and contains any event handler. Otherwise returns <b>
    ///   False</b>.
    /// </summary>
    property CanInvoke: Boolean read GetCanInvoke;

    /// <summary>
    ///   Gets the value indicates whether the multicast event is enabled, or
    ///   sets the value to enable or disable the event.
    /// </summary>
    property Enabled: Boolean read GetEnabled write SetEnabled;

    property Invoke: TMethodPointer read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;

    /// <summary>
    ///   Specifies if the event internally tracks if the event handlers are
    ///   implemented by a TComponent descendant and automatically unsubscribes
    ///   those when the implementing component is being destroyed.
    /// </summary>
    property UseFreeNotification: Boolean read GetUseFreeNotification write SetUseFreeNotification;
  end;

  /// <summary>
  ///   Represents a multicast event.
  /// </summary>
  /// <typeparam name="T">
  ///   The event handler type must be an instance procedural type such as
  ///   TNotifyEvent.
  /// </typeparam>
  IEvent<T> = interface(IEvent)
  {$REGION 'Property Accessors'}
    function GetInvoke: T;
  {$ENDREGION}

    /// <summary>
    ///   Adds an event handler to the list.
    /// </summary>
    procedure Add(handler: T);

    /// <summary>
    ///   Removes an event handler if it was added to the event.
    /// </summary>
    procedure Remove(handler: T);

    /// <summary>
    ///   Invokes all event handlers.
    /// </summary>
    property Invoke: T read GetInvoke;
  end;

  Event<T> = record
  private
    fInstance: IEvent<T>;
    function GetCanInvoke: Boolean;
    function GetEnabled: Boolean;
    function GetInvoke: T;
    function GetOnChanged: TNotifyEvent;
    function GetUseFreeNotification: Boolean;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(value: TNotifyEvent);
    procedure SetUseFreeNotification(const value: Boolean);
    procedure EnsureInitialized;
  public
    procedure Add(const handler: T);
    procedure Remove(const handler: T);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;

    property CanInvoke: Boolean read GetCanInvoke;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: T read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;

    /// <summary>
    ///   Specifies if the event internally tracks if the event handlers are
    ///   implemented by a TComponent descendant and automatically unsubscribes
    ///   those when the implementing component is being destroyed.
    /// </summary>
    property UseFreeNotification: Boolean read GetUseFreeNotification write SetUseFreeNotification;

    class operator Implicit(const value: IEvent<T>): Event<T>;
    class operator Implicit(var value: Event<T>): IEvent<T>;
    class operator Implicit(var value: Event<T>): T;
  end;

  INotifyEvent = IEvent<TNotifyEvent>;

  INotifyEvent<T> = interface(IEvent<TNotifyEvent<T>>)
  end;

  {$ENDREGION}


  {$REGION 'Interfaces'}

  /// <summary>
  ///   Supports cloning, which creates a new instance of a class with the same
  ///   value as an existing instance.
  /// </summary>
  IClonable = interface(IInvokable)
    ['{B6BC3795-624B-434F-BB19-6E8F55149D0A}']

    /// <summary>
    ///   Creates a new object that is a copy of the current instance.
    /// </summary>
    /// <returns>
    ///   A new object that is a copy of this instance.
    /// </returns>
    function Clone: TObject;
  end;

  /// <summary>
  ///   Defines a generalized type-specific comparison method that a class
  ///   implements to order or sort its instances.
  /// </summary>
  IComparable = interface(IInvokable)
    ['{7F0E25C8-50D7-4CF0-AB74-1913EBD3EE42}']

    /// <summary>
    ///   Compares the current instance with another object of the same type
    ///   and returns an integer that indicates whether the current instance
    ///   precedes, follows, or occurs in the same position in the sort order
    ///   as the other object.
    /// </summary>
    /// <param name="obj">
    ///   An object to compare with this instance.
    /// </param>
    /// <returns>
    ///   <para>
    ///     A value that indicates the relative order of the objects being
    ///     compared. The return value has these meanings:
    ///   </para>
    ///   <list type="table">
    ///     <listheader>
    ///       <term>Value</term>
    ///       <description>Meaning</description>
    ///     </listheader>
    ///     <item>
    ///       <term>Less than zero</term>
    ///       <description>This instance precedes <i>obj</i> in the sort
    ///         order.</description>
    ///     </item>
    ///     <item>
    ///       <term>Zero</term>
    ///       <description>This instance occurs in the same position in
    ///         the sort order as <i>obj</i>.</description>
    ///     </item>
    ///     <item>
    ///       <term>Greater than zero</term>
    ///       <description>This instance follows <i>obj</i> in the sort
    ///         order.</description>
    ///     </item>
    ///   </list>
    /// </returns>
    /// <exception cref="Spring|EArgumentException">
    ///   <i>obj</i> is not the same type as this instance.
    /// </exception>
    function CompareTo(const obj: TObject): Integer;
  end;

  /// <summary>
  ///   Base interface for anything that has a countable quantity.
  /// </summary>
  ICountable = interface(IInvokable)
    ['{CA225A9C-B6FD-4D6E-B3BD-22119CCE6C87}']
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
  {$ENDREGION}

    /// <summary>
    ///   Returns the number of elements in a countable.
    /// </summary>
    property Count: Integer read GetCount;

    /// <summary>
    ///   Determines whether a countable contains no elements.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  {$ENDREGION}


  {$REGION 'TNamedValue'}

  /// <summary>
  ///   A record type that stores a TValue and a name.
  /// </summary>
  TNamedValue = record
  private
    fValue: TValue;
    fName: string;
  public
    constructor Create(const value: TValue; const name: string);
    class function From<T>(const value: T; const name: string): TNamedValue; overload; static;

    class operator Implicit(const value: TNamedValue): TValue;
    class operator Implicit(const value: TValue): TNamedValue;

    property Name: string read fName;
    property Value: TValue read fValue;
  end;

  {$ENDREGION}


  {$REGION 'TTypedValue'}

  /// <summary>
  ///   A record type that stores a TValue and a typeinfo.
  /// </summary>
  TTypedValue = record
  private
    fValue: TValue;
    fTypeInfo: PTypeInfo;
  public
    constructor Create(const value: TValue; const typeInfo: PTypeInfo);
    class function From<T>(const value: T): TTypedValue; overload; static;
    class function From<T>(const value: T; const typeInfo: PTypeInfo): TTypedValue; overload; static;

    class operator Implicit(const value: TTypedValue): TValue;
    class operator Implicit(const value: TValue): TTypedValue;

    property TypeInfo: PTypeInfo read fTypeInfo;
    property Value: TValue read fValue;
  end;

  {$ENDREGION}


  {$REGION 'TInterfaceBase'}

  /// <summary>
  ///   Provides a non-reference-counted <see cref="System|IInterface" />
  ///   implementation.
  /// </summary>
  TInterfaceBase = class abstract(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  {$ENDREGION}


  {$REGION 'TInterfacedObjectEx'}

  /// <summary>
  ///   Provides an improved implementation for TInterfacedObject that was
  ///   introduced in Delphi XE7 for earlier versions. It makes sure that
  ///   reference counting during destruction does not call the destructor
  ///   recursively by using the highest bit in the FRefCount field to mark the
  ///   instance as currently being destroyed.
  /// </summary>
  {$IF not defined(DELPHIXE7_UP) and not defined(AUTOREFCOUNT)}
  TInterfacedObjectEx = class(TInterfacedObject)
  private
    const objDestroyingFlag = Integer($80000000);
    function GetRefCount: Integer; inline;
  public
    procedure BeforeDestruction; override;
    property RefCount: Integer read GetRefCount;
  end;
  {$ELSE}
  TInterfacedObjectEx = TInterfacedObject;
  {$IFEND}

  {$ENDREGION}


  {$REGION 'Guard'}

  /// <summary>
  ///   Provides static methods to check arguments and raise argument
  ///   exceptions.
  /// </summary>
  /// <remarks>
  ///   It's recommended that all arguments of public types and members should
  ///   be checked.
  /// </remarks>
  Guard = record
  private
    class procedure RaiseArgumentException(typeKind: TTypeKind; const argumentName: string); overload; static;
    class procedure RaiseNullableHasNoValue; static;
    class procedure RaiseNoDelegateAssigned; static;
    class procedure RaiseInvalidTypeCast(sourceType, targetType: PTypeInfo); static;
  public
    class procedure CheckTrue(condition: Boolean; const msg: string = ''); static; inline;
    class procedure CheckFalse(condition: Boolean; const msg: string = ''); static; inline;

    class procedure CheckInheritsFrom(const obj: TObject; parentClass: TClass; const argumentName: string); overload; static; inline;
    class procedure CheckInheritsFrom(cls, parentClass: TClass; const argumentName: string); overload; static; inline;

    class procedure CheckNotNull(const argumentValue: TObject; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(argumentValue: Pointer; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(const argumentValue: IInterface; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(condition: Boolean; const parameterName: string); overload; static; inline;
    class procedure CheckNotNull<T>(const argumentValue: T; const argumentName: string); overload; static; inline;

    class procedure CheckEnum<T{:enum}>(const argumentValue: T; const argumentName: string); overload; static; inline;
    class procedure CheckEnum<T{:enum}>(argumentValue: Integer; const argumentName: string); overload; static; inline;

    class procedure CheckSet<T{:set}>(const argumentValue: T; const argumentName: string); overload; static; inline;
    class procedure CheckSet<T{:set}>(argumentValue: Cardinal; const argumentName: string); overload; static; inline;

    class procedure CheckIndex(length, index: Integer; indexBase: Integer = 0); static; inline;

    class procedure CheckRange(const buffer: array of Byte; index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Byte; index, count: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; index, count: Integer); overload; static;
    class procedure CheckRange<T>(const buffer: array of T; index: Integer); overload; static;
    class procedure CheckRange<T>(const buffer: array of T; index, count: Integer); overload; static;
    class procedure CheckRange(const s: string; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: string; index, count: Integer); overload; static; inline;
{$IFNDEF NEXTGEN}
    class procedure CheckRange(const s: WideString; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: WideString; index, count: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; index, count: Integer); overload; static; inline;
{$ENDIF}
    class procedure CheckRange(condition: Boolean; const argumentName: string); overload; static; inline;
    class procedure CheckRange(length, index, count: Integer; indexBase: Integer = 0); overload; static; inline;

    /// <summary>
    ///   Checks an argument to ensure it is in the specified range including
    ///   the bounds.
    /// </summary>
    /// <param name="value">
    ///   The argument value to check.
    /// </param>
    /// <param name="min">
    ///   The minimum allowed value for the argument.
    /// </param>
    /// <param name="max">
    ///   The maximum allowed value for the argument.
    /// </param>
    /// <exception cref="EArgumentOutOfRangeException">
    ///   The value is not within the specified range.
    /// </exception>
    class procedure CheckRangeInclusive(value, min, max: Integer); overload; static; inline;

    /// <summary>
    ///   Checks an argument to ensure it is in the specified range excluding
    ///   the bounds.
    /// </summary>
    /// <param name="value">
    ///   The argument value to check.
    /// </param>
    /// <param name="min">
    ///   The minimum allowed value for the argument.
    /// </param>
    /// <param name="max">
    ///   The maximum allowed value for the argument. <br />
    /// </param>
    /// <exception cref="EArgumentOutOfRangeException">
    ///   The value is not within the specified range.
    /// </exception>
    class procedure CheckRangeExclusive(value, min, max: Integer); overload; static; inline;

    class procedure CheckTypeKind(typeInfo: PTypeInfo; expectedTypeKind: TTypeKind; const argumentName: string); overload; static;
    class procedure CheckTypeKind(typeInfo: PTypeInfo; expectedTypeKinds: TTypeKinds; const argumentName: string); overload; static;
    class procedure CheckTypeKind(typeKind: TTypeKind; expectedTypeKind: TTypeKind; const argumentName: string); overload; static; inline;
    class procedure CheckTypeKind(typeKind: TTypeKind; expectedTypeKinds: TTypeKinds; const argumentName: string); overload; static; inline;
    class procedure CheckTypeKind<T>(expectedTypeKind: TTypeKind; const argumentName: string); overload; static; inline;
    class procedure CheckTypeKind<T>(expectedTypeKinds: TTypeKinds; const argumentName: string); overload; static; inline;

    class function IsNullReference(const value; typeInfo: PTypeInfo): Boolean; static;

    /// <summary>
    ///   Raises an <see cref="EArgumentException" /> exception.
    /// </summary>
    /// <param name="msg">
    ///   The general error message.
    /// </param>
    class procedure RaiseArgumentException(const msg: string); overload; static;

    /// <summary>
    ///   Raises an <see cref="EFormatException" /> exception.
    /// </summary>
    class procedure RaiseArgumentFormatException(const argumentName: string); overload; static;

    /// <summary>
    ///   Raises an <see cref="EArgumentNullException" /> exception.
    /// </summary>
    class procedure RaiseArgumentNullException(const argumentName: string); overload; static;

    /// <summary>
    ///   Raises an <see cref="EArgumentOutOfRangeException" /> exception.
    /// </summary>
    class procedure RaiseArgumentOutOfRangeException(const argumentName: string); overload; static;

    /// <summary>
    ///   Raises an <see cref="EInvalidEnumArgumentException" /> exception.
    /// </summary>
    class procedure RaiseInvalidEnumArgumentException(const argumentName: string); overload; static;
  end;

  TArgument = Guard deprecated 'Use Guard instead';

  {$ENDREGION}


  {$REGION 'Nullable Types'}

  Nullable = record
  private
    const HasValue = 'True';
    type Null = interface end;
  end;

  /// <summary>
  ///   A nullable type can represent the normal range of values for its
  ///   underlying value type, plus an additional <c>Null</c> value.
  /// </summary>
  /// <typeparam name="T">
  ///   The underlying value type of the <see cref="Nullable&lt;T&gt;" />
  ///   generic type.
  /// </typeparam>
  Nullable<T> = record
  private
    fValue: T;
    fHasValue: string;
    class var fComparer: IEqualityComparer<T>;
    class function EqualsComparer(const left, right: T): Boolean; static;
    class function EqualsInternal(const left, right: T): Boolean; static; inline;
    function GetValue: T; inline;
    function GetHasValue: Boolean; inline;
  public
    /// <summary>
    ///   Initializes a new instance of the <see cref="Nullable&lt;T&gt;" />
    ///   structure to the specified value.
    /// </summary>
    constructor Create(const value: T); overload;

    /// <summary>
    ///   Initializes a new instance of the <see cref="Nullable&lt;T&gt;" />
    ///   structure to the specified value.
    /// </summary>
    constructor Create(const value: Variant); overload;

    /// <summary>
    ///   Retrieves the value of the current <see cref="Nullable&lt;T&gt;" />
    ///   object, or the object's default value.
    /// </summary>
    function GetValueOrDefault: T; overload;

    /// <summary>
    ///   Retrieves the value of the current <see cref="Nullable&lt;T&gt;" />
    ///   object, or the specified default value.
    /// </summary>
    /// <param name="defaultValue">
    ///   A value to return if the <see cref="HasValue" /> property is <c>False</c>
    ///    .
    /// </param>
    /// <returns>
    ///   The value of the <see cref="Value" /> property if the <see cref="HasValue" />
    ///    property is true; otherwise, the <paramref name="defaultValue" />
    ///   parameter.
    /// </returns>
    /// <remarks>
    ///   The <see cref="GetValueOrDefault" /> method returns a value even if
    ///   the <see cref="HasValue" /> property is false (unlike the <see cref="Value" />
    ///    property, which throws an exception).
    /// </remarks>
    function GetValueOrDefault(const defaultValue: T): T; overload;

    /// <summary>
    ///   Determines whether two nullable value are equal.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     If both two nullable values are null, return true;
    ///   </para>
    ///   <para>
    ///     If either one is null, return false;
    ///   </para>
    ///   <para>
    ///     else compares their values as usual.
    ///   </para>
    /// </remarks>
    function Equals(const other: Nullable<T>): Boolean;

    function ToString: string;

    /// <summary>
    ///   Returns the stored value as variant.
    /// </summary>
    /// <exception cref="EInvalidCast">
    ///   The type of T cannot be cast to Variant
    /// </exception>
    function ToVariant: Variant;

    /// <summary>
    ///   Gets the stored value. Returns <c>False</c> if it does not contain a
    ///   value.
    /// </summary>
    function TryGetValue(out value: T): Boolean; inline;

    /// <summary>
    ///   Gets a value indicating whether the current <see cref="Nullable&lt;T&gt;" />
    ///    structure has a value.
    /// </summary>
    property HasValue: Boolean read GetHasValue;

    /// <summary>
    ///   Gets the value of the current <see cref="Nullable&lt;T&gt;" /> value.
    /// </summary>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   Raised if the value is null.
    /// </exception>
    property Value: T read GetValue;

    class operator Implicit(const value: Nullable.Null): Nullable<T>;
    class operator Implicit(const value: T): Nullable<T>;

{$IFDEF IMPLICIT_NULLABLE}
    class operator Implicit(const value: Nullable<T>): T; inline;
      {$IFDEF IMPLICIT_NULLABLE_WARN}inline; deprecated 'Possible unsafe operation involving implicit operator - use Value property';{$ENDIF}
{$ENDIF}

{$IFDEF UNSAFE_NULLABLE}
    class operator Implicit(const value: Nullable<T>): Variant;
      {$IFNDEF DELPHIXE4}
      {$IFDEF UNSAFE_NULLABLE_WARN}inline; deprecated 'Possible unsafe operation involving implicit Variant conversion - use ToVariant';{$ENDIF}
      {$ENDIF}
    class operator Implicit(const value: Variant): Nullable<T>;
      {$IFDEF UNSAFE_NULLABLE_WARN}inline; deprecated 'Possible unsafe operation involving implicit Variant conversion - use explicit cast';{$ENDIF}
{$ENDIF}

    class operator Explicit(const value: Variant): Nullable<T>;
    class operator Explicit(const value: Nullable<T>): T; inline;

    class operator Equal(const left, right: Nullable<T>): Boolean; inline;
    class operator Equal(const left: Nullable<T>; const right: Nullable.Null): Boolean; inline;
    class operator Equal(const left: Nullable<T>; const right: T): Boolean; inline;
    class operator NotEqual(const left, right: Nullable<T>): Boolean; inline;
    class operator NotEqual(const left: Nullable<T>; const right: Nullable.Null): Boolean; inline;
    class operator NotEqual(const left: Nullable<T>; const right: T): Boolean; inline;
  end;

  TNullableString = Nullable<string>;
{$IFNDEF NEXTGEN}
  TNullableAnsiString = Nullable<AnsiString>;
  TNullableWideString = Nullable<WideString>;
{$ENDIF}
  TNullableInteger = Nullable<Integer>;
  TNullableInt64 = Nullable<Int64>;
  TNullableNativeInt = Nullable<NativeInt>;
  TNullableDateTime = Nullable<TDateTime>;
  TNullableCurrency = Nullable<Currency>;
  TNullableDouble = Nullable<Double>;
  TNullableBoolean = Nullable<Boolean>;
  TNullableGuid = Nullable<TGUID>;

  /// <summary>
  ///   Helper record for fast access to nullable value via RTTI.
  /// </summary>
  TNullableHelper = record
  strict private
    fValueType: PTypeInfo;
    fHasValueOffset: NativeInt;
  public
    constructor Create(typeInfo: PTypeInfo);
    function GetValue(instance: Pointer): TValue; inline;
    function HasValue(instance: Pointer): Boolean; inline;
    procedure SetValue(instance: Pointer; const value: TValue); inline;
    property ValueType: PTypeInfo read fValueType;
  end;

  {$ENDREGION}


  {$REGION 'Lazy Initialization'}

  /// <summary>
  ///   Specifies the kind of a lazy type.
  /// </summary>
  TLazyKind = (

    /// <summary>
    ///   Not a lazy type.
    /// </summary>
    lkNone,

    /// <summary>
    ///   Type is <see cref="SysUtils|TFunc&lt;T&gt;" />.
    /// </summary>
    lkFunc,

    /// <summary>
    ///   Type is <see cref="Spring|Lazy&lt;T&gt;" />.
    /// </summary>
    lkRecord,

    /// <summary>
    ///   Type is <see cref="Spring|ILazy&lt;T&gt;" />.
    /// </summary>
    lkInterface
  );

  /// <summary>
  ///   Provides support for lazy initialization.
  /// </summary>
  ILazy = interface(IInvokable)
    ['{40223BA9-0C66-49E7-AA33-BDAEF9F506D6}']
  {$REGION 'Property Accessors'}
    function GetIsValueCreated: Boolean;
    function GetValue: TValue;
  {$ENDREGION}

    /// <summary>
    ///   Gets a value that indicates whether a value has been created for this
    ///   <see cref="ILazy" /> instance.
    /// </summary>
    /// <value>
    ///   <b>True</b> if a value has been created for this <see cref="ILazy" />
    ///   instance; otherwise, <b>False</b>.
    /// </value>
    property IsValueCreated: Boolean read GetIsValueCreated;

    /// <summary>
    ///   Gets the lazily initialized value of the current <see cref="ILazy" />
    ///   instance.
    /// </summary>
    /// <value>
    ///   The lazily initialized value of the current <see cref="ILazy" />
    ///   instance.
    /// </value>
    property Value: TValue read GetValue;
  end;

  /// <summary>
  ///   Provides support for lazy initialization.
  /// </summary>
  ILazy<T> = interface(ILazy)
  {$REGION 'Property Accessors'}
    function GetValue: T;
  {$ENDREGION}

    /// <summary>
    ///   Gets the lazily initialized value of the current <see cref="ILazy&lt;T&gt;" />
    ///    instance.
    /// </summary>
    /// <value>
    ///   The lazily initialized value of the current <see cref="ILazy&lt;T&gt;" />
    ///    instance.
    /// </value>
    property Value: T read GetValue;
  end;

  /// <summary>
  ///   The base class of the lazy initialization type.
  /// </summary>
  TLazy = class(TInterfacedObject, ILazy)
  private
    fLock: TCriticalSection;
    fIsValueCreated: Boolean;
    fOwnsObjects: Boolean;
  {$REGION 'Property Accessors'}
    function GetIsValueCreated: Boolean;
    function GetValueNonGeneric: TValue; virtual; abstract;
    function ILazy.GetValue = GetValueNonGeneric;
  {$ENDREGION}
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Gets a value that indicates whether a value has been created for this
    ///   <see cref="TLazy&lt;T&gt;" /> instance.
    /// </summary>
    /// <value>
    ///   <b>True</b> if a value has been created for this <see cref="TLazy&lt;T&gt;" />
    ///    instance; otherwise, <b>False</b>.
    /// </value>
    property IsValueCreated: Boolean read GetIsValueCreated;
  end;

  /// <summary>
  ///   Provides support for lazy initialization.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of object that is being lazily initialized.
  /// </typeparam>
  TLazy<T> = class(TLazy, ILazy<T>, TFunc<T>)
  private
    fValueFactory: TFunc<T>;
    fValue: T;
    procedure InitializeValue;
  {$REGION 'Property Accessors'}
    function GetValue: T;
    function GetValueNonGeneric: TValue; override; final;
    function TFunc<T>.Invoke = GetValue;
  {$ENDREGION}
  public
    /// <summary>
    ///   Initializes a new instance of the <see cref="Lazy&lt;T&gt;" />
    ///   record. When lazy initialization occurs, the default constructor of
    ///   the target type is used.
    /// </summary>
    constructor Create; overload;

    /// <summary>
    ///   Initializes a new instance of the <see cref="TLazy&lt;T&gt;" />
    ///   class. When lazy initialization occurs, the specified initialization
    ///   function is used.
    /// </summary>
    /// <param name="valueFactory">
    ///   The delegate that is invoked to produce the lazily initialized value
    ///   when it is needed.
    /// </param>
    /// <param name="ownsObject">
    ///   If <b>true</b> the value - if any got created - will be destroyed
    ///   when going out of scope. Only when T is a class type.
    /// </param>
    /// <exception cref="EArgumentNullException">
    ///   <i>valueFactory</i> is <b>nil</b>.
    /// </exception>
    constructor Create(const valueFactory: TFunc<T>; ownsObject: Boolean = False); overload;

    /// <summary>
    ///   Initializes a new instance of <see cref="TLazy&lt;T&gt;" /> with the
    ///   specified value.
    /// </summary>
    /// <param name="value">
    ///   The initialized value.
    /// </param>
    /// <param name="ownsObject">
    ///   If <b>true</b> the value - if any got created - will be destroyed
    ///   when going out of scope. Only when T is a class type.
    /// </param>
    constructor CreateFrom(const value: T; ownsObject: Boolean = False);

    destructor Destroy; override;

    /// <summary>
    ///   Gets the lazily initialized value of the current <see cref="TLazy&lt;T&gt;" />
    ///    instance.
    /// </summary>
    /// <value>
    ///   The lazily initialized value of the current <see cref="TLazy&lt;T&gt;" />
    ///    instance.
    /// </value>
    property Value: T read GetValue;
  end;

  /// <summary>
  ///   Provides support for lazy initialization.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of object that is being lazily initialized.
  /// </typeparam>
  Lazy<T> = record
  private
    fLazy: ILazy<T>; // DO NOT ADD ANY OTHER FIELDS !!!
    function GetIsAssigned: Boolean;
    function GetIsValueCreated: Boolean;
    function GetValue: T; inline;
  public
    /// <summary>
    ///   Initializes a new instance of the <see cref="Lazy&lt;T&gt;" />
    ///   record. When lazy initialization occurs, the default constructor of
    ///   the target type is used.
    /// </summary>
    class function Create: Lazy<T>; overload; static;

    /// <summary>
    ///   Initializes a new instance of the <see cref="Lazy&lt;T&gt;" />
    ///   record. When lazy initialization occurs, the specified initialization
    ///   function is used.
    /// </summary>
    /// <param name="valueFactory">
    ///   The delegate that is invoked to produce the lazily initialized value
    ///   when it is needed.
    /// </param>
    /// <param name="ownsObject">
    ///   If <b>true</b> the value - if any got created - will be destroyed
    ///   when going out of scope. Only when T is a class type.
    /// </param>
    /// <exception cref="EArgumentNullException">
    ///   <i>valueFactory</i> is <b>nil</b>.
    /// </exception>
    constructor Create(const valueFactory: TFunc<T>; ownsObject: Boolean = False); overload;

    /// <summary>
    ///   Initializes a new instance of <see cref="Lazy&lt;T&gt;" /> with the
    ///   specified value.
    /// </summary>
    /// <param name="value">
    ///   The initialized value.
    /// </param>
    /// <param name="ownsObject">
    ///   If <b>true</b> the value - if any got created - will be destroyed
    ///   when going out of scope. Only when T is a class type.
    /// </param>
    constructor CreateFrom(const value: T; ownsObject: Boolean = False);

    class operator Implicit(const value: Lazy<T>): ILazy<T>;
    class operator Implicit(const value: Lazy<T>): T;
    class operator Implicit(const value: T): Lazy<T>;
    class operator Implicit(const value: TFunc<T>): Lazy<T>;
    class operator Implicit(const value: TLazy<T>): Lazy<T>;

    /// <summary>
    ///   Returns true if the value is assigned and contains an ILazy&lt;T&gt;
    ///   reference; otherwise returns false.
    /// </summary>
    property IsAssigned: Boolean read GetIsAssigned;

    /// <summary>
    ///   Gets a value that indicates whether a value has been created for this
    ///   <see cref="Lazy&lt;T&gt;" /> instance.
    /// </summary>
    /// <value>
    ///   <b>True</b> if a value has been created for this <see cref="Lazy&lt;T&gt;" />
    ///    instance; otherwise, <b>False</b>.
    /// </value>
    property IsValueCreated: Boolean read GetIsValueCreated;

    /// <summary>
    ///   Gets the lazily initialized value of the current <see cref="Lazy&lt;T&gt;" />
    ///    instance.
    /// </summary>
    /// <value>
    ///   The lazily initialized value of the current <see cref="Lazy&lt;T&gt;" />
    ///    instance.
    /// </value>
    /// <exception cref="Spring|EInvalidOperationException" />
    property Value: T read GetValue;
  end;

  /// <summary>
  ///   Provides lazy initialization routines.
  /// </summary>
  /// <remarks>
  ///   The methods are using AtomicCmpExchange to ensure thread-safety when
  ///   initializing instances.
  /// </remarks>
  TLazyInitializer = record
  public
    /// <summary>
    ///   Initializes a target reference type by using a specified function if
    ///   it hasn't already been initialized.
    /// </summary>
    /// <remarks>
    ///   In the event that multiple threads access this method concurrently,
    ///   multiple instances of T may be created, but only one will be stored
    ///   into target. In such an occurrence, this method will destroy the
    ///   instances that were not stored.
    /// </remarks>
    class function EnsureInitialized<T: class, constructor>(var target: T): T; overload; static;

    /// <summary>
    ///   Initializes a target reference type by using a specified function if
    ///   it hasn't already been initialized.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     This method may only be used on reference types, and <i>
    ///     valueFactory</i> may not return a nil reference.
    ///   </para>
    ///   <para>
    ///     In the event that multiple threads access this method
    ///     concurrently, multiple instances of T may be created, but only
    ///     one will be stored into target. In such an occurrence, this
    ///     method will destroy the instances that were not stored.
    ///   </para>
    /// </remarks>
    class function EnsureInitialized<T>(var target: T; const valueFactory: TFunc<T>): T; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'Shared smart pointer'}

  IShared<T> = reference to function: T;

  Shared<T> = record
  strict private
    fValue: T;
    fFinalizer: IInterface;
    class function GetNew: IShared<T>; static;
  public
    class operator Implicit(const value: T): Shared<T>;
    class operator Implicit(const value: Shared<T>): T; {$IFNDEF DELPHIXE4}inline;{$ENDIF}
    property Value: T read fValue;

    class property New: IShared<T> read GetNew;
  end;

  Shared = record
  private type
    TObjectFinalizer = class(TInterfacedObject, IShared<TObject>)
    private
      fValue: TObject;
      function Invoke: TObject;
    public
      constructor Create(typeInfo: PTypeInfo); overload;
      constructor Create(const value: TObject); overload;
    {$IFNDEF AUTOREFCOUNT}
      destructor Destroy; override;
    {$ENDIF}
    end;

    TRecordFinalizer = class(TInterfacedObject, IShared<Pointer>)
    private
      fValue: Pointer;
      fTypeInfo: PTypeInfo;
      function Invoke: Pointer;
    public
      constructor Create(typeInfo: PTypeInfo); overload;
      constructor Create(const value: Pointer; typeInfo: PTypeInfo); overload;
      destructor Destroy; override;
    end;
  public
    class function New<T>(const value: T): IShared<T>; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'Weak smart pointer'}

  IWeakReference<T> = interface
  {$REGION 'Property Accessors'}
    function GetIsAlive: Boolean;
    function GetTarget: T;
    procedure SetTarget(const value: T);
  {$ENDREGION}
    function TryGetTarget(out target: T): Boolean;
    property IsAlive: Boolean read GetIsAlive;
    property Target: T read GetTarget write SetTarget;
  end;

  TWeakReference = class abstract(TInterfacedObject)
  private
    fTarget: Pointer;
  protected
    function GetIsAlive: Boolean; inline;
    procedure RegisterWeakRef(address: Pointer; instance: Pointer);
    procedure UnregisterWeakRef(address: Pointer; instance: Pointer);
  public
    property IsAlive: Boolean read GetIsAlive;
  end;

  TWeakReference<T> = class(TWeakReference, IWeakReference<T>)
  private
    function GetTarget: T;
    procedure SetTarget(const value: T);
    constructor CreateInternal(const target: T; var ref: PPointer);
  public
    constructor Create(const target: T);
    destructor Destroy; override;

    function TryGetTarget(out target: T): Boolean;
    property Target: T read GetTarget write SetTarget;
  end;

  Weak<T> = record
  strict private
    fTarget: PPointer;
    fReference: IWeakReference<T>;
    function GetIsAlive: Boolean;
    function GetTarget: T;
    procedure SetTarget(const value: T);
    type PT = ^T;
  public
    constructor Create(const target: T);

    class operator Implicit(const value: Shared<T>): Weak<T>;
    class operator Implicit(const value: T): Weak<T>;
    class operator Implicit(const value: Weak<T>): T;

    class operator Equal(const left: Weak<T>; const right: T): Boolean; inline;
    class operator NotEqual(const left: Weak<T>; const right: T): Boolean; inline;

    function TryGetTarget(out target: T): Boolean;
    property Target: T read GetTarget write SetTarget;
    property IsAlive: Boolean read GetIsAlive;
  end;

  {$ENDREGION}


  {$REGION 'Property change notification'}

  IEventArgs = interface
    ['{162CDCDF-F8FC-4E5A-9CE8-55EABAE42EC3}']
  end;

  IPropertyChangedEventArgs = interface(IEventArgs)
    ['{DC7B4497-FA42-46D1-BE50-C764C4808197}']
    function GetPropertyName: string;
    property PropertyName: string read GetPropertyName;
  end;

  TEventArgs = class(TInterfacedObject, IEventArgs)
  strict protected
    constructor Create;
  end;

  TPropertyChangedEventArgs = class(TEventArgs, IPropertyChangedEventArgs)
  private
    fPropertyName: string;
    function GetPropertyName: string;
  public
    constructor Create(const propertyName: string);
    property PropertyName: string read GetPropertyName;
  end;

  {$M+}
  TEventHandler<T: IEventArgs> = reference to procedure(Sender: TObject;
    const EventArgs: T);
  {$M-}

  TPropertyChangedEvent = procedure(Sender: TObject;
    const EventArgs: IPropertyChangedEventArgs) of object;

  IPropertyChangedEvent = IEvent<TPropertyChangedEvent>;

  INotifyPropertyChanged = interface
    ['{A517EC98-C651-466B-8290-F7EE96877E03}']
    function GetOnPropertyChanged: IPropertyChangedEvent;
    property OnPropertyChanged: IPropertyChangedEvent read GetOnPropertyChanged;
  end;

  {$ENDREGION}


  {$REGION 'Notification handler'}

  TNotificationEvent = procedure(Component: TComponent;
    Operation: TOperation) of object;

  TNotificationHandler = class(TComponent)
  private
    fOnNotification: TNotificationEvent;
  protected
    procedure Notification(Component: TComponent;
      Operation: TOperation); override;
  public
    property OnNotification: TNotificationEvent
      read fOnNotification write fOnNotification;
  end;

  {$ENDREGION}


  {$REGION 'Exceptions'}

  ENotSupportedException = SysUtils.ENotSupportedException;

{$IFNDEF DELPHI2010}
  ENotImplementedException = SysUtils.ENotImplemented;
  EInvalidOperationException = SysUtils.EInvalidOpException;
  EArgumentNilException = SysUtils.EArgumentNilException;
{$ELSE}
  ENotImplementedException = class(Exception);
  EInvalidOperationException = class(Exception);
  EArgumentNilException = class(EArgumentException);
{$ENDIF}

  EInvalidCastException = SysUtils.EInvalidCast;

  EInsufficientMemoryException = EOutOfMemory;

  EFormatException = class(Exception);
  EIndexOutOfRangeException = class(Exception);

  EArgumentException = SysUtils.EArgumentException;
  EArgumentOutOfRangeException = SysUtils.EArgumentOutOfRangeException;
  EArgumentNullException = EArgumentNilException;
  EInvalidEnumArgumentException = class(EArgumentException);

  ERttiException = class(Exception);

  {$ENDREGION}


  {$REGION 'TTypeInfoHelper'}

  TTypeInfoHelper = record helper for TTypeInfo
  strict private
    function GetRttiType: TRttiType; inline;
  public
{$IFNDEF DELPHIXE3_UP}
    function TypeData: PTypeData; inline;
{$ENDIF}
    function TypeName: string; inline;

    property RttiType: TRttiType read GetRttiType;
  end;

  {$ENDREGION}


  {$REGION 'TTypeDataHelper'}

  TTypeDataHelper = record helper for TTypeData
  public
{$IFNDEF DELPHIXE3_UP}
    function DynArrElType: PPTypeInfo; inline;
{$ENDIF}
  end;

  {$ENDREGION}


  {$REGION 'TInterfacedCriticalSection'}

  ICriticalSection = interface(IInvokable)
    ['{16C21E9C-6450-4EA4-A3D3-1D59277C9BA6}']
    procedure Enter;
    procedure Leave;
    function ScopedLock: IInterface;
  end;

  TInterfacedCriticalSection = class(TCriticalSection, IInterface, ICriticalSection)
  private type
    TScopedLock = class(TInterfacedObject)
    private
      fCriticalSection: ICriticalSection;
    public
      constructor Create(const criticalSection: ICriticalSection);
      destructor Destroy; override;
    end;
  protected
    fRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ScopedLock: IInterface;
  end;

  {$ENDREGION}


  {$REGION 'Lock'}

  /// <summary>
  ///   Provides an easy to use wrapper around TCriticalSection. It
  ///   automatically initializes the TCriticalSection instance when required
  ///   and destroys it when the Lock goes out of scope.
  /// </summary>
  Lock = record
  private
    fCriticalSection: ICriticalSection;
    procedure EnsureInitialized;
  public
    /// <summary>
    ///   Calls Enter on the underlying TCriticalSection. The first call also
    ///   initializes the TCriticalSection instance.
    /// </summary>
    procedure Enter;

    /// <summary>
    ///   Calls Leave on the underlying TCriticalSection. If no call to Enter
    ///   has been made before it will raise an exception.
    /// </summary>
    /// <exception cref="EInvalidOperationException">
    ///   When Enter was not called before
    /// </exception>
    procedure Leave;

    /// <summary>
    ///   Calls Enter on the underlying TCriticalSection and returns an
    ///   interface reference that will call Leave once it goes out of scope.
    /// </summary>
    /// <remarks>
    ///   Use this to avoid the classic try/finally block but keep in mind that
    ///   the scope will be the entire method this is used in unless you keep
    ///   hold of the returned interface and explicitly set it to nil causing
    ///   its destruction.
    /// </remarks>
    function ScopedLock: IInterface;
  end;

  {$ENDREGION}


  {$REGION 'Tuples'}

  Tuple<T1, T2> = record
  private
    fValue1: T1;
    fValue2: T2;
  public
    constructor Create(const value1: T1; const value2: T2);
    function Equals(const value: Tuple<T1, T2>): Boolean;
    procedure Unpack(out value1: T1; out value2: T2); inline;
    class operator Equal(const left, right: Tuple<T1, T2>): Boolean;
    class operator NotEqual(const left, right: Tuple<T1, T2>): Boolean;
    class operator Implicit(const values: Tuple<T1, T2>): TArray<TValue>;
    class operator Implicit(const values: TArray<TValue>): Tuple<T1, T2>;
    class operator Implicit(const values: array of const): Tuple<T1, T2>;
    property Value1: T1 read fValue1;
    property Value2: T2 read fValue2;
  end;

  Tuple<T1, T2, T3> = record
  private
    fValue1: T1;
    fValue2: T2;
    fValue3: T3;
  public
    constructor Create(const value1: T1; const value2: T2; const value3: T3);
    function Equals(const value: Tuple<T1, T2, T3>): Boolean;
    procedure Unpack(out value1: T1; out value2: T2); overload; inline;
    procedure Unpack(out value1: T1; out value2: T2; out value3: T3); overload; inline;
    class operator Equal(const left, right: Tuple<T1, T2, T3>): Boolean;
    class operator NotEqual(const left, right: Tuple<T1, T2, T3>): Boolean;
    class operator Implicit(const values: Tuple<T1, T2, T3>): TArray<TValue>;
    class operator Implicit(const values: Tuple<T1, T2, T3>): Tuple<T1, T2>;
    class operator Implicit(const values: TArray<TValue>): Tuple<T1, T2, T3>;
    class operator Implicit(const values: array of const): Tuple<T1, T2, T3>;
    property Value1: T1 read fValue1;
    property Value2: T2 read fValue2;
    property Value3: T3 read fValue3;
  end;

  Tuple<T1, T2, T3, T4> = record
  private
    fValue1: T1;
    fValue2: T2;
    fValue3: T3;
    fValue4: T4;
  public
    constructor Create(const value1: T1; const value2: T2; const value3: T3; const value4: T4);
    function Equals(const value: Tuple<T1, T2, T3, T4>): Boolean;
    procedure Unpack(out value1: T1; out value2: T2); overload; inline;
    procedure Unpack(out value1: T1; out value2: T2; out value3: T3); overload; inline;
    procedure Unpack(out value1: T1; out value2: T2; out value3: T3; out value4: T4); overload; inline;
    class operator Equal(const left, right: Tuple<T1, T2, T3, T4>): Boolean;
    class operator NotEqual(const left, right: Tuple<T1, T2, T3, T4>): Boolean;
    class operator Implicit(const values: Tuple<T1, T2, T3, T4>): TArray<TValue>;
    class operator Implicit(const values: Tuple<T1, T2, T3, T4>): Tuple<T1, T2>;
    class operator Implicit(const values: Tuple<T1, T2, T3, T4>): Tuple<T1, T2, T3>;
    class operator Implicit(const values: TArray<TValue>): Tuple<T1, T2, T3, T4>;
    class operator Implicit(const values: array of const): Tuple<T1, T2, T3, T4>;
    property Value1: T1 read fValue1;
    property Value2: T2 read fValue2;
    property Value3: T3 read fValue3;
    property Value4: T4 read fValue4;
  end;

  Tuple = class
  public
    class function Create<T1, T2>(const value1: T1;
      const value2: T2): Tuple<T1, T2>; overload; static; inline;
    class function Create<T1, T2, T3>(const value1: T1; const value2: T2;
      const value3: T3): Tuple<T1, T2, T3>; overload; static; inline;
    class function Create<T1, T2, T3, T4>(const value1: T1; const value2: T2;
      const value3: T3; const value4: T4): Tuple<T1, T2, T3, T4>; overload; static; inline;
  end;

  {$ENDREGION}


  {$REGION 'TArray'}

  TArray = class
  private
    const IntrosortSizeThreshold = 16;
    class function GetDepthLimit(count: Integer): Integer; static;

    class procedure Swap<T>(var left, right: T); static; inline;

    class procedure SortTwoItems<T>(const comparer: IComparer<T>; var left, right: T); static;
    class procedure SortThreeItems<T>(const comparer: IComparer<T>; var left, mid, right: T); static;

    class procedure InsertionSort<T>(var values: array of T; const comparer: IComparer<T>; left, right: Integer); static;

    class procedure DownHeap<T>(var values: array of T; const comparer: IComparer<T>; left, count, i: Integer); static;
    class procedure HeapSort<T>(var values: array of T; const comparer: IComparer<T>; left, right: Integer); static;

    class function QuickSortPartition<T>(var values: array of T; const comparer: IComparer<T>; left, right: Integer): Integer; static;

    class procedure IntroSort<T>(var values: array of T; const comparer: IComparer<T>; left, right, depthLimit: Integer); static;
  public

    /// <summary>
    ///   Searches a range of elements in a sorted array for the given value,
    ///   using a binary search algorithm returning the index for the first
    ///   found value using the specified comparer.
    /// </summary>
    class function BinarySearch<T>(const values: array of T; const item: T;
      out foundIndex: Integer; const comparer: IComparer<T>;
      index, count: Integer): Boolean; overload; static;

    /// <summary>
    ///   Searches a sorted array for the given value, using a binary search
    ///   algorithm returning the index for the first found value using the
    ///   specified comparer.
    /// </summary>
    class function BinarySearch<T>(const values: array of T; const item: T;
      out foundIndex: Integer; const comparer: IComparer<T>): Boolean; overload; static;

    /// <summary>
    ///   Searches a sorted array for the given value, using a binary search
    ///   algorithm returning the index for the first found value using the
    ///   default comparer.
    /// </summary>
    class function BinarySearch<T>(const values: array of T; const item: T;
      out foundIndex: Integer): Boolean; overload; static; static;

    /// <summary>
    ///   Searches a range of elements in a sorted array for the given value,
    ///   using a binary search algorithm returning the index for the first
    ///   found value using the specified comparison.
    /// </summary>
    class function BinarySearch<T>(const values: array of T; const item: T;
      out foundIndex: Integer; const comparison: TComparison<T>;
      index, count: Integer): Boolean; overload; static;

    /// <summary>
    ///   Searches a sorted array for the given value, using a binary search
    ///   algorithm returning the index for the first found value using the
    ///   specified comparison.
    /// </summary>
    class function BinarySearch<T>(const values: array of T; const item: T;
      out foundIndex: Integer; const comparison: TComparison<T>): Boolean; overload; static;

    /// <summary>
    ///   Searches a range of elements in a sorted array for the given value,
    ///   using a binary search algorithm returning the index for the last
    ///   found value using the specified comparer.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer; const comparer: IComparer<T>;
      index, count: Integer): Boolean; overload; static;

    /// <summary>
    ///   Searches a range of elements in a sorted array for the given value,
    ///   using a binary search algorithm returning the index for the last
    ///   found value using the specified comparison.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer; const comparison: TComparison<T>;
      index, count: Integer): Boolean; overload; static;

    /// <summary>
    ///   Searches a sorted array for the given value, using a binary search
    ///   algorithm returning the index for the last found value using the
    ///   specified comparer.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer;
      const comparer: IComparer<T>): Boolean; overload; static;

    /// <summary>
    ///   Searches a sorted array for the given value, using a binary search
    ///   algorithm returning the index for the last found value using the
    ///   specified comparer.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer;
      const comparison: TComparison<T>): Boolean; overload; static;

    /// <summary>
    ///   Searches a sorted array for the given value, using a binary search
    ///   algorithm returning the index for the last found value.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer): Boolean; overload; static; static;

    /// <summary>
    ///   Concatenates an array of arrays to one array
    /// </summary>
    class function Concat<T>(const values: array of TArray<T>): TArray<T>; static;

    /// <summary>
    ///   Determines whether the specified item exists as an element in an
    ///   array.
    /// </summary>
    class function Contains<T>(const values: array of T;
      const item: T): Boolean; static;

    /// <summary>
    ///   Copies an open array to a dynamic array.
    /// </summary>
    class function Copy<T>(const values: array of T): TArray<T>; overload; static;

    /// <summary>
    ///   Copies the specified count of elements from the source array to the
    ///   target array.
    /// </summary>
    class procedure Copy<T>(const source: array of T;
      var target: array of T; count: NativeInt); overload; static;

    /// <summary>
    ///   Copies the specified count of elements from the specified position in
    ///   the source array to the specified position in the target array.
    /// </summary>
    class procedure Copy<T>(const source: array of T; var target: array of T;
      sourceIndex, targetIndex, count: NativeInt); overload; static;

    /// <summary>
    ///   Executes the specified action for each item in the specified array.
    /// </summary>
    class procedure ForEach<T>(const values: array of T;
      const action: TAction<T>); static;

    /// <summary>
    ///   Searches for the specified element and returns the index of the first
    ///   occurrence within the entire array.
    /// </summary>
    class function IndexOf<T>(const values: array of T;
      const item: T): Integer; overload; static;

    /// <summary>
    ///   Searches for the specified element and returns the index of the first
    ///   occurrence within the range of elements in the array that extends
    ///   from the specified index to the last element.
    /// </summary>
    class function IndexOf<T>(const values: array of T; const item: T;
      index: Integer): Integer; overload; static;

    /// <summary>
    ///   Searches for the specified element and returns the index of the first
    ///   occurrence within the range of elements in the array that starts at
    ///   the specified index and contains the specified number of elements.
    /// </summary>
    class function IndexOf<T>(const values: array of T; const item: T;
      index, count: Integer): Integer; overload; static;

    /// <summary>
    ///   Searches for the specified element and returns the index of the first
    ///   occurrence within the range of elements in the array that starts at
    ///   the specified index and contains the specified number of elements
    ///   using the specified equality comparer.
    /// </summary>
    class function IndexOf<T>(const values: array of T; const item: T;
      index, count: Integer;
      const comparer: IEqualityComparer<T>): Integer; overload; static;

    /// <summary>
    ///   Searches for the specified element and returns the index of the last
    ///   occurrence within the entire array.
    /// </summary>
    class function LastIndexOf<T>(const values: array of T;
      const item: T): Integer; overload; static;

    /// <summary>
    ///   Searches for the specified element and returns the index of the last
    ///   occurrence within the range of elements in the array that extends
    ///   from the specified index to the last element.
    /// </summary>
    class function LastIndexOf<T>(const values: array of T; const item: T;
      index: Integer): Integer; overload; static;

    /// <summary>
    ///   Searches for the specified element and returns the index of the last
    ///   occurrence within the range of elements in the array that starts at
    ///   the specified index and contains the specified number of elements.
    /// </summary>
    class function LastIndexOf<T>(const values: array of T; const item: T;
      index, count: Integer): Integer; overload; static;

    /// <summary>
    ///   Searches for the specified element and returns the index of the last
    ///   occurrence within the range of elements in the array that starts at
    ///   the specified index and contains the specified number of elements
    ///   using the specified equality comparer.
    /// </summary>
    class function LastIndexOf<T>(const values: array of T; const item: T;
      index, count: Integer;
      const comparer: IEqualityComparer<T>): Integer; overload; static;

    /// <summary>
    ///   Reverses the elements in the entire array.
    /// </summary>
    class procedure Reverse<T>(var values: array of T); overload; static;

    /// <summary>
    ///   Reverses the elements in the specified range in the array.
    /// </summary>
    class procedure Reverse<T>(var values: array of T;
      index, count: Integer); overload; static;

    /// <summary>
    ///   Shuffles the elements in the array using the Fisher-Yates algorithm.
    /// </summary>
    class procedure Shuffle<T>(var values: array of T); overload; static;

    /// <summary>
    ///   Shuffles the elements in the array starting at the specified index
    ///   using the Fisher-Yates algorithm.
    /// </summary>
    class procedure Shuffle<T>(var values: array of T;
      index: Integer); overload; static;

    /// <summary>
    ///   Shuffles the specified count of elements in the array starting at the
    ///   specified index using the Fisher-Yates algorithm.
    /// </summary>
    class procedure Shuffle<T>(var values: array of T;
      index, count: Integer); overload; static;

    /// <summary>
    ///   Sorts the elements in an array using the default comparer.
    /// </summary>
    class procedure Sort<T>(var values: array of T); overload; static;

    /// <summary>
    ///   Sorts the elements in an array using the specified comparer.
    /// </summary>
    class procedure Sort<T>(var values: array of T; const comparer: IComparer<T>); overload; static;

    /// <summary>
    ///   Sorts the specified range of elements in an array using the specified
    ///   comparer.
    /// </summary>
    class procedure Sort<T>(var values: array of T;
      const comparer: IComparer<T>; index, count: Integer); overload; static;

    /// <summary>
    ///   Sorts the elements in an array using the specified comparison.
    /// </summary>
    class procedure Sort<T>(var values: array of T; const comparison: TComparison<T>); overload; static;

    /// <summary>
    ///   Sorts the specified range of elements in an array using the specified
    ///   comparison.
    /// </summary>
    class procedure Sort<T>(var values: array of T;
      const comparison: TComparison<T>; index, count: Integer); overload; static;
  end;

  {$ENDREGION}


  {$REGION 'Vector<T>'}

{$IFDEF DELPHI2010}
  TArrayEnumerator<T> = class
{$ELSE}
  TArrayEnumerator<T> = record
{$ENDIF}
  private
    fItems: TArray<T>;
    fIndex: Integer;
    function GetCurrent: T; inline;
  public
    constructor Create(const items: TArray<T>);
    function MoveNext: Boolean; inline;
    property Current: T read GetCurrent;
  end;

  VectorHelper = record
  private
    class function InternalIndexOfInt8(const data: Pointer; const item: ShortInt): Integer; static;
    class function InternalIndexOfInt16(const data: Pointer; const item: SmallInt): Integer; static;
    class function InternalIndexOfInt32(const data: Pointer; const item: Integer): Integer; static;
    class function InternalIndexOfInt64(const data: Pointer; const item: Int64): Integer; static;
    class function InternalIndexOfStr(const data: Pointer; const item: string): Integer; static;
  end;

  Vector<T> = record
  private
    fData: TArray<T>; // DO NOT ADD ANY OTHER FIELDS !!!
    function GetCount: Integer; inline;
    function GetFirst: T; inline;
    function GetItem(index: Integer): T; inline;
    function GetLast: T; inline;
    procedure SetCount(value: Integer); inline;
    procedure SetItem(index: Integer; const value: T); inline;
    procedure InternalInsert(index: Integer; const items: array of T);
    function InternalEquals(const items: array of T): Boolean;
    function InternalIndexOf(const item: T): Integer;
  public
    class operator Implicit(const value: TArray<T>): Vector<T>; inline;
    class operator Implicit(const value: Vector<T>): TArray<T>; inline;
    class operator Add(const left, right: Vector<T>): Vector<T>; inline;
    class operator Add(const left: Vector<T>; const right: TArray<T>): Vector<T>; inline;
    class operator Add(const left: TArray<T>; const right: Vector<T>): Vector<T>; inline;
    class operator Add(const left: Vector<T>; const right: T): Vector<T>; inline;
    class operator Add(const left: T; const right: Vector<T>): Vector<T>; inline;
    class operator Subtract(const left, right: Vector<T>): Vector<T>; inline;
    class operator Subtract(const left: Vector<T>; const right: T): Vector<T>; inline;
    class operator In(const left: T; const right: Vector<T>): Boolean; inline;
    class operator In(const left, right: Vector<T>): Boolean; inline;
    class operator In(const left: TArray<T>; const right: Vector<T>): Boolean; inline;
    class operator Equal(const left, right: Vector<T>): Boolean; inline;
    class operator NotEqual(const left, right: Vector<T>): Boolean; inline;

    procedure Assign(const items: array of T);
    procedure Clear; inline;

    function Add(const item: T): Integer; overload; inline;
    procedure Add(const items: array of T); overload;
    procedure Add(const items: TArray<T>); overload; inline;
    procedure Add(const items: Vector<T>); overload; inline;
    procedure Insert(index: Integer; const item: T); overload; inline;
    procedure Insert(index: Integer; const items: array of T); overload;
    procedure Insert(index: Integer; const items: TArray<T>); overload; inline;
    procedure Delete(index: Integer); overload; inline;
    procedure Delete(index: Integer; count: Integer); overload; inline;
    function Remove: T; overload; inline;
    procedure Remove(const item: T); overload; inline;
    procedure Remove(const items: array of T); overload;
    procedure Remove(const items: TArray<T>); overload; inline;

    function Contains(const item: T): Boolean; overload; inline;
    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; overload;
    function Contains(const item: T; const comparer: TEqualityComparison<T>): Boolean; overload;
    function Contains(const items: array of T): Boolean; overload;
    function Contains(const items: TArray<T>): Boolean; overload;
    function IndexOf(const item: T): Integer; inline;
    function Equals(const items: array of T): Boolean; overload;
    function Equals(const items: TArray<T>): Boolean; overload; inline;

    function Slice(index: Integer): Vector<T>; overload; inline;
    function Slice(index: Integer; count: Integer): Vector<T>; overload; inline;
    function Splice(index: Integer; count: Integer): Vector<T>; overload; inline;
    function Splice(index: Integer; count: Integer; const items: array of T): Vector<T>; overload;

    procedure Sort; overload; inline;
    procedure Sort(const comparer: IComparer<T>); overload; inline;
    procedure Sort(const comparer: TComparison<T>); overload; inline;
    procedure Reverse;

    procedure ForEach(const action: TAction<T>); inline;

    function GetEnumerator: TArrayEnumerator<T>; inline;
    property Count: Integer read GetCount;
    property Data: TArray<T> read fData;
    property First: T read GetFirst;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property Last: T read GetLast;
    property Length: Integer read GetCount write SetCount;
  end;

  {$ENDREGION}


  {$REGION 'TFormatSettingsHelper'}

{$IFDEF DELPHI2010}
  TFormatSettingsHelper = record helper for TFormatSettings
  public
    /// <summary>
    ///   Creates a TFormatSettings record with current default values provided
    ///   by the operating system.
    /// </summary>
    class function Create: TFormatSettings; static; inline;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'Routines'}

{$IFDEF DELPHI2010}
function SplitString(const s, delimiters: string): TStringDynArray;
{$ENDIF}

{$IFNDEF DELPHIXE2_UP}
function ReturnAddress: Pointer;
{$ENDIF}

{$IFNDEF DELPHIXE3_UP}
function Pos(const SubStr, Str: UnicodeString; Offset: Integer): Integer; overload;
{$ENDIF}

procedure PlatformNotImplemented;

/// <summary>
///   Raises an <see cref="Spring|EArgumentNullException" /> if the <paramref name="value" />
///    is nil.
/// </summary>
procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string); overload; deprecated 'Use Guard.CheckNotNull instead';

/// <summary>
///   Raises an <see cref="Spring|EArgumentNullException" /> if the <paramref name="value" />
///    is nil.
/// </summary>
procedure CheckArgumentNotNull(value: Pointer; const argumentName: string); overload; deprecated 'Use Guard.CheckNotNull instead';

function GetQualifiedClassName(AInstance: TObject): string; overload; inline;
function GetQualifiedClassName(AClass: TClass): string; overload; {$IFDEF DELPHIXE2_UP}inline;{$ENDIF}

/// <summary>
///   Determines whether an instance of <c>leftType</c> can be assigned from an
///   instance of <c>rightType</c>.
/// </summary>
function IsAssignableFrom(leftType, rightType: PTypeInfo): Boolean; overload;

function IsAssignableFrom(const leftTypes, rightTypes: array of PTypeInfo): Boolean; overload;

/// <summary>
///   Returns <c>True</c> if the type is a nullable type.
/// </summary>
function IsNullable(typeInfo: PTypeInfo): Boolean;

/// <summary>
///   Returns the underlying type argument of the specified nullable type.
/// </summary>
function GetUnderlyingType(typeInfo: PTypeInfo): PTypeInfo;

/// <summary>
///   Returns the <see cref="TLazyKind" /> of the typeInfo.
/// </summary>
function GetLazyKind(typeInfo: PTypeInfo): TLazyKind;

/// <summary>
///   Returns the underlying type of the lazy type.
/// </summary>
function GetLazyType(typeInfo: PTypeInfo): PTypeInfo;

/// <summary>
///   Returns the underlying type name of the lazy type.
/// </summary>
function GetLazyTypeName(typeInfo: PTypeInfo): string;

/// <summary>
///   Returns <c>True</c> if the type is a lazy type.
/// </summary>
function IsLazyType(typeInfo: PTypeInfo): Boolean;

/// <summary>
///   Returns the size that is needed in order to pass an argument of the given
///   type.
/// </summary>
/// <remarks>
///   While in most cases the result is equal to the actual type size for short
///   strings it always returns SizeOf(Pointer) as short strings are always
///   passed as pointer.
/// </remarks>
function GetTypeSize(typeInfo: PTypeInfo): Integer;

/// <summary>
///   Returns the size of the passed set type
/// </summary>
function GetSetSize(typeInfo: PTypeInfo): Integer;

/// <summary>
///   Compares two TValue instances.
/// </summary>
function CompareValue(const left, right: TValue): Integer; overload;

/// <summary>
///   Returns the types of the values.
/// </summary>
function TypesOf(const values: array of TValue): TArray<PTypeInfo>;

function MethodReferenceToMethodPointer(const methodRef): TMethodPointer;
function MethodPointerToMethodReference(const method: TMethodPointer): IInterface;

function SkipShortString(P: PByte): Pointer; inline;

function StreamToVariant(const stream: TStream): Variant;

function GetGenericTypeParameters(const typeName: string): TArray<string>;

/// <summary>
///   Indicates whether two Variant values are equal. Unlike using the equals
///   operator this function also supports variant arrays.
/// </summary>
function SameValue(const left, right: Variant): Boolean; overload;

/// <summary>
///   Determines whether a variant value is null or empty.
/// </summary>
function VarIsNullOrEmpty(const value: Variant): Boolean;

/// <summary>
///   Returns the length of the variant array for the specified dimension.
/// </summary>
function VarArrayLength(const value: Variant; dim: Integer): Integer;

{$IFDEF USE_VMTAUTOTABLE}
function CreateFieldTable(classType: TClass): TInitTable;
{$ENDIF}

/// <summary>
///   Returns the field table for the given class that contains all fields that
///   have Default or Managed attribute annotations.
/// </summary>
function GetInitTable(classType: TClass): TInitTable; {$IFDEF USE_VMTAUTOTABLE}inline;{$ENDIF}

function GetVirtualMethod(const classType: TClass; const index: Integer): Pointer; inline;

function GetAbstractError: Pointer;

{$IFNDEF DELPHIXE3_UP}
function AtomicIncrement(var target: Integer): Integer;
function AtomicDecrement(var target: Integer): Integer;
function AtomicCmpExchange(var target: Integer; newValue, comparand: Integer): Integer; overload;
function AtomicCmpExchange(var target: Pointer; newValue, comparand: Pointer): TObject; overload;
{$ENDIF}

procedure IncUnchecked(var i: Integer; const n: Integer = 1); inline;

procedure SwapPtr(var left, right); inline;

  {$ENDREGION}


const
  EmptyValue: TValue = ();

implementation

uses
  DateUtils,
  Math,
  RTLConsts,
  StrUtils,
  SysConst,
  VarUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Spring.Events,
  Spring.ResourceStrings,
{$IFNDEF DELPHI2010}
  Spring.ValueConverters,
{$ENDIF}
  Spring.VirtualClass;


{$REGION 'Routines'}

{$IFDEF DELPHI2010}
function SplitString(const s, delimiters: string): TStringDynArray;
var
  splitCount: Integer;
  startIndex: Integer;
  foundIndex: Integer;
  i: Integer;
begin
  Result := nil;

  if s <> '' then
  begin
    splitCount := 0;
    for i := 1 to Length(s) do
      if IsDelimiter(delimiters, s, i) then
        Inc(splitCount);

    SetLength(Result, splitCount + 1);

    startIndex := 1;
    for i := 0 to splitCount - 1 do
    begin
      foundIndex := FindDelimiter(delimiters, s, startIndex);
      Result[i] := Copy(s, startIndex, foundIndex - startIndex);
      startIndex := foundIndex + 1;
    end;

    Result[splitCount] := Copy(s, startIndex, Length(s) - startIndex + 1);
  end;
end;
{$ENDIF}

{$IFNDEF DELPHIXE2_UP}
function ReturnAddress: Pointer;
asm
  mov eax,[ebp+4]
end;
{$ENDIF}

{$IFNDEF DELPHIXE3_UP}
function Pos(const SubStr, Str: UnicodeString; Offset: Integer): Integer;
asm
  jmp PosEx
end;
{$ENDIF}

procedure PlatformNotImplemented;
begin
  raise ENotImplementedException.Create('Not implemented in present platform.') at ReturnAddress;
end;

procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string);
begin
  CheckArgumentNotNull(Pointer(value), argumentName);
end;

procedure CheckArgumentNotNull(value: Pointer; const argumentName: string);
begin
  if not Assigned(value) then
    Guard.RaiseArgumentNullException(argumentName);
end;

function GetQualifiedClassName(AInstance: TObject): string;
begin
  Result := GetQualifiedClassName(AInstance.ClassType);
end;

function GetQualifiedClassName(AClass: TClass): string;
{$IFNDEF DELPHIXE2_UP}
var
  LUnitName: string;
{$ENDIF}
begin
{$IFDEF DELPHIXE2_UP}
  Result := AClass.QualifiedClassName;
{$ELSE}
  LUnitName := AClass.UnitName;
  if LUnitName = '' then
    Result := AClass.ClassName
  else
    Result := LUnitName + '.' + AClass.ClassName;
{$ENDIF}
end;

function IsAssignableFrom(leftType, rightType: PTypeInfo): Boolean;
var
  leftData, rightData: PTypeData;
begin
  Guard.CheckNotNull(leftType, 'leftType');
  Guard.CheckNotNull(rightType, 'rightType');

  if leftType = rightType then
    Exit(True);

  leftData := leftType.TypeData;
  rightData := rightType.TypeData;
  if (rightType.Kind = tkClass) and (leftType.Kind = tkClass) then
    Result := rightData.ClassType.InheritsFrom(leftData.ClassType)
  else if (rightType.Kind = tkClass) and (leftType.Kind = tkInterface) then
  begin
    Result := (ifHasGuid in leftData.IntfFlags) and
      Supports(rightData.ClassType, leftData.Guid);
  end
  else if (rightType.Kind = tkInterface) and (leftType.Kind = tkInterface) then
  begin
    if (ifHasGuid in leftData.IntfFlags) and (leftData.Guid = rightData.Guid) then
      Exit(True);
    Result := Assigned(rightData.IntfParent) and (rightData.IntfParent^ = leftType);
    while not Result and Assigned(rightData.IntfParent) do
    begin
      Result := rightData.IntfParent^ = leftType;
      rightData := rightData.IntfParent^.TypeData;
    end;
  end
  else
    Result := False;
end;

function IsAssignableFrom(const leftTypes, rightTypes: array of PTypeInfo): Boolean;
var
  i: Integer;
begin
  Result := Length(leftTypes) = Length(rightTypes);
  if Result then
    for i := Low(leftTypes) to High(leftTypes) do
      if not IsAssignableFrom(leftTypes[i], rightTypes[i]) then
        Exit(False);
end;

function IsNullable(typeInfo: PTypeInfo): Boolean;
const
  PrefixString = 'Nullable<';    // DO NOT LOCALIZE
begin
  Result := Assigned(typeInfo) and (typeInfo.Kind = tkRecord)
    and StartsText(PrefixString, typeInfo.TypeName);
end;

function GetUnderlyingType(typeInfo: PTypeInfo): PTypeInfo;
var
  nullable: TNullableHelper;
begin
  if IsNullable(typeInfo) then
  begin
    nullable := TNullableHelper.Create(typeInfo);
    Result := nullable.ValueType;
  end
  else
    Result := nil;
end;

const
  LazyPrefixStrings: array[lkFunc..High(TLazyKind)] of string = (
    'TFunc<', 'Lazy<', 'ILazy<');

function GetLazyKind(typeInfo: PTypeInfo): TLazyKind;
var
  name: string;
begin
  if Assigned(typeInfo) then
  begin
    name := typeInfo.TypeName;
    for Result := lkFunc to High(TLazyKind) do
      if StartsText(LazyPrefixStrings[Result], name)
        and (Length(GetGenericTypeParameters(name)) = 1) then
        Exit;
  end;
  Result := lkNone;
end;

function GetLazyTypeName(typeInfo: PTypeInfo): string;
var
  lazyKind: TLazyKind;
  name: string;
  i: Integer;
begin
  lazyKind := GetLazyKind(typeInfo);
  name := typeInfo.TypeName;
  if lazyKind > lkNone then
  begin
    i := Length(LazyPrefixStrings[lazyKind]) + 1;
    Result := Copy(name, i, Length(name) - i )
  end
  else
    Result := '';
end;

function GetLazyType(typeInfo: PTypeInfo): PTypeInfo;

  function GetLazyTypeUnsafe(typeInfo: PTypeInfo): PTypeInfo;
  var
    typeName: string;
    rttiType: TrttiType;
  begin
    typeName := GetGenericTypeParameters(typeInfo.TypeName)[0];
    rttiType := TType.Context.FindType(typeName);
    if Assigned(rttiType) then
      Result := rttiType.Handle
    else
    begin
      for rttiType in TType.Context.GetTypes do
        if rttiType.IsPublicType and (rttiType.QualifiedName = typeName) then
          Exit(rttiType.Handle);
      raise EInvalidOperationException.CreateResFmt(@STypeInfoNotFound, [typeName]);
    end;
  end;

var
  lazyKind: TLazyKind;
  method: TRttiMethod;
begin
  lazyKind := GetLazyKind(typeInfo);
  case lazyKind of
    lkFunc:
    begin
      method := TType.GetType(typeInfo).GetMethod('Invoke');
      if Assigned(method) then
        Result := method.ReturnType.Handle
      else
        Result := GetLazyTypeUnsafe(typeInfo);
    end;
    lkRecord, lkInterface:
    begin
      if lazyKind = lkRecord then
        typeInfo := PManagedField(PByte(@typeInfo.TypeData.ManagedFldCount) + SizeOf(Integer)).TypeRef^;
      method := TType.GetType(typeInfo).GetMethod('GetValue');
      if Assigned(method) then
        Result := method.ReturnType.Handle
      else
        Result := nil; // must not happen - ILazy<T> has methodinfo
    end;
  else
    Result := nil;
  end;
end;

function IsLazyType(typeInfo: PTypeInfo): Boolean;
begin
  Result := GetLazyKind(typeInfo) <> lkNone;
end;

// TODO: use typekind matrix for comparer functions
function CompareValue(const left, right: TValue): Integer;
const
  EmptyResults: array[Boolean, Boolean] of Integer = ((0, -1), (1, 0));
var
  leftIsEmpty, rightIsEmpty: Boolean;
  leftValue, rightValue: TValue;
begin
  leftIsEmpty := left.IsEmpty;
  rightIsEmpty := right.IsEmpty;
  if leftIsEmpty or rightIsEmpty then
    Result := EmptyResults[leftIsEmpty, rightIsEmpty]
  else if left.IsOrdinal and right.IsOrdinal then
    Result := Math.CompareValue(left.AsOrdinal, right.AsOrdinal)
  else if left.IsFloat and right.IsFloat then
    Result := Math.CompareValue(left.AsExtended, right.AsExtended)
  else if left.IsString and right.IsString then
    Result := SysUtils.AnsiCompareStr(left.AsString, right.AsString)
  else if left.IsObject and right.IsObject then
    Result := NativeInt(left.AsObject) - NativeInt(right.AsObject) // TODO: instance comparer
  else if left.IsVariant and right.IsVariant then
  begin
    case VarCompareValue(left.AsVariant, right.AsVariant) of
      vrEqual: Result := 0;
      vrLessThan: Result := -1;
      vrGreaterThan: Result := 1;
      vrNotEqual: Result := -1;
    else
      Result := 0;
    end;
  end
  else if IsNullable(left.TypeInfo) and IsNullable(right.TypeInfo) then
  begin
    leftIsEmpty := not left.TryGetNullableValue(leftValue);
    rightIsEmpty := not right.TryGetNullableValue(rightValue);
    if leftIsEmpty or rightIsEmpty then
      Result := EmptyResults[leftIsEmpty, rightIsEmpty]
    else
      Result := CompareValue(leftValue, rightValue);
  end else
    Result := 0;
end;

function GetSetSize(typeInfo: PTypeInfo): Integer;
var
  typeData: PTypeData;
  count: Integer;
begin
  typeData := GetTypeData(typeInfo);
  typeData := GetTypeData(typeData.CompType^);
  if typeData.MinValue = 0 then
    case typeData.MaxValue of
      0..7: Exit(1);
      8..15: Exit(2);
      16..31: Exit(4);
    end;
  count := typeData.MaxValue - typeData.MinValue + 1;
  Result := count div 8;
  if count mod 8 <> 0 then
    Inc(Result);
end;

function GetTypeSize(typeInfo: PTypeInfo): Integer;
const
  COrdinalSizes: array[TOrdType] of Integer = (
    SizeOf(ShortInt){1},
    SizeOf(Byte){1},
    SizeOf(SmallInt){2},
    SizeOf(Word){2},
    SizeOf(Integer){4},
    SizeOf(Cardinal){4});
  CFloatSizes: array[TFloatType] of Integer = (
    SizeOf(Single){4},
    SizeOf(Double){8},
{$IFDEF ALIGN_STACK}
    16,
{$ELSE}
    SizeOf(Extended){10},
{$ENDIF}
    SizeOf(Comp){8},
    SizeOf(Currency){8});
begin
  case typeInfo.Kind of
{$IFNDEF NEXTGEN}
    tkChar:
      Result := SizeOf(AnsiChar){1};
{$ENDIF}
    tkWChar:
      Result := SizeOf(WideChar){2};
    tkInteger, tkEnumeration:
      Result := COrdinalSizes[typeInfo.TypeData.OrdType];
    tkFloat:
      Result := CFloatSizes[typeInfo.TypeData.FloatType];
    tkString, tkLString, tkUString, tkWString, tkInterface, tkClass, tkClassRef, tkDynArray, tkPointer, tkProcedure:
      Result := SizeOf(Pointer);
    tkMethod:
      Result := SizeOf(TMethod);
    tkInt64:
      Result := SizeOf(Int64){8};
    tkVariant:
      Result := SizeOf(Variant);
    tkSet:
      Result := GetSetSize(typeInfo);
    tkRecord:
      Result := typeInfo.TypeData.RecSize;
    tkArray:
      Result := typeInfo.TypeData.ArrayData.Size;
  else
    Assert(False, 'Unsupported type'); { TODO -o##jwp -cEnhance : add more context to the assert }
    Result := -1;
  end;
end;

function TypesOf(const values: array of TValue): TArray<PTypeInfo>;
var
  i: Integer;
begin
  SetLength(Result, Length(values));
  for i := 0 to High(values) do
    Result[i] := values[i].TypeInfo;
end;

function MethodReferenceToMethodPointer(const methodRef): TMethodPointer;
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  if Pointer(methodRef) = nil then
    Exit(nil);
  // 3 is offset of Invoke, after QI, AddRef, Release
  TMethod(Result).Code := PPVtable(methodRef)^^[3];
  TMethod(Result).Data := Pointer(methodRef);
end;

function MethodPointerToMethodReference(const method: TMethodPointer): IInterface;
begin
  Result := IInterface(TMethod(method).Data);
end;

function SkipShortString(P: PByte): Pointer;
begin
  Result := P + P^ + 1;
end;

function StreamToVariant(const stream: TStream): Variant;
var
  lock: Pointer;
  size: Integer;
begin
  if not Assigned(stream) then
    Exit(Null);
  size := stream.Size;
  if size = 0 then
    Exit(Null);
  stream.Position := 0;
  Result := VarArrayCreate([0, size - 1], varByte);
  lock := VarArrayLock(Result);
  try
    stream.ReadBuffer(lock^, stream.Size);
  finally
    VarArrayUnlock(Result);
  end;
end;

function GetGenericTypeParameters(const typeName: string): TArray<string>;

  function ScanChar(const s: string; var index: Integer): Boolean;
  var
    level: Integer;
  begin
    Result := False;
    level := 0;
    while index <= Length(s) do
    begin
      case s[index] of
        ',': if level = 0 then Exit(True);
        '<': Inc(level);
        '>': Dec(level);
      end;
      Inc(index);
      Result := level = 0;
    end;
  end;

  function SplitTypes(const s: string): TArray<string>;
  var
    startPos, index, len: Integer;
  begin
    Result := nil;
    startPos := 1;
    index := 1;
    while ScanChar(s, index) do
    begin
      len := Length(Result);
      SetLength(Result, len + 1);
      Result[len] := Copy(s, startPos, index - startPos);
      Inc(index);
      startPos := index;
    end;
  end;

var
  i: Integer;
  s: string;
begin
  s := typeName;
  i := Pos('<', s);
  if i = 0 then
    Exit(nil);
  s := Copy(s, i + 1, Length(s) - i - 1);
  Result := SplitTypes(s);
end;

type
  TVarArrayBoundHelper = record helper for TVarArrayBound
    function GetHighBound: Integer; inline;
    property HighBound: Integer read GetHighBound;
  end;

function TVarArrayBoundHelper.GetHighBound: Integer;
begin
  Result := LowBound + ElementCount - 1;
end;

function SameValue(const left, right: Variant): Boolean;

  function MoveNext(const bounds: TArray<TVarArrayBound>;
    var indices: TArray<Integer>): Boolean;
  var
    i: Integer;
  begin
    for i := Length(indices) - 1 downto 0 do
      if indices[i] < bounds[i].HighBound then
      begin
        Inc(indices[i]);
        Exit(True);
      end
      else
        indices[i] := bounds[i].LowBound;
    Result := False;
  end;

var
  isArray: Boolean;
  leftArr, rightArr: PVarArray;
  i, count: Integer;
  indices: TArray<Integer>;
  bounds: TArray<TVarArrayBound>;
begin
  isArray := VarType(left) and varArray = varArray;
  if isArray <> (VarType(right) and varArray = varArray) then
    Exit(False);

  if not isArray then
    Exit(left = right);

  leftArr := VarArrayAsPSafeArray(left);
  rightArr := VarArrayAsPSafeArray(right);
  if leftArr.DimCount <> rightArr.DimCount then
    Exit(False);
  SetLength(indices, leftArr.DimCount);
  SetLength(bounds, leftArr.DimCount);
{$RANGECHECKS OFF}
  for i := leftArr.DimCount - 1 downto 0 do
  begin
    count := leftArr.Bounds[i].ElementCount;
    if count = 0 then
      Exit(True);
    if count <> rightArr.Bounds[i].ElementCount then
      Exit(False);
    bounds[leftArr.DimCount - 1 - i] := leftArr.Bounds[i];
  end;
{$IFDEF RANGECHECKS_ON}
{$RANGECHECKS ON}
{$ENDIF}
  repeat
    if not SameValue(VarArrayGet(left, indices), VarArrayGet(right, indices)) then
      Exit(False);
  until not MoveNext(bounds, indices);
  Result := True;
end;

function VarIsNullOrEmpty(const value: Variant): Boolean;
begin
  Result := FindVarData(value).VType in [varEmpty, varNull];
end;

function VarArrayLength(const value: Variant; dim: Integer): Integer;
var
  arrayRef: PVarArray;
  lo, hi: Integer;
begin
  arrayRef := VarArrayAsPSafeArray(value);
  VarResultCheck(SafeArrayGetLBound(arrayRef, dim, lo));
  VarResultCheck(SafeArrayGetUBound(arrayRef, dim, hi));
  Result := hi - lo + 1;
end;

function GetVirtualMethod(const classType: TClass; const index: Integer): Pointer;
begin
  Result := PPointer(IntPtr(classType) + IntPtr(index * SizeOf(Pointer)))^;
end;

type
  TAbstractObject = class
    procedure AbstractMethod; virtual; abstract;
  end;

function GetAbstractError: Pointer;
begin
  Result := PPointer(TAbstractObject)^
end;

{$IFNDEF DELPHIXE3_UP}
function AtomicIncrement(var target: Integer): Integer;
asm
{$IFDEF CPUX86}
  mov ecx,eax
  mov eax,1
  lock xadd [ecx],eax
  inc eax
{$ENDIF}
{$IFDEF CPUX64}
  mov eax,1
  lock xadd [rcx],eax
  inc eax
{$ENDIF}
end;

function AtomicDecrement(var target: Integer): Integer;
asm
{$IFDEF CPUX86}
  mov ecx,eax
  mov eax,-1
  lock xadd [ecx],eax
  dec eax
{$ENDIF}
{$IFDEF CPUX64}
  mov eax,-1
  lock xadd [rcx],eax
  dec eax
{$ENDIF}
end;

function AtomicCmpExchange(var target: Integer; newValue, comparand: Integer): Integer;
asm
{$IFDEF CPUX86}
  xchg eax,ecx
  lock cmpxchg [ecx],edx  
{$ENDIF}
{$IFDEF CPUX64}
  mov rax,r8
  lock cmpxchg [rcx],edx
{$ENDIF}
end;

function AtomicCmpExchange(var target: Pointer; newValue, comparand: Pointer): TObject;
asm
{$IFDEF CPUX86}
  xchg eax,ecx
  lock cmpxchg [ecx],edx
{$ENDIF}
{$IFDEF CPUX64}
  mov rax,r8
  lock cmpxchg [rcx],edx
{$ENDIF}
end;
{$ENDIF}

procedure IncUnchecked(var i: Integer; const n: Integer = 1); inline;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(i, n);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TGuidHelper'}

{$IFDEF DELPHI2010}
class function TGuidHelper.Create(const B: TBytes): TGUID;
begin
  if Length(B) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  Move(B[0], Result, SizeOf(Result));
end;

class function TGuidHelper.Create(const S: string): TGUID;
begin
  Result := StringToGUID(S);
end;

class function TGuidHelper.Create(A: Integer; B, C: SmallInt;
  const D: TBytes): TGUID;
begin
  if Length(D) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [8]);
  Result.D1 := LongWord(A);
  Result.D2 := Word(B);
  Result.D3 := Word(C);
  Move(D[0], Result.D4, SizeOf(Result.D4));
end;

class function TGuidHelper.Empty: TGuid;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TGuidHelper.NewGuid: TGuid;
begin
  if CreateGUID(Result) <> S_OK then
    RaiseLastOSError;
end;

class function TGuidHelper.&&op_Equality(const left, right: TGUID): Boolean;
var
  a, b: PIntegerArray;
begin
  a := PIntegerArray(@left);
  b := PIntegerArray(@right);
  Result := (a^[0] = b^[0]) and (a^[1] = b^[1]) and (a^[2] = b^[2]) and (a^[3] = b^[3]);
end;

class function TGuidHelper.&&op_Inequality(const left, right: TGUID): Boolean;
begin
  Result := not (left = right);
end;

function TGuidHelper.ToByteArray: TBytes;
begin
  SetLength(Result, 16);
  Move(D1, Result[0], SizeOf(Self));
end;

function TGuidHelper.ToString: string;
begin
  Result := GuidToString(Self);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TMethodHelper'}

{$IFNDEF DELPHIXE3_UP}
class function TMethodHelper.&&op_Equality(const left, Right: TMethod): Boolean;
begin
  Result := (left.Data = right.Data) and (left.Code = right.Code);
end;

class function TMethodHelper.&&op_Inequality(const left, Right: TMethod): Boolean;
begin
  Result := (left.Data <> right.Data) or (left.Code <> right.Code);
end;

class function TMethodHelper.&&op_GreaterThan(const left, right: TMethod): Boolean;
begin
  Result := (UIntPtr(left.Data) > UIntPtr(right.Data))
    or ((left.Data = right.Data) and (UIntPtr(left.Code) > UIntPtr(right.Code)));
end;

class function TMethodHelper.&&op_LessThan(const left, right: TMethod): Boolean;
begin
  Result := (UIntPtr(left.Data) < UIntPtr(right.Data))
    or ((left.Data = right.Data) and (UIntPtr(left.Code) < UIntPtr(right.Code)));
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TType'}

class constructor TType.Create;
begin
  fContext := TRttiContext.Create;
end;

class destructor TType.Destroy;
begin
  fContext.Free;
end;

class function TType.GetType(typeInfo: PTypeInfo): TRttiType;
begin
  Result := fContext.GetType(typeInfo);
end;

class function TType.GetType(classType: TClass): TRttiInstanceType;
begin
  Result := TRttiInstanceType(fContext.GetType(classType));
end;

class function TType.GetType<T>: TRttiType;
begin
  Result := fContext.GetType(TypeInfo(T));
end;

class function TType.HasWeakRef<T>: Boolean;
begin
{$IFDEF DELPHIXE7_UP}
  Result := System.HasWeakRef(T);
{$ELSE}
  {$IFDEF WEAKREF}
  Result := TypInfo.HasWeakRef(TypeInfo(T));
  {$ELSE}
  Result := False;
  {$ENDIF}
{$ENDIF}
end;

class function TType.IsManaged<T>: Boolean;
begin
{$IFDEF DELPHIXE7_UP}
  Result := System.IsManagedType(T);
{$ELSE}
  Result := Rtti.IsManaged(TypeInfo(T));
{$ENDIF}
end;

class function TType.Kind<T>: TTypeKind;
{$IFDEF DELPHIXE7_UP}
begin
  Result := System.GetTypeKind(T);
{$ELSE}
var
  typeInfo: PTypeInfo;
begin
  typeInfo := System.TypeInfo(T);
  if typeInfo = nil then
    Exit(tkUnknown);
  Result := typeInfo.Kind;
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TEnum'}

class function TEnum.ToInteger<T>(const value: T): Integer;
begin
  case SizeOf(T) of
    1: Result := PByte(@value)^;
    2: Result := PWord(@value)^;
    4: Result := PInteger(@value)^;
  end;
end;

class function TEnum.IsValid<T>(const value: Integer): Boolean;
var
  data: PTypeData;
begin
  Guard.CheckTypeKind<T>(tkEnumeration, 'T');
  data := GetTypeData(TypeInfo(T));
  Result := (value >= data.MinValue) and (value <= data.MaxValue);
end;

class function TEnum.IsValid<T>(const value: T): Boolean;
var
  intValue: Integer;
begin
  intValue := ToInteger<T>(value);
  Result := IsValid<T>(intValue);
end;

class function TEnum.GetName<T>(const value: Integer): string;
begin
  Guard.CheckEnum<T>(value, 'value');
  Result := GetEnumName(TypeInfo(T), value);
end;

class function TEnum.GetName<T>(const value: T): string;
var
  intValue: Integer;
begin
  intValue := ToInteger<T>(value);
  Result := GetName<T>(intValue);
end;

class function TEnum.GetNames<T>: TStringDynArray;
var
  typeData: PTypeData;
{$IFDEF NEXTGEN}
  p: TTypeInfoFieldAccessor;
{$ELSE}
  p: PShortString;
{$ENDIF}
  i: Integer;
begin
  Guard.CheckTypeKind<T>(tkEnumeration, 'T');
  typeData := GetTypeData(TypeInfo(T));
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
{$IFDEF NEXTGEN}
  p := typedata^.NameListFld;
{$ELSE}
  p := @typedata.NameList;
{$ENDIF}
  for i := Low(Result) to High(Result) do
  begin
{$IFDEF NEXTGEN}
    Result[i] := p.ToString;
    p.SetData(p.Tail);
{$ELSE}
    Result[i] := UTF8ToString(p^);
    Inc(PByte(p), Length(p^) + 1);
{$ENDIF}
  end;
end;

class function TEnum.GetValue<T>(const value: string): Integer;
var
  temp: T;
begin
  temp := Parse<T>(value);
  Result := ToInteger<T>(temp);
end;

class function TEnum.GetValue<T>(const value: T): Integer;
begin
  Guard.CheckEnum<T>(value, 'value');
  Result := ToInteger<T>(value);
end;

class function TEnum.GetValues<T>: TIntegerDynArray;
var
  typeData: PTypeData;
  i: Integer;
begin
  Guard.CheckTypeKind<T>(tkEnumeration, 'T');
  typeData := GetTypeData(TypeInfo(T));
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
  for i := Low(Result) to High(Result) do
    Result[i] := i;
end;

class function TEnum.TryParse<T>(const value: Integer; out enum: T): Boolean;
begin
  Result := IsValid<T>(value);
  if Result then
    Move(value, enum, SizeOf(T));
end;

class function TEnum.TryParse<T>(const value: string; out enum: T): Boolean;
var
  intValue: Integer;
begin
  Guard.CheckTypeKind<T>(tkEnumeration, 'T');
  intValue := GetEnumValue(TypeInfo(T), value);
  Result := TryParse<T>(intValue, enum);
end;

class function TEnum.Parse<T>(const value: Integer): T;
begin
  if not TryParse<T>(value, Result) then
    raise EFormatException.CreateResFmt(@SIncorrectFormat, [IntToStr(value)]);
end;

class function TEnum.Parse<T>(const value: string): T;
begin
  if not TryParse<T>(value, Result) then
    raise EFormatException.CreateResFmt(@SIncorrectFormat, [value]);
end;

{$ENDREGION}


{$REGION 'TBaseAttribute'}

constructor TBaseAttribute.Create;
begin
  inherited Create;
end;

{$ENDREGION}


{$REGION 'DefaultAttribute'}

{$IFNDEF DELPHIXE3_UP}
constructor DefaultAttribute.Create(const defaultValue: Integer);
begin
  inherited Create;
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: Boolean);
begin
  inherited Create;
  fValue := Ord(defaultValue);
end;

constructor DefaultAttribute.Create(const defaultValue: Cardinal);
begin
  inherited Create;
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: string);
begin
  inherited Create;
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: Extended);
begin
  inherited Create;
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: Int64);
begin
  inherited Create;
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: UInt64);
begin
  inherited Create;
  fValue := defaultValue;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'ManagedAttribute'}

constructor ManagedAttribute.Create(createInstance: Boolean);
begin
  inherited Create;
  fCreateInstance := createInstance;
end;

constructor ManagedAttribute.Create(instanceClass: TClass);
begin
  inherited Create;
  fCreateInstance := True;
  fInstanceClass := instanceClass;
end;

constructor ManagedAttribute.Create(const factory: TFunc<PTypeInfo,Pointer>);
begin
  Create(instanceClass);
  fFactory := factory;
end;

{$ENDREGION}


{$REGION 'TInitTable'}

class constructor TInitTable.Create;
begin
{$IFDEF USE_VMTAUTOTABLE}
  InitTables := TObjectList<TInitTable>.Create;
{$ELSE}
  InitTables := TObjectDictionary<TClass,TInitTable>.Create([doOwnsValues]);
{$ENDIF}
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  FormatSettings.ShortTimeFormat := 'hh:mm:ss';
end;

class destructor TInitTable.Destroy;
begin
  InitTables.Free;
end;

constructor TInitTable.Create(classType: TClass);
var
  t: TRttiType;
  f: TRttiField;
  p: TRttiProperty;
  a: TCustomAttribute;
  setter: Pointer;
begin
  t := TType.GetType(classType);
  for f in t.GetFields do
    for a in f.GetAttributes do
      if a is DefaultAttribute then
        AddDefaultField(f.FieldType.Handle, DefaultAttribute(a).Value, f.Offset)
      else if a is ManagedAttribute then
        if f.FieldType.TypeKind in [tkClass, tkInterface] then
          AddManagedField(f, ManagedAttribute(a));

  for p in t.GetProperties do
    for a in p.GetAttributes do
      if a is DefaultAttribute then
      begin
        if p.IsWritable then
          setter := TRttiInstanceProperty(p).PropInfo.SetProc
        else
        begin
          // if the property is read-only but backed by a field it can be initialized
          setter := TRttiInstanceProperty(p).PropInfo.GetProc;
          if IntPtr(setter) and PROPSLOT_MASK <> PROPSLOT_FIELD then
            raise EInvalidOperationException.Create('Property not writable'); // TODO
        end;

        if IntPtr(setter) and PROPSLOT_MASK = PROPSLOT_FIELD then
          AddDefaultField(p.PropertyType.Handle, DefaultAttribute(a).Value,
            IntPtr(setter) and not PROPSLOT_MASK)
        else
          AddDefaultProperty(p.PropertyType.Handle, DefaultAttribute(a).Value,
            TRttiInstanceProperty(p).PropInfo);
      end;
end;

destructor TInitTable.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(DefaultFields) do
    FreeAndNil(DefaultFields[i]);
  for i := 0 to High(ManagedFields) do
    FreeAndNil(ManagedFields[i]);
  inherited Destroy;
end;

procedure TInitTable.AddDefaultField(fieldType: PTypeInfo;
  const value: Variant; offset: Integer);
var
  defaultField: TInitializableField;
begin
  defaultField := nil;
  case fieldType.Kind of
    tkInteger, tkEnumeration:
      case fieldType.TypeData.OrdType of
        otSByte: defaultField := TDefaultField<ShortInt>.Create(offset, value);
        otSWord: defaultField := TDefaultField<SmallInt>.Create(offset, value);
        otSLong: defaultField := TDefaultField<Integer>.Create(offset, value);
        otUByte: defaultField := TDefaultField<Byte>.Create(offset, value);
        otUWord: defaultField := TDefaultField<Word>.Create(offset, value);
        otULong: defaultField := TDefaultField<Cardinal>.Create(offset, value);
      end;
    {$IFNDEF NEXTGEN}
    tkChar:
      defaultField  := TDefaultField<AnsiChar>.Create(offset, value);
    {$ENDIF}
    tkFloat:
      if (fieldType = TypeInfo(TDateTime)) and (VarType(value) = varUString) then
        defaultField := TDefaultField<TDateTime>.Create(offset, StrToDateTime(value, FormatSettings))
      else if (fieldType = TypeInfo(TDate)) and (VarType(value) = varUString) then
        defaultField := TDefaultField<TDate>.Create(offset, StrToDate(value, FormatSettings))
      else if (fieldType = TypeInfo(TTime)) and (VarType(value) = varUString) then
        defaultField := TDefaultField<TTime>.Create(offset, StrToTime(value, FormatSettings))
      else
        case FieldType.TypeData.FloatType of
          ftSingle: defaultField := TDefaultField<Single>.Create(offset, value);
          ftDouble: defaultField := TDefaultField<Double>.Create(offset, value);
          ftExtended: defaultField := TDefaultField<Extended>.Create(offset, value);
          ftComp: defaultField := TDefaultField<Comp>.Create(offset, value);
          ftCurr: defaultField := TDefaultField<Currency>.Create(offset, value);
        end;
    tkWChar:
      defaultField := TDefaultField<Char>.Create(offset, value);
    {$IFNDEF NEXTGEN}
    tkWString:
      defaultField := TDefaultField<WideString>.Create(offset, value);
    {$ENDIF}
    tkVariant:
      defaultField := TDefaultField<Variant>.Create(offset, value);
    tkInt64:
      if fieldType.TypeData.MinInt64Value > fieldType.TypeData.MaxInt64Value then
        defaultField := TDefaultField<UInt64>.Create(offset, value)
      else
        defaultField := TDefaultField<Int64>.Create(offset, value);
    tkUString:
      defaultField := TDefaultField<UnicodeString>.Create(offset, value);
    tkClassRef, tkPointer:
      defaultField := TDefaultField<Pointer>.Create(offset, value);
  end;
  if defaultField <> nil then
  begin
    DefaultFieldCount := Length(DefaultFields) + 1;
    SetLength(DefaultFields, DefaultFieldCount);
    DefaultFields[DefaultFieldCount - 1] := defaultField;
  end;
end;

procedure TInitTable.AddDefaultProperty(fieldType: PTypeInfo;
  const value: Variant; propInfo: PPropInfo);
var
  defaultField: TInitializableField;
begin
  defaultField := nil;
  case fieldType.Kind of
    tkInteger, tkEnumeration:
      case fieldType.TypeData.OrdType of
        otSByte: defaultField := TDefaultProperty<ShortInt>.Create(propInfo, value);
        otSWord: defaultField := TDefaultProperty<SmallInt>.Create(propInfo, value);
        otSLong: defaultField := TDefaultProperty<Integer>.Create(propInfo, value);
        otUByte: defaultField := TDefaultProperty<Byte>.Create(propInfo, value);
        otUWord: defaultField := TDefaultProperty<Word>.Create(propInfo, value);
        otULong: defaultField := TDefaultProperty<Cardinal>.Create(propInfo, value);
      end;
    {$IFNDEF NEXTGEN}
    tkChar:
      defaultField  := TDefaultProperty<AnsiChar>.Create(propInfo, value);
    {$ENDIF}
    tkFloat:
      if (fieldType = TypeInfo(TDateTime)) and (VarType(value) = varUString) then
        defaultField := TDefaultProperty<TDateTime>.Create(propInfo, StrToDateTime(value, FormatSettings))
      else if (fieldType = TypeInfo(TDate)) and (VarType(value) = varUString) then
        defaultField := TDefaultProperty<TDate>.Create(propInfo, StrToDate(value, FormatSettings))
      else if (fieldType = TypeInfo(TTime)) and (VarType(value) = varUString) then
        defaultField := TDefaultProperty<TTime>.Create(propInfo, StrToTime(value, FormatSettings))
      else
        case fieldType.TypeData.FloatType of
          ftSingle: defaultField := TDefaultProperty<Single>.Create(propInfo, value);
          ftDouble: defaultField := TDefaultProperty<Double>.Create(propInfo, value);
          ftExtended: defaultField := TDefaultProperty<Extended>.Create(propInfo, value);
          ftComp: defaultField := TDefaultProperty<Comp>.Create(propInfo, value);
          ftCurr: defaultField := TDefaultProperty<Currency>.Create(propInfo, value);
        end;
    tkWChar:
      defaultField := TDefaultProperty<Char>.Create(propInfo, value);
    {$IFNDEF NEXTGEN}
    tkWString:
      defaultField := TDefaultProperty<WideString>.Create(propInfo, value);
    {$ENDIF}
    tkVariant:
      defaultField := TDefaultProperty<Variant>.Create(propInfo, value);
    tkInt64:
      if fieldType.TypeData.MinInt64Value > fieldType.TypeData.MaxInt64Value then
        defaultField := TDefaultProperty<UInt64>.Create(propInfo, value)
      else
        defaultField := TDefaultProperty<Int64>.Create(propInfo, value);
    tkUString:
      defaultField := TDefaultProperty<UnicodeString>.Create(propInfo, value);
    tkClassRef, tkPointer:
      defaultField := TDefaultProperty<Pointer>.Create(propInfo, value);
  end;
  if defaultField <> nil then
  begin
    DefaultFieldCount := Length(DefaultFields) + 1;
    SetLength(DefaultFields, DefaultFieldCount);
    DefaultFields[DefaultFieldCount - 1] := defaultField;
  end;
end;

procedure TInitTable.AddManagedField(const field: TRttiField;
  const attribute: ManagedAttribute);

  function GetInterfaceEntry(cls: TClass; intf: PTypeInfo): PInterfaceEntry;
  var
    intfGuid: TGUID;
    interfaceTable: PInterfaceTable;
    {$IFNDEF DELPHI2010}
    p: PPPTypeInfo;
    {$ENDIF}
    i: Integer;
  begin
    {$IFDEF DELPHI2010}
    // Delphi 2010 does not have the PPTypeInfo array
    // after the TInterfaceEntry array in TInterfaceTable
    // so only interfaces with a GUID can be used
    if not (ifHasGuid in intf.TypeData.IntfFlags) then
      Exit(nil);
    {$ENDIF}
    intfGuid := intf.TypeData.Guid;
    repeat
      interfaceTable := cls.GetInterfaceTable;
      if interfaceTable <> nil then
      begin
        {$IFNDEF DELPHI2010}
        p := @interfaceTable.Entries[interfaceTable.EntryCount];
        {$ENDIF}
        for i := 0 to interfaceTable.EntryCount - 1 do
        begin
          Result := @interfaceTable.Entries[i];
          {$IFNDEF DELPHI2010}
          if p^^ = intf then
            Exit;
          Inc(p);
          {$ENDIF}
          if Result.IID = intf.TypeData.Guid then
            Exit;
        end;
      end;
      cls := cls.ClassParent;
    until cls = nil;
    Result := nil;
  end;

var
  fieldType: PTypeInfo;
  offset: Integer;
  createInstance: Boolean;
  cls: TClass;
  factory: TFunc<PTypeInfo,Pointer>;
  managedField: TFinalizableField;
  entry: PInterfaceEntry;
begin
  fieldType := field.FieldType.Handle;
  offset := field.Offset;
  createInstance := attribute.CreateInstance;
  cls := attribute.InstanceClass;
  factory := attribute.Factory;
  managedField := nil;
  case fieldType.Kind of
    tkClass:
    begin
      if not Assigned(factory) and not Assigned(cls) and createInstance then
        cls := fieldType.TypeData.ClassType;
      managedField := TManagedObjectField.Create(offset, fieldType, cls, factory);
    end;
    tkInterface:
    begin
      if Assigned(cls) then
      begin
        entry := GetInterfaceEntry(cls, fieldType);
        if entry = nil then
          raise EInvalidOperationException.CreateFmt(
            'class %s is not compatible with interface %s (field %s)', [
            cls.ClassName, fieldType.TypeName, field.Name]);
      end
      else
        entry := nil;
      managedField := TManagedInterfaceField.Create(offset, fieldType, cls, factory, entry);
    end;
  end;
  if managedField <> nil then
  begin
    ManagedFieldCount := Length(ManagedFields) + 1;
    SetLength(ManagedFields, ManagedFieldCount);
    ManagedFields[ManagedFieldCount - 1] := managedField;
  end;
end;

{$IFDEF USE_VMTAUTOTABLE}
function CreateFieldTable(classType: TClass): TInitTable;
var
  n: UINT_PTR;
begin
  Result := TInitTable.Create(classType);
  WriteProcessMemory(GetCurrentProcess,
    Pointer(NativeInt(classType) + vmtAutoTable), @Result, SizeOf(Pointer), n);
  TInitTable.InitTables.Add(Result);
end;
{$ENDIF}

function GetInitTable(classType: TClass): TInitTable;
{$IFDEF USE_VMTAUTOTABLE}
begin
  Result := PPointer(NativeInt(classType) + vmtAutoTable)^;
  if Result = nil then
    Result := CreateFieldTable(classType);
{$ELSE}
begin
  TMonitor.Enter(TInitTable.InitTables);
  try
    if not TInitTable.InitTables.TryGetValue(classType, Result) then
    begin
      Result := TInitTable.Create(classType);
      TInitTable.InitTables.Add(classType, Result);
    end;
  finally
    TMonitor.Exit(TInitTable.InitTables);
  end;
{$ENDIF}
end;

{$IFDEF RANGECHECKS_ON}{$RANGECHECKS OFF}{$ENDIF}
procedure TInitTable.InitInstance(instance: Pointer);
var
  f: ^TInitializableField;
  i: Integer;
begin
  f := @DefaultFields[0];
  for i := 0 to DefaultFieldCount - 1 do //FI:W528
  begin
    f.InitializeValue(instance);
    Inc(f);
  end;
  f := @ManagedFields[0];
  for i := 0 to ManagedFieldCount - 1 do //FI:W528
  begin
    f.InitializeValue(instance);
    Inc(f);
  end;
end;

{$IFNDEF AUTOREFCOUNT}
procedure TInitTable.CleanupInstance(instance: Pointer);
var
  f: ^TFinalizableField;
  i: Integer;
begin
  f := @ManagedFields[0];
  for i := 0 to ManagedFieldCount - 1 do //FI:W528
  begin
    f.FinalizeValue(instance);
    Inc(f);
  end;
end;
{$ENDIF}
{$IFDEF RANGECHECKS_ON}{$RANGECHECKS ON}{$ENDIF}

{$ENDREGION}


{$REGION 'TInitTable.TDefaultField<T>'}

constructor TInitTable.TDefaultField<T>.Create(offset: Integer; const value: Variant);
begin
  inherited Create;
  fOffset := offset;
  fValue := TValue.FromVariant(value).AsType<T>; // TODO
end;

procedure TInitTable.TDefaultField<T>.InitializeValue(instance: Pointer);
begin
  PT(PByte(instance) + fOffset)^ := fValue;
end;

{$ENDREGION}


{$REGION 'TInitTable.TDefaultProperty<T>'}

constructor TInitTable.TDefaultProperty<T>.Create(propInfo: PPropInfo; const value: Variant);
begin
  inherited Create;
  fPropInfo := propInfo;
  fValue := TValue.FromVariant(value).AsType<T>; // TODO
end;

class function TInitTable.GetCodePointer(instance: TObject; p: Pointer): Pointer;
begin
  if IntPtr(p) and PROPSLOT_MASK = PROPSLOT_VIRTUAL then
    Result := PPointer(PNativeInt(instance)^ + SmallInt(IntPtr(p)))^
  else
    Result := p;
end;

procedure TInitTable.TDefaultProperty<T>.InitializeValue(instance: Pointer);
var
  method: TMethod;
begin
  method.Code := GetCodePointer(instance, fPropInfo.SetProc);
  method.Data := instance;
  if fPropInfo.Index = Low(fPropInfo.Index) then
    TSetter(method)(fValue)
  else
    TIndexedSetter(method)(fPropInfo.Index, fValue);
end;

{$ENDREGION}


{$REGION 'TInitTable.TManagedObjectField'}

constructor TInitTable.TManagedObjectField.Create(offset: Integer;
  fieldType: PTypeInfo; cls: TClass; const factory: TFunc<PTypeInfo,Pointer>);
begin
  inherited Create;
  fOffset := offset;
  fFieldType := fieldType;
  fCls := cls;
  fFactory := factory;
  if Assigned(cls) and not Assigned(factory) then
    fCtor := TActivator.FindConstructor(cls);
end;

procedure TInitTable.TManagedObjectField.FinalizeValue(instance: Pointer);
begin
  FreeAndNil(Pointer(PByte(instance) + fOffset)^);
end;

procedure TInitTable.TManagedObjectField.InitializeValue(instance: Pointer);
begin
  if Assigned(fCtor) then
    TObject(Pointer(PByte(instance) + fOffset)^) := TObject(fCtor(fCls))
  else if Assigned(fFactory) then
    TObject(Pointer(PByte(instance) + fOffset)^) := fFactory(fFieldType);
end;

{$ENDREGION}


{$REGION 'TInitTable.TManagedInterfaceField'}

function InvokeImplGetter(const Self: TObject; implGetter: NativeUInt): IInterface;
var
  method: function: IInterface of object;
begin
  TMethod(method).Data := Self;
  {$IF SizeOf(NativeUInt) = 4}
  case implGetter of
    $FF000000..$FFFFFFFF:
      Result := IInterface(PPointer(PByte(Self) + (implGetter and $00FFFFFF))^);
    $FE000000..$FEFFFFFF:
    begin
      TMethod(method).Code := PPointer(PNativeInt(Self)^ + SmallInt(implGetter))^;
      Result := method;
    end;
  else
    TMethod(method).Code := Pointer(implGetter);
    Result := method;
  end;
  {$ELSE}
  if (implGetter and $FF00000000000000) = $FF00000000000000 then
    Result := IInterface(PPointer(PByte(Self) + (implGetter and $00FFFFFFFFFFFFFF))^)
  else if (implGetter and $FF00000000000000) = $FE00000000000000 then
  begin
    TMethod(method).Code := PPointer(PNativeInt(Self)^ + SmallInt(implGetter))^;
    Result := method;
  end
  else
  begin
    TMethod(method).Code := Pointer(implGetter);
    Result := method;
  end;
  {$IFEND}
end;

constructor TInitTable.TManagedInterfaceField.Create(offset: Integer;
  fieldType: PTypeInfo; cls: TClass; const factory: TFunc<PTypeInfo,Pointer>;
  entry: PInterfaceEntry);
begin
  inherited Create(offset, fieldType, cls, factory);
  fEntry := entry;
end;

function TInitTable.TManagedInterfaceField.CreateInstance: Pointer;
var
  obj: Pointer;
begin
  obj := fCtor(fCls);
  if fEntry.IOffset <> 0 then
  begin
    Result := Pointer(PByte(obj) + fEntry.IOffset);
    if Result <> nil then
      IInterface(Result)._AddRef;
  end
  else
  begin
    Result := nil;
    IInterface(Result) := InvokeImplGetter(obj, fEntry.ImplGetter);
  end;
end;

procedure TInitTable.TManagedInterfaceField.FinalizeValue(instance: Pointer);
begin
end;

procedure TInitTable.TManagedInterfaceField.InitializeValue(instance: Pointer);
var
  intf: Pointer;
begin
  if Assigned(fCtor) then
    intf := CreateInstance
  else if Assigned(fFactory) then
    intf := fFactory(fFieldType)
  else
    Exit;

  PPointer(PByte(instance) + fOffset)^ := intf;
end;

{$ENDREGION}


{$REGION 'TManagedObject'}

{$IFNDEF AUTOREFCOUNT}
procedure TManagedObject.FreeInstance;
begin
  GetInitTable(ClassType).CleanupInstance(Self);
  inherited FreeInstance;
end;
{$ENDIF}

class function TManagedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  GetInitTable(Self).InitInstance(Result);
end;

{$ENDREGION}


{$REGION 'TManagedInterfacedObject'}

{$IFNDEF AUTOREFCOUNT}
procedure TManagedInterfacedObject.FreeInstance;
begin
  GetInitTable(ClassType).CleanupInstance(Self);
  inherited FreeInstance;
end;
{$ENDIF}

class function TManagedInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  GetInitTable(Self).InitInstance(Result);
end;

{$ENDREGION}


{$REGION 'TValueHelper'}

var
  Nop_Instance: Pointer;

procedure TValueHelper.Init(typeInfo: Pointer);
begin
  with TValueData(Self) do
  begin
    FTypeInfo := typeInfo;
{$IF SizeOf(Extended) > SizeOf(TMethod)}
    FAsExtended := 0;
{$ELSE SizeOf(Extended) <= SizeOf(TMethod)}
    FAsMethod.Code := nil;
    FAsMethod.Data := nil;
{$IFEND}
{$IFDEF DELPHI2010}
    FHeapData := nil;
    Pointer(FHeapData) := Nop_Instance;
{$ELSE}
    FValueData := nil;
    Pointer(FValueData) := Nop_Instance;
{$ENDIF}
  end;
end;

function TValueHelper.AsPointer: Pointer;
begin
  case Kind of
    tkPointer:
{$IFDEF DELPHI2010}
      Result := Pointer(TValueData(Self).FAsSLong);
{$ELSE}
      Result := TValueData(Self).FAsPointer;
{$ENDIF}
    tkClass:
      Result := AsObject;
    tkInterface:
      Result := Pointer(AsInterface);
  else
    Guard.RaiseInvalidTypeCast(TypeInfo, System.TypeInfo(Pointer));
    Result := nil;
  end;
end;

{$IFDEF DELPHI2010}
function TValueHelper.AsString: string;
begin
  Result := AsType<string>;
end;
{$ENDIF}

function TValueHelper.AsType<T>: T;
begin
{$IFDEF DELPHI2010}
  if IsEmpty then
    Exit(Default(T));
{$ENDIF}
  if not TryAsInterface(System.TypeInfo(T), Result) then
  if not TryAsType<T>(Result) then
    Guard.RaiseInvalidTypeCast(TypeInfo, System.TypeInfo(T));
end;

function TValueHelper.Cast(typeInfo: PTypeInfo): TValue;
var
  intf: IInterface;
begin
  if TryAsInterface(typeInfo, intf) then
    TValue.Make(@intf, typeInfo, Result)
  else if not TryCast(typeInfo, Result) then
    Guard.RaiseInvalidTypeCast(Self.TypeInfo, typeInfo);
end;

function TValueHelper.CompareTo(const value: TValue): Integer;
begin
  Result := CompareValue(Self, value);
end;

function TValueHelper.Convert(targetType: PTypeInfo): TValue;
begin
  if not TryConvert(targetType, Result) then
    RaiseConversionError(TypeInfo, targetType);
end;

function TValueHelper.Convert(targetType: PTypeInfo;
  const formatSettings: TFormatSettings): TValue;
begin
  if not TryConvert(targetType, Result, formatSettings) then
    RaiseConversionError(TypeInfo, targetType);
end;

function TValueHelper.Convert<T>: TValue;
begin
  if not TryConvert(System.TypeInfo(T), Result) then
    RaiseConversionError(TypeInfo, System.TypeInfo(T));
end;

function TValueHelper.Convert<T>(const formatSettings: TFormatSettings): TValue;
begin
  if not TryConvert(System.TypeInfo(T), Result, formatSettings) then
    RaiseConversionError(TypeInfo, System.TypeInfo(T));
end;


{$REGION 'Equals functions'}

function EqualsFail(const left, right: TValue): Boolean; //FI:O804
begin
  Result := False;
end;

function EqualsInt2Int(const left, right: TValue): Boolean;
var
  leftValue, rightValue: Int64;
begin
  case left.TypeInfo.TypeData.OrdType of
    otSByte: leftValue := TValueData(left).FAsSByte;
    otSWord: leftValue := TValueData(left).FAsSWord;
    otSLong: leftValue := TValueData(left).FAsSLong;
  else
    leftValue := TValueData(left).FAsULong;
  end;

  case right.TypeInfo.TypeData.OrdType of
    otSByte: rightValue := TValueData(right).FAsSByte;
    otSWord: rightValue := TValueData(right).FAsSWord;
    otSLong: rightValue := TValueData(right).FAsSLong;
  else
    rightValue := TValueData(right).FAsULong;
  end;

  Result := leftValue = rightValue;
end;

function EqualsInt2Float(const left, right: TValue): Boolean;
begin
  if right.IsType<Single> then
    Result := Math.SameValue(left.AsInteger, right.AsType<Single>)
  else if right.IsType<Double> then
    Result := Math.SameValue(left.AsInteger, right.AsType<Double>)
  else
    Result := Math.SameValue(left.AsInteger, right.AsExtended);
end;

function EqualsInt2Int64(const left, right: TValue): Boolean;
begin
  Result := left.AsInteger = right.AsInt64;
end;

function EqualsFloat2Int(const left, right: TValue): Boolean;
begin
  case left.TypeData.FloatType of
    ftSingle: Result := Math.SameValue(left.AsType<Single>, right.AsInteger);
    ftDouble: Result := Math.SameValue(left.AsType<Double>, right.AsInteger);
  else
    Result := Math.SameValue(left.AsExtended, right.AsInteger);
  end;
end;

function EqualsFloat2Float(const left, right: TValue): Boolean;
begin
  case left.TypeData.FloatType of
    ftSingle:
      case right.TypeData.FloatType of
        ftSingle: Result := Math.SameValue(left.AsType<Single>, right.AsType<Single>);
        ftDouble: Result := Math.SameValue(left.AsType<Single>, right.AsType<Double>);
      else
        Result := Math.SameValue(left.AsType<Single>, right.AsExtended);
      end;
    ftDouble:
      case right.TypeData.FloatType of
        ftSingle: Result := Math.SameValue(left.AsType<Double>, right.AsType<Single>);
        ftDouble: Result := Math.SameValue(left.AsType<Double>, right.AsType<Double>);
      else
        Result := Math.SameValue(left.AsType<Double>, right.AsExtended);
      end;
  else
    case right.TypeData.FloatType of
      ftSingle: Result := Math.SameValue(left.AsExtended, right.AsType<Single>);
      ftDouble: Result := Math.SameValue(left.AsExtended, right.AsType<Double>);
    else
      Result := Math.SameValue(left.AsExtended, right.AsExtended);
    end;
  end;
end;

function EqualsFloat2Int64(const left, right: TValue): Boolean;
begin
  case left.TypeData.FloatType of
    ftSingle: Result := Math.SameValue(left.AsType<Single>, right.AsInt64);
    ftDouble: Result := Math.SameValue(left.AsType<Double>, right.AsInt64);
  else
    Result := Math.SameValue(left.AsExtended, right.AsInt64);
  end;
end;

function EqualsInt642Int(const left, right: TValue): Boolean;
begin
  Result := left.AsInt64 = right.AsInteger;
end;

function EqualsInt64ToFloat(const left, right: TValue): Boolean;
begin
  if right.IsType<Single> then
    Result := Math.SameValue(left.AsInt64, right.AsType<Single>)
  else if right.IsType<Double> then
    Result := Math.SameValue(left.AsInt64, right.AsType<Double>)
  else
    Result := Math.SameValue(left.AsInt64, right.AsExtended);
end;

function EqualsInt642Int64(const left, right: TValue): Boolean;
begin
  Result := left.AsInt64 = right.AsInt64;
end;

function EqualsStr2Str(const left, right: TValue): Boolean;
begin
  Result := left.AsString = right.AsString;
end;

function EqualsStr2Var(const left, right: TValue): Boolean;
begin
  Result := SameValue(left.AsString, right.AsVariant);
end;

function EqualsClass2Class(const left, right: TValue): Boolean;
begin
  Result := left.AsObject = right.AsObject;
end;

function EqualsPointer2Pointer(const left, right: TValue): Boolean;
begin
  Result := left.AsPointer = right.AsPointer;
end;

function EqualsIntf2Intf(const left, right: TValue): Boolean;
begin
  Result := left.AsInterface = right.AsInterface;
end;

function EqualsClassRef2ClassRef(const left, right: TValue): Boolean;
begin
  Result := left.AsClass = right.AsClass;
end;

function EqualsVar2Var(const left, right: TValue): Boolean;
begin
  Result := SameValue(left.AsVariant, right.AsVariant);
end;

function EqualsVar2Str(const left, right: TValue): Boolean;
begin
  Result := SameValue(left.AsVariant, right.AsString);
end;

function EqualsRec2Rec(const left, right: TValue): Boolean;

  function RawEquals(const recordType: TRttiType): Boolean;
  var
    leftRec, rightRec: Pointer;
    field: TRttiField;
    leftValue, rightValue: TValue;
  begin
    if left.TypeInfo = right.TypeInfo then
    begin
      if IsManaged(left.TypeInfo) then
      begin
        leftRec := left.GetReferenceToRawData;
        rightRec := right.GetReferenceToRawData;
        for field in recordType.GetFields do
        begin
          leftValue := field.GetValue(leftRec);
          rightValue := field.GetValue(rightRec);
          if not leftValue.Equals(rightValue) then
            Exit(False);
        end;
        Result := True;
      end
      else
        Result := CompareMem(left.GetReferenceToRawData, right.GetReferenceToRawData, left.DataSize)
    end
    else
      Result := False;
  end;

var
  recordType: TRttiType;
  method: TRttiMethod;
  parameters: TArray<TRttiParameter>;
begin
  if (left.TypeInfo = TypeInfo(TValue)) and (right.TypeInfo = TypeInfo(TValue)) then
    Exit(PValue(left.GetReferenceToRawData).Equals(
      PValue(right.GetReferenceToRawData)^));

  recordType := left.TypeInfo.RttiType;
  for method in recordType.GetMethods('&op_Equality') do
  begin
    parameters := method.GetParameters;
    if (Length(parameters) = 2)
      and (parameters[0].ParamType.Handle = left.TypeInfo)
      and (parameters[1].ParamType.Handle = right.TypeInfo) then
      Exit(method.Invoke(nil, [left, right]).AsBoolean);
  end;

  Result := RawEquals(recordType);
end;

function EqualsDynArray2DynArray(const left, right: TValue): Boolean;
var
  len, i: Integer;
begin
  if PPointer(left.GetReferenceToRawData)^ = PPointer(right.GetReferenceToRawData)^ then
    Exit(True);
  len := left.GetArrayLength;
  if len <> right.GetArrayLength then
    Exit(False);
  for i := 0 to len - 1 do
    if not left.GetArrayElement(i).Equals(right.GetArrayElement(i)) then
      Exit(False);
  Result := True;
end;

function EqualsSet2Set(const left, right: TValue): Boolean;
var
  size: Integer;
begin
  size := left.DataSize;
  if size <> right.DataSize then
    Exit(False);

  case size of
    1: Result := TValueData(left).FAsUByte = TValueData(right).FAsUByte;
    2: Result := TValueData(left).FAsUWord = TValueData(right).FAsUWord;
    3..4: Result := TValueData(left).FAsULong = TValueData(right).FAsULong;
    5..8: Result := TValueData(left).FAsUInt64 = TValueData(right).FAsUInt64;
  else
    Result := CompareMem(left.GetReferenceToRawData, right.GetReferenceToRawData, size);
  end;
end;

type
  TEqualsFunc = function(const left, right: TValue): Boolean;
const
  EqualsFunctions: array[TTypeKind,TTypeKind] of TEqualsFunc = (
    // tkUnknown
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkInteger
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsInt2Int, EqualsFail, EqualsFail, EqualsInt2Float,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsInt2Int64, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsStr2Var, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkEnumeration
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsInt2Int, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkFloat
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFloat2Int, EqualsFail, EqualsFail, EqualsFloat2Float,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFloat2Int64, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsStr2Var, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkSet
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsSet2Set, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkClass
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsClass2Class, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkMethod
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail, // TODO: tkMethod
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkWChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsStr2Var, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkLString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsStr2Var, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkWString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkVariant
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsVar2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsVar2Str, EqualsFail, EqualsFail, EqualsFail, EqualsVar2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsVar2Str, EqualsVar2Str, EqualsVar2Var, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsVar2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkRecord
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsRec2Rec,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkInterface
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsIntf2Intf, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkInt64
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsInt642Int, EqualsFail, EqualsFail, EqualsInt64ToFloat,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsInt642Int64, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkDynArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsDynArray2DynArray, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkUString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsStr2Var, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkClassRef
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsClassRef2ClassRef,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkPointer
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsPointer2Pointer, EqualsFail
    ),
    // tkProcedure
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    )
  );
{$ENDREGION}


function TValueHelper.Equals(const value: TValue): Boolean;
begin
  if Assigned(TypeInfo) then
    Result := EqualsFunctions[Kind, value.Kind](Self, value)
  else
    Result := value.IsEmpty;
end;

procedure TValueHelper.Free;
begin
  if IsObject then
{$IFNDEF AUTOREFCOUNT}
    AsObject.Free;
{$ELSE}
    AsObject.DisposeOf;
{$ENDIF}
end;

class function TValueHelper.From(buffer: Pointer; typeInfo: PTypeInfo): TValue;
begin
  TValue.Make(buffer, typeInfo, Result);
end;

class function TValueHelper.From(instance: TObject; classType: TClass): TValue;
begin
  TValue.Make(NativeInt(instance), classType.ClassInfo, Result);
end;

class function TValueHelper.FromFloat(typeInfo: PTypeInfo;
  value: Extended): TValue;
begin
  case typeInfo.TypeData.FloatType of
    ftSingle: Result := TValue.From<Single>(value);
    ftDouble: Result := TValue.From<Double>(value);
    ftExtended: Result := TValue.From<Extended>(value);
    ftComp: Result := TValue.From<Comp>(value);
    ftCurr: Result := TValue.From<Currency>(value);
  end;
end;

class function TValueHelper.FromVariant(const value: Variant): TValue;

  procedure FromCustomVariant(const value: Variant; out result: TValue);
  type
    PCustomVariantTypeInfo = ^TCustomVariantTypeInfo;
    TCustomVariantTypeInfo = record
      Name: string;
      VType: TVarType;
    end;
  const
    CustomVariantTypes: array[0..2] of TCustomVariantTypeInfo = (
      (Name: 'SQLTimeStampVariantType'; VType: varDouble),
      (Name: 'SQLTimeStampOffsetVariantType'; VType: varDouble),
      (Name: 'FMTBcdVariantType'; VType: varInt64)
    );
  var
    typeName: string;
    i: Integer;
    tmp: Int64;
    info: PCustomVariantTypeInfo;
  begin
    typeName := VarTypeAsText(TVarData(value).VType);
    for i := 0 to High(CustomVariantTypes) do
    begin
      info := @CustomVariantTypes[i];
      if typeName = info.Name then
      begin
        case info.VType of
          varDouble: result := Double(value);
          varInt64:
            if TryStrToInt64(VarToStr(value), tmp) then
              Result := tmp
            else
              Result := Double(value);
        else
          raise EVariantTypeCastError.CreateRes(@SInvalidVarCast);
        end;
        Exit;
      end;
    end;
    raise EVariantTypeCastError.CreateRes(@SInvalidVarCast);
  end;

var
  typeInfo: PTypeInfo;
  arr: Pointer;
begin
  case TVarData(value).VType of
    varEmpty, varNull: Exit(Empty);
    varBoolean: Result := TVarData(value).VBoolean;
    varShortInt: Result := TVarData(value).VShortInt;
    varSmallint: Result := TVarData(value).VSmallInt;
    varInteger: Result := TVarData(value).VInteger;
{$IFDEF DELPHIXE4_UP}
    varSingle: Result := TVarData(value).VSingle;
    varDouble: Result := TVarData(value).VDouble;
    varCurrency: Result := TVarData(value).VCurrency;
{$ELSE}
    varSingle: Result := TValue.From<Single>(TVarData(value).VSingle);
    varDouble: Result := TValue.From<Double>(TVarData(value).VDouble);
    varCurrency: Result := TValue.From<Currency>(TVarData(value).VCurrency);
{$ENDIF}
    varDate: Result := From<TDateTime>(TVarData(value).VDate);
    varOleStr: Result := string(TVarData(value).VOleStr);
    varDispatch: Result := From<IDispatch>(IDispatch(TVarData(value).VDispatch));
    varError: Result := From<HRESULT>(TVarData(value).VError);
    varUnknown: Result := From<IInterface>(IInterface(TVarData(value).VUnknown));
    varByte: Result := TVarData(value).VByte;
    varWord: Result := TVarData(value).VWord;
    varLongWord: Result := TVarData(value).VLongWord;
    varInt64: Result := TVarData(value).VInt64;
{$IFDEF DELPHIXE4_UP}
    varUInt64: Result := TVarData(value).VUInt64;
{$ELSE}
    varUInt64: Result := TValue.From<UInt64>(TVarData(value).VUInt64);
{$ENDIF}
{$IFNDEF NEXTGEN}
    varString: Result := string(AnsiString(TVarData(value).VString));
{$ENDIF}
    varUString: Result := UnicodeString(TVarData(value).VUString);
  else
    if TVarData(value).VType and varArray = varArray then
    begin
      case TVarData(value).VType and not varArray of
        varSmallint: typeInfo := System.TypeInfo(TArray<SmallInt>);
        varInteger: typeInfo := System.TypeInfo(TArray<Integer>);
        varSingle: typeInfo := System.TypeInfo(TArray<Single>);
        varDouble: typeInfo := System.TypeInfo(TArray<Double>);
        varCurrency: typeInfo := System.TypeInfo(TArray<Currency>);
        varDate: typeInfo := System.TypeInfo(TArray<TDateTime>);
        varOleStr: typeInfo := System.TypeInfo(TArray<string>);
        varDispatch: typeInfo := System.TypeInfo(TArray<IDispatch>);
        varError: typeInfo := System.TypeInfo(TArray<HRESULT>);
        varBoolean: typeInfo := System.TypeInfo(TArray<Boolean>);
        varVariant: typeInfo := System.TypeInfo(TArray<Variant>);
        varUnknown: typeInfo := System.TypeInfo(TArray<IInterface>);
        varShortInt: typeInfo := System.TypeInfo(TArray<ShortInt>);
        varByte: typeInfo := System.TypeInfo(TArray<Byte>);
        varWord: typeInfo := System.TypeInfo(TArray<Word>);
        varLongWord: typeInfo := System.TypeInfo(TArray<Cardinal>);
        varInt64: typeInfo := System.TypeInfo(TArray<Int64>);
        varUInt64: typeInfo := System.TypeInfo(TArray<UInt64>);
        varUString:  typeInfo := System.TypeInfo(TArray<string>);
      else
        raise EVariantTypeCastError.CreateRes(@SInvalidVarCast);
      end;
      arr := nil;
      DynArrayFromVariant(arr, value, typeInfo);
      TValue.MakeWithoutCopy(@arr, typeInfo, Result);
    end
    else
      FromCustomVariant(value, Result);
  end;
end;

class function TValueHelper.FromVarRec(const value: TVarRec): TValue;
begin
  case value.VType of
    vtInteger: Result := value.VInteger;
    vtBoolean: Result := value.VBoolean;
{$IF Declared(AnsiChar)}
    vtChar: Result := string(value.VChar);
{$IFEND}
    vtExtended: Result := value.VExtended^;
{$IF Declared(ShortString)}
    vtString: Result := string(value.VString^);
{$IFEND}
    vtPointer: Result := value.VPointer;
{$IF Declared(PAnsiChar)}
    vtPChar: Result := string(value.VPChar);
{$IFEND}
    vtObject: Result := TObject(value.VObject);
    vtClass: Result := value.VClass;
    vtWideChar: Result := value.VWideChar;
    vtPWideChar: Result := string(value.VPWideChar);
{$IF Declared(AnsiString)}
    vtAnsiString: Result := string(value.VAnsiString);
{$IFEND}
    vtCurrency: Result := value.VCurrency^;
    vtVariant: Result := TValue.FromVariant(value.VVariant^);
    vtInterface: Result := TValue.From<IInterface>(IInterface(value.VInterface));
{$IF Declared(WideString)}
    vtWideString: Result := string(value.VWideString);
{$IFEND}
    vtInt64: Result := value.VInt64^;
    vtUnicodeString: Result := string(value.VUnicodeString);
  end;
end;

function TValueHelper.GetArray: TArray<TValue>;
var
  len: Integer;
  i: Integer;
begin
  len := GetArrayLength;
  SetLength(Result, len);
  for i := 0 to len - 1 do
    Result[i] := GetArrayElement(i);
end;

function TValueHelper.GetNullableValue: TValue;
var
  nullable: TNullableHelper;
  instance: Pointer;
begin
  if not IsNullable(TypeInfo) then
    raise EInvalidOperationException.CreateRes(@SValueDoesNotContainNullable);

  instance := GetReferenceToRawData;
  if instance = nil then
    Exit(TValue.Empty);
  nullable := TNullableHelper.Create(TypeInfo);
  if nullable.HasValue(instance) then
    Result := nullable.GetValue(instance)
  else
    Result := TValue.Empty;
end;

{$IFNDEF DELPHIXE8_UP}
function TValueHelper.GetTypeKind: TTypeKind;
begin
{$IFDEF DELPHI2010}
  if (TValueData(Self).FTypeInfo = nil) or (TValueData(Self).FHeapData = nil) then
{$ELSE}
  if (TValueData(Self).FTypeInfo = nil) or (TValueData(Self).FValueData = nil) then
{$ENDIF}
    Result := tkUnknown
  else
    Result := TValueData(Self).FTypeInfo.Kind;
end;
{$ENDIF}

function TValueHelper.GetValueType: TRttiType;
begin
  Result := TypeInfo.RttiType;
end;

function TValueHelper.IsFloat: Boolean;
begin
  Result := Kind in [tkInteger, tkFloat, tkInt64];
end;

function TValueHelper.IsInstance: Boolean;
begin
  Result := Kind in [tkClass, tkInterface];
end;

function TValueHelper.IsInterface: Boolean;
begin
  Result := Kind = tkInterface;
end;

function TValueHelper.IsNumeric: Boolean;
const
  NumericKinds = [tkInteger, tkChar, tkEnumeration, tkFloat, tkWChar, tkInt64];
begin
  Result := Kind in NumericKinds;
end;

function TValueHelper.IsString: Boolean;
const
  StringKinds = [tkString, tkLString, tkWString, tkUString, tkChar, tkWChar];
begin
  Result := Kind in StringKinds;
end;

{$IFDEF DELPHI2010}
function TValueHelper.IsType(ATypeInfo: PTypeInfo): Boolean;
var
  unused: TValue;
begin
  Result := IsEmpty or TryCast(ATypeInfo, unused);
end;

function TValueHelper.IsType<T>: Boolean;
begin
  Result := IsType(System.TypeInfo(T));
end;
{$ENDIF}

function TValueHelper.IsVariant: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Variant);
end;

class function TValueHelper.&&op_Equality(const left, right: TValue): Boolean;
begin
  Result := left.Equals(right);
end;

{$IFNDEF DELPHIXE4_UP}
class function TValueHelper.&&op_Implicit(value: Double): TValue;
begin
  Result.Init(System.TypeInfo(Double));
  TValueData(Result).FAsDouble := value;
end;

class function TValueHelper.&&op_Implicit(value: Single): TValue;
begin
  Result.Init(System.TypeInfo(Single));
  TValueData(Result).FAsSingle := value;
end;

class function TValueHelper.&&op_Implicit(value: UInt64): TValue;
begin
  Result.Init(System.TypeInfo(UInt64));
  TValueData(Result).FAsUInt64 := value;
end;

class function TValueHelper.&&op_Implicit(value: Currency): TValue;
begin
  Result.Init(System.TypeInfo(Currency));
  TValueData(Result).FAsCurr := value;
end;
{$ENDIF}

{$IFNDEF DELPHIXE8_UP}
class function TValueHelper.&&op_Implicit(const value: TVarRec): TValue;
begin
  Result := TValue.FromVarRec(value);
end;
{$ENDIF}

class function TValueHelper.&&op_Implicit(value: TDate): TValue;
begin
  Result.Init(System.TypeInfo(TDate));
  TValueData(Result).FAsDouble := value;
end;

class function TValueHelper.&&op_Implicit(value: TTime): TValue;
begin
  Result.Init(System.TypeInfo(TTime));
  TValueData(Result).FAsDouble := value;
end;

class function TValueHelper.&&op_Implicit(value: TDateTime): TValue;
begin
  Result.Init(System.TypeInfo(TDateTime));
  TValueData(Result).FAsDouble := value;
end;

class function TValueHelper.&&op_Inequality(const left, right: TValue): Boolean;
begin
  Result := not left.Equals(right);
end;

class procedure TValueHelper.RaiseConversionError(source, target: PTypeInfo);
var
  sourceTypeName: string;
begin
  if Assigned(source) then
    sourceTypeName := source.TypeName
  else
    sourceTypeName := '<unknown>';
  raise EConvertError.CreateResFmt(@STypeConversionError, [
    sourceTypeName, target.TypeName]) at ReturnAddress;
end;

procedure TValueHelper.SetNullableValue(const value: TValue);
var
  typeInfo: PTypeInfo;
  nullable: TNullableHelper;
  instance: Pointer;
begin
  typeInfo := TValueData(Self).FTypeInfo;
  if IsNullable(typeInfo) then
  begin
    instance := GetReferenceToRawData;
    nullable := TNullableHelper.Create(typeInfo);
    nullable.SetValue(instance, value);
  end;
end;

function TValueHelper.ToObject: TObject;
begin
  if IsInterface then
    Result := AsInterface as TObject
  else
    Result := AsObject;
end;

type
  TValueHack = type TValue; // make an alias to access "inherited" ToString

function TValueHelper.ToString: string;
var
  value: TValue;
begin
  if IsNullable(TypeInfo) then
    if TryGetNullableValue(value) then
      Result := value.ToString
    else
      Result := '(null)'
  else
    Result := TValueHack(Self).ToString;
end;

function TValueHelper.ToType<T>: T;
begin
  if not TryToType<T>(Result) then
    RaiseConversionError(TypeInfo, System.TypeInfo(T));
end;

function TValueHelper.ToType<T>(const formatSettings: TFormatSettings): T;
begin
  if not TryToType<T>(Result, formatSettings) then
    RaiseConversionError(TypeInfo, System.TypeInfo(T));
end;

function TValueHelper.ToVariant: Variant;
var
  value: TValue;
  obj: TObject;
  stream: TStream;
  persist: IStreamPersist;

{$IFNDEF DELPHI2010}
  function TryConvertToVariant(out returnValue: Variant): Boolean;
  begin
    Result := TValueConverter.Default.TryConvertTo(Self, System.TypeInfo(Variant), value);
    if Result then
      returnValue := value.AsVariant
    else
      returnValue := Null;
  end;
{$ENDIF}

begin
  Result := Null;
  case Kind of
    tkEnumeration:
      if IsType<Boolean> then
        Exit(AsBoolean)
      else
        Exit(AsOrdinal);
    tkFloat:
      if (TypeInfo = System.TypeInfo(TDateTime))
        or (TypeInfo = System.TypeInfo(TDate))
        or (TypeInfo = System.TypeInfo(TTime)) then
        Exit(AsType<TDateTime>)
      else if TypeInfo = System.TypeInfo(Currency) then
        Exit(AsCurrency)
      else
        Exit(AsExtended);
    tkRecord:
    begin
      if IsNullable(TypeInfo) then
        if TryGetNullableValue(value) then
          Exit(value.ToVariant);

      if IsLazyType(TypeInfo) then
        if TryGetLazyValue(value) then
          Exit(value.ToVariant);

      if TypeInfo = System.TypeInfo(TGUID) then
        Exit(AsType<TGUID>.ToString);
    end;
    tkClass:
    begin
    {$IFNDEF DELPHI2010}
      if TryConvertToVariant(Result) then
        Exit;
    {$ENDIF}

      obj := AsObject;
      if obj is TStream then
      begin
        stream := TStream(obj);
        stream.Position := 0;
        Exit(StreamToVariant(stream));
      end
      else if Supports(obj, IStreamPersist, persist) then
      begin
        stream := TMemoryStream.Create;
        try
          persist.SaveToStream(stream);
          stream.Position := 0;
          Exit(StreamToVariant(stream));
        finally
          stream.Free;
        end;
      end;
    end;
    tkInterface:
      Exit(AsInterface);
  else
    Exit(AsVariant);
  end;
{$IFNDEF DELPHI2010}
  TryConvertToVariant(Result);
{$ENDIF}
end;

function TValueHelper.TryAsInterface(typeInfo: PTypeInfo; out Intf): Boolean;
var
  typeData: PTypeData;
  obj: TObject;
begin
  if not (Kind in [tkClass, tkInterface]) then
    Exit(False);
  if typeInfo.Kind <> tkInterface then
    Exit(False);
  if Self.TypeInfo = typeInfo then
    Result := True
  else
  begin
    typeData := typeInfo.TypeData;
    if Kind = tkClass then
    begin
{$IFDEF AUTOREFCOUNT}
      TValueData(Self).FValueData.ExtractRawData(@obj);
{$ELSE}
      obj := TObject(TValueData(Self).FAsObject);
{$ENDIF}
      Exit(obj.GetInterface(typeData.Guid, Intf));
    end;
    Result := False;
    typeData := Self.TypeData;
    while Assigned(typeData) and Assigned(typeData.IntfParent) do
    begin
      if typeData.IntfParent^ = typeInfo then
      begin
        Result := True;
        Break;
      end;
      typeData := typeData.IntfParent^.TypeData;
    end;
  end;
  if Result then
    IInterface(Intf) := AsInterface;
end;


{$REGION 'Conversion functions'}
type
  TConvertFunc = function(const source: TValue; target: PTypeInfo;
    out value: TValue; const formatSettings: TFormatSettings): Boolean;

function ConvFail(const source: TValue; target: PTypeInfo; out value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
begin
  Result := False;
end;

function ConvClass2Class(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
begin
  Result := source.TryCast(target, value);
end;

function ConvClass2Enum(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
begin
  Result := target = TypeInfo(Boolean);
  if Result then
    value := source.AsObject <> nil;
end;

function ConvFloat2Ord(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
begin
  Result := Frac(source.AsExtended) = 0;
  if Result then
    value := TValue.FromOrdinal(target, Trunc(source.AsExtended));
end;

function ConvFloat2Str(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
var
  temp: TValue;
begin
  if source.TypeInfo = TypeInfo(TDate) then
    temp := DateToStr(source.AsExtended, formatSettings)
  else if source.TypeInfo = TypeInfo(TDateTime) then
    temp := DateTimeToStr(source.AsExtended, formatSettings)
  else if source.TypeInfo = TypeInfo(TTime) then
    temp := TimeToStr(source.AsExtended, formatSettings)
  else
    temp := FloatToStr(source.AsExtended, formatSettings);
  Result := temp.TryCast(target, value);
end;

function ConvIntf2Class(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
begin
  Result := ConvClass2Class(source.AsInterface as TObject, target, value, formatSettings);
end;

function ConvIntf2Intf(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
var
  intf: IInterface;
begin
  Result := source.TryAsInterface(target, intf);
  if Result then
    TValue.Make(@intf, target, value)
  else
    value := TValue.Empty;
end;

function ConvOrd2Float(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
begin
  value := TValue.FromFloat(target, source.AsOrdinal);
  Result := True;
end;

function ConvOrd2Ord(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
var
  i: Int64;
begin
  i := source.AsOrdinal;
  with target.TypeData^ do
    if (i < MinValue) or (i > MaxValue) then
      Exit(False);
  value := TValue.FromOrdinal(target, i);
  Result := True;
end;

function ConvOrd2Str(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
var
  temp: TValue;
begin
  temp := source.ToString;
  Result := temp.TryCast(target, value);
end;

function ConvRec2Meth(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
begin
  Result := source.TypeInfo = TypeInfo(TMethod);
  if Result then
  begin
    value := TValue.From(source.GetReferenceToRawData, target);
    Result := True;
  end
end;

function ConvStr2Enum(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
var
  temp: Integer;
begin
  temp := GetEnumValue(target, source.AsString);
  Result := (temp >= 0) or (target.TypeData.MinValue < 0);
  if Result then
    value := TValue.FromOrdinal(target, temp);
end;

function ConvStr2Float(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
var
  s: string;
  d: TDateTime;
  f: Extended;
begin
  s := source.AsString;
  if target = TypeInfo(TDateTime) then
  begin
    Result := TryStrToDateTime(s, d, formatSettings);
    if Result then
      value := TValue.From<TDateTime>(d);
  end else
  if target = TypeInfo(TDate) then
  begin
    Result := TryStrToDate(s, d, formatSettings);
    if Result then
      value := TValue.From<TDate>(d);
  end else
  if target = TypeInfo(TTime) then
  begin
    Result := TryStrToTime(s, d, formatSettings);
    if Result then
      value := TValue.From<TTime>(d);
  end else
  begin
    Result := TryStrToFloat(s, f, formatSettings);
    if Result then
      value := TValue.FromFloat(target, f);
  end;
end;

function ConvStr2Ord(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
var
  i: Int64;
begin
  Result := TryStrToInt64(source.AsString, i);
  if Result then
    value := TValue.FromOrdinal(target, i);
end;

function ConvStr2DynArray(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
var
  s: string;
  values: TStringDynArray;
  i: Integer;
  p: Pointer;
  res, v1, v2: TValue;
  elType: PTypeInfo;
begin
  s := source.AsString;
  if StartsStr('[', s) and EndsStr(']', s) then
    s := Copy(s, 2, Length(s) - 2);
  values := SplitString(s, ',');
  i := Length(values);
  p := nil;
  DynArraySetLength(p, target, 1, @i);
  TValue.MakeWithoutCopy(@p, target, res);
  elType := target.TypeData.DynArrElType^;
  for i := 0 to High(values) do
  begin
    v1 := TValue.From(values[i]);
    if not v1.TryConvert(elType, v2) then
      Exit(False);
    res.SetArrayElement(i, v2);
  end;
  value := res;
  Result := True;
end;

function ConvStr2Array(const source: TValue; target: PTypeInfo;
  out value: TValue; const formatSettings: TFormatSettings): Boolean;
var
  s: string;
  values: TStringDynArray;
  arrData: TArrayTypeData;
  elType: PTypeInfo;
  i: Integer;
  res, v1, v2: TValue;
begin
  s := source.AsString;
  if StartsStr('[', s) and EndsStr(']', s) then
    s := Copy(s, 2, Length(s) - 2);
  values := SplitString(s, ',');

  // todo: support multi dim arrays - assume one dim for now
  arrData := GetTypeData(target).ArrayData;
  elType := arrData.ElType^;
  if Length(values) <> arrData.ElCount then
    Exit(False);

  TValue.Make(nil, target, res);
  for i := 0 to arrData.ElCount - 1 do
  begin
    v1 := TValue.From(values[i]);
    if not v1.TryConvert(elType, v2) then
      Exit(False);
    res.SetArrayElement(i, v2);
  end;
  value := res;
  Result := True;
end;

{$ENDREGION}


{$REGION 'Conversions'}
const
  Conversions: array[TTypeKind, TTypeKind] of TConvertFunc = (
    // tkUnknown
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkInteger
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvOrd2Str,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvOrd2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkEnumeration
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvOrd2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkFloat
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFloat2Ord, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFloat2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFloat2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkSet
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkClass
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvClass2Enum, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvClass2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkMethod
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkWChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkLString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkWString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkVariant
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkRecord
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvRec2Meth, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkInterface
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvIntf2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvIntf2Intf, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkInt64
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvOrd2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkDynArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkUString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvStr2Ord, ConvFail, ConvStr2Enum, ConvStr2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvStr2Array, ConvFail, ConvFail, ConvStr2Ord, ConvStr2DynArray,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkClassRef
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkPointer
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkProcedure
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    )
  );
{$ENDREGION}


function TValueHelper.TryConvert(targetType: PTypeInfo;
  out targetValue: TValue): Boolean;
var
  formatSettings: TFormatSettings;
begin
  formatSettings := TFormatSettings.Create;
  Result := TryConvert(targetType, targetValue, formatSettings);
end;

function TValueHelper.TryConvert(targetType: PTypeInfo;
  out targetValue: TValue; const formatSettings: TFormatSettings): Boolean;
var
  value: TValue;
begin
  {$IFDEF DELPHI2010}
  // Fix for TValue.Cast not converting TValue.Empty to any type
  if (TypeInfo = nil) and (targetType <> nil) then
  begin
    TValue.Make(nil, targetType, targetValue);
    Exit(True);
  end;
  {$ENDIF}

  if (TypeInfo = nil) or (targetType = nil) then
  begin
    targetValue := EmptyValue;
    Exit(True);
  end;

  if TypeInfo = targetType then
  begin
    targetValue := Self;
    Exit(True);
  end;

  Result := Conversions[Kind, targetType.Kind](Self, targetType, targetValue, formatSettings);
  if not Result then
  begin
    if TryGetNullableValue(value) and value.TryCast(targetType, targetValue) then
      Exit(True);

    if TryGetLazyValue(value) and value.TryCast(targetType, targetValue) then
      Exit(True);

    if IsNullable(targetType) and TryConvert(GetUnderlyingType(targetType), value) then
    begin
      TValue.Make(nil, targetType, targetValue);
      targetValue.SetNullableValue(value);
      Exit(True);
    end;

    case Kind of
      tkRecord:
        if TypeInfo = System.TypeInfo(TValue) then
          Exit(AsType<TValue>.TryConvert(targetType, targetValue));
      {$IFDEF DELPHI2010}
      // workaround for bug in RTTI.pas (fixed in XE)
      tkUnknown:
      begin
        case targetType.Kind of
          tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64:
          begin
            targetValue := TValue.FromOrdinal(targetType, 0);
            Exit(True);
          end;
          tkFloat:
          begin
            targetValue := TValue.From<Extended>(0);
            Exit(True);
          end;
          tkUString:
          begin
            targetValue := '';
            Exit(True);
          end;
        end;
      end;
      {$ENDIF}
    end;

    {$IFNDEF DELPHI2010}
    Result := TValueConverter.Default.TryConvertTo(Self, targetType, targetValue, TValue.From(formatSettings));
    {$ELSE}
    Result := False;
    {$ENDIf}
  end;
end;

function TValueHelper.TryGetLazyValue(out value: TValue): Boolean;
var
  instance: PInterface;
  lazy: ILazy;
begin
  case GetLazyKind(TValueData(Self).FTypeInfo) of
    lkRecord:
    begin
      instance := GetReferenceToRawData;
      if instance = nil then
        Exit(False);
      lazy := instance^ as ILazy;
      Result := Assigned(lazy);
      if Result then
        value := lazy.Value;
    end;
  else
    Result := False;
  end;
end;

function TValueHelper.TryGetNullableValue(out value: TValue): Boolean;
var
  typeInfo: PTypeInfo;
  nullable: TNullableHelper;
  instance: Pointer;
begin
  typeInfo := TValueData(Self).FTypeInfo;
  Result := IsNullable(typeInfo);
  if Result then
  begin
    instance := GetReferenceToRawData;
    if instance = nil then
      Exit(False);
    nullable := TNullableHelper.Create(typeInfo);
    Result := nullable.HasValue(instance);
    if Result then
      value := nullable.GetValue(instance);
  end;
end;

function TValueHelper.TryToType<T>(out targetValue: T): Boolean;
var
  value: TValue;
begin
  Result := TryConvert(System.TypeInfo(T), value);
  if Result then
  begin
    // avoid extra overhead of value.AsType<T>
    // since we know value contains the exact type of T
    // use the same code as the private TValue.Get<T> method
    if TValueData(value).FTypeInfo = nil then
    begin
      FillChar(Pointer(@targetValue)^, SizeOf(T), 0);
      Exit;
    end;
    value.ExtractRawData(@targetValue);
  end;
end;

function TValueHelper.TryToType<T>(out targetValue: T;
  const formatSettings: TFormatSettings): Boolean;
var
  value: TValue;
begin
  Result := TryConvert(System.TypeInfo(T), value, formatSettings);
  if Result then
  begin
    // avoid extra overhead of value.AsType<T>
    // since we know value contains the exact type of T
    // use the same code as the private TValue.Get<T> method
    if TValueData(value).FTypeInfo = nil then
    begin
      FillChar(Pointer(@targetValue)^, SizeOf(T), 0);
      Exit;
    end;
    value.ExtractRawData(@targetValue);
  end;
end;

{$ENDREGION}


{$REGION 'TRttiMethodHelper'}

function TRttiMethodHelper.GetIsAbstract: Boolean;
var
  code: Pointer;
begin
  case DispatchKind of
    dkVtable: code := GetVirtualMethod(Parent.AsInstance.MetaclassType, VirtualIndex);
    dkDynamic: code := GetDynaMethod(Parent.AsInstance.MetaclassType, VirtualIndex);
  else
    code := nil;
  end;
  Result := code = GetAbstractError;
end;

function TRttiMethodHelper.GetReturnTypeHandle: PTypeInfo;
var
  returnType: TRttiType;
begin
  returnType := Self.ReturnType;
  if Assigned(returnType) then
    Result := returnType.Handle
  else
    Result := nil;
end;

{$IF CompilerVersion < 31}
procedure TRttiMethodHelper.DispatchValue(const value: TValue;
  typeInfo: PTypeInfo);
type
  PValueData = ^TValueData;
begin
  if (value.TypeInfo <> typeInfo) and (value.Kind = tkInterface)
    and (typeInfo.Kind = tkInterface)
    and IsAssignableFrom(typeInfo, value.TypeInfo) then
    PValueData(@value).FTypeInfo := typeInfo;
end;

type
  TRttiObjectHelper = class helper for TRttiObject
  private
    procedure SetParent(const parent: TRttiObject); inline;
  end;

procedure TRttiObjectHelper.SetParent(const parent: TRttiObject);
begin
  Self.FParent := parent;
end;

procedure TRttiMethodHelper.FixParameters(
  const parameters: TArray<TRttiParameter>);
var
  i: Integer;
begin
  for i := 0 to High(parameters) do
    parameters[i].SetParent(Self);
end;

function TRttiMethodHack.GetParameters: TArray<TRttiParameter>; //FI:W521
begin //FI:W519
end;

function TRttiMethodHelper.GetParameters: TArray<TRttiParameter>;
begin
  Result := TRttiMethodHack(Self).GetParameters;
  FixParameters(Result);
end;

function TRttiMethodHelper.Invoke(Instance: TObject;
  const Args: array of TValue): TValue;
begin
  Result := Invoke(TValue(Instance), Args);
end;

function TRttiMethodHelper.Invoke(Instance: TClass;
  const Args: array of TValue): TValue;
begin
  Result := Invoke(TValue(Instance), Args);
end;

function TRttiMethodHelper.Invoke(Instance: TValue;
  const Args: array of TValue): TValue;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := GetParameters;
  if Length(Args) <> Length(parameters) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);
  for i := Low(Args) to High(Args) do
    DispatchValue(Args[i], parameters[i].ParamType.Handle);
  if MethodKind = mkOperatorOverload then
    Result := Rtti.Invoke(CodeAddress, TArray.Copy<TValue>(Args),
      CallingConvention, ReturnTypeHandle{$IFDEF DELPHIXE2_UP}, IsStatic{$ENDIF})
  else
    Result := Self.DispatchInvoke(Instance, Args);
end;
{$IFEND}

{$ENDREGION}


{$REGION 'TRttiInvokableTypeHelper'}

{$IFDEF DELPHIXE2_UP}
type
  // this is the class used to create a TMethodImplementation for a
  // TRttiInvokableType by passing in an instance of TRttiInvokableType
  // and "overriding" its private virtual methods
  TRttiInvokableMethod = class(TRttiMethod)
  private
    FType: TRttiInvokableType;
    constructor Create(AType: TRttiInvokableType);
  end;

  // this classes is needed to access FParent
  // it needs to have the exact same fields as System.Rtti.TRttiObject
  TRttiObjectHack = class abstract
  protected
    FHandle: Pointer;
    FRttiDataSize: Integer;
    {$IFDEF WEAKINSTREF}[Weak]{$ENDIF}
    FPackage: TRttiPackage;
    {$IFDEF WEAKINSTREF}[Weak]{$ENDIF}
    FParent: TRttiObject;
  end;

  // this class is needed to "override" private virtual methods
  // it needs to have the exact same virtual methods as System.Rtti.TRttiMethod
  TRttiInvokableMethodHack = class(TRttiMember)
  protected
    FInvokeInfo: TObject; //TMethodImplementation.TInvokeInfo
    FType: TRttiInvokableType;
    function GetMethodKind: TMethodKind; virtual; abstract;
    function GetCallingConvention: TCallConv; virtual;
    function GetReturnType: TRttiType; virtual;
    function GetDispatchKind: TDispatchKind; virtual; abstract;
    function GetHasExtendedInfo: Boolean; virtual; abstract;
    function GetVirtualIndex: SmallInt; virtual; abstract;
    function GetCodeAddress: Pointer; virtual; abstract;
    function GetIsClassMethod: Boolean; virtual;
    function GetIsStatic: Boolean; virtual;
    function DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue; virtual; abstract;
  public
    function GetParameters: TArray<TRttiParameter>; virtual;
  end;

  // this class is needed to "override" the destructor of
  // the TMethodImplementation instances that are created inside of
  // TRttiMethod.CreateImplementation
  TMethodImplementationHack = class(TMethodImplementation)
  {$IFDEF DELPHIXE2}
  private
    function FInvokeInfo: TObject; inline;
  {$ENDIF}
  public
    destructor Destroy; override;
  end;

function TRttiInvokableMethodHack.GetCallingConvention: TCallConv;
begin
  Result := FType.CallingConvention;
end;

function TRttiInvokableMethodHack.GetIsClassMethod: Boolean;
begin
  Result := False;
end;

function TRttiInvokableMethodHack.GetIsStatic: Boolean;
begin
  Result := FType is TRttiProcedureType;
end;

function TRttiInvokableMethodHack.GetParameters: TArray<TRttiParameter>;
begin
  Result := FType.GetParameters;
end;

function TRttiInvokableMethodHack.GetReturnType: TRttiType;
begin
  Result := FType.ReturnType;
end;

{$IFDEF DELPHIXE2}
function TMethodImplementationHack.FInvokeInfo: TObject;
begin
  Result := PPointer(PByte(Self) + SizeOf(Pointer) * 2)^;
end;
{$ENDIF}

destructor TMethodImplementationHack.Destroy;
begin
  if FInvokeInfo <> nil then
    FInvokeInfo.Free;
  inherited Destroy;
end;

constructor TRttiInvokableMethod.Create(AType: TRttiInvokableType);
var
  ctx: TRttiContext;
begin
  inherited Create;
  // GetInvokeInfo need the Parent property
  TRttiObjectHack(Self).FParent := ctx.GetType(TObject);
  FType := AType;
  // change the type of this class to the class that has its private
  // methods "overridden"
  PPointer(Self)^ := TRttiInvokableMethodHack;
end;

function TRttiInvokableTypeHelper.CreateImplementation(AUserData: Pointer; //FI:O804
  const ACallback: TMethodImplementationCallback): TMethodImplementation;
var
  m: TRttiMethod;
begin
  {$WARN CONSTRUCTING_ABSTRACT OFF}
  m := TRttiInvokableMethod.Create(Self);
  try
    // there is no way to directly create a TMethodImplementation instance
    // because it requires an instance of the private TInvokeInfo class to be
    // passed which can only be produced by the private method GetInvokeInfo

    // since TRttiInvokableMethod has the necessary private virtual methods
    // "overridden" it will create the correct TMethodImplementation instance
    // for the given TRttiInvokableType
    Result := m.CreateImplementation(Self, ACallback);
    // "override" the destructor so FInvokeMethod which is not owned by the
    // TRttiInvokableMethod is properly destroyed at the end
    PPointer(Result)^ := TMethodImplementationHack;
  finally
    m.Free;
  end;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TMethodImplementationHelper'}

{$IFNDEF DELPHI2010}
function TMethodImplementationHelper.AsMethod: TMethod;
begin
  Result.Code := CodeAddress;
  Result.Data := Self;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TNamedValue'}

constructor TNamedValue.Create(const value: TValue; const name: string);
begin
  fValue := value;
  fName := name;
end;

class function TNamedValue.From<T>(const value: T;
  const name: string): TNamedValue;
begin
  Result.fValue := TValue.From<T>(value);
  Result.fName := name;
end;

class operator TNamedValue.Implicit(const value: TNamedValue): TValue;
begin
  Result := TValue.From(value);
end;

class operator TNamedValue.Implicit(const value: TValue): TNamedValue;
begin
  Result := value.AsType<TNamedValue>;
end;

{$ENDREGION}


{$REGION 'TTypedValue'}

constructor TTypedValue.Create(const value: TValue; const typeInfo: PTypeInfo);
begin
  fValue := value;
  fTypeInfo := typeInfo;
end;

class function TTypedValue.From<T>(const value: T): TTypedValue;
begin
  Result.fValue := TValue.From<T>(value);
  Result.fTypeInfo := System.TypeInfo(T);
end;

class function TTypedValue.From<T>(const value: T;
  const typeInfo: PTypeInfo): TTypedValue;
begin
  Result.fValue := TValue.From<T>(value);
  Result.fTypeInfo := typeInfo;
end;

class operator TTypedValue.Implicit(const value: TTypedValue): TValue;
begin
  Result := TValue.From(value);
end;

class operator TTypedValue.Implicit(const value: TValue): TTypedValue;
begin
  Result := value.AsType<TTypedValue>;
end;

{$ENDREGION}


{$REGION 'TInterfaceBase'}

function TInterfaceBase.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TInterfaceBase._AddRef: Integer;
begin
  Result := -1;
end;

function TInterfaceBase._Release: Integer;
begin
  Result := -1;
end;

{$ENDREGION}


{$REGION 'TInterfacedObjectEx'}

{$IF not defined(DELPHIXE7_UP) and not defined(AUTOREFCOUNT)}
procedure TInterfacedObjectEx.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FRefCount := objDestroyingFlag;
end;

function TInterfacedObjectEx.GetRefCount: Integer;
begin
  Result := FRefCount and not objDestroyingFlag;
end;
{$IFEND}

{$ENDREGION}


{$REGION 'Guard'}

class procedure Guard.RaiseArgumentException(const msg: string);
begin
  raise EArgumentException.Create(msg) at ReturnAddress;
end;

class procedure Guard.RaiseArgumentNullException(const argumentName: string);
begin
  raise EArgumentNullException.CreateResFmt(
    @SArgumentNullException, [argumentName]) at ReturnAddress;
end;

class procedure Guard.RaiseArgumentOutOfRangeException(const argumentName: string);
begin
  raise EArgumentOutOfRangeException.CreateResFmt(
    @SArgumentOutOfRangeException, [argumentName]) at ReturnAddress;
end;

class procedure Guard.RaiseArgumentException(typeKind: TTypeKind; const argumentName: string);
begin
  raise EArgumentException.CreateResFmt(@SUnexpectedTypeKindArgument,
    [GetEnumName(TypeInfo(TTypeKind), Ord(typeKind)), argumentName]) at ReturnAddress;
end;

class procedure Guard.RaiseArgumentFormatException(const argumentName: string);
begin
  raise EFormatException.CreateResFmt(
    @SInvalidArgumentFormat, [argumentName]) at ReturnAddress;
end;

class procedure Guard.RaiseInvalidEnumArgumentException(const argumentName: string);
begin
  raise EInvalidEnumArgumentException.CreateResFmt(
    @SInvalidEnumArgument, [argumentName]) at ReturnAddress;
end;

class procedure Guard.RaiseInvalidTypeCast(sourceType, targetType: PTypeInfo);
begin
  raise EInvalidCastException.CreateResFmt(@SInvalidTypeCast, [
    sourceType.TypeName, targetType.TypeName]) at ReturnAddress;
end;

class procedure Guard.RaiseNullableHasNoValue;
begin
  raise EInvalidOperationException.CreateRes(@SNullableHasNoValue) at ReturnAddress;
end;

class procedure Guard.RaiseNoDelegateAssigned;
begin
  raise EInvalidOperationException.CreateRes(@SNoDelegateAssigned) at ReturnAddress;
end;

class procedure Guard.CheckIndex(length, index, indexBase: Integer);
const
  IndexArgName = 'index';
begin
  if (index < indexBase) or (index >= indexBase + length) then
    Guard.RaiseArgumentOutOfRangeException(IndexArgName);
end;

class procedure Guard.CheckRange(length, index, count, indexBase: Integer);
const
  CountArgName = 'count';
begin
  Guard.CheckIndex(length, index, indexBase);
  if (count < 0) or (index + count > indexBase + length) then
    Guard.RaiseArgumentOutOfRangeException(CountArgName);
end;

class procedure Guard.CheckRange<T>(const buffer: array of T; index: Integer);
begin
  Guard.CheckIndex(Length(buffer), index);
end;

class procedure Guard.CheckRange<T>(const buffer: array of T;
  index, count: Integer);
begin
  Guard.CheckRange(Length(buffer), index, count);
end;

class procedure Guard.CheckTrue(condition: Boolean; const msg: string);
begin
  if not condition then
    Guard.RaiseArgumentException(msg);
end;

class procedure Guard.CheckTypeKind(typeKind: TTypeKind;
  expectedTypeKind: TTypeKind; const argumentName: string);
begin
  if typeKind <> expectedTypeKind then
    Guard.RaiseArgumentException(typeKind, argumentName);
end;

class procedure Guard.CheckTypeKind(typeKind: TTypeKind;
  expectedTypeKinds: TTypeKinds; const argumentName: string);
begin
  if not (typeKind in expectedTypeKinds) then
    RaiseArgumentException(typeKind, argumentName);
end;

class procedure Guard.CheckTypeKind<T>(expectedTypeKind: TTypeKind;
  const argumentName: string);
begin
  if TType.Kind<T> <> expectedTypeKind then
    RaiseArgumentException(TType.Kind<T>, argumentName);
end;

class procedure Guard.CheckTypeKind<T>(expectedTypeKinds: TTypeKinds;
  const argumentName: string);
begin
  if not (TType.Kind<T> in expectedTypeKinds) then
    RaiseArgumentException(TType.Kind<T>, argumentName);
end;

class procedure Guard.CheckFalse(condition: Boolean; const msg: string);
begin
  if condition then
    Guard.RaiseArgumentException(msg);
end;

class procedure Guard.CheckInheritsFrom(cls, parentClass: TClass;
  const argumentName: string);
begin
  Guard.CheckNotNull(cls, 'cls');
  Guard.CheckNotNull(parentClass, 'parentClass');

  if not cls.InheritsFrom(parentClass) then
    raise EArgumentException.CreateResFmt(@SBadObjectInheritance, [argumentName,
      cls.ClassName, parentClass.ClassName]);
end;

class procedure Guard.CheckInheritsFrom(const obj: TObject; parentClass: TClass;
  const argumentName: string);
begin
  if Assigned(obj) then
    Guard.CheckInheritsFrom(obj.ClassType, parentClass, argumentName);
end;

class procedure Guard.CheckNotNull(condition: Boolean;
  const parameterName: string);
begin
  if not condition then
    Guard.RaiseArgumentNullException(parameterName);
end;

class procedure Guard.CheckNotNull(argumentValue: Pointer;
  const argumentName: string);
begin
  Guard.CheckNotNull(Assigned(argumentValue), argumentName);
end;

class procedure Guard.CheckNotNull(const argumentValue: IInterface;
  const argumentName: string);
begin
  Guard.CheckNotNull(Assigned(argumentValue), argumentName);
end;

class procedure Guard.CheckNotNull(const argumentValue: TObject;
  const argumentName: string);
begin
  Guard.CheckNotNull(Assigned(argumentValue), argumentName);
end;

class procedure Guard.CheckNotNull<T>(const argumentValue: T;
  const argumentName: string);
begin
  if Guard.IsNullReference(argumentValue, TypeInfo(T)) then
    Guard.RaiseArgumentNullException(argumentName);
end;

class procedure Guard.CheckEnum<T>(const argumentValue: T;
  const argumentName: string);
var
  intValue: Integer;
begin
  intValue := 0;
  Move(argumentValue, intValue, SizeOf(T));
  Guard.CheckEnum<T>(intValue, argumentName);
end;

class procedure Guard.CheckEnum<T>(argumentValue: Integer;
  const argumentName: string);
var
  typeInfo: PTypeInfo;
  data: PTypeData;
begin
  Guard.CheckTypeKind<T>(tkEnumeration, 'T');

  typeInfo := System.TypeInfo(T);
  data := typeInfo.TypeData;
  Guard.CheckNotNull(data, 'data');

  if (argumentValue < data.MinValue) or (argumentValue > data.MaxValue) then
    raise EInvalidEnumArgumentException.CreateResFmt(@SInvalidEnumArgument, [
      argumentName, typeInfo.TypeName, argumentValue]);
end;

class procedure Guard.CheckRange(condition: Boolean;
  const argumentName: string);
begin
  if not condition then
    Guard.RaiseArgumentOutOfRangeException(argumentName);
end;

class procedure Guard.CheckRange(const buffer: array of Byte;
  index, count: Integer);
begin
  Guard.CheckRange(Length(buffer), index, count);
end;

class procedure Guard.CheckRange(const buffer: array of Char;
  index, count: Integer);
begin
  Guard.CheckRange(Length(buffer), index, count);
end;

class procedure Guard.CheckRange(const buffer: array of Byte; index: Integer);
begin
  Guard.CheckIndex(Length(buffer), index);
end;

class procedure Guard.CheckRange(const buffer: array of Char; index: Integer);
begin
  Guard.CheckIndex(Length(buffer), index);
end;

class procedure Guard.CheckRange(const s: string; index: Integer);
begin
  Guard.CheckIndex(Length(s), index, 1);
end;

class procedure Guard.CheckRange(const s: string; index, count: Integer);
begin
  Guard.CheckRange(Length(s), index, count, 1);
end;

class procedure Guard.CheckRangeInclusive(value, min, max: Integer);
const
  ValueArgName = 'value';
begin
  if (value < min) or (value > max) then
    Guard.RaiseArgumentOutOfRangeException(ValueArgName);
end;

class procedure Guard.CheckSet<T>(const argumentValue: T;
  const argumentName: string);
var
  value: Integer;
begin
  value := 0;
  Move(argumentValue, value, SizeOf(T));
  Guard.CheckSet<T>(value, argumentName);
end;

class procedure Guard.CheckSet<T>(argumentValue: Cardinal;
  const argumentName: string);
var
  typeInfo: PTypeInfo;
  data: PTypeData;
  maxValue: Cardinal;
begin
  Guard.CheckTypeKind<T>(tkSet, 'T');

  typeInfo := System.TypeInfo(T);
  data := typeInfo.TypeData;
  Guard.CheckNotNull(data, 'data');

  if Assigned(data.CompType) then
  begin
    data := data.CompType^.TypeData;
    maxValue := (1 shl (data.MaxValue - data.MinValue + 1)) - 1;
  end
  else
    case data^.OrdType of
      otSByte, otUByte: maxValue := High(Byte);
      otSWord, otUWord: maxValue := High(Word);
      otSLong, otULong: Exit;
    else
      maxValue := 0;
    end;

  if argumentValue > maxValue then
    raise EInvalidEnumArgumentException.CreateResFmt(@SInvalidSetArgument, [
      argumentName, typeInfo.TypeName, argumentValue]);
end;

class procedure Guard.CheckRangeExclusive(value, min, max: Integer);
const
  ValueArgName = 'value';
begin
  if (value <= min) or (value >= max) then
    Guard.RaiseArgumentOutOfRangeException(ValueArgName);
end;


{$IFNDEF NEXTGEN}
class procedure Guard.CheckRange(const s: WideString; index: Integer);
begin
  Guard.CheckIndex(Length(s), index, 1);
end;

class procedure Guard.CheckRange(const s: WideString; index, count: Integer);
begin
  Guard.CheckRange(Length(s), index, count, 1);
end;

class procedure Guard.CheckRange(const s: RawByteString; index: Integer);
begin
  Guard.CheckIndex(Length(s), index, 1);
end;

class procedure Guard.CheckRange(const s: RawByteString; index, count: Integer);
begin
  Guard.CheckRange(Length(s), index, count, 1);
end;
{$ENDIF}

class procedure Guard.CheckTypeKind(typeInfo: PTypeInfo;
  expectedTypeKind: TTypeKind; const argumentName: string);
begin
  Guard.CheckNotNull(typeInfo, argumentName);
  if typeInfo.Kind <> expectedTypeKind then
    RaiseArgumentException(typeInfo.Kind, argumentName);
end;

class procedure Guard.CheckTypeKind(typeInfo: PTypeInfo;
  expectedTypeKinds: TTypeKinds; const argumentName: string);
begin
  Guard.CheckNotNull(typeInfo, argumentName);
  if not (typeInfo.Kind in expectedTypeKinds) then
    RaiseArgumentException(typeInfo.Kind, argumentName);
end;

class function Guard.IsNullReference(const value; typeInfo: PTypeInfo): Boolean;
const
  ReferenceKinds = [
    tkClass, tkMethod, tkInterface, tkClassRef, tkPointer, tkProcedure];
begin
  Result := False;
  if Assigned(typeInfo) and (typeInfo.Kind in ReferenceKinds) then
    if typeInfo.Kind = tkMethod then
      Result := not Assigned(TMethod(value).Code) and not Assigned(TMethod(value).Data)
    else
      Result := not Assigned(PPointer(@value)^);
end;

{$ENDREGION}


{$REGION 'Nullable<T>'}

constructor Nullable<T>.Create(const value: T);
begin
  fValue := value;
  fHasValue := Nullable.HasValue;
end;

constructor Nullable<T>.Create(const value: Variant);
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    fValue := v.AsType<T>;
    fHasValue := Nullable.HasValue;
  end
  else
  begin
    fHasValue := '';
    fValue := Default(T);
  end;
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := fHasValue <> '';
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
    Guard.RaiseNullableHasNoValue;
  Result := fValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := fValue
  else
    Result := Default(T);
end;

function Nullable<T>.GetValueOrDefault(const defaultValue: T): T;
begin
  if HasValue then
    Result := fValue
  else
    Result := defaultValue;
end;

class function Nullable<T>.EqualsComparer(const left, right: T): Boolean;
begin
  if not Assigned(fComparer) then
    fComparer := TEqualityComparer<T>.Default;
  Result := fComparer.Equals(left, right);
end;

class function Nullable<T>.EqualsInternal(const left, right: T): Boolean;
begin
  case TType.Kind<T> of
    tkInteger, tkEnumeration:
    begin
      case SizeOf(T) of
        1: Result := PByte(@left)^ = PByte(@right)^;
        2: Result := PWord(@left)^ = PWord(@right)^;
        4: Result := PCardinal(@left)^ = PCardinal(@right)^;
      end;
    end;
{$IFNDEF NEXTGEN}
    tkChar: Result := PAnsiChar(@left)^ = PAnsiChar(@right)^;
    tkString: Result := PShortString(@left)^ = PShortString(@right)^;
    tkLString: Result := PAnsiString(@left)^ = PAnsiString(@right)^;
    tkWString: Result := PWideString(@left)^ = PWideString(@right)^;
{$ENDIF}
    tkFloat:
    begin
      if TypeInfo(T) = TypeInfo(Single) then
        Result := Math.SameValue(PSingle(@left)^, PSingle(@right)^)
      else if TypeInfo(T) = TypeInfo(Double) then
        Result := Math.SameValue(PDouble(@left)^, PDouble(@right)^)
      else if TypeInfo(T) = TypeInfo(Extended) then
        Result := Math.SameValue(PExtended(@left)^, PExtended(@right)^)
      else if TypeInfo(T) = TypeInfo(TDateTime) then
        Result := SameDateTime(PDateTime(@left)^, PDateTime(@right)^)
      else
        case GetTypeData(TypeInfo(T)).FloatType of
          ftSingle: Result := Math.SameValue(PSingle(@left)^, PSingle(@right)^);
          ftDouble: Result := Math.SameValue(PDouble(@left)^, PDouble(@right)^);
          ftExtended: Result := Math.SameValue(PExtended(@left)^, PExtended(@right)^);
          ftComp: Result := PComp(@left)^ = PComp(@right)^;
          ftCurr: Result := PCurrency(@left)^ = PCurrency(@right)^;
        end;
    end;
    tkWChar: Result := PWideChar(@left)^ = PWideChar(@right)^;
    tkInt64: Result := PInt64(@left)^ = PInt64(@right)^;
    tkUString: Result := PUnicodeString(@left)^ = PUnicodeString(@right)^;
  else
    Result := EqualsComparer(left, right);
  end;
end;

function Nullable<T>.Equals(const other: Nullable<T>): Boolean;
begin
  if not HasValue then
    Exit(not other.HasValue);
  if not other.HasValue then
    Exit(False);
  Result := EqualsInternal(fValue, other.fValue);
end;

class operator Nullable<T>.Implicit(const value: T): Nullable<T>;
begin
  Result.fValue := value;
  Result.fHasValue := Nullable.HasValue;
end;

{$IFDEF IMPLICIT_NULLABLE}
class operator Nullable<T>.Implicit(const value: Nullable<T>): T;
begin
  Result := value.Value;
end;
{$ENDIF}

{$IFDEF UNSAFE_NULLABLE}
class operator Nullable<T>.Implicit(const value: Nullable<T>): Variant;
var
  v: TValue;
begin
  if value.HasValue then
  begin
    v := TValue.From<T>(value.fValue);
    if v.IsType<Boolean> then
      Result := v.AsBoolean
    else
      Result := v.AsVariant;
  end
  else
    Result := Null;
end;

class operator Nullable<T>.Implicit(const value: Variant): Nullable<T>;
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    Result.fValue := v.AsType<T>;
    Result.fHasValue := Nullable.HasValue;
  end
  else
    Result := Default(Nullable<T>);
end;
{$ENDIF}

class operator Nullable<T>.Explicit(const value: Variant): Nullable<T>;
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    Result.fValue := v.AsType<T>;
    Result.fHasValue := Nullable.HasValue;
  end
  else
    Result := Default(Nullable<T>);
end;

class operator Nullable<T>.Explicit(const value: Nullable<T>): T;
begin
  Result := value.Value;
end;

class operator Nullable<T>.Implicit(const value: Nullable.Null): Nullable<T>;
begin
  Result.fValue := Default(T);
  Result.fHasValue := '';
end;

class operator Nullable<T>.Equal(const left, right: Nullable<T>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Nullable<T>.Equal(const left: Nullable<T>;
  const right: T): Boolean;
begin
  if left.fHasValue = '' then
    Exit(False);
  Result := EqualsInternal(left.fValue, right);
end;

class operator Nullable<T>.Equal(const left: Nullable<T>;
  const right: Nullable.Null): Boolean;
begin
  Result := left.fHasValue = '';
end;

class operator Nullable<T>.NotEqual(const left, right: Nullable<T>): Boolean;
begin
  Result := not left.Equals(right);
end;

class operator Nullable<T>.NotEqual(const left: Nullable<T>;
  const right: Nullable.Null): Boolean;
begin
  Result := left.fHasValue <> '';
end;

class operator Nullable<T>.NotEqual(const left: Nullable<T>;
  const right: T): Boolean;
begin
  if left.fHasValue = '' then
    Exit(True);
  Result := not EqualsInternal(left.fValue, right);
end;

function Nullable<T>.ToString: string;
var
  v: TValue;
begin
  if HasValue then
  begin
    v := TValue.From<T>(fValue);
    Result := v.ToString;
  end
  else
    Result := 'Null';
end;

function Nullable<T>.ToVariant: Variant;
var
  v: TValue;
begin
  if HasValue then
  begin
    v := TValue.From<T>(fValue);
    if v.IsType<Boolean> then
      Result := v.AsBoolean
    else
      Result := v.AsVariant;
  end
  else
    Result := Null;
end;

function Nullable<T>.TryGetValue(out value: T): Boolean;
begin
  Result := fHasValue <> '';
  if Result then
    value := fValue;
end;

{$ENDREGION}


{$REGION 'TNullableHelper'}

constructor TNullableHelper.Create(typeInfo: PTypeInfo);
var
  p: PByte;
  field: PRecordTypeField;
begin
  p := @typeInfo.TypeData.ManagedFldCount;
  // skip TTypeData.ManagedFldCount and TTypeData.ManagedFields
  Inc(p, SizeOf(Integer) + SizeOf(TManagedField) * PInteger(p)^);
  // skip TTypeData.NumOps and TTypeData.RecOps
  Inc(p, SizeOf(Byte) + SizeOf(Pointer) * p^);
  // skip TTypeData.RecFldCnt
  Inc(p, SizeOf(Integer));
  // get TTypeData.RecFields[0]
  field := PRecordTypeField(p);
  fValueType := field.Field.TypeRef^;
  // get TTypeData.RecFields[1]
  field := PRecordTypeField(PByte(SkipShortString(@field.Name)) + SizeOf(TAttrData));
  fHasValueOffset := field.Field.FldOffset;
end;

function TNullableHelper.GetValue(instance: Pointer): TValue;
begin
  TValue.Make(instance, fValueType, Result);
end;

function TNullableHelper.HasValue(instance: Pointer): Boolean;
begin
  Result := PUnicodeString(PByte(instance) + fHasValueOffset)^ <> '';
end;

procedure TNullableHelper.SetValue(instance: Pointer; const value: TValue);
begin
  value.Cast(fValueType).ExtractRawData(instance);
  if value.IsEmpty then
    PUnicodeString(PByte(instance) + fHasValueOffset)^ := ''
  else
    PUnicodeString(PByte(instance) + fHasValueOffset)^ := Nullable.HasValue;
end;

{$ENDREGION}


{$REGION 'TLazy'}

constructor TLazy.Create;
begin
  inherited Create;
  fLock := TCriticalSection.Create;
end;

destructor TLazy.Destroy;
begin
  fLock.Free;
  inherited Destroy;
end;

function TLazy.GetIsValueCreated: Boolean;
begin
  Result := fIsValueCreated;
end;

{$ENDREGION}


{$REGION 'TLazy<T>'}

constructor TLazy<T>.Create;
var
  classType: TClass;
  ctor: TConstructor;
begin
  Guard.CheckTypeKind<T>([tkClass], 'T');

  classType := GetTypeData(TypeInfo(T)).ClassType;
  ctor := TActivator.FindConstructor(classType);

  inherited Create;
  fValueFactory :=
    function: T
    begin
      PObject(@Result)^ := ctor(classType);
    end;
end;

constructor TLazy<T>.Create(const valueFactory: TFunc<T>; ownsObject: Boolean);
begin
  Guard.CheckNotNull(Assigned(valueFactory), 'valueFactory');

  inherited Create;
  fOwnsObjects := ownsObject;
  fValueFactory := valueFactory;
end;

constructor TLazy<T>.CreateFrom(const value: T; ownsObject: Boolean);
begin
  inherited Create;
  fValue := value;
  fIsValueCreated := True;
  fOwnsObjects := ownsObject;
end;

destructor TLazy<T>.Destroy;
begin
  inherited Destroy;
  if TType.Kind<T> = tkClass then
{$IFNDEF AUTOREFCOUNT}
    if fOwnsObjects then
      FreeAndNil(fValue);
{$ENDIF}
end;

procedure TLazy<T>.InitializeValue;
begin
  fLock.Enter;
  try
    if fIsValueCreated then
      Exit;

    fValue := fValueFactory();
    fValueFactory := nil;
    fIsValueCreated := True;
  finally
    fLock.Leave;
  end;
end;

function TLazy<T>.GetValue: T;
begin
  if not fIsValueCreated then
    InitializeValue;
  Result := fValue;
end;

function TLazy<T>.GetValueNonGeneric: TValue;
begin
  Result := TValue.From<T>(Value);
end;

{$ENDREGION}


{$REGION 'Lazy<T>'}

class function Lazy<T>.Create: Lazy<T>;
begin
  Result.fLazy := TLazy<T>.Create;
end;

constructor Lazy<T>.Create(const valueFactory: TFunc<T>; ownsObject: Boolean);
begin
  fLazy := TLazy<T>.Create(valueFactory, ownsObject);
end;

constructor Lazy<T>.CreateFrom(const value: T; ownsObject: Boolean);
begin
  fLazy := TLazy<T>.CreateFrom(value, ownsObject);
end;

function Lazy<T>.GetValue: T;
begin
  if not Assigned(fLazy) then
    Guard.RaiseNoDelegateAssigned;
  Result := fLazy.Value;
end;

function Lazy<T>.GetIsValueCreated: Boolean;
begin
  Result := Assigned(fLazy) and fLazy.IsValueCreated;
end;

function Lazy<T>.GetIsAssigned: Boolean;
begin
  Result := Assigned(fLazy);
end;

class operator Lazy<T>.Implicit(const value: Lazy<T>): ILazy<T>;
begin
  Result := value.fLazy;
end;

class operator Lazy<T>.Implicit(const value: Lazy<T>): T;
begin
  Result := value.Value;
end;

class operator Lazy<T>.Implicit(const value: T): Lazy<T>;
begin
  Result.fLazy := TLazy<T>.CreateFrom(value);
end;

class operator Lazy<T>.Implicit(const value: TFunc<T>): Lazy<T>;
begin
  Result.fLazy := TLazy<T>.Create(value);
end;

class operator Lazy<T>.Implicit(const value: TLazy<T>): Lazy<T>;
begin
  Result.fLazy := value;
end;

{$ENDREGION}


{$REGION 'TLazyInitializer'}

class function TLazyInitializer.EnsureInitialized<T>(var target: T): T;
var
  value: T;
begin
  if target = nil then
  begin
    value := T.Create;
    if AtomicCmpExchange(PPointer(@target)^, PPointer(@value)^, nil) <> nil then
      value.Free;
  end;
  Result := target;
end;

class function TLazyInitializer.EnsureInitialized<T>(var target: T;
  const valueFactory: TFunc<T>): T;
var
  value: T;
begin
  Guard.CheckTypeKind<T>([tkClass, tkInterface], 'T');

  if PPointer(@target)^ = nil then
  begin
    value := valueFactory;
    if PPointer(@value)^ = nil then
      raise EInvalidOperationException.CreateRes(@SValueFactoryReturnedNil);
    case TType.Kind<T> of
      tkClass:
        if AtomicCmpExchange(PPointer(@target)^, PPointer(@value)^, nil) <> nil then
          PObject(@value)^.Free;
      tkInterface:
        if AtomicCmpExchange(PPointer(@target)^, PPointer(@value)^, nil) <> nil then
          value := Default(T);
    end;
  end;
  Result := target;
end;

{$ENDREGION}


{$REGION 'Shared<T>'}

class operator Shared<T>.Implicit(const value: T): Shared<T>;
begin
  Result.fValue := value;
  case TType.Kind<T> of
{$IFNDEF AUTOREFCOUNT}
    tkClass:
      if PPointer(@value)^ = nil then
        Result.fFinalizer := nil
      else
        Result.fFinalizer := Shared.TObjectFinalizer.Create(PObject(@value)^);
{$ENDIF}
    tkPointer:
      if PPointer(@value)^ = nil then
        Result.fFinalizer := nil
      else
        Result.fFinalizer := Shared.TRecordFinalizer.Create(PPointer(@value)^, TypeInfo(T));
  end;
end;

class function Shared<T>.GetNew: IShared<T>;
begin
  case TType.Kind<T> of
    tkClass: IShared<TObject>(Result) := Shared.TObjectFinalizer.Create(TypeInfo(T));
    tkPointer: IShared<Pointer>(Result) := Shared.TRecordFinalizer.Create(TypeInfo(T));
  end;
end;

class operator Shared<T>.Implicit(const value: Shared<T>): T;
begin
  Result := value.fValue;
end;

{$ENDREGION}


{$REGION 'Shared'}

class function Shared.New<T>(const value: T): IShared<T>;
begin
  case TType.Kind<T> of
    tkClass: IShared<TObject>(Result) := Shared.TObjectFinalizer.Create(PObject(@value)^);
    tkPointer: IShared<Pointer>(Result) := Shared.TRecordFinalizer.Create(PPointer(@value)^, TypeInfo(T));
  end;
end;

{$ENDREGION}


{$REGION 'Shared.TObjectFinalizer'}

constructor Shared.TObjectFinalizer.Create(typeInfo: PTypeInfo);
begin
  inherited Create;
  fValue := TActivator.CreateInstance(typeInfo.TypeData.ClassType);
end;

constructor Shared.TObjectFinalizer.Create(const value: TObject);
begin
  inherited Create;
  fValue := value;
end;

{$IFNDEF AUTOREFCOUNT}
destructor Shared.TObjectFinalizer.Destroy;
begin
  fValue.Free;
  inherited;
end;
{$ENDIF}

function Shared.TObjectFinalizer.Invoke: TObject;
begin
  Result := fValue;
end;

{$ENDREGION}


{$REGION 'Shared.TRecordFinalizer'}

constructor Shared.TRecordFinalizer.Create(typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo.TypeData.RefType^;
  fValue := AllocMem(GetTypeSize(fTypeInfo));
end;

constructor Shared.TRecordFinalizer.Create(const value: Pointer; typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo.TypeData.RefType^;
  fValue := value;
end;

destructor Shared.TRecordFinalizer.Destroy;
begin
  FinalizeArray(fValue, fTypeInfo, 1);
  FillChar(fValue^, fTypeInfo.TypeData.RecSize, 0);
  FreeMem(fValue);
  inherited;
end;

function Shared.TRecordFinalizer.Invoke: Pointer;
begin
  Result := fValue;
end;

{$ENDREGION}


{$REGION 'TWeakReferences'}

type
  TWeakReferences = class
  strict private
    fLock: TCriticalSection;
    fWeakReferences: TDictionary<Pointer, TList>;
    class var fDefault: TWeakReferences;
  protected
    class property Default: TWeakReferences read fDefault;
  public
    constructor Create;
    destructor Destroy; override;

    class constructor Create;
    class destructor Destroy;

    procedure RegisterWeakRef(address: Pointer; instance: Pointer);
    procedure UnregisterWeakRef(address: Pointer; instance: Pointer);
  end;

constructor TWeakReferences.Create;
begin
  inherited Create;
  fLock := TCriticalSection.Create;
  fWeakReferences := TObjectDictionary<Pointer, TList>.Create([doOwnsValues]);
end;

destructor TWeakReferences.Destroy;
begin
  fWeakReferences.Free;
  fLock.Free;
  inherited Destroy;
end;

class constructor TWeakReferences.Create;
begin
  fDefault := TWeakReferences.Create;
end;

class destructor TWeakReferences.Destroy;
begin
  fDefault.Free;
end;

procedure TWeakReferences.RegisterWeakRef(address, instance: Pointer);
var
  addresses: TList;
begin
  fLock.Enter;
  try
    if not fWeakReferences.TryGetValue(instance, addresses) then
    begin
      addresses := TList.Create;
      fWeakReferences.Add(instance, addresses);
    end;
    addresses.Add(address);
  finally
    fLock.Leave;
  end;
end;

procedure TWeakReferences.UnregisterWeakRef(address, instance: Pointer);
var
  addresses: TList;
begin
  fLock.Enter;
  try
    if fWeakReferences.TryGetValue(instance, addresses) then
    begin
      if Assigned(address) then
      begin
        PPointer(address)^ := nil;
        addresses.Remove(address);
        if addresses.Count = 0 then
          fWeakReferences.Remove(instance);
      end
      else
      begin
        for address in addresses do
          PPointer(address)^ := nil;
        fWeakReferences.Remove(instance);
      end;
    end;
  finally
    fLock.Leave;
  end;
end;

{$ENDREGION}


{$REGION 'TWeakReference'}

type
  TVirtualClasses = class(Spring.VirtualClass.TVirtualClasses);

function TWeakReference.GetIsAlive: Boolean;
begin
  Result := Assigned(fTarget);
end;

procedure WeakRefFreeInstance(const Self: TObject);
var
  freeInstance: TFreeInstance;
begin
  freeInstance := GetClassData(Self.ClassParent).FreeInstance;
  TWeakReferences.Default.UnregisterWeakRef(nil, Self);
  freeInstance(Self);
end;

procedure TWeakReference.RegisterWeakRef(address, instance: Pointer); //FI:O804
begin
  TVirtualClasses.Default.Proxify(instance);
  GetClassData(TObject(instance).ClassType).FreeInstance := WeakRefFreeInstance;
  TWeakReferences.Default.RegisterWeakRef(@fTarget, instance);
end;

procedure TWeakReference.UnregisterWeakRef(address, instance: Pointer);
begin
  TWeakReferences.Default.UnregisterWeakRef(address, instance);
end;

{$ENDREGION}


{$REGION 'TWeakReference<T>'}

constructor TWeakReference<T>.Create(const target: T);
begin
  inherited Create;
  SetTarget(target);
end;

constructor TWeakReference<T>.CreateInternal(const target: T;
  var ref: PPointer);
begin
  inherited Create;
  SetTarget(target);
  ref := @fTarget;
end;

destructor TWeakReference<T>.Destroy;
begin
  SetTarget(Default(T));
  inherited Destroy;
end;

function TWeakReference<T>.GetTarget: T;
begin
  if IsAlive then
    case PTypeInfo(TypeInfo(T)).Kind of
      tkClass: PObject(@Result)^ := TObject(fTarget);
      tkInterface: PInterface(@Result)^ := IInterface(fTarget)
    end
  else
    Result := Default(T);
end;

procedure TWeakReference<T>.SetTarget(const value: T);
var
  typeInfo: PTypeInfo;
begin
  typeInfo := System.TypeInfo(T);
  if Assigned(fTarget) then
    case typeInfo.Kind of
      tkClass: UnregisterWeakRef(@fTarget, fTarget);
      tkInterface: UnregisterWeakRef(@fTarget, IInterface(fTarget) as TObject);
    end;
  fTarget := PPointer(@value)^;
  if Assigned(fTarget) then
    case typeInfo.Kind of
      tkClass: RegisterWeakRef(@fTarget, fTarget);
      tkInterface: RegisterWeakRef(@fTarget, IInterface(fTarget) as TObject);
    end;
end;

function TWeakReference<T>.TryGetTarget(out target: T): Boolean;
begin
  target := GetTarget;
  Result := IsAlive;
end;

{$ENDREGION}


{$REGION 'Weak<T>'}

constructor Weak<T>.Create(const target: T);
begin
  fReference := TWeakReference<T>.CreateInternal(target, fTarget);
end;

function Weak<T>.GetIsAlive: Boolean;
begin
  Result := Assigned(fReference) and Assigned(fTarget^);
end;

function Weak<T>.GetTarget: T;
begin
  if Assigned(fReference) then
    Result := PT(fTarget)^
  else
    Result := Default(T);
end;

procedure Weak<T>.SetTarget(const value: T);
begin
  if Assigned(fReference) then
    fReference.Target := value
  else
    fReference := TWeakReference<T>.CreateInternal(value, fTarget);
end;

function Weak<T>.TryGetTarget(out target: T): Boolean;
begin
  Result := Assigned(fReference) and Assigned(fTarget^);
  if Result then
    target := PT(fTarget)^
  else
    target := Default(T);
end;

class operator Weak<T>.Implicit(const value: Shared<T>): Weak<T>;
begin
  Result.Target := value.Value;
end;

class operator Weak<T>.Implicit(const value: T): Weak<T>;
begin
  Result.Target := value;
end;

class operator Weak<T>.Implicit(const value: Weak<T>): T;
begin
  Result := value.Target;
end;

class operator Weak<T>.Equal(const left: Weak<T>;
  const right: T): Boolean;
begin
  if Assigned(left.fReference) then
    Result := PPointer(@right)^ = left.fTarget^
  else
    Result := PPointer(@right)^ = nil;
end;

class operator Weak<T>.NotEqual(const left: Weak<T>;
  const right: T): Boolean;
begin
  if Assigned(left.fReference) then
    Result := PPointer(@right)^ <> left.fTarget^
  else
    Result := PPointer(@right)^ <> nil;
end;

{$ENDREGION}


{$REGION 'Event<T>'}

procedure Event<T>.Add(const handler: T);
begin
  EnsureInitialized;
  fInstance.Add(handler);
end;

procedure Event<T>.Clear;
begin
  if Assigned(fInstance) then
    fInstance.Clear;
end;

procedure Event<T>.EnsureInitialized;
begin
  if not Assigned(fInstance) then
    fInstance := TEvent<T>.Create;
end;

function Event<T>.GetCanInvoke: Boolean;
begin
  Result := Assigned(fInstance) and fInstance.CanInvoke;
end;

function Event<T>.GetEnabled: Boolean;
begin
  Result := not Assigned(fInstance) or fInstance.Enabled;
end;

function Event<T>.GetInvoke: T;
begin
  EnsureInitialized;
  Result := fInstance.Invoke;
end;

function Event<T>.GetOnChanged: TNotifyEvent;
begin
  EnsureInitialized;
  Result := fInstance.OnChanged;
end;

function Event<T>.GetUseFreeNotification: Boolean;
begin
  Result := not Assigned(fInstance) or fInstance.UseFreeNotification;
end;

procedure Event<T>.Remove(const handler: T);
begin
  if Assigned(fInstance) then
    fInstance.Remove(handler);
end;

procedure Event<T>.RemoveAll(instance: Pointer);
begin
  if Assigned(fInstance) then
    fInstance.RemoveAll(instance);
end;

procedure Event<T>.SetEnabled(const value: Boolean);
begin
  EnsureInitialized;
  fInstance.Enabled := value;
end;

procedure Event<T>.SetOnChanged(value: TNotifyEvent);
begin
  EnsureInitialized;
  fInstance.OnChanged := value;
end;

procedure Event<T>.SetUseFreeNotification(const value: Boolean);
begin
  EnsureInitialized;
  fInstance.UseFreeNotification := value;
end;

class operator Event<T>.Implicit(const value: IEvent<T>): Event<T>;
begin
  Result.fInstance := value;
end;

class operator Event<T>.Implicit(var value: Event<T>): IEvent<T>;
begin
  value.EnsureInitialized;
  Result := value.fInstance;
end;

class operator Event<T>.Implicit(var value: Event<T>): T;
begin
  Result := value.Invoke;
end;

{$ENDREGION}


{$REGION 'TTypeInfoHelper'}

function TTypeInfoHelper.GetRttiType: TRttiType;
begin
  Result := TType.GetType(@Self);
end;

{$IFNDEF DELPHIXE3_UP}
function TTypeInfoHelper.TypeData: PTypeData;
begin
  Result := GetTypeData(@Self);
end;
{$ENDIF}

function TTypeInfoHelper.TypeName: string;
begin
{$IFNDEF NEXTGEN}
  Result := UTF8ToString(Name);
{$ELSE}
  Result := NameFld.ToString;
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TTypeDataHelper'}

{$IFNDEF DELPHIXE3_UP}
function TTypeDataHelper.DynArrElType: PPTypeInfo;
begin
  Result := PPointer(SkipShortString(@DynUnitName))^;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TEventArgs'}

constructor TEventArgs.Create;
begin
  inherited Create;
end;

{$ENDREGION}


{$REGION 'TPropertyChangedEventArgs'}

constructor TPropertyChangedEventArgs.Create(const propertyName: string);
begin
  inherited Create;
  fPropertyName := propertyName;
end;

function TPropertyChangedEventArgs.GetPropertyName: string;
begin
  Result := fPropertyName;
end;

{$ENDREGION}


{$REGION 'TNotificationHandler'}

procedure TNotificationHandler.Notification(Component: TComponent;
  Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if Assigned(fOnNotification) then
    fOnNotification(Component, Operation);
end;

{$ENDREGION}


{$REGION 'TInterfacedCriticalSection'}

function TInterfacedCriticalSection.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedCriticalSection._AddRef: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicIncrement(fRefCount);
{$ELSE}
  Result := __ObjAddRef;
{$ENDIF}
end;

function TInterfacedCriticalSection._Release: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicDecrement(fRefCount);
  if Result = 0 then
    Destroy;
{$ELSE}
  Result := __ObjRelease;
{$ENDIF}
end;

function TInterfacedCriticalSection.ScopedLock: IInterface;
begin
  Result := TScopedLock.Create(Self);
end;

{$ENDREGION}


{$REGION 'TInterfacedCriticalSection.TScopedLock'}

constructor TInterfacedCriticalSection.TScopedLock.Create(
  const criticalSection: ICriticalSection);
begin
  inherited Create;
  fCriticalSection := criticalSection;
  fCriticalSection.Enter;
end;

destructor TInterfacedCriticalSection.TScopedLock.Destroy;
begin
  fCriticalSection.Leave;
  inherited Destroy;
end;

{$ENDREGION}


{$REGION 'Lock'}

procedure Lock.EnsureInitialized;
var
  criticalSection: ICriticalSection;
begin
  if not Assigned(fCriticalSection) then
  begin
    criticalSection := TInterfacedCriticalSection.Create;
    if AtomicCmpExchange(Pointer(fCriticalSection),
      Pointer(criticalSection), nil) = nil then
      Pointer(criticalSection) := nil;
  end;
end;

procedure Lock.Enter;
begin
  EnsureInitialized;
  fCriticalSection.Enter;
end;

procedure Lock.Leave;
begin
  if not Assigned(fCriticalSection) then
    raise EInvalidOperationException.CreateRes(@SCriticalSectionNotInitialized);
  fCriticalSection.Leave;
end;

function Lock.ScopedLock: IInterface;
begin
  EnsureInitialized;
  Result := fCriticalSection.ScopedLock;
end;

{$ENDREGION}


{$REGION 'TActivator'}

class constructor TActivator.Create;
begin
  ConstructorCache := TDictionary<PTypeInfo,TConstructor>.Create;
end;

class destructor TActivator.Destroy;
begin
  ConstructorCache.Free;
end;

class procedure TActivator.ClearCache;
begin
  ConstructorCache.Clear;
end;

class function TActivator.CreateInstance(
  const classType: TRttiInstanceType): TValue;
begin
  Result := CreateInstance(classType, []);
end;

class function TActivator.CreateInstance(const classType: TRttiInstanceType;
  const arguments: array of TValue): TValue;
var
  method: TRttiMethod;
begin
  method := FindConstructor(classType, arguments);
  if not Assigned(method) then
    RaiseNoConstructorFound(classType.MetaclassType);
  Result := CreateInstance(classType, method, arguments)
end;

class function TActivator.CreateInstance(const classType: TRttiInstanceType;
  const constructorMethod: TRttiMethod; const arguments: array of TValue): TValue;
begin
  Result := constructorMethod.Invoke(classType.MetaclassType, arguments);
end;

class function TActivator.CreateInstance(typeInfo: PTypeInfo): TObject;
begin
  Result := CreateInstance(typeInfo.TypeData.ClassType);
end;

class function TActivator.CreateInstance(const typeName: string): TObject;
begin
  Result := CreateInstance(typeName, []);
end;

class function TActivator.CreateInstance(const typeName: string;
  const arguments: array of TValue): TObject;
var
  rttiType: TRttiType;
begin
  rttiType := TType.Context.FindType(typeName);
  Result := CreateInstance(TRttiInstanceType(rttiType), arguments).AsObject;
end;

class function TActivator.CreateInstance(classType: TClass): TObject;
var
  ctor: TConstructor;
begin
  ctor := FindConstructor(classType);
  Result := ctor(classType);
end;

class function TActivator.CreateInstance(classType: TClass;
  const arguments: array of TValue): TObject;
var
  ctor: TRttiMethod;
begin
  if Length(arguments) = 0 then
    Exit(CreateInstance(classType));
  ctor := FindConstructor(TType.GetType(classType), arguments);
  if not Assigned(ctor) then
    RaiseNoConstructorFound(classType);
  Result := ctor.Invoke(classType, arguments).AsObject;
end;

class function TActivator.CreateInstance<T>: T;
begin
  Result := T(CreateInstance(TClass(T)));
end;

class function TActivator.CreateInstance<T>(
  const arguments: array of TValue): T;
begin
  Result := T(CreateInstance(TClass(T), arguments));
end;

class function TActivator.FindConstructor(classType: TClass): TConstructor;
var
  classInfo: PTypeInfo;
  method: TRttiMethod;
begin
  Assert(Assigned(classType));
  classInfo := classType.ClassInfo;
  if ConstructorCache.TryGetValue(classInfo, Result) then
    Exit;

  for method in TType.GetType(classInfo).GetMethods do
  begin
    if not method.IsConstructor then
      Continue;

    if Length(method.GetParameters) = 0 then
    begin
      Result := method.CodeAddress;
      ConstructorCache.AddOrSetValue(classInfo, Result);
      Exit;
    end;
  end;
  Result := nil;
end;

class function TActivator.FindConstructor(const classType: TRttiInstanceType;
  const arguments: array of TValue): TRttiMethod;

  function Assignable(const params: TArray<TRttiParameter>;
    const args: array of TValue): Boolean;
  var
    i: Integer;
    v: TValue;
  begin
    Result := Length(params) = Length(args);
    if Result then
      for i := Low(args) to High(args) do
        if not args[i].TryCast(params[i].paramType.Handle, v) then
          Exit(False);
  end;

var
  method: TRttiMethod;
begin
  for method in classType.GetMethods do
  begin
    if not method.IsConstructor then
      Continue;

    if Assignable(method.GetParameters, arguments) then
    begin
      if Length(arguments) = 0 then
        ConstructorCache.AddOrSetValue(classType.Handle, method.CodeAddress);
      Exit(method);
    end;
  end;
  Result := nil;
end;

class procedure TActivator.RaiseNoConstructorFound(classType: TClass);
begin
  raise ENotSupportedException.CreateResFmt(
    @SNoConstructorFound, [classType.ClassName]);
end;

{$ENDREGION}


{$REGION 'Tuple<T1, T2>'}

constructor Tuple<T1, T2>.Create(const value1: T1; const value2: T2);
begin
  fValue1 := value1;
  fValue2 := value2;
end;

function Tuple<T1, T2>.Equals(const value: Tuple<T1, T2>): Boolean;
var
  comparer1: IEqualityComparer<T1>;
  comparer2: IEqualityComparer<T2>;
begin
  comparer1 := TEqualityComparer<T1>.Default;
  comparer2 := TEqualityComparer<T2>.Default;
  Result := comparer1.Equals(fValue1, value.Value1)
    and comparer2.Equals(fValue2, value.Value2);
end;

class operator Tuple<T1, T2>.Equal(const left, right: Tuple<T1, T2>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Tuple<T1, T2>.Implicit(
  const values: Tuple<T1, T2>): TArray<TValue>;
begin
  SetLength(Result, 2);
  Result[0] := TValue.From<T1>(values.Value1);
  Result[1] := TValue.From<T2>(values.Value2);
end;

class operator Tuple<T1, T2>.Implicit(
  const values: TArray<TValue>): Tuple<T1, T2>;
begin
  Result.fValue1 := values[0].AsType<T1>;
  Result.fValue2 := values[1].AsType<T2>;
end;

class operator Tuple<T1, T2>.Implicit(
  const values: array of const): Tuple<T1, T2>;
var
  value: TValue;
begin
  value := TValue.FromVarRec(values[0]);
  Result.fValue1 := value.AsType<T1>;
  value := TValue.FromVarRec(values[1]);
  Result.fValue2 := value.AsType<T2>;
end;

class operator Tuple<T1, T2>.NotEqual(const left,
  right: Tuple<T1, T2>): Boolean;
begin
  Result := not left.Equals(right);
end;

procedure Tuple<T1, T2>.Unpack(out value1: T1; out value2: T2);
begin
  value1 := fValue1;
  value2 := fValue2;
end;

{$ENDREGION}


{$REGION 'Tuple<T1, T2, T3>'}

constructor Tuple<T1, T2, T3>.Create(const value1: T1; const value2: T2;
  const value3: T3);
begin
  fValue1 := value1;
  fValue2 := value2;
  fValue3 := value3;
end;

function Tuple<T1, T2, T3>.Equals(const value: Tuple<T1, T2, T3>): Boolean;
var
  comparer1: IEqualityComparer<T1>;
  comparer2: IEqualityComparer<T2>;
  comparer3: IEqualityComparer<T3>;
begin
  comparer1 := TEqualityComparer<T1>.Default;
  comparer2 := TEqualityComparer<T2>.Default;
  comparer3 := TEqualityComparer<T3>.Default;
  Result := comparer1.Equals(fValue1, value.Value1)
    and comparer2.Equals(fValue2, value.Value2)
    and comparer3.Equals(fValue3, value.Value3);
end;

class operator Tuple<T1, T2, T3>.Equal(const left,
  right: Tuple<T1, T2, T3>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Tuple<T1, T2, T3>.Implicit(
  const values: Tuple<T1, T2, T3>): TArray<TValue>;
begin
  SetLength(Result, 3);
  Result[0] := TValue.From<T1>(values.Value1);
  Result[1] := TValue.From<T2>(values.Value2);
  Result[2] := TValue.From<T3>(values.Value3);
end;

class operator Tuple<T1, T2, T3>.Implicit(
  const values: Tuple<T1, T2, T3>): Tuple<T1, T2>;
begin
  Result.fValue1 := values.Value1;
  Result.fValue2 := values.Value2;
end;

class operator Tuple<T1, T2, T3>.Implicit(
  const values: TArray<TValue>): Tuple<T1, T2, T3>;
begin
  Result.fValue1 := values[0].AsType<T1>;
  Result.fValue2 := values[1].AsType<T2>;
  Result.fValue3 := values[2].AsType<T3>;
end;

class operator Tuple<T1, T2, T3>.Implicit(
  const values: array of const): Tuple<T1, T2, T3>;
var
  value: TValue;
begin
  value := TValue.FromVarRec(values[0]);
  Result.fValue1 := value.AsType<T1>;
  value := TValue.FromVarRec(values[1]);
  Result.fValue2 := value.AsType<T2>;
  value := TValue.FromVarRec(values[2]);
  Result.fValue3 := value.AsType<T3>;
end;

class operator Tuple<T1, T2, T3>.NotEqual(const left,
  right: Tuple<T1, T2, T3>): Boolean;
begin
  Result := not left.Equals(right);
end;

procedure Tuple<T1, T2, T3>.Unpack(out value1: T1; out value2: T2);
begin
  value1 := fValue1;
  value2 := fValue2;
end;

procedure Tuple<T1, T2, T3>.Unpack(out value1: T1; out value2: T2;
  out value3: T3);
begin
  value1 := fValue1;
  value2 := fValue2;
  value3 := fValue3;
end;

{$ENDREGION}


{$REGION 'Tuple<T1, T2, T3, T4>'}

constructor Tuple<T1, T2, T3, T4>.Create(const value1: T1; const value2: T2;
  const value3: T3; const value4: T4);
begin
  fValue1 := value1;
  fValue2 := value2;
  fValue3 := value3;
  fValue4 := value4;
end;

function Tuple<T1, T2, T3, T4>.Equals(
  const value: Tuple<T1, T2, T3, T4>): Boolean;
var
  comparer1: IEqualityComparer<T1>;
  comparer2: IEqualityComparer<T2>;
  comparer3: IEqualityComparer<T3>;
  comparer4: IEqualityComparer<T4>;
begin
  comparer1 := TEqualityComparer<T1>.Default;
  comparer2 := TEqualityComparer<T2>.Default;
  comparer3 := TEqualityComparer<T3>.Default;
  comparer4 := TEqualityComparer<T4>.Default;
  Result := comparer1.Equals(fValue1, value.Value1)
    and comparer2.Equals(fValue2, value.Value2)
    and comparer3.Equals(fValue3, value.Value3)
    and comparer4.Equals(fValue4, value.Value4);
end;

class operator Tuple<T1, T2, T3, T4>.Equal(const left,
  right: Tuple<T1, T2, T3, T4>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const values: Tuple<T1, T2, T3, T4>): TArray<TValue>;
begin
  SetLength(Result, 4);
  Result[0] := TValue.From<T1>(values.Value1);
  Result[1] := TValue.From<T2>(values.Value2);
  Result[2] := TValue.From<T3>(values.Value3);
  Result[3] := TValue.From<T4>(values.Value4);
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const values: Tuple<T1, T2, T3, T4>): Tuple<T1, T2>;
begin
  Result.fValue1 := values.Value1;
  Result.fValue2 := values.Value2;
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const values: Tuple<T1, T2, T3, T4>): Tuple<T1, T2, T3>;
begin
  Result.fValue1 := values.Value1;
  Result.fValue2 := values.Value2;
  Result.fValue3 := values.Value3;
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const values: TArray<TValue>): Tuple<T1, T2, T3, T4>;
begin
  Result.fValue1 := values[0].AsType<T1>;
  Result.fValue2 := values[1].AsType<T2>;
  Result.fValue3 := values[2].AsType<T3>;
  Result.fValue4 := values[3].AsType<T4>;
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const values: array of const): Tuple<T1, T2, T3, T4>;
var
  value: TValue;
begin
  value := TValue.FromVarRec(values[0]);
  Result.fValue1 := value.AsType<T1>;
  value := TValue.FromVarRec(values[1]);
  Result.fValue2 := value.AsType<T2>;
  value := TValue.FromVarRec(values[2]);
  Result.fValue3 := value.AsType<T3>;
  value := TValue.FromVarRec(values[3]);
  Result.fValue4 := value.AsType<T4>;
end;

class operator Tuple<T1, T2, T3, T4>.NotEqual(const left,
  right: Tuple<T1, T2, T3, T4>): Boolean;
begin
  Result := not left.Equals(right);
end;

procedure Tuple<T1, T2, T3, T4>.Unpack(out value1: T1; out value2: T2);
begin
  value1 := fValue1;
  value2 := fValue2;
end;

procedure Tuple<T1, T2, T3, T4>.Unpack(out value1: T1; out value2: T2;
  out value3: T3);
begin
  value1 := fValue1;
  value2 := fValue2;
  value3 := fValue3;
end;

procedure Tuple<T1, T2, T3, T4>.Unpack(out value1: T1; out value2: T2;
  out value3: T3; out value4: T4);
begin
  value1 := fValue1;
  value2 := fValue2;
  value3 := fValue3;
  value4 := fValue4;
end;

{$ENDREGION}


{$REGION 'Tuple'}

class function Tuple.Create<T1, T2>(const value1: T1;
  const value2: T2): Tuple<T1, T2>;
begin
  Result.fValue1 := value1;
  Result.fValue2 := value2;
end;

class function Tuple.Create<T1, T2, T3>(const value1: T1; const value2: T2;
  const value3: T3): Tuple<T1, T2, T3>;
begin
  Result.fValue1 := value1;
  Result.fValue2 := value2;
  Result.fValue3 := value3;
end;

class function Tuple.Create<T1, T2, T3, T4>(const value1: T1; const value2: T2;
  const value3: T3; const value4: T4): Tuple<T1, T2, T3, T4>;
begin
  Result.fValue1 := value1;
  Result.fValue2 := value2;
  Result.fValue3 := value3;
  Result.fValue4 := value4;
end;

{$ENDREGION}


{$REGION 'TArray'}

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparer: IComparer<T>; index,
  count: Integer): Boolean;
var
  left, right, i, c: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(values) - index), 'count');
{$ENDIF}

  if count = 0 then
  begin
    foundIndex := index;
    Exit(False);
  end;

  Result := False;
  left := index;
  right := index + count - 1;
  while left <= right do
  begin
    i := left + (right - left) shr 1;
    c := comparer.Compare(values[i], Item);
    if c < 0 then
      left := i + 1
    else
    begin
      right := i - 1;
      if c = 0 then
        Result := True;
    end;
  end;
  foundIndex := left;
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparer: IComparer<T>): Boolean;
begin
  Result := BinarySearch<T>(values, item, foundIndex, comparer,
    Low(values), Length(values));
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer): Boolean;
begin
  Result := BinarySearch<T>(values, item, foundIndex, TComparer<T>.Default(),
    Low(values), Length(values));
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparison: TComparison<T>; index,
  count: Integer): Boolean;
begin
  Result := BinarySearch<T>(values, item, foundIndex,
    IComparer<T>(PPointer(@comparison)^), index, count);
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparison: TComparison<T>): Boolean;
begin
  Result := BinarySearch<T>(values, item, foundIndex,
    IComparer<T>(PPointer(@comparison)^));
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T;
  const item: T; out foundIndex: Integer; const comparer: IComparer<T>;
  index, count: Integer): Boolean;
var
  left, right, i, c: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(values) - index), 'count');
{$ENDIF}

  if count = 0 then
  begin
    foundIndex := index;
    Exit(False);
  end;

  Result := False;
  left := index;
  right := index + count - 1;
  while left <= right do
  begin
    i := left + (right - left) shr 1;
    c := comparer.Compare(values[i], item);
    if c > 0 then
      right := i - 1
    else
    begin
      left := i + 1;
      if c = 0 then
        Result := True;
    end;
  end;
  foundIndex := right;
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T;
  const item: T; out foundIndex: Integer;
  const comparer: IComparer<T>): Boolean;
begin
  Result := BinarySearchUpperBound<T>(values, item, foundIndex, comparer,
    Low(values), Length(values));
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T;
  const item: T; out foundIndex: Integer): Boolean;
begin
  Result := BinarySearchUpperBound<T>(values, item, foundIndex,
    TComparer<T>.Default(), Low(values), Length(values));
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T;
  const item: T; out foundIndex: Integer; const comparison: TComparison<T>;
  index, count: Integer): Boolean;
begin
  Result := BinarySearchUpperBound<T>(values, item, foundIndex,
    IComparer<T>(PPointer(@comparison)^), index, count);
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T;
  const item: T; out foundIndex: Integer;
  const comparison: TComparison<T>): Boolean;
begin
  Result := BinarySearchUpperBound<T>(values, item, foundIndex,
    IComparer<T>(PPointer(@comparison)^), Low(values), Length(values));
end;

class function TArray.Concat<T>(const values: array of TArray<T>): TArray<T>;
var
  i, k, n: Integer;
begin
  n := 0;
  for i := Low(values) to High(values) do
    Inc(n, Length(values[i]));
  SetLength(Result, n);
  n := 0;
  for i := Low(values) to High(values) do
    for k := Low(values[i]) to High(values[i]) do
    begin
      Result[n] := values[i, k];
      Inc(n);
    end;
end;

class function TArray.Contains<T>(const values: array of T;
  const item: T): Boolean;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := TEqualityComparer<T>.Default;
  for i := Low(Values) to High(Values) do
    if comparer.Equals(values[i], item) then
      Exit(True);
  Result := False;
end;

class function TArray.Copy<T>(const values: array of T): TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Length(values));
  for i := Low(values) to High(values) do
    Result[i] := values[i];
end;

class procedure TArray.Copy<T>(const source: array of T;
  var target: array of T; count: NativeInt);
begin
  Copy<T>(source, target, 0, 0, count);
end;

class procedure TArray.Copy<T>(const source: array of T;
  var target: array of T; sourceIndex, targetIndex, count: NativeInt);
var
  sourceLength, targetLength: NativeInt;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  sourceLength := Length(source);
  targetLength := Length(target);
  Guard.CheckRange((sourceIndex >= 0) and (sourceIndex <= sourceLength), 'sourceIndex');
  Guard.CheckRange((targetIndex >= 0) and (targetIndex <= targetLength), 'targetIndex');
  Guard.CheckRange((count >= 0)
    and (count <= sourceLength - sourceIndex)
    and (count <= targetLength - targetIndex), 'count');
  if Pointer(@source[0]) = Pointer(@target[0]) then
    raise EArgumentException.CreateRes(@SArraysIdentical);
{$ENDIF}
  if TType.IsManaged<T> then
    System.CopyArray(Pointer(@target[targetIndex]), Pointer(@source[sourceIndex]), TypeInfo(T), count)
  else
    System.Move(Pointer(@source[sourceIndex])^, Pointer(@target[targetIndex])^, count * SizeOf(T));
end;

class procedure TArray.ForEach<T>(const values: array of T;
  const action: TAction<T>);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    action(values[i]);
end;

class function TArray.IndexOf<T>(const values: array of T;
  const item: T): Integer;
begin
  Result := IndexOf<T>(values, item,
    0, Length(values), TEqualityComparer<T>.Default);
end;

class function TArray.IndexOf<T>(const values: array of T; const item: T;
  index: Integer): Integer;
begin
  Result := IndexOf<T>(values, item,
    index, Length(values) - index, TEqualityComparer<T>.Default);
end;

class function TArray.IndexOf<T>(const values: array of T; const item: T;
  index, count: Integer): Integer;
begin
  Result := IndexOf<T>(values, item,
    index, count, TEqualityComparer<T>.Default);
end;

class function TArray.IndexOf<T>(const values: array of T; const item: T;
  index, count: Integer; const comparer: IEqualityComparer<T>): Integer;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(values) - index), 'count');
{$ENDIF}

  for i := index to index + count - 1 do
    if comparer.Equals(values[i], item) then
      Exit(i);
  Result := -1;
end;

class function TArray.LastIndexOf<T>(const values: array of T;
  const item: T): Integer;
begin
  Result := LastIndexOf<T>(values, item,
    0, Length(values), TEqualityComparer<T>.Default);
end;

class function TArray.LastIndexOf<T>(const values: array of T; const item: T;
  index: Integer): Integer;
begin
  Result := LastIndexOf<T>(values, item,
    index, Length(values) - index, TEqualityComparer<T>.Default);
end;

class function TArray.LastIndexOf<T>(const values: array of T; const item: T;
  index, count: Integer): Integer;
begin
  Result := LastIndexOf<T>(values, item,
    index, count, TEqualityComparer<T>.Default);
end;

class function TArray.LastIndexOf<T>(const values: array of T; const item: T;
  index, count: Integer; const comparer: IEqualityComparer<T>): Integer;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(values) - index), 'count');
{$ENDIF}

  for i := index + count - 1 downto index do
    if comparer.Equals(values[i], item) then
      Exit(i);
  Result := -1;
end;

class procedure TArray.Reverse<T>(var values: array of T);
begin
  Reverse<T>(values, 0, Length(values));
end;

class procedure TArray.Reverse<T>(var values: array of T; index,
  count: Integer);
var
  temp: T;
  index1, index2: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(values) - index), 'count');
{$ENDIF}

  index1 := index;
  index2 := index + count - 1;
  while index1 < index2 do
  begin
    temp := values[index1];
    values[index1] := values[index2];
    values[index2] := temp;
    Inc(index1);
    Dec(index2);
  end;
end;

class procedure TArray.Shuffle<T>(var values: array of T);
begin
  Shuffle<T>(values, 0, Length(values));
end;

class procedure TArray.Shuffle<T>(var values: array of T; index: Integer);
begin
  Shuffle<T>(values, index, Length(values) - index);
end;

class procedure TArray.Shuffle<T>(var values: array of T; index,
  count: Integer);
var
  i: Integer;
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(values) - index), 'count');
{$ENDIF}

  while count > 1 do
  begin
    i := Random(count) + index;
    Dec(count);
    temp := values[index];
    values[index] := values[i];
    values[i] := temp;
    Inc(index);
  end;
end;

procedure SwapPtr(var left, right);
var
  temp: Pointer;
begin
  temp := Pointer(left);
  Pointer(left) := Pointer(right);
  Pointer(right) := temp;
end;

class function TArray.GetDepthLimit(count: Integer): Integer;
begin
  Result := 0;
  while count > 0 do
  begin
    Inc(Result);
    count := count div 2;
  end;
  Result := Result * 2;
end;

class procedure TArray.Swap<T>(var left, right: T);
var
  temp: T;
begin
{$IFDEF DELPHIXE7_UP} // XE7 and higher
  case GetTypeKind(T) of
{$IFDEF AUTOREFCOUNT}
    tkClass,
{$ENDIF AUTOREFCOUNT}
    tkInterface,
    tkDynArray,
    tkUString:
      SwapPtr(left, right);
  else
    temp := left;
    left := right;
    right := temp;
  end;
{$ELSE}
  temp := left;
  left := right;
  right := temp;
{$ENDIF}
end;

class procedure TArray.SortTwoItems<T>(const comparer: IComparer<T>;
  var left, right: T);
begin
  if comparer.Compare(left, right) > 0 then
    Swap<T>(left, right);
end;

class procedure TArray.SortThreeItems<T>(const comparer: IComparer<T>;
  var left, mid, right: T);
begin
  if comparer.Compare(left, mid) > 0 then
    Swap<T>(left, mid);
  if comparer.Compare(left, right) > 0 then
    Swap<T>(left, right);
  if comparer.Compare(mid, right) > 0 then
    Swap<T>(mid, right);
end;

class procedure TArray.DownHeap<T>(var values: array of T;
  const comparer: IComparer<T>; left, count, i: Integer);
var
  temp: T;
  child, n, x: Integer;
begin
  temp := values[left + i - 1];
  n := count div 2;
  while i <= n do
  begin
    child := i * 2;
    if (child < count) and (comparer.Compare(values[left + child - 1], values[left + child]) < 0) then
      Inc(child);
    if not comparer.Compare(temp, values[left + child - 1]) < 0 then
      Break;
    values[left + i - 1] := values[left + child - 1];
    i := child;
  end;
  values[left + i - 1] := temp;
end;

class procedure TArray.HeapSort<T>(var values: array of T;
  const comparer: IComparer<T>; left, right: Integer);
var
  count, i: Integer;
begin
  count := right - left + 1;
  for i := count div 2 downto 1 do
    DownHeap<T>(values, comparer, left, count, i);
  for i := count downto 2 do
  begin
    Swap<T>(values[left], values[left + i - 1]);
    DownHeap<T>(values, comparer, left, i - 1, 1);
  end;
end;

class procedure TArray.InsertionSort<T>(var values: array of T;
  const comparer: IComparer<T>; left, right: Integer);
var
  i, j: Integer;
  temp: T;
begin
  for i := left + 1 to right do
  begin
    j := i;
    temp := values[i];
    while (j > left) and (comparer.Compare(temp, values[j - 1]) < 0) do
    begin
      values[j] := values[j - 1];
      Dec(j);
    end;
    values[j] := temp;
  end;
end;

class function TArray.QuickSortPartition<T>(var values: array of T;
  const comparer: IComparer<T>; left, right: Integer): Integer;
var
  mid, pivotIndex: Integer;
  pivot: T;
begin
  mid := left + (right - left) div 2;

  SortThreeItems<T>(comparer, values[left], values[mid], values[right]);

  Dec(right);
  pivotIndex := right;

  pivot := values[mid];
  Swap<T>(values[mid], values[right]);

  while left < right do
  begin
    repeat
      Inc(left);
    until comparer.Compare(values[left], pivot) >= 0;
    repeat
      Dec(right);
    until comparer.Compare(pivot, values[right]) >= 0;

    if left >= right then
      Break;

    Swap<T>(values[left], values[right]);
  end;

  Swap<T>(values[left], values[pivotIndex]);
  Result := left;
end;

class procedure TArray.IntroSort<T>(var values: array of T;
  const comparer: IComparer<T>; left, right, depthLimit: Integer);
var
  count, pivot: Integer;
begin
  while right > left do
  begin
    count := right - left + 1;
    if count = 1 then
      Exit;
    if count = 2 then
    begin
      SortTwoItems<T>(comparer, values[left], values[right]);
      Exit;
    end;
    if count = 3 then
    begin
      SortThreeItems<T>(comparer, values[left], values[right - 1], values[right]);
      Exit;
    end;
    if count <= IntrosortSizeThreshold then
    begin
      InsertionSort<T>(values, comparer, left, right);
      Exit;
    end;

    if depthLimit = 0 then
    begin
      HeapSort<T>(values, comparer, left, right);
      Exit;
    end;

    Dec(depthLimit);
    pivot := QuickSortPartition<T>(values, comparer, left, right);
    IntroSort<T>(values, comparer, pivot + 1, right, depthLimit);
    right := pivot - 1;
  end;
end;

class procedure TArray.Sort<T>(var values: array of T);
begin
  IntroSort<T>(values, TComparer<T>.Default,
    Low(values), High(values), GetDepthLimit(Length(values)));
end;

class procedure TArray.Sort<T>(var values: array of T;
  const comparer: IComparer<T>);
begin
  IntroSort<T>(values, comparer,
    Low(values), High(values), GetDepthLimit(Length(values)));
end;

class procedure TArray.Sort<T>(var values: array of T;
  const comparer: IComparer<T>; index, count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(values) - index), 'count');
{$ENDIF}

  if count <= 1 then
    Exit;
  IntroSort<T>(values, comparer, index, index + count - 1, GetDepthLimit(count));
end;

class procedure TArray.Sort<T>(var values: array of T;
  const comparison: TComparison<T>);
begin
  IntroSort<T>(values, IComparer<T>(PPointer(@comparison)^),
    Low(values), High(values), GetDepthLimit(Length(values)));
end;

class procedure TArray.Sort<T>(var values: array of T;
  const comparison: TComparison<T>; index, count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparison), 'comparison');
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(values) - index), 'count');
{$ENDIF}

  if count <= 1 then
    Exit;
  IntroSort<T>(values, IComparer<T>(PPointer(@comparison)^),
    index, index + count - 1, GetDepthLimit(count));
end;

{$ENDREGION}


{$REGION 'Vector<T>'}

class function VectorHelper.InternalIndexOfInt8(const data: Pointer;
  const item: ShortInt): Integer;
begin
  for Result := 0 to High(TArray<ShortInt>(data)) do
    if TArray<ShortInt>(data)[Result] = item then
      Exit;
  Result := -1;
end;

class function VectorHelper.InternalIndexOfInt16(const data: Pointer;
  const item: SmallInt): Integer;
begin
  for Result := 0 to High(TArray<SmallInt>(data)) do
    if TArray<SmallInt>(data)[Result] = item then
      Exit;
  Result := -1;
end;

class function VectorHelper.InternalIndexOfInt32(const data:Pointer;
  const item: Integer): Integer;
begin
  for Result := 0 to High(TArray<Integer>(data)) do
    if TArray<Integer>(data)[Result] = item then
      Exit;
  Result := -1;
end;

class function VectorHelper.InternalIndexOfInt64(const data: Pointer;
  const item: Int64): Integer;
begin
  for Result := 0 to High(TArray<Int64>(data)) do
    if TArray<Int64>(data)[Result] = item then
      Exit;
  Result := -1;
end;

class function VectorHelper.InternalIndexOfStr(const data: Pointer;
  const item: string): Integer;
begin
  for Result := 0 to High(TArray<string>(data)) do
    if TArray<string>(data)[Result] = item then
      Exit;
  Result := -1;
end;

class operator Vector<T>.Add(const left, right: Vector<T>): Vector<T>;
begin
  Result := left;
  Result.Add(right.fData);
end;

class operator Vector<T>.Add(const left: Vector<T>;
  const right: TArray<T>): Vector<T>;
begin
  Result := left;
  Result.Add(right);
end;

class operator Vector<T>.Add(const left: TArray<T>;
  const right: Vector<T>): Vector<T>;
begin
  Result := left;
  Result.Add(right.fData);
end;

class operator Vector<T>.Add(const left: Vector<T>;
  const right: T): Vector<T>;
begin
  Result := left;
  Result.Add(right);
end;

class operator Vector<T>.Add(const left: T;
  const right: Vector<T>): Vector<T>;
begin
  SetLength(Result.fData, 1);
  Result.fData[0] := left;
  Result.Add(right);
end;

function Vector<T>.Add(const item: T): Integer;
begin
  Result := System.Length(fData);
  SetLength(fData, Result + 1);
  fData[Result] := item;
end;

procedure Vector<T>.Add(const items: array of T);
begin
  InternalInsert(System.Length(fData), items);
end;

procedure Vector<T>.Add(const items: TArray<T>);
begin
{$IFNDEF DELPHIXE7_UP}
  InternalInsert(System.Length(fData), items);
{$ELSE}
  System.Insert(items, fData, System.Length(fData));
{$ENDIF}
end;

procedure Vector<T>.Add(const items: Vector<T>);
begin
{$IFNDEF DELPHIXE7_UP}
  InternalInsert(System.Length(items.fData), items.fData);
{$ELSE}
  System.Insert(items.fData, fData, System.Length(items.fData));
{$ENDIF}
end;

procedure Vector<T>.Assign(const items: array of T);
begin
  fData := TArray.Copy<T>(items);
end;

procedure Vector<T>.Clear;
begin
  fData := nil;
end;

function Vector<T>.IndexOf(const item: T): Integer;
begin
  case TType.Kind<T> of
    tkInteger:
      case SizeOf(T) of
        1: Result := VectorHelper.InternalIndexOfInt8(fData, PShortInt(@item)^);
        2: Result := VectorHelper.InternalIndexOfInt16(fData, PSmallInt(@item)^);
        4: Result := VectorHelper.InternalIndexOfInt32(fData, PInteger(@item)^);
      end;
    tkInt64: Result := VectorHelper.InternalIndexOfInt64(fData, PInt64(@item)^);
    tkUString: Result := VectorHelper.InternalIndexOfStr(fData, PUnicodeString(@item)^);
  else
    Result := InternalIndexOf(item);
  end;
end;

function Vector<T>.Contains(const item: T): Boolean;
begin
  Result := IndexOf(item) > -1;
end;

function Vector<T>.Contains(const item: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(fData) do
    if comparer.Equals(fData[i], item) then
      Exit(True);
  Result := False;
end;

function Vector<T>.Contains(const item: T;
  const comparer: TEqualityComparison<T>): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(fData) do
    if comparer(fData[i], item) then
      Exit(True);
  Result := False;
end;

function Vector<T>.Contains(const items: array of T): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(items) do
    if IndexOf(items[i]) = -1 then
      Exit(False);
  Result := True;
end;

function Vector<T>.Contains(const items: TArray<T>): Boolean;
var
  i: Integer;
begin
  for i := 0 to System.Length(items) - 1 do
    if IndexOf(items[i]) = -1 then
      Exit(False);
  Result := True;
end;

procedure Vector<T>.Delete(index: Integer);
{$IFNDEF DELPHIXE7_UP}
var
  n, i: Integer;
{$ENDIF}
begin
{$IFNDEF DELPHIXE7_UP}
  n := System.Length(fData);
  if (index < 0) or (index >= n) then
    Exit;
  Dec(n);
  fData[index] := Default(T);
  if index <> n then
{$IFDEF WEAKREF}
    if TType.HasWeakRef<T> then
    begin
      for i := index to n - 1 do
        fData[i] := fData[i + 1];
    end
    else
{$ENDIF}
    begin
      System.Move(fData[index + 1], fData[index], (n - index) * SizeOf(T));
      System.FillChar(fData[n], SizeOf(T), 0);
    end;
  SetLength(fData, n);
{$ELSE}
  System.Delete(fData, index, 1);
{$ENDIF}
end;

procedure Vector<T>.Delete(index, count: Integer);
{$IFNDEF DELPHIXE7_UP}
var
  n, i: Integer;
{$ENDIF}
begin
{$IFNDEF DELPHIXE7_UP}
  n := System.Length(fData);
  if (index < 0) or (index >= n) then
    Exit;
  if count > n - index then
    count := n - index;
  Dec(n, count);
  for i := index to index + count - 1 do
    fData[i] := Default(T);
  if index <> n then
{$IFDEF WEAKREF}
    if TType.HasWeakRef<T> then
    begin
      for i := index to n - count do
        fData[i] := fData[i + count];
    end
    else
{$ENDIF}
    begin
      System.Move(fData[index + count], fData[index], (n - index) * SizeOf(T));
      System.FillChar(fData[n], count * SizeOf(T), 0);
    end;
  SetLength(fData, n);
{$ELSE}
  System.Delete(fData, index, count);
{$ENDIF}
end;

class operator Vector<T>.Equal(const left, right: Vector<T>): Boolean;
begin
  Result := left.Equals(right.fData);
end;

function Vector<T>.Equals(const items: array of T): Boolean;
var
  n, i: Integer;
begin
  n := System.Length(fData);
  if n <> System.Length(items) then
    Exit(False);
  Result := True;
  case TType.Kind<T> of
    tkInteger:
      for i := 0 to n - 1 do
        if PInteger(@fData[i])^ <> PInteger(@items[i])^ then
          Exit(False);
    tkUString:
      for i := 0 to n - 1 do
        if PUnicodeString(@fData[i])^ <> PUnicodeString(@items[i])^ then
          Exit(False);
  else
    Result := InternalEquals(items);
  end;
end;

function Vector<T>.Equals(const items: TArray<T>): Boolean;
var
  n, i: Integer;
begin
  n := System.Length(fData);
  if n <> System.Length(items) then
    Exit(False);
  Result := True;
  case TType.Kind<T> of
    tkInteger:
      for i := 0 to n - 1 do
        if PInteger(@fData[i])^ <> PInteger(@items[i])^ then
          Exit(False);
    tkUString:
      for i := 0 to n - 1 do
        if PUnicodeString(@fData[i])^ <> PUnicodeString(@items[i])^ then
          Exit(False);
  else
    Result := InternalEquals(items);
  end;
end;

procedure Vector<T>.ForEach(const action: TAction<T>);
var
  i: Integer;
begin
  for i := Low(fData) to High(fData) do
    action(fData[i]);
end;

function Vector<T>.GetCount: Integer;
begin
  Result := System.Length(fData);
end;

function Vector<T>.GetEnumerator: TArrayEnumerator<T>;
begin
{$IFDEF DELPHI2010}
  Result := TArrayEnumerator<T>.Create(fData);
{$ELSE}
  Result.fItems := fData;
  Result.fIndex := -1;
{$ENDIF}
end;

function Vector<T>.GetFirst: T;
begin
  Result := fData[0];
end;

function Vector<T>.GetItem(index: Integer): T;
begin
  Result := fData[index];
end;

function Vector<T>.GetLast: T;
begin
  Result := fData[High(fData)];
end;

class operator Vector<T>.Implicit(const value: TArray<T>): Vector<T>;
begin
  Result.fData := value;
end;

class operator Vector<T>.Implicit(const value: Vector<T>): TArray<T>;
begin
  Result := value.fData;
end;

class operator Vector<T>.In(const left: T;
  const right: Vector<T>): Boolean;
begin
  Result := right.Contains(left);
end;

class operator Vector<T>.In(const left, right: Vector<T>): Boolean;
begin
  Result := right.Contains(left.fData);
end;

class operator Vector<T>.In(const left: TArray<T>;
  const right: Vector<T>): Boolean;
begin
  Result := right.Contains(left);
end;

procedure Vector<T>.Insert(index: Integer; const item: T);
{$IFNDEF DELPHIXE7_UP}
var
  count: Integer;
  i: Integer;
{$ENDIF}
begin
{$IFNDEF DELPHIXE7_UP}
  count := System.Length(fData);
  SetLength(fData, count + 1);
  if index <> count then
{$IFDEF WEAKREF}
    if TType.HasWeakRef<T> then
    begin
      for i := count - 1 downto index do
        fData[i + 1] := fData[i];
    end
    else
{$ENDIF}
    begin
      System.Move(fData[index], fData[index + 1], (count - index) * SizeOf(T));
      System.FillChar(fData[index], SizeOf(T), 0);
    end;
  fData[index] := item;
{$ELSE}
  System.Insert(item, fData, index);
{$ENDIF}
end;

procedure Vector<T>.Insert(index: Integer; const items: array of T);
begin
  InternalInsert(index, items);
end;

procedure Vector<T>.Insert(index: Integer; const items: TArray<T>);
begin
{$IFNDEF DELPHIXE7_UP}
  InternalInsert(index, items);
{$ELSE}
  System.Insert(items, fData, index);
{$ENDIF}
end;

function Vector<T>.InternalEquals(const items: array of T): Boolean;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := TEqualityComparer<T>.Default;
  for i := 0 to System.Length(fData) - 1 do
    if not comparer.Equals(fData[i], items[i]) then
      Exit(False);
  Result := True;
end;

function Vector<T>.InternalIndexOf(const item: T): Integer;
var
  comparer: IEqualityComparer<T>;
begin
  comparer := TEqualityComparer<T>.Default;
  for Result := 0 to High(fData) do
    if comparer.Equals(fData[Result], item) then
      Exit;
  Result := -1;
end;

procedure Vector<T>.InternalInsert(index: Integer; const items: array of T);
var
  count, len, i: Integer;
begin
  count := System.Length(fData);
  len := System.Length(items);
  SetLength(fData, count + len);
  if index <> count then
{$IFDEF WEAKREF}
    if TType.HasWeakRef<T> then
    begin
      for i := count - 1 downto index do
        fData[i + len] := fData[i];
    end
    else
{$ENDIF}
    begin
      System.Move(fData[index], fData[index + len], (count - index) * SizeOf(T));
      if TType.IsManaged<T> then
        System.FillChar(fData[index], len * SizeOf(T), 0);
    end;
  if TType.IsManaged<T> then
  begin
    for i := Low(items) to High(items) do
    begin
      fData[index] := items[i];
      Inc(index);
    end;
  end
  else
    System.Move(items[0], fData[index], len * SizeOf(T));
end;

class operator Vector<T>.NotEqual(const left, right: Vector<T>): Boolean;
begin
  Result := not left.Equals(right.fData);
end;

function Vector<T>.Remove: T;
var
  n: Integer;
begin
  n := High(fData);
  Result := fData[n];
  SetLength(fData, n);
end;

procedure Vector<T>.Remove(const item: T);
var
  index: Integer;
begin
  index := IndexOf(item);
  if index > -1 then
    Delete(index);
end;

procedure Vector<T>.Remove(const items: array of T);
var
  i, index: Integer;
begin
  for i := Low(items) to High(items) do
  begin
    index := IndexOf(items[i]);
    if index > -1 then
      Delete(index);
  end;
end;

procedure Vector<T>.Remove(const items: TArray<T>);
var
  i, index: Integer;
begin
  for i := 0 to System.Length(items) - 1 do
  begin
    index := IndexOf(items[i]);
    if index > -1 then
      Delete(index);
  end;
end;

procedure Vector<T>.Reverse;
var
  tmp: T;
  b, e: Integer;
begin
  b := 0;
  e := Count - 1;
  while b < e do
  begin
    tmp := fData[b];
    fData[b] := fData[e];
    fData[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure Vector<T>.SetCount(value: Integer);
begin
  SetLength(fData, value);
end;

procedure Vector<T>.SetItem(index: Integer; const value: T);
begin
  fData[index] := value;
end;

function Vector<T>.Slice(index: Integer): Vector<T>;
begin
  Result.fData := Copy(fData, index);
end;

function Vector<T>.Slice(index, count: Integer): Vector<T>;
begin
  Result.fData := Copy(fData, index, count);
end;

procedure Vector<T>.Sort;
begin
  TArray.Sort<T>(fData);
end;

procedure Vector<T>.Sort(const comparer: IComparer<T>);
begin
  TArray.Sort<T>(fData, comparer);
end;

procedure Vector<T>.Sort(const comparer: TComparison<T>);
begin
  TArray.Sort<T>(fData, IComparer<T>(PPointer(@comparer)^));
end;

function Vector<T>.Splice(index, count: Integer): Vector<T>;
begin
  Result := Splice(index, count, []);
end;

function Vector<T>.Splice(index, count: Integer;
  const items: array of T): Vector<T>;
var
  i: Integer;
begin
  i := System.Length(fData);
  if (index < 0) or (index >= i) then
    Exit;
  if count > i - index then
    count := i - index;
  Result.fData := Copy(fData, index, count);
  Delete(index, count);
  Insert(index, items);
end;

class operator Vector<T>.Subtract(const left,
  right: Vector<T>): Vector<T>;
begin
  Result := left;
  Result.Remove(right.fData);
end;

class operator Vector<T>.Subtract(const left: Vector<T>;
  const right: T): Vector<T>;
begin
  Result := left;
  Result.Remove(right);
end;

{$ENDREGION}


{$REGION 'TArrayEnumerator<T>' }

constructor TArrayEnumerator<T>.Create(const items: TArray<T>);
begin
  fItems := items;
  fIndex := -1;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := fItems[fIndex];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex < System.Length(fItems);
end;

{$ENDREGION}


{$REGION 'TFormatSettingsHelper'}

{$IFDEF DELPHI2010}
class function TFormatSettingsHelper.Create: TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, Result);
end;
{$ENDIF}

{$ENDREGION}


procedure Init;
begin
{$IFDEF DELPHI2010}
  Nop_Instance := Pointer(TValueData(TValue.Empty).FHeapData);
{$ELSE}
  Nop_Instance := Pointer(TValueData(TValue.Empty).FValueData);
{$ENDIF}
end;

initialization
  Init;

finalization
  // make sure this properly gets freed because it appears
  // the class destructor is not running all the time
  TType.fContext.Free;

end.
