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

/// <summary>
///   Declares the fundamental types for the <see href="http://www.spring4d.org">
///   Spring4D</see> Framework.
/// </summary>
unit Spring;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}
{$DEFINE FIELD_RTTI}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  Diagnostics,
  Generics.Collections,
  Generics.Defaults,
  Rtti,
  SyncObjs,
  SysUtils,
  TimeSpan,
  Types,
  TypInfo;


  {$REGION 'Interface helper routines'}

function NopRef(inst: Pointer): Integer; stdcall;
function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
function RecAddRef(inst: Pointer): Integer; stdcall;
procedure IntfAssign(const source: IInterface; var target: IInterface);

  {$ENDREGION}


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

{$IFNDEF DELPHIXE2_UP}
  IntPtr = NativeInt;
  UIntPtr = NativeUInt;

  SIZE_T = ULONG_PTR;
{$ENDIF}

{$IFNDEF DELPHIXE3_UP}
  TSymbolName = ShortString;
{$ENDIF}

  PObject = ^TObject;
  {$POINTERMATH ON}
  PVTable = ^Pointer;
  {$POINTERMATH OFF}
  PPVTable = ^PVTable;

{$IFNDEF DELPHIXE3_UP}
  PMethod = ^TMethod;
{$ENDIF}
  PMethodPointer = ^TMethodPointer;
  TMethodPointer = procedure of object;

  PInt8 = ^Int8;
  PInt16 = ^Int16;
  PInt32 = ^Int32;
  PInt64 = ^Int64;

{$POINTERMATH ON}
  PMethodArray = ^TMethod;
{$POINTERMATH OFF}

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

    class function HasWeakRef<T>: Boolean; static; inline;
    class function IsManaged<T>: Boolean; static; inline;
    class function Kind<T>: TTypeKind; static; inline;

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

  // simple multiple read exclusive write lock using busy wait
  // taken from OTL with permission of Primoz Gabrijelcic
  ReadWriteLock = record
  strict private
    Lock: IInterface;
    ThreadId: IInterface;
  public
    procedure EnterRead;
    procedure LeaveRead;
    procedure EnterWrite;
    procedure LeaveWrite;
    procedure EnterUpgradableRead;
    procedure LeaveUpgradableRead;
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
    class var CacheLock: ReadWriteLock;
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

  TFieldInitializer = reference to procedure(typeInfo: PTypeInfo; var value);

  /// <summary>
  ///   This attribute marks automatically initialized interface or object
  ///   fields inside of classes that inherit from TManagedObject or are using
  ///   the mechanism provided by TInitTable.
  /// </summary>
  ManagedAttribute = class(TBaseAttribute)
  strict private
    fCreateInstance: Boolean;
    fInstanceClass: TClass;
    fInitializer: TFieldInitializer;
  strict protected
    constructor Create(const initializer: TFieldInitializer); overload;
  public
    constructor Create(createInstance: Boolean = True); overload;
    constructor Create(instanceClass: TClass) overload;

    property CreateInstance: Boolean read fCreateInstance;
    property InstanceClass: TClass read fInstanceClass;
    property Initializer: TFieldInitializer read fInitializer;
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

    TInitializerField = class(TFinalizableField)
    private
      fOffset: Integer;
      fFieldType: PTypeInfo;
      fInitializer: TFieldInitializer;
    public
      constructor Create(offset: Integer; fieldType: PTypeInfo;
        const initializer: TFieldInitializer);
      procedure InitializeValue(instance: Pointer); override;
      procedure FinalizeValue(instance: Pointer); override;
    end;

    TManagedObjectField = class(TInitializerField)
    private
      fCls: TClass;
      fCtor: TConstructor;
    public
      constructor Create(offset: Integer; fieldType: PTypeInfo;
        const initializer: TFieldInitializer; cls: TClass);
      procedure InitializeValue(instance: Pointer); override;
      procedure FinalizeValue(instance: Pointer); override;
    end;

    TManagedInterfaceField = class(TManagedObjectField)
    private
      fEntry: PInterfaceEntry;
      function CreateInstance: Pointer;
    public
      constructor Create(offset: Integer; fieldType: PTypeInfo;
        const initializer: TFieldInitializer; cls: TClass; entry: PInterfaceEntry);
      procedure InitializeValue(instance: Pointer); override;
      procedure FinalizeValue(instance: Pointer); override;
    end;

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
    procedure AddDefaultField(fieldType: PTypeInfo; const value: Variant; offset: Integer);
    procedure AddDefaultProperty(fieldType: PTypeInfo; const value: Variant; propInfo: PPropInfo);
    procedure AddManagedField(const field: TRttiField; const attribute: ManagedAttribute);
    class function GetCodePointer(instance: TObject; p: Pointer): Pointer; static; inline;
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create(classType: TClass);
    destructor Destroy; override;

    procedure InitInstance(instance: Pointer);
    procedure CleanupInstance(instance: Pointer);
  end;

  {$ENDREGION}


  {$REGION 'TManagedObject'}

  TManagedObject = class(TObject)
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

  TManagedInterfacedObject = class(TInterfacedObject)
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

  {$ENDREGION}


  {$REGION 'TDictionaryOwnerships}

  /// <summary>
  ///   Controls the ownership of objects stored in any associative collection.
  /// </summary>
  TDictionaryOwnerships = set of (
    /// <summary>
    ///   Objects stored as key are being destroyed when removed
    /// </summary>
    doOwnsKeys,

    /// <summary>
    ///   Objects stored as value are being destroyed when removed
    /// </summary>
    doOwnsValues
  );

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
    caReset,

    /// <summary>
    ///   An item in the collection was changed.
    /// </summary>
    caChanged
  );

  {$ENDREGION}


  {$REGION 'TValueHelper'}

  TValueConverterCallback = function (const value: TValue;
    const targetTypeInfo: PTypeInfo; var targetValue: TValue;
    const parameter: TValue): Boolean;

  TValueHelper = record helper for TValue
  private class var
    ConvertSettings: TFormatSettings;
    fValueConverterCallback: TValueConverterCallback;
  private
    procedure Init(typeInfo: Pointer);
    function GetTypeKind: TTypeKind;
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

{$IFNDEF DELPHIX_ALEXANDRIA_UP}
    class function &&op_Implicit(value: TDateTime): TValue; overload; static; inline;
    class function &&op_Implicit(value: TDate): TValue; overload; static; inline;
    class function &&op_Implicit(value: TTime): TValue; overload; static; inline;
{$ENDIF}

    class function &&op_Implicit(const value: TGUID): TValue; overload; static;

    class function From(const value; typeInfo: PTypeInfo): TValue; overload; static;
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

    /// <summary>
    ///   Casts the currently stored value to another type.
    /// </summary>
    /// <remarks>
    ///   This method fixes the missing interface cast support of
    ///   TValue.AsType&lt;T&gt;.
    /// </remarks>
    function AsType<T>: T; overload;

    procedure AsType(typeInfo: PTypeInfo; out target); overload;

    /// <summary>
    ///   Casts the currently stored value to another type.
    /// </summary>
    /// <remarks>
    ///   This method uses an equality check on the typeInfo to support types
    ///   across binary boundaries
    /// </remarks>
    procedure AsTypeRelaxed(typeInfo: PTypeInfo; out target);

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
    ///   Checks whether the stored value is a boolean type.
    /// </summary>
    function IsBoolean: Boolean;

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

    function IsType(ATypeInfo: PTypeInfo): Boolean; overload;

    /// <summary>
    ///   Checks whether the stored value is a <c>Variant</c>.
    /// </summary>
    function IsVariant: Boolean;

    /// <summary>
    ///   Sets the stored value of a nullable.
    /// </summary>
    procedure SetNullableValue(const value: TValue);

    function TryAsType(typeInfo: PTypeInfo; out target): Boolean; overload;

    function TryCast(ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;

    /// <summary>
    ///   Tries to convert the stored value. Returns false when the conversion
    ///   is not possible.
    /// </summary>
    function TryConvert(targetType: PTypeInfo; var targetValue: TValue): Boolean; overload;

    /// <summary>
    ///   Tries to convert the stored value using the specified format
    ///   settings. Returns false when the conversion is not possible.
    /// </summary>
    function TryConvert(targetType: PTypeInfo; var targetValue: TValue;
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
    ///   Update the internal TFormatSettings record.
    /// </summary>
    /// <remarks>
    ///   This is not thread-safe and should only be used if the settings of
    ///   the operating system have been changed.
    /// </remarks>
    class procedure UpdateFormatSettings; static;

    /// <summary>
    ///   Specifies the type kind of the stored value.
    /// </summary>
    /// <remarks>
    ///   This fixes the issue with returning <c>tkUnknown</c> when the stored
    ///   value is an empty reference type (RSP-10071).
    /// </remarks>
    property Kind: TTypeKind read GetTypeKind;

    /// <summary>
    ///   Returns the TRttiType of the stored value.
    /// </summary>
    property ValueType: TRttiType read GetValueType;

    class property ValueConverterCallback: TValueConverterCallback
      read fValueConverterCallback write fValueConverterCallback;
  end;

  {$ENDREGION}


  {$REGION 'TRttiMethodHelper'}

{$IFNDEF DELPHIX_BERLIN_UP}
  {$HINTS OFF}
  TRttiMethodHack = class(TRttiMethod)
  private
    function GetParameters: TArray<TRttiParameter>; override;
  end;
  {$HINTS ON}
{$ENDIF}

  TRttiMethodHelper = class helper for TRttiMethod
  private
    function GetIsAbstract: Boolean;
    function GetReturnTypeHandle: PTypeInfo;
{$IFNDEF DELPHIX_BERLIN_UP}
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
{$ENDIF}
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

  TMethodImplementationHelper = class helper for TMethodImplementation
  public
    function AsMethod: TMethod;
  end;

  {$ENDREGION}


  {$REGION 'Procedure types'}

  {$M+}
  /// <summary>
  ///   Encapsulates a method that has no parameters and does not return a
  ///   value.
  /// </summary>
  Action = reference to procedure;

  /// <summary>
  ///   Encapsulates a method that has a single parameter and does not return a
  ///   value.
  /// </summary>
  Action<T> = reference to procedure(const arg: T);

  /// <summary>
  ///   Encapsulates a method that has two parameters and does not return a
  ///   value.
  /// </summary>
  Action<T1, T2> = reference to procedure(const arg1: T1; const arg2: T2);

  /// <summary>
  ///   Encapsulates a method that has three parameters and does not return a
  ///   value.
  /// </summary>
  Action<T1, T2, T3> = reference to procedure(const arg1: T1; const arg2: T2; const arg3: T3);

  /// <summary>
  ///   Encapsulates a method that has four parameters and does not return a
  ///   value.
  /// </summary>
  Action<T1, T2, T3, T4> = reference to procedure(const arg1: T1; const arg2: T2; const arg3: T3; const arg4: T4);

  /// <summary>
  ///   Encapsulates a method that has no parameters and returns a value of the
  ///   type specified by the <i>TResult</i> parameter.
  /// </summary>
  Func<TResult> = reference to function: TResult;

  /// <summary>
  ///   Encapsulates a method that has one parameter and returns a value of the
  ///   type specified by the <i>TResult</i> parameter.
  /// </summary>
  Func<T, TResult> = reference to function(const arg: T): TResult;

  /// <summary>
  ///   Encapsulates a method that has two parameters and returns a value of
  ///   the type specified by the <i>TResult</i> parameter.
  /// </summary>
  Func<T1, T2, TResult> = reference to function(const arg1: T1; const arg2: T2): TResult;

  /// <summary>
  ///   Encapsulates a method that has three parameters and returns a value of
  ///   the type specified by the <i>TResult</i> parameter.
  /// </summary>
  Func<T1, T2, T3, TResult> = reference to function(const arg1: T1; const arg2: T2; const arg3: T3): TResult;

  /// <summary>
  ///   Encapsulates a method that has four parameters and returns a value of
  ///   the type specified by the <i>TResult</i> parameter.
  /// </summary>
  Func<T1, T2, T3, T4, TResult> = reference to function(const arg1: T1; const arg2: T2; const arg3: T3; const arg4: T4): TResult;

  /// <summary>
  ///   Represents the method that defines a set of criteria and determines
  ///   whether the specified object meets those criteria.
  /// </summary>
  Predicate<T> = reference to function(const arg: T): Boolean;
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

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

  IEvent = interface
    ['{CFC14C4D-F559-4A46-A5B1-3145E9B182D8}']
  {$REGION 'Property Accessors'}
    function GetCanInvoke: Boolean;
    function GetEnabled: Boolean;
    function GetOnChanged: TNotifyEvent;
    function GetUseFreeNotification: Boolean;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
    procedure SetUseFreeNotification(const value: Boolean);
  {$ENDREGION}

    procedure Add(const handler: TMethod);
    procedure Remove(const handler: TMethod);

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

    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;

    /// <summary>
    ///   Specifies if the event internally tracks if the event handlers are
    ///   implemented by a TComponent descendant and automatically unsubscribes
    ///   those when the implementing component is being destroyed.
    /// </summary>
    property UseFreeNotification: Boolean read GetUseFreeNotification write SetUseFreeNotification;
  end;

  /// <summary>
  ///   Represents a multicast event that provides adding and removing event
  ///   handlers.
  /// </summary>
  /// <typeparam name="T">
  ///   The event handler type must be an instance procedural type such as
  ///   TNotifyEvent.
  /// </typeparam>
  IEvent<T> = interface(IEvent)

    /// <summary>
    ///   Adds an event handler to the list.
    /// </summary>
    procedure Add(handler: T);

    /// <summary>
    ///   Removes an event handler if it was added to the event.
    /// </summary>
    procedure Remove(handler: T);

{$IFDEF INVOKABLE_EVENT}
    function GetInvoke: T;

    /// <summary>
    ///   Invokes all event handlers.
    /// </summary>
    property Invoke: T read GetInvoke;
{$ENDIF}
  end;

  /// <summary>
  ///   Represents a multicast event that can be invoked.
  /// </summary>
  IInvokableEvent<T> = interface(IEvent<T>)
{$IFNDEF INVOKABLE_EVENT}
  {$REGION 'Property Accessors'}
    function GetInvoke: T;
  {$ENDREGION}

    /// <summary>
    ///   Invokes all event handlers.
    /// </summary>
    property Invoke: T read GetInvoke;
{$ENDIF}
  end;

  Event<T> = record
  private
    fInstance: IInvokableEvent<T>;
    function GetCanInvoke: Boolean;
    function GetEnabled: Boolean;
    function GetInvoke: T;
    function GetOnChanged: TNotifyEvent;
    function GetUseFreeNotification: Boolean;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
    procedure SetUseFreeNotification(const value: Boolean);
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

    class operator Implicit(const value: IInvokableEvent<T>): Event<T>;
    class operator Implicit(var value: Event<T>): IInvokableEvent<T>;
    class operator Implicit(var value: Event<T>): T;
  end;

  INotifyEvent = IEvent<TNotifyEvent>;

  INotifyEvent<T> = interface(IEvent<TNotifyEvent<T>>)
  end;

  IInvokableNotifyEvent = interface(INotifyEvent) //FI:W523
    function GetInvoke: TNotifyEvent;
    property Invoke: TNotifyEvent read GetInvoke;
  end;

  IInvokableNotifyEvent<T> = interface(INotifyEvent<T>)
    function GetInvoke: TNotifyEvent<T>;
    property Invoke: TNotifyEvent<T> read GetInvoke;
  end;

  {$RTTI INHERIT
      METHODS(DefaultMethodRttiVisibility)
      FIELDS(DefaultFieldRttiVisibility)
      PROPERTIES(DefaultPropertyRttiVisibility)}

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

  /// <summary>
  ///   Supports a simple iteration over a non-generic collection.
  /// </summary>
  IEnumerator = interface(IInvokable)
    ['{A2AD52DC-FA9F-4121-9B54-5C427DA5E62C}']
  {$REGION 'Property Accessors'}
    function GetCurrent: TValue;
  {$ENDREGION}

    /// <summary>
    ///   Advances the enumerator to the next element of the collection.
    /// </summary>
    /// <returns>
    ///   <b>True</b> if the enumerator was successfully advanced to the next
    ///   element; <b>False</b> if the enumerator has passed the end of the
    ///   collection.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The collection was modified after the enumerator was created.
    /// </exception>
    function MoveNext: Boolean;

    /// <summary>
    ///   Gets the current element in the collection.
    /// </summary>
    /// <value>
    ///   The current element in the collection.
    /// </value>
    property Current: TValue read GetCurrent;
  end;

  /// <summary>
  ///   Exposes an enumerator, which supports a simple iteration over a
  ///   non-generic collection.
  /// </summary>
  IEnumerable = interface(IInvokable)
    ['{6BC97F33-C0A8-4770-8E1C-C2017527B7E7}']

    /// <summary>
    ///   Returns an enumerator that iterates through a collection.
    /// </summary>
    /// <returns>
    ///   An <see cref="IEnumerator" /> object that can be used to iterate
    ///   through the collection.
    /// </returns>
    function GetEnumerator: IEnumerator;

  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
    function GetIsEmpty: Boolean;

    /// <summary>
    ///   Attempts to retrieve the count without calling the enumerator; returns
    ///   -1 otherwise.
    /// </summary>
    /// <remarks>
    ///   This method is primarily for internal use to provide count based
    ///   results as efficient as possible.
    /// </remarks>
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}

    /// <summary>
    ///   Returns the reference to this instance.
    /// </summary>
    /// <returns>
    ///   The <see cref="TObject" /> instance behind this IEnumerable
    ///   reference.
    /// </returns>
    function AsObject: TObject;

    /// <summary>
    ///   Returns the number of elements in a sequence.
    /// </summary>
    /// <value>
    ///   The number of elements in the sequence.
    /// </value>
    property Count: Integer read GetCount;

    /// <summary>
    ///   Returns the type of the elements in the sequence.
    /// </summary>
    /// <value>
    ///   The type of the elements in the sequence.
    /// </value>
    property ElementType: PTypeInfo read GetElementType;

    /// <summary>
    ///   Determines whether the sequence contains no elements.
    /// </summary>
    /// <value>
    ///   <b>True</b> if the source sequence contains no elements; otherwise, <b>
    ///   False</b>.
    /// </value>
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


  {$REGION 'TRefCountedObject'}
  /// <summary>
  ///   Provides an abstract implementation for any interface implementing
  ///   class without already implementing IInterface to avoid unnecessary
  ///   waste of instance space for its VMT. If you inherit from this class you
  ///   implement an interface that is not IInterface and thus would just waste
  ///   this space.
  /// </summary>
  TRefCountedObject = class abstract
  private const
    objDestroyingFlag = Integer($80000000);
    function GetRefCount: Integer; inline;
  protected
{$IF Declared(VolatileAttribute)}
    [Volatile]
{$IFEND}
    fRefCount: Integer;
    function AsObject: TObject;
  public
    function QueryInterface(const IID: TGUID; out obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetInterface(const IID: TGUID; out Obj): Boolean;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read GetRefCount;
  end;

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
    class procedure CheckRange(const s: WideString; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: WideString; index, count: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; index, count: Integer); overload; static; inline;
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
    ///   Raises an <see cref="EArgumentNilException" /> exception.
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

    class function RaiseInvalidTypeCast(sourceType, targetType: PTypeInfo): Boolean; static;
  end;

  TArgument = Guard deprecated 'Use Guard instead';

  {$ENDREGION}


  {$REGION 'RaiseHelper'}

  {$SCOPEDENUMS ON}
  ExceptionArgument = (
    action,
    capacity,
    collection,
    collectionSelector,
    comparer,
    count,
    elementSelector,
    first,
    func,
    index,
    index1,
    index2,
    inner,
    innerKeySelector,
    items,
    keySelector,
    match,
    max,
    min,
    other,
    outer,
    outerKeySelector,
    predicate,
    resultSelector,
    second,
    selector,
    size,
    sorter,
    source,
    sourceIndex,
    targetIndex,
    value,
    values
  );

  ExceptionResource = (
    ArgumentOutOfRange_Capacity,
    ArgumentOutOfRange_Count,
    ArgumentOutOfRange_Index,
    ArgumentOutOfRange_NeedNonNegNum,
    Argument_InvalidIndexCount
  );
  {$SCOPEDENUMS OFF}

  RaiseHelper = record
  private
    class function GetArgumentName(argument: ExceptionArgument): string; static;
    class function GetResourceString(resource: ExceptionResource): string; static;

    class function GetArgumentOutOfRangeException(argument: ExceptionArgument; resource: ExceptionResource): EArgumentException; overload; static;
    class function GetArgumentOutOfRangeException(resource: ExceptionResource): EArgumentException; overload; static;
  public
    class procedure ArgumentNil(argument: ExceptionArgument); static;
    class function ArgumentOutOfRange(argument: ExceptionArgument): NativeInt; overload; static;
    class function ArgumentOutOfRange(argument: ExceptionArgument; resource: ExceptionResource): NativeInt; overload; static;
    class function ArgumentOutOfRange(resource: ExceptionResource): NativeInt; overload; static;

    class function ArgumentOutOfRange_Count: NativeInt; static;
    class function ArgumentOutOfRange_Index: NativeInt; static;

    class procedure DuplicateKey; static;
    class procedure KeyNotFound; static;
    class procedure MoreThanOneElement; static;
    class procedure MoreThanOneMatch; static;
    class procedure NoClassType(t: PTypeInfo); static;
    class procedure NoElements; static;
    class procedure NoMatch; static;
    class procedure NotSupported; static;

    class function EnumFailedVersion: Boolean; static;
  end;

  {$ENDREGION}


  {$REGION 'Nullable Types'}

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(DefaultFieldRttiVisibility)}{$ENDIF}

  Nullable = record
  private
    class var HasValue: string;
    type Null = interface end;
  public
    {$IFNDEF NULLABLE_CMR}
    class constructor Create;
    {$ENDIF}
  end;

  /// <summary>
  ///   A nullable type can represent the normal range of values for its
  ///   underlying value type, plus an additional <c>Null</c> value.
  /// </summary>
  /// <typeparam name="T">
  ///   The underlying value type of the <see cref="Nullable&lt;T&gt;" />
  ///   generic type.
  /// </typeparam>
  Nullable<T> = {$IFDEF NULLABLE_PACKED}packed {$ENDIF}record
  private
    fValue: T;
    fHasValue: {$IFNDEF NULLABLE_CMR}string{$ELSE}Boolean{$ENDIF};
    class var
      fComparer: Pointer;
      fEquals: function(const left, right: T): Boolean;
    class function EqualsComparer(const left, right: T): Boolean; static;
    class function EqualsInternal(const left, right: T): Boolean; static; inline;
    class procedure InitEquals; static;
    function GetValue: T;
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

{$IFDEF NULLABLE_CMR}
    class operator Initialize(out value: Nullable<T>);
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
  TNullableAnsiString = Nullable<AnsiString>;
  TNullableWideString = Nullable<WideString>;
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
    fHasValueKind: TTypeKind;
  public
    constructor Create(typeInfo: PTypeInfo);
    function GetValue(instance: Pointer): TValue; inline;
    function HasValue(instance: Pointer): Boolean; inline;
    procedure SetValue(instance: Pointer; const value: TValue);
    property ValueType: PTypeInfo read fValueType;
  end;

  {$RTTI INHERIT
      METHODS(DefaultMethodRttiVisibility)
      FIELDS(DefaultFieldRttiVisibility)
      PROPERTIES(DefaultPropertyRttiVisibility)}

  {$ENDREGION}


  {$REGION 'Lazy Initialization'}

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(DefaultFieldRttiVisibility)}{$ENDIF}

  /// <summary>
  ///   Specifies the kind of a lazy type.
  /// </summary>
  TLazyKind = (

    /// <summary>
    ///   Not a lazy type.
    /// </summary>
    lkNone,

    /// <summary>
    ///   Type is <see cref="Spring|Func&lt;T&gt;" />.
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
  ILazy = interface
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
  {$M+}
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
  {$M-}

  /// <summary>
  ///   Provides support for lazy initialization.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of object that is being lazily initialized.
  /// </typeparam>
  Lazy<T> = record
  private
    fInstance: ILazy<T>; // DO NOT ADD ANY OTHER FIELDS !!!
    function GetIsAssigned: Boolean; inline;
    function GetIsValueCreated: Boolean; inline;
    function GetValue: T;
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
    /// <exception cref="Spring|EArgumentNilException">
    ///   <i>valueFactory</i> is <b>nil</b>.
    /// </exception>
    constructor Create(const valueFactory: Func<T>; ownsObject: Boolean = False); overload;

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

    class operator Implicit(const {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}value: Lazy<T>): ILazy<T>;
    class operator Implicit(const {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}value: Lazy<T>): T;
    class operator Implicit(const value: T): Lazy<T>;
    class operator Implicit(const valueFactory: Func<T>): Lazy<T>;
    class operator Implicit(const value: ILazy<T>): Lazy<T>;
    class operator Implicit(const value: Nullable.Null): Lazy<T>;

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

  Lazy = record
  private type
    ILazyInternal = interface(ILazy)
      procedure GetValue;
      procedure GetValueInternal(var result);
    end;

    ILazyInternal<T> = interface(ILazy)
      function GetValue: T;
      procedure GetValueInternal(var result);
    end;

    TLazy = class(TRefCountedObject)
    protected
      fValueFactory: IInterface;
      function GetIsValueCreated: Boolean;
      procedure CreateValue;
      procedure InvokeFactory(const valueFactory: Pointer); virtual; abstract;
    public
      property IsValueCreated: Boolean read GetIsValueCreated;
    end;

    TLazy<T> = class(TLazy, ILazy, ILazyInternal<T>)
    protected
      fValue: T;
      function GetValue: TValue;
      function GetValueT: T;
      function ILazyInternal<T>.GetValue = GetValueT;
      procedure GetValueInternal(var result);
      procedure InvokeFactory(const valueFactory: Pointer); override;
    end;

    TDefaultCtorFactory = class(TRefCountedObject, Func<TObject>)
    private
      classType: TClass;
      ctor: TConstructor;
      function Invoke: TObject;
    end;

    TReference = record
      Vtable: Pointer;
      RefCount: Integer;
      function QueryInterface(const IID: TGUID; out obj): HResult; stdcall;
    end;

    PObjectReference = ^TObjectReference;
    TObjectReference = record
      Vtable: Pointer;
      RefCount: Integer;
      Factory: IInterface;

      Value: TObject;
      OwnsObject: Boolean;
      function _Release: Integer; stdcall;
      function GetIsValueCreated: Boolean;
      function GetValue: TValue;
      function GetObject: TObject;
      procedure GetValueInternal(var result);
      procedure CreateValue;
    end;

    PInterfaceReference = ^TInterfaceReference;
    TInterfaceReference = record
      Vtable: Pointer;
      RefCount: Integer;
      Factory: IInterface;

      Value: IInterface;
      TypeInfo: PTypeInfo;
      function _Release: Integer; stdcall;
      function GetIsValueCreated: Boolean;
      function GetValue: TValue;
      function GetInterface: IInterface;
      procedure GetValueInternal(var result);
      procedure CreateValue;
    end;

  const
    ObjectReferenceVtable: array[0..6] of Pointer =
    (
      @TReference.QueryInterface,
      @RecAddRef,
      @TObjectReference._Release,

      @TObjectReference.GetIsValueCreated,
      @TObjectReference.GetValue,
      @TObjectReference.GetObject,
      @TObjectReference.GetValueInternal
    );

    InterfaceReferenceVtable: array[0..6] of Pointer =
    (
      @TReference.QueryInterface,
      @RecAddRef,
      @TInterfaceReference._Release,

      @TInterfaceReference.GetIsValueCreated,
      @TInterfaceReference.GetValue,
      @TInterfaceReference.GetInterface,
      @TInterfaceReference.GetValueInternal
    );
  var
    fInstance: ILazy;
    procedure GetValue(var result);
    function GetIsValueCreated: Boolean;
  public
    class procedure Make(const value: TObject; var result; ownsObject: Boolean); overload; static;
    class procedure Make(const value: IInterface; var result; typeInfo: PTypeInfo); overload; static;
    class procedure Make<T>(const value: T; var result); overload; static;

    class procedure MakeFromDefaultCtor(var result; const typeInfo: PTypeInfo); static;
    class procedure MakeFromFactory(factory: Pointer; var result; ownsObject: Boolean); overload; static;
    class procedure MakeFromFactory(factory: Pointer; var result; typeInfo: PTypeInfo); overload; static;
    class procedure MakeFromFactory<T>(factory: Pointer; var result); overload; static;
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
    class function EnsureInitialized<T>(var target: T; const valueFactory: Func<T>): T; overload; static;
  end;

  {$RTTI INHERIT
      METHODS(DefaultMethodRttiVisibility)
      FIELDS(DefaultFieldRttiVisibility)
      PROPERTIES(DefaultPropertyRttiVisibility)}

  {$ENDREGION}


  {$REGION 'Weak smart pointer'}

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

  Weak<T> = record
  private
    fReference: IInterface;
    function GetIsAlive: Boolean; inline;
    function GetTarget: T;
  public
    class operator Implicit(const value: T): Weak<T>;
    class operator Implicit({$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const value: Weak<T>): T;

    class operator Equal({$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const left: Weak<T>; const right: T): Boolean;
    class operator NotEqual({$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const left: Weak<T>; const right: T): Boolean;

    function TryGetTarget(var target: T): Boolean;
    property Target: T read GetTarget;
    property IsAlive: Boolean read GetIsAlive;
  end;

  Weak = record
  private type
    PReference = ^TReference;
    TReference = record
    private
      Vtable: Pointer;
      RefCount: Integer;
      Target: Pointer;
      function _Release_Obj: Integer; stdcall;
      function _Release_Intf: Integer; stdcall;
    end;

  const
    ObjectReferenceVtable: array[0..2] of Pointer =
    (
      @NopQueryInterface,
      @RecAddRef,
      @TReference._Release_Obj
    );

    InterfaceReferenceVtable: array[0..2] of Pointer =
    (
      @NopQueryInterface,
      @RecAddRef,
      @TReference._Release_Intf
    );
  private
    class function Equal(const left; const right): Boolean; static;
    class function NotEqual(const left; const right): Boolean; static;
    class function GetIsAlive(const reference): Boolean; static;
    class procedure GetTarget(const reference; var result; kind: TTypeKind); static;
    class function TryGetTarget(const reference; var target; kind: TTypeKind): Boolean; static;
    class procedure MakeFromObject(const value; var result); overload; static;
    class procedure MakeFromInterface(const value; var result); overload; static;
  end;

  {$RTTI INHERIT
      METHODS(DefaultMethodRttiVisibility)
      FIELDS(DefaultFieldRttiVisibility)
      PROPERTIES(DefaultPropertyRttiVisibility)}

  {$ENDREGION}


  {$REGION 'Shared smart pointer'}

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

  IShared<T> = reference to function: T;

  Shared<T> = record
  private
    fValue: T;
    fFinalizer: IInterface;
    class function GetMake: IShared<T>; static;
  public
    class operator Implicit(const value: IShared<T>): Shared<T>;
    class operator Implicit(const value: Shared<T>): IShared<T>;
    class operator Implicit(const value: Shared<T>): T; {$IFNDEF DELPHIXE4}inline;{$ENDIF}
    class operator Implicit(const value: Shared<T>): Weak<T>;
    class operator Implicit(const value: T): Shared<T>;
    property Value: T read fValue;

    class property Make: IShared<T> read GetMake;
  end;

  Shared = record
  private type
    PObjectFinalizer = ^TObjectFinalizer;
    TObjectFinalizer = record
    private
      Vtable: Pointer;
      RefCount: Integer;
      Value: TObject;
      function _Release: Integer; stdcall;
      function Invoke: TObject;
    end;

    PRecordFinalizer = ^TRecordFinalizer;
    TRecordFinalizer = record
    private
      Vtable: Pointer;
      RefCount: Integer;
      Value: Pointer;
      TypeInfo: PTypeInfo;
      function _Release: Integer; stdcall;
      function Invoke: Pointer;
    end;

    THandleFinalizer<T> = class(TInterfacedObject, IShared<T>)
    private
      fValue: T;
      fFinalizer: Action<T>;
      function Invoke: T;
    public
      constructor Create(const value: T; finalizer: Action<T>);
      destructor Destroy; override;
    end;

  const
    ObjectFinalizerVtable: array[0..3] of Pointer =
    (
      @NopQueryInterface,
      @RecAddRef,
      @TObjectFinalizer._Release,
      @TObjectFinalizer.Invoke
    );
    RecordFinalizerVtable: array[0..3] of Pointer =
    (
      @NopQueryInterface,
      @RecAddRef,
      @TRecordFinalizer._Release,
      @TRecordFinalizer.Invoke
    );
  private
    class procedure Make(const value: TObject; var result); overload; static;
    class procedure Make(const value: Pointer; typeInfo: PTypeInfo; var result); overload; static;
    class procedure Make(typeInfo: PTypeInfo; var result); overload; static;
  public
    class function Make<T: class, constructor>: IShared<T>; overload; static;
    class function Make<T>(const value: T): IShared<T>; overload; static;
    class function Make<T>(const value: T; const finalizer: Action<T>): IShared<T>; overload; static;
  end;

  {$RTTI INHERIT
      METHODS(DefaultMethodRttiVisibility)
      FIELDS(DefaultFieldRttiVisibility)
      PROPERTIES(DefaultPropertyRttiVisibility)}

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

  IPropertyChangedEvent = IInvokableEvent<TPropertyChangedEvent>;

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

  ENotImplementedException = SysUtils.ENotImplemented;
  EInvalidOperationException = SysUtils.EInvalidOpException;
  EKeyNotFoundException = class(SysUtils.EArgumentException);
  EArgumentNilException = SysUtils.EArgumentNilException;

  EInvalidCastException = SysUtils.EInvalidCast;

  EInsufficientMemoryException = EOutOfMemory;

  EFormatException = class(Exception);
  EIndexOutOfRangeException = class(Exception);

  EArgumentException = SysUtils.EArgumentException;
  EArgumentOutOfRangeException = SysUtils.EArgumentOutOfRangeException;
  EInvalidEnumArgumentException = class(EArgumentException);

  ERttiException = class(Exception);

  {$ENDREGION}


  {$REGION 'TTypeInfoHelper'}

  TTypeInfoHelper = record helper for TTypeInfo
  strict private
    function GetRttiType: TRttiType; inline;
  public
    function TypeData: PTypeData;
    function TypeName: string; inline;
    function TypeSize: Integer; inline;

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


  {$REGION 'TTimSort'}

{$IFDEF DELPHIXE7_UP}
  Pointer<T> = record
    {$POINTERMATH ON}
    type P = ^T;
    {$POINTERMATH OFF}
  end;

  PTimSort = ^TTimSort;
  TTimSort = record
  private
    type
      TCompareFunc = function(const left, right): Integer of object;
      TMergeFunc = procedure(left: Pointer; leftLen: Integer; right: Pointer; rightLen: Integer; self: PTimSort);
      TComparerMethod<T> = function(const left, right: T): Integer of object;
      {$POINTERMATH ON}
      PSlice = ^TSlice;
      {$POINTERMATH OFF}
      TSlice = record
        base: Pointer; len: Integer;
      end;
      TSliceArray = array[0..48] of TSlice;
  private
    const
      MIN_MERGE = 64;
      MIN_GALLOP = 7;
      INITIAL_TMP_STORAGE_LENGTH = 256;
  private
    fMinGallop: Integer;
    fTmp: Pointer; // TArray<T>
    fCompare: TMethodPointer;
    fCompareFunc: TCompareFunc;
    fMergeLo: TMergeFunc;
    fMergeHi: TMergeFunc;
    fItemSize: Integer;
    fStackLen: Integer;
    fRuns: TSliceArray;
    fArrayTypeInfo: PTypeInfo;
  private
    procedure Initialize(const comparer: IInterface; const compareFunc: Pointer;
      mergeLo, mergeHi: TMergeFunc; arrayTypeInfo: PTypeInfo; itemSize: Integer);
    procedure Finalize;
    function at(items: Pointer; index: Integer): Pointer; inline;
  public
    class function CompareThunk<T>(instance: Pointer; const left, right): Integer; static;
    class procedure BinaryInsertionSort<T>(var values: array of T; start: NativeInt; const comparer: IComparer<T>); static;
    class function CountRunAndMakeAscending<T>(var values: array of T; const comparer: IComparer<T>): Integer; static;
    class function MinRunLength(n: Integer): Integer; static;
    procedure PushRun(runBase: Pointer; runLen: Integer); inline;
    procedure MergeCollapse;
    procedure MergeForceCollapse;
    procedure MergeAt(i: Integer);
    function GallopLeft(key: Pointer; items: Pointer; len, hint: Integer): Integer;
    function GallopRight(key: Pointer; items: Pointer; len, hint: Integer): Integer;
    class procedure MergeLo<T>(left: Pointer<T>.P; leftLen: Integer; right: Pointer<T>.P; rightLen: Integer; ts: PTimSort); static;
    class procedure MergeHi<T>(left: Pointer<T>.P; leftLen: Integer; right: Pointer<T>.P; rightLen: Integer; ts: PTimSort); static;
    function EnsureTmpCapacity(neededCapacity: Integer): Pointer; inline;
    procedure Grow(neededCapacity: NativeInt);
  public
    class procedure Sort(items: Pointer; const comparer: IComparer<Pointer>; index, count: Integer); overload; static;
    class procedure Sort<T>(items: Pointer; const comparer: IComparer<T>; index, count: Integer); overload; static;
  end;
  {$ENDIF}

  {$ENDREGION}


  {$REGION 'TArray'}

  IComparerRef = interface //FI:W523
    function Compare(const left, right): Integer;
  end;
  TSlice<T> = array[0..2] of T;
  Int24 = packed record
  case Integer of
    0: (Low: Word; High: Byte);
    1: (Bytes: array[0..2] of Byte);
  end;
  PInt24 = ^Int24;
  TArray = class
  private type
    TCompareMethodRef = function(const left, right): Integer of object;
    TCompareMethod<T> = function(const left, right: T): Integer of object;
    TQuickSortPartitionHelper<T> = record
      compare: TCompareMethod<T>;
      pivotIndex: NativeInt;
      temp, pivot: T;
    end;
    TInsertionSortHelper<T> = record
      compare: TCompareMethod<T>;
      hi: NativeInt;
      temp: T;
    end;
  protected
    const FoldedTypeKinds = [tkInteger, tkChar, tkEnumeration, tkClass, tkMethod, tkWChar, tkInterface, tkInt64, tkUString, tkClassRef, tkPointer, tkProcedure];
    const PointerTypeKinds = [tkClass, tkInterface, tkDynArray, tkUString, tkClassRef, tkPointer, tkProcedure];
    const IntrosortSizeThreshold = 16;
    class function GetDepthLimit(count: Integer): Integer; static;

    class procedure DownHeap<T>(var values: array of T; {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>; i: NativeInt); static;
    class procedure HeapSort<T>(var values: array of T; {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>); static;

    class procedure InsertionSort<T>(var values: array of T; const comparer: IComparer<T>); static;
    class function QuickSortPartition<T>(var values: array of T; {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>): NativeInt; static;
    class procedure IntroSort<T>(var values: array of T; const comparer: IComparer<T>; depthLimit: Integer = -1); static;

    class procedure DownHeap_Ref(values: PByte; hi: NativeInt; const compare: TCompareMethodRef; i, size: NativeInt); static;
    class procedure HeapSort_Ref(values: PByte; hi: NativeInt; const compare: TCompareMethodRef; size: NativeInt); static;
    class procedure InsertionSort_Ref(values: PByte; hi: NativeInt; const compare: TCompareMethodRef; size: NativeInt); static;
    class function QuickSortPartition_Ref(values: PByte; hi: NativeInt; {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethodRef; size: NativeInt): NativeInt; static;
    class procedure IntroSort_Ref(values: PByte; hi: NativeInt; const comparer: IComparerRef; size: NativeInt; depthLimit: Integer = -1); static;

    class procedure IntroSort_Int8(var values: array of Int8; const comparer: IComparer<Int8>); static;
    class procedure IntroSort_Int16(var values: array of Int16; const comparer: IComparer<Int16>); static;
    class procedure IntroSort_Int24(var values: array of Int24; const comparer: IComparer<Int24>); static;
    class procedure IntroSort_Int32(var values: array of Int32; const comparer: IComparer<Int32>); static;
    class procedure IntroSort_Int64(var values: array of Int64; const comparer: IComparer<Int64>); static;
    class procedure IntroSort_Single(var values: array of Single; const comparer: IComparer<Single>); static;
    class procedure IntroSort_Double(var values: array of Double; const comparer: IComparer<Double>); static;
    class procedure IntroSort_Extended(var values: array of Extended; const comparer: IComparer<Extended>); static;
    class procedure IntroSort_Method(var values: array of TMethodPointer; const comparer: IComparer<TMethodPointer>); static;

    class procedure Reverse_Int8(const values: PInt8; right: NativeInt); static;
    class procedure Reverse_Int16(const values: PInt16; right: NativeInt); static;
    class procedure Reverse_Int24(const values: PInt24; right: NativeInt); static;
    class procedure Reverse_Int32(const values: PInt32; right: NativeInt); static;
    class procedure Reverse_Int64(const values: PInt64; right: NativeInt); static;
    class procedure Reverse_Single(const values: PSingle; right: NativeInt); static;
    class procedure Reverse_Double(const values: PDouble; right: NativeInt); static;
    class procedure Reverse_Extended(const values: PExtended; right: NativeInt); static;
    class procedure Reverse_Method(const values: PMethodPointer; right: NativeInt); static;
    class procedure Reverse_Ref(const values: PByte; right, size: NativeInt); static;
    class procedure Reverse_Generic<T>(var values: array of T); static;

    class procedure Shuffle_Int8(const values: PInt8; hi: NativeInt); static;
    class procedure Shuffle_Int16(const values: PInt16; hi: NativeInt); static;
    class procedure Shuffle_Int24(const values: PInt24; hi: NativeInt); static;
    class procedure Shuffle_Int32(const values: PInt32; hi: NativeInt); static;
    class procedure Shuffle_Int64(const values: PInt64; hi: NativeInt); static;
    class procedure Shuffle_Single(const values: PSingle; hi: NativeInt); static;
    class procedure Shuffle_Double(const values: PDouble; hi: NativeInt); static;
    class procedure Shuffle_Extended(const values: PExtended; hi: NativeInt); static;
    class procedure Shuffle_Method(const values: PMethodPointer; hi: NativeInt); static;
    class procedure Shuffle_Ref(const values: PByte; hi, size: NativeInt); static;
    class procedure Shuffle_Generic<T>(var values: array of T); static;

    class function BinarySearchInternal<T>(const values: array of T;
      const item: T; out foundIndex: Integer; const comparer: IComparer<T>): Boolean; static;
    class function BinarySearchUpperBoundInternal<T>(const values: array of T;
      const item: T; out foundIndex: Integer; const comparer: IComparer<T>): Boolean; static;
  public
    /// <summary>
    ///   Searches a sorted array for the given value, using a binary search
    ///   algorithm returning the index for the first found value using the
    ///   default comparer.
    /// </summary>
    class function BinarySearch<T>(const values: array of T; const item: T;
      out foundIndex: Integer): Boolean; overload; static;

    /// <summary>
    ///   Searches a range of elements in a sorted array for the given value,
    ///   using a binary search algorithm returning the index for the first
    ///   found value using the default comparer.
    /// </summary>
    class function BinarySearch<T>(const values: array of T; const item: T;
      out foundIndex: Integer; index, count: Integer): Boolean; overload; static;

    /// <summary>
    ///   Searches a sorted array for the given value, using a binary search
    ///   algorithm returning the index for the first found value using the
    ///   specified comparer.
    /// </summary>
    class function BinarySearch<T>(const values: array of T; const item: T;
      out foundIndex: Integer; const comparer: IComparer<T>): Boolean; overload; static;

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
    ///   specified comparison.
    /// </summary>
    class function BinarySearch<T>(const values: array of T; const item: T;
      out foundIndex: Integer; const comparison: TComparison<T>): Boolean; overload; static;

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
    ///   algorithm returning the index for the last found value.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer): Boolean; overload; static;

    /// <summary>
    ///   Searches a range of elements in a sorted array for the given value,
    ///   using a binary search algorithm returning the index for the last
    ///   found value using the default comparer.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer;
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
    ///   Searches a range of elements in a sorted array for the given value,
    ///   using a binary search algorithm returning the index for the last
    ///   found value using the specified comparer.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer; const comparer: IComparer<T>;
      index, count: Integer): Boolean; overload; static;

    /// <summary>
    ///   Searches a sorted array for the given value, using a binary search
    ///   algorithm returning the index for the last found value using the
    ///   specified comparer.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer;
      const comparison: TComparison<T>): Boolean; overload; static;

    /// <summary>
    ///   Searches a range of elements in a sorted array for the given value,
    ///   using a binary search algorithm returning the index for the last
    ///   found value using the specified comparison.
    /// </summary>
    class function BinarySearchUpperBound<T>(const values: array of T;
      const item: T; out foundIndex: Integer; const comparison: TComparison<T>;
      index, count: Integer): Boolean; overload; static;

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
      const action: Action<T>); static;

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
    ///   Swaps the values of the specified variables.
    /// </summary>
    class procedure Swap<T>(left, right: Pointer); static; inline;

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
    ///   Reverses the elements in the specified range in the array.
    /// </summary>
    class procedure Reverse<T>(const values: Pointer; hi: NativeInt); overload; static; inline;

    /// <summary>
    ///   Shuffles the elements in the array using the Fisher-Yates algorithm.
    /// </summary>
    class procedure Shuffle<T>(var values: array of T); overload; static;

    /// <summary>
    ///   Shuffles the specified count of elements in the array starting at the
    ///   specified index using the Fisher-Yates algorithm.
    /// </summary>
    class procedure Shuffle<T>(var values: array of T;
      index, count: Integer); overload; static;

    /// <summary>
    ///   Shuffles the elements in the specified range in the array using the
    ///   Fisher-Yates algorithm.
    /// </summary>
    class procedure Shuffle<T>(const values: Pointer; hi: NativeInt); overload; static; inline;

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

{$IFDEF DELPHIXE7_UP}
    /// <summary>
    ///   When <c>True</c> any managed reference type is treated as Pointer by
    ///   StableSort results in faster sorting.
    /// </summary>
    class var UnsafeStableSort: Boolean;
    const ManagedPointerTypeKinds = [tkInterface, tkDynArray, tkUString];
    const UnmanagedPointerTypeKinds = [tkClass, tkClassRef, tkPointer, tkProcedure];

    /// <summary>
    ///   Sorts the elements in an array using the default comparer.
    /// </summary>
    class procedure StableSort<T>(var values: array of T); overload; static;

    /// <summary>
    ///   Sorts the specified range of elements in an array using the default
    ///   comparer.
    /// </summary>
    class procedure StableSort<T>(var values: array of T; index, count: Integer); overload; static;

    /// <summary>
    ///   Sorts the elements in an array using the specified comparer.
    /// </summary>
    class procedure StableSort<T>(var values: array of T; const comparer: IComparer<T>); overload; static;

    /// <summary>
    ///   Sorts the specified range of elements in an array using the specified
    ///   comparer.
    /// </summary>
    class procedure StableSort<T>(var values: array of T;
      const comparer: IComparer<T>; index, count: Integer); overload; static;

    /// <summary>
    ///   Sorts the elements in an array using the specified comparison.
    /// </summary>
    class procedure StableSort<T>(var values: array of T; const comparison: TComparison<T>); overload; static;

    /// <summary>
    ///   Sorts the specified range of elements in an array using the specified
    ///   comparison.
    /// </summary>
    class procedure StableSort<T>(var values: array of T;
      const comparison: TComparison<T>; index, count: Integer); overload; static;
{$ENDIF}
  end;

  {$ENDREGION}


  {$REGION 'TTypeInfo<T>'}

  TTypeInfo<T> = record
  private
  {$IFDEF DELPHIXE7_UP}
    {$IFDEF WEAKREF}
    class function GetHasWeakRef: Boolean; static; inline;
    {$ENDIF}
    class function GetIsManaged: Boolean; static; inline;
  {$ELSE}
    {$IFDEF WEAKREF}
    class var fHasWeakRef: Boolean;
    {$ENDIF}
    class var fIsManaged: Boolean;
    class constructor Create;
  {$ENDIF}
  public
    {$IFDEF WEAKREF}
    class property HasWeakRef: Boolean read {$IFDEF DELPHIXE7_UP}GetHasWeakRef{$ELSE}fHasWeakRef{$ENDIF};
    {$ELSE}
    const HasWeakRef = False;
    {$ENDIF}
    class property IsManaged: Boolean read {$IFDEF DELPHIXE7_UP}GetIsManaged{$ELSE}fIsManaged{$ENDIF};
  end;

  {$ENDREGION}


  {$REGION 'Vector<T>'}

  TArrayEnumerator<T> = record
  private
    fItems: Pointer;
    fIndex: Integer;
    function GetCurrent: T; inline;
  public
    constructor Create(const items: TArray<T>);
    function MoveNext: Boolean; inline;
    property Current: T read GetCurrent;
  end;

  VectorHelper = record
  private
    class function InternalIndexOfInt8(const data: Pointer; const item: ShortInt): NativeInt; static;
    class function InternalIndexOfInt16(const data: Pointer; const item: SmallInt): NativeInt; static;
    class function InternalIndexOfInt32(const data: Pointer; const item: Integer): NativeInt; static;
    class function InternalIndexOfInt64(const data: Pointer; const item: Int64): NativeInt; static;
    class function InternalIndexOfStr(const data: Pointer; const item: string): NativeInt; static;
  end;

  Vector<T> = record
  private
    fData: TArray<T>; // DO NOT ADD ANY OTHER FIELDS !!!
    function GetCount: NativeInt; inline;
    function GetFirst: T; inline;
    function GetItem(index: NativeInt): T; inline;
    function GetLast: T; inline;
    procedure SetCount(value: NativeInt); inline;
    procedure SetItem(index: NativeInt; const value: T); inline;
    procedure InternalInsert(index: NativeInt; const items: array of T);
    function InternalEquals(const items: array of T): Boolean;
    function InternalIndexOf(const item: T): NativeInt;
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

    function Add(const item: T): NativeInt; overload; inline;
    procedure Add(const items: array of T); overload;
    procedure Add(const items: TArray<T>); overload; inline;
    procedure Add(const items: Vector<T>); overload; inline;
    procedure Insert(index: NativeInt; const item: T); overload; inline;
    procedure Insert(index: NativeInt; const items: array of T); overload;
    procedure Insert(index: NativeInt; const items: TArray<T>); overload; inline;
    procedure Delete(index: NativeInt); overload; inline;
    procedure Delete(index: NativeInt; count: NativeInt); overload; inline;
    function Remove: T; overload; inline;
    procedure Remove(const item: T); overload; inline;
    procedure Remove(const items: array of T); overload;
    procedure Remove(const items: TArray<T>); overload; inline;

    function Contains(const item: T): Boolean; overload; inline;
    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; overload;
    function Contains(const item: T; const comparer: TEqualityComparison<T>): Boolean; overload;
    function Contains(const items: array of T): Boolean; overload;
    function Contains(const items: TArray<T>): Boolean; overload;
    function IndexOf(const item: T): NativeInt; inline;
    function Equals(const items: array of T): Boolean; overload;
    function Equals(const items: TArray<T>): Boolean; overload; inline;

    function Slice(index: NativeInt): Vector<T>; overload; inline;
    function Slice(index: NativeInt; count: NativeInt): Vector<T>; overload; inline;
    function Splice(index: NativeInt; count: NativeInt): Vector<T>; overload; inline;
    function Splice(index: NativeInt; count: NativeInt; const items: array of T): Vector<T>; overload;

    procedure Sort; overload; inline;
    procedure Sort(const comparer: IComparer<T>); overload; inline;
    procedure Sort(const comparer: TComparison<T>); overload; inline;
    procedure Reverse;

    procedure ForEach(const action: Action<T>); inline;

    function GetEnumerator: TArrayEnumerator<T>; inline;
    property Count: NativeInt read GetCount;
    property Data: TArray<T> read fData;
    property First: T read GetFirst;
    property Items[index: NativeInt]: T read GetItem write SetItem; default;
    property Last: T read GetLast;
    property Length: NativeInt read GetCount write SetCount;
  end;

  {$ENDREGION}


  {$REGION 'Routines'}

{$IFNDEF DELPHIXE2_UP}
function ReturnAddress: Pointer;
{$ENDIF}

{$IFNDEF DELPHIXE3_UP}
function Pos(const SubStr, Str: UnicodeString; Offset: Integer): Integer; overload;
{$ENDIF}

{$IFNDEF DELPHIXE7_UP}
procedure DynArrayAssign(var Dest: Pointer; Source: Pointer; typeInfo: Pointer);
procedure DynArrayCopyRange(var Result: Pointer; A: Pointer; TypeInfo: Pointer; Index, Count: NativeInt);
{$ENDIF}

procedure PlatformNotImplemented;

/// <summary>
///   Raises an <see cref="Spring|EArgumentNilException" /> if the <paramref name="value" />
///    is nil.
/// </summary>
procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string); overload; deprecated 'Use Guard.CheckNotNull instead';

/// <summary>
///   Raises an <see cref="Spring|EArgumentNilException" /> if the <paramref name="value" />
///    is nil.
/// </summary>
procedure CheckArgumentNotNull(value: Pointer; const argumentName: string); overload; deprecated 'Use Guard.CheckNotNull instead';

function GetQualifiedClassName(AInstance: TObject): string; overload; inline;
function GetQualifiedClassName(AClass: TClass): string; overload; {$IFDEF DELPHIXE2_UP}inline;{$ENDIF}

function FormatValue(const value: TValue): string;

/// <summary>
///   Determines whether an instance of <c>leftType</c> can be assigned from an
///   instance of <c>rightType</c>.
/// </summary>
function IsAssignableFrom(leftType, rightType: PTypeInfo): Boolean; overload;

function IsAssignableFrom(const leftTypes, rightTypes: array of PTypeInfo): Boolean; overload;

/// <summary>
///   Determines whether an instance of <c>leftType</c> can be assigned from an
///   instance of <c>rightType</c> using an equality check of the typeinfo.
/// </summary>
function IsAssignableFromRelaxed(leftType, rightType: PTypeInfo): Boolean;

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
/// <remarks>
///   A value that returns <c>True</c> from <see cref="IsEmpty" />
///   it is considered greater than a non empty value.
/// </remarks>
function CompareValue(const left, right: TValue): Integer; overload;

/// <summary>
///   Returns the types of the values.
/// </summary>
function TypesOf(const values: array of TValue): TArray<PTypeInfo>;

function InterfaceToMethodPointer(const intf; index: Integer): TMethodPointer;
function MethodReferenceToMethod(const methodRef): TMethod;
function MethodReferenceToMethodPointer(const methodRef): TMethodPointer;
function MethodToMethodReference(const method: TMethod): IInterface;
function MethodPointerToMethodReference(const method: TMethodPointer): IInterface;
function HasMethodInfo(typeInfo: PTypeInfo): Boolean;
function IsMethodReference(typeInfo: PTypeInfo): Boolean;

function GetInterfaceByTypeInfo(self: TObject; intf: PTypeInfo; out obj): Boolean;

{$IFDEF MSWINDOWS}
function UTF8IdentIdentCompare(left, right: PByte): Boolean;
{$ENDIF}

function SameTypeInfo(const left, right: PTypeInfo): Boolean;
function GetTypeInfoHashCode(const typeInfo: PTypeInfo): Integer;

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
function AtomicIncrement(var target: Integer): Integer; overload;
function AtomicIncrement(var target: Integer; increment: Integer): Integer; overload;
function AtomicIncrement(var target: Int64; increment: Int64 = 1): Int64; overload;
function AtomicDecrement(var target: Integer): Integer; overload;
function AtomicDecrement(var target: NativeInt; decrement: NativeInt): NativeInt; overload;
function AtomicExchange(var target: Integer; value: Integer): Integer; overload;
function AtomicExchange(var target: NativeInt; value: NativeInt): NativeInt; overload;
function AtomicExchange(var target: Pointer; value: Pointer): Pointer; overload;
function AtomicCmpExchange(var target: Integer; newValue, comparand: Integer): Integer; overload;
function AtomicCmpExchange(var target: NativeInt; newValue, comparand: NativeInt): NativeInt; overload;
function AtomicCmpExchange(var target: Pointer; newValue, comparand: Pointer): Pointer; overload;
{$ENDIF}
procedure AtomicStore(var target: Int64; const {$IFDEF CPUX86}{$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}{$ENDIF}value: Int64); {$IFNDEF CPUX86}inline;{$ENDIF}
function AtomicLoad(var source: Int64): Int64; {$IFNDEF CPUX86}inline;{$ENDIF}
function AtomicExchangeAdd(var target: Int64; const value: Int64): Int64;

procedure IncUnchecked(var i: Integer; const n: Integer = 1); inline;

procedure SwapPtr(arr: PPointer; left, right: Integer); inline;

function IsPowerOf2(value: NativeInt): Boolean;

function NextPowerOf2(value: NativeInt): NativeInt;

// copy from System.pas to make it possible to inline
function DynArrayLength(const A: Pointer): NativeInt; inline;
function DynArrayHigh(const A: Pointer): NativeInt; inline;

function GetEqualsOperator(const typeInfo: PTypeInfo): TRttiMethod;

function GrowCapacity(oldCapacity: Integer): Integer; overload;
function GrowCapacity(oldCapacity, newCount: Integer): Integer; overload;

{$IFNDEF DELPHIX_BERLIN_UP}
procedure CopyRecord(Dest, Source, TypeInfo: Pointer);
procedure FinalizeRecord(P: Pointer; TypeInfo: Pointer);
{$ENDIF}

procedure RegisterWeakRef(address: Pointer; const instance: TObject);
procedure UnregisterWeakRef(address: Pointer; const instance: TObject);

procedure MoveManaged(source, target, typeInfo: Pointer; count: NativeInt);

procedure CheckIndex(index, size: Integer); inline;
procedure CheckRange(index, count, size: Integer); inline;

procedure BinarySwap(left, right: Pointer; size: NativeInt);

{$IFNDEF MSWINDOWS}
function RegisterExpectedMemoryLeak(P: Pointer): Boolean;
{$ENDIF}

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv; IsConst: Boolean = False): Boolean;

{$IFNDEF DELPHIX_TOKYO_UP}
function StrToUInt(const s: string): Cardinal;
{$ENDIF}
{$IFNDEF DELPHIXE6_UP}
function StrToUInt64(const s: string): UInt64;
{$ENDIF}

procedure __SuppressWarning(var value); inline;

  {$ENDREGION}


const
  EmptyValue: TValue = ();
  caReseted = caReset deprecated 'Use caReset'; //FI:O803

  ObjCastGUID: TGUID = '{CEDF24DE-80A4-447D-8C75-EB871DC121FD}';

  FieldVisibility = [{$IFDEF FIELD_RTTI}vcPrivate..vcProtected{$ENDIF}]; //FI:O803

  InvokableEvent = {$IFDEF INVOKABLE_EVENT}True{$ELSE}False{$ENDIF};

{$IFNDEF DELPHIXE3_UP}
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
{$ENDIF}

const
  ISO8601FormatSettings: TFormatSettings = (
    DateSeparator: '-';
    TimeSeparator: ':';
    ShortDateFormat: 'YYYY-MM-DD';
    LongDateFormat: 'YYYY-MM-DD';
    TimeAMString: '';
    TimePMString: '';
    ShortTimeFormat: 'hh:nn:ss';
    LongTimeFormat: 'hh:nn:ss';
    DecimalSeparator: '.';
  );

implementation

uses
  DateUtils,
{$IFDEF DELPHIXE8_UP}
  System.Hash,
{$ENDIF}
  Math,
{$IFNDEF DELPHIX_BERLIN_UP}
  RTLConsts,
{$ENDIF}
  StrUtils,
  SysConst,
  Variants,
  VarUtils,
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
  Spring.Comparers,
  Spring.Events,
  Spring.ResourceStrings,
  Spring.VirtualClass;

const
  TypeDataOffset    = SizeOf(TTypeKind) + SizeOf(Byte);
  UnitNameOffset    = TypeDataOffset + SizeOf(TClass) + SizeOf(PPTypeInfo) + SizeOf(SmallInt);
  IntfUnitOffset    = TypeDataOffset + SizeOf(PPTypeInfo) + SizeOf(TIntfFlagsBase) + SizeOf(TGUID);
  DynUnitNameOffset = TypeDataOffset + SizeOf(Integer) + SizeOf(PPTypeInfo) + SizeOf(Integer) + SizeOf(PPTypeInfo);


{$REGION 'Routines'}

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

{$IFNDEF DELPHIXE7_UP}
procedure DynArrayAssign(var Dest: Pointer; Source: Pointer; typeInfo: Pointer);
asm
  jmp System.@DynArrayAsg;
end;

procedure DynArrayCopyRange(var Result: Pointer; A: Pointer; TypeInfo: Pointer; Index, Count: NativeInt);
asm
{$IFDEF CPUX64}
  .noframe
  jmp System.@DynArrayCopyRange
{$ELSE}
  push ebx
  mov ebx,[ebp+$08]
  push ebx
  push eax
  mov eax,edx
  mov edx,ecx
  mov ecx,[ebp+$0c]
  call System.@DynArrayCopyRange
  pop ebx
{$ENDIF}
end;

{$ENDIF}

{$IFNDEF DELPHIXE2_UP}{$IFNDEF STACKFRAMES_ON}{$W+}{$ENDIF}{$ENDIF}
procedure PlatformNotImplemented;
begin
  raise ENotImplementedException.Create('Not implemented in present platform.') at ReturnAddress;
end;
{$IFNDEF DELPHIXE2_UP}{$IFNDEF STACKFRAMES_ON}{$W-}{$ENDIF}{$ENDIF}

procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string);
begin
  CheckArgumentNotNull(Pointer(value), argumentName);
end;

procedure CheckArgumentNotNull(value: Pointer; const argumentName: string);
begin
  if not Assigned(value) then
    Guard.RaiseArgumentNullException(argumentName);
end;

function FormatValue(const value: TValue): string;

  function FormatArray(const value: TValue): string;
  var
    i: Integer;
  begin
    Result := '[';
    for i := 0 to value.GetArrayLength - 1 do
    begin
      if i > 0 then
        Result := Result + ',';
      Result := Result + FormatValue(value.GetArrayElement(i));
    end;
    Result := Result + ']';
  end;

  function FormatRecord(const value: TValue): string;
  var
    guid: TGUID;
    method: TRttiMethod;
    i: NativeInt;
    fields: TArray<TRttiField>;
  begin
    // handle TGUID explicitly
    if value.TryAsType(TypeInfo(TGUID), guid) then
      Exit(guid.ToString);

    // use function ToString: string when available
    for method in value.TypeInfo.RttiType.GetMethods do
      if SameText(method.Name, 'ToString')
        and (method.MethodKind = mkFunction)
        and (method.GetParameters = nil)
        and (method.ReturnType.TypeKind = tkUString) then
        Exit(method.Invoke(value, []).AsString);

    // write fields otherwise
    Result := '(';
    fields := value.TypeInfo.RttiType.GetFields;
    for i := 0 to High(fields) do
    begin
      if i > 0 then
        Result := Result + '; ';
      Result := Result + fields[i].Name +': ';
      if Assigned(fields[i].FieldType) then
        Result := Result + FormatValue(fields[i].GetValue(value.GetReferenceToRawData))
      else
        Result := Result + '(unknown)';
    end;
    Result := Result + ')';
  end;

  function FormatEnumerable(const enumerable: IEnumerable): string;
  var
    i: Integer;
    value: TValue;
  begin
    Result := '[';
    i := 0;
    for value in enumerable do
    begin
      if i > 0 then
        Result := Result + ',';
      Inc(i);
      Result := Result + FormatValue(value);
    end;
    Result := Result + ']';
  end;

  function StripUnitName(const s: string): string;
  begin
    Result := ReplaceText(s, 'System.', '');
  end;

const
  IEnumerableGuid: TGUID = '{6BC97F33-C0A8-4770-8E1C-C2017527B7E7}'; // copy from Spring.Collections
var
  intf: IInterface;
  enumerable: IEnumerable;
  obj: TObject;
begin
  case value.Kind of
    tkFloat:
      if value.TypeInfo = TypeInfo(TDateTime) then
        Result := DateTimeToStr(TValueData(value).FAsDouble)
      else if value.TypeInfo = TypeInfo(TDate) then
        Result := DateToStr(TValueData(value).FAsDouble)
      else if value.TypeInfo = TypeInfo(TTime) then
        Result := TimeToStr(TValueData(value).FAsDouble)
      else
        Result := value.ToString;
    tkClass:
    begin
      obj := value.AsObject;
      Result := Format('%s($%x)', [StripUnitName(obj.ClassName), NativeInt(obj)]);
    end;
    tkInterface:
    begin
      intf := value.AsInterface;
      if Supports(intf, IEnumerableGuid, enumerable) then
        Result := FormatEnumerable(enumerable)
      else
      begin
        obj := intf as TObject;
        Result := Format('%s($%x) as %s', [StripUnitName(obj.ClassName),
          NativeInt(intf), StripUnitName(value.TypeInfo.TypeName)]);
      end;
    end;
    tkArray, tkDynArray:
      Result := FormatArray(value);
    tkChar, tkWChar:
      if TValueData(value).FAsUWord < 20 then
        Result := '#' + IntToStr(TValueData(value).FAsUWord)
      else
        Result := QuotedStr(value.ToString);
    tkString, tkLString, tkWString, tkUString:
      Result := QuotedStr(value.ToString);
    tkClassRef:
      Result := value.AsClass.ClassName;
    tkRecord{$IF Declared(tkMRecord)}, tkMRecord{$IFEND}:
      Result := FormatRecord(value);
  else
    Result := value.ToString;
  end;
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

{$IFDEF MSWINDOWS}
function InternalUTF8ShortShortCompare(left, right: Pointer): Boolean; overload;
var
  leftLen, rightLen: Integer;
  leftBuffer, rightBuffer: array[0..255] of WideChar;
begin
  leftLen := MultiByteToWideChar(CP_UTF8, 0, @PByte(left)[1], PByte(left)^, leftBuffer, Length(leftBuffer));
  rightLen := MultiByteToWideChar(CP_UTF8, 0, @PByte(right)[1], PByte(right)^, rightBuffer, Length(rightBuffer));
  Result := CompareString(UTF8CompareLocale, NORM_IGNORECASE, leftBuffer, leftLen, rightBuffer, rightLen) = CSTR_EQUAL;
end;

function UTF8IdentIdentCompare(left, right: PByte): Boolean;
label
  NotEqual, Utf8Compare;
var
  i, n: NativeInt;
begin
  if left^ <> right^ then
    goto NotEqual;

  i := 1;
  for n := 1 to left^ div 4 do //FI:W528
  begin
    if PCardinal(@left[i])^ and $80808080 <> 0 then
      goto Utf8Compare;
    if PCardinal(@right[i])^ and $80808080 <> 0 then
      goto Utf8Compare;
    if (PCardinal(@left[i])^ xor PCardinal(@right[i])^) and $5F5F5F5F <> 0 then
      goto NotEqual;
    Inc(i, 4);
  end;
  for n := 1 to left^ mod 4 do //FI:W528
  begin
    if left[i] and $80 <> 0 then
      goto Utf8Compare;
    if right[i] and $80 <> 0 then
      goto Utf8Compare;
    if (left[i] xor right[i]) and $5F <> 0 then
      goto NotEqual;
    Inc(i);
  end;
  Exit(True);
NotEqual:
  Exit(False);
Utf8Compare:
  Result := InternalUTF8ShortShortCompare(left, right);
end;
{$ENDIF}

function SameTypeInfo(const left, right: PTypeInfo): Boolean;
label
  BothTypesNonNil, Equal;
var
  leftUnitName, rightUnitName: ^TSymbolName;
begin
  if left <> right then
  begin
    if NativeUInt(left) and NativeUInt(right) <> 0 then
    begin
  BothTypesNonNil:
      if left.Kind = right.Kind then
      if UTF8IdentIdentCompare(@left.Name, @right.Name) then
      begin
        case left.Kind of
          tkDynArray:
          begin
            leftUnitName := @PByte(left)[Length(left.Name) + DynUnitNameOffset];
            rightUnitName := @PByte(right)[Length(right.Name) + DynUnitNameOffset];
          end;
          tkInterface:
          begin
            leftUnitName := @PByte(left)[Length(left.Name) + IntfUnitOffset];
            rightUnitName := @PByte(right)[Length(right.Name) + IntfUnitOffset];
          end;
          tkClass:
          begin
            leftUnitName := @PByte(left)[Length(left.Name) + UnitNameOffset];
            rightUnitName := @PByte(right)[Length(right.Name) + UnitNameOffset];
          end;
        else
          goto Equal;
        end;
        Exit(UTF8IdentIdentCompare(Pointer(leftUnitName), Pointer(rightUnitName)));
      end;
    end else if Assigned(left) and Assigned(right) then
      goto BothTypesNonNil;
    Exit(False);
  end;
Equal:
  Result := True;
end;

function GetTypeInfoHashCode(const typeInfo: PTypeInfo): Integer;
var
  hashFunction: THashFunction;
  unitName: ^TSymbolName;
begin
  hashFunction := DefaultHashFunction;
  Result := hashFunction(typeInfo.Name[1], Length(typeInfo.Name), Ord(typeInfo.Kind));
  case typeInfo.Kind of
    tkDynArray:  unitName := @PByte(typeInfo)[Length(typeInfo.Name) + DynUnitNameOffset];
    tkInterface: unitName := @PByte(typeInfo)[Length(typeInfo.Name) + IntfUnitOffset];
    tkClass:     unitName := @PByte(typeInfo)[Length(typeInfo.Name) + UnitNameOffset];
  else
    Exit;
  end;
  Result := hashFunction(unitName^[1], Length(unitName^), Result);
end;

function GetInterfaceEntryByTypeInfo(cls: TClass; intf: PTypeInfo): PInterfaceEntry;
var
  interfaceTable: PInterfaceTable;
  p: PPPTypeInfo;
  i: Integer;
begin
  repeat
    interfaceTable := cls.GetInterfaceTable;
    if interfaceTable <> nil then
    begin
      p := @interfaceTable.Entries[interfaceTable.EntryCount];
      for i := 0 to interfaceTable.EntryCount - 1 do
      begin
        Result := @interfaceTable.Entries[i];
        if p^^ = intf then
          Exit;
        if (p^^.Kind = tkInterface)
          and UTF8IdentIdentCompare(@p^^.Name, @intf.Name)
          and UTF8IdentIdentCompare(@p^^.TypeData.IntfUnit, @intf.TypeData.IntfUnit) then
          Exit;
        Inc(p);
      end;
    end;
    cls := cls.ClassParent;
  until cls = nil;
  Result := nil;
end;

procedure InvokeImplGetter(const self: TObject; implGetter: NativeUInt; var result: IInterface);
{$IFNDEF CPUX86}
type
{$IF Defined(MSWINDOWS) or Defined(OSX32)}
  TGetProc = procedure (const Self: TObject; var Result: IInterface);
{$ELSEIF Defined(LINUX64) or Defined(OSX64) or Defined(CPUARM32)}
  TGetProc = procedure (var Result: IInterface; const Self: TObject);
{$ELSEIF Defined(CPUARM64)}
  TGetProc = function (const Self: TObject): IInterface;
{$ELSE}
  {$MESSAGE Fatal 'InvokeImplGetter not implemented for platform'}
{$IFEND}
var
  getProc: TGetProc;
begin
  if (implGetter and PROPSLOT_MASK) = PROPSLOT_FIELD then
    Result := IInterface(PPointer(PByte(self) + (implGetter and not PROPSLOT_MASK))^)
  else
  begin
    if (implGetter and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
      getProc := PPointer(PNativeInt(self)^ + SmallInt(implGetter))^
    else
      getProc := Pointer(implGetter);
{$IF Defined(MSWINDOWS) or Defined(OSX32)}
    GetProc(Self, Result);
{$ELSEIF Defined(LINUX64) or Defined(OSX64) or Defined(CPUARM32)}
    GetProc(Result, Self);
{$ELSEIF Defined(CPUARM64)}
    Result := GetProc(Self);
{$ELSE}
    {$MESSAGE Fatal 'InvokeImplGetter not implemented for platform'}
{$IFEND}
  end;
end;
{$ELSE}
asm
  xchg edx,ecx
  cmp ecx,PROPSLOT_FIELD
  jae @@isField
  cmp ecx,PROPSLOT_VIRTUAL
  jb @@isStaticMethod

  movsx ecx,cx
  add ecx,[eax]
  jmp dword ptr [ecx]

@@isStaticMethod:
  jmp ecx

@@isField:
  and ecx,not PROPSLOT_MASK
  add ecx,eax
  mov eax,edx
  mov edx,[ecx]
  jmp System.@IntfCopy
end;
{$ENDIF}

function GetInterfaceByTypeInfo(self: TObject; intf: PTypeInfo; out Obj): Boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  Pointer(obj) := nil;
  InterfaceEntry := GetInterfaceEntryByTypeInfo(self.ClassType, intf);
  if InterfaceEntry <> nil then
    if InterfaceEntry^.IOffset <> 0 then
    begin
      Pointer(obj) := Pointer(PByte(self) + InterfaceEntry^.IOffset);
      if Pointer(obj) <> nil then IInterface(obj)._AddRef;
    end
    else
      InvokeImplGetter(self, InterfaceEntry^.ImplGetter, IInterface(obj));
  Result := Pointer(obj) <> nil;
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
    if ifHasGuid in leftData.IntfFlags then
      Result := Supports(rightData.ClassType, leftData.Guid)
    else
      Result := GetInterfaceEntryByTypeInfo(rightData.ClassType, leftType) <> nil;
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

function IsAssignableFromRelaxed(leftType, rightType: PTypeInfo): Boolean;
begin
  Result := SameTypeInfo(leftType, rightType) or IsAssignableFrom(leftType, rightType);
end;

function IsNullable(typeInfo: PTypeInfo): Boolean;
const
  PrefixString = 'Nullable<';    // DO NOT LOCALIZE
begin
  Result := Assigned(typeInfo) and (typeInfo.Kind in [tkRecord{$IF Declared(tkMRecord)}, tkMRecord{$IFEND}])
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
    'Func<', 'Lazy<', 'ILazy<');

function GetLazyKind(typeInfo: PTypeInfo): TLazyKind;
var
  name: string;
begin
  if Assigned(typeInfo) then
  begin
    name := typeInfo.TypeName;
    for Result := lkFunc to High(TLazyKind) do
      if (StartsText(LazyPrefixStrings[Result], name)
        or ((Result = lkFunc) and StartsText('T' + LazyPrefixStrings[Result], name)))
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

  function CompareValueCurr(const A, B: Currency): TValueRelationship;
  begin
    if A = B then
      Result := EqualsValue
    else if A < B then
      Result := LessThanValue
    else
      Result := GreaterThanValue;
  end;

const
  EmptyResults: array[Boolean, Boolean] of Integer = ((0, -1), (1, 0));
var
  leftIsEmpty, rightIsEmpty: Boolean;
  leftValue, rightValue: TValue;
  leftObject, rightObject: TObject;
  equal: Boolean;
begin
  leftIsEmpty := left.IsEmpty;
  rightIsEmpty := right.IsEmpty;
  if leftIsEmpty or rightIsEmpty then
    Result := EmptyResults[leftIsEmpty, rightIsEmpty]
  else if left.IsOrdinal and right.IsOrdinal then
    Result := Math.CompareValue(left.AsOrdinal, right.AsOrdinal)
  else if left.IsFloat and right.IsFloat then
    if (left.TypeData.FloatType = ftCurr) and (right.TypeData.FloatType = ftCurr) then
      Result := CompareValueCurr(left.AsCurrency, right.AsCurrency)
    else
      Result := Math.CompareValue(left.AsExtended, right.AsExtended)
  else if left.IsString and right.IsString then
    Result := SysUtils.AnsiCompareStr(left.AsString, right.AsString)
  else if left.IsObject and right.IsObject then
  begin
    leftObject := left.AsObject;
    rightObject := right.AsObject;
    if (NativeInt(leftObject) or NativeInt(rightObject)) = 0 then
      Exit(0);
    if leftObject <> nil then
      equal := leftObject.Equals(rightObject)
    else
      equal := rightObject.Equals(leftObject);
    if equal then
      Result := 0
    else if NativeInt(leftObject) < NativeInt(rightObject) then
      Result := -1
    else if NativeInt(leftObject) > NativeInt(rightObject) then
      Result := 1
    else
      Result := 0;
  end
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
    tkChar:
      Result := SizeOf(AnsiChar){1};
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
    tkRecord{$IF Declared(tkMRecord)}, tkMRecord{$IFEND}:
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

function InterfaceToMethodPointer(const intf; index: Integer): TMethodPointer;
begin
  if Pointer(intf) = nil then
    Exit(nil);
  // 3 is offset of the first declared method in the interface, after QI, AddRef, Release
  TMethod(Result).Code := PPVtable(intf)^[index + 3];
  TMethod(Result).Data := Pointer(intf);
end;

function MethodReferenceToMethod(const methodRef): TMethod;
begin
  if Pointer(methodRef) = nil then
    Exit(Default(TMethod));
  // 3 is offset of Invoke, after QI, AddRef, Release
  Result.Code := PPVtable(methodRef)^[3];
  Result.Data := Pointer(methodRef);
end;

function MethodReferenceToMethodPointer(const methodRef): TMethodPointer;
begin
  if Pointer(methodRef) = nil then
    Exit(nil);
  // 3 is offset of Invoke, after QI, AddRef, Release
  TMethod(Result).Code := PPVtable(methodRef)^[3];
  TMethod(Result).Data := Pointer(methodRef);
end;

function MethodToMethodReference(const method: TMethod): IInterface;
begin
  Result := IInterface(TMethod(method).Data);
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
    stream.ReadBuffer(lock^, size);
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
    startPos, index: Integer;
    len: NativeInt;
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
    i: NativeInt;
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
{$R-}
  for i := leftArr.DimCount - 1 downto 0 do
  begin
    count := leftArr.Bounds[i].ElementCount;
    if count = 0 then
      Exit(True);
    if count <> rightArr.Bounds[i].ElementCount then
      Exit(False);
    bounds[leftArr.DimCount - 1 - i] := leftArr.Bounds[i];
  end;
{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
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
  {$Q-}
  Result := PPointer(IntPtr(classType) + IntPtr(index * SizeOf(Pointer)))^;
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
end;

type
  TIntfFlagEx = (ifHasGuid, ifDispInterface, ifDispatch, ifMethodInfo, ifUnused, ifUnused2, ifMethodReference);
  TIntfFlagsEx = set of TIntfFlagEx;

function HasMethodInfo(typeInfo: PTypeInfo): Boolean;
begin
  Result := Assigned(typeInfo) and (typeInfo.Kind = tkInterface)
    and (ifMethodInfo in TIntfFlagsEx(typeInfo.TypeData.IntfFlags));
end;

function IsMethodReference(typeInfo: PTypeInfo): Boolean;
begin
  Result := Assigned(typeInfo) and (typeInfo.Kind = tkInterface)
    and (ifMethodReference in TIntfFlagsEx(typeInfo.TypeData.IntfFlags));
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

function AtomicIncrement(var target: Integer; increment: Integer): Integer;
asm
{$IFDEF CPUX86}
  mov ecx,edx
  lock xadd [eax],edx
  add edx,ecx
  mov eax,edx
{$ENDIF}
{$IFDEF CPUX64}
  mov rax,rdx
  lock xadd [rcx],rdx
  add rax,rdx
{$ENDIF}
end;

function AtomicIncrement(var target: Int64; increment: Int64): Int64;
asm
{$IFDEF CPUX86}
  push ebx
  push esi
  mov esi,target
  mov eax,[esi]
  mov edx,[esi+4]
@@1:
  mov ebx,eax
  mov ecx,edx
  add ebx,low increment
  adc ecx,high increment
  lock cmpxchg8b [esi]
  jnz @@1
  add eax,low increment
  adc edx,high increment
  pop esi
  pop ebx
{$ENDIF}
{$IFDEF CPUX64}
  mov rax,rdx
  lock xadd [rcx],rdx
  add rax,rdx
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

function AtomicDecrement(var target: NativeInt; decrement: NativeInt): NativeInt;
asm
{$IFDEF CPUX86}
  neg edx
  mov ecx,edx
  lock xadd [eax],edx
  add edx,ecx
  mov eax,edx
{$ENDIF}
{$IFDEF CPUX64}
  neg rdx
  mov rax,rdx
  lock xadd [rcx],rdx
  add rax,rdx
{$ENDIF}
end;

function AtomicExchange(var target: Integer; value: Integer): Integer;
asm
{$IFDEF CPUX86}
  lock xchg [eax],edx
  mov eax,edx
{$ENDIF}
{$IFDEF CPUX64}
  lock xchg [rcx],rdx
  mov rax,rdx
{$ENDIF}
end;

function AtomicExchange(var target: NativeInt; value: NativeInt): NativeInt;
asm
{$IFDEF CPUX86}
  lock xchg [eax],edx
  mov eax,edx
{$ENDIF}
{$IFDEF CPUX64}
  lock xchg [rcx],rdx
  mov rax,rdx
{$ENDIF}
end;

function AtomicExchange(var target: Pointer; value: Pointer): Pointer;
asm
{$IFDEF CPUX86}
  lock xchg [eax],edx
  mov eax,edx
{$ENDIF}
{$IFDEF CPUX64}
  lock xchg [rcx],rdx
  mov rax,rdx
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

function AtomicCmpExchange(var target: NativeInt; newValue, comparand: NativeInt): NativeInt;
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

function AtomicCmpExchange(var target: Pointer; newValue, comparand: Pointer): Pointer;
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

procedure AtomicStore(var target: Int64;
  const {$IFDEF CPUX86}{$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}{$ENDIF}value: Int64); {$IFNDEF CPUX86}inline;{$ENDIF}
{$IFDEF CPUX86}
asm
{$IFDEF SUPPORTS_CONSTREF}
  movq xmm0,[edx]
{$ELSE}
  movq xmm0,[ebp+8]
{$ENDIF}
  movq [eax],xmm0
end;
{$ELSE}
begin
  target := value;
end;
{$ENDIF}

function AtomicLoad(var source: Int64): Int64; {$IFNDEF CPUX86}inline;{$ENDIF}
{$IFDEF CPUX86}
asm
  movq xmm0,[source]
  movd eax,xmm0
  psrldq xmm0,4
  movd edx,xmm0
end;
{$ELSE}
begin
  Result := source;
end;
{$ENDIF}

function AtomicExchangeAdd(var target: Int64; const value: Int64): Int64;
{$IFDEF ASSEMBLER}
asm
{$IFDEF CPUX86}
  push    ebx
  push    esi
  mov     esi,eax
  mov     eax,[esi]
  mov     edx,[esi+4]
@1:
  mov     ebx,eax
  mov     ecx,edx
  add     ebx,[ebp+8]
  adc     ecx,[ebp+12]
  lock cmpxchg8b [esi]
  jnz @1
  pop     esi
  pop     ebx
{$ELSE}
  mov rax,rdx
  lock xadd [rcx],rax
{$ENDIF}
end;
{$ELSE}
begin
  Result := AtomicIncrement(target, value) - value;
end;
{$ENDIF}

procedure IncUnchecked(var i: Integer; const n: Integer = 1); inline;
begin
  {$Q-}
  Inc(i, n);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
end;

function IsPowerOf2(value: NativeInt): Boolean;
begin
  Result := (value > 0) and (value and (value - 1) = 0);
end;

function NextPowerOf2(value: NativeInt): NativeInt;
{$IFDEF ASSEMBLER}
{$IFDEF CPUX86}
asm
  test eax, eax
  jle @negative
  bsr ecx, eax
  mov eax, 2
  shl eax, cl
  ret
@negative:
  mov eax, 1
end;
{$ELSE}
asm
  test rcx, rcx
  jle @negative
  bsr rcx, rcx
  mov eax, 2
  shl rax, cl
  ret
@negative:
  mov eax, 1
end;
{$ENDIF}
{$ELSE}
begin
  Result := 1;
  while (Result <= value) and (Result > 0) do
    Result := Result shl 1;
end;
{$ENDIF}

{$Q-}
{$R-}
function DynArrayLength(const A: Pointer): NativeInt;
begin
  Result := NativeInt(A);
  if Result <> 0 then
    {$POINTERMATH ON}
    Result := PNativeInt(Result)[-1];
    {$POINTERMATH OFF}
end;

function DynArrayHigh(const A: Pointer): NativeInt;
begin
  Result := NativeInt(A);
  if Result <> 0 then
    {$POINTERMATH ON}
    Result := PNativeInt(Result)[-1];
    {$POINTERMATH OFF}
  Dec(Result);
end;
{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
{$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

function GetEqualsOperator(const typeInfo: PTypeInfo): TRttiMethod;
const
  EqualsOperatorName = '&op_Equality';
var
  method: TRttiMethod;
  parameters: TArray<TRttiParameter>;
begin
  for method in TType.GetType(typeInfo).GetMethods(EqualsOperatorName) do
  begin
    if method.MethodKind <> mkOperatorOverload then
      Continue;
    if method.CallingConvention <> ccReg then
      Continue;
    parameters := method.GetParameters;
    if (Length(parameters) = 2)
      and (parameters[0].ParamType.Handle = typeInfo) and (parameters[1].ParamType.Handle = typeInfo)
      and (pfConst in parameters[0].Flags) and (pfConst in parameters[1].Flags) then
     Exit(method);
  end;
  Result := nil;
end;

function OutOfMemoryError: Integer;
begin
  SysUtils.OutOfMemoryError;
  Result := 0;
end;

function GrowCapacity(oldCapacity: Integer): Integer;
begin
  if oldCapacity >= 4 then
  begin
    if oldCapacity < 1024 then
      Result := oldCapacity * 2
    else
    begin
      Result := oldCapacity + oldCapacity shr 1;
      if Result < 0 then
        Exit(OutOfMemoryError);
    end;
  end
  else
    Result := 4;
end;

function GrowCapacity(oldCapacity, newCount: Integer): Integer;
begin
  Result := oldCapacity;
  repeat
    if Result >= 1024 then
      Result := Result + Result shr 1
    else
      if Result >= 4 then
        Result := Result * 2
      else
        Result := 4;
    if Result < 0 then
      Exit(OutOfMemoryError);
  until Result >= newCount;
end;

{$IFNDEF DELPHIX_BERLIN_UP}
procedure CopyRecord(Dest, Source, TypeInfo: Pointer);
asm
  jmp System.@CopyRecord
end;

procedure FinalizeRecord(P: Pointer; TypeInfo: Pointer);
asm
  jmp System.@FinalizeRecord
end;
{$ENDIF}

function NopRef(inst: Pointer): Integer; //FI:O804
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; //FI:O804
begin
  Result := E_NOINTERFACE;
end;

function RecAddRef(inst: Pointer): Integer;
type
  PIntfRef = ^TIntfRef;
  TIntfRef = record
    VTable: Pointer;
    RefCount: Integer;
  end;
begin
  Result := AtomicIncrement(PIntfRef(inst).RefCount);
end;

procedure IntfAssign(const source: IInterface; var target: IInterface);
begin
  target := source;
end;

procedure CheckIndex(index, size: Integer);
begin
  if Cardinal(index) >= Cardinal(size) then RaiseHelper.ArgumentOutOfRange_Index;
end;

procedure CheckRange(index, count, size: Integer);
begin
  if Cardinal(index) > Cardinal(size) then RaiseHelper.ArgumentOutOfRange_Index;
  {$Q-}
  if (count < 0) or (index > size - count) then RaiseHelper.ArgumentOutOfRange_Count;
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
end;

{$IFNDEF MSWINDOWS}
function RegisterExpectedMemoryLeak(P: Pointer): Boolean;
var
  MemoryManager: TMemoryManagerEx;
begin
  GetMemoryManager(MemoryManager);
  Result := MemoryManager.RegisterExpectedMemoryLeak(P);
end;
{$ENDIF}

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
    tkString:
      Result := GetTypeData(TypeInfo)^.MaxLength > SizeOf(Pointer);
{$IF declared(tkMRecord)}
    tkMRecord:
      Result := True;
{$IFEND}
  else
    Result := False;
  end;
end;

{$IFNDEF DELPHIX_TOKYO_UP}
function StrToUInt(const s: string): Cardinal;
var
  i: Int64;
  e: Integer;
begin
  Val(s, i, e);
  if (e <> 0) or not((Low(Cardinal) <= i) and (i <= High(Cardinal))) then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [s]);
  Result := Cardinal(i);
end;
{$ENDIF}

{$IFNDEF DELPHIXE6_UP}
function StrToUInt64(const s: string): UInt64;
var
  e: Integer;
begin
  Val(s, Result, e);
  if e <> 0 then raise EConvertError.CreateResFmt(@SInvalidInteger, [s]);
end;
{$ENDIF}

procedure __SuppressWarning(var value); inline;
begin
end;

{$ENDREGION}


{$REGION 'ReadWriteLock'}

procedure ReadWriteLock.EnterRead;
var
  current: NativeInt;
begin
  while True do
  begin
    current := NativeInt(Lock) and not 1;
    if AtomicCmpExchange(NativeInt(Lock), current + 4, current) = current then
      Break;

    {$IFDEF CPUX86}asm pause end;{$ELSE}YieldProcessor;{$ENDIF}
  end;
end;

procedure ReadWriteLock.LeaveRead;
begin
  AtomicDecrement(NativeInt(Lock), 4);
end;

procedure ReadWriteLock.EnterUpgradableRead;
var
  current: NativeInt;
begin
  while True do
  begin
    current := NativeInt(Lock) and not 3;
    if AtomicCmpExchange(NativeInt(Lock), current + 2, current) = current then
      Break;

    {$IFDEF CPUX86}asm pause end;{$ELSE}YieldProcessor;{$ENDIF}
  end;

  NativeUInt(ThreadId) := GetCurrentThreadId;
end;

procedure ReadWriteLock.LeaveUpgradableRead;
begin
  NativeUInt(ThreadId) := 0;
  AtomicDecrement(NativeInt(Lock), 2);
end;

procedure ReadWriteLock.EnterWrite;
var
  current: NativeInt;
begin
  while True do
  begin
    while True do
    begin
      current := NativeInt(Lock) and not 1;
      if AtomicCmpExchange(NativeInt(Lock), current + 1, current) = current then
        Break;

      {$IFDEF CPUX86}asm pause end;{$ELSE}YieldProcessor;{$ENDIF}
    end;

    if (NativeInt(Lock) and 2 = 2) and (NativeUInt(ThreadId) <> GetCurrentThreadId) then
      AtomicDecrement(NativeInt(Lock), 1)
    else
      Break;
  end;

  while NativeInt(Lock) > 3 do;
end;

procedure ReadWriteLock.LeaveWrite;
begin
  AtomicDecrement(NativeInt(Lock), 1);
end;

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
  else
    __SuppressWarning(Result);
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
  p: PShortString;
  i: NativeInt;
begin
  Guard.CheckTypeKind<T>(tkEnumeration, 'T');
  typeData := GetTypeData(TypeInfo(T));
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
  p := @typedata.NameList;
  for i := Low(Result) to High(Result) do
  begin
    Result[i] := UTF8ToString(p^);
    Inc(PByte(p), Length(p^) + 1);
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
  i: NativeInt;
begin
  Guard.CheckTypeKind<T>(tkEnumeration, 'T');
  typeData := GetTypeData(TypeInfo(T));
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
  for i := Low(Result) to High(Result) do
    Result[i] := Integer(i);
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

constructor TBaseAttribute.Create; //FI:W525
begin //FI:W519
end;

{$ENDREGION}


{$REGION 'DefaultAttribute'}

{$IFNDEF DELPHIXE3_UP}
constructor DefaultAttribute.Create(const defaultValue: Integer);
begin
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: Boolean);
begin
  fValue := Ord(defaultValue);
end;

constructor DefaultAttribute.Create(const defaultValue: Cardinal);
begin
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: string);
begin
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: Extended);
begin
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: Int64);
begin
  fValue := defaultValue;
end;

constructor DefaultAttribute.Create(const defaultValue: UInt64);
begin
  fValue := defaultValue;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'ManagedAttribute'}

constructor ManagedAttribute.Create(createInstance: Boolean); //FI:W525
begin
  fCreateInstance := createInstance;
end;

constructor ManagedAttribute.Create(instanceClass: TClass); //FI:W525
begin
  fCreateInstance := True;
  fInstanceClass := instanceClass;
end;

constructor ManagedAttribute.Create(const initializer: TFieldInitializer); //FI:W525
begin
  fInitializer := initializer;
end;

{$ENDREGION}


{$REGION 'TInitTable'}

class constructor TInitTable.Create;
begin
{$IFDEF USE_VMTAUTOTABLE}
  InitTables := TObjectList<TInitTable>.Create;
{$ELSE}
  InitTables := TObjectDictionary<TClass,TInitTable>.Create([Generics.Collections.doOwnsValues]);
{$ENDIF}
end;

class destructor TInitTable.Destroy;
begin
  InitTables.Free;
end;

constructor TInitTable.Create(classType: TClass);
var
  t: TRttiType;
  types: TArray<TRttiType>;
  i: NativeInt;
  f: TRttiField;
  p: TRttiProperty;
  a: TCustomAttribute;
  setter: Pointer;
begin
  t := TType.GetType(classType);

  repeat
    SetLength(types, Length(types) + 1);
    types[High(types)] := t;
    t := t.BaseType;
  until t.Handle = TypeInfo(TObject);

  for i := High(types) downto 0 do
  begin
    t := types[i];

    for f in t.GetDeclaredFields do
    begin
      if not Assigned(f.FieldType) then
        Continue;

      for a in f.FieldType.GetAttributes do
        if a is DefaultAttribute then
          AddDefaultField(f.FieldType.Handle, DefaultAttribute(a).Value, f.Offset)
        else if a is ManagedAttribute then
          AddManagedField(f, ManagedAttribute(a));

      for a in f.GetAttributes do
        if a is DefaultAttribute then
          AddDefaultField(f.FieldType.Handle, DefaultAttribute(a).Value, f.Offset)
        else if a is ManagedAttribute then
          AddManagedField(f, ManagedAttribute(a));
    end;

    for p in t.GetDeclaredProperties do
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
              Integer(IntPtr(setter) and not PROPSLOT_MASK))
          else
            AddDefaultProperty(p.PropertyType.Handle, DefaultAttribute(a).Value,
              TRttiInstanceProperty(p).PropInfo);
        end;
  end;
end;

destructor TInitTable.Destroy; //FI:W504
var
  i: NativeInt;
begin
  for i := 0 to High(DefaultFields) do
    FreeAndNil(DefaultFields[i]);
  for i := 0 to High(ManagedFields) do
    FreeAndNil(ManagedFields[i]);
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
    tkChar:
      defaultField  := TDefaultField<AnsiChar>.Create(offset, value);
    tkFloat:
      if (fieldType = TypeInfo(TDateTime)) and (VarType(value) = varUString) then
        defaultField := TDefaultField<TDateTime>.Create(offset, StrToDateTime(value, ISO8601FormatSettings))
      else if (fieldType = TypeInfo(TDate)) and (VarType(value) = varUString) then
        defaultField := TDefaultField<TDate>.Create(offset, StrToDate(value, ISO8601FormatSettings))
      else if (fieldType = TypeInfo(TTime)) and (VarType(value) = varUString) then
        defaultField := TDefaultField<TTime>.Create(offset, StrToTime(value, ISO8601FormatSettings))
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
    tkWString:
      defaultField := TDefaultField<WideString>.Create(offset, value);
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
    DefaultFieldCount := Integer(Length(DefaultFields) + 1);
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
    tkChar:
      defaultField  := TDefaultProperty<AnsiChar>.Create(propInfo, value);
    tkFloat:
      if (fieldType = TypeInfo(TDateTime)) and (VarType(value) = varUString) then
        defaultField := TDefaultProperty<TDateTime>.Create(propInfo, StrToDateTime(value, ISO8601FormatSettings))
      else if (fieldType = TypeInfo(TDate)) and (VarType(value) = varUString) then
        defaultField := TDefaultProperty<TDate>.Create(propInfo, StrToDate(value, ISO8601FormatSettings))
      else if (fieldType = TypeInfo(TTime)) and (VarType(value) = varUString) then
        defaultField := TDefaultProperty<TTime>.Create(propInfo, StrToTime(value, ISO8601FormatSettings))
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
    tkWString:
      defaultField := TDefaultProperty<WideString>.Create(propInfo, value);
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
    DefaultFieldCount := Integer(Length(DefaultFields) + 1);
    SetLength(DefaultFields, DefaultFieldCount);
    DefaultFields[DefaultFieldCount - 1] := defaultField;
  end;
end;

procedure TInitTable.AddManagedField(const field: TRttiField;
  const attribute: ManagedAttribute);
var
  fieldType: PTypeInfo;
  offset: Integer;
  createInstance: Boolean;
  cls: TClass;
  initializer: TFieldInitializer;
  managedField: TFinalizableField;
  entry: PInterfaceEntry;
begin
  fieldType := field.FieldType.Handle;
  offset := field.Offset;
  createInstance := attribute.CreateInstance;
  cls := attribute.InstanceClass;
  initializer := attribute.Initializer;
  case fieldType.Kind of
    tkClass:
    begin
      if not Assigned(initializer) and not Assigned(cls) and createInstance then
        cls := fieldType.TypeData.ClassType;
      managedField := TManagedObjectField.Create(offset, fieldType, initializer, cls);
    end;
    tkInterface:
    begin
      if Assigned(cls) then
      begin
        entry := GetInterfaceEntryByTypeInfo(cls, fieldType);
        if entry = nil then
          raise EInvalidOperationException.CreateFmt(
            'class %s is not compatible with interface %s (field %s)', [
            cls.ClassName, fieldType.TypeName, field.Name]);
      end
      else
        entry := nil;
      managedField := TManagedInterfaceField.Create(offset, fieldType, initializer, cls, entry);
    end;
  else
    managedField := TInitializerField.Create(offset, fieldType, initializer);
  end;
  if managedField <> nil then
  begin
    ManagedFieldCount := Integer(Length(ManagedFields) + 1);
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

{$R-}
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
{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}

{$ENDREGION}


{$REGION 'TInitTable.TDefaultField<T>'}

constructor TInitTable.TDefaultField<T>.Create(offset: Integer; const value: Variant); //FI:W525
begin
  fOffset := offset;
  TValue.FromVariant(value).AsType(TypeInfo(T), fValue); // TODO
end;

procedure TInitTable.TDefaultField<T>.InitializeValue(instance: Pointer);
begin
  PT(PByte(instance) + fOffset)^ := fValue;
end;

{$ENDREGION}


{$REGION 'TInitTable.TDefaultProperty<T>'}

constructor TInitTable.TDefaultProperty<T>.Create(propInfo: PPropInfo; const value: Variant); //FI:W525
begin
  fPropInfo := propInfo;
  TValue.FromVariant(value).AsType(TypeInfo(T), fValue); // TODO
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


{$REGION 'TInitTable.TInitializerField'}

constructor TInitTable.TInitializerField.Create(offset: Integer; //FI:W525
  fieldType: PTypeInfo; const initializer: TFieldInitializer);
begin
  fOffset := offset;
  fFieldType := fieldType;
  fInitializer := initializer;
end;

procedure TInitTable.TInitializerField.FinalizeValue(instance: Pointer);
begin
end;

procedure TInitTable.TInitializerField.InitializeValue(instance: Pointer);
begin
  fInitializer(fFieldType, Pointer(PByte(instance) + fOffset)^);
end;

{$ENDREGION}


{$REGION 'TInitTable.TManagedObjectField'}

constructor TInitTable.TManagedObjectField.Create(offset: Integer;
  fieldType: PTypeInfo; const initializer: TFieldInitializer; cls: TClass);
begin
  inherited Create(offset, fieldType, initializer);
  fCls := cls;
  if Assigned(cls) and not Assigned(initializer) then
    fCtor := TActivator.FindConstructor(cls);
end;

procedure TInitTable.TManagedObjectField.FinalizeValue(instance: Pointer);
begin
  FreeAndNil(PObject(PByte(instance) + fOffset)^);
end;

procedure TInitTable.TManagedObjectField.InitializeValue(instance: Pointer);
begin
  if Assigned(fCtor) then
    TObject(Pointer(PByte(instance) + fOffset)^) := TObject(fCtor(fCls))
  else if Assigned(fInitializer) then
    fInitializer(fFieldType, Pointer(PByte(instance) + fOffset)^);
end;

{$ENDREGION}


{$REGION 'TInitTable.TManagedInterfaceField'}

constructor TInitTable.TManagedInterfaceField.Create(offset: Integer;
  fieldType: PTypeInfo; const initializer: TFieldInitializer; cls: TClass;
  entry: PInterfaceEntry);
begin
  inherited Create(offset, fieldType, initializer, cls);
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
    InvokeImplGetter(obj, fEntry.ImplGetter, IInterface(Result));
  end;
end;

procedure TInitTable.TManagedInterfaceField.FinalizeValue(instance: Pointer);
begin
end;

procedure TInitTable.TManagedInterfaceField.InitializeValue(instance: Pointer);
begin
  if Assigned(fCtor) then
    PPointer(PByte(instance) + fOffset)^ := CreateInstance
  else if Assigned(fInitializer) then
    fInitializer(fFieldType, PPointer(PByte(instance) + fOffset)^);
end;

{$ENDREGION}


{$REGION 'TManagedObject'}

procedure TManagedObject.FreeInstance;
begin
  GetInitTable(ClassType).CleanupInstance(Self);
  inherited FreeInstance;
end;

class function TManagedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  GetInitTable(Self).InitInstance(Result);
end;

{$ENDREGION}


{$REGION 'TManagedInterfacedObject'}

procedure TManagedInterfacedObject.FreeInstance;
begin
  GetInitTable(ClassType).CleanupInstance(Self);
  inherited FreeInstance;
end;

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
    FValueData := nil;
    Pointer(FValueData) := Nop_Instance;
  end;
end;

function TValueHelper.AsPointer: Pointer;
begin
  case Kind of
    tkPointer:
      Result := TValueData(Self).FAsPointer;
    tkClass:
      Result := AsObject;
    tkInterface:
      Result := Pointer(AsInterface);
  else
    Guard.RaiseInvalidTypeCast(TypeInfo, System.TypeInfo(Pointer));
    Result := nil;
  end;
end;

function TValueHelper.AsType<T>: T;
begin
  if not TryAsInterface(System.TypeInfo(T), Result) then
  if not TryAsType(System.TypeInfo(T), Result) then
    Guard.RaiseInvalidTypeCast(TypeInfo, System.TypeInfo(T));
end;

procedure TValueHelper.AsTypeRelaxed(typeInfo: PTypeInfo; out target);
begin
  if SameTypeInfo(TValueData(Self).FTypeInfo, typeInfo) then
    typeInfo := TValueData(Self).FTypeInfo;
  AsType(typeInfo, target);
end;

procedure TValueHelper.AsType(typeInfo: PTypeInfo; out target);
begin
  if not TryAsInterface(typeInfo, target) then
  if not TryAsType(typeInfo, target) then
    Guard.RaiseInvalidTypeCast(Self.TypeInfo, typeInfo);
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
var
  valueSingle: Single;
  valueDouble: Double;
begin
  if right.TryAsType(TypeInfo(Single), valueSingle) then
    Result := Math.SameValue(left.AsInteger, valueSingle)
  else if right.TryAsType(TypeInfo(Double), valueDouble) then
    Result := Math.SameValue(left.AsInteger, valueDouble)
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
    ftSingle: Result := Math.SameValue(TValueData(left).FAsSingle, right.AsInteger);
    ftDouble: Result := Math.SameValue(TValueData(left).FAsDouble, right.AsInteger);
  else
    Result := Math.SameValue(left.AsExtended, right.AsInteger);
  end;
end;

function EqualsFloat2Float(const left, right: TValue): Boolean;
begin
  case left.TypeData.FloatType of
    ftSingle:
      case right.TypeData.FloatType of
        ftSingle: Result := Math.SameValue(TValueData(left).FAsSingle, TValueData(right).FAsSingle);
        ftDouble: Result := Math.SameValue(TValueData(left).FAsSingle, TValueData(right).FAsDouble);
        ftCurr: Result := Math.SameValue(TValueData(left).FAsSingle, TValueData(right).FAsCurr);
      else
        Result := Math.SameValue(TValueData(left).FAsSingle, right.AsExtended);
      end;
    ftDouble:
      case right.TypeData.FloatType of
        ftSingle: Result := Math.SameValue(TValueData(left).FAsDouble, TValueData(right).FAsSingle);
        ftDouble: Result := Math.SameValue(TValueData(left).FAsDouble, TValueData(right).FAsDouble);
        ftCurr: Result := Math.SameValue(TValueData(left).FAsDouble, TValueData(right).FAsCurr);
      else
        Result := Math.SameValue(TValueData(left).FAsDouble, right.AsExtended);
      end;
    ftCurr:
      case right.TypeData.FloatType of
        ftSingle: Result := Math.SameValue(TValueData(left).FAsCurr, TValueData(right).FAsSingle);
        ftDouble: Result := Math.SameValue(TValueData(left).FAsCurr, TValueData(right).FAsDouble);
        ftCurr: Result := TValueData(left).FAsCurr = TValueData(right).FAsCurr;
      else
        Result := Math.SameValue(TValueData(left).FAsCurr, right.AsExtended);
      end;
  else
    case right.TypeData.FloatType of
      ftSingle: Result := Math.SameValue(left.AsExtended, TValueData(right).FAsSingle);
      ftDouble: Result := Math.SameValue(left.AsExtended, TValueData(right).FAsDouble);
      ftCurr: Result := Math.SameValue(left.AsExtended, TValueData(right).FAsCurr);
    else
      Result := Math.SameValue(left.AsExtended, right.AsExtended);
    end;
  end;
end;

function EqualsFloat2Int64(const left, right: TValue): Boolean;
begin
  case left.TypeData.FloatType of
    ftSingle: Result := Math.SameValue(TValueData(left).FAsSingle, right.AsInt64);
    ftDouble: Result := Math.SameValue(TValueData(left).FAsDouble, right.AsInt64);
  else
    Result := Math.SameValue(left.AsExtended, right.AsInt64);
  end;
end;

function EqualsInt642Int(const left, right: TValue): Boolean;
begin
  Result := left.AsInt64 = right.AsInteger;
end;

function EqualsInt64ToFloat(const left, right: TValue): Boolean;
var
  valueSingle: Single;
  valueDouble: Double;
begin
  if right.TryAsType(TypeInfo(Single), valueSingle) then
    Result := Math.SameValue(left.AsInt64, valueSingle)
  else if right.TryAsType(TypeInfo(Double), valueDouble) then
    Result := Math.SameValue(left.AsInt64, valueDouble)
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

function EqualsMeth2Meth(const left, right: TValue): Boolean;
begin
  Result := (left.TypeInfo = right.TypeInfo)
    and (TValueData(left).FAsMethod.Code = TValueData(right).FAsMethod.Code)
    and (TValueData(left).FAsMethod.Data = TValueData(right).FAsMethod.Data);
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
  method: TRttiMethod;
begin
  if (left.TypeInfo = TypeInfo(TValue)) and (right.TypeInfo = TypeInfo(TValue)) then
    Exit(PValue(left.GetReferenceToRawData).Equals(
      PValue(right.GetReferenceToRawData)^));

  method := GetEqualsOperator(left.TypeInfo);
  if Assigned(method) then
    Result := method.Invoke(nil, [left, right]).AsBoolean
  else
    Result := RawEquals(left.TypeInfo.RttiType);
end;

function EqualsArray2Array(const left, right: TValue): Boolean;
var
  len, i: Integer;
begin
  if (left.Kind = tkDynArray) and (right.Kind = tkDynArray)
    and (PPointer(left.GetReferenceToRawData)^ = PPointer(right.GetReferenceToRawData)^) then
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
    ),
    // tkMethod
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsMeth2Meth, EqualsFail, // TODO: tkMethod
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
    ),
    // tkArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsArray2Array, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsArray2Array, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsRec2Rec{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
    ),
    // tkDynArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsArray2Array, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsArray2Array, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsPointer2Pointer, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
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
      EqualsFail, EqualsFail {$IF Declared(tkMRecord)}, EqualsFail{$IFEND}
    )
{$IF Declared(tkMRecord)}
    // tkMRecord
    , (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsRec2Rec,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure, tkMRecord
      EqualsFail, EqualsFail, EqualsRec2Rec
    )
{$IFEND}
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
    AsObject.Free;
end;

class function TValueHelper.From(const value; typeInfo: PTypeInfo): TValue;
begin
  TValue.Make(@value, typeInfo, Result);
end;

class function TValueHelper.From(instance: TObject; classType: TClass): TValue;
begin
  TValue.Make(NativeInt(instance), classType.ClassInfo, Result);
end;

class function TValueHelper.FromFloat(typeInfo: PTypeInfo;
  value: Extended): TValue;
begin
  Result.Init(typeInfo);
  case typeInfo.TypeData.FloatType of
    ftSingle: TValueData(Result).FAsSingle := value;
    ftDouble: TValueData(Result).FAsDouble := value;
    ftExtended: TValueData(Result).FAsExtended := value;
    ftComp: TValueData(Result).FAsComp := value;
    ftCurr: TValueData(Result).FAsCurr := value;
  end;
end;

class function TValueHelper.FromVariant(const value: Variant): TValue;

  procedure FromCustomVariant(const value: Variant; var result: TValue);
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
    typeName, tmpStr: string;
    i: Integer;
    tmpInt64: Int64;
    tmpDouble: Double;
    tmpCurrency: Currency;
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
          begin
            tmpStr := VarToStr(value);
            if TryStrToInt64(tmpStr, tmpInt64) then
              Result := tmpInt64
            else
            begin
              tmpDouble := Double(value);
              if FloatToStr(tmpDouble) = tmpStr then
                Result := tmpDouble
              else if TryStrToCurr(tmpStr, tmpCurrency) and (CurrToStr(tmpCurrency) = tmpStr) then
                Result := tmpCurrency
              else
                Result := tmpStr;
            end;
          end;
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
    varSingle: Result := TVarData(value).VSingle;
    varDouble: Result := TVarData(value).VDouble;
    varCurrency: Result := TVarData(value).VCurrency;
    varDate: Result := TVarData(value).VDate;
    varOleStr: Result := string(TVarData(value).VOleStr);
    varDispatch: Result := From(TVarData(value).VDispatch, System.TypeInfo(IDispatch));
    varError: Result := From(TVarData(value).VError, System.TypeInfo(HRESULT));
    varUnknown: Result := From(TVarData(value).VUnknown, System.TypeInfo(IInterface));
    varByte: Result := TVarData(value).VByte;
    varWord: Result := TVarData(value).VWord;
    varLongWord: Result := TVarData(value).VLongWord;
    varInt64: Result := TVarData(value).VInt64;
    varUInt64: Result := TVarData(value).VUInt64;
    varString: Result := string(AnsiString(TVarData(value).VString));
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
    vtPointer: Result := TValue.From(value.VPointer, System.TypeInfo(Pointer));
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
    vtInterface: Result := TValue.From(value.VInterface, System.TypeInfo(IInterface));
{$IF Declared(WideString)}
    vtWideString: Result := WideString(value.VWideString);
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

function TValueHelper.GetTypeKind: TTypeKind;
begin
  if (TValueData(Self).FTypeInfo <> nil) and (TValueData(Self).FValueData <> nil) then
    Result := TValueData(Self).FTypeInfo.Kind
  else
    Result := tkUnknown;
end;

function TValueHelper.GetValueType: TRttiType;
begin
  Result := TypeInfo.RttiType;
end;

function TValueHelper.IsBoolean: Boolean;
begin
  Result := IsType(System.TypeInfo(Boolean));
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

function TValueHelper.IsType(ATypeInfo: PTypeInfo): Boolean;
var
  unused: TValue;
begin
  Result := TryCast(ATypeInfo, unused);
end;

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

{$IFNDEF DELPHIX_ALEXANDRIA_UP}
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
{$ENDIF}

class function TValueHelper.&&op_Implicit(const value: TGUID): TValue;
begin
  Result := TValue.From(value);
end;

class function TValueHelper.&&op_Inequality(const left, right: TValue): Boolean;
begin
  Result := not left.Equals(right);
end;

{$IFNDEF DELPHIXE2_UP}{$IFNDEF STACKFRAMES_ON}{$W+}{$ENDIF}{$ENDIF}
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
{$IFNDEF DELPHIXE2_UP}{$IFNDEF STACKFRAMES_ON}{$W-}{$ENDIF}{$ENDIF}

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

  function TryConvertToVariant(var returnValue: Variant): Boolean;
  var
    value: TValue;
  begin
    Result := Assigned(fValueConverterCallback)
      and fValueConverterCallback(Self, System.TypeInfo(Variant), value, EmptyValue);

    if Result then
      returnValue := value.AsVariant
    else
      returnValue := Null;
  end;

  function TryConvertStreamToVariant(var returnValue: Variant): Boolean;
  var
    obj: TObject;
    stream: TStream;
    persist: IStreamPersist;
  begin
    obj := AsObject;
    if obj is TStream then
    begin
      stream := TStream(obj);
      stream.Position := 0;
      returnValue := StreamToVariant(stream);
      Result := True;
    end
    else if Supports(obj, IStreamPersist, persist) then
    begin
      stream := TMemoryStream.Create;
      try
        persist.SaveToStream(stream);
        stream.Position := 0;
        returnValue := StreamToVariant(stream);
        Result := True;
      finally
        stream.Free;
      end;
    end
    else
      Result := False;
  end;

  function TryConvertRecordToVariant(var returnValue: Variant): Boolean;
  var
    value: TValue;
    guid: TGUID;
  begin
    if IsNullable(TypeInfo) and TryGetNullableValue(value) then
    begin
      returnValue := value.ToVariant;
      Exit(True);
    end;

    if IsLazyType(TypeInfo) and TryGetLazyValue(value) then
    begin
      returnValue := value.ToVariant;
      Exit(True);
    end;

    if TypeInfo = System.TypeInfo(TGUID) then
    begin
      AsType(System.TypeInfo(TGUID), guid);
      returnValue := guid.ToString;
      Exit(True);
    end;

    Result := False;
  end;

var
  arr: PPointer;
begin
  Result := Null;
  case Kind of
    tkEnumeration:
      if IsBoolean then
        Exit(AsBoolean)
      else
        Exit(AsOrdinal);
    tkFloat:
      if (TypeInfo = System.TypeInfo(TDateTime))
        or (TypeInfo = System.TypeInfo(TDate))
        or (TypeInfo = System.TypeInfo(TTime)) then
        Exit(TValueData(Self).FAsDouble)
      else if TypeInfo = System.TypeInfo(Currency) then
        Exit(AsCurrency)
      else
        Exit(AsExtended);
    tkRecord:
      if TryConvertRecordToVariant(Result) then
        Exit;
    tkClass:
      if TryConvertToVariant(Result)
        or TryConvertStreamToVariant(Result) then
        Exit;
    tkInterface:
      Exit(AsInterface);
    tkDynArray:
    begin
      arr := GetReferenceToRawData;
      DynArrayToVariant(Result, arr^, TypeInfo);
      Exit;
    end
  else
    Exit(AsVariant);
  end;
  TryConvertToVariant(Result);
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
    if Kind = tkClass then
    begin
      obj := TObject(TValueData(Self).FAsObject);
      typeData := typeInfo.TypeData;
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
    TValueData(Self).FValueData.ExtractRawData(@Intf);
end;

function TValueHelper.TryAsType(typeInfo: PTypeInfo; out target): Boolean;
var
  value: TValue;
begin
  Result := TryCast(typeInfo, value);
  if Result then
    value.ExtractRawData(@target);
end;

function TValueHelper.TryCast(ATypeInfo: PTypeInfo;
  out AResult: TValue): Boolean;
begin
  // fix wrong logic in RTL:
  // typecasting a TValue that contains a reference type that is nil succeeds
  // in all cases which clearly is against the otherwise strict type cast rules

  if not ((TValueData(Self).FTypeInfo = nil) or (TValueData(Self).FValueData = nil)) then
    if IsEmpty and Assigned(ATypeInfo) and (Kind <> ATypeInfo.Kind) then
      case Kind of
        tkClass:
          if not (ATypeInfo.Kind in [tkVariant, tkInterface, tkPointer]) then
            Exit(False);
        tkInterface:
          if not (ATypeInfo.Kind in [tkVariant, tkPointer]) then
            Exit(False);
        tkDynArray:
          if TValueData(Self).FTypeInfo <> ATypeInfo then
            Exit(False);
        tkPointer:;
      else
        Exit(False);
      end;
  Result := TValueHack(Self).TryCast(ATypeInfo, AResult);
  if Result then
    TValueData(AResult).FTypeInfo := ATypeInfo;
end;


{$REGION 'Conversion functions'}
type
  TConvertFunc = function(const source: TValue; target: PTypeInfo;
    var value: TValue; const formatSettings: TFormatSettings): Boolean;

function ConvFail(const source: TValue; target: PTypeInfo; var value: TValue; //FI:O804
  const formatSettings: TFormatSettings): Boolean; //FI:O804
begin
  Result := False;
end;

function ConvClass2Class(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
begin
  Result := source.TryCast(target, value);
end;

function ConvClass2Enum(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
begin
  Result := target = TypeInfo(Boolean);
  if Result then
    value := source.AsObject <> nil;
end;

function ConvFloat2Ord(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
begin
  Result := Frac(source.AsExtended) = 0;
  if Result then
    value := TValue.FromOrdinal(target, Trunc(source.AsExtended));
end;

function ConvFloat2Str(const source: TValue; target: PTypeInfo;
  var value: TValue; const formatSettings: TFormatSettings): Boolean;
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
  var value: TValue; const formatSettings: TFormatSettings): Boolean;
begin
  Result := ConvClass2Class(source.AsInterface as TObject, target, value, formatSettings);
end;

function ConvIntf2Intf(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
var
  intf: IInterface;
begin
  Result := source.TryAsInterface(target, intf);
  if Result then
    TValue.Make(@intf, target, value)
  else
    value := TValue.Empty;
end;

function ConvOrd2Float(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
begin
  value := TValue.FromFloat(target, source.AsOrdinal);
  Result := True;
end;

function ConvOrd2Ord(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
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

function ConvOrd2Str(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
var
  temp: TValue;
begin
  temp := source.ToString;
  Result := temp.TryCast(target, value);
end;

function ConvRec2Meth(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
begin
  Result := source.TypeInfo = TypeInfo(TMethod);
  if Result then
  begin
    value := TValue.From(source.GetReferenceToRawData^, target);
    Result := True;
  end
end;

function ConvStr2Enum(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
var
  temp: Integer;
begin
  temp := GetEnumValue(target, source.AsString);
  Result := (temp >= 0) or (target.TypeData.MinValue < 0);
  if Result then
    value := TValue.FromOrdinal(target, temp);
end;

function ConvStr2Float(const source: TValue; target: PTypeInfo;
  var value: TValue; const formatSettings: TFormatSettings): Boolean;
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
      value := TValue.From(d, TypeInfo(TDateTime));
  end else
  if target = TypeInfo(TDate) then
  begin
    Result := TryStrToDate(s, d, formatSettings);
    if Result then
      value := TValue.From(d, TypeInfo(TDate));
  end else
  if target = TypeInfo(TTime) then
  begin
    Result := TryStrToTime(s, d, formatSettings);
    if Result then
      value := TValue.From(d, TypeInfo(TTime));
  end else
  begin
    Result := TryStrToFloat(s, f, formatSettings);
    if Result then
      value := TValue.FromFloat(target, f);
  end;
end;

function ConvStr2Ord(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
var
  s: string;
  i: Int64;
begin
  s := source.AsString;
  Result := TryStrToInt64(s, i);
  if not Result and SameText(Trim(s), 'maxint') then
  begin
    Result := True;
    i := MaxInt;
  end;
  if Result then
    value := TValue.FromOrdinal(target, i);
end;

procedure MakeDynArray(typeInfo: PTypeInfo; count: NativeInt; var result: TValue);
var
  p: Pointer;
begin
  p := nil;
  DynArraySetLength(p, typeInfo, 1, @count);
  TValue.MakeWithoutCopy(@p, typeInfo, result);
end;

function SplitString(const s, delimiter: string): TStringDynArray;

  function ScanChar(const s: string; var index: Integer): Boolean;
  var
    level: Integer;
  begin
    Result := False;
    level := 0;
    while index <= Length(s) do
    begin
      case s[index] of
        '[': Inc(level);
        ']': Dec(level);
      else
        if Copy(s, index, Length(delimiter)) = delimiter then
          if level = 0 then
            Exit(True);
      end;
      Inc(index);
      Result := level = 0;
    end;
  end;

var
  startPos, index: Integer;
  len: NativeInt;
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

function ConvStr2DynArray(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
var
  s: string;
  values: TStringDynArray;
  i: NativeInt;
  res, v1, v2: TValue;
  elType: PTypeInfo;
begin
  s := source.AsString;
  if StartsStr('[', s) and EndsStr(']', s) then
    s := Copy(s, 2, Length(s) - 2);
  values := SplitString(s, ',');
  MakeDynArray(target, Length(values), res);
  elType := target.TypeData.DynArrElType^;
  for i := 0 to High(values) do
  begin
    case elType.Kind of
      tkString, tkLString, tkWString, tkUString: ;
      tkChar, tkWChar:
        if Length(values[i]) > 1 then
          values[i] := Trim(values[i]);
    else
      values[i] := Trim(values[i]);
    end;

    v1 := TValue.From(values[i]);
    if not v1.TryConvert(elType, v2) then
      Exit(False);
    res.SetArrayElement(i, v2);
  end;
  value := res;
  Result := True;
end;

function ConvStr2Array(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
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
    case elType.Kind of
      tkString, tkLString, tkWString, tkUString: ;
      tkChar, tkWChar:
        if Length(values[i]) > 1 then
          values[i] := Trim(values[i]);
    else
      values[i] := Trim(values[i]);
    end;

    v1 := TValue.From(values[i]);
    if not v1.TryConvert(elType, v2) then
      Exit(False);
    res.SetArrayElement(i, v2);
  end;
  value := res;
  Result := True;
end;

function ConvVariant2Enum(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
var
  v: Variant;
  temp: TValue;
begin
  // workaround for RSP-20160
  v := source.AsVariant;
  if TVarData(v).VType <> varBoolean then
    Exit(False);

  temp := TValue.From(TVarData(v).VBoolean, TypeInfo(Boolean));
  Result := temp.TryCast(target, value);
end;

function ConvDynArray2DynArray(const source: TValue; target: PTypeInfo; var value: TValue;
  const formatSettings: TFormatSettings): Boolean; //FI:O804
var
  len, i: Integer;
  res, v1, v2: TValue;
  elType: PTypeInfo;
begin
  len := source.GetArrayLength;
  MakeDynArray(target, len, res);

  elType := target.TypeData.DynArrElType^;
  for i := 0 to len - 1 do
  begin
    v1 := source.GetArrayElement(i);
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvOrd2Str, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvOrd2Str, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFloat2Str, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
    ),
    // tkVariant
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvVariant2Enum, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvOrd2Str, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
    ),
    // tkDynArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvDynArray2DynArray,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
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
      ConvFail, ConvFail, ConvFail, ConvFail {$IF Declared(tkMRecord)}, ConvFail{$IFEND}
    )
{$IF Declared(tkMRecord)}
    // tkMRecord
    , (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail
    )
{$IFEND}
  );
{$ENDREGION}


function TValueHelper.TryConvert(targetType: PTypeInfo;
  var targetValue: TValue): Boolean;
begin
  Result := TryConvert(targetType, targetValue, ConvertSettings);
end;

function TValueHelper.TryConvert(targetType: PTypeInfo;
  var targetValue: TValue; const formatSettings: TFormatSettings): Boolean;
var
  value: TValue;
begin
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

    case Kind of //FI:W535
      tkRecord{$IF Declared(tkMRecord)}, tkMRecord{$IFEND}:
        if TypeInfo = System.TypeInfo(TValue) then
        begin
          AsType(System.TypeInfo(TValue), value);
          Exit(value.TryConvert(targetType, targetValue));
        end;
    end;

{$IFNDEF DELPHIXE2_UP}
    // workaround for wrong TValue.TryCast for string to float (it calls ConvStr2Str by mistake)
    if not ((Kind in [tkString, tkLString, tkWString, tkUString])
      and (targetType.Kind = tkFloat)) then
{$ENDIF}
    if TryCast(targetType, targetValue) then
      Exit(True);

    Result := Assigned(fValueConverterCallback)
      and fValueConverterCallback(Self, targetType, targetValue, TValue.From(formatSettings, System.TypeInfo(TFormatSettings)));
    if not Result then
      Finalize(targetValue);
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

class procedure TValueHelper.UpdateFormatSettings;
begin
  ConvertSettings := TFormatSettings.Create;
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

{$IFNDEF DELPHIX_BERLIN_UP}
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
{$ENDIF}

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
    FPackage: TRttiPackage;
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

function TMethodImplementationHelper.AsMethod: TMethod;
begin
  Result.Code := CodeAddress;
  Result.Data := Self;
end;

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
  Result.fValue := TValue.From(value, System.TypeInfo(T));
  Result.fName := name;
end;

class operator TNamedValue.Implicit(const value: TNamedValue): TValue;
begin
  Result := TValue.From(value, TypeInfo(TNamedValue));
end;

class operator TNamedValue.Implicit(const value: TValue): TNamedValue;
begin
  value.AsType(TypeInfo(TNamedValue), Result);
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
  Result.fValue := TValue.From(value, System.TypeInfo(T));
  Result.fTypeInfo := System.TypeInfo(T);
end;

class function TTypedValue.From<T>(const value: T;
  const typeInfo: PTypeInfo): TTypedValue;
begin
  Result.fValue := TValue.From(value, System.TypeInfo(T));
  Result.fTypeInfo := typeInfo;
end;

class operator TTypedValue.Implicit(const value: TTypedValue): TValue;
begin
  Result := TValue.From(value);
end;

class operator TTypedValue.Implicit(const value: TValue): TTypedValue;
begin
  value.AsType(System.TypeInfo(TTypedValue), Result);
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


{$REGION 'TRefCountedObject'}

function TRefCountedObject.AsObject: TObject;
begin
  Result := Self;
end;

function TRefCountedObject.GetInterface(const IID: TGUID; out Obj): Boolean;
var
  interfaceEntry: PInterfaceEntry;
begin
  Pointer(Obj) := nil;
  interfaceEntry := GetInterfaceEntry(IID);
  if interfaceEntry <> nil then
  begin
    if interfaceEntry.IOffset <> 0 then
    begin
      Pointer(Obj) := Pointer(PByte(Self) + interfaceEntry.IOffset);
      if Pointer(Obj) <> nil then IInterface(Obj)._AddRef;
    end
    else
      InvokeImplGetter(Self, interfaceEntry.ImplGetter, IInterface(Obj));
  end else if IID = ObjCastGUID then
    Pointer(Obj) := Pointer(Self);
  Result := Pointer(Obj) <> nil;
end;

function TRefCountedObject.GetRefCount: Integer;
begin
  Result := fRefCount and not objDestroyingFlag;
end;

procedure TRefCountedObject.AfterConstruction;
begin
  AtomicDecrement(fRefCount);
end;

procedure TRefCountedObject.BeforeDestruction;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
end;

class function TRefCountedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TRefCountedObject(Result).fRefCount := 1;
end;

function TRefCountedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TRefCountedObject._AddRef: Integer;
begin
  Result := AtomicIncrement(fRefCount);
end;

function TRefCountedObject._Release: Integer;
begin
  Result := AtomicDecrement(fRefCount);
  if Result = 0 then
  begin
    fRefCount := objDestroyingFlag;
    Destroy;
  end;
end;

{$ENDREGION}


{$REGION 'Guard'}

{$IFNDEF DELPHIXE2_UP}{$IFNDEF STACKFRAMES_ON}{$W+}{$ENDIF}{$ENDIF}
class procedure Guard.RaiseArgumentException(const msg: string);
begin
  raise EArgumentException.Create(msg) at ReturnAddress;
end;

class procedure Guard.RaiseArgumentNullException(const argumentName: string);
begin
  raise EArgumentNilException.CreateResFmt(
    @SArgumentNilException, [argumentName]) at ReturnAddress;
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

class function Guard.RaiseInvalidTypeCast(sourceType, targetType: PTypeInfo): Boolean;
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
{$IFNDEF DELPHIXE2_UP}{$IFNDEF STACKFRAMES_ON}{$W-}{$ENDIF}{$ENDIF}

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
  value: Cardinal;
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
    maxValue := Cardinal(1 shl (data.MaxValue - data.MinValue + 1)) - 1;
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


{$REGION 'RaiseHelper'}

{$IFNDEF DELPHIXE2_UP}{$IFNDEF STACKFRAMES_ON}{$W+}{$ENDIF}{$ENDIF}
class procedure RaiseHelper.ArgumentNil(argument: ExceptionArgument);
begin
  raise EArgumentNilException.Create(GetArgumentName(argument)) at ReturnAddress;
end;

class function RaiseHelper.ArgumentOutOfRange(argument: ExceptionArgument): NativeInt;
begin
  raise EArgumentOutOfRangeException.Create(GetArgumentName(argument)) at ReturnAddress;
end;

class function RaiseHelper.ArgumentOutOfRange(argument: ExceptionArgument; resource: ExceptionResource): NativeInt;
begin
  raise GetArgumentOutOfRangeException(argument, resource) at ReturnAddress;
end;

class function RaiseHelper.ArgumentOutOfRange(resource: ExceptionResource): NativeInt;
begin
  raise GetArgumentOutOfRangeException(resource) at ReturnAddress;
end;

class function RaiseHelper.ArgumentOutOfRange_Count: NativeInt;
begin
  raise GetArgumentOutOfRangeException(ExceptionArgument.count,
    ExceptionResource.ArgumentOutOfRange_Count) at ReturnAddress;
end;

class function RaiseHelper.ArgumentOutOfRange_Index: NativeInt;
begin
  raise GetArgumentOutOfRangeException(ExceptionArgument.index,
    ExceptionResource.ArgumentOutOfRange_Index) at ReturnAddress;
end;

class procedure RaiseHelper.DuplicateKey;
begin
  raise EArgumentException.CreateRes(@SArgument_DuplicateKey) at ReturnAddress;
end;

class procedure RaiseHelper.KeyNotFound;
begin
  raise EKeyNotFoundException.CreateRes(@SArgument_KeyNotFound) at ReturnAddress;
end;

class procedure RaiseHelper.MoreThanOneElement;
begin
  raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement) at ReturnAddress;
end;

class procedure RaiseHelper.MoreThanOneMatch;
begin
  raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneMatchingElement) at ReturnAddress;
end;

class procedure RaiseHelper.NoClassType(t: PTypeInfo);
begin
  raise EInvalidCast.CreateResFmt(@SNotClassType, [t.TypeName]) at ReturnAddress;
end;

class procedure RaiseHelper.NoElements;
begin
  raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements) at ReturnAddress;
end;

class procedure RaiseHelper.NoMatch;
begin
  raise EInvalidOperationException.CreateRes(@SSequenceContainsNoMatchingElement) at ReturnAddress;
end;

class procedure RaiseHelper.NotSupported;
begin
  raise ENotSupportedException.Create('') at ReturnAddress;
end;

class function RaiseHelper.EnumFailedVersion: Boolean;
begin
  raise EInvalidOperationException.CreateRes(@SInvalidOperation_EnumFailedVersion) at ReturnAddress;
end;
{$IFNDEF DELPHIXE2_UP}{$IFNDEF STACKFRAMES_ON}{$W-}{$ENDIF}{$ENDIF}

class function RaiseHelper.GetArgumentOutOfRangeException(
  argument: ExceptionArgument; resource: ExceptionResource): EArgumentException;
begin
  Result := EArgumentOutOfRangeException.CreateFmt(GetResourceString(resource), [GetArgumentName(argument)]);
end;

class function RaiseHelper.GetArgumentOutOfRangeException(
  resource: ExceptionResource): EArgumentException;
begin
  Result := EArgumentOutOfRangeException.Create(GetResourceString(resource));
end;

class function RaiseHelper.GetArgumentName(argument: ExceptionArgument): string;
const
  ArgumentNames: array[ExceptionArgument] of string = (
    'action',
    'capacity',
    'collection',
    'collectionSelector',
    'comparer',
    'count',
    'elementSelector',
    'first',
    'func',
    'index',
    'index1',
    'index2',
    'inner',
    'innerKeySelector',
    'items',
    'keySelector',
    'match',
    'max',
    'min',
    'other',
    'outer',
    'outerKeySelector',
    'predicate',
    'resultSelector',
    'second',
    'selector',
    'size',
    'sorter',
    'source',
    'sourceIndex',
    'targetIndex',
    'value',
    'values'
  );
begin
  Result := ArgumentNames[argument];
end;

class function RaiseHelper.GetResourceString(resource: ExceptionResource): string;
const
  ResourceStrings: array[ExceptionResource] of PResStringRec = (
    @SArgumentOutOfRange_Capacity,
    @SArgumentOutOfRange_Count,
    @SArgumentOutOfRange_Index,
    @SArgumentOutOfRange_NeedNonNegNum,
    @SArgument_InvalidIndexCount
  );
begin
  Result := LoadResString(ResourceStrings[resource]);
end;

{$ENDREGION}


{$REGION 'Nullable<T>'}

{$IFNDEF NULLABLE_CMR}
class constructor Nullable.Create;
begin
  HasValue := 'True';
  UniqueString(HasValue);
end;
{$ENDIF}

constructor Nullable<T>.Create(const value: T);
begin
  fValue := value;
  fHasValue := {$IFNDEF NULLABLE_CMR}Nullable.HasValue{$ELSE}True{$ENDIF};
end;

constructor Nullable<T>.Create(const value: Variant);
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    v.AsType(TypeInfo(T), fValue);
    fHasValue := {$IFNDEF NULLABLE_CMR}Nullable.HasValue{$ELSE}True{$ENDIF};
  end
  else
  begin
    fHasValue := {$IFNDEF NULLABLE_CMR}''{$ELSE}False{$ENDIF};
    fValue := Default(T);
  end;
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := fHasValue{$IFNDEF NULLABLE_CMR} <> ''{$ENDIF};
end;

function Nullable<T>.GetValue: T; //FI:W521
begin
  if HasValue then
    Exit(fValue);
  Guard.RaiseNullableHasNoValue;
  __SuppressWarning(Result);
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

class procedure Nullable<T>.InitEquals;
var
  method: TRttiMethod;
begin
  method := GetEqualsOperator(TypeInfo(T));
  if Assigned(method) then
    fEquals := method.CodeAddress;
  if not Assigned(fEquals) then
  begin
    fComparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
    fEquals := Nullable<T>.EqualsComparer;
  end;
end;

class function Nullable<T>.EqualsComparer(const left, right: T): Boolean;
begin
  Result := IEqualityComparer<T>(fComparer).Equals(left, right);
end;

class function Nullable<T>.EqualsInternal(const left, right: T): Boolean;
begin
  case TType.Kind<T> of
    tkInteger, tkChar, tkEnumeration, tkWChar:
      case SizeOf(T) of
        1: Result := PByte(@left)^ = PByte(@right)^;
        2: Result := PWord(@left)^ = PWord(@right)^;
        4: Result := PCardinal(@left)^ = PCardinal(@right)^;
      else
        __SuppressWarning(Result);
      end;
    tkString: Result := PShortString(@left)^ = PShortString(@right)^;
    tkLString: Result := PAnsiString(@left)^ = PAnsiString(@right)^;
    tkWString: Result := PWideString(@left)^ = PWideString(@right)^;
    tkFloat:
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
    tkInt64: Result := PInt64(@left)^ = PInt64(@right)^;
    tkUString: Result := PUnicodeString(@left)^ = PUnicodeString(@right)^;
  else
    if not Assigned(fEquals) then
      InitEquals;
    Result := fEquals(left, right)
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

{$IFDEF NULLABLE_CMR}
class operator Nullable<T>.Initialize(out value: Nullable<T>);
begin
  value.fHasValue := False;
end;
{$ENDIF}

class operator Nullable<T>.Implicit(const value: T): Nullable<T>;
begin
  Result.fValue := value;
  Result.fHasValue := {$IFNDEF NULLABLE_CMR}Nullable.HasValue{$ELSE}True{$ENDIF};
end;

{$IFDEF IMPLICIT_NULLABLE}
class operator Nullable<T>.Implicit(const value: Nullable<T>): T;
begin
  Result := value.Value;
end;
{$ENDIF}

class operator Nullable<T>.Explicit(const value: Variant): Nullable<T>;
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    v.AsType(TypeInfo(T), Result.fValue);
    Result.fHasValue := {$IFNDEF NULLABLE_CMR}Nullable.HasValue{$ELSE}True{$ENDIF};
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
  Result.fHasValue := {$IFNDEF NULLABLE_CMR}''{$ELSE}False{$ENDIF};
end;

class operator Nullable<T>.Equal(const left, right: Nullable<T>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Nullable<T>.Equal(const left: Nullable<T>;
  const right: T): Boolean;
begin
  if {$IFDEF NULLABLE_CMR}not {$ENDIF}left.fHasValue{$IFNDEF NULLABLE_CMR} = ''{$ENDIF} then
    Exit(False);
  Result := EqualsInternal(left.fValue, right);
end;

class operator Nullable<T>.Equal(const left: Nullable<T>;
  const right: Nullable.Null): Boolean;
begin
  Result := {$IFDEF NULLABLE_CMR}not {$ENDIF}left.fHasValue{$IFNDEF NULLABLE_CMR} = ''{$ENDIF};
end;

class operator Nullable<T>.NotEqual(const left, right: Nullable<T>): Boolean;
begin
  Result := not left.Equals(right);
end;

class operator Nullable<T>.NotEqual(const left: Nullable<T>;
  const right: Nullable.Null): Boolean;
begin
  Result := left.fHasValue{$IFNDEF NULLABLE_CMR} <> ''{$ENDIF};
end;

class operator Nullable<T>.NotEqual(const left: Nullable<T>;
  const right: T): Boolean;
begin
  if {$IFDEF NULLABLE_CMR}not {$ENDIF}left.fHasValue{$IFNDEF NULLABLE_CMR} = ''{$ENDIF} then
    Exit(True);
  Result := not EqualsInternal(left.fValue, right);
end;

function Nullable<T>.ToString: string;
var
  v: TValue;
begin
  if HasValue then
  begin
    v := TValue.From(fValue, TypeInfo(T));
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
    v := TValue.From(fValue, TypeInfo(T));
    if v.IsType(TypeInfo(Boolean)) then
      Result := v.AsBoolean
    else
      Result := v.AsVariant;
  end
  else
    Result := Null;
end;

function Nullable<T>.TryGetValue(out value: T): Boolean;
begin
  Result := fHasValue{$IFNDEF NULLABLE_CMR} <> ''{$ENDIF};
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
  fHasValueKind := field.Field.TypeRef^.Kind;
end;

function TNullableHelper.GetValue(instance: Pointer): TValue;
begin
  TValue.Make(instance, fValueType, Result);
end;

function TNullableHelper.HasValue(instance: Pointer): Boolean;
begin
  case fHasValueKind of
    tkUString: Result := PUnicodeString(PByte(instance) + fHasValueOffset)^ <> '';
    tkEnumeration: Result := PBoolean(PByte(instance) + fHasValueOffset)^;
  else
    Result := False;
  end;
end;

procedure TNullableHelper.SetValue(instance: Pointer; const value: TValue);

  function IsEmpty(const value: TValue): Boolean;
  begin
    // don't use TValue.IsEmpty here as that also considers reference types that are nil or [] as empty
    Result := (TValueData(value).FTypeInfo = nil) or (TValueData(value).FValueData = nil);
  end;

begin
  value.Cast(fValueType).ExtractRawData(instance);
  case fHasValueKind of //FI:W535
    tkUString:
      if IsEmpty(value) then
        PUnicodeString(PByte(instance) + fHasValueOffset)^ := ''
      else
        PUnicodeString(PByte(instance) + fHasValueOffset)^ := Nullable.HasValue;
    tkEnumeration:
      PBoolean(PByte(instance) + fHasValueOffset)^ := not IsEmpty(value);
  end;
end;

{$ENDREGION}


{$REGION 'TLazy'}

function Lazy.TLazy.GetIsValueCreated: Boolean;
begin
  Result := fValueFactory = nil;
end;

procedure Lazy.TLazy.CreateValue;
var
  valueFactory: Pointer;
  spinWait: TSpinWait;
begin
  valueFactory := Pointer(fValueFactory);
  if valueFactory = nil then Exit;
  valueFactory := AtomicCmpExchange(Pointer(fValueFactory), Pointer(1), valueFactory);
  if valueFactory = Pointer(1) then
  begin
    spinWait := Default(TSpinWait);
    while fValueFactory <> nil do
      spinWait.SpinCycle;
  end
  else if valueFactory <> nil then
  try
    InvokeFactory(valueFactory);
  finally
    AtomicExchange(Pointer(fValueFactory), nil);
    IInterface(valueFactory)._Release;
  end;
end;

{$ENDREGION}


{$REGION 'Lazy.TLazy<T>'}

function Lazy.TLazy<T>.GetValue: TValue;
begin
  if fValueFactory <> nil then
    CreateValue;
  Result := TValue.From(fValue, TypeInfo(T));
end;

function Lazy.TLazy<T>.GetValueT: T;
begin
  if fValueFactory <> nil then
    CreateValue;
  Result := fValue;
end;

procedure Lazy.TLazy<T>.GetValueInternal(var result);
begin
  if fValueFactory <> nil then
    CreateValue;
  T(result) := fValue;
end;

procedure Lazy.TLazy<T>.InvokeFactory(const valueFactory: Pointer);
begin
  fValue := Func<T>(valueFactory)();
end;

{$ENDREGION}


{$REGION 'Lazy.TDefaultCtorFactory'}

function Lazy.TDefaultCtorFactory.Invoke: TObject;
begin
  Result := ctor(classType);
end;

{$ENDREGION}


{$REGION 'Lazy<T>'}

class function Lazy<T>.Create: Lazy<T>; //FI:W521
begin
  Lazy.MakeFromDefaultCtor(Result.fInstance, TypeInfo(T));
end;

constructor Lazy<T>.Create(const valueFactory: Func<T>; ownsObject: Boolean);
begin
  case TType.Kind<T> of
    tkClass: Lazy.MakeFromFactory(PPointer(@valueFactory)^, fInstance, ownsObject);
    tkInterface: Lazy.MakeFromFactory(PPointer(@valueFactory)^, fInstance, TypeInfo(T));
  else
    Lazy.MakeFromFactory<T>(PPointer(@valueFactory)^, fInstance);
  end;
end;

constructor Lazy<T>.CreateFrom(const value: T; ownsObject: Boolean);
begin
  case TType.Kind<T> of
    tkClass: Lazy.Make(PObject(@value)^, fInstance, ownsObject);
    tkInterface: Lazy.Make(PInterface(@value)^, fInstance, TypeInfo(T));
  else
    Lazy.Make<T>(value, fInstance);
  end;
end;

function Lazy<T>.GetValue: T;
begin
  Lazy(fInstance).GetValue(Result);
end;

function Lazy<T>.GetIsValueCreated: Boolean;
begin
  Result := Lazy(fInstance).GetIsValueCreated;
end;

function Lazy<T>.GetIsAssigned: Boolean;
begin
  Result := Assigned(fInstance);
end;

class operator Lazy<T>.Implicit(const {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}value: Lazy<T>): ILazy<T>;
begin
  IntfAssign(value.fInstance, IInterface(Result));
end;

class operator Lazy<T>.Implicit(const {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}value: Lazy<T>): T;
begin
  Lazy(value.fInstance).GetValue(Result);
end;

class operator Lazy<T>.Implicit(const value: T): Lazy<T>;
begin
  case TType.Kind<T> of
    tkClass: Lazy.Make(PObject(@value)^, Result.fInstance, False);
    tkInterface: Lazy.Make(PInterface(@value)^, Result.fInstance, TypeInfo(T));
  else
    Lazy.Make<T>(value, Result.fInstance);
  end;
end;

class operator Lazy<T>.Implicit(const valueFactory: Func<T>): Lazy<T>;
begin
  case TType.Kind<T> of
    tkClass: Lazy.MakeFromFactory(PPointer(@valueFactory)^, Result.fInstance, False);
    tkInterface: Lazy.MakeFromFactory(PPointer(@valueFactory)^, Result.fInstance, TypeInfo(T));
  else
    Lazy.MakeFromFactory<T>(PPointer(@valueFactory)^, Result.fInstance);
  end;
end;

class operator Lazy<T>.Implicit(const value: ILazy<T>): Lazy<T>;
begin
  Result.fInstance := value;
end;

class operator Lazy<T>.Implicit(const value: Nullable.Null): Lazy<T>;
begin
  Result.fInstance := nil;
end;

{$ENDREGION}


{$REGION 'Lazy'}

function Lazy.GetIsValueCreated: Boolean;
begin
  if Assigned(fInstance) then
    Result := fInstance.IsValueCreated
  else
    Result := False;
end;

procedure Lazy.GetValue(var result);
begin
  if Assigned(fInstance) then
    ILazyInternal(fInstance).GetValueInternal(result)
  else
    Guard.RaiseNoDelegateAssigned;
end;

class procedure Lazy.Make(const value: TObject; var result; ownsObject: Boolean);
var
  rec: PObjectReference absolute result;
begin
  if Assigned(rec) and (AtomicDecrement(rec.RefCount) = 0) then
  begin
    if rec.OwnsObject then
      rec.Value.Free;
  end
  else
  begin
    GetMem(rec, SizeOf(TObjectReference));
    rec.Vtable := @Lazy.ObjectReferenceVtable;
    Pointer(rec.Factory) := nil;
    Pointer(rec.Value) := nil;
  end;
  rec.RefCount := 1;
  rec.OwnsObject := ownsObject;
  rec.Factory := nil;
  rec.Value := value;
end;

class procedure Lazy.Make(const value: IInterface; var result; typeInfo: PTypeInfo);
var
  rec: PInterfaceReference absolute result;
begin
  if not Assigned(rec) or (AtomicDecrement(rec.RefCount) <> 0) then
  begin
    GetMem(rec, SizeOf(TInterfaceReference));
    rec.Vtable := @Lazy.InterfaceReferenceVtable;
    Pointer(rec.Factory) := nil;
    Pointer(rec.Value) := nil;
  end;
  rec.RefCount := 1;
  rec.Factory := nil;
  rec.Value := value;
  rec.TypeInfo := typeInfo;
end;

class procedure Lazy.Make<T>(const value: T; var result);
var
  instance: TLazy<T>;
begin
  TObject(instance) := TLazy<T>.Create;
  instance.fValue := value;
  ILazyInternal<T>(result) := instance;
end;

class procedure Lazy.MakeFromDefaultCtor(var result; const typeInfo: PTypeInfo);
var
  defaultCtor: TDefaultCtorFactory;
begin
  Guard.CheckTypeKind(typeInfo, tkClass, 'classInfo');

  defaultCtor := TDefaultCtorFactory.Create;
  defaultCtor.classType := typeInfo.TypeData.ClassType;
  defaultCtor.ctor := TActivator.FindConstructor(defaultCtor.classType);

  MakeFromFactory(Pointer(Func<TObject>(defaultCtor)), result, True);
end;

class procedure Lazy.MakeFromFactory(factory: Pointer; var result; ownsObject: Boolean);
var
  rec: PObjectReference absolute result;
begin
  if Assigned(rec) and (AtomicDecrement(rec.RefCount) = 0) then
  begin
    if rec.OwnsObject then
      rec.Value.Free;
  end
  else
  begin
    GetMem(rec, SizeOf(TObjectReference));
    rec.Vtable := @Lazy.ObjectReferenceVtable;
    Pointer(rec.Factory) := nil;
    Pointer(rec.Value) := nil;
  end;
  rec.RefCount := 1;
  rec.OwnsObject := ownsObject;
  rec.Factory := IInterface(factory);
  rec.Value := nil;
end;

class procedure Lazy.MakeFromFactory(factory: Pointer; var result; typeInfo: PTypeInfo);
var
  rec: PInterfaceReference absolute result;
begin
  if not Assigned(rec) or (AtomicDecrement(rec.RefCount) <> 0) then
  begin
    GetMem(rec, SizeOf(TInterfaceReference));
    rec.Vtable := @Lazy.InterfaceReferenceVtable;
    Pointer(rec.Factory) := nil;
    Pointer(rec.Value) := nil;
  end;
  rec.RefCount := 1;
  rec.Factory := IInterface(factory);
  rec.Value := nil;
  rec.TypeInfo := typeInfo;
end;

class procedure Lazy.MakeFromFactory<T>(factory: Pointer; var result);
var
  instance: TLazy<T>;
begin
  TObject(instance) := TLazy<T>.Create;
  instance.fValueFactory := IInterface(factory);
  ILazyInternal<T>(result) := instance;
end;

{$ENDREGION}


{$REGION 'Lazy.TReference'}

function Lazy.TReference.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  if IID = ILazy then
  begin
    Pointer(obj) := @Vtable;
    AtomicIncrement(RefCount);
    Result := S_OK;
  end
  else
    Result := E_NOINTERFACE;
end;

{$ENDREGION}


{$REGION 'Lazy.TObjectReference'}

function Lazy.TObjectReference._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    Factory := nil;
    if OwnsObject then
      Value.Free;
    FreeMem(@Self);
  end;
end;

procedure Lazy.TObjectReference.CreateValue;
var
  valueFactory: Pointer;
  spinWait: TSpinWait;
begin
  valueFactory := Pointer(Factory);
  if valueFactory = nil then Exit;
  valueFactory := AtomicCmpExchange(Pointer(Factory), Pointer(1), valueFactory);
  if valueFactory = Pointer(1) then
  begin
    spinWait := Default(TSpinWait);
    while Factory <> nil do
      spinWait.SpinCycle;
  end
  else if valueFactory <> nil then
  try
    Value := Func<TObject>(valueFactory)();
  finally
    AtomicExchange(Pointer(Factory), nil);
    IInterface(valueFactory)._Release;
  end;
end;

function Lazy.TObjectReference.GetIsValueCreated: Boolean;
begin
  Result := Factory = nil;
end;

function Lazy.TObjectReference.GetValue: TValue;
begin
  if Factory <> nil then
    CreateValue;
  Result := Value;
end;

function Lazy.TObjectReference.GetObject: TObject;
begin
  if Factory <> nil then
    CreateValue;
  Result := Value;
end;

procedure Lazy.TObjectReference.GetValueInternal(var result);
begin
  if Factory <> nil then
    CreateValue;
  TObject(result) := Value;
end;

{$ENDREGION}


{$REGION 'Lazy.TInterfaceReference'}

function Lazy.TInterfaceReference._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    Factory := nil;
    Value := nil;
    FreeMem(@Self);
  end;
end;

procedure Lazy.TInterfaceReference.CreateValue;
var
  valueFactory: Pointer;
  spinWait: TSpinWait;
begin
  valueFactory := Pointer(Factory);
  if valueFactory = nil then Exit;
  valueFactory := AtomicCmpExchange(Pointer(Factory), Pointer(1), valueFactory);
  if valueFactory = Pointer(1) then
  begin
    spinWait := Default(TSpinWait);
    while Factory <> nil do
      spinWait.SpinCycle;
  end
  else if valueFactory <> nil then
  try
    Value := Func<IInterface>(valueFactory)();
  finally
    AtomicExchange(Pointer(Factory), nil);
    IInterface(valueFactory)._Release;
  end;
end;

function Lazy.TInterfaceReference.GetIsValueCreated: Boolean;
begin
  Result := Factory = nil;
end;

function Lazy.TInterfaceReference.GetValue: TValue;
begin
  if Factory <> nil then
    CreateValue;
  Result := TValue.From(Value, TypeInfo);
end;

function Lazy.TInterfaceReference.GetInterface: IInterface;
begin
  if Factory <> nil then
    CreateValue;
  Result := Value;
end;

procedure Lazy.TInterfaceReference.GetValueInternal(var result);
begin
  if Factory <> nil then
    CreateValue;
  IInterface(result) := Value;
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
  const valueFactory: Func<T>): T;
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

class function Shared<T>.GetMake: IShared<T>;
begin
  case TType.Kind<T> of
    tkClass, tkPointer: Shared.Make(TypeInfo(T), Result);
  end;
end;

class operator Shared<T>.Implicit(const value: IShared<T>): Shared<T>;
begin
  Result.fValue := value();
  IShared<T>(Result.fFinalizer) := value;
end;

class operator Shared<T>.Implicit(const value: Shared<T>): IShared<T>;
begin
  Result := IShared<T>(value.fFinalizer);
end;

class operator Shared<T>.Implicit(const value: Shared<T>): T;
begin
  Result := value.fValue;
end;

class operator Shared<T>.Implicit(const value: Shared<T>): Weak<T>;
begin
  case TType.Kind<T> of
    tkClass: Weak.MakeFromObject(value.Value, Result.fReference);
    tkInterface: Weak.MakeFromInterface(value.Value, Result.fReference);
  end;
end;

class operator Shared<T>.Implicit(const value: T): Shared<T>;
begin
  Result.fValue := value;
  case TType.Kind<T> of
    tkClass: Shared.Make(PObject(@Result.fValue)^, Result.fFinalizer);
    tkPointer: Shared.Make(PPointer(@Result.fValue)^, TypeInfo(T), Result.fFinalizer);
  end;
end;

{$ENDREGION}


{$REGION 'Shared'}

class procedure Shared.Make(const value: TObject; var result);
var
  finalizer: PObjectFinalizer absolute result;
begin
  if Assigned(finalizer) and (AtomicDecrement(finalizer.RefCount) = 0) then
    finalizer.Value.Free
  else
  begin
    GetMem(finalizer, SizeOf(TObjectFinalizer));
    finalizer.Vtable := @Shared.ObjectFinalizerVtable;
  end;
  finalizer.RefCount := 1;
  finalizer.Value := value;
end;

class procedure Shared.Make(const value: Pointer; typeInfo: PTypeInfo; var result);
var
  finalizer: PRecordFinalizer absolute result;
begin
  typeInfo := typeInfo.TypeData.RefType^;
  if Assigned(finalizer) and (AtomicDecrement(finalizer.RefCount) = 0) then
  begin
    FinalizeArray(finalizer.Value, typeInfo, 1);
    FillChar(finalizer.Value^, typeInfo.TypeData.RecSize, 0);
    FreeMem(finalizer.Value);
  end
  else
  begin
    GetMem(finalizer, SizeOf(TRecordFinalizer));
    finalizer.Vtable := @Shared.RecordFinalizerVtable;
  end;
  finalizer.RefCount := 1;
  finalizer.Value := value;
  finalizer.TypeInfo := typeInfo;
end;

class procedure Shared.Make(typeInfo: PTypeInfo; var result);

  function AllocRecord(typeInfo: PTypeInfo): Pointer;
  begin
    Result := AllocMem(typeInfo.TypeData.RefType^.TypeData.RecSize);
  end;

begin
  if typeInfo.Kind = tkClass then
    Make(TActivator.CreateInstance(typeInfo), result)
  else if typeInfo.Kind = tkPointer then
    Make(AllocRecord(typeInfo), typeInfo, result);
end;

class function Shared.Make<T>: IShared<T>;
begin
  Make(T.Create, Result);
end;

class function Shared.Make<T>(const value: T): IShared<T>;
begin
  case TType.Kind<T> of
    tkClass: Make(PObject(@value)^, Result);
    tkPointer: Make(PPointer(@value)^, TypeInfo(T), Result);
  end;
end;

class function Shared.Make<T>(const value: T;
  const finalizer: Action<T>): IShared<T>;
begin
  Result := THandleFinalizer<T>.Create(value, finalizer);
end;

{$ENDREGION}


{$REGION 'Shared.TObjectFinalizer'}

function Shared.TObjectFinalizer._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    Value.Free;
    FreeMem(@Self);
  end;
end;

function Shared.TObjectFinalizer.Invoke: TObject;
begin
  Result := Value;
end;

{$ENDREGION}


{$REGION 'Shared.TRecordFinalizer'}

function Shared.TRecordFinalizer._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    FinalizeArray(Value, TypeInfo, 1);
    FillChar(Value^, TypeInfo.TypeData.RecSize, 0);
    FreeMem(Value);
    FreeMem(@Self);
  end;
end;

function Shared.TRecordFinalizer.Invoke: Pointer;
begin
  Result := Value;
end;

{$ENDREGION}


{$REGION 'Shared.THandleFinalizer<T>'}

constructor Shared.THandleFinalizer<T>.Create(const value: T;
  finalizer: Action<T>);
begin
  fValue := value;
  fFinalizer := finalizer;
end;

destructor Shared.THandleFinalizer<T>.Destroy; //FI:W504
begin
  fFinalizer(fValue);
end;

function Shared.THandleFinalizer<T>.Invoke: T;
begin
  Result := fValue;
end;

{$ENDREGION}


{$REGION 'TWeakReferences'}

type
  TWeakReferences = record
  strict private
    fLock: TCriticalSection;
    fWeakReferences: TDictionary<Pointer, TList>;
  public
    procedure Initialize;
    procedure Finalize;

    procedure RegisterWeakRef(address: Pointer; instance: Pointer);
    procedure UnregisterWeakRef(address: Pointer; instance: Pointer);
  end;

procedure TWeakReferences.Initialize;
begin
  fLock := TCriticalSection.Create;
  fWeakReferences := TObjectDictionary<Pointer, TList>.Create([Generics.Collections.doOwnsValues]);
end;

procedure TWeakReferences.Finalize;
begin
  FreeAndNil(fWeakReferences);
  FreeAndNil(fLock);
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
  if fLock = nil then Exit;

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

var
  WeakRefInstances: TWeakReferences;

type
  TVirtualClasses = class(Spring.VirtualClass.TVirtualClasses);

procedure WeakRefFreeInstance(const Self: TObject);
var
  freeInstance: TFreeInstance;
begin
  freeInstance := GetClassData(Self.ClassParent).FreeInstance;
  WeakRefInstances.UnregisterWeakRef(nil, Self);
  freeInstance(Self);
end;

procedure RegisterWeakRef(address: Pointer; const instance: TObject);
begin
  TVirtualClasses.Default.Proxify(instance);
  GetClassData(PPointer(instance)^).FreeInstance := WeakRefFreeInstance;
  WeakRefInstances.RegisterWeakRef(address, instance);
end;

procedure UnregisterWeakRef(address: Pointer; const instance: TObject);
begin
  WeakRefInstances.UnregisterWeakRef(address, instance);
end;

procedure MoveManaged(source, target, typeInfo: Pointer; count: NativeInt);
type
  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: NativeInt;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    Size: Integer;
    Count: Integer;
    case TTypeKind of
    tkArray:  ( ElemType: PPTypeInfo );
    tkRecord: ( Fields: array[0..0] of TFieldInfo );
  end;

  function GetFieldTable(typeInfo: Pointer): PFieldTable; inline;
  begin
    Result := PFieldTable(PByte(typeInfo) + PByte(typeInfo)[1] + 2);
  end;

var
  fieldTable: PFieldTable;
  elemType: PTypeInfo;
  size, elemCount: Integer;
  n: NativeInt;
begin
  if count = 0 then Exit;
  if PByte(target) < PByte(source) then
    case PTypeInfo(typeInfo).Kind of
      tkLString:
        repeat
          PAnsiString(target)^ := PAnsiString(source)^;
          Inc(PByte(target), SizeOf(Pointer));
          Inc(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
      tkWString:
        repeat
          PWideString(target)^ := PWideString(source)^;
          Inc(PByte(target), SizeOf(Pointer));
          Inc(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
      tkUString:
        repeat
          PUnicodeString(target)^ := PUnicodeString(source)^;
          Inc(PByte(target), SizeOf(Pointer));
          Inc(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
      tkVariant:
        repeat
          PVariant(target)^ := PVariant(source)^;
          Inc(PByte(target), SizeOf(TVarData));
          Inc(PByte(source), SizeOf(TVarData));
          Dec(count);
        until count = 0;
      tkArray:
      begin
        fieldTable := GetFieldTable(typeInfo);
        size := fieldTable.Size;
        elemCount := fieldTable.Count;
        elemType := fieldTable.ElemType^;
        repeat
          CopyArray(target, source, elemType, elemCount);
          Inc(PByte(target), size);
          Inc(PByte(source), size);
          Dec(count);
        until count = 0;
      end;
      tkRecord{$IF Declared(tkMRecord)}, tkMRecord{$IFEND}:
      begin
        size := GetFieldTable(typeInfo).Size;
        repeat
          CopyRecord(target, source, typeInfo);
          Inc(PByte(target), size);
          Inc(PByte(source), size);
          Dec(count);
        until count = 0;
      end;
      tkInterface:
        repeat
          PInterface(target)^ := PInterface(source)^;
          Inc(PByte(target), SizeOf(Pointer));
          Inc(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
      tkDynArray:
        repeat
          DynArrayAssign(PPointer(target)^, PPointer(source)^, typeInfo);
          Inc(PByte(target), SizeOf(Pointer));
          Inc(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
    end
  else
    case PTypeInfo(typeInfo).Kind of
      tkLString:
      begin
        n := (count - 1) * SizeOf(Pointer);
        Inc(PByte(target), n);
        Inc(PByte(source), n);
        repeat
          PAnsiString(target)^ := PAnsiString(source)^;
          Dec(PByte(target), SizeOf(Pointer));
          Dec(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
      end;
      tkWString:
      begin
        n := (count - 1) * SizeOf(Pointer);
        Inc(PByte(target), n);
        Inc(PByte(source), n);
        repeat
          PWideString(target)^ := PWideString(source)^;
          Dec(PByte(target), SizeOf(Pointer));
          Dec(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
      end;
      tkUString:
      begin
        n := (count - 1) * SizeOf(Pointer);
        Inc(PByte(target), n);
        Inc(PByte(source), n);
        repeat
          PUnicodeString(target)^ := PUnicodeString(source)^;
          Dec(PByte(target), SizeOf(Pointer));
          Dec(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
      end;
      tkVariant:
      begin
        n := (count - 1) * SizeOf(TVarData);
        Inc(PByte(target), n);
        Inc(PByte(source), n);
        repeat
          PVariant(target)^ := PVariant(source)^;
          Dec(PByte(target), SizeOf(TVarData));
          Dec(PByte(source), SizeOf(TVarData));
          Dec(count);
        until count = 0;
      end;
      tkArray:
      begin
        fieldTable := GetFieldTable(typeInfo);
        size := fieldTable.Size;
        elemCount := fieldTable.Count;
        elemType := fieldTable.ElemType^;
        n := (count - 1) * size;
        Inc(PByte(target), n);
        Inc(PByte(source), n);
        repeat
          CopyArray(target, source, elemType, elemCount);
          Dec(PByte(target), size);
          Dec(PByte(source), size);
          Dec(count);
        until count = 0;
      end;
      tkRecord{$IF Declared(tkMRecord)}, tkMRecord{$IFEND}:
      begin
        size := GetFieldTable(typeInfo).Size;
        n := (count - 1) * size;
        Inc(PByte(target), n);
        Inc(PByte(source), n);
        repeat
          CopyRecord(target, source, typeInfo);
          Dec(PByte(target), size);
          Dec(PByte(source), size);
          Dec(count);
        until count = 0;
      end;
      tkInterface:
      begin
        n := (count - 1) * SizeOf(Pointer);
        Inc(PByte(target), n);
        Inc(PByte(source), n);
        repeat
          PInterface(target)^ := PInterface(source)^;
          Dec(PByte(target), SizeOf(Pointer));
          Dec(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
      end;
      tkDynArray:
      begin
        n := (count - 1) * SizeOf(Pointer);
        Inc(PByte(target), n);
        Inc(PByte(source), n);
        repeat
          DynArrayAssign(PPointer(target)^, PPointer(source)^, typeInfo);
          Dec(PByte(target), SizeOf(Pointer));
          Dec(PByte(source), SizeOf(Pointer));
          Dec(count);
        until count = 0;
      end;
    end
end;

{$ENDREGION}


{$REGION 'Weak<T>'}

function Weak<T>.GetIsAlive: Boolean;
begin
  Result := Weak.GetIsAlive(fReference);
end;

function Weak<T>.GetTarget: T;
begin
  Weak.GetTarget(fReference, Result, TType.Kind<T>);
end;

function Weak<T>.TryGetTarget(var target: T): Boolean;
begin
  Result := Weak.TryGetTarget(fReference, target, TType.Kind<T>);
end;

class operator Weak<T>.Implicit(const value: T): Weak<T>;
begin
  case TType.Kind<T> of
    tkClass: Weak.MakeFromObject(value, Result.fReference);
    tkInterface: Weak.MakeFromInterface(value, Result.fReference);
  end;
end;

class operator Weak<T>.Implicit(
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}
  const value: Weak<T>): T;
begin
  Weak.GetTarget(value.fReference, Result, TType.Kind<T>);
end;

class operator Weak<T>.Equal(
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}
  const left: Weak<T>; const right: T): Boolean;
begin
  Result := Weak.Equal(left.fReference, right);
end;

class operator Weak<T>.NotEqual(
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}
  const left: Weak<T>; const right: T): Boolean;
begin
  Result := Weak.NotEqual(left.fReference, right);
end;

{$ENDREGION}


{$REGION 'Weak'}

class procedure Weak.MakeFromObject(const value; var result);
var
  ref: PReference absolute result;
begin
  repeat
    if Assigned(ref) then
    begin
      if ref.Target = Pointer(value) then
        Exit;

      if AtomicDecrement(ref.RefCount) = 0 then
      begin
        if Assigned(ref.Target) then
          UnregisterWeakRef(@ref.Target, ref.Target);
        if Assigned(Pointer(value)) then
          Break;

        FreeMem(ref);
        Exit;
      end;
    end;

    if not Assigned(Pointer(value)) then
      Exit;

    GetMem(ref, SizeOf(TReference));
    ref.Vtable := @Weak.ObjectReferenceVtable;
  until True;
  ref.RefCount := 1;
  ref.Target := Pointer(value);
  RegisterWeakRef(@ref.Target, ref.Target);
end;

class procedure Weak.MakeFromInterface(const value; var result);
var
  ref: PReference absolute result;
begin
  repeat
    if Assigned(ref) then
    begin
      if ref.Target = Pointer(value) then
        Exit;

      if AtomicDecrement(ref.RefCount) = 0 then
      begin
        if Assigned(ref.Target) then
          UnregisterWeakRef(@ref.Target, IInterface(ref.Target) as TObject);
        if Assigned(Pointer(value)) then
          Break;

        FreeMem(ref);
        Exit;
      end;
    end;

    if not Assigned(Pointer(value)) then
      Exit;

    GetMem(ref, SizeOf(TReference));
    ref.Vtable := @Weak.InterfaceReferenceVtable;
  until True;
  ref.RefCount := 1;
  ref.Target := Pointer(value);
  RegisterWeakRef(@ref.Target, IInterface(ref.Target) as TObject);
end;

class function Weak.Equal(const left; const right): Boolean;
var
  p: Pointer;
begin
  p := Pointer(left);
  if Assigned(p) then
    p := PReference(p).Target;
  Result := p = Pointer(right);
end;

class function Weak.NotEqual(const left; const right): Boolean;
var
  p: Pointer;
begin
  p := Pointer(left);
  if Assigned(p) then
    p := PReference(p).Target;
  Result := p <> Pointer(right);
end;

class function Weak.GetIsAlive(const reference): Boolean;
var
  p: Pointer;
begin
  p := Pointer(reference);
  if Assigned(p) then
    p := PReference(p).Target;
  Result := Assigned(p);
end;

class procedure Weak.GetTarget(const reference; var result; kind: TTypeKind);
{$IFNDEF CPUX86}
var
  p: Pointer;
begin
  p := Pointer(reference);
  if Assigned(p) then
    p := PReference(p).Target;
  if kind = tkInterface then
    IInterface(result) := IInterface(p)
  else
    TObject(result) := TObject(p);
end;
{$ELSE}
asm
  mov eax,[eax]
  test eax,eax
  jz @@niltarget
  mov eax,dword ptr [eax].TReference.Target
@@niltarget:
  cmp cl,tkInterface
  jne @@exit
  mov ecx,eax
  mov eax,edx
  mov edx,ecx
  call System.@IntfCopy
  ret
@@exit:
  mov [edx],eax
end;
{$ENDIF}

class function Weak.TryGetTarget(const reference; var target; kind: TTypeKind): Boolean;
var
  p: Pointer;
begin
  p := Pointer(reference);
  if Assigned(p) then
    p := PReference(p).Target;
  if kind = tkInterface then
    IInterface(target) := IInterface(p)
  else
    TObject(target) := TObject(p);
  Result := Assigned(p);
end;

{$ENDREGION}


{$REGION 'Weak.TReference'}

function Weak.TReference._Release_Obj: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    if Assigned(Target) then
      UnregisterWeakRef(@Target, Target);
    Target := nil;
    FreeMem(@Self);
  end;
end;

function Weak.TReference._Release_Intf: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    if Assigned(Target) then
      UnregisterWeakRef(@Target, IInterface(Target) as TObject);
    Target := nil;
    FreeMem(@Self);
  end;
end;

{$ENDREGION}


{$REGION 'Event<T>'}

procedure Event<T>.Add(const handler: T);
begin
  EventHelper(fInstance).Add(handler, TypeInfo(T));
end;

procedure Event<T>.Clear;
begin
  EventHelper(fInstance).Clear;
end;

function Event<T>.GetCanInvoke: Boolean;
begin
  Result := EventHelper(fInstance).GetCanInvoke;
end;

function Event<T>.GetEnabled: Boolean;
begin
  Result := EventHelper(fInstance).GetEnabled;
end;

function Event<T>.GetInvoke: T;
begin
  EventHelper(fInstance).GetInvoke(Result, TypeInfo(T));
end;

function Event<T>.GetOnChanged: TNotifyEvent;
begin
  Result := EventHelper(fInstance).GetOnChanged();
end;

function Event<T>.GetUseFreeNotification: Boolean;
begin
  Result := EventHelper(fInstance).GetUseFreeNotification;
end;

procedure Event<T>.Remove(const handler: T);
begin
  EventHelper(fInstance).Remove(handler);
end;

procedure Event<T>.RemoveAll(instance: Pointer);
begin
  EventHelper(fInstance).RemoveAll(instance);
end;

procedure Event<T>.SetEnabled(const value: Boolean);
begin
  EventHelper(fInstance).SetEnabled(value, TypeInfo(T));
end;

procedure Event<T>.SetOnChanged(const value: TNotifyEvent);
begin
  EventHelper(fInstance).SetOnChanged(value, TypeInfo(T));
end;

procedure Event<T>.SetUseFreeNotification(const value: Boolean);
begin
  EventHelper(fInstance).SetUseFreeNotification(value, TypeInfo(T));
end;

class operator Event<T>.Implicit(const value: IInvokableEvent<T>): Event<T>;
begin
  IntfAssign(value, IInterface(Result.fInstance));
end;

class operator Event<T>.Implicit(var value: Event<T>): IInvokableEvent<T>;
begin
  EventHelper(value.fInstance).EnsureInstance(Result, TypeInfo(T));
end;

class operator Event<T>.Implicit(var value: Event<T>): T;
begin
  EventHelper(value.fInstance).GetInvoke(Result, TypeInfo(T));
end;

{$ENDREGION}


{$REGION 'TTypeInfoHelper'}

function TTypeInfoHelper.GetRttiType: TRttiType;
begin
  Result := TType.GetType(@Self);
end;

function TTypeInfoHelper.TypeData: PTypeData;
var
  p: PByte;
begin
  p := @Self;
  Result := @p[p[1]+2];
end;

function TTypeInfoHelper.TypeName: string;
begin
  Result := UTF8ToString(Name);
end;

function TTypeInfoHelper.TypeSize: Integer;
begin
  Result := GetTypeSize(@Self);
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
begin //FI:W519
end;

{$ENDREGION}


{$REGION 'TPropertyChangedEventArgs'}

constructor TPropertyChangedEventArgs.Create(const propertyName: string); //FI:W525
begin
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
  Result := AtomicIncrement(fRefCount);
end;

function TInterfacedCriticalSection._Release: Integer;
begin
  Result := AtomicDecrement(fRefCount);
  if Result = 0 then
    Destroy;
end;

function TInterfacedCriticalSection.ScopedLock: IInterface;
begin
  Result := TScopedLock.Create(Self); //FI:W534
end;

{$ENDREGION}


{$REGION 'TInterfacedCriticalSection.TScopedLock'}

constructor TInterfacedCriticalSection.TScopedLock.Create(
  const criticalSection: ICriticalSection);
begin
  fCriticalSection := criticalSection;
  fCriticalSection.Enter;
end;

destructor TInterfacedCriticalSection.TScopedLock.Destroy; //FI:W504
begin
  fCriticalSection.Leave;
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
  CacheLock.EnterWrite;
  try
    ConstructorCache.Clear;
  finally
    CacheLock.LeaveWrite;
  end;
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
  ctorFound: Boolean;
  method: TRttiMethod;
begin
  Assert(Assigned(classType));
  classInfo := classType.ClassInfo;

  CacheLock.EnterRead;
  try
    ctorFound := ConstructorCache.TryGetValue(classInfo, Result);
  finally
    CacheLock.LeaveRead;
  end;

  if ctorFound then
    Exit;

  CacheLock.EnterUpgradableRead;
  try
    for method in TType.GetType(classInfo).GetMethods do
    begin
      if not method.IsConstructor then
        Continue;

      if method.GetParameters = nil then
      begin
        Result := method.CodeAddress;
        CacheLock.EnterWrite;
        try
          ConstructorCache.AddOrSetValue(classInfo, Result);
        finally
          CacheLock.LeaveWrite;
        end;
        Exit;
      end;
    end;
    Result := nil;
  finally
    CacheLock.LeaveUpgradableRead;
  end;
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
      begin
        CacheLock.EnterWrite;
        try
          ConstructorCache.AddOrSetValue(classType.Handle, method.CodeAddress);
        finally
          CacheLock.LeaveWrite;
        end;
      end;
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
  comparer1, comparer2: Pointer;
begin
  comparer1 := _LookupVtableInfo(giEqualityComparer, TypeInfo(T1), SizeOf(T1));
  comparer2 := _LookupVtableInfo(giEqualityComparer, TypeInfo(T2), SizeOf(T2));
  Result := IEqualityComparer<T1>(comparer1).Equals(fValue1, value.Value1)
    and IEqualityComparer<T2>(comparer2).Equals(fValue2, value.Value2);
end;

class operator Tuple<T1, T2>.Equal(const left, right: Tuple<T1, T2>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Tuple<T1, T2>.Implicit(
  const values: Tuple<T1, T2>): TArray<TValue>;
begin
  SetLength(Result, 2);
  Result[0] := TValue.From(values.Value1, TypeInfo(T1));
  Result[1] := TValue.From(values.Value2, TypeInfo(T2));
end;

class operator Tuple<T1, T2>.Implicit(
  const values: TArray<TValue>): Tuple<T1, T2>;
begin
  values[0].AsType(TypeInfo(T1), Result.fValue1);
  values[1].AsType(TypeInfo(T2), Result.fValue2);
end;

class operator Tuple<T1, T2>.Implicit(
  const values: array of const): Tuple<T1, T2>;
var
  value: TValue;
begin
  value := TValue.FromVarRec(values[0]);
  value.AsType(TypeInfo(T1), Result.fValue1);
  value := TValue.FromVarRec(values[1]);
  value.AsType(TypeInfo(T2), Result.fValue2);
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
  comparer1, comparer2, comparer3: Pointer;
begin
  comparer1 := _LookupVtableInfo(giEqualityComparer, TypeInfo(T1), SizeOf(T1));
  comparer2 := _LookupVtableInfo(giEqualityComparer, TypeInfo(T2), SizeOf(T2));
  comparer3 := _LookupVtableInfo(giEqualityComparer, TypeInfo(T3), SizeOf(T3));
  Result := IEqualityComparer<T1>(comparer1).Equals(fValue1, value.Value1)
    and IEqualityComparer<T2>(comparer2).Equals(fValue2, value.Value2)
    and IEqualityComparer<T3>(comparer3).Equals(fValue3, value.Value3);
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
  Result[0] := TValue.From(values.Value1, TypeInfo(T1));
  Result[1] := TValue.From(values.Value2, TypeInfo(T2));
  Result[2] := TValue.From(values.Value3, TypeInfo(T3));
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
  values[0].AsType(TypeInfo(T1), Result.fValue1);
  values[1].AsType(TypeInfo(T2), Result.fValue2);
  values[2].AsType(TypeInfo(T3), Result.fValue3);
end;

class operator Tuple<T1, T2, T3>.Implicit(
  const values: array of const): Tuple<T1, T2, T3>;
var
  value: TValue;
begin
  value := TValue.FromVarRec(values[0]);
  value.AsType(TypeInfo(T1), Result.fValue1);
  value := TValue.FromVarRec(values[1]);
  value.AsType(TypeInfo(T2), Result.fValue2);
  value := TValue.FromVarRec(values[2]);
  value.AsType(TypeInfo(T3), Result.fValue3);
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
  comparer1, comparer2, comparer3, comparer4: Pointer;
begin
  comparer1 := _LookupVtableInfo(giEqualityComparer, TypeInfo(T1), SizeOf(T1));
  comparer2 := _LookupVtableInfo(giEqualityComparer, TypeInfo(T2), SizeOf(T2));
  comparer3 := _LookupVtableInfo(giEqualityComparer, TypeInfo(T3), SizeOf(T3));
  comparer4 := _LookupVtableInfo(giEqualityComparer, TypeInfo(T4), SizeOf(T4));
  Result := IEqualityComparer<T1>(comparer1).Equals(fValue1, value.Value1)
    and IEqualityComparer<T2>(comparer2).Equals(fValue2, value.Value2)
    and IEqualityComparer<T3>(comparer3).Equals(fValue3, value.Value3)
    and IEqualityComparer<T4>(comparer4).Equals(fValue4, value.Value4);
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
  Result[0] := TValue.From(values.Value1, TypeInfo(T1));
  Result[1] := TValue.From(values.Value2, TypeInfo(T2));
  Result[2] := TValue.From(values.Value3, TypeInfo(T3));
  Result[3] := TValue.From(values.Value4, TypeInfo(T4));
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
  values[0].AsType(TypeInfo(T1), Result.fValue1);
  values[1].AsType(TypeInfo(T2), Result.fValue2);
  values[2].AsType(TypeInfo(T3), Result.fValue3);
  values[3].AsType(TypeInfo(T4), Result.fValue4);
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const values: array of const): Tuple<T1, T2, T3, T4>;
var
  value: TValue;
begin
  value := TValue.FromVarRec(values[0]);
  value.AsType(TypeInfo(T1), Result.fValue1);
  value := TValue.FromVarRec(values[1]);
  value.AsType(TypeInfo(T2), Result.fValue2);
  value := TValue.FromVarRec(values[2]);
  value.AsType(TypeInfo(T3), Result.fValue3);
  value := TValue.FromVarRec(values[3]);
  value.AsType(TypeInfo(T4), Result.fValue4);
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

class function TArray.BinarySearchInternal<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparer: IComparer<T>): Boolean;
var
  count, left, right: NativeInt;
  compareResult: Integer;
  compare: TCompareMethod<T>;
begin
  TMethod(compare).Data := Pointer(comparer);
  TMethod(compare).Code := PPVTable(comparer)^[3];
  Result := False;
  count := Length(values);
  left := 0;
  if count > 0 then
  repeat
    right := left + (1 and count);
    count := count shr 1;
    right := right + count;
    compareResult := compare(values[left + count], item);
    if compareResult < 0 then
      left := right;
    {$B+}
    Result := Result or (compareResult = 0);
    {$B-}
  until count = 0;
  foundIndex := left;
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer): Boolean;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  Result := BinarySearchInternal<T>(values, item, foundIndex, IComparer<T>(comparer));
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer; index, count: Integer): Boolean;
var
  comparer: Pointer;
begin
  CheckRange(index, count, Length(values));
  comparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  {$R-}
  Result := BinarySearchInternal<T>(
    Slice(TSlice<T>((@values[index])^), count),
    item, foundIndex, IComparer<T>(comparer));
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  Inc(foundIndex, index);
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparer: IComparer<T>): Boolean;
begin
  Result := BinarySearchInternal<T>(values, item, foundIndex, comparer);
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparer: IComparer<T>;
  index, count: Integer): Boolean;
begin
  CheckRange(index, count, Length(values));
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);
  {$R-}
  Result := BinarySearchInternal<T>(
    Slice(TSlice<T>((@values[index])^), count),
    item, foundIndex, comparer);
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  Inc(foundIndex, index);
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparison: TComparison<T>): Boolean;
begin
  Result := BinarySearchInternal<T>(values, item, foundIndex, IComparer<T>((@comparison)^));
end;

class function TArray.BinarySearch<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparison: TComparison<T>;
  index, count: Integer): Boolean;
begin
  CheckRange(index, count, Length(values));
  if not Assigned(comparison) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);
  {$R-}
  Result := BinarySearchInternal<T>(
    Slice(TSlice<T>((@values[index])^), count),
    item, foundIndex, IComparer<T>((@comparison)^));
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  Inc(foundIndex, index);
end;

class function TArray.BinarySearchUpperBoundInternal<T>(const values: array of T;
  const item: T; out foundIndex: Integer; const comparer: IComparer<T>): Boolean;
var
  count, left, right: NativeInt;
  compareResult: Integer;
  compare: TCompareMethod<T>;
begin
  TMethod(compare).Data := Pointer(comparer);
  TMethod(compare).Code := PPVTable(comparer)^[3];
  Result := False;
  count := Length(values);
  left := 0;
  if count > 0 then
  repeat
    right := left + (1 and count);
    count := count shr 1;
    right := right + count;
    compareResult := compare(values[left + count], item);
    if compareResult > 0 then Continue;
    left := right;
    {$B+}
    Result := Result or (compareResult = 0);
    {$B-}
  until count = 0;
  foundIndex := left - Ord(Result);
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T;
  const item: T; out foundIndex: Integer): Boolean;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  Result := BinarySearchUpperBoundInternal<T>(values, item, foundIndex, IComparer<T>(comparer));
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T;
  const item: T; out foundIndex: Integer; index, count: Integer): Boolean;
var
  comparer: Pointer;
begin
  CheckRange(index, count, Length(values));
  comparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  {$R-}
  Result := BinarySearchUpperBoundInternal<T>(
    Slice(TSlice<T>((@values[index])^), count),
    item, foundIndex, IComparer<T>(comparer));
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  Inc(foundIndex, index);
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T;
  const item: T; out foundIndex: Integer; const comparer: IComparer<T>): Boolean;
begin
  Result := BinarySearchUpperBoundInternal<T>(values, item, foundIndex, comparer);
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T;
  const item: T; out foundIndex: Integer; const comparer: IComparer<T>;
  index, count: Integer): Boolean;
begin
  CheckRange(index, count, Length(values));
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);
  {$R-}
  Result := BinarySearchUpperBoundInternal<T>(
    Slice(TSlice<T>((@values[index])^), count),
    item, foundIndex, comparer);
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  Inc(foundIndex, index);
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparison: TComparison<T>): Boolean;
begin
  Result := BinarySearchUpperBoundInternal<T>(values, item, foundIndex, IComparer<T>((@comparison)^));
end;

class function TArray.BinarySearchUpperBound<T>(const values: array of T; const item: T;
  out foundIndex: Integer; const comparison: TComparison<T>;
  index, count: Integer): Boolean;
begin
  CheckRange(index, count, Length(values));
  if not Assigned(comparison) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);
  {$R-}
  Result := BinarySearchUpperBoundInternal<T>(
    Slice(TSlice<T>((@values[index])^), count),
    item, foundIndex, IComparer<T>((@comparison)^));
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  Inc(foundIndex, index);
end;

class function TArray.Concat<T>(const values: array of TArray<T>): TArray<T>;
var
  i, k, n: NativeInt;
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
  comparer: Pointer;
  i: Integer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  for i := Low(Values) to High(Values) do
    if IEqualityComparer<T>(comparer).Equals(values[i], item) then
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
{$IFDEF SPRING_ENABLE_GUARD}
var
  sourceLength, targetLength: NativeInt;
{$ENDIF}
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
    MoveManaged(@source[sourceIndex], @target[targetIndex], TypeInfo(T), count)
  else
    System.Move(source[sourceIndex], target[targetIndex], count * SizeOf(T));
end;

class procedure TArray.ForEach<T>(const values: array of T;
  const action: Action<T>);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    action(values[i]);
end;

class function TArray.IndexOf<T>(const values: array of T; const item: T): Integer;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  Result := IndexOf<T>(values, item, 0, Length(values), IEqualityComparer<T>(comparer));
end;

class function TArray.IndexOf<T>(const values: array of T; const item: T;
  index: Integer): Integer;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  Result := IndexOf<T>(values, item, index, Length(values) - index, IEqualityComparer<T>(comparer));
end;

class function TArray.IndexOf<T>(const values: array of T; const item: T;
  index, count: Integer): Integer;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  Result := IndexOf<T>(values, item, index, count, IEqualityComparer<T>(comparer));
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
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  Result := LastIndexOf<T>(values, item, High(values), Length(values), IEqualityComparer<T>(comparer));
end;

class function TArray.LastIndexOf<T>(const values: array of T; const item: T;
  index: Integer): Integer;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  Result := LastIndexOf<T>(values, item, index, Length(values) - index, IEqualityComparer<T>(comparer));
end;

class function TArray.LastIndexOf<T>(const values: array of T; const item: T;
  index, count: Integer): Integer;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  Result := LastIndexOf<T>(values, item, index, count, IEqualityComparer<T>(comparer));
end;

class function TArray.LastIndexOf<T>(const values: array of T; const item: T;
  index, count: Integer; const comparer: IEqualityComparer<T>): Integer;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= index + 1), 'count');
{$ENDIF}

  for i := index downto index - count do
    if comparer.Equals(values[i], item) then
      Exit(i);
  Result := -1;
end;

procedure SwapPtr(arr: PPointer; left, right: Integer);
var
  temp: Pointer;
begin
{$POINTERMATH ON}
  temp := arr[left];
  arr[left] := arr[right];
  arr[right] := temp;
{$POINTERMATH OFF}
end;

procedure BinarySwap(left, right: Pointer; size: NativeInt);
{$IFDEF ASSEMBLER}
{$IFDEF CPUX64}
asm
  mov       rax, r8
  and       r8, 15
  and       rax, -16
  jz        @@Swap8Byte
  add       rcx, rax
  add       rdx, rax
  neg       rax

  .align 16
@@Swap16Byte:
  movdqu    xmm0, [rcx+rax]
  movdqu    xmm1, [rdx+rax]
  movdqu    [rcx+rax], xmm1
  movdqu    [rdx+rax], xmm0
  add       rax, 16
  jl        @@Swap16Byte

@@Swap8Byte:
  test      r8, r8
  jz        @@Exit
  cmp       r8, 8
  jl        @@Swap4Byte
  mov       rax, [rcx]
  mov       r9, [rdx]
  mov       [rcx], r9
  mov       [rdx], rax
  sub       r8, 8
  jz        @@Exit
  add       rcx, 8
  add       rdx, 8

@@Swap4Byte:
  cmp       r8, 4
  jl        @@Swap2Byte
  mov       eax, [rcx]
  mov       r9d, [rdx]
  mov       [rcx], r9d
  mov       [rdx], eax
  sub       r8, 4
  jz        @@Exit
  add       rcx, 4
  add       rdx, 4

@@Swap2Byte:
  cmp       r8, 2
  jl        @@Swap1Byte
  movzx     eax, word ptr [rcx]
  movzx     r9d, word ptr [rdx]
  mov       [rcx], r9w
  mov       [rdx], ax
  sub       r8, 2
  jz        @@Exit
  add       rcx, 2
  add       rdx, 2

@@Swap1Byte:
  movzx     eax, byte ptr [rcx]
  movzx     r9d, byte ptr [rdx]
  mov       [rcx], r9b
  mov       [rdx], al

@@Exit:
end;
{$ELSE}
asm
  push      esi
  push      ebx
  mov       esi, ecx
  and       esi, 15
  and       ecx, -16
  jz        @@Swap8Byte
  add       eax, ecx
  add       edx, ecx
  neg       ecx

  .align 16
@@Swap16Byte:
  movdqu    xmm0, [eax+ecx]
  movdqu    xmm1, [edx+ecx]
  movdqu    [eax+ecx], xmm1
  movdqu    [edx+ecx], xmm0
  add       ecx, 16
  jl        @@Swap16Byte

@@Swap8Byte:
  test      esi, esi
  jz        @@Exit
  cmp       esi, 8
  jl        @@Swap4Byte
  movq      xmm0, [eax]
  movq      xmm1, [edx]
  movq      [eax], xmm1
  movq      [edx], xmm0
  sub       esi, 8
  jz        @@Exit
  add       eax, 8
  add       edx, 8

@@Swap4Byte:
  cmp       esi, 4
  jl        @@Swap2Byte
  mov       ebx, [eax]
  mov       ecx, [edx]
  mov       [eax], ecx
  mov       [edx], ebx
  sub       esi, 4
  jz        @@Exit
  add       eax, 4
  add       edx, 4

@@Swap2Byte:
  cmp       esi, 2
  jl        @@Swap1Byte
  movzx     ebx, word ptr [eax]
  movzx     ecx, word ptr [edx]
  mov       [eax], cx
  mov       [edx], bx
  sub       esi, 2
  jz        @@Exit
  add       eax, 2
  add       edx, 2

@@Swap1Byte:
  movzx     ebx, byte ptr [eax]
  movzx     ecx, byte ptr [edx]
  mov       [eax], cl
  mov       [edx], bl

@@Exit:
  pop       ebx
  pop       esi
end;
{$ENDIF}
{$ELSE}
type
  PInt128 = ^Int128;
  Int128 = array[0..3] of Int32;
var
  temp: Int128;
  b: Byte;
begin
  if size >= 16 then
  repeat
    temp := PInt128(left)^;
    PInt128(left)^ := PInt128(right)^;
    PInt128(right)^ := temp;
    Inc(PInt128(left));
    Inc(PInt128(right));
    Dec(size, 16);
  until size < 16;

  if size > 0 then
  repeat
    b := PByte(left)^;
    PByte(left)^ := PByte(right)^;
    PByte(right)^ := b;
    Inc(PByte(left));
    Inc(PByte(right));
    Dec(size);
  until size = 0;
end;
{$ENDIF}

class procedure TArray.Swap<T>(left, right: Pointer);
begin
{$IFDEF DELPHIXE7_UP}
  if not System.HasWeakRef(T) then
{$ENDIF}
    BinarySwap(left, right, SizeOf(T))
end;

class procedure TArray.Reverse_Int8(const values: PInt8; right: NativeInt);
var
  left: NativeInt;
  temp: Int8;
begin
  left := 0;
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Int16(const values: PInt16; right: NativeInt);
var
  left: NativeInt;
  temp: Int16;
begin
  left := 0;
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Int24(const values: PInt24; right: NativeInt);
var
  left: NativeInt;
  temp: Int24;
begin
  left := 0;
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Int32(const values: PInt32; right: NativeInt);
var
  left: NativeInt;
  temp: Int32;
begin
  left := 0;
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Int64(const values: PInt64; right: NativeInt);
var
  left: NativeInt;
  temp: Int64;
begin
  left := 0;
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Single(const values: PSingle; right: NativeInt);
var
  left: NativeInt;
  temp: Single;
begin
  left := 0;
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Double(const values: PDouble; right: NativeInt);
var
  left: NativeInt;
  temp: Double;
begin
  left := 0;
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Extended(const values: PExtended; right: NativeInt);
var
  left: NativeInt;
  temp: Extended;
begin
  left := 0;
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Method(const values: PMethodPointer; right: NativeInt);
var
  left: NativeInt;
  temp: TMethodPointer;
begin
  left := 0;
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Ref(const values: PByte; right, size: NativeInt);
var
  left: NativeInt;
begin
  left := 0;
  while left < right do
  begin
    BinarySwap(@values[left * size], @values[right * size], size);
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse_Generic<T>(var values: array of T);
var
  left, right: NativeInt;
  temp: T;
begin
  left := 0;
  right := High(values);
  while left < right do
  begin
    {$POINTERMATH ON}
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
    {$POINTERMATH OFF}
    Inc(left);
    Dec(right);
  end;
end;

class procedure TArray.Reverse<T>(const values: Pointer; hi: NativeInt);
var
  items: Pointer;
begin
  {$R-}
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkLString, tkWString,
    tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: Reverse_Int8(values, hi);
        2: Reverse_Int16(values, hi);
        4: Reverse_Int32(values, hi);
        8: Reverse_Int64(values, hi);
      end;
    tkFloat:
      case SizeOf(T) of
        4: Reverse_Single(values, hi);
        10: Reverse_Extended(values, hi);
      else
        if GetTypeData(TypeInfo(T)).FloatType = ftDouble then
          Reverse_Double(values, hi)
        else
          Reverse_Int64(values, hi);
      end;
    tkString:
      Reverse_Ref(values, hi, SizeOf(T));
    tkSet:
      case SizeOf(T) of
        1: Reverse_Int8(values, hi);
        2: Reverse_Int16(values, hi);
        4: Reverse_Int32(values, hi);
        8: Reverse_Int64(values, hi);
      else
        Reverse_Ref(values, hi, SizeOf(T));
      end;
    tkMethod:
      Reverse_Method(values, hi);
    tkVariant,
    {$IF Declared(tkMRecord)}
    tkMRecord,
    {$IFEND}
    tkRecord:
      if not System.HasWeakRef(T) then
        case SizeOf(T) of
          1: Reverse_Int8(values, hi);
          2: Reverse_Int16(values, hi);
          3: Reverse_Int24(values, hi);
          4: Reverse_Int32(values, hi);
          8: Reverse_Int64(values, hi);
        else
          Reverse_Ref(values, hi, SizeOf(T))
        end
      else
      begin
        items := values;
        Reverse_Generic<T>(Slice(TSlice<T>(items^), hi+1));
      end;
    tkArray:
      case SizeOf(T) of
        1: Reverse_Int8(values, hi);
        2: Reverse_Int16(values, hi);
        3: Reverse_Int24(values, hi);
        4: Reverse_Int32(values, hi);
        8: Reverse_Int64(values, hi);
      else
        Reverse_Ref(values, hi, SizeOf(T));
      end;
  else
  {$ELSE}
  begin
  {$ENDIF}
    items := values;
    Reverse_Generic<T>(Slice(TSlice<T>(items^), hi+1));
  end;
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.Reverse<T>(var values: array of T);
begin
  {$R-}
  Reverse<T>(@values[0], High(values));
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.Reverse<T>(var values: array of T; index, count: Integer);
begin
  CheckRange(index, count, Length(values));

  Reverse<T>(@values[index], count - 1);
end;

class procedure TArray.Shuffle_Int8(const values: PInt8; hi: NativeInt);
var
  i, randomIndex: NativeInt;
  temp: Int8;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle_Int16(const values: PInt16; hi: NativeInt);
var
  i, randomIndex: NativeInt;
  temp: Int16;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle_Int24(const values: PInt24; hi: NativeInt);
var
  i, randomIndex: NativeInt;
  temp: Int24;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle_Int32(const values: PInt32; hi: NativeInt);
var
  i, randomIndex: NativeInt;
  temp: Int32;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle_Int64(const values: PInt64; hi: NativeInt);
var
  i, randomIndex: NativeInt;
  temp: Int64;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle_Single(const values: PSingle; hi: NativeInt);
var
  i, randomIndex: NativeInt;
  temp: Single;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle_Double(const values: PDouble; hi: NativeInt);
var
  i, randomIndex: NativeInt;
  temp: Double;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle_Extended(const values: PExtended; hi: NativeInt);
var
  i, randomIndex: NativeInt;
  temp: Extended;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle_Method(const values: PMethodPointer; hi: NativeInt);
var
  i, randomIndex: NativeInt;
  temp: TMethodPointer;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle_Ref(const values: PByte; hi, size: NativeInt);
var
  i, randomIndex: NativeInt;
begin
  for i := hi downto 1 do
  begin
    randomIndex := Random(i + 1);
    BinarySwap(@values[randomIndex * size], @values[i * size], size);
  end;
end;

class procedure TArray.Shuffle_Generic<T>(var values: array of T);
var
  i, randomIndex: NativeInt;
  temp: T;
begin
  for i := High(values) downto 1 do
  begin
    randomIndex := Random(i + 1);
    {$POINTERMATH ON}
    temp := values[randomIndex];
    values[randomIndex] := values[i];
    values[i] := temp;
    {$POINTERMATH OFF}
  end;
end;

class procedure TArray.Shuffle<T>(const values: Pointer; hi: NativeInt);
begin
  {$R-}
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkLString, tkWString,
    tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: Shuffle_Int8(values, hi);
        2: Shuffle_Int16(values, hi);
        4: Shuffle_Int32(values, hi);
        8: Shuffle_Int64(values, hi);
      end;
    tkFloat:
      case SizeOf(T) of
        4: Shuffle_Single(values, hi);
        10: Shuffle_Extended(values, hi);
      else
        if GetTypeData(TypeInfo(T)).FloatType = ftDouble then
          Shuffle_Double(values, hi)
        else
          Shuffle_Int64(values, hi);
      end;
    tkString:
      Shuffle_Ref(values, hi, SizeOf(T));
    tkSet:
      case SizeOf(T) of
        1: Shuffle_Int8(values, hi);
        2: Shuffle_Int16(values, hi);
        4: Shuffle_Int32(values, hi);
        8: Shuffle_Int64(values, hi);
      else
        Shuffle_Ref(values, hi, SizeOf(T));
      end;
    tkMethod:
      Shuffle_Method(values, hi);
    tkVariant,
    {$IF Declared(tkMRecord)}
    tkMRecord,
    {$IFEND}
    tkRecord:
      if not System.HasWeakRef(T) then
        case SizeOf(T) of
          1: Shuffle_Int8(values, hi);
          2: Shuffle_Int16(values, hi);
          3: Shuffle_Int24(values, hi);
          4: Shuffle_Int32(values, hi);
          8: Shuffle_Int64(values, hi);
        else
          Shuffle_Ref(values, hi, SizeOf(T))
        end
      else
        Shuffle_Generic<T>(Slice(TSlice<T>(values^), hi+1));
    tkArray:
      case SizeOf(T) of
        1: Shuffle_Int8(values, hi);
        2: Shuffle_Int16(values, hi);
        3: Shuffle_Int24(values, hi);
        4: Shuffle_Int32(values, hi);
        8: Shuffle_Int64(values, hi);
      else
        Shuffle_Ref(values, hi, SizeOf(T));
      end;
  else
  {$ELSE}
  begin
  {$ENDIF}
    Shuffle_Generic<T>(Slice(TSlice<T>(values^), hi+1));
  end;
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.Shuffle<T>(var values: array of T);
begin
  {$R-}
  Shuffle<T>(@values[0], High(values));
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.Shuffle<T>(var values: array of T; index, count: Integer);
begin
  CheckRange(index, count, Length(values));

  {$R-}
  Shuffle<T>(@values[index], count - 1);
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class function TArray.GetDepthLimit(count: Integer): Integer;
{$IFDEF ASSEMBLER}
asm
  inc eax
  or eax,1
  bsr eax,eax
  shl eax,1
end;
{$ELSE}
const
  Log2DeBruijn: array[0..31] of Byte = (
    00, 09, 01, 10, 13, 21, 02, 29,
    11, 14, 16, 18, 22, 25, 03, 30,
    08, 12, 20, 28, 15, 17, 24, 07,
    19, 27, 23, 06, 26, 05, 04, 31
  );
begin
  Inc(count);
  count := count or (count shr 1);
  count := count or (count shr 2);
  count := count or (count shr 4);
  count := count or (count shr 8);
  count := count or (count shr 16);
  {$Q-}
  Result := Log2DeBruijn[(count * $07C4ACDD) shr 27] * 2;
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
end;
{$ENDIF}

{$IFDEF DELPHIXE7_UP}
class procedure TArray.StableSort<T>(var values: array of T);
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  if ((GetTypeKind(T) in ManagedPointerTypeKinds) and UnsafeStableSort)
    or (GetTypeKind(T) in UnmanagedPointerTypeKinds) then
    TTimSort.Sort(@values, IComparer<Pointer>(comparer), 0, Length(values))
  else
    TTimSort.Sort<T>(@values, IComparer<T>(comparer), 0, Length(values));
end;

class procedure TArray.StableSort<T>(var values: array of T; index, count: Integer);
var
  comparer: Pointer;
begin
  CheckRange(index, count, Length(values));

  comparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  if ((GetTypeKind(T) in ManagedPointerTypeKinds) and UnsafeStableSort)
    or (GetTypeKind(T) in UnmanagedPointerTypeKinds) then
    TTimSort.Sort(@values, IComparer<Pointer>(comparer), index, count)
  else
    TTimSort.Sort<T>(@values, IComparer<T>(comparer), index, count);
end;

class procedure TArray.StableSort<T>(var values: array of T; const comparer: IComparer<T>);
begin
  if ((GetTypeKind(T) in ManagedPointerTypeKinds) and UnsafeStableSort)
    or (GetTypeKind(T) in UnmanagedPointerTypeKinds) then
    TTimSort.Sort(@values, IComparer<Pointer>(comparer), 0, Length(values))
  else
    TTimSort.Sort<T>(@values, comparer, 0, Length(values));
end;

class procedure TArray.StableSort<T>(var values: array of T;
  const comparer: IComparer<T>; index, count: Integer);
begin
  CheckRange(index, count, Length(values));

  if ((GetTypeKind(T) in ManagedPointerTypeKinds) and UnsafeStableSort)
    or (GetTypeKind(T) in UnmanagedPointerTypeKinds) then
    TTimSort.Sort(@values, IComparer<Pointer>(comparer), index, count)
  else
    TTimSort.Sort<T>(@values, comparer, index, count);
end;

class procedure TArray.StableSort<T>(var values: array of T;
  const comparison: TComparison<T>);
begin
  if ((GetTypeKind(T) in ManagedPointerTypeKinds) and UnsafeStableSort)
    or (GetTypeKind(T) in UnmanagedPointerTypeKinds) then
    TTimSort.Sort(@values, IComparer<Pointer>(PPointer(@comparison)^), 0, Length(values))
  else
    TTimSort.Sort<T>(@values, IComparer<T>(PPointer(@comparison)^), 0, Length(values));
end;

class procedure TArray.StableSort<T>(var values: array of T;
  const comparison: TComparison<T>; index, count: Integer);
begin
  CheckRange(index, count, Length(values));

  if ((GetTypeKind(T) in ManagedPointerTypeKinds) and UnsafeStableSort)
    or (GetTypeKind(T) in UnmanagedPointerTypeKinds) then
    TTimSort.Sort(@values, IComparer<Pointer>(PPointer(@comparison)^), index, count)
  else
    TTimSort.Sort<T>(@values, IComparer<T>(PPointer(@comparison)^), index, count);
end;
{$ENDIF}

class procedure TArray.DownHeap<T>(var values: array of T;
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>; i: NativeInt);
var
  hi, child: NativeInt;
  temp: T;
begin
  hi := High(values);
  while True do
  begin
    child := i * 2 + 1;
    if child > hi then Break;
    if (child < hi) and (compare(values[child], values[child + 1]) < 0) then
      Inc(child);
    if compare(values[i], values[child]) >= 0 then
      Break;
    temp := values[i];
    values[i] := values[child];
    values[child] := temp;
    i := child;
  end;
end;

class procedure TArray.HeapSort<T>(var values: array of T;
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>);
var
  i: NativeInt;
  temp: T;
begin
  for i := Length(values) shr 1 - 1 downto 0 do
    DownHeap<T>(values, compare, i);
  for i := High(values) downto 1 do
  begin
    temp := values[0];
    values[0] := values[i];
    values[i] := temp;
    DownHeap<T>(Slice(values, i), compare, 0);
  end;
end;

class procedure TArray.InsertionSort<T>(var values: array of T;
  const comparer: IComparer<T>);
var
  index, start: NativeInt;
  temp: T;
  compare: TCompareMethod<T>;
begin
  TMethod(compare).Data := Pointer(comparer);
  TMethod(compare).Code := PPVTable(comparer)^[3];
  start := 1;
  repeat
    index := start;
    repeat
      if compare(values[index-1], values[start]) <= 0 then Break;
      Dec(index);
    until index = 0;

    if index < start then
    begin
      temp := values[start];
      if TType.HasWeakRef<T> then
        MoveManaged(@values[index], @values[index+1], TypeInfo(T), start - index)
      else
      begin
        if TType.IsManaged<T> then
          System.Finalize(values[start]);
        Move(values[index], values[index+1], SizeOf(T) * (start - index));
        if TType.IsManaged<T> then
          if SizeOf(T) = SizeOf(Pointer) then
            PPointer(@values[index])^ := nil
          else
            System.FillChar(values[index], SizeOf(T), 0);
      end;
      values[index] := temp;
    end;

    Inc(start);
  until start > High(values);
end;

class function TArray.QuickSortPartition<T>(var values: array of T;
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethod<T>): NativeInt;
var
  left, right, middle: NativeInt;
  temp: T;
  helper: TQuickSortPartitionHelper<T>;
begin
  right := High(values);
  middle := right shr 1;
  helper.compare := compare;

  if helper.compare(values[0], values[middle]) > 0 then
  begin
    temp := values[0];
    values[0] := values[middle];
    values[middle] := temp;
  end;
  if helper.compare(values[0], values[right]) > 0 then
  begin
    temp := values[0];
    values[0] := values[right];
    values[right] := temp;
  end;
  if helper.compare(values[middle], values[right]) > 0 then
  begin
    temp := values[middle];
    values[middle] := values[right];
    values[right] := temp;
  end;

  Dec(right);
  helper.pivotIndex := right;
  temp := values[middle];
  values[middle] := values[right];
  values[right] := temp;
  helper.pivot := temp;

  left := 0;

  if left < right then
    while True do
    begin
      repeat
        Inc(left);
      until helper.compare(values[left], helper.pivot) >= 0;
      repeat
        Dec(right);
      until helper.compare(helper.pivot, values[right]) >= 0;

      if left >= right then
        Break;

      helper.temp := values[left];
      values[left] := values[right];
      values[right] := helper.temp;
    end;

  right := helper.pivotIndex;
  if left <> right then
  begin
    temp := values[left];
    values[left] := values[right];
    values[right] := temp;
  end;

  Result := left;
end;

class procedure TArray.IntroSort<T>(var values: array of T;
  const comparer: IComparer<T>; depthLimit: Integer);
var
  partitionSize, pivot: NativeInt;
  compare: TCompareMethod<T>;
begin
  TMethod(compare).Data := Pointer(comparer);
  TMethod(compare).Code := PPVTable(comparer)^[3];

  partitionSize := Length(values);
  while partitionSize > 1 do
  begin
    if partitionSize <= IntrosortSizeThreshold then
    begin
      InsertionSort<T>(Slice(values, partitionSize), IComparer<T>(TMethod(compare).Data));
      Exit;
    end
    else
    begin
      if depthLimit = 0 then
      begin
        HeapSort<T>(Slice(values, partitionSize), compare);
        Exit;
      end;

      if depthLimit < 0 then
        depthLimit := GetDepthLimit(partitionSize);
      Dec(depthLimit);

      pivot := QuickSortPartition<T>(Slice(values, partitionSize), compare);
      {$R-}
      IntroSort<T>(Slice(TSlice<T>((@values[pivot + 1])^), partitionSize - (pivot + 1)), IComparer<T>(TMethod(compare).Data), depthLimit);
      {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
      partitionSize := pivot;
    end;
  end;
end;

class procedure TArray.DownHeap_Ref(values: PByte; hi: NativeInt;
  const compare: TCompareMethodRef; i, size: NativeInt);
var
  child: NativeInt;
begin
  while True do
  begin
    child := i * 2 + 1;
    if child > hi then
      Break;

    if (child < hi) and (compare(values[child*size], values[(child+1)*size]) < 0) then
      inc(child);
    if compare(values[i*size], values[child*size]) >= 0 then
      Break;

    BinarySwap(@values[i*size], @values[child*size], size);
    i := child;
  end;
end;

class procedure TArray.HeapSort_Ref(values: PByte; hi: NativeInt;
  const compare: TCompareMethodRef; size: NativeInt);
var
  i: NativeInt;
begin
  for i := (hi+1) shr 1 - 1 downto 0 do
    DownHeap_Ref(values, hi, compare, i, size);
  for i := hi downto 1 do
  begin
    BinarySwap(@values[i*size], @values[0], size);
    DownHeap_Ref(values, i-1, compare, 0, size);
  end;
end;

class procedure TArray.InsertionSort_Ref(values: PByte; hi: NativeInt;
  const compare: TCompareMethodRef; size: NativeInt);
var
  i, j: NativeInt;
begin
  i := size;
  hi := hi * size;
  repeat
    j := i;
    repeat
      if compare(values[j-size], values[j]) <= 0 then Break;
      BinarySwap(@values[j-size], @values[j], size);
      Dec(j, size);
    until j = 0;
    Inc(i, size);
  until i > hi;
end;

class function TArray.QuickSortPartition_Ref(values: PByte; hi: NativeInt;
  {$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}const compare: TCompareMethodRef; size: NativeInt): NativeInt;
var
  left, right, middle, pivotIndex: NativeInt;
begin
  middle := (hi shr 1) * size;
  right := hi * size;

  if compare(values[0], values[middle]) > 0 then
    BinarySwap(@values[0], @values[middle], size);
  if compare(values[0], values[right]) > 0 then
    BinarySwap(@values[0], @values[right], size);
  if compare(values[middle], values[right]) > 0 then
    BinarySwap(@values[middle], @values[right], size);

  Dec(right, size);
  BinarySwap(@values[middle], @values[right], size);

  pivotIndex := right;
  left := 0;

  repeat
    repeat
      Inc(left, size);
    until compare(values[left], values[pivotIndex]) >= 0;
    repeat
      Dec(right, size);
    until compare(values[pivotIndex], values[right]) >= 0;

    if left >= right then
      Break;

    BinarySwap(@values[left], @values[right], size);
  until left >= right;

  if left <> pivotIndex then
    BinarySwap(@values[left], @values[pivotIndex], size);

  Result := left div size;
end;

class procedure TArray.IntroSort_Ref(values: PByte; hi: NativeInt;
  const comparer: IComparerRef; size: NativeInt; depthLimit: Integer);
var
  partitionSize, pivot: NativeInt;
  compare: TCompareMethodRef;
begin
  TMethod(compare).Data := Pointer(comparer);
  TMethod(compare).Code := PPVTable(comparer)^[3];

  partitionSize := hi + 1;
  while partitionSize > 1 do
  begin
    if partitionSize <= IntrosortSizeThreshold then
    begin
      InsertionSort_Ref(values, partitionSize - 1, compare, size);
      Exit;
    end
    else
    begin
      if depthLimit = 0 then
      begin
        HeapSort_Ref(values, partitionSize - 1, compare, size);
        Exit;
      end;

      if depthLimit < 0 then
        depthLimit := GetDepthLimit(partitionSize);

      Dec(depthLimit);
      pivot := QuickSortPartition_Ref(values, partitionSize - 1, compare, size);
      {$R-}
      IntroSort_Ref(@values[(pivot + 1) * size], partitionSize - (pivot + 1) - 1, IComparerRef(TMethod(compare).Data), size, depthLimit);
      {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
      partitionSize := pivot;
    end;
  end;
end;

class procedure TArray.IntroSort_Int8(var values: array of Int8; const comparer: IComparer<Int8>);
begin
  {$Q-}{$R-}
  IntroSort<Int8>(values, comparer);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.IntroSort_Int16(var values: array of Int16; const comparer: IComparer<Int16>);
begin
  {$Q-}{$R-}
  IntroSort<Int16>(values, comparer);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.IntroSort_Int24(var values: array of Int24; const comparer: IComparer<Int24>);
begin
  {$Q-}{$R-}
  IntroSort<Int24>(values, comparer);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.IntroSort_Int32(var values: array of Int32; const comparer: IComparer<Int32>);
begin
  {$Q-}{$R-}
  IntroSort<Int32>(values, comparer);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.IntroSort_Int64(var values: array of Int64; const comparer: IComparer<Int64>);
begin
  {$Q-}{$R-}
  IntroSort<Int64>(values, comparer);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.IntroSort_Single(var values: array of Single; const comparer: IComparer<Single>);
begin
  {$Q-}{$R-}
  IntroSort<Single>(values, comparer);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.IntroSort_Double(var values: array of Double; const comparer: IComparer<Double>);
begin
  {$Q-}{$R-}
  IntroSort<Double>(values, comparer);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.IntroSort_Extended(var values: array of Extended; const comparer: IComparer<Extended>);
begin
  {$Q-}{$R-}
  IntroSort<Extended>(values, comparer);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.IntroSort_Method(var values: array of TMethodPointer; const comparer: IComparer<TMethodPointer>);
begin
  {$Q-}{$R-}
  IntroSort<TMethodPointer>(values, comparer);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}{$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.Sort<T>(var values: array of T);
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  {$R-}
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkLString, tkWString,
    tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(comparer));
        2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(comparer));
        4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(comparer));
        8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
      end;
    tkFloat:
      case SizeOf(T) of
        4: IntroSort_Single(Slice(TSlice<Single>((@values[0])^), Length(values)), IComparer<Single>(comparer));
        10,16: IntroSort_Extended(Slice(TSlice<Extended>((@values[0])^), Length(values)), IComparer<Extended>(comparer));
      else
        if GetTypeData(TypeInfo(T)).FloatType = ftDouble then
          IntroSort_Double(Slice(TSlice<Double>((@values[0])^), Length(values)), IComparer<Double>(comparer))
        else
          IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
      end;
    tkString:
      IntroSort_Ref(@values[0], High(values), IComparerRef(comparer), SizeOf(T));
    tkSet:
      case SizeOf(T) of
        1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(comparer));
        2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(comparer));
        4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(comparer));
      {$IFDEF PASS_64BIT_VALUE_REGISTER}
        8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
      {$ENDIF}
      else
        IntroSort_Ref(@values[0], High(values), IComparerRef(comparer), SizeOf(T));
      end;
    tkMethod:
      IntroSort_Method(Slice(TSlice<TMethodPointer>((@values[0])^), Length(values)), IComparer<TMethodPointer>(comparer));
    tkVariant,
    {$IF Declared(tkMRecord)}
    tkMRecord,
    {$IFEND}
    tkRecord:
      if not System.HasWeakRef(T) then
        case SizeOf(T) of
          1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(comparer));
          2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(comparer));
          3: IntroSort_Int24(Slice(TSlice<Int24>((@values[0])^), Length(values)), IComparer<Int24>(comparer));
          4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(comparer));
        {$IFDEF PASS_64BIT_VALUE_REGISTER}
          8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
        {$ENDIF}
        else
          IntroSort_Ref(@values[0], High(values), IComparerRef(comparer), SizeOf(T))
        end
      else
        IntroSort<T>(values, IComparer<T>(comparer));
    tkArray:
      case SizeOf(T) of
        1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(comparer));
        2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(comparer));
        3: IntroSort_Int24(Slice(TSlice<Int24>((@values[0])^), Length(values)), IComparer<Int24>(comparer));
        4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(comparer));
      {$IFDEF PASS_64BIT_VALUE_REGISTER}
        8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
      {$ENDIF}
      else
        IntroSort_Ref(@values[0], High(values), IComparerRef(comparer), SizeOf(T));
      end;
  else{$ELSE}begin{$ENDIF}
    IntroSort<T>(values, IComparer<T>(comparer));
  end;
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.Sort<T>(var values: array of T; const comparer: IComparer<T>);
begin
  {$R-}
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkLString, tkWString,
    tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(comparer));
        2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(comparer));
        4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(comparer));
        8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
      end;
    tkFloat:
      case SizeOf(T) of
        4: IntroSort_Single(Slice(TSlice<Single>((@values[0])^), Length(values)), IComparer<Single>(comparer));
        10,16: IntroSort_Extended(Slice(TSlice<Extended>((@values[0])^), Length(values)), IComparer<Extended>(comparer));
      else
        if GetTypeData(TypeInfo(T)).FloatType = ftDouble then
          IntroSort_Double(Slice(TSlice<Double>((@values[0])^), Length(values)), IComparer<Double>(comparer))
        else
          IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
      end;
    tkString:
      IntroSort_Ref(@values[0], High(values), IComparerRef(comparer), SizeOf(T));
    tkSet:
      case SizeOf(T) of
        1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(comparer));
        2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(comparer));
        4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(comparer));
      {$IFDEF PASS_64BIT_VALUE_REGISTER}
        8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
      {$ENDIF}
      else
        IntroSort_Ref(@values[0], High(values), IComparerRef(comparer), SizeOf(T));
      end;
    tkMethod:
      IntroSort_Method(Slice(TSlice<TMethodPointer>((@values[0])^), Length(values)), IComparer<TMethodPointer>(comparer));
    tkVariant,
    {$IF Declared(tkMRecord)}
    tkMRecord,
    {$IFEND}
    tkRecord:
      if not System.HasWeakRef(T) then
        case SizeOf(T) of
          1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(comparer));
          2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(comparer));
          3: IntroSort_Int24(Slice(TSlice<Int24>((@values[0])^), Length(values)), IComparer<Int24>(comparer));
          4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(comparer));
        {$IFDEF PASS_64BIT_VALUE_REGISTER}
          8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
        {$ENDIF}
        else
          IntroSort_Ref(@values[0], High(values), IComparerRef(comparer), SizeOf(T))
        end
      else
        IntroSort<T>(values, comparer);
    tkArray:
      case SizeOf(T) of
        1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(comparer));
        2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(comparer));
        3: IntroSort_Int24(Slice(TSlice<Int24>((@values[0])^), Length(values)), IComparer<Int24>(comparer));
        4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(comparer));
      {$IFDEF PASS_64BIT_VALUE_REGISTER}
        8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(comparer));
      {$ENDIF}
      else
        IntroSort_Ref(@values[0], High(values), IComparerRef(comparer), SizeOf(T));
      end;
  else{$ELSE}begin{$ENDIF}
    IntroSort<T>(values, comparer);
  end;
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.Sort<T>(var values: array of T; const comparer: IComparer<T>; index, count: Integer);
begin
  CheckRange(index, count, Length(values));

  if count > 1 then
  begin
    {$R-}
    {$IFDEF DELPHIXE7_UP}
    case GetTypeKind(T) of
      tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkLString, tkWString,
      tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef, tkPointer, tkProcedure:
        case SizeOf(T) of
          1: IntroSort_Int8(Slice(TSlice<Int8>((@values[index])^), count), IComparer<Int8>(comparer));
          2: IntroSort_Int16(Slice(TSlice<Int16>((@values[index])^), count), IComparer<Int16>(comparer));
          4: IntroSort_Int32(Slice(TSlice<Int32>((@values[index])^), count), IComparer<Int32>(comparer));
          8: IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(comparer));
        end;
      tkFloat:
        case SizeOf(T) of
          4: IntroSort_Single(Slice(TSlice<Single>((@values[index])^), count), IComparer<Single>(comparer));
          10,16: IntroSort_Extended(Slice(TSlice<Extended>((@values[index])^), count), IComparer<Extended>(comparer));
        else
          if GetTypeData(TypeInfo(T)).FloatType = ftDouble then
            IntroSort_Double(Slice(TSlice<Double>((@values[index])^), count), IComparer<Double>(comparer))
          else
            IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(comparer));
        end;
      tkString:
        IntroSort_Ref(@values[index], count - 1, IComparerRef(comparer), SizeOf(T));
      tkSet:
        case SizeOf(T) of
          1: IntroSort_Int8(Slice(TSlice<Int8>((@values[index])^), count), IComparer<Int8>(comparer));
          2: IntroSort_Int16(Slice(TSlice<Int16>((@values[index])^), count), IComparer<Int16>(comparer));
          4: IntroSort_Int32(Slice(TSlice<Int32>((@values[index])^), count), IComparer<Int32>(comparer));
        {$IFDEF PASS_64BIT_VALUE_REGISTER}
          8: IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(comparer));
        {$ENDIF}
        else
          IntroSort_Ref(@values[index], count - 1, IComparerRef(comparer), SizeOf(T));
        end;
      tkMethod:
        IntroSort_Method(Slice(TSlice<TMethodPointer>((@values[index])^), count), IComparer<TMethodPointer>(comparer));
      tkVariant,
      {$IF Declared(tkMRecord)}
      tkMRecord,
      {$IFEND}
      tkRecord:
        if not System.HasWeakRef(T) then
          case SizeOf(T) of
            1: IntroSort_Int8(Slice(TSlice<Int8>((@values[index])^), count), IComparer<Int8>(comparer));
            2: IntroSort_Int16(Slice(TSlice<Int16>((@values[index])^), count), IComparer<Int16>(comparer));
            3: IntroSort_Int24(Slice(TSlice<Int24>((@values[index])^), count), IComparer<Int24>(comparer));
            4: IntroSort_Int32(Slice(TSlice<Int32>((@values[index])^), count), IComparer<Int32>(comparer));
          {$IFDEF PASS_64BIT_VALUE_REGISTER}
            8: IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(comparer));
          {$ENDIF}
          else
            IntroSort_Ref(@values[index], count - 1, IComparerRef(comparer), SizeOf(T))
          end
        else
          IntroSort<T>(values, comparer);
      tkArray:
        case SizeOf(T) of
          1: IntroSort_Int8(Slice(TSlice<Int8>((@values[index])^), count), IComparer<Int8>(comparer));
          2: IntroSort_Int16(Slice(TSlice<Int16>((@values[index])^), count), IComparer<Int16>(comparer));
          3: IntroSort_Int24(Slice(TSlice<Int24>((@values[index])^), count), IComparer<Int24>(comparer));
          4: IntroSort_Int32(Slice(TSlice<Int32>((@values[index])^), count), IComparer<Int32>(comparer));
        {$IFDEF PASS_64BIT_VALUE_REGISTER}
          8: IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(comparer));
        {$ENDIF}
        else
          IntroSort_Ref(@values[index], count - 1, IComparerRef(comparer), SizeOf(T));
        end;
    else{$ELSE}begin{$ENDIF}
      IntroSort<T>(values, comparer);
    end;
    {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  end;
end;

class procedure TArray.Sort<T>(var values: array of T; const comparison: TComparison<T>);
begin
  {$R-}
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkLString, tkWString,
    tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(PPointer(@comparison)^));
        2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(PPointer(@comparison)^));
        4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(PPointer(@comparison)^));
        8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(PPointer(@comparison)^));
      end;
    tkFloat:
      case SizeOf(T) of
        4: IntroSort_Single(Slice(TSlice<Single>((@values[0])^), Length(values)), IComparer<Single>(PPointer(@comparison)^));
        10,16: IntroSort_Extended(Slice(TSlice<Extended>((@values[0])^), Length(values)), IComparer<Extended>(PPointer(@comparison)^));
      else
        if GetTypeData(TypeInfo(T)).FloatType = ftDouble then
          IntroSort_Double(Slice(TSlice<Double>((@values[0])^), Length(values)), IComparer<Double>(PPointer(@comparison)^))
        else
          IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(PPointer(@comparison)^));
      end;
    tkString:
      IntroSort_Ref(@values[0], High(values), IComparerRef(PPointer(@comparison)^), SizeOf(T));
    tkSet:
      case SizeOf(T) of
        1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(PPointer(@comparison)^));
        2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(PPointer(@comparison)^));
        4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(PPointer(@comparison)^));
      {$IFDEF PASS_64BIT_VALUE_REGISTER}
        8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(PPointer(@comparison)^));
      {$ENDIF}
      else
        IntroSort_Ref(@values[0], High(values), IComparerRef(PPointer(@comparison)^), SizeOf(T));
      end;
    tkMethod:
      IntroSort_Method(Slice(TSlice<TMethodPointer>((@values[0])^), Length(values)), IComparer<TMethodPointer>(PPointer(@comparison)^));
    tkVariant,
    {$IF Declared(tkMRecord)}
    tkMRecord,
    {$IFEND}
    tkRecord:
      if not System.HasWeakRef(T) then
        case SizeOf(T) of
          1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(PPointer(@comparison)^));
          2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(PPointer(@comparison)^));
          3: IntroSort_Int24(Slice(TSlice<Int24>((@values[0])^), Length(values)), IComparer<Int24>(PPointer(@comparison)^));
          4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(PPointer(@comparison)^));
        {$IFDEF PASS_64BIT_VALUE_REGISTER}
          8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(PPointer(@comparison)^));
        {$ENDIF}
        else
          IntroSort_Ref(@values[0], High(values), IComparerRef(PPointer(@comparison)^), SizeOf(T))
        end
      else
        IntroSort<T>(values, IComparer<T>(PPointer(@comparison)^));
    tkArray:
      case SizeOf(T) of
        1: IntroSort_Int8(Slice(TSlice<Int8>((@values[0])^), Length(values)), IComparer<Int8>(PPointer(@comparison)^));
        2: IntroSort_Int16(Slice(TSlice<Int16>((@values[0])^), Length(values)), IComparer<Int16>(PPointer(@comparison)^));
        3: IntroSort_Int24(Slice(TSlice<Int24>((@values[0])^), Length(values)), IComparer<Int24>(PPointer(@comparison)^));
        4: IntroSort_Int32(Slice(TSlice<Int32>((@values[0])^), Length(values)), IComparer<Int32>(PPointer(@comparison)^));
      {$IFDEF PASS_64BIT_VALUE_REGISTER}
        8: IntroSort_Int64(Slice(TSlice<Int64>((@values[0])^), Length(values)), IComparer<Int64>(PPointer(@comparison)^));
      {$ENDIF}
      else
        IntroSort_Ref(@values[0], High(values), IComparerRef(PPointer(@comparison)^), SizeOf(T));
      end;
  else{$ELSE}begin{$ENDIF}
    IntroSort<T>(values, IComparer<T>(PPointer(@comparison)^));
  end;
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class procedure TArray.Sort<T>(var values: array of T; const comparison: TComparison<T>; index, count: Integer);
begin
  CheckRange(index, count, Length(values));

  if count > 1 then
  begin
    {$R-}
    {$IFDEF DELPHIXE7_UP}
    case GetTypeKind(T) of
      tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkLString, tkWString,
      tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef, tkPointer, tkProcedure:
        case SizeOf(T) of
          1: IntroSort_Int8(Slice(TSlice<Int8>((@values[index])^), count), IComparer<Int8>(PPointer(@comparison)^));
          2: IntroSort_Int16(Slice(TSlice<Int16>((@values[index])^), count), IComparer<Int16>(PPointer(@comparison)^));
          4: IntroSort_Int32(Slice(TSlice<Int32>((@values[index])^), count), IComparer<Int32>(PPointer(@comparison)^));
          8: IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(PPointer(@comparison)^));
        end;
      tkFloat:
        case SizeOf(T) of
          4: IntroSort_Single(Slice(TSlice<Single>((@values[index])^), count), IComparer<Single>(PPointer(@comparison)^));
          10,16: IntroSort_Extended(Slice(TSlice<Extended>((@values[index])^), count), IComparer<Extended>(PPointer(@comparison)^));
        else
          if GetTypeData(TypeInfo(T)).FloatType = ftDouble then
            IntroSort_Double(Slice(TSlice<Double>((@values[index])^), count), IComparer<Double>(PPointer(@comparison)^))
          else
            IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(PPointer(@comparison)^));
        end;
      tkString:
        IntroSort_Ref(@values[index], count - 1, IComparerRef(PPointer(@comparison)^), SizeOf(T));
      tkSet:
        case SizeOf(T) of
          1: IntroSort_Int8(Slice(TSlice<Int8>((@values[index])^), count), IComparer<Int8>(PPointer(@comparison)^));
          2: IntroSort_Int16(Slice(TSlice<Int16>((@values[index])^), count), IComparer<Int16>(PPointer(@comparison)^));
          4: IntroSort_Int32(Slice(TSlice<Int32>((@values[index])^), count), IComparer<Int32>(PPointer(@comparison)^));
        {$IFDEF PASS_64BIT_VALUE_REGISTER}
          8: IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(PPointer(@comparison)^));
        {$ENDIF}
        else
          IntroSort_Ref(@values[index], count - 1, IComparerRef(PPointer(@comparison)^), SizeOf(T));
        end;
      tkMethod:
        IntroSort_Method(Slice(TSlice<TMethodPointer>((@values[index])^), count), IComparer<TMethodPointer>(PPointer(@comparison)^));
      tkVariant,
      {$IF Declared(tkMRecord)}
      tkMRecord,
      {$IFEND}
      tkRecord:
        if not System.HasWeakRef(T) then
          case SizeOf(T) of
            1: IntroSort_Int8(Slice(TSlice<Int8>((@values[index])^), count), IComparer<Int8>(PPointer(@comparison)^));
            2: IntroSort_Int16(Slice(TSlice<Int16>((@values[index])^), count), IComparer<Int16>(PPointer(@comparison)^));
            3: IntroSort_Int24(Slice(TSlice<Int24>((@values[index])^), count), IComparer<Int24>(PPointer(@comparison)^));
            4: IntroSort_Int32(Slice(TSlice<Int32>((@values[index])^), count), IComparer<Int32>(PPointer(@comparison)^));
          {$IFDEF PASS_64BIT_VALUE_REGISTER}
            8: IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(PPointer(@comparison)^));
          {$ENDIF}
          else
            IntroSort_Ref(@values[index], count - 1, IComparerRef(PPointer(@comparison)^), SizeOf(T))
          end
        else
          IntroSort<T>(values, IComparer<T>(PPointer(@comparison)^));
      tkArray:
        case SizeOf(T) of
          1: IntroSort_Int8(Slice(TSlice<Int8>((@values[index])^), count), IComparer<Int8>(PPointer(@comparison)^));
          2: IntroSort_Int16(Slice(TSlice<Int16>((@values[index])^), count), IComparer<Int16>(PPointer(@comparison)^));
          3: IntroSort_Int24(Slice(TSlice<Int24>((@values[index])^), count), IComparer<Int24>(PPointer(@comparison)^));
          4: IntroSort_Int32(Slice(TSlice<Int32>((@values[index])^), count), IComparer<Int32>(PPointer(@comparison)^));
        {$IFDEF PASS_64BIT_VALUE_REGISTER}
          8: IntroSort_Int64(Slice(TSlice<Int64>((@values[index])^), count), IComparer<Int64>(PPointer(@comparison)^));
        {$ENDIF}
        else
          IntroSort_Ref(@values[index], count - 1, IComparerRef(PPointer(@comparison)^), SizeOf(T));
        end;
    else{$ELSE}begin{$ENDIF}
      IntroSort<T>(values, IComparer<T>(PPointer(@comparison)^));
    end;
    {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TTimSort'}

{$IFDEF DELPHIXE7_UP}
procedure TTimSort.Initialize(const comparer: IInterface;
  const compareFunc: Pointer; mergeLo, mergeHi: TMergeFunc;
  arrayTypeInfo: PTypeInfo; itemSize: Integer);
begin
  TMethod(fCompare).Data := Pointer(comparer);
  TMethod(fCompare).Code := PPVTable(comparer)^[3];
  TMethod(fCompareFunc).Code := compareFunc;
  TMethod(fCompareFunc).Data := @TMethod(fCompare);
  fMergeLo := mergeLo;
  fMergeHi := mergeHi;
  fTmp := nil;
  fItemSize := itemSize;
  fMinGallop := MIN_GALLOP;
  fStackLen := 0;
  fArrayTypeInfo := arrayTypeInfo;
end;

procedure TTimSort.Finalize;
begin
  DynArrayClear(fTmp, fArrayTypeInfo);
end;

function TTimSort.at(items: Pointer; index: Integer): Pointer;
begin
  Result := PByte(items) + index * fItemSize;
end;

class function TTimSort.CompareThunk<T>(instance: Pointer; const left, right): Integer;
begin
  Result := TComparerMethod<T>(instance^)(T(left), T(right));
end;

class procedure TTimSort.BinaryInsertionSort<T>(var values: array of T;
  start: NativeInt; const comparer: IComparer<T>);
var
  index, count, right: NativeInt;
  pivot: T;
  compare: TComparerMethod<T>;
begin
  TMethod(compare).Data := Pointer(comparer);
  TMethod(compare).Code := PPVTable(comparer)^[3];
  while start <= High(values) do
  begin
    count := start;
    index := 0;
    if count > 0 then
    repeat
      right := index + (1 and count);
      count := count shr 1;
      right := right + count;
      if compare(values[index + count], values[start]) > 0 then Continue;
      index := right;
    until count = 0;
    if index < start then
    begin
      pivot := values[start];
      if TType.HasWeakRef<T> then
        MoveManaged(@values[index], @values[index+1], TypeInfo(T), start - index)
      else
      begin
        if TType.IsManaged<T> then
          System.Finalize(values[start]);
        Move(values[index], values[index+1], SizeOf(T) * (start - index));
        if TType.IsManaged<T> then
          System.FillChar(values[index], SizeOf(T), 0);
      end;
      values[index] := pivot;
    end;
    Inc(start);
  end;
end;

class function TTimSort.CountRunAndMakeAscending<T>(var values: array of T;
  const comparer: IComparer<T>): Integer;
var
  {$POINTERMATH ON}
  items: ^T;
  {$POINTERMATH ON}
  compare: TComparerMethod<T>;
begin
  TMethod(compare).Data := Pointer(comparer);
  TMethod(compare).Code := PPVTable(comparer)^[3];
  Result := 1;
  if High(values) > 0 then
  begin
    items := @values[0];
    // descending?
    if compare(items[0], items[1]) > 0 then
    begin
      // find end of run, and reverse range if descending
      while (Result < High(values)) and (compare(items[Result], items[Result + 1]) > 0) do
        Inc(Result);
      TArray.Reverse<T>(items, Result);
    end;
    (* ascending
       even if the run was initially descending, after reversing it the
       following elements may form an ascending continuation of the
       now-reversed run.
       unconditionally attempt to continue the ascending run *)
    repeat
      Inc(Result);
    until (Result > High(values)) or (compare(items[Result-1], items[Result]) > 0);
  end;
end;

class function TTimSort.MinRunLength(n: Integer): Integer;
var
  r: Integer;
begin
  Assert(n >= 0);
  r := 0; // becomes 1 if any 1 bits are shifted off
  while n >= MIN_MERGE do
  begin
    r := r or (n and 1);
    n := n shr 1;
  end;
  Result := n + r;
end;

procedure TTimSort.PushRun(runBase: Pointer; runLen: Integer);
begin
  with fRuns[fStackLen] do
  begin
    base := runBase;
    len := runLen;
  end;
  Inc(fStackLen);
end;

procedure TTimSort.MergeCollapse;
var
  n: Integer;
  p: PSlice;
begin
  p := @fRuns[0];

  while fStackLen > 1 do
  begin
    n := fStackLen - 2;
    if (n > 0) and (p[n-1].len <= p[n].len + p[n+1].len) then
    begin
      if p[n-1].len < p[n+1].len then
        Dec(n);
      MergeAt(n);
    end
    else if p[n].len <= p[n+1].len then
      MergeAt(n)
    else
      Break; // invariant is established
  end;
end;

procedure TTimSort.MergeForceCollapse;
var
  n: Integer;
  p: PSlice;
begin
  p := @fRuns[0];

  while fStackLen > 1 do
  begin
    n := fStackLen - 2;
    if (n > 0) and (p[n-1].len < p[n+1].len) then
      Dec(n);
    MergeAt(n);
  end;
end;

procedure TTimSort.MergeAt(i: Integer);
var
  left, right: Pointer;
  leftLen, rightLen, k: Integer;
begin
  Assert(fStackLen >= 2);
  Assert(i >= 0);
  Assert((i = fStackLen - 2) or (i = fStackLen - 3));

  with fRuns[i] do
  begin
    left := base;
    leftLen := len;
  end;
  with fRuns[i+1] do
  begin
    right := base;
    rightLen := len;
  end;
  Assert(leftLen > 0);
  Assert(rightLen > 0);
  Assert(at(left, leftLen) = right);

  (* Record the length of the combined runs; if i is the 3rd-last run now, also slide over the
     last run (which isn't involved in this merge).  The current run (i+1) goes away in any case. *)
  fRuns[i].len := leftLen + rightLen;
  if i = fStackLen - 3 then
    fRuns[i+1] := fRuns[i+2];
  Dec(fStackLen);

  (* Find where the first element of run2 goes in run1. Prior elements in run1 can be ignored
     because they're already in place. *)
  k := GallopRight(right, left, leftLen, 0);
  Assert(k >= 0);
  left := at(left, k);
  Dec(leftLen, k);
  if leftLen = 0 then
    Exit;

  (* Find where the last element of left goes in right. Subsequent elements in right can be
     ignored because they're already in place. *)
  rightLen := GallopLeft(at(left, leftLen - 1), right, rightLen, rightLen - 1);
  Assert(rightLen >= 0);
  if rightLen = 0 then
    Exit;

  // merge remaining runs, using tmp array with min(leftLen, rightLen) elements
  if leftLen <= rightLen then
    fMergeLo(left, leftLen, right, rightLen, @Self)
  else
    fMergeHi(left, leftLen, right, rightLen, @Self);
end;

function TTimSort.GallopLeft(key: Pointer; items: Pointer; len, hint: Integer): Integer;
var
  lastOfs, ofs, maxOfs, k, m: Integer;
begin
  Assert(len > 0);
  Assert(hint >= 0);
  Assert(hint < len);

  items := at(items, hint);
  lastOfs := 0;
  ofs := 1;
  if fCompareFunc(items^, key^) < 0 then
  begin
    (* items[hint] < key -- gallop right, until
       items[hint + lastOfs] < key <= items[hint + ofs] *)
    maxOfs := len - hint;
    while (ofs < maxOfs) and (fCompareFunc(at(items, ofs)^, key^) < 0) do
    begin
      lastOfs := ofs;
      Assert(ofs <= (MaxInt - 1) div 2);
      ofs := (ofs shl 1) + 1;
    end;
    if ofs > maxOfs then
      ofs := maxOfs;

    // Translate back to offsets relative to items[0].
    Inc(lastOfs, hint);
    Inc(ofs, hint);
  end
  else
  begin
    (* key <= items[hint] -- gallop left, until
       items[hint - ofs] < key <= items[hint - lastOfs] *)
    maxOfs := hint + 1;
    while (ofs < maxOfs) and (fCompareFunc(at(items, -ofs)^, key^) >= 0) do
    begin
      lastOfs := ofs;
      Assert(ofs <= (MaxInt - 1) div 2);
      ofs := (ofs shl 1) + 1;
    end;
    if ofs > maxOfs then
      ofs := maxOfs;

    // Translate back to positive offsets relative to items[0].
    k := lastOfs;
    lastOfs := hint - ofs;
    ofs := hint - k;
  end;
  items := at(items, -hint);

  Assert(-1 <= lastOfs);
  Assert(lastOfs < ofs);
  Assert(ofs <= len);

  (* Now items[lastOfs] < key <= items[ofs], so key belongs somewhere to the
     right of lastOfs but no farther right than ofs.  Do a binary
     search, with invariant items[lastOfs-1] < key <= item[ofs]. *)
  Inc(lastOfs);
  while lastOfs < ofs do
  begin
    m := lastOfs + ((ofs - lastOfs) shr 1);
    if fCompareFunc(at(items, m)^, key^) < 0 then
      lastOfs := m + 1    // items[m] < key
    else
      ofs := m;           // key <= items[m]
  end;
  Assert(lastOfs = ofs);  // so items[ofs-1] < key <= items[ofs]
  Result := ofs;
end;

function TTimSort.GallopRight(key: Pointer; items: Pointer; len, hint: Integer): Integer;
var
  lastOfs, ofs, maxOfs, k, m: Integer;
begin
  Assert(len > 0);
  Assert(hint >= 0);
  Assert(hint < len);

  items := at(items, hint);
  lastOfs := 0;
  ofs := 1;
  if fCompareFunc(key^, items^) < 0 then
  begin
    (* key < items[hint] -- gallop left, until
       items[hint - ofs] <= key < items[hint - lastOfs] *)
    maxOfs := hint + 1;
    while (ofs < maxOfs) and (fCompareFunc(key^, at(items, -ofs)^) < 0) do
    begin
      lastOfs := ofs;
      Assert(ofs <= (MaxInt - 1) div 2);
      ofs := (ofs shl 1) + 1;
    end;
    if ofs > maxOfs then
      ofs := maxOfs;

    // Translate back to positive offsets relative to items[0].
    k := lastOfs;
    lastOfs := hint - ofs;
    ofs := hint - k;
  end
  else
  begin
    (* items[hint] <= key -- gallop right, until
       items[hint + lastOfs] <= key < items[hint + ofs] *)
    maxOfs := len - hint;
    while (ofs < maxOfs) and (fCompareFunc(key^, at(items, ofs)^) >= 0) do
    begin
      lastOfs := ofs;
      Assert(ofs <= (MaxInt - 1) div 2);
      ofs := (ofs shl 1) + 1;
    end;
    if ofs > maxOfs then
      ofs := maxOfs;

    // Translate back to offsets relative to items[0].
    Inc(lastOfs, hint);
    Inc(ofs, hint);
  end;
  items := at(items, -hint);

  Assert(-1 <= lastOfs);
  Assert(lastOfs < ofs);
  Assert(ofs <= len);

  (* Now items[lastOfs] <= key < items[ofs], so key belongs somewhere to the
     right of lastOfs but no farther right than ofs.  Do a binary
     search, with invariant items[lastOfs-1] <= key < items[ofs]. *)
  Inc(lastOfs);
  while lastOfs < ofs do
  begin
    m := lastOfs + ((ofs - lastOfs) shr 1);
    if fCompareFunc(key^, at(items, m)^) < 0 then
      ofs := m            // key < items[m]
    else
      lastOfs := m + 1;   // items[m] <= key
  end;
  Assert(lastOfs = ofs);  // so items[ofs-1] <= key < items[ofs]
  Result := ofs;
end;

function TTimSort.EnsureTmpCapacity(neededCapacity: Integer): Pointer;
begin
  if DynArrayLength(fTmp) < neededCapacity then
    Grow(neededCapacity);
  Result := fTmp;
end;

procedure TTimSort.Grow(neededCapacity: NativeInt);
begin
  neededCapacity := NextPowerOf2(neededCapacity);
  DynArraySetLength(fTmp, fArrayTypeInfo, 1, @neededCapacity);
end;

class procedure TTimSort.MergeLo<T>(left: Pointer<T>.P; leftLen: Integer; right: Pointer<T>.P; rightLen: Integer; ts: PTimSort);
label
  copyLeft, copyRight, gallopLeft, gallopRight;
var
  dest: Pointer<T>.P;
  leftCount, rightCount: Integer;
{$IF Defined(DELPHIX_TOKYO_UP) Defined(WIN64) and Defined(OPTIMIZATION_ON)}
  // Win64 with O+ has a codegen glitch as it later moves value from an uninitialized register
  temp_ts: PTimSort;
{$IFEND}
begin
  Assert(leftLen > 0);
  Assert(rightLen > 0);
  Assert(left + leftLen = right);
{$IF Declared(temp_ts)}
  temp_ts := ts;
{$IFEND}

  // copy first run into temp array
  dest := left;
  left := ts.EnsureTmpCapacity(leftLen);
  if TType.IsManaged<T> then
    MoveManaged(dest, left, TypeInfo(T), leftLen)
  else
    Move(dest^, left^, SizeOf(T) * leftLen);

  // move first element of second run and deal with degenerate cases
  dest^ := right^;
  Inc(dest);
  Inc(right);
  Dec(rightLen);
  if rightLen = 0 then
    goto copyLeft;
  if leftLen = 1 then
    goto copyRight;

  while True do
  begin
    leftCount := 0; // number of times in a row that first run won
    rightCount := 0; // number of times in a row that second run won

    // do the straightforward thing until (if ever) one run starts winning consistently
    while True do
    begin
      Assert(leftLen > 1);
      Assert(rightLen > 0);
      if TComparerMethod<T>(ts.fCompare)(right^, left^) < 0 then
      begin
        dest^ := right^;
        Inc(dest);
        Inc(right);
        Inc(rightCount);
        leftCount := 0;
        Dec(rightLen);
        if rightLen = 0 then
          goto copyLeft;
        if rightCount >= ts.fMinGallop then
          goto gallopLeft;
      end
      else
      begin
        dest^ := left^;
        Inc(dest);
        Inc(left);
        Inc(leftCount);
        rightCount := 0;
        Dec(leftLen);
        if leftLen = 1 then
          goto copyRight;
        if leftCount >= ts.fMinGallop then
          goto gallopRight;
      end;
    end;

    (* One run is winning so consistently that galloping may be a huge win. So
       try that, and continue galloping until (if ever) neither run appears to
       be winning consistently anymore. *)
    Inc(ts.fMinGallop);
    repeat
      Assert(leftLen > 1);
      Assert(rightLen > 0);
      Dec(ts.fMinGallop, Integer(ts.fMinGallop > 1));

    gallopRight:
      leftCount := ts.GallopRight(right, left, leftLen, 0);
      if leftCount <> 0 then
      begin
        if TType.IsManaged<T> then
          MoveManaged(left, dest, TypeInfo(T), leftCount)
        else
          Move(left^, dest^, SizeOf(T) * leftCount);
        Inc(dest, leftCount);
        Inc(left, leftCount);
        Dec(leftLen, leftCount);
        if leftLen = 1 then
          goto copyRight;
        if leftLen = 0 then
          goto copyLeft;
      end;
      dest^ := right^;
      Inc(dest);
      Inc(right);
      Dec(rightLen);
      if rightLen = 0 then
        goto copyLeft;

    gallopLeft:
{$IF Declared(temp_ts)}
      ts := temp_ts;
{$IFEND}
      rightCount := ts.GallopLeft(left, right, rightLen, 0);
      if rightCount <> 0 then
      begin
        if TType.IsManaged<T> then
          MoveManaged(right, dest, TypeInfo(T), rightCount)
        else
          Move(right^, dest^, SizeOf(T) * rightCount);
        Inc(dest, rightCount);
        Inc(right, rightCount);
        Dec(rightLen, rightCount);
        if rightLen = 0 then
          goto copyLeft;
      end;
      dest^ := left^;
      Inc(dest);
      Inc(left);
      Dec(leftLen);
      if leftLen = 1 then
        goto copyRight;
    until (leftCount < MIN_GALLOP) and (rightCount < MIN_GALLOP);
    Inc(ts.fMinGallop); // penalize it for leaving galloping mode
  end;

copyLeft:
  if leftLen > 0 then
    if TType.IsManaged<T> then
      MoveManaged(left, dest, TypeInfo(T), leftLen)
    else
      Move(left^, dest^, SizeOf(T) * leftLen);
  Exit;
copyRight:
  Assert(leftLen = 1); //FI:W509
  Assert(rightLen > 0);
  if TType.IsManaged<T> then
    MoveManaged(right, dest, TypeInfo(T), rightLen)
  else
    Move(right^, dest^, SizeOf(T) * rightLen);
  dest[rightLen] := left^;
end;

class procedure TTimSort.MergeHi<T>(left: Pointer<T>.P; leftLen: Integer; right: Pointer<T>.P; rightLen: Integer; ts: PTimSort);
label
  copyLeft, copyRight, gallopLeft, gallopRight;
var
  dest, leftBase, rightBase: Pointer<T>.P;
  leftCount, rightCount: Integer;
{$IF Defined(DELPHIX_TOKYO_UP) Defined(WIN64) and Defined(OPTIMIZATION_ON)}
  // Win64 with O+ has a codegen glitch as it later moves value from an uninitialized register
  temp_ts: PTimSort;
{$IFEND}
begin
  Assert(leftLen > 0);
  Assert(rightLen > 0);
  Assert(left + leftLen = right);
{$IF Declared(temp_ts)}
  temp_ts := ts;
{$IFEND}

  // copy second run into temp array
  rightBase := ts.EnsureTmpCapacity(rightLen);
  dest := right + rightLen - 1;
  if TType.IsManaged<T> then
    MoveManaged(right, rightBase, TypeInfo(T), rightLen)
  else
    Move(right^, rightBase^, SizeOf(T) * rightLen);
  leftBase := left;
  right := rightBase + rightLen - 1;
  Inc(left, leftLen - 1);

  // move last element of first run and deal with degenerate cases
  dest^ := left^;
  Dec(dest);
  Dec(left);
  Dec(leftLen);
  if leftLen = 0 then
    goto copyRight;
  if rightLen = 1 then
    goto copyLeft;

  while True do
  begin
    leftCount := 0; // number of times in a row that first run won
    rightCount := 0; // number of times in a row that second run won

    // do the straightforward thing until (if ever) one run appears to win consistently
    while True do
    begin
      Assert(leftLen > 0);
      Assert(rightLen > 1);
      if TComparerMethod<T>(ts.fCompare)(right^, left^) < 0 then
      begin
        dest^ := left^;
        Dec(dest);
        Dec(left);
        Inc(leftCount);
        rightCount := 0;
        Dec(leftLen);
        if leftLen = 0 then
          goto copyRight;
        if leftCount >= ts.fMinGallop then
          goto gallopRight;
      end
      else
      begin
        dest^ := right^;
        Dec(dest);
        Dec(right);
        Inc(rightCount);
        leftCount := 0;
        Dec(rightLen);
        if rightLen = 1 then
          goto copyLeft;
        if rightCount >= ts.fMinGallop then
          goto gallopLeft;
      end;
    end;

    (* One run is winning so consistently that galloping may be a huge win.
       So try that, and continue galloping until (if ever) neither run appears
       to be winning consistently anymore. *)
    Inc(ts.fMinGallop);
    repeat
      Assert(leftLen > 0);
      Assert(rightLen > 1);
      Dec(ts.fMinGallop, Integer(ts.fMinGallop > 1));

    gallopRight:
      leftCount := leftLen - ts.GallopRight(right, leftBase, leftLen, leftLen - 1);
      if leftCount <> 0 then
      begin
        Dec(dest, leftCount);
        Dec(left, leftCount);
        if TType.IsManaged<T> then
          MoveManaged(@left[1], @dest[1], TypeInfo(T), leftCount)
        else
          Move(left[1], dest[1], SizeOf(T) * leftCount);
        Dec(leftLen, leftCount);
        if leftLen = 0 then
          goto copyRight;
      end;
      dest^ := right^;
      Dec(dest);
      Dec(right);
      Dec(rightLen);
      if rightLen = 1 then
        goto copyLeft;

    gallopLeft:
{$IF Declared(temp_ts)}
      ts := temp_ts;
{$IFEND}
      rightCount := rightLen - ts.GallopLeft(left, rightBase, rightLen, rightLen - 1);
      if rightCount <> 0 then
      begin
        Dec(dest, rightCount);
        Dec(right, rightCount);
        if TType.IsManaged<T> then
          MoveManaged(@right[1], @dest[1], TypeInfo(T), rightCount)
        else
          Move(right[1], dest[1], SizeOf(T) * rightCount);
        Dec(rightLen, rightCount);
        if rightLen = 1 then
          goto copyLeft;
        if rightLen = 0 then
          goto copyRight;
      end;
      dest^ := left^;
      Dec(dest);
      Dec(left);
      Dec(leftLen);
      if leftLen = 0 then
        goto copyRight;
    until (leftCount < MIN_GALLOP) and (rightCount < MIN_GALLOP);
    Inc(ts.fMinGallop); // penalize for leaving gallop mode
  end;

copyRight:
  if rightLen > 0 then
    if TType.IsManaged<T> then
      MoveManaged(rightBase, @dest[-(rightLen - 1)], TypeInfo(T), rightLen)
    else
      Move(rightBase^, dest[-(rightLen - 1)], SizeOf(T) * rightLen);
  Exit;
copyLeft:
  Assert(rightLen = 1); //FI:W509
  Assert(leftLen > 0);
  Dec(dest, leftLen);
  Dec(left, leftLen);
  if TType.IsManaged<T> then
    MoveManaged(@left[1], @dest[1], TypeInfo(T), leftLen)
  else
    Move(left[1], dest[1], SizeOf(T) * leftLen);
  dest^ := right^;
end;

class procedure TTimSort.Sort(items: Pointer; const comparer: IComparer<Pointer>; index, count: Integer);
begin
  TTimSort.Sort<Pointer>(items, comparer, index, count);
end;

class procedure TTimSort.Sort<T>(items: Pointer; const comparer: IComparer<T>; index, count: Integer);
var
  runLen, minRun: Integer;
  force: Integer;
  ts: TTimSort;
begin
  Assert(index >= 0);
  Assert(count >= 0);

  if count < 2 then
    Exit; // arrays of length 0 and 1 are always sorted

  Inc(PByte(items), index * SizeOf(T));

  // if array is small, do a "mini-TimSort" with no merges
  if count < MIN_MERGE then
  begin
    {$R-}
    runLen := CountRunAndMakeAscending<T>(Slice(TSlice<T>(items^), count), comparer);
    if GetTypeKind(T) in TArray.ManagedPointerTypeKinds then
      BinaryInsertionSort<Pointer>(Slice(TSlice<Pointer>(items^), count), runLen, IComparer<Pointer>(comparer))
    else
      BinaryInsertionSort<T>(Slice(TSlice<T>(items^), count), runLen, comparer);
    {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
    Exit;
  end;

  ts.Initialize(comparer, @CompareThunk<T>, @TTimSort.MergeLo<T>, @TTimSort.MergeHi<T>, TypeInfo(TArray<T>), Integer(SizeOf(T)));
  try
    (* March over the array once, left to right, finding natural runs, extending short
       natural runs to minRun elements, and merging runs to maintain stack invariant. *)
    minRun := MinRunLength(count);
    repeat
      // identify next run
      {$R-}
      runLen := CountRunAndMakeAscending<T>(Slice(TSlice<T>(items^), count), comparer);

      // if run is short, extend to min(minRun, count)
      if runLen < minRun then
      begin
        if count <= minRun then
          force := count
        else
          force := minRun;
        if GetTypeKind(T) in TArray.ManagedPointerTypeKinds then
          BinaryInsertionSort<Pointer>(Slice(TSlice<Pointer>(items^), force), runLen, IComparer<Pointer>(comparer))
        else
          BinaryInsertionSort<T>(Slice(TSlice<T>(items^), force), runLen, comparer);
        runLen := force;
      end;
      {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}

      // push run onto pending-run stack, and maybe merge
      ts.PushRun(items, runLen);
      ts.MergeCollapse;

      // advance to find next run
      Inc(PByte(items), runLen * SizeOf(T));
      Dec(count, runLen);
    until count = 0;

    // Merge all remaining runs to complete sort
    ts.MergeForceCollapse;
    Assert(ts.fStackLen = 1);
  finally
    ts.Finalize;
  end;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTypeInfo<T>'}

{$IFNDEF DELPHIXE7_UP}
class constructor TTypeInfo<T>.Create;
begin
{$IFDEF WEAKREF}
  fHasWeakRef := TType.HasWeakRef<T>;
{$ENDIF}
  fIsManaged := TType.IsManaged<T>;
end;
{$ELSE}
{$IFDEF WEAKREF}
class function TTypeInfo<T>.GetHasWeakRef: Boolean;
begin
  Result := System.HasWeakRef(T);
end;
{$ENDIF}

class function TTypeInfo<T>.GetIsManaged: Boolean;
begin
  Result := System.IsManagedType(T);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'Vector<T>'}

class function VectorHelper.InternalIndexOfInt8(const data: Pointer;
  const item: ShortInt): NativeInt;
begin
  for Result := 0 to High(TArray<ShortInt>(data)) do
    if TArray<ShortInt>(data)[Result] = item then
      Exit;
  Result := -1;
end;

class function VectorHelper.InternalIndexOfInt16(const data: Pointer;
  const item: SmallInt): NativeInt;
begin
  for Result := 0 to High(TArray<SmallInt>(data)) do
    if TArray<SmallInt>(data)[Result] = item then
      Exit;
  Result := -1;
end;

class function VectorHelper.InternalIndexOfInt32(const data:Pointer;
  const item: Integer): NativeInt;
begin
  for Result := 0 to High(TArray<Integer>(data)) do
    if TArray<Integer>(data)[Result] = item then
      Exit;
  Result := -1;
end;

class function VectorHelper.InternalIndexOfInt64(const data: Pointer;
  const item: Int64): NativeInt;
begin
  for Result := 0 to High(TArray<Int64>(data)) do
    if TArray<Int64>(data)[Result] = item then
      Exit;
  Result := -1;
end;

class function VectorHelper.InternalIndexOfStr(const data: Pointer;
  const item: string): NativeInt;
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

function Vector<T>.Add(const item: T): NativeInt;
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
  SetLength(fData, System.Length(items));
  if TType.IsManaged<T> then
    MoveManaged(@items[0], fData, TypeInfo(T), System.Length(items))
  else
    Move(items[0], fData[0], SizeOf(T) * System.Length(items));
end;

procedure Vector<T>.Clear;
begin
  fData := nil;
end;

function Vector<T>.IndexOf(const item: T): NativeInt;
begin
  case TType.Kind<T> of
    tkInteger, tkChar, tkEnumeration, tkWChar:
      case SizeOf(T) of
        1: Result := VectorHelper.InternalIndexOfInt8(fData, PShortInt(@item)^);
        2: Result := VectorHelper.InternalIndexOfInt16(fData, PSmallInt(@item)^);
        4: Result := VectorHelper.InternalIndexOfInt32(fData, PInteger(@item)^);
      else
        __SuppressWarning(Result);
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
  i: NativeInt;
begin
  for i := 0 to High(fData) do
    if comparer.Equals(fData[i], item) then
      Exit(True);
  Result := False;
end;

function Vector<T>.Contains(const item: T;
  const comparer: TEqualityComparison<T>): Boolean;
var
  i: NativeInt;
begin
  for i := 0 to High(fData) do
    if comparer(fData[i], item) then
      Exit(True);
  Result := False;
end;

function Vector<T>.Contains(const items: array of T): Boolean;
var
  i: NativeInt;
begin
  for i := 0 to High(items) do
    if IndexOf(items[i]) = -1 then
      Exit(False);
  Result := True;
end;

function Vector<T>.Contains(const items: TArray<T>): Boolean;
var
  i: NativeInt;
begin
  for i := 0 to DynArrayHigh(items) do
    if IndexOf(items[i]) = -1 then
      Exit(False);
  Result := True;
end;

procedure Vector<T>.Delete(index: NativeInt);
{$IFNDEF DELPHIXE7_UP}
var
  n, i: NativeInt;
{$ENDIF}
begin
{$IFNDEF DELPHIXE7_UP}
  n := DynArrayLength(fData);
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

procedure Vector<T>.Delete(index, count: NativeInt);
{$IFNDEF DELPHIXE7_UP}
var
  n, i: NativeInt;
{$ENDIF}
begin
{$IFNDEF DELPHIXE7_UP}
  n := DynArrayLength(fData);
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
  n, i: NativeInt;
begin
  n := DynArrayLength(fData);
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
  n, i: NativeInt;
begin
  n := DynArrayLength(fData);
  if n <> DynArrayLength(items) then
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

procedure Vector<T>.ForEach(const action: Action<T>);
var
  i: NativeInt;
begin
  for i := Low(fData) to High(fData) do
    action(fData[i]);
end;

function Vector<T>.GetCount: NativeInt;
begin
  Result := DynArrayLength(fData);
end;

function Vector<T>.GetEnumerator: TArrayEnumerator<T>;
begin
  Result.fItems := fData;
  Result.fIndex := -1;
end;

function Vector<T>.GetFirst: T;
begin
  Result := fData[0];
end;

function Vector<T>.GetItem(index: NativeInt): T;
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

class operator Vector<T>.In(const left: T; const right: Vector<T>): Boolean;
begin
  Result := right.Contains(left);
end;

class operator Vector<T>.In(const left, right: Vector<T>): Boolean;
begin
  Result := right.Contains(left.fData);
end;

class operator Vector<T>.In(const left: TArray<T>; const right: Vector<T>): Boolean;
begin
  Result := right.Contains(left);
end;

procedure Vector<T>.Insert(index: NativeInt; const item: T);
{$IFNDEF DELPHIXE7_UP}
var
  count, i: NativeInt;
{$ENDIF}
begin
{$IFNDEF DELPHIXE7_UP}
  count := DynArrayLength(fData);
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

procedure Vector<T>.Insert(index: NativeInt; const items: array of T);
begin
  InternalInsert(index, items);
end;

procedure Vector<T>.Insert(index: NativeInt; const items: TArray<T>);
begin
{$IFNDEF DELPHIXE7_UP}
  InternalInsert(index, items);
{$ELSE}
  System.Insert(items, fData, index);
{$ENDIF}
end;

function Vector<T>.InternalEquals(const items: array of T): Boolean;
var
  comparer: Pointer;
  i: NativeInt;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  for i := 0 to DynArrayHigh(fData) do
    if not IEqualityComparer<T>(comparer).Equals(fData[i], items[i]) then
      Exit(False);
  Result := True;
end;

function Vector<T>.InternalIndexOf(const item: T): NativeInt;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  for Result := 0 to DynArrayHigh(fData) do
    if IEqualityComparer<T>(comparer).Equals(fData[Result], item) then
      Exit;
  Result := -1;
end;

procedure Vector<T>.InternalInsert(index: NativeInt; const items: array of T);
var
  count, len, i: NativeInt;
begin
  count := DynArrayLength(fData);
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
  n: NativeInt;
begin
  n := DynArrayHigh(fData);
  Result := fData[n];
  SetLength(fData, n);
end;

procedure Vector<T>.Remove(const item: T);
var
  index: NativeInt;
begin
  index := IndexOf(item);
  if index > -1 then
    Delete(index);
end;

procedure Vector<T>.Remove(const items: array of T);
var
  i, index: NativeInt;
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
  i, index: NativeInt;
begin
  for i := 0 to DynArrayHigh(items) do
  begin
    index := IndexOf(items[i]);
    if index > -1 then
      Delete(index);
  end;
end;

procedure Vector<T>.Reverse;
var
  tmp: T;
  b, e: NativeInt;
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

procedure Vector<T>.SetCount(value: NativeInt);
begin
  SetLength(fData, value);
end;

procedure Vector<T>.SetItem(index: NativeInt; const value: T);
begin
  fData[index] := value;
end;

function Vector<T>.Slice(index: NativeInt): Vector<T>;
begin
  Result.fData := Copy(fData, index);
end;

function Vector<T>.Slice(index, count: NativeInt): Vector<T>;
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

function Vector<T>.Splice(index, count: NativeInt): Vector<T>;
begin
  Result := Splice(index, count, []);
end;

function Vector<T>.Splice(index, count: NativeInt;
  const items: array of T): Vector<T>;
var
  i: NativeInt;
begin
  i := DynArrayLength(fData);
  if (index < 0) or (index >= i) then
    Exit;
  if count > i - index then
    count := i - index;
  Result.fData := Copy(fData, index, count);
  Delete(index, count);
  Insert(index, items);
end;

class operator Vector<T>.Subtract(const left, right: Vector<T>): Vector<T>;
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


{$REGION 'TArrayEnumerator<T>'}

constructor TArrayEnumerator<T>.Create(const items: TArray<T>);
begin
  fItems := items;
  fIndex := -1;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := TArray<T>(fItems)[fIndex];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Inc(fIndex);
  {$POINTERMATH ON}
  Result := Assigned(fItems) and (fIndex < PNativeInt(fItems)[-1]);
  {$POINTERMATH OFF}
end;

{$ENDREGION}


procedure Init;
begin
  Nop_Instance := Pointer(TValueData(TValue.Empty).FValueData);

  TValue.UpdateFormatSettings;
end;


initialization
  Init;
  WeakRefInstances.Initialize;

finalization
  // make sure this properly gets freed because it appears
  // the class destructor is not running all the time
  TType.fContext.Free;
  WeakRefInstances.Finalize;

end.
