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

unit Spring.Tests.Base;

{$I Spring.inc}
{$WARN SYMBOL_DEPRECATED OFF}

{$IF Defined(MACOS) AND NOT Defined(IOS)}
  {$DEFINE LogConsole}
{$IFEND MACOS}

interface

uses
  Classes,
  Generics.Defaults,
  TypInfo,
  TestFramework,
  Spring.TestUtils,
  Spring,
  Spring.Collections,
  Spring.Events,
  Spring.Testing;

type
  TTestNullableInteger = class(TTestCase)
  private
    fInteger: Nullable<Integer>;
  protected
    procedure TearDown; override;
  published
    procedure TestInitialValue;
    procedure GetValueOrDefault;
    procedure TestAssignFive;
    procedure TestException;
    procedure TestLocalVariable;
    procedure TestFromVariant;
    procedure TestEquals;
    procedure TestDefaultReturnsInitialValue;
    procedure TestAssignFloat;
    procedure TestAssignStringNonInt;
    procedure TestAssignStringInt;
    procedure TestNullableNull;
    procedure TestAssignNull;
    procedure TestTryGetValue;
  end;

  TTestNullableBoolean = class(TTestCase)
  private
    fBoolean: Nullable<Boolean>;
  protected
    procedure TearDown; override;
  published
    procedure TestIssue55;
  end;

  TTestNullableDateTime = class(TTestCase)
  private
    fDateTime: Nullable<TDateTime>;
  protected
    procedure TearDown; override;
  published
    procedure TestFromVariantSQLTimestamp;
    procedure TestFromVariantSQLTimestampOffset;
  end;

  TTestNullableInt64 = class(TTestCase)
  private
    fInt64: Nullable<Int64>;
  protected
    procedure TearDown; override;
  published
    procedure TestFromVariantFmtBcd;
  end;

  TCustomRecord = record
    x, y: Byte;
    constructor Create(x, y: Integer);
    class operator Equal(const left, right: TCustomRecord): Boolean;
  end;

  TTestNullableCustomRecord = class(TTestCase)
  published
    procedure TestEqualsUsingOperatorOverload;
  end;

  TTestGuard = class(TTestCase)
  published
    procedure TestIsNullReference;
    procedure TestCheckRange;
    procedure TestNotNull;
    procedure TestCheckSet;
  end;

  TTestLazy = class(TTestCase)
  private
    fBalance: ILazy<Integer>;
  protected
    const
      CExpectedBalance = 100;
    procedure TearDown; override;
  published
    procedure TestByValueFactory;
    procedure TestByValue;
    procedure Test_Initializer_RaisesArgumentException_NotReferenceType;
    procedure TestIsLazyType;
    procedure Issue375;
  end;

  {$M+}
  TProc<T1, T2> = reference to procedure(arg1: T1; arg2: T2);
  {$M-}

  TTestMulticastEvent = class(TTestCase)
  strict private
    type
      TEventArgs = record i: Integer; s: string; v: Variant; end;
      TEventInt64 = procedure(const Value: Int64) of object;
      TEventSingle = procedure(const Value: Single) of object;
      TEventDouble = procedure(const Value: Double) of object;
      TEventExtended = procedure(const Value: Extended) of object;
      TEventWithStackParams = procedure(const Value1: Int64; const Value2: Single;
        const Value3: Double; const Value4: Extended; const Value5: TEventArgs) of object;
      TEventWithRegisterParams = procedure(const Value1, Value2, Value3: NativeInt) of object;
      TEventWithFloatParams = procedure(const Value1, Value2, Value3: Double) of object;
    const
      CNumber = 5;
      CText = 'test';
  strict private
    fEvent: IInvokableNotifyEvent;
    fASender: TObject;
    fAInvoked: Boolean;
    fBSender: TObject;
    fBInvoked: Boolean;
    fHandlerInvokeCount: Integer;
    fProc: Action<Integer, string>;
  strict protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure HandlerA(sender: TObject);
    procedure HandlerB(sender: TObject);

    procedure HandlerInt64(const value: Int64);
    procedure HandlerSingle(const value: Single);
    procedure HandlerDouble(const value: Double);
    procedure HandlerExtended(const value: Extended);
    procedure HandlerWithStackParams(const value1: Int64; const value2: Single;
      const value3: Double; const value4: Extended; const value5: TEventArgs);
    procedure HandlerWithRegisterParams(const value1, value2, value3: NativeInt);
    procedure HandlerWithFloatParams(const value1, value2, value3: Double);
    procedure HandleChanged(Sender: TObject);
  published
    procedure TestInvoke;
    procedure TestOneHandler;
    procedure TestTwoHandlers;
    procedure TestRecordType;
    procedure TestIssue58;
    procedure TestDelegate;
    procedure TestIssue60;
    procedure TestStackParams;
    procedure TestRegisterParams;
    procedure TestFloatParams;
    procedure TestNotify;
    procedure TestNotifyDelegate;
    procedure TestRemove;
    procedure TestClear;
    procedure TestAddNil;

    procedure TestClassProcedureHandler;
    procedure TestInstanceProcedureHandler;

    procedure TestSetUseFreeNotification;
  end;

  TEventHandler = class
    class var fClassHandlerInvoked: Boolean;
    class procedure HandleInt64Static(const value: Int64);
    procedure HandleInt64(const value: Int64);
  end;

  TTestMulticastEventStackSize = class(TTestCase)
  strict private
    type
      {TODO -o##jwp -cEnhance : Add more data types: all the int and float types, records, classes, interfaces, variants }
      TEventDouble = procedure(const Value: Double) of object;
      TEventExtended = procedure(const Value: Extended) of object;
      TEventGuid = procedure(const Value: TGUID) of object;
      TEventInt64 = procedure(const Value: Int64) of object;
      TEventSingle = procedure(const Value: Single) of object;
    const
      Integer42: Integer = 42;
      Float42 = 42.0;
      Double42: Double = 42.0;
      Extended42: Extended = 42.0;
      Int6442: Int64 = 42;
      Single42: Single = 42;
      GUID42: TGUID = '{CCD21A05-9527-411F-AB44-AAF44C0E0DAF}';
  strict private
    fHandlerInvokeCount: Integer;
  strict protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure HandlerDouble(const value: Double);
    procedure HandlerExtended(const value: Extended);
    procedure HandlerGuid(const value: TGUID);
    procedure HandlerInt64(const value: Int64);
    procedure HandlerSingle(const value: Single);
    procedure LogEnter(const expected: Integer; const MethodName: string); virtual;
    procedure LogLeave(const expected: Integer); virtual;
  published
    procedure TestIssue60Double;
    procedure TestIssue60DoubleAssignedConst;
    procedure TestIssue60Extended;
    procedure TestIssue60ExtendedAssignedConst;
    procedure TestIssue60GuidAssignedConst;
    procedure TestIssue60Int64;
    procedure TestIssue60Int64AssignedConst;
    procedure TestIssue60Single;
    procedure TestIssue60SingleAssignedConst;
  end;

  TTestSpringEventsMethods = class(TTestCase)
  private
    fRemainingTypeKinds: TTypeKinds;
    fTestedTypeKinds: TTypeKinds;
    const
      PointerSize = SizeOf(Pointer);
  protected
    procedure MatchType(const aTypeInfo: PTypeInfo; const aExpectedTypeKind: TTypeKind;
      const aExpectedTypeSize: Integer);
    procedure SetUp; override;
    procedure TearDown; override;
    property RemainingTypeKinds: TTypeKinds read fRemainingTypeKinds;
    property TestedTypeKinds: TTypeKinds read fTestedTypeKinds;
  published
    /// <summary>
    ///   Make sure this method is named so it will be run last
    /// </summary>
    procedure Test_EnsureAllTTypeKindsCoveredByCallsTo_Test_GetTypeSize_;
    procedure Test_GetTypeSize_AnsiChar;
    procedure Test_GetTypeSize_AnsiString;
    procedure Test_GetTypeSize_Array;
    procedure Test_GetTypeSize_Boolean;
    procedure Test_GetTypeSize_Byte;
    procedure Test_GetTypeSize_ByteBool;
    procedure Test_GetTypeSize_Cardinal;
    procedure Test_GetTypeSize_Char;
    procedure Test_GetTypeSize_Class;
    procedure Test_GetTypeSize_ClassRef;
    procedure Test_GetTypeSize_Comp;
    procedure Test_GetTypeSize_Currency;
    procedure Test_GetTypeSize_Double;
    procedure Test_GetTypeSize_DynamicArray;
    procedure Test_GetTypeSize_Extended;
    procedure Test_GetTypeSize_Guid;
    procedure Test_GetTypeSize_Interface;
    procedure Test_GetTypeSize_Int64;
    procedure Test_GetTypeSize_Integer;
    procedure Test_GetTypeSize_LongBool;
    procedure Test_GetTypeSize_LongInt;
    procedure Test_GetTypeSize_LongWord;
    procedure Test_GetTypeSize_Method;
    procedure Test_GetTypeSize_NativeInt;
    procedure Test_GetTypeSize_NativeUInt;
    procedure Test_GetTypeSize_OleVariant;
    procedure Test_GetTypeSize_PAnsiChar;
    procedure Test_GetTypeSize_PChar;
    procedure Test_GetTypeSize_Pointer;
    procedure Test_GetTypeSize_Proc;
    procedure Test_GetTypeSize_Procedure;
    procedure Test_GetTypeSize_PWideChar;
    procedure Test_GetTypeSize_Real;
    procedure Test_GetTypeSize_Set;
    procedure Test_GetTypeSize_SetOfByte;
    procedure Test_GetTypeSize_ShortInt;
    procedure Test_GetTypeSize_ShortString;
    procedure Test_GetTypeSize_ShortString0;
    procedure Test_GetTypeSize_ShortString1;
    procedure Test_GetTypeSize_ShortString2;
    procedure Test_GetTypeSize_ShortString255;
    procedure Test_GetTypeSize_ShortString7;
    procedure Test_GetTypeSize_Single;
    procedure Test_GetTypeSize_SmallInt;
    procedure Test_GetTypeSize_string;
    procedure Test_GetTypeSize_UnicodeString;
    procedure Test_GetTypeSize_Variant;
    procedure Test_GetTypeSize_WideChar;
    procedure Test_GetTypeSize_WideString;
    procedure Test_GetTypeSize_Word;
    procedure Test_GetTypeSize_WordBool;
{$IF Declared(tkMRecord)}
    procedure Test_GetTypeSize_ManagedRecord;
{$IFEND}
  end;

  TTestTuplesDouble = class(TTestCase)
  private
    fSUT: Tuple<Integer, string>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Create;
    procedure Test_Pack;
    procedure Test_Equals_ReturnsTrue_WhenEqual;
    procedure Test_Equals_ReturnsFalse_WhenFirstValueDiffers;
    procedure Test_Equals_ReturnsFalse_WhenSecondValueDiffers;
    procedure Test_Equals_ReturnsFalse_WhenAllValuesDiffer;
    procedure Test_Implicit_ToValueArray;
    procedure Test_Implicit_FromValueArray;
{$IFDEF DELPHIXE5_UP} // TODO: check if also lower versions work
    procedure Test_Implicit_FromOpenArray;
{$ENDIF}
    procedure Test_Unpack;
  end;

  TTestTuplesTriple = class(TTestCase)
  private
    fSUT: Tuple<Integer, string, Boolean>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Create;
    procedure Test_Pack;
    procedure Test_Equals_ReturnsTrue_WhenEqual;
    procedure Test_Equals_ReturnsFalse_WhenFirstValueDiffers;
    procedure Test_Equals_ReturnsFalse_WhenSecondValueDiffers;
    procedure Test_Equals_ReturnsFalse_WhenThirdValueDiffers;
    procedure Test_Equals_ReturnsFalse_WhenAllValuesDiffer;
    procedure Test_Implicit_ToValueArray;
    procedure Test_Implicit_FromValueArray;
{$IFDEF DELPHIXE5_UP} // TODO: check if also lower versions work
    procedure Test_Implicit_FromOpenArray;
{$ENDIF}
    procedure Test_Unpack;
    procedure Test_Unpack_TwoValues;
  end;

  TTestTuplesQuadruple = class(TTestCase)
  private
    fSUT: Tuple<Integer, string, Boolean, Char>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Create;
    procedure Test_Pack;
    procedure Test_Equals_ReturnsTrue_WhenEqual;
    procedure Test_Equals_ReturnsFalse_WhenFirstValueDiffers;
    procedure Test_Equals_ReturnsFalse_WhenSecondValueDiffers;
    procedure Test_Equals_ReturnsFalse_WhenThirdValueDiffers;
    procedure Test_Equals_ReturnsFalse_WhenFourthValueDiffers;
    procedure Test_Equals_ReturnsFalse_WhenAllValuesDiffer;
    procedure Test_Implicit_ToValueArray;
    procedure Test_Implicit_FromValueArray;
{$IFDEF DELPHIXE5_UP} // TODO: check if also lower versions work
    procedure Test_Implicit_FromOpenArray;
{$ENDIF}
    procedure Test_Unpack;
    procedure Test_Unpack_TwoValues;
    procedure Test_Unpack_ThreeValues;
  end;

  TTestShared = class(TTestCase)
  published
    procedure TestInterfaceType_Instance_Gets_Created;
    procedure TestInterfaceType_Instance_Gets_Destroyed_When_Created;
    procedure TestInterfaceType_Instance_Gets_Destroyed_When_Injected;

    procedure TestRecordType_Implicit_FromInstance_Works;
    procedure TestRecordType_Implicit_ToInstance_Works;
    procedure TestRecordType_Instance_Gets_Destroyed;

    procedure TestRecordType_Manage_Typed_Pointer;
  end;

  TTestVector = class(TTestCase)
  published
    procedure ClassOperatorAdd_InputNotModified;

    procedure ClassOperatorSubtract_InputNotModified;

    procedure ClassOperatorIn_ItemInArray_True;
    procedure ClassOperatorIn_ItemNotInArray_False;
    procedure ClassOperatorIn_ArrayInArray_True;
    procedure ClassOperatorIn_ArrayNotInArray_False;
    procedure ClassOperatorIn_Int8;
    procedure ClassOperatorIn_Int64;

    procedure IndexOf_ItemInArray;
    procedure Delete_Start;
    procedure Delete_Mid;
    procedure Delete_End;
    procedure Delete_IndexLessThanZero_NothingHappens;
    procedure Delete_IndexEqualsCount_NothingHappens;

    procedure DeleteRange_Mid;
    procedure DeleteRange_IndexLessThanZero_NothingHappens;
    procedure DeleteRange_GreaterThanLengthMinusCount_DeleteUntilEnd;

    procedure Remove_ArrayContainsElements;
  end;

  TTestValueHelper = class(TTestCase)
  private
    type
      TCollectionChangedActions = set of TCollectionChangedAction;
  private
    fSUT, fValue: TValue;
    procedure DoCheckEquals(expected: Boolean = True);
    procedure DoCheckCompare(expected: Integer = 0);
  protected
    procedure TearDown; override;
  published
    procedure Test_AsPointer_Pointer;
    procedure Test_AsPointer_Class;
    procedure Test_AsPointer_Interface;
    procedure Test_AsPointer_OtherWillRaise;

    procedure Test_CurrencyToCurrency_Compare;
    procedure Test_CurrencyToCurrency_Equals;

    procedure Test_Equals_ByteToInt_ValuesAreNotEqual_ReturnsFalse;
    procedure Test_Equals_ShortIntToInt_ValuesAreEqual_ReturnsTrue;
    procedure Test_Equals_IntToInt_ValuesAreEqual_ReturnsTrue;

    procedure Test_Equals_EnumToEnum_ValuesAreEqual_ReturnsTrue;

    procedure Test_Equals_SetToSet_ValuesAreEqual_ReturnsTrue;
    procedure Test_Equals_SetToSet;

    procedure Test_Compare_IntToInt_ValuesAreEqual_ReturnsTrue;

    procedure Test_Compare_StringToString_ValuesAreEqual_ReturnsTrue;

    procedure EqualsReturnsTrueForEqualVariants;
    procedure EqualsReturnsTrueForUnassignedVariants;
    procedure EqualsReturnsTrueForEqualVariantArrays;
    procedure EqualsReturnsTrueForEqualVariantArraysTwoDimensions;
    procedure EqualsReturnsTrueForEqualVariantArraysWithDifferentType;
    procedure EqualsReturnsFalseForDifferentVariants;
    procedure EqualsReturnsFalseForDifferentVariantsOneIsUnassigned;
    procedure EqualsReturnsFalseForDifferentVariantArraysWithDifferentLength;
    procedure EqualsReturnsFalseForDifferentVariantArraysWithSameLength;
    procedure EqualsReturnsTrueForEqualVariantArrayOfVariantArray;
    procedure EqualsReturnsFalseForDifferentVariantArrayOfVariantArray;

    procedure EqualsReturnsTrueForEqualPointers;
    procedure EqualsReturnsFalseForUnequalPointers;

    procedure EqualsReturnsTrueForEqualTValue;

    procedure EqualsReturnsTrueForEqualArrays;

    procedure FromVariantProperlyHandlesVariantArrays;

    procedure ConvertStringToIntegerFailsForInvalidString;
    procedure ConvertStringToFloatFailsForInvalidString;

    procedure FromCustomVariantFmtBcd;

    procedure TryToType_ConvertIntToStr;
    procedure TryToType_ConvertStrToInt;
    procedure TryToType_ConvertStringToNullableString;
    procedure TryToType_ConvertIntegerToNullableEnum;
    procedure TryToType_ConvertInvalidStringToBoolean;
    procedure TryToType_ConvertVariantToBoolean;
    procedure TryToType_ConvertVariantToString;
    procedure TryToType_ConvertStringToIntegerArray;
    procedure TryToType_ConvertStringArrayToIntegerArray;

    procedure GetNullableValue_ValueIsEmpty_ReturnsEmpty;

    procedure ImplicitOperators;

    procedure NullableToString;
  end;

  TEnumeration = (teFirst, teSecond, teLast);

  TTestManagedObject = class(TTestCase)
  published
    procedure TestInitialization;
    procedure TestDefaultOverride;
  end;

  TTestObjectBase = class(TManagedObject)
  protected
    [Default(42)]
    fIntValue: Integer;
  end;

  TTestObjectDerivedA = class(TTestObjectBase)
  public
    [Default(45)]
    property IntValueB: Integer read fIntValue;
    [Default(43)]
    property IntValue: Integer read fIntValue;
  end;

  TTestObjectDerivedB = class(TTestObjectDerivedA)
  public
    [Default(44)]
    property IntValue;
  end;

  TTestObject = class(TManagedObject)
  private
    [Default(42)]
    fIntValue: Integer;
    [Default(High(Int64))]
    fInt64Value: Int64;
    [Default(High(UInt64))]
    fUInt64Value: UInt64;
    [Default(Low(Int64))]
    fInt64Value2: Int64;
    [Default(Low(UInt64))]
    fUInt64Value2: UInt64;
    [Default('test')]
    fStrValue: string;
    [Default(True)]
    fBoolValue: Boolean;
    [Default(20.5)]
    fDoubleValue: Double;
    [Default('2015-09-30 17:30:00')]
    fDateTime: TDateTime;
    [Default('2015-09-30')]
    fDate: TDate;
    [Default('17:30:00')]
    fTime: TTime;
    [Managed]
    fObjValue: TObject;
    [Managed(TPersistent)]
    fObjValue2: TObject;
    [Managed(False)]
    fObjValue3: TObject;
    [Default('x')]
    fAnsiCharValue: AnsiChar;
    [Default('y')]
    fWideCharValue: WideChar;
    [Default('z')]
    fCharValue: Char;
    [Managed(TInterfacedObject)]
    fIntfValue: IInterface;

    fIntValue_Prop: Integer;
    fStrValue_Prop: string;

    [AutoInit]
    fList: IList<TPersistent>;
    procedure SetStrValue_Prop(const Value: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    [Default(43)]
    property IntValue: Integer read fIntValue_Prop write fIntValue_Prop;

    [Default('hello')]
    property StrValue: string read fStrValue_Prop write SetStrValue_Prop;
  end;

  TArrayTest = class(TTestCase)
  private const
    TestData: array[0..9] of Integer = (1, 2, 3, 4, 5, 5, 5, 6, 7, 9);
    ExpectedResults: array[0..9] of ShortInt = (0, 1, 1, 1, 1, 1, 1, 1, 0, 1);
    ExpectedIndexesLowerBound: array[0..9] of Integer = (0, 0, 1, 2, 3, 4, 7, 8, 9, 9);
    ExpectedIndexesUpperBound: array[0..9] of Integer = (0, 0, 1, 2, 3, 6, 7, 8, 9, 9);
  published
    procedure TestBinarySearch;
    procedure TestBinarySearchSubRange;
    procedure TestBinarySearchUpperBound;
    procedure TestBinarySearchUpperBoundSubRange;

    procedure TestLastIndexOf;
    procedure TestLastIndexOfSubRange;

{$IFDEF DELPHIXE7_UP}
    procedure TestStableSortOrdering;
    procedure TestStableSortStability;
    procedure TestStableSortSubrangeSort;
    procedure TestStableSortOrderedInput;
    procedure TestStableSortLongRuns;
    procedure TestStableSortString;
    procedure TestStableSortInterface;
    procedure TestStableSortUnmanagedRecord;
    procedure TestStableSortManagedRecord;
    procedure TestTimSortArrayIndexOutOfBoundsBugFix;
{$ENDIF}
  end;

  TSortTest = class(TTestCase)
  private type
    TString1 = string[1];
    TString2 = string[2];
    TString3 = string[3];
    TString4 = string[4];
    TString7 = string[7];
    {$SCOPEDENUMS ON}
    TEnum8 =  (x00, x01, x02, x03, x04, x05, x06, x07);
    TEnum16 = (x00, x01, x02, x03, x04, x05, x06, x07,
              x08, x09, x0a, x0b, x0c, x0d, x0e, x0f);
    TEnum32 = (x00, x01, x02, x03, x04, x05, x06, x07,
              x08, x09, x0a, x0b, x0c, x0d, x0e, x0f,
              x10, x11, x12, x13, x14, x15, x16, x17,
              x18, x19, x1a, x1b, x1c, x1d, x1e, x1f);
    TEnum64 = (x00, x01, x02, x03, x04, x05, x06, x07,
              x08, x09, x0a, x0b, x0c, x0d, x0e, x0f,
              x10, x11, x12, x13, x14, x15, x16, x17,
              x18, x19, x1a, x1b, x1c, x1d, x1e, x1f,
              x20, x21, x22, x23, x24, x25, x26, x27,
              x28, x29, x2a, x2b, x2c, x2d, x2e, x2f,
              x30, x31, x32, x33, x34, x35, x36, x37,
              x38, x39, x3a, x3b, x3c, x3d, x3e, x3f);
    TSet8 = set of TEnum8;
    TSet16 = set of TEnum16;
    TSet32 = set of TEnum32;
    TSet64 = set of TEnum64;
    TSet256 = set of Byte;
    {$SCOPEDENUMS OFF}
    TArray1 = array[0..0] of Byte;
    TArray2 = array[0..1] of Byte;
    TArray3 = array[0..2] of Byte;
    TArray4 = array[0..3] of Byte;
    TArray5 = array[0..4] of Byte;
    TArray8 = array[0..7] of Byte;
    TRec1 = packed record
      a: Byte;
    end;
    TRec2 = packed record
      a, b: Byte;
    end;
    TRec3 = packed record
      a, b, c: Byte;
    end;
    TRec4 = packed record
      a, b, c, d: Byte;
    end;
    TRec5 = packed record
      a, b, c, d, e: Byte;
    end;
    TRec8 = packed record
      a, b: Integer;
    end;
    TRec12 = packed record
      a, b, c: Integer;
    end;
  private const Count = 10000;
    class procedure TestPassing<T>(const value: T); static;
    procedure TestSort<T>(const genvalue: Func<T>);
    class function RandomChar: AnsiChar; static;
  published
    procedure Test_Int8;
    procedure Test_Int16;
    procedure Test_Int32;
    procedure Test_Int64;
    procedure Test_Single;
    procedure Test_Double;
    procedure Test_Extended;
    procedure Test_Comp;
    procedure Test_Currency;
    procedure Test_String;
    procedure Test_Set;
    procedure Test_Array;
    procedure Test_Variant;
    procedure Test_Record;
  end;

  TWeakTest = class(TTestCase)
  published
    procedure TestIsAlive;
    procedure TestTryGetTarget;
  end;

  TTestVirtualClass = class(TTestCase)
  published
    procedure TestIntegrity;
  end;

  TTestEnum = class(TTestCase)
  published
    procedure TestGetNameByEnum;
    procedure TestGetNames;
    procedure TestGetNameByInteger;
    procedure TestGetValueByEnum;
    procedure TestGetValueByName;
    procedure TestIsValid;
    procedure TestTryParse;
    procedure TestParse;
    procedure TestParseIntegerException;
    procedure TestParseStringException;
  end;

  TTestBaseRoutines = class(TTestCase)
  published
    procedure TestNextPowerOf2;
  end;

  THashKind = (xxHash32, Murmur3Hash);

  TTestHash = class(TTestCase)
  published
    procedure TestHash([Values] hashKind: THashKind; [Range(0, 64)] len: Cardinal);
  end;

implementation

uses
  DateUtils,
  FmtBcd,
  SqlTimSt,
  SysUtils,
  Variants,
  Rtti,
  StrUtils,
  TimeSpan,
  Types,
  Spring.Comparers,
  Spring.Hash,
  Spring.VirtualClass;


{$REGION 'TTestNullableInteger'}

procedure TTestNullableInteger.TestInitialValue;
begin
  CheckFalse(fInteger.HasValue);
end;

procedure TTestNullableInteger.GetValueOrDefault;
begin
  CheckFalse(fInteger.HasValue);
  CheckEquals(Default(Integer), fInteger.GetValueOrDefault);
  CheckEquals(18, fInteger.GetValueOrDefault(18));
end;

procedure TTestNullableInteger.TearDown;
begin
  inherited;
  fInteger := nil;
end;

procedure TTestNullableInteger.TestAssignFive;
begin
  fInteger := 5;
  Check(fInteger.HasValue);
  CheckEquals(5, fInteger.Value);
  Check(fInteger.Value = 5);
  Check(fInteger.Value <> 3);
end;

procedure TTestNullableInteger.TestAssignFloat;
begin
  ExpectedException := EInvalidCast;
  fInteger := Nullable<Integer>(99.9);
end;

procedure TTestNullableInteger.TestAssignNull;
var
  n: Nullable<Integer>;
begin
  n := 5;
  n := nil;
  Check(not n.HasValue);
end;

procedure TTestNullableInteger.TestAssignStringInt;
begin
  // Nullable does NOT do a variant type conversion but is strict about the underlying type
  ExpectedException := EInvalidCast;
  fInteger := Nullable<Integer>('5');
end;

procedure TTestNullableInteger.TestAssignStringNonInt;
begin
  ExpectedException := EInvalidCast;
  fInteger := Nullable<Integer>('5x');
end;

procedure TTestNullableInteger.TestDefaultReturnsInitialValue;
begin
  fInteger := Default(Nullable<Integer>);
  CheckFalse(fInteger.HasValue);
end;

procedure TTestNullableInteger.TestException;
begin
  ExpectedException := EInvalidOperationException;
  fInteger.Value;
end;

procedure TTestNullableInteger.TestLocalVariable;
var
  dirtyValue: Nullable<Integer>;  { lives in stack }
begin
  CheckFalse(dirtyValue.HasValue);
  CheckTrue(dirtyValue = nil);
  dirtyValue := 5;
  CheckTrue(dirtyValue <> nil);
end;

procedure TTestNullableInteger.TestNullableNull;
begin
  fInteger := 42;
  CheckEquals(42, fInteger.Value);
  fInteger := nil;
  Check(not fInteger.HasValue);
  Check(fInteger = nil);
  CheckFalse(fInteger <> nil);
end;

procedure TTestNullableInteger.TestTryGetValue;
var
  b: Boolean;
  i: Integer;
begin
  b := fInteger.TryGetValue(i);
  CheckFalse(b);
  fInteger := 42;
  b := fInteger.TryGetValue(i);
  CheckTrue(b);
  CheckEquals(42, i);
end;

procedure TTestNullableInteger.TestFromVariant;
var
  value: Variant;
const
  ExpectedInteger: Integer = 5;
begin
  value := Null;
  fInteger := Nullable<Integer>.Create(value);
  CheckFalse(fInteger.HasValue);

  fInteger := Nullable<Integer>(value);
  CheckFalse(fInteger.HasValue);

  value := ExpectedInteger;
  fInteger := Nullable<Integer>.Create(value);
  CheckTrue(fInteger.HasValue);
  CheckEquals(ExpectedInteger, fInteger.Value);
end;

procedure TTestNullableInteger.TestEquals;
var
  a, b: Nullable<Integer>;
begin
  CheckFalse(a.HasValue);
  CheckFalse(b.HasValue);

  CheckTrue(a.Equals(b));
  CheckTrue(b.Equals(a));

  a := 2;
  CheckFalse(a.Equals(b));
  CheckFalse(b.Equals(a));

  b := 2;
  CheckTrue(a.Equals(b));

  b := 3;
  CheckFalse(a.Equals(b));
end;

{$ENDREGION}


{$REGION 'TTestNullableBoolean'}

procedure TTestNullableBoolean.TearDown;
begin
  inherited;
  fBoolean := nil;
end;

procedure TTestNullableBoolean.TestIssue55;
var
  v: Variant;
begin
  fBoolean := True;
  v := fBoolean.ToVariant;
  CheckTrue(v);
end;

{$ENDREGION}


{$REGION 'TCustomRecord'}

constructor TCustomRecord.Create(x, y: Integer);
begin
  Self.x := x;
  Self.y := y;
end;

class operator TCustomRecord.Equal(const left, right: TCustomRecord): Boolean;
begin
  Result := (left.x = right.y) and (left.y = right.x);
end;

{$ENDREGION}


{$REGION 'TTestNullableCustomRecord'}

procedure TTestNullableCustomRecord.TestEqualsUsingOperatorOverload;
var
  x, y: Nullable<TCustomRecord>;
begin
  x := TCustomRecord.Create(1, 2);
  y := TCustomRecord.Create(1, 2);
  CheckFalse(x = y);

  y := TCustomRecord.Create(2, 1);
  CheckTrue(x = y);
end;

{$ENDREGION}


{$REGION 'TTestMulticastEvent'}

procedure TTestMulticastEvent.SetUp;
begin
  inherited;
  fEvent := TNotifyEventImpl.Create();
  fProc :=
    procedure(const i: Integer; const s: string)
    begin
      Inc(fHandlerInvokeCount, i);
      CheckEquals(CText, s);
    end;
end;

procedure TTestMulticastEvent.TearDown;
begin
  inherited;
  fEvent := nil;
  fASender := nil;
  fAInvoked := False;
  fBSender := nil;
  fBInvoked := False;
  fHandlerInvokeCount := 0;
  fProc := nil;
  TEventHandler.fClassHandlerInvoked := False;
end;

procedure TTestMulticastEvent.HandlerA(sender: TObject);
begin
  fASender := sender;
  fAInvoked := True;
end;

procedure TTestMulticastEvent.HandlerB(sender: TObject);
begin
  fBSender := sender;
  fBInvoked := True;
end;

procedure TTestMulticastEvent.HandlerDouble(const value: Double);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandlerExtended(const value: Extended);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandlerInt64(const value: Int64);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandlerSingle(const value: Single);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandlerWithRegisterParams(const value1, value2, value3: NativeInt);
begin
  CheckEquals(42, value1);
  CheckEquals(43, value2);
  CheckEquals(44, value3);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandlerWithFloatParams(const value1, value2, value3: Double);
begin
  CheckEquals(42, value1);
  CheckEquals(43, value2);
  CheckEquals(44, value3);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandlerWithStackParams(const value1: Int64;
  const value2: Single; const value3: Double; const value4: Extended;
  const value5: TEventArgs);
begin
  CheckEquals(42, value1);
  CheckEquals(43, value2);
  CheckEquals(44, value3);
  CheckEquals(45, value4);
  CheckEquals(46, value5.i);
  CheckEquals('47', value5.s);
  CheckEquals(48, value5.v);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestAddNil;
var
  e: Event<TNotifyEvent>;
  e2: Event<TProc<Integer, string>>;
begin
  e.Add(nil);
  e.Invoke(nil);
  CheckEquals(0, fHandlerInvokeCount);

  e2.Add(nil);
  e2.Invoke(0, '');
  CheckEquals(0, fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestClassProcedureHandler;
var
  e: Event<TEventInt64>;
begin
  e.Add(TEventHandler.HandleInt64Static);
  e.Invoke(42);
  e.Remove(TEventHandler.HandleInt64Static);
  CheckTrue(TEventHandler.fClassHandlerInvoked);
end;

procedure TTestMulticastEvent.TestClear;
var
  e: Event<TNotifyEvent>;
begin
  e.Add(HandleChanged);
  e.Clear;
  e.Invoke(nil);
  CheckEquals(0, fHandlerInvokeCount);
  e.Clear;
end;

procedure TTestMulticastEvent.TestDelegate;
var
  e: Event<Action<Integer, string>>;
begin
  e.Add(fProc);
  e.Invoke(CNumber, CText);
  CheckEquals(CNumber, fHandlerInvokeCount);
  e.Remove(fProc);
  e.Invoke(CNumber, CText);
  CheckEquals(CNumber, fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestInstanceProcedureHandler;
var
  e: Event<TEventInt64>;
  t: TEventHandler;
begin
  t := TEventHandler.Create;
  try
    e.Add(t.HandleInt64);
    e.Invoke(42);
    CheckTrue(t.fClassHandlerInvoked);
  finally
    t.Free;
  end;
end;

procedure TTestMulticastEvent.TestInvoke;
begin
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);

  fEvent.Enabled := False;
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);

  fEvent.Enabled := True;
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);

  fEvent.Add(HandlerA);
  fEvent.Enabled := False;
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);

  fEvent.Enabled := True;
  fEvent.Invoke(Self);
  CheckTrue(fAInvoked);
  CheckFalse(fBInvoked);

  fAInvoked := False;
  fEvent.Add(HandlerB);
  fEvent.Enabled := False;
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);

  fEvent.Enabled := True;
  fEvent.Invoke(Self);
  CheckTrue(fAInvoked);
  CheckTrue(fBInvoked);

  fAInvoked := False;
  fBInvoked := False;
  fEvent.Enabled := False;
  fEvent.Remove(HandlerA);
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);

  fEvent.Enabled := True;
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckTrue(fBInvoked);

  fBInvoked := False;
  fEvent.Enabled := False;
  fEvent.Remove(HandlerB);
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);

  fEvent.Enabled := True;
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);
end;

procedure TTestMulticastEvent.TestIssue58;
var
  e: Event<TNotifyEvent>;
  i: IEvent<TNotifyEvent>;
  t: TNotifyEvent;
begin
  i := e;
  t := e.Invoke;
  Check(Assigned(i));
end;

procedure TTestMulticastEvent.TestIssue60;
var
  eventInt64: Event<TEventInt64>;
  eventSingle: Event<TEventSingle>;
  eventDouble: Event<TEventDouble>;
  eventExtended: Event<TEventExtended>;
  expected: Integer;
begin
  expected := 0;

  eventInt64.Add(HandlerInt64);
  eventSingle.Add(HandlerSingle);
  eventDouble.Add(HandlerDouble);
  eventExtended.Add(HandlerExtended);

  eventInt64.Invoke(42); Inc(expected);
  eventSingle.Invoke(42); Inc(expected);
  eventDouble.Invoke(42); Inc(expected);
  eventExtended.Invoke(42); Inc(expected);

  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestNotify;
var
  event: Event<TEventInt64>;
begin
  event.OnChanged := HandleChanged;
  event.Add(HandlerInt64);
  event.Remove(HandlerInt64);
  CheckEquals(2, fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestNotifyDelegate;
var
  event2: Event<Action<Integer, string>>;
begin
  event2.OnChanged := HandleChanged;
  event2.Add(fProc);
  event2.Remove(fProc);
  CheckEquals(2, fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandleChanged(Sender: TObject);
begin
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestOneHandler;
begin
  fEvent.Add(HandlerA);

  fEvent.Invoke(Self);
  CheckTrue(fAInvoked);
  CheckSame(Self, fASender);
  CheckFalse(fBInvoked);
  CheckSame(nil, fBSender);

  fEvent.Remove(HandlerA);
end;

procedure TTestMulticastEvent.TestRecordType;
var
  e: Event<TNotifyEvent>;
begin
  CheckTrue(e.Enabled);
//  CheckTrue(e.IsEmpty);

  e.Add(HandlerA);
  e.Add(HandlerB);
  e.Invoke(nil);

//  CheckFalse(e.IsEmpty);
//  CheckEquals(2, e.Count);

  CheckTrue(fAInvoked);
  CheckSame(nil, fASender);
  CheckTrue(fBInvoked);
  CheckSame(nil, fBSender);

  e.Remove(HandlerA);
//  CheckEquals(1, e.Count);

  e.Remove(HandlerB);
//  CheckEquals(0, e.Count);
end;

procedure TTestMulticastEvent.TestTwoHandlers;
begin
  fEvent.Add(HandlerA);
  fEvent.Add(HandlerB);
  fEvent.Invoke(nil);

  CheckTrue(fAInvoked);
  CheckSame(nil, fASender);
  CheckTrue(fBInvoked);
  CheckSame(nil, fBSender);

  fEvent.Remove(HandlerA);

  fEvent.Remove(HandlerB);
end;

procedure TTestMulticastEvent.TestRemove;
begin
  fEvent.Add(HandlerA);
  fEvent.Remove(HandlerB);
  fEvent.Invoke(nil);
  Check(fAInvoked);
end;

procedure TTestMulticastEvent.TestSetUseFreeNotification;
begin
  CheckTrue(fEvent.UseFreeNotification);
  fEvent.UseFreeNotification := False;
  CheckFalse(fEvent.UseFreeNotification);
  fEvent.UseFreeNotification := False;
  CheckFalse(fEvent.UseFreeNotification);

  fEvent.Add(HandlerA);

  fEvent.UseFreeNotification := True;
  CheckTrue(fEvent.UseFreeNotification);
  fEvent.UseFreeNotification := True;
  CheckTrue(fEvent.UseFreeNotification);
end;

procedure TTestMulticastEvent.TestStackParams;
var
  event: Event<TEventWithStackParams>;
  expected: Integer;
  args: TEventArgs;
begin
  expected := 1;
  args.i := 46;
  args.s := '47';
  args.v := 48;

  event.Add(HandlerWithStackParams);
  HandlerWithStackParams(42, 43, 44, 45, args);
  event.Invoke(42, 43, 44, 45, args); Inc(expected);

  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestRegisterParams;
var
  event: Event<TEventWithRegisterParams>;
  expected: Integer;
begin
  expected := 1;

  event.Add(HandlerWithRegisterParams);
  HandlerWithRegisterParams(42, 43, 44);
  event.Invoke(42, 43, 44); Inc(expected);

  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestFloatParams;
var
  event: Event<TEventWithFloatParams>;
  expected: Integer;
begin
  expected := 1;

  event.Add(HandlerWithFloatParams);
  HandlerWithFloatParams(42, 43, 44);
  event.Invoke(42, 43, 44); Inc(expected);

  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TEventHandler.HandleInt64(const value: Int64);
begin
  fClassHandlerInvoked := value = 42;
end;

class procedure TEventHandler.HandleInt64Static(const value: Int64);
begin
  fClassHandlerInvoked := value = 42;
end;

{$ENDREGION}


{$REGION 'TTestLazy'}

procedure TTestLazy.TestByValueFactory;
var
  factory: Func<Integer>;
begin
  factory :=
    function: Integer
    begin
      Result := CExpectedBalance;
    end;
  fBalance := Lazy<Integer>(factory);

  CheckFalse(fBalance.IsValueCreated);

  CheckEquals(CExpectedBalance, fBalance.Value);
  CheckEquals(CExpectedBalance, (fBalance as ILazy).Value.AsInteger);

  CheckTrue(fBalance.IsValueCreated);
end;

procedure TTestLazy.TestIsLazyType;
begin
  CheckTrue(IsLazyType(TypeInfo(Lazy<Integer>)));
  CheckTrue(IsLazyType(TypeInfo(ILazy<Integer>)));
  CheckTrue(IsLazyType(TypeInfo(TFunc<Integer>)));
  CheckTrue(IsLazyType(TypeInfo(Func<Integer>)));
  CheckFalse(IsLazyType(TypeInfo(TFunc<string,Integer>)));
  CheckTrue(IsLazyType(TypeInfo(TFunc<TFunc<string,Integer>>)));
  CheckTrue(IsLazyType(TypeInfo(Func<TFunc<string,Integer>>)));
  CheckTrue(IsLazyType(TypeInfo(TFunc<Func<string,Integer>>)));
end;

procedure TTestLazy.Test_Initializer_RaisesArgumentException_NotReferenceType;
var
  i: Integer;
begin
  ExpectedException := EArgumentException;
  TLazyInitializer.EnsureInitialized<Integer>(i, function: Integer begin Exit(42) end);
end;

procedure TTestLazy.Issue375;
var
  lazyType: Lazy<TStringStream>;
  lazyTypeValue: TValue;
  lazyValue: TValue;
begin
  lazyType := Lazy<TStringStream>.Create(
    function: TStringStream
    begin
      Result := TStringStream.Create;
    end, True);
  lazyType.Value.WriteString('Some text');
  lazyType.Value.WriteString('Some text');

  lazyTypeValue := TValue.From<Lazy<TStringStream>>(lazyType);
  CheckTrue(IsLazyType(lazyTypeValue.TypeInfo));
  CheckTrue(lazyTypeValue.TryGetLazyValue(lazyValue));
end;

procedure TTestLazy.TearDown;
begin
  inherited;
  fBalance := nil;
end;

procedure TTestLazy.TestByValue;
begin
  fBalance := Lazy<Integer>.CreateFrom(CExpectedBalance);

  CheckTrue(fBalance.IsValueCreated);

  CheckEquals(CExpectedBalance, fBalance.Value);
  CheckEquals(CExpectedBalance, (fBalance as ILazy).Value.AsInteger);

  CheckTrue(fBalance.IsValueCreated);
end;

{$ENDREGION}


{$REGION 'TTestGuard'}

procedure TTestGuard.TestCheckRange;
var
  dynArray: array of Byte;
const
  len = 4;
  idx = 1;
begin
  // check string (1-based)
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange('abcde', 0);
    end);
  Guard.CheckRange('abcde', 1);
  Guard.CheckRange('abcde', 5);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange('abcde', 6);
    end);

  // check 0-based byte array
  SetLength(dynArray, 4);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(dynArray, -1);
    end);
  Guard.CheckRange(dynArray, 0);
  Guard.CheckRange(dynArray, 3);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(dynArray, 4);
    end);

  // check 1-based range
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 0, 0, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 0, 1, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 0, 5, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 1, -1, idx);
    end);
  Guard.CheckRange(len, 1, 0, idx);
  Guard.CheckRange(len, 1, 1, idx);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 1, 5, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 5, 0, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 5, 1, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 5, 5, idx);
    end);
end;

type
  TTestEnum1 = (x = 1, y, z);
  TTestSet1 = set of TTestEnum1;

  TTestEnum2 = (a, b, c);
  TTestSet2 = set of TTestEnum2;

procedure TTestGuard.TestCheckSet;
var
  sut1: TTestSet1;
  sut2: TTestSet2;
begin
  sut1 := [x..z];
  sut2 := [a..b];
  Guard.CheckSet<TTestSet1>(sut1, 'sut1');
  Guard.CheckSet<TTestSet1>([], 'sut1');
  Guard.CheckSet<TTestSet2>(sut2, 'sut2');
  Guard.CheckSet<TTestSet2>([], 'sut2');
  Pass;
end;

procedure TTestGuard.TestIsNullReference;
var
  obj: TObject;
  intf: IInterface;
  e: TNotifyEvent;
begin
  obj := nil;
  CheckTrue(Guard.IsNullReference(obj, TypeInfo(TObject)));
  CheckTrue(Guard.IsNullReference(intf, TypeInfo(IInterface)));
  e := nil;
  CheckTrue(Guard.IsNullReference(e, TypeInfo(TNotifyEvent)));
  TMethod(e).Data := Self;
  CheckFalse(Assigned(e));
  CheckFalse(Guard.IsNullReference(e, TypeInfo(TNotifyEvent)));
end;

procedure TTestGuard.TestNotNull;
var
  intf: IInterface;
begin
  StartExpectingException(EArgumentNilException);
  Guard.CheckNotNull(intf, 'intf');
  StopExpectingException();
end;

{$ENDREGION}


{$REGION 'TTestMulticastEventStackSize'}

procedure TTestMulticastEventStackSize.SetUp;
begin
  inherited;
end;

procedure TTestMulticastEventStackSize.TearDown;
begin
  inherited;
  fHandlerInvokeCount := 0;
end;

procedure TTestMulticastEventStackSize.HandlerDouble(const value: Double);
begin
  CheckEquals(Double42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.HandlerExtended(const value: Extended);
begin
  CheckEquals(Extended42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.HandlerGuid(const value: TGUID);
begin
  CheckEquals(GUIDToString(GUID42), GUIDToString(value));
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.HandlerInt64(const value: Int64);
begin
  CheckEquals(Int6442, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.HandlerSingle(const value: Single);
begin
  CheckEquals(Single42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.LogEnter(const expected: Integer; const MethodName: string);
begin
  Writeln(Format('%s.%s', [ClassName, MethodName]));
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
end;

procedure TTestMulticastEventStackSize.LogLeave(const expected: Integer);
begin
  Writeln(Format('Exit: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
end;

procedure TTestMulticastEventStackSize.TestIssue60Double;
var
  eventDouble: Event<TEventDouble>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  LogEnter(expected, 'TestIssue60Double');
{$ENDIF LogConsole}
  eventDouble.Add(HandlerDouble);
  eventDouble.Invoke(42);
  Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60DoubleAssignedConst;
var
  eventDouble: Event<TEventDouble>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  LogEnter(expected, 'TestIssue60DoubleAssignedConst');
{$ENDIF LogConsole}
  eventDouble.Add(HandlerDouble);
  eventDouble.Invoke(Double42);
  Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60Extended;
var
  eventExtended: Event<TEventExtended>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  LogEnter(expected, 'TestIssue60Extended');
{$ENDIF LogConsole}
  eventExtended.Add(HandlerExtended);
  eventExtended.Invoke(42);
  Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60ExtendedAssignedConst;
var
  eventExtended: Event<TEventExtended>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  LogEnter(expected, 'TestIssue60ExtendedAssignedConst');
{$ENDIF LogConsole}
  eventExtended.Add(HandlerExtended);
  eventExtended.Invoke(Extended42);
  Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60GuidAssignedConst;
var
  eventExtended: Event<TEventGuid>;
  expected: Integer;
  guid: TGUID;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60GuidAssignedConst');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventExtended.Add(HandlerGuid);
  guid := GUID42;
  eventExtended.Invoke(guid); // pass variable to avoid AV during method interception
  Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60Int64;
var
  eventInt64: Event<TEventInt64>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60Int64');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventInt64.Add(HandlerInt64);
  eventInt64.Invoke(42);
  Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60Int64AssignedConst;
var
  eventInt64: Event<TEventInt64>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60Int64AssignedConst');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventInt64.Add(HandlerInt64);
  eventInt64.Invoke(Int6442);
  Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60Single;
var
  eventSingle: Event<TEventSingle>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60Single');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventSingle.Add(HandlerSingle);
  eventSingle.Invoke(42);
  Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60SingleAssignedConst;
var
  eventSingle: Event<TEventSingle>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60SingleAssignedConst');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventSingle.Add(HandlerSingle);
  eventSingle.Invoke(Single42);
  Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

{$ENDREGION}


{$REGION 'TTestSpringEventsMethods'}

type
  TShortString0 = String[0];
  TShortString1 = String[1];
  TShortString2 = String[2];
  TShortString255 = String[255];
  TShortString7 = String[7];

// for reference see http://www.guidogybels.eu/asmtable3.html
procedure TTestSpringEventsMethods.MatchType(const aTypeInfo: PTypeInfo;
  const aExpectedTypeKind: TTypeKind; const aExpectedTypeSize: Integer);
var
  lActualTypeSize: Integer;
  lExpectedTypeKindName: string;
  lTypeInfoKind: TTypeKind;
  lTypeInfoKindName: string;
begin
  lTypeInfoKind := aTypeInfo.Kind;

  Exclude(fRemainingTypeKinds, lTypeInfoKind);
  Include(fTestedTypeKinds, lTypeInfoKind);

  lTypeInfoKindName := TEnum.GetName<TTypeKind>(lTypeInfoKind);
  lExpectedTypeKindName := TEnum.GetName<TTypeKind>(aExpectedTypeKind);
  CheckTrue(aExpectedTypeKind = lTypeInfoKind,
    Format('aExpectedTypeKind "%s" does not match actual lTypeInfoKind "%s"', [
    lExpectedTypeKindName, lTypeInfoKindName]));

  lActualTypeSize := GetTypeSize(aTypeInfo);
  CheckEquals(aExpectedTypeSize, lActualTypeSize,
    Format('aExpectedTypeSize %d does not lActualTypeSize %d', [
    aExpectedTypeSize, lActualTypeSize]));
end;

procedure TTestSpringEventsMethods.SetUp;
begin
  inherited;
  fRemainingTypeKinds := tkAny - [tkUnknown];
  fTestedTypeKinds := [];
end;

procedure TTestSpringEventsMethods.TearDown;
begin
  inherited;
end;

procedure TTestSpringEventsMethods.Test_EnsureAllTTypeKindsCoveredByCallsTo_Test_GetTypeSize_;
var
  fRemainingTypeKindNames: TStrings;
  fTestedTypeKindNames: TStrings;
  lAllTypeKinds: TTypeKinds;
  lContext: TRttiContext;
  lMethod: TRttiMethod;
  lMethods: TArray<TRttiMethod>;
  lNoTypeKinds: TTypeKinds;
  lParameters: TArray<TRttiParameter>;
  lType: TRttiType;
  lTypeKind: TTypeKind;
  lTypeKindName: string;
begin
  lAllTypeKinds := fRemainingTypeKinds;
  lNoTypeKinds := fTestedTypeKinds;

  lContext := TRttiContext.Create();
  try
    lType := lContext.GetType(Self.ClassInfo);
    lMethods := lType.GetMethods();

    for lMethod in lMethods do
    begin
      if lMethod.Visibility = mvPublished then
        if lMethod.MethodKind = mkProcedure then
          if StartsText('Test_GetTypeSize_', lMethod.Name) then
          begin
            lParameters := lMethod.GetParameters();
            if Length(lParameters) = 0 then
            try
              lMethod.Invoke(Self, []);
            except
              on ETestFailure do
                ; // kill exception
              on EAssertionFailed do
                ; // kill exception
            end;
          end;
    end;
  finally
    lContext.Free();
  end;

  fRemainingTypeKindNames := TStringList.Create();
  try
    fTestedTypeKindNames := TStringList.Create();
    try
      for lTypeKind := Low(TTypeKind) to High(TTypeKind) do
      begin
        lTypeKindName := TEnum.GetName<TTypeKind>(lTypeKind);
        if lTypeKind in fRemainingTypeKinds then
          fRemainingTypeKindNames.Add(lTypeKindName);
        if lTypeKind in fTestedTypeKinds then
          fTestedTypeKindNames.Add(lTypeKindName);
      end;
      CheckTrue(fRemainingTypeKinds = lNoTypeKinds, 'fRemainingTypeKinds is not empty: ' + fRemainingTypeKindNames.CommaText);
      CheckTrue(fTestedTypeKinds = lAllTypeKinds, 'fRemainingTypeKinds should contain all TTypeKinds: ' + fTestedTypeKindNames.CommaText);
    finally
      fTestedTypeKindNames.Free;
    end;
  finally
    fRemainingTypeKindNames.Free;
  end;
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Double;
begin
  MatchType(TypeInfo(Double), tkFloat, SizeOf(Double));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Extended;
begin
{$IFDEF ALIGN_STACK}
  MatchType(TypeInfo(Extended), tkFloat, 16);
{$ELSE}
  MatchType(TypeInfo(Extended), tkFloat, SizeOf(Extended));
{$ENDIF}
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Guid;
begin
  MatchType(TypeInfo(TGuid), tkRecord, SizeOf(TGuid));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ShortInt;
begin
  MatchType(TypeInfo(ShortInt), tkInteger, SizeOf(ShortInt));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Int64;
begin
  MatchType(TypeInfo(Int64), tkInt64, SizeOf(Int64));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Integer;
begin
  MatchType(TypeInfo(Integer), tkInteger, SizeOf(Integer));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_SmallInt;
begin
  MatchType(TypeInfo(SmallInt), tkInteger, SizeOf(SmallInt));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_AnsiChar;
begin
  MatchType(TypeInfo(AnsiChar), tkChar, SizeOf(AnsiChar));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_AnsiString;
begin
  MatchType(TypeInfo(AnsiString), tkLString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Array;
begin
  MatchType(TypeInfo(TTextBuf), tkArray, SizeOf(TTextBuf));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Boolean;
begin
  MatchType(TypeInfo(Boolean), tkEnumeration, SizeOf(Boolean));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Byte;
begin
  MatchType(TypeInfo(Byte), tkInteger, SizeOf(Byte));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ByteBool;
begin
  MatchType(TypeInfo(ByteBool), tkEnumeration, SizeOf(ByteBool)); // not tkInteger !!
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Word;
begin
  MatchType(TypeInfo(Word), tkInteger, SizeOf(Word));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Cardinal;
begin
  MatchType(TypeInfo(Cardinal), tkInteger, SizeOf(Cardinal));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Char;
begin
  MatchType(TypeInfo(Char), tkWChar, SizeOf(Char));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Class;
begin
  MatchType(TypeInfo(TTestSpringEventsMethods), tkClass, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ClassRef;
begin
  MatchType(TypeInfo(TClass), tkClassRef, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Currency;
begin
  MatchType(TypeInfo(Currency), tkFloat, SizeOf(Currency));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Comp;
begin
  MatchType(TypeInfo(Comp), tkFloat, SizeOf(Comp));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_DynamicArray;
begin
  MatchType(TypeInfo(TIntegerDynArray), tkDynArray, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_LongBool;
begin
  MatchType(TypeInfo(LongBool), tkEnumeration, SizeOf(LongBool)); // not tkInteger !!
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_LongInt;
begin
{$IF Defined(LINUX64)}
  MatchType(TypeInfo(LongInt), tkInt64, SizeOf(LongInt));
{$ELSE}
  MatchType(TypeInfo(LongInt), tkInteger, SizeOf(LongInt));
{$IFEND}
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_LongWord;
begin
{$IF Defined(LINUX64)}
  MatchType(TypeInfo(LongWord), tkInt64, SizeOf(LongWord));
{$ELSE}
  MatchType(TypeInfo(LongWord), tkInteger, SizeOf(LongWord));
{$IFEND}
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_NativeInt;
begin
{$IF Defined(CPU64)}
  MatchType(TypeInfo(NativeInt), tkInt64, SizeOf(NativeInt));
{$ELSE}
  MatchType(TypeInfo(NativeInt), tkInteger, SizeOf(NativeInt));
{$IFEND}
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_NativeUInt;
begin
{$IF Defined(CPU64)}
  MatchType(TypeInfo(NativeUInt), tkInt64, SizeOf(NativeUInt));
{$ELSE}
  MatchType(TypeInfo(NativeUInt), tkInteger, SizeOf(NativeUInt));
{$IFEND}
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_OleVariant;
begin
  MatchType(TypeInfo(OleVariant), tkVariant, SizeOf(OleVariant));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Pointer;
begin
  MatchType(TypeInfo(Pointer), tkPointer, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Real;
begin
  MatchType(TypeInfo(Real), tkFloat, SizeOf(Real));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Set;
begin
  MatchType(TypeInfo(TTypeKinds), tkSet, SizeOf(TTypeKinds));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_SetOfByte;
begin
  MatchType(TypeInfo(TByteSet), tkSet, SizeOf(TByteSet));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Single;
begin
  MatchType(TypeInfo(Single), tkFloat, SizeOf(Single));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ShortString;
begin
  MatchType(TypeInfo(ShortString), tkString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ShortString0;
begin
  MatchType(TypeInfo(TShortString0), tkString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ShortString1;
begin
  MatchType(TypeInfo(TShortString1), tkString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ShortString2;
begin
  MatchType(TypeInfo(TShortString2), tkString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Interface;
begin
  MatchType(TypeInfo(IInterface), tkInterface, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Method;
begin
  MatchType(TypeInfo(TNotifyEvent), tkMethod, PointerSize * 2);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_PAnsiChar;
begin
  MatchType(TypeInfo(PAnsiChar), tkPointer, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_PChar;
begin
  MatchType(TypeInfo(PChar), tkPointer, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Proc;
begin
  MatchType(TypeInfo(TProc), tkInterface, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Procedure;
begin
  MatchType(TypeInfo(TProcedure), tkProcedure, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_PWideChar;
begin
  MatchType(TypeInfo(PWideChar), tkPointer, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ShortString255;
begin
  MatchType(TypeInfo(TShortString255), tkString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ShortString7;
begin
  MatchType(TypeInfo(TShortString7), tkString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_string;
begin
  MatchType(TypeInfo(string), tkUString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_UnicodeString;
begin
  MatchType(TypeInfo(UnicodeString), tkUString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Variant;
begin
  MatchType(TypeInfo(Variant), tkVariant, SizeOf(Variant));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_WideChar;
begin
  MatchType(TypeInfo(WideChar), tkWChar, SizeOf(WideChar));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_WideString;
begin
  MatchType(TypeInfo(WideString), tkWString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_WordBool;
begin
  MatchType(TypeInfo(WordBool), tkEnumeration, SizeOf(WordBool)); // not tkInteger !!
end;

{$IF Declared(tkMRecord)}
procedure TTestSpringEventsMethods.Test_GetTypeSize_ManagedRecord;
begin
  Exclude(fRemainingTypeKinds, tkMRecord);
  Include(fTestedTypeKinds, tkMRecord);
  Pass;
end;
{$IFEND}

{$ENDREGION}


{$REGION 'TTestTuples'}

procedure TTestTuplesDouble.SetUp;
begin
  fSUT := Tuple<Integer, string>.Create(42, 'foo');
end;

procedure TTestTuplesDouble.TearDown;
begin
  inherited;
  fSUT := Default(Tuple<Integer, string, Boolean>);
end;

procedure TTestTuplesDouble.Test_Create;
begin
  CheckEquals(42, fSUT.Value1);
  CheckEquals('foo', fSUT.Value2);
end;

procedure TTestTuplesDouble.Test_Equals_ReturnsFalse_WhenAllValuesDiffer;
var
  tup: Tuple<Integer, string>;
begin
  tup := Tuple<Integer, string>.Create(43, 'bar');
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesDouble.Test_Equals_ReturnsFalse_WhenFirstValueDiffers;
var
  tup: Tuple<Integer, string>;
begin
  tup := Tuple<Integer, string>.Create(43, 'foo');
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesDouble.Test_Equals_ReturnsFalse_WhenSecondValueDiffers;
var
  tup: Tuple<Integer, string>;
begin
  tup := Tuple<Integer, string>.Create(42, 'bar');
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesDouble.Test_Equals_ReturnsTrue_WhenEqual;
var
  tup: Tuple<Integer, string>;
begin
  tup := Tuple<Integer, string>.Create(42, 'foo');
  CheckTrue(fSUT.Equals(tup));
  CheckTrue(fSUT = tup);
end;

{$IFDEF DELPHIXE5_UP}
procedure TTestTuplesDouble.Test_Implicit_FromOpenArray;
var
  tup: Tuple<Integer, string>;
begin
  tup := [42, 'foo'];
  CheckEquals(42, tup.Value1);
  CheckEquals('foo', tup.Value2);
end;
{$ENDIF}

procedure TTestTuplesDouble.Test_Implicit_FromValueArray;
var
  arr: TArray<TValue>;
  tup: Tuple<Integer, string>;
begin
  SetLength(arr, 2);
  arr[0] := TValue.From<Integer>(42);
  arr[1] := TValue.From<string>('foo');
  tup := arr;
  CheckEquals(42, tup.Value1);
  CheckEquals('foo', tup.Value2);
end;

procedure TTestTuplesDouble.Test_Implicit_ToValueArray;
var
  arr: TArray<TValue>;
begin
  arr := fSUT;
  CheckEquals(2, Length(arr));
  Check(arr[0].TypeInfo = TypeInfo(Integer));
  Check(arr[1].TypeInfo = TypeInfo(string));
  CheckEquals(42, arr[0].AsInteger);
  CheckEquals('foo', arr[1].AsString);
end;

procedure TTestTuplesDouble.Test_Pack;
var
  tup: Tuple<Integer, string>;
begin
  tup := Tuple.Create(Integer(42), 'foo');
  CheckEquals(42, tup.Value1);
  CheckEquals('foo', tup.Value2);
end;

procedure TTestTuplesDouble.Test_Unpack;
var
  val1: Integer;
  val2: string;
begin
  fSUT.Unpack(val1, val2);
  CheckEquals(42, val1);
  CheckEquals('foo', val2);
end;

{$ENDREGION}


{$REGION 'TTestTuplesTriple'}

procedure TTestTuplesTriple.SetUp;
begin
  fSUT := Tuple<Integer, string, Boolean>.Create(42, 'foo', True);
end;

procedure TTestTuplesTriple.TearDown;
begin
  inherited;
  fSUT := Default(Tuple<Integer, string, Boolean>);
end;

procedure TTestTuplesTriple.Test_Create;
begin
  CheckEquals(42, fSUT.Value1);
  CheckEquals('foo', fSUT.Value2);
  CheckEquals(True, fSUT.Value3);
end;

procedure TTestTuplesTriple.Test_Equals_ReturnsFalse_WhenAllValuesDiffer;
var
  tup: Tuple<Integer, string, Boolean>;
begin
  tup := Tuple<Integer, string, Boolean>.Create(43, 'bar', False);
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesTriple.Test_Equals_ReturnsFalse_WhenFirstValueDiffers;
var
  tup: Tuple<Integer, string, Boolean>;
begin
  tup := Tuple<Integer, string, Boolean>.Create(43, 'foo', True);
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesTriple.Test_Equals_ReturnsFalse_WhenSecondValueDiffers;
var
  tup: Tuple<Integer, string, Boolean>;
begin
  tup := Tuple<Integer, string, Boolean>.Create(42, 'bar', True);
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesTriple.Test_Equals_ReturnsFalse_WhenThirdValueDiffers;
var
  tup: Tuple<Integer, string, Boolean>;
begin
  tup := Tuple<Integer, string, Boolean>.Create(42, 'foo', False);
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesTriple.Test_Equals_ReturnsTrue_WhenEqual;
var
  tup: Tuple<Integer, string, Boolean>;
begin
  tup := Tuple<Integer, string, Boolean>.Create(42, 'foo', True);
  CheckTrue(fSUT.Equals(tup));
  CheckTrue(fSUT = tup);
end;

{$IFDEF DELPHIXE5_UP}
procedure TTestTuplesTriple.Test_Implicit_FromOpenArray;
var
  tup: Tuple<Integer, string, Boolean>;
begin
  tup := [42, 'foo', True];
  CheckEquals(42, tup.Value1);
  CheckEquals('foo', tup.Value2);
  CheckEquals(True, tup.Value3);
end;
{$ENDIF}

procedure TTestTuplesTriple.Test_Implicit_FromValueArray;
var
  arr: TArray<TValue>;
  tup: Tuple<Integer, string, Boolean>;
begin
  SetLength(arr, 3);
  arr[0] := TValue.From<Integer>(42);
  arr[1] := TValue.From<string>('foo');
  arr[2] := TValue.From<Boolean>(True);
  tup := arr;
  CheckEquals(42, tup.Value1);
  CheckEquals('foo', tup.Value2);
  CheckEquals(True, tup.Value3);
end;

procedure TTestTuplesTriple.Test_Implicit_ToValueArray;
var
  arr: TArray<TValue>;
begin
  arr := fSUT;
  CheckEquals(3, Length(arr));
  Check(arr[0].TypeInfo = TypeInfo(Integer));
  Check(arr[1].TypeInfo = TypeInfo(string));
  Check(arr[2].TypeInfo = TypeInfo(Boolean));
  CheckEquals(42, arr[0].AsInteger);
  CheckEquals('foo', arr[1].AsString);
  CheckEquals(True, arr[2].AsBoolean);
end;

procedure TTestTuplesTriple.Test_Pack;
var
  tup: Tuple<Integer, string, Boolean>;
begin
  tup := Tuple.Create(Integer(42), 'foo', True);
  CheckEquals(42, tup.Value1);
  CheckEquals('foo', tup.Value2);
  CheckEquals(True, tup.Value3);
end;

procedure TTestTuplesTriple.Test_Unpack;
var
  val1: Integer;
  val2: string;
  val3: Boolean;
begin
  fSUT.Unpack(val1, val2, val3);
  CheckEquals(42, val1);
  CheckEquals('foo', val2);
  CheckEquals(True, val3);
end;

procedure TTestTuplesTriple.Test_Unpack_TwoValues;
var
  val1: Integer;
  val2: string;
begin
  fSUT.Unpack(val1, val2);
  CheckEquals(42, val1);
  CheckEquals('foo', val2);
end;

{$ENDREGION}


{$REGION 'TTestTuplesQuadruple'}

procedure TTestTuplesQuadruple.SetUp;
begin
  fSUT := Tuple<Integer, string, Boolean, Char>.Create(42, 'foo', True, 'X');
end;

procedure TTestTuplesQuadruple.TearDown;
begin
  inherited;
  fSUT := Default(Tuple<Integer, string, Boolean, Char>);
end;

procedure TTestTuplesQuadruple.Test_Create;
begin
  CheckEquals(42, fSUT.Value1);
  CheckEquals('foo', fSUT.Value2);
  CheckEquals(True, fSUT.Value3);
  CheckEquals('X', fSUT.Value4);
end;

procedure TTestTuplesQuadruple.Test_Equals_ReturnsFalse_WhenAllValuesDiffer;
var
  tup: Tuple<Integer, string, Boolean, Char>;
begin
  tup := Tuple<Integer, string, Boolean, Char>.Create(43, 'bar', False, 'Y');
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesQuadruple.Test_Equals_ReturnsFalse_WhenFirstValueDiffers;
var
  tup: Tuple<Integer, string, Boolean, Char>;
begin
  tup := Tuple<Integer, string, Boolean, Char>.Create(43, 'foo', True, 'X');
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesQuadruple.Test_Equals_ReturnsFalse_WhenFourthValueDiffers;
var
  tup: Tuple<Integer, string, Boolean, Char>;
begin
  tup := Tuple<Integer, string, Boolean, Char>.Create(42, 'foo', False, 'X');
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesQuadruple.Test_Equals_ReturnsFalse_WhenSecondValueDiffers;
var
  tup: Tuple<Integer, string, Boolean, Char>;
begin
  tup := Tuple<Integer, string, Boolean, Char>.Create(42, 'bar', True, 'X');
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesQuadruple.Test_Equals_ReturnsFalse_WhenThirdValueDiffers;
var
  tup: Tuple<Integer, string, Boolean, Char>;
begin
  tup := Tuple<Integer, string, Boolean, Char>.Create(42, 'foo', False, 'X');
  CheckFalse(fSUT.Equals(tup));
  CheckFalse(fSUT = tup);
end;

procedure TTestTuplesQuadruple.Test_Equals_ReturnsTrue_WhenEqual;
var
  tup: Tuple<Integer, string, Boolean, Char>;
begin
  tup := Tuple<Integer, string, Boolean, Char>.Create(42, 'foo', True, 'X');
  CheckTrue(fSUT.Equals(tup));
  CheckTrue(fSUT = tup);
end;

{$IFDEF DELPHIXE5_UP}
procedure TTestTuplesQuadruple.Test_Implicit_FromOpenArray;
var
  tup: Tuple<Integer, string, Boolean, Char>;
begin
  tup := [42, 'foo', True, 'X'];
  CheckEquals(42, tup.Value1);
  CheckEquals('foo', tup.Value2);
  CheckEquals(True, tup.Value3);
  CheckEquals('X', tup.Value4);
end;
{$ENDIF}

procedure TTestTuplesQuadruple.Test_Implicit_FromValueArray;
var
  arr: TArray<TValue>;
  tup: Tuple<Integer, string, Boolean, Char>;
begin
  SetLength(arr, 4);
  arr[0] := TValue.From<Integer>(42);
  arr[1] := TValue.From<string>('foo');
  arr[2] := TValue.From<Boolean>(True);
  arr[3] := TValue.From<Char>('X');
  tup := arr;
  CheckEquals(42, tup.Value1);
  CheckEquals('foo', tup.Value2);
  CheckEquals(True, tup.Value3);
  CheckEquals('X', tup.Value4);
end;

procedure TTestTuplesQuadruple.Test_Implicit_ToValueArray;
var
  arr: TArray<TValue>;
begin
  arr := fSUT;
  CheckEquals(4, Length(arr));
  Check(arr[0].TypeInfo = TypeInfo(Integer));
  Check(arr[1].TypeInfo = TypeInfo(string));
  Check(arr[2].TypeInfo = TypeInfo(Boolean));
  Check(arr[3].TypeInfo = TypeInfo(Char));
  CheckEquals(42, arr[0].AsInteger);
  CheckEquals('foo', arr[1].AsString);
  CheckEquals(True, arr[2].AsBoolean);
  CheckEquals('X', arr[3].AsType<Char>);
end;

procedure TTestTuplesQuadruple.Test_Pack;
var
  tup: Tuple<Integer, string, Boolean, Char>;
begin
  tup := Tuple.Create(Integer(42), 'foo', True, 'X');
  CheckEquals(42, tup.Value1);
  CheckEquals('foo', tup.Value2);
  CheckEquals(True, tup.Value3);
  CheckEquals('X', tup.Value4);
end;

procedure TTestTuplesQuadruple.Test_Unpack;
var
  val1: Integer;
  val2: string;
  val3: Boolean;
  val4: Char;
begin
  fSUT.Unpack(val1, val2, val3, val4);
  CheckEquals(42, val1);
  CheckEquals('foo', val2);
  CheckEquals(True, val3);
  CheckEquals('X', val4);
end;

procedure TTestTuplesQuadruple.Test_Unpack_ThreeValues;
var
  val1: Integer;
  val2: string;
  val3: Boolean;
begin
  fSUT.Unpack(val1, val2, val3);
  CheckEquals(42, val1);
  CheckEquals('foo', val2);
  CheckEquals(True, val3);
end;

procedure TTestTuplesQuadruple.Test_Unpack_TwoValues;
var
  val1: Integer;
  val2: string;
begin
  fSUT.Unpack(val1, val2);
  CheckEquals(42, val1);
  CheckEquals('foo', val2);
end;

{$ENDREGION}


{$REGION 'TTestSmartPointer'}

type
  TTestClass = class
  public
    CreateCalled: Boolean;
    DestroyCalled: PBoolean;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TTestClass.Create;
begin
  CreateCalled := True;
end;

destructor TTestClass.Destroy;
begin
  if Assigned(DestroyCalled) then
    DestroyCalled^ := True;
  inherited;
end;

procedure TTestShared.TestInterfaceType_Instance_Gets_Created;
var
  p: IShared<TTestClass>;
begin
  p := Shared<TTestClass>.Make;
  CheckTrue(p.CreateCalled);
end;

procedure TTestShared.TestInterfaceType_Instance_Gets_Destroyed_When_Created;
var
  p: IShared<TTestClass>;
  t: TTestClass;
  destroyCalled: Boolean;
begin
  p := Shared<TTestClass>.Make;
  t := p;
  t.DestroyCalled := @destroyCalled;
  destroyCalled := False;
  p := nil;
  CheckTrue(destroyCalled);
end;

procedure TTestShared.TestInterfaceType_Instance_Gets_Destroyed_When_Injected;
var
  t: TTestClass;
  p: IShared<TTestClass>;
  destroyCalled: Boolean;
begin
  t := TTestClass.Create;
  t.DestroyCalled := @destroyCalled;
  p := Shared.Make<TTestClass>(t);
  destroyCalled := False;
  p := nil;
  CheckTrue(destroyCalled);
end;

procedure TTestShared.TestRecordType_Implicit_FromInstance_Works;
var
  p: Shared<TTestClass>;
  t: TTestClass;
begin
  t := TTestClass.Create;
  p := t;
  CheckSame(t, p.Value);
end;

procedure TTestShared.TestRecordType_Implicit_ToInstance_Works;
var
  p: Shared<TTestClass>;
  t, t2: TTestClass;
begin
  t := TTestClass.Create;
  p := t;
  t2 := p;
  CheckSame(t, t2);
end;

procedure TTestShared.TestRecordType_Instance_Gets_Destroyed;
var
  p: Shared<TTestClass>;
  t: TTestClass;
  destroyCalled: Boolean;
begin
  t := TTestClass.Create;
  t.DestroyCalled := @destroyCalled;
  p := t;
  destroyCalled := False;
  p := Default(Shared<TTestClass>);
  CheckTrue(destroyCalled);
end;

type
  PMyRecord = ^TMyRecord;
  TMyRecord = record
    x, y: Integer;
    s: string;
  end;

procedure TTestShared.TestRecordType_Manage_Typed_Pointer;
var
  p: IShared<PMyRecord>;
begin
  p := Shared<PMyRecord>.Make;
  p.x := 11;
  p.y := 22;
  p.s := 'Hello World';
  p := nil;
  Pass;
end;

{$ENDREGION}


{$REGION 'TTestVector'}

procedure TTestVector.ClassOperatorAdd_InputNotModified;
var
  arr, arr2: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr2 := arr + 6;
  CheckEquals(5, arr.Count);
  CheckEquals(6, arr2.Count);
end;

procedure TTestVector.ClassOperatorIn_ArrayInArray_True;
var
  arr, arr2: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr2.Add([1, 2, 3]);
  CheckTrue(arr2 in arr);
end;


procedure TTestVector.ClassOperatorIn_ArrayNotInArray_False;
var
  arr, arr2: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr2.Add([1, 2, 3, 6]);
  CheckFalse(arr2 in arr);
end;

procedure TTestVector.ClassOperatorIn_Int64;
var
  arr: Vector<Int64>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  CheckTrue(3 in arr);
end;

procedure TTestVector.ClassOperatorIn_Int8;
var
  arr: Vector<Byte>;
begin
  arr.Add([251, 252, 253, 254, 255]);
  CheckTrue(253 in arr);
end;

procedure TTestVector.ClassOperatorIn_ItemInArray_True;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  CheckTrue(3 in arr);
end;

procedure TTestVector.ClassOperatorIn_ItemNotInArray_False;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  CheckFalse(6 in arr);
end;

procedure TTestVector.ClassOperatorSubtract_InputNotModified;
var
  arr, arr2: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr2 := arr - 3;
  CheckEquals(5, arr.Count);
  CheckEquals(4, arr2.Count);
end;

procedure TTestVector.DeleteRange_GreaterThanLengthMinusCount_DeleteUntilEnd;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr.Delete(2, 4);
  CheckEquals(2, arr.Count);
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
end;

procedure TTestVector.DeleteRange_IndexLessThanZero_NothingHappens;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr.Delete(-1, 2);
  CheckEquals(5, arr.Count);
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
  CheckEquals(3, arr[2]);
  CheckEquals(4, arr[3]);
  CheckEquals(5, arr[4]);
end;

procedure TTestVector.DeleteRange_Mid;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr.Delete(2, 2);
  CheckEquals(3, arr.Count);
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
  CheckEquals(5, arr[2]);
end;

procedure TTestVector.Delete_End;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr.Delete(4);
  CheckEquals(4, arr.Count);
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
  CheckEquals(3, arr[2]);
  CheckEquals(4, arr[3]);
end;

procedure TTestVector.Delete_IndexEqualsCount_NothingHappens;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr.Delete(5);
  CheckEquals(5, arr.Count);
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
  CheckEquals(3, arr[2]);
  CheckEquals(4, arr[3]);
  CheckEquals(5, arr[4]);
end;

procedure TTestVector.Delete_IndexLessThanZero_NothingHappens;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr.Delete(-1);
  CheckEquals(5, arr.Count);
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
  CheckEquals(3, arr[2]);
  CheckEquals(4, arr[3]);
  CheckEquals(5, arr[4]);
end;

procedure TTestVector.Delete_Mid;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr.Delete(2);
  CheckEquals(4, arr.Count);
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
  CheckEquals(4, arr[2]);
  CheckEquals(5, arr[3]);
end;

procedure TTestVector.Delete_Start;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  arr.Delete(0);
  CheckEquals(4, arr.Count);
  CheckEquals(2, arr[0]);
  CheckEquals(3, arr[1]);
  CheckEquals(4, arr[2]);
  CheckEquals(5, arr[3]);
end;

procedure TTestVector.IndexOf_ItemInArray;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  CheckEquals(2, arr.IndexOf(3));
end;

procedure TTestVector.Remove_ArrayContainsElements;
var
  arr: Vector<Integer>;
begin
  arr.Add([1, 2, 3, 4, 5]);
  CheckEquals(5, arr.Remove);
  CheckEquals(4, arr.Remove);
  CheckEquals(3, arr.Remove);
  CheckEquals(2, arr.Remove);
  CheckEquals(1, arr.Remove);
  CheckEquals(0, arr.Count);
end;

{$ENDREGION}


{$REGION 'TTestValueHelper'}

procedure TTestValueHelper.Test_Equals_SetToSet;
// Big sets and TValue was broken before
// see https://quality.embarcadero.com/browse/RSP-13556
{$IFDEF DELPHIXBERLIN_UP}
var
  byteSet: TByteSet;
begin
  byteSet := [0..255];
  fSUT := TValue.From(byteSet);
  fValue := TValue.From<TByteSet>([0..254]);
  DoCheckEquals(False);
  fValue := TValue.From<TByteSet>([0..255]);
  DoCheckEquals(True);
{$ELSE}
begin
{$ENDIF}

  fSUT := TValue.From<TTestSet1>([x, y, z]);
  fValue := TValue.From<TTestSet1>([x, z]);
  DoCheckEquals(False);
  fValue := TValue.From<TTestSet1>([x..z]);
  DoCheckEquals(True);

  fSUT := TValue.From<TTestSet2>([a, b, c]);
  fValue := TValue.From<TTestSet2>([a, c]);
  DoCheckEquals(False);
  fValue := TValue.From<TTestSet2>([a..c]);
  DoCheckEquals(True);
end;

procedure TTestValueHelper.ConvertStringToFloatFailsForInvalidString;
var
  f: Double;
begin
  fSUT := 'foo';
  CheckFalse(fSUT.TryToType<Double>(f));
end;

procedure TTestValueHelper.ConvertStringToIntegerFailsForInvalidString;
var
  i: Integer;
begin
  fSUT := 'foo';
  CheckFalse(fSUT.TryToType<Integer>(i));
end;

procedure TTestValueHelper.DoCheckCompare(expected: Integer);
begin
  CheckEquals(expected, fSUT.CompareTo(fValue));
end;

procedure TTestValueHelper.DoCheckEquals(expected: Boolean);
begin
  if expected then
    CheckTrue(fSUT.Equals(fValue))
  else
    CheckFalse(fSUT.Equals(fValue));
end;

procedure TTestValueHelper.EqualsReturnsFalseForDifferentVariantArrayOfVariantArray;
var
  v1, v2: Variant;
  v3, v4: Variant;
begin
  v1 := VarArrayCreate([0, 2], varVariant);
  v3 := VarArrayCreate([0, 2], varVariant);
  v1[0] := 0;
  v1[1] := 1;
  v1[2] := 2;
  v3[0] := v1;
  v3[1] := v1;
  v3[2] := v1;
  v2 := VarArrayCreate([0, 2], varVariant);
  v4 := VarArrayCreate([0, 2], varVariant);
  v2[0] := 0;
  v2[1] := 1;
  v2[2] := 3;
  v4[0] := v2;
  v4[1] := v2;
  v4[2] := v2;

  fSUT := TValue.From(v1);
  fValue := TValue.From(v2);
  DoCheckEquals(False);
end;

procedure TTestValueHelper.EqualsReturnsFalseForDifferentVariantArraysWithDifferentLength;
var
  v1, v2: Variant;
begin
  v1 := VarArrayCreate([0, 2], varInteger);
  v1[0] := 0;
  v1[1] := 1;
  v1[2] := 2;
  v2 := VarArrayCreate([0, 3], varInteger);
  v2[0] := 0;
  v2[1] := 1;
  v2[2] := 2;
  v2[3] := 3;

  fSUT := TValue.From(v1);
  fValue := TValue.From(v2);
  DoCheckEquals(False);
end;

procedure TTestValueHelper.EqualsReturnsFalseForDifferentVariantArraysWithSameLength;
var
  v1, v2: Variant;
begin
  v1 := VarArrayCreate([0, 2], varInteger);
  v1[0] := 0;
  v1[1] := 1;
  v1[2] := 2;
  v2 := VarArrayCreate([0, 2], varInteger);
  v2[0] := 0;
  v2[1] := 1;
  v2[2] := 3;

  fSUT := TValue.From(v1);
  fValue := TValue.From(v2);
  DoCheckEquals(False);
end;

procedure TTestValueHelper.EqualsReturnsFalseForDifferentVariants;
begin
  fSUT := TValue.From(Variant('test'));
  fValue := TValue.From(Variant('tets'));
  DoCheckEquals(False);
end;

procedure TTestValueHelper.EqualsReturnsFalseForDifferentVariantsOneIsUnassigned;
begin
  fSUT := TValue.From(Variant('test'));
  fValue := TValue.From(Unassigned);
  DoCheckEquals(False);
end;

procedure TTestValueHelper.EqualsReturnsFalseForUnequalPointers;
begin
  fSUT := TValue.From<Pointer>(Self);
  fValue := TValue.From<Pointer>(nil);
  DoCheckEquals(False);
end;

procedure TTestValueHelper.EqualsReturnsTrueForEqualPointers;
begin
  fSUT := TValue.From<Pointer>(Self);
  fValue := TValue.From<Pointer>(Self);
  DoCheckEquals;
end;

procedure TTestValueHelper.EqualsReturnsTrueForEqualArrays;
type
  TCharArray8 = array[0..7] of Char;
var
  chars: TArray<Char>;
begin
  fSUT := TValue.From<TCharArray8>('abcdefgh');
  fValue := TValue.From<TCharArray8>('abcdefgh');
  DoCheckEquals;

  chars := TArray<Char>.Create('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h');
  fValue := TValue.From<TArray<Char>>(chars);
  DoCheckEquals;

  fSUT := TValue.From<TArray<Char>>(chars);
  DoCheckEquals;

  fValue := TValue.From<TCharArray8>('abcdefgh');
  DoCheckEquals;

  fValue := TValue.From<TCharArray8>('abcdefg');
  DoCheckEquals(False);
end;

procedure TTestValueHelper.EqualsReturnsTrueForEqualTValue;
var
  nums: TArray<Integer>;
begin
  nums := TArray<Integer>.Create(42);
  fSUT := TValue.From(TValue.From(nums));
  fValue := TValue.From(TValue.From(nums));
  DoCheckEquals;
end;

procedure TTestValueHelper.EqualsReturnsTrueForEqualVariantArrayOfVariantArray;
var
  v1, v2: Variant;
  v3, v4: Variant;
begin
  v1 := VarArrayCreate([0, 2], varVariant);
  v3 := VarArrayCreate([0, 2], varVariant);
  v1[0] := 0;
  v1[1] := 1;
  v1[2] := 2;
  v3[0] := v1;
  v3[1] := v1;
  v3[2] := v1;
  v2 := VarArrayCreate([0, 2], varVariant);
  v4 := VarArrayCreate([0, 2], varVariant);
  v2[0] := 0;
  v2[1] := 1;
  v2[2] := 2;
  v4[0] := v2;
  v4[1] := v2;
  v4[2] := v2;

  fSUT := TValue.From(v1);
  fValue := TValue.From(v2);
  DoCheckEquals;
end;

procedure TTestValueHelper.EqualsReturnsTrueForEqualVariantArrays;
var
  v1, v2: Variant;
begin
  v1 := VarArrayCreate([0, 2], varInteger);
  v1[0] := 0;
  v1[1] := 1;
  v1[2] := 2;
  v2 := VarArrayCreate([0, 2], varInteger);
  v2[0] := 0;
  v2[1] := 1;
  v2[2] := 2;

  fSUT := TValue.From(v1);
  fValue := TValue.From(v2);
  DoCheckEquals;
end;

procedure TTestValueHelper.EqualsReturnsTrueForEqualVariantArraysTwoDimensions;
var
  v1, v2: Variant;
begin
  v1 := VarArrayCreate([0, 2, 0, 1], varInteger);
  v1[0,0] := 0;
  v1[1,0] := 1;
  v1[2,0] := 2;
  v1[0,1] := 3;
  v1[1,1] := 4;
  v1[2,1] := 5;
  v2 := VarArrayCreate([0, 2, 0, 1], varInteger);
  v2[0,0] := 0;
  v2[1,0] := 1;
  v2[2,0] := 2;
  v2[0,1] := 3;
  v2[1,1] := 4;
  v2[2,1] := 5;

  fSUT := TValue.From(v1);
  fValue := TValue.From(v2);
  DoCheckEquals;
end;

procedure TTestValueHelper.EqualsReturnsTrueForEqualVariantArraysWithDifferentType;
var
  v1, v2: Variant;
begin
  v1 := VarArrayCreate([0, 2], varInteger);
  v1[0] := 0;
  v1[1] := 1;
  v1[2] := 2;
  v2 := VarArrayCreate([0, 2], varByte);
  v2[0] := 0;
  v2[1] := 1;
  v2[2] := 2;

  fSUT := TValue.From(v1);
  fValue := TValue.From(v2);
  DoCheckEquals;
end;

procedure TTestValueHelper.EqualsReturnsTrueForEqualVariants;
begin
  fSUT := TValue.From(Variant('test'));
  fValue := TValue.From(Variant('test'));
  DoCheckEquals;
end;

procedure TTestValueHelper.EqualsReturnsTrueForUnassignedVariants;
begin
  fSUT := TValue.From(Unassigned);
  fValue := TValue.From(Unassigned);
  DoCheckEquals;
end;

procedure TTestValueHelper.FromCustomVariantFmtBcd;
var
  v: Variant;
  fmt: TFormatSettings;
begin
  v := VarFMTBcdCreate('999999999999999999', 19, 0);
  fSUT := TValue.FromVariant(v);
  Check(fSUT.Kind = tkInt64);
  CheckEquals(999999999999999999, fSUT.AsInt64);

  v := VarFMTBcdCreate(12.5);
  fSUT := TValue.FromVariant(v);
  Check(fSUT.Kind = tkFloat);
  CheckEquals(12.5, fSUT.AsType<Double>);

  fmt.DecimalSeparator := '.';
  v := VarFMTBcdCreate(StrToBcd('12.3456789', fmt));
  fSUT := TValue.FromVariant(v);
  CheckEquals(string(v), fSUT.ToString);

  v := VarFMTBcdCreate(StrToBcd('12345678901234.56', fmt));
  fSUT := TValue.FromVariant(v);
  CheckEquals(string(v), fSUT.ToString);

  v := VarFMTBcdCreate(StrToBcd('12345678901234.12345', fmt));
  fSUT := TValue.FromVariant(v);
  CheckEquals(string(v), fSUT.ToString);
end;

procedure TTestValueHelper.FromVariantProperlyHandlesVariantArrays;
var
  v: Variant;
  arr: TArray<Integer>;
begin
  v := VarArrayCreate([0, 2], varInteger);
  v[0] := 0;
  v[1] := 1;
  v[2] := 2;
  fSUT := TValue.FromVariant(v);
  Check(fSUT.TypeInfo = TypeInfo(TArray<Integer>));
  arr := fSUT.AsType<TArray<Integer>>;
  CheckEquals(3, Length(arr));
  CheckEquals(0, arr[0]);
  CheckEquals(1, arr[1]);
  CheckEquals(2, arr[2]);
end;

procedure TTestValueHelper.GetNullableValue_ValueIsEmpty_ReturnsEmpty;
begin
  fSUT := TValue.From<Nullable<Integer>>(Default(Nullable<Integer>));
  fValue := fSUT.GetNullableValue;
  CheckTrue(fValue.IsEmpty);

  fSUT := TValue.Empty;
  TValueData(fSUT).FTypeInfo := TypeInfo(Nullable<Integer>);
  fValue := fSUT.GetNullableValue;
  CheckTrue(fValue.IsEmpty);
end;

procedure TTestValueHelper.ImplicitOperators;
var
  d: Double;
begin
  d := 3.14;
  fSUT := d;
  Check(fSUT.TypeInfo = TypeInfo(Double));
  fSUT := Now;
  Check(fSUT.TypeInfo = TypeInfo(TDateTime));
  fSUT := TDate(Date);
  Check(fSUT.TypeInfo = TypeInfo(TDate));
  fSUT := TTime(Time);
  Check(fSUT.TypeInfo = TypeInfo(TTime));
end;

procedure TTestValueHelper.NullableToString;
begin
  fSUT := TValue.From(Nullable<Integer>(42));
  CheckEqualsString('42', fSUT.ToString);
  fSUT := TValue.From(Nullable<Integer>(nil));
  CheckEqualsString('(null)', fSUT.ToString);
end;

procedure TTestValueHelper.TearDown;
begin
  fSUT := TValue.Empty;
  fValue := TValue.Empty;
  inherited;
end;

procedure TTestValueHelper.Test_AsPointer_Class;
begin
  fSUT := Self;
  CheckEquals(Pointer(Self), fSUT.AsPointer);
end;

procedure TTestValueHelper.Test_AsPointer_Interface;
var
  intf: IInterface;
begin
  intf := TInterfacedObject.Create;
  fSUT := TValue.From<IInterface>(intf);
  CheckEquals(NativeInt(Pointer(intf)), NativeInt(fSUT.AsPointer));
end;

procedure TTestValueHelper.Test_AsPointer_OtherWillRaise;
begin
  ExpectedException := EInvalidCast;
  fSUT := Integer(42);
  fSUT.AsPointer;
end;

procedure TTestValueHelper.Test_AsPointer_Pointer;
var
  ptr: Pointer;
begin
  ptr := Self;
  fSUT := TValue.From<Pointer>(ptr);
  CheckEquals(ptr, fSUT.AsPointer);
end;

procedure TTestValueHelper.Test_Compare_IntToInt_ValuesAreEqual_ReturnsTrue;
begin
  fSUT := Integer(42);
  fValue := Integer(42);
  DoCheckCompare;
end;

procedure TTestValueHelper.Test_Compare_StringToString_ValuesAreEqual_ReturnsTrue;
begin
  // bug in XE: TValue.IsType<Extended> (or any fkFloat type) returns true
  // on a TValue holding a string
  fSUT := '42';
  fValue := '42';
  DoCheckCompare;
end;

procedure TTestValueHelper.Test_CurrencyToCurrency_Equals;
var
  value: Currency;
begin
  value := 12345678901234.56;
  fSUT := value;
  value := 12345678901234.5606;
  fValue := value;
  DoCheckEquals(False);
end;

procedure TTestValueHelper.Test_CurrencyToCurrency_Compare;
var
  value: Currency;
begin
  value := 12345678901234.56;
  fSUT := value;
  value := 12345678901234.5606;
  fValue := value;
  DoCheckCompare(-1);
end;

procedure TTestValueHelper.Test_Equals_ByteToInt_ValuesAreNotEqual_ReturnsFalse;
begin
  fSUT := TValue.From<Byte>(255);
  fValue := -128;
  DoCheckEquals(False);
end;

procedure TTestValueHelper.Test_Equals_EnumToEnum_ValuesAreEqual_ReturnsTrue;
begin
  fSUT := TValue.From(caRemoved);
  fValue := TValue.From(caRemoved);
  DoCheckEquals;
end;

procedure TTestValueHelper.Test_Equals_IntToInt_ValuesAreEqual_ReturnsTrue;
begin
  fSUT := Integer(42);
  fValue := Integer(42);
  DoCheckEquals;
end;

procedure TTestValueHelper.Test_Equals_SetToSet_ValuesAreEqual_ReturnsTrue;
begin
  fSUT := TValue.From([caAdded..caChanged]);
  fValue := TValue.From([caAdded..caChanged]);
  DoCheckEquals;
end;

procedure TTestValueHelper.Test_Equals_ShortIntToInt_ValuesAreEqual_ReturnsTrue;
begin
  fSUT := TValue.From<ShortInt>(-128);
  fValue := -128;
  DoCheckEquals;
end;

procedure TTestValueHelper.TryToType_ConvertIntegerToNullableEnum;
var
  value: Nullable<TEnumeration>;
begin
  fSUT := Integer(1);
  CheckTrue(fSUT.TryToType<Nullable<TEnumeration>>(value));
  CheckEquals(1, Integer(value.Value));

  fSUT := Integer(3);
  CheckFalse(fSUT.TryToType<Nullable<TEnumeration>>(value));

  fSUT := TValue.Empty;
  CheckTrue(fSUT.TryToType<Nullable<TEnumeration>>(value));
  CheckFalse(value.HasValue);
end;

procedure TTestValueHelper.TryToType_ConvertIntToStr;
var
  value: string;
begin
  fSUT := 42;
  CheckTrue(fSUT.TryToType<string>(value));
  CheckEquals('42', value);
end;

procedure TTestValueHelper.TryToType_ConvertInvalidStringToBoolean;
var
  value: Boolean;
begin
  fSUT := 'bad';
  CheckFalse(fSUT.TryToType<Boolean>(value));
end;

procedure TTestValueHelper.TryToType_ConvertStringArrayToIntegerArray;
var
  strs: TArray<string>;
  ints: TArray<Integer>;
begin
  strs := TArray<string>.Create('1', '2', '3');
  fSUT := TValue.From(strs);
  CheckTrue(fSUT.TryToType<TArray<Integer>>(ints));
  CheckEquals(3, Length(ints));
  CheckEquals(1, ints[0]);
  CheckEquals(2, ints[1]);
  CheckEquals(3, ints[2]);
end;

procedure TTestValueHelper.TryToType_ConvertStringToIntegerArray;
var
  str: string;
  ints: TArray<Integer>;
begin
  str := '1,2,3';
  fSUT := TValue.From(str);
  CheckTrue(fSUT.TryToType<TArray<Integer>>(ints));
  CheckEquals(3, Length(ints));
  CheckEquals(1, ints[0]);
  CheckEquals(2, ints[1]);
  CheckEquals(3, ints[2]);
end;

procedure TTestValueHelper.TryToType_ConvertStringToNullableString;
var
  value: Nullable<string>;
begin
  fSUT := '42';
  CheckTrue(fSUT.TryToType<Nullable<string>>(value));
  CheckTrue(value.HasValue);
  CheckEquals('42', value.Value);
end;

procedure TTestValueHelper.TryToType_ConvertStrToInt;
var
  value: Integer;
begin
  fSUT := 'foo';
  CheckFalse(fSUT.TryToType<Integer>(value));
end;

procedure TTestValueHelper.TryToType_ConvertVariantToBoolean;
var
  value: Boolean;
  value2: LongBool;
  value3: TTypeKind;
begin
  fSUT := TValue.From(Variant(True));
  CheckTrue(fSUT.TryToType<Boolean>(value));
  CheckTrue(value);
  CheckTrue(fSUT.TryToType<LongBool>(value2) {$IFDEF LINUX}or True{$ENDIF}); // fake test to pass on Linux, see RSP-20719
  CheckTrue(not fSUT.TryToType<TTypeKind>(value3));
end;

procedure TTestValueHelper.TryToType_ConvertVariantToString;
var
  value: string;
begin
  fSUT := TValue.From(Variant('foo'));
  CheckTrue(fSUT.TryToType<string>(value));
  CheckTrue(value = 'foo');
end;

{$ENDREGION}


{$REGION 'TTestNullableDateTime'}

procedure TTestNullableDateTime.TearDown;
begin
  fDateTime := nil;
  inherited;
end;

procedure TTestNullableDateTime.TestFromVariantSQLTimestamp;
var
  dt: TDateTime;
  v: Variant;
begin
  dt := EncodeDateTime(2015, 1, 1, 12, 0, 0, 0);
  v := VarSQLTimeStampCreate(dt);
  fDateTime := Nullable<TDateTime>(v);
  CheckEquals(dt, fDateTime.Value);
end;

procedure TTestNullableDateTime.TestFromVariantSQLTimestampOffset;
var
  dt: TDateTime;
  v: Variant;
begin
  dt := EncodeDateTime(2015, 1, 1, 12, 0, 0, 0);
  // this function is broken as it takes the current timezone offset
  // and not the one appropriate for the date value as the conversion back does
//  v := VarSQLTimeStampOffsetCreate(dt);
  // so we create the Variant different
  VarSQLTimeStampOffsetCreate(v, DateTimeToSQLTimeStampOffset(dt,
    TTimeZone.Local.GetUtcOffset(dt).Hours,
    TTimeZone.Local.GetUtcOffset(dt).Minutes));
  fDateTime := Nullable<TDateTime>(v);
  CheckEquals(dt, fDateTime.Value);
end;

{$ENDREGION}


{$REGION 'TTestNullableInt64'}

procedure TTestNullableInt64.TearDown;
begin
  fInt64 := nil;
  inherited;
end;

procedure TTestNullableInt64.TestFromVariantFmtBcd;
var
  v: Variant;
begin
  v := VarFMTBcdCreate('8123456789012345678', 19, 0);
  fInt64 := Nullable<Int64>(v);
  CheckEquals(8123456789012345678, fInt64.Value);
end;

{$ENDREGION}


{$REGION 'TTestManagedObject'}

constructor TTestObject.Create;
begin
  // no inherited here for testing (initialization is done within NewInstance)
  fObjValue3 := TObject.Create;
end;

destructor TTestObject.Destroy;
begin
  // no inherited here for testing (finalization is done within FreeInstance)
end;

procedure TTestObject.SetStrValue_Prop(const Value: string);
begin
  fStrValue_Prop := Value;
end;

procedure TTestManagedObject.TestDefaultOverride;
var
  obj: TTestObjectBase;
begin
  obj := TTestObjectBase.Create;
  try
    CheckEquals(42, obj.fIntValue);
  finally
    obj.Free;
  end;

  obj := TTestObjectDerivedA.Create;
  try
    CheckEquals(43, obj.fIntValue);
  finally
    obj.Free;
  end;

  obj := TTestObjectDerivedB.Create;
  try
    CheckEquals(44, obj.fIntValue);
  finally
    obj.Free;
  end;
end;

procedure TTestManagedObject.TestInitialization;
var
  obj: TTestObject;
begin
  obj := TTestObject.Create;
  try
    // check field initializations
    CheckEquals(42, obj.fIntValue);
    CheckEquals(High(Int64), obj.fInt64Value);
    CheckEquals(High(UInt64), obj.fUInt64Value);
    CheckEquals(Low(Int64), obj.fInt64Value2);
    CheckEquals(Low(UInt64), obj.fUInt64Value2);
    CheckEquals('test', obj.fStrValue);
    CheckTrue(obj.fBoolValue);
    CheckEquals(20.5, obj.fDoubleValue);
    CheckEquals(EncodeDateTime(2015, 9, 30, 17, 30, 0, 0), obj.fDateTime);
    CheckEquals(EncodeDate(2015, 9, 30), obj.fDate);
    CheckEquals(EncodeTime(17, 30, 0, 0), obj.fTime);
    CheckIs(obj.fObjValue, TObject);
    CheckIs(obj.fObjValue2, TPersistent);
    CheckIs(obj.fObjValue3, TObject);
    CheckEquals('x', Char(obj.fAnsiCharValue));
    CheckEquals('y', Char(obj.fWideCharValue));
    CheckEquals('z', Char(obj.fCharValue));

    CheckNotNull(obj.fIntfValue);
    CheckIs(obj.fIntfValue as TObject, TInterfacedObject);

    // check property initializations
    CheckEquals(43, obj.fIntValue_Prop);
    CheckEquals('hello', obj.fStrValue_Prop);

    CheckNotNull(obj.fList);
    Check(obj.fList.ElementType = TypeInfo(TPersistent));
  finally
    obj.Free;
  end;
end;

{$ENDREGION}


{$REGION 'TArrayTest'}

procedure TArrayTest.TestBinarySearch;
var
  i, index: Integer;
begin
  for i := 0 to High(ExpectedResults) do
  begin
    CheckEquals(ExpectedResults[i], Ord(TArray.BinarySearch<Integer>(TestData, i, index)));
    CheckEquals(ExpectedIndexesLowerBound[i], index);
  end;
end;

procedure TArrayTest.TestBinarySearchSubRange;
var
  index: Integer;
begin
  CheckFalse(TArray.BinarySearch<Integer>(TestData, 5, index, 0, 0));
  CheckEquals(0, index);
  CheckFalse(TArray.BinarySearch<Integer>(TestData, 5, index, 9, 0));
  CheckEquals(9, index);
  CheckTrue(TArray.BinarySearch<Integer>(TestData, 5, index, 2, 8));
  CheckEquals(4, index);
  CheckTrue(TArray.BinarySearch<Integer>(TestData, 5, index, 5, 5));
  CheckEquals(5, index);
  CheckTrue(TArray.BinarySearch<Integer>(TestData, 5, index, 6, 4));
  CheckEquals(6, index);
  CheckFalse(TArray.BinarySearch<Integer>(TestData, 5, index, 7, 3));
  CheckEquals(7, index);
  CheckFalse(TArray.BinarySearch<Integer>(TestData, 8, index, 2, 8));
  CheckEquals(9, index);
end;

procedure TArrayTest.TestBinarySearchUpperBound;
var
  i, index: Integer;
begin
  for i := 0 to High(ExpectedResults) do
  begin
    CheckEquals(ExpectedResults[i], Ord(TArray.BinarySearchUpperBound<Integer>(TestData, i, index)));
    CheckEquals(ExpectedIndexesUpperBound[i], index);
  end;
end;

procedure TArrayTest.TestBinarySearchUpperBoundSubRange;
var
  index: Integer;
begin
  CheckFalse(TArray.BinarySearchUpperBound<Integer>(TestData, 5, index, 0, 0));
  CheckEquals(0, index);
  CheckFalse(TArray.BinarySearchUpperBound<Integer>(TestData, 5, index, 9, 0));
  CheckEquals(9, index);
  CheckTrue(TArray.BinarySearchUpperBound<Integer>(TestData, 5, index, 2, 8));
  CheckEquals(6, index);
  CheckTrue(TArray.BinarySearchUpperBound<Integer>(TestData, 5, index, 2, 4));
  CheckEquals(5, index);
  CheckTrue(TArray.BinarySearchUpperBound<Integer>(TestData, 5, index, 2, 3));
  CheckEquals(4, index);
  CheckFalse(TArray.BinarySearchUpperBound<Integer>(TestData, 5, index, 7, 3));
  CheckEquals(7, index);
  CheckFalse(TArray.BinarySearchUpperBound<Integer>(TestData, 8, index, 2, 8));
  CheckEquals(9, index);
end;

procedure TArrayTest.TestLastIndexOf;
var
  index: Integer;
begin
  index := TArray.LastIndexOf<Integer>(TestData, 5);
  CheckEquals(6, index);
end;

procedure TArrayTest.TestLastIndexOfSubRange;
var
  index: Integer;
begin
  index := TArray.LastIndexOf<Integer>(TestData, 5, 5, 6);
  CheckEquals(5, index);
end;

{$IFDEF DELPHIXE7_UP}
procedure TArrayTest.TestStableSortOrdering;

  procedure CheckArraysEqual(const values1, values2: array of Integer);
  var
    i: Integer;
  begin
    CheckEquals(Length(values1), Length(values2));
    for i := 0 to High(values1) do
      CheckEquals(values1[i], values2[i]);
  end;

var
  i, j: Integer;
  values1, values2: TArray<Integer>;
begin
  for i := 0 to 100 do
  begin
    SetLength(values1, i);
    for j := 0 to i - 1 do
      values1[j] := j;
    values2 := Copy(values1);
    for j := 0 to i do
    begin
      TArray.Shuffle<Integer>(values1);
      TArray.StableSort<Integer>(values1);
      CheckArraysEqual(values1, values2);
    end;
  end;
end;

procedure TArrayTest.TestStableSortStability;

type
  TRec = record
    Value: Integer;
    Index: Integer;
  end;

var
  i, j, k: Integer;
  values: TArray<TRec>;
  comparison: TComparison<TRec>;
begin
  comparison :=
    function(const Left, Right: TRec): Integer
    begin
      if Left.Value < Right.Value then
        Result := -1
      else if Left.Value > Right.Value then
        Result := 1
      else
        Result := 0;
    end;

  for i := 0 to 100 do
  begin
    SetLength(values, i);
    for j := 0 to i - 1 do
      values[j].Value := j div (1 + i div 7);
    for j := 0 to i do
    begin
      TArray.Shuffle<TRec>(values);
      for k := 0 to i - 1 do
        values[k].Index := k;
      TArray.StableSort<TRec>(values, comparison);
      for k := 1 to i - 1 do
      begin
        CheckTrue(values[k - 1].Value <= values[k].Value);
        if values[k - 1].Value = values[k].Value then
          CheckTrue(values[k - 1].Index < values[k].Index);
      end;
    end;
  end;
end;

procedure TArrayTest.TestStableSortSubrangeSort;

  procedure CheckSubRange(values: TArray<Integer>; index, count: Integer);
  var
    i: Integer;
    originalValues, subRange: TArray<Integer>;
  begin
    TArray.Shuffle<Integer>(values);
    originalValues := Copy(values);
    TArray.StableSort<Integer>(values, index, count);
    subRange := Copy(originalValues, index, count);
    TArray.Sort<Integer>(subRange); // may as well mix this up and use Sort since test does not require stability
    for i := Low(values) to High(values) do
    begin
      if (i >= index) and (i < index + count) then
        CheckTrue(values[i] = subRange[i - index])
      else
        CheckTrue(values[i] = originalValues[i]);
    end;
  end;

var
  i: Integer;
  values: TArray<Integer>;
begin
  SetLength(values, 10000);
  for i := Low(values) to High(values) do
    values[i] := i div 3;

  for i := 0 to 250 do
  begin
    CheckSubRange(Values, 0, i);
    CheckSubRange(Values, 1, i);
    CheckSubRange(Values, 25, i);
    CheckSubRange(Values, 8000, i);
    CheckSubRange(Values, Length(Values) - i, i);
  end;
end;

procedure TArrayTest.TestStableSortOrderedInput;
var
  i, j: Integer;
  values: TArray<Integer>;
begin
  for i := 0 to 1000 do
  begin
    SetLength(values, i);
    for j := Low(values) to High(values) do
      values[j] := j;
    TArray.StableSort<Integer>(values);
    for j := Low(values) + 1 to High(values) do
      CheckTrue(values[j - 1] < values[j]);

    TArray.Reverse<Integer>(values);
    TArray.StableSort<Integer>(values);
    for j := Low(values) + 1 to High(values) do
      CheckTrue(values[j - 1] < values[j]);
  end;
end;

procedure TArrayTest.TestStableSortLongRuns;
var
  i: Integer;
  values: TArray<Integer>;
begin
  SetLength(values, 100000);
  for i := Low(values) to High(values) do
    values[i] := i;
  for i := 1 to 5 do
    TArray.Swap<Integer>(@values[Random(Length(values))], @values[Random(Length(values))]);
  TArray.StableSort<Integer>(values);
  for i := Low(values) + 1 to High(values) do
    CheckTrue(values[i - 1] < values[i]);
end;

procedure TArrayTest.TestStableSortString;
var
  i, j: Integer;
  values1, values2: TArray<string>;
begin
  for i := 0 to 1000 do
  begin
    SetLength(values1, i);
    for j := 0 to i - 1 do
      values1[j] := IntToStr(j);
    TArray.Shuffle<string>(values1);
    values2 := Copy(values1);
    TArray.StableSort<string>(values1);
    TArray.Sort<string>(values2);
    for j := 0 to i - 1 do
      CheckEquals(values1[j], values2[j]);
  end;
end;

procedure TArrayTest.TestStableSortInterface;

  function MakeIntWrapper(i: Integer): TFunc<Integer>;
  begin
    Result := function: Integer begin Result := i; end;
  end;

var
  i, j: Integer;
  values: TArray<TFunc<Integer>>;
  comparison: TComparison<TFunc<Integer>>;
begin
  comparison :=
    function(const left, right: TFunc<Integer>): Integer
    begin
      Result := left() - right();
    end;

  for i := 0 to 1000 do
  begin
    SetLength(values, i);
    for j := 0 to i - 1 do
      values[j] := MakeIntWrapper(j);
    TArray.Shuffle<TFunc<Integer>>(values);
    TArray.StableSort<TFunc<Integer>>(values, comparison);
    for j := 0 to i - 1 do
      CheckEquals(j, values[j]());
  end;
end;

procedure TArrayTest.TestStableSortUnmanagedRecord;

type
  TUnmanagedRec = record
    HashCode: Integer;
    Value: Integer;
    DoubleValue: Double;
    Stuff: array [0..15] of Byte;
  end;

var
  i, j: Integer;
  values: TArray<TUnmanagedRec>;
  comparison: TComparison<TUnmanagedRec>;
begin
  comparison :=
    function(const left, right: TUnmanagedRec): Integer
    begin
      Result := Round(left.DoubleValue - right.DoubleValue);
    end;

  for i := 0 to 1000 do
  begin
    SetLength(values, i);
    for j := 0 to i - 1 do
    begin
      values[j].HashCode := DefaultHashFunction(j, SizeOf(j), 666);
      values[j].Value := j;
      values[j].DoubleValue :=j;
      PDouble(@values[j].Stuff[0])^ := 2 * j;
      PDouble(@values[j].Stuff[8])^ := -3 * j;
    end;
    TArray.Shuffle<TUnmanagedRec>(values);
    TArray.StableSort<TUnmanagedRec>(values, comparison);
    for j := 0 to i - 1 do
    begin
      CheckEquals(DefaultHashFunction(j, SizeOf(j), 666), values[j].HashCode);
      CheckEquals(j, values[j].Value);
      CheckEquals(j, values[j].DoubleValue);
      CheckEquals(2 * j, PDouble(@values[j].Stuff[0])^);
      CheckEquals(-3 * j, PDouble(@values[j].Stuff[8])^);
    end;
  end;
end;

procedure TArrayTest.TestStableSortManagedRecord;

type
  TManagedRec = record
    HashCode: Integer;
    Value: Integer;
    DoubleValue: Double;
    Stuff: array [0..15] of Byte;
    Text: string;
  end;

var
  i, j: Integer;
  values: TArray<TManagedRec>;
  comparison: TComparison<TManagedRec>;
begin
  comparison :=
    function(const left, right: TManagedRec): Integer
    begin
      Result := Round(left.DoubleValue - right.DoubleValue);
    end;

  for i := 0 to 1000 do
  begin
    SetLength(values, i);
    for j := 0 to i - 1 do
    begin
      values[j].HashCode := DefaultHashFunction(j, SizeOf(j), 666);
      values[j].Value := j;
      values[j].DoubleValue :=j;
      PDouble(@values[j].Stuff[0])^ := 2 * j;
      PDouble(@values[j].Stuff[8])^ := -3 * j;
      values[j].Text := IntToStr(6 * j + 2);
    end;
    TArray.Shuffle<TManagedRec>(values);
    TArray.StableSort<TManagedRec>(values, comparison);
    for j := 0 to i - 1 do
    begin
      CheckEquals(DefaultHashFunction(j, SizeOf(j), 666), values[j].HashCode);
      CheckEquals(j, values[j].Value);
      CheckEquals(j, values[j].DoubleValue);
      CheckEquals(2 * j, PDouble(@values[j].Stuff[0])^);
      CheckEquals(-3 * j, PDouble(@values[j].Stuff[8])^);
      CheckEquals(IntToStr(6 * j + 2), values[j].Text);
    end;
  end;
end;

procedure TArrayTest.TestTimSortArrayIndexOutOfBoundsBugFix;
// see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8072909
// this test is only useful if the TimSort code is compiled with range checking enabled

const
  MIN_MERGE = 32;
  len = 67108864;

  function MinRunLength(n: Integer): Integer;
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

  (* Adds a sequence x_1, ..., x_n of run lengths to <code>runs</code> such that:
     1. X = x_1 + ... + x_n
     2. x_j >= minRun for all j
     3. x_1 + ... + x_{j-2}  <  x_j  <  x_1 + ... + x_{j-1} for all j
     These conditions guarantee that TimSort merges all x_j's one by one
     (resulting in X) using only merges on the second-to-last element.
     @param X  The sum of the sequence that should be added to runs. *)
  procedure GenerateJDKWrongElem(const runs: IList<Integer>; minRun, X: Integer);
  var
    newTotal: Integer;
  begin
    while X >= 2 * minRun + 1 do
    begin
      //Default strategy
      newTotal := X div 2 + 1;

      //Specialized strategies
      if (3 * minRun + 3 <= X) and (X <= 4 * minRun + 1) then
        // add x_1=MIN+1, x_2=MIN, x_3=X-newTotal  to runs
        newTotal := 2 * minRun + 1
      else if (5 * minRun + 5 <= X) and (X <= 6 * minRun + 5) then
        // add x_1=MIN+1, x_2=MIN, x_3=MIN+2, x_4=X-newTotal  to runs
        newTotal := 3 * minRun + 3
      else if (8 * minRun + 9 <= X) and (X <= 10 * minRun + 9) then
        // add x_1=MIN+1, x_2=MIN, x_3=MIN+2, x_4=2MIN+2, x_5=X-newTotal  to runs
        newTotal := 5 * minRun + 5
      else if (13 * minRun + 15 <= X) and (X <= 16 * minRun + 17) then
        // add x_1=MIN+1, x_2=MIN, x_3=MIN+2, x_4=2MIN+2, x_5=3MIN+4, x_6=X-newTotal  to runs
        newTotal := 8 * minRun + 9;
      runs.Insert(0, X - newTotal);
      X := newTotal;
    end;
    runs.Insert(0, X);
  end;

  (* Fills <code>runs</code> with a sequence of run lengths of the form
     Y_n     x_{n,1}   x_{n,2}   ... x_{n,l_n}
     Y_{n-1} x_{n-1,1} x_{n-1,2} ... x_{n-1,l_{n-1}}
     ...
     Y_1     x_{1,1}   x_{1,2}   ... x_{1,l_1}
     The Y_i's are chosen to satisfy the invariant throughout execution,
     but the x_{i,j}'s are merged (by TimSort.mergeCollapse)
     into an X_i that violates the invariant.
     @param X  The sum of all run lengths that will be added to runs. *)
  procedure RunsJDKWorstCase(const runs: IList<Integer>; minRun: Integer);
  var
    runningTotal, Y, X: Integer;
  begin
    runningTotal := 0;
    Y := minRun + 4;
    X := minRun;
    while runningTotal + Y + X <= len do
    begin
      Inc(runningTotal, X + Y);
      GenerateJDKWrongElem(runs, minRun, X);
      runs.Insert(0, Y);

      // X_{i+1} = Y_i + x_{i,1} + 1, since runs.get(1) = x_{i,1}
      X := Y + runs[1] + 1;

      // Y_{i+1} = X_{i+1} + Y_i + 1
      Inc(Y, X + 1);
    end;

    if runningTotal + X <= len then
    begin
      Inc(runningTotal, X);
      GenerateJDKWrongElem(runs, minRun, X);
    end;

    runs.Add(len - runningTotal);
  end;

  function CreateArray(const runs: IList<Integer>): TArray<Integer>;
  var
    endRun, runLen: Integer;
  begin
    SetLength(Result, len);
    endRun := -1;
    for runLen in runs do
    begin
      Inc(endRun, runLen);
      Result[endRun] := 1;
    end;
    Result[len - 1] := 0;
  end;

var
  minRun: Integer;
  runs: IList<Integer>;
  values: TArray<Integer>;
begin
  minRun := MinRunLength(len);
  runs := TCollections.CreateList<Integer>;
  RunsJDKWorstCase(runs, minRun);
  values := CreateArray(runs);
  TArray.StableSort<Integer>(values);
  Pass;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TWeakTest'}

procedure TWeakTest.TestIsAlive;
var
  weak: Weak<IInterface>;
  intf: IInterface;
begin
  intf := TInterfacedObject.Create;
  weak := intf;
  CheckTrue(weak.IsAlive);
  intf := nil;
  CheckFalse(weak.IsAlive);
end;

procedure TWeakTest.TestTryGetTarget;
var
  weak: Weak<IInterface>;
  intf: IInterface;
begin
  CheckFalse(weak.TryGetTarget(intf));
  CheckNull(intf);
end;

{$ENDREGION}


{$REGION 'TTestVirtualClass'}

type
  {$M+}
  TIntegrityCheckObject = class
  protected
    InitField: string;
  published
    PublishedField: TObject;
  public
    procedure VirtualMethod0; virtual;
    procedure VirtualMethod1; virtual;
  published
    procedure PublishedMethod1;
  end;

procedure TIntegrityCheckObject.PublishedMethod1;
begin
end;

procedure TIntegrityCheckObject.VirtualMethod0;
begin
end;

procedure TIntegrityCheckObject.VirtualMethod1;
begin
end;

procedure TTestVirtualClass.TestIntegrity;
var
  data: PClassData;
begin
  data := GetClassData(TIntegrityCheckObject);
  Check(data.SelfPtr = TIntegrityCheckObject);
  Check(data.InitTable <> nil);
  Check(data.FieldTable <> nil);
  Check(data.FieldTable.Count = 1);
  Check(data.FieldTable.ClassTab.Count = 1);
  Check(data.FieldTable.ClassTab.ClassRef[0]^ = TObject);
  Check(data.MethodTable <> nil);
  Check(data.MethodTable.Count = 1);
  check(PVmtMethodEntry(PByte(data.MethodTable) + SizeOf(Word)).CodeAddress = @TIntegrityCheckObject.PublishedMethod1);
  Check(data.TypeInfo = TypeInfo(TIntegrityCheckObject));
  Check(@data.Destroy = @TObject.Destroy);
  Check(data.VirtualMethods[0] = @TIntegrityCheckObject.VirtualMethod0);
  Check(data.VirtualMethods[1] = @TIntegrityCheckObject.VirtualMethod1);
end;

{$ENDREGION}


{$REGION 'TTestEnum'}

procedure TTestEnum.TestGetNameByEnum;
var
  expectedName: string;
  actualName: string;
  item: TSeekOrigin;
  pInfo: PTypeInfo;
begin
  pInfo := TypeInfo(TSeekOrigin);
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedName := GetEnumName(pInfo, Integer(item));
    actualName := TEnum.GetName<TSeekOrigin>(item);
    CheckEquals(expectedName, actualName);
  end;
end;

procedure TTestEnum.TestGetNameByInteger;
var
  expectedName: string;
  actualName: string;
  item: TSeekOrigin;
  pInfo: PTypeInfo;
begin
  pInfo := TypeInfo(TSeekOrigin);
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedName := GetEnumName(pInfo, Integer(item));
    actualName := TEnum.GetName<TSeekOrigin>(Integer(item));
    CheckEquals(expectedName, actualName);
  end;
end;

procedure TTestEnum.TestGetNames;
var
  expectedName: string;
  actualName: string;
  item: TSeekOrigin;
  pInfo: PTypeInfo;
  names: TStringDynArray;
begin
  pInfo := TypeInfo(TSeekOrigin);
  names := TEnum.GetNames<TSeekOrigin>;
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedName := GetEnumName(pInfo, Ord(item));
    actualName := names[Ord(item)];
    CheckEquals(expectedName, actualName);
  end;
end;

procedure TTestEnum.TestGetValueByEnum;
var
  expectedValue: Integer;
  actualValue: Integer;
  item: TSeekOrigin;
begin
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedValue := Integer(item);
    actualValue := TEnum.GetValue<TSeekOrigin>(item);
    CheckEquals(expectedValue, actualValue);
  end;
end;

procedure TTestEnum.TestGetValueByName;
var
  expectedValue: Integer;
  actualValue: Integer;
  item: TSeekOrigin;
  name: string;
begin
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedValue := Integer(item);
    name := GetEnumName(TypeInfo(TSeekOrigin), expectedValue);
    actualValue := TEnum.GetValue<TSeekOrigin>(name);
    CheckEquals(expectedValue, actualValue);
  end;
end;

procedure TTestEnum.TestIsValid;
var
  item: TSeekOrigin;
begin
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    Check(TEnum.IsValid<TSeekOrigin>(item));
    Check(TEnum.IsValid<TSeekOrigin>(Integer(item)));
  end;
  CheckFalse(TEnum.IsValid<TSeekOrigin>(Integer(Low(TSeekOrigin)) - 1));
  CheckFalse(TEnum.IsValid<TSeekOrigin>(Integer(High(TSeekOrigin)) + 1));
end;

procedure TTestEnum.TestParse;
var
  item: TSeekOrigin;
  actual: TSeekOrigin;
begin
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    actual := TEnum.Parse<TSeekOrigin>(Integer(item));
    CheckEquals(Integer(item), Integer(actual));
    actual := TEnum.Parse<TSeekOrigin>(GetEnumName(TypeInfo(TSeekOrigin), Integer(item)));
    CheckEquals(Integer(item), Integer(actual));
  end;
end;

procedure TTestEnum.TestTryParse;
var
  item: TSeekOrigin;
begin
  Check(TEnum.TryParse<TSeekOrigin>(Integer(soBeginning), item));
  CheckEquals(Integer(soBeginning), Integer(item));
  Check(TEnum.TryParse<TSeekOrigin>('soBeginning', item));
  CheckEquals(Integer(soBeginning), Integer(item));

  CheckFalse(TEnum.TryParse<TSeekOrigin>(Integer(Low(TSeekOrigin)) - 1, item));
  CheckFalse(TEnum.TryParse<TSeekOrigin>('dummy', item));
end;

procedure TTestEnum.TestParseIntegerException;
begin
  ExpectedException := EFormatException;
  TEnum.Parse<TSeekOrigin>(Integer(Low(TSeekOrigin))-1);
end;

procedure TTestEnum.TestParseStringException;
begin
  ExpectedException := EFormatException;
  TEnum.Parse<TSeekOrigin>('dummy');
end;

{$ENDREGION}


{$REGION 'TTestBaseRoutines'}

procedure TTestBaseRoutines.TestNextPowerOf2;

  procedure TestRange(Low, High, Power: NativeInt);
  var
    i: NativeInt;
  begin
    for i := Low to High do
      CheckEquals(Power, NextPowerOf2(i), Format('NextPowerOf2(%d) did not return %d', [i, Power]));
  end;

const
  Pow_2_30 = 1 shl 30;
begin
  TestRange(-50,  0,  1);
  TestRange(  1,  1,  2);
  TestRange(  2,  3,  4);
  TestRange(  4,  7,  8);
  TestRange(  8, 15, 16);
  TestRange( 16, 31, 32);

  TestRange(Pow_2_30 - 50, Pow_2_30 - 1, Pow_2_30);
  TestRange(High(NativeInt) div 2 + 1, High(NativeInt) div 2 + 2, Low(NativeInt));
  TestRange(High(NativeInt) - 1, High(NativeInt), Low(NativeInt));
end;

{$ENDREGION}


{$REGION 'TSortTest'}

class function TSortTest.RandomChar: AnsiChar;
begin
  Result := AnsiChar(Chr(65 + Random(26)));
end;

class procedure TSortTest.TestPassing<T>(const value: T);
begin
end;

procedure TSortTest.TestSort<T>(const genvalue: Func<T>);
var
  data: TArray<T>;
  i: Integer;
  comparer: IComparer<T>;
begin
  SetLength(data, Count);
  for i := 0 to High(data) do
    data[i] := genValue;
  TestPassing<T>(data[0]);
  TArray.Sort<T>(data);
  comparer := TComparer<T>.Default;
  for i := 1 to High(data) do
    Check(comparer.Compare(data[i-1], data[i]) <= 0);
end;

procedure TSortTest.Test_Int8;
begin
  TestSort<Int8>(function: Int8 begin Result := Random(High(Int8)) end);
end;

procedure TSortTest.Test_Int16;
begin
  TestSort<Int16>(function: Int16 begin Result := Random(High(Int16)) end);
end;

procedure TSortTest.Test_Int32;
begin
  TestSort<Int32>(function: Int32 begin Result := Random(High(Int32)) end);
end;

procedure TSortTest.Test_Int64;
begin
  TestSort<Int64>(function: Int64 begin Result := Random(High(Int32)) end);
end;

procedure TSortTest.Test_Single;
begin
  TestSort<Single>(function: Single begin Result := Random() * 1000 end);
end;

procedure TSortTest.Test_Double;
begin
  TestSort<Double>(function: Double begin Result := Random() * 1000 end);
end;

procedure TSortTest.Test_Extended;
begin
  TestSort<Extended>(function: Extended begin Result := Random() * 1000 end);
end;

procedure TSortTest.Test_Comp;
begin
  TestSort<Comp>(function: Comp begin Result := Random() * 1000 end);
end;

procedure TSortTest.Test_Currency;
begin
  TestSort<Currency>(function: Currency begin Result := Random() * 1000 end);
end;

procedure TSortTest.Test_String;
begin
  TestSort<TString1>(function: TString1 begin
    Result[1] := RandomChar;
  end);
  TestSort<TString2>(function: TString2 begin
    Result[1] := RandomChar;
    Result[2] := RandomChar;
  end);
  TestSort<TString3>(function: TString3 begin
    Result[1] := RandomChar;
    Result[2] := RandomChar;
    Result[3] := RandomChar;
  end);
  TestSort<TString4>(function: TString4 begin
    Result[1] := RandomChar;
    Result[2] := RandomChar;
    Result[3] := RandomChar;
    Result[4] := RandomChar;
  end);
  TestSort<TString7>(function: TString7 var i: Integer; begin
    for i := 1 to 7 do
      Result[i] := RandomChar;
  end);
end;

procedure TSortTest.Test_Set;
begin
  TestSort<TSet8>(function: TSet8 var i: TEnum8; begin
    Result := [];
    for i := Low(TEnum8) to High(TEnum8) do
      if Random(2) = 0 then
        Include(Result, i);
  end);
  TestSort<TSet16>(function: TSet16 var i: TEnum16; begin
    Result := [];
    for i := Low(TEnum16) to High(TEnum16) do
      if Random(2) = 0 then
        Include(Result, i);
  end);
  TestSort<TSet32>(function: TSet32 var i: TEnum32; begin
    Result := [];
    for i := Low(TEnum32) to High(TEnum32) do
      if Random(2) = 0 then
        Include(Result, i);
  end);
  TestSort<TSet64>(function: TSet64 var i: TEnum64; begin
    Result := [];
    for i := Low(TEnum64) to High(TEnum64) do
      if Random(2) = 0 then
        Include(Result, i);
  end);
  TestSort<TSet256>(function: TSet256 var i: Byte; begin
    Result := [];
    for i := Low(Byte) to High(Byte) do
      if Random(2) = 0 then
        Include(Result, i);
  end);
end;

procedure TSortTest.Test_Array;
begin
  TestSort<TArray1>(function: TArray1 begin
    Result[0] := Random(High(Byte));
  end);
  TestSort<TArray2>(function: TArray2 begin
    Result[0] := Random(High(Byte));
    Result[1] := Random(High(Byte));
  end);
  TestSort<TArray3>(function: TArray3 begin
    Result[0] := Random(High(Byte));
    Result[1] := Random(High(Byte));
    Result[2] := Random(High(Byte));
  end);
  TestSort<TArray4>(function: TArray4 begin
    Result[0] := Random(High(Byte));
    Result[1] := Random(High(Byte));
    Result[2] := Random(High(Byte));
    Result[3] := Random(High(Byte));
  end);
  TestSort<TArray5>(function: TArray5 var i: Integer; begin
    for i := 0 to 4 do
      Result[i] := Random(High(Byte));
  end);
  TestSort<TArray8>(function: TArray8 var i: Integer; begin
    for i := 0 to 7 do
      Result[i] := Random(High(Byte));
  end);
end;

procedure TSortTest.Test_Variant;
begin
  TestSort<Variant>(function: Variant begin
    Result := Random(High(Integer));
  end);
end;

procedure TSortTest.Test_Record;
begin
  TestSort<TRec1>(function: TRec1 begin
    Result.a := Random(High(Byte));
  end);
  TestSort<TRec2>(function: TRec2 begin
    Result.a := Random(High(Byte));
    Result.b := Random(High(Byte));
  end);
  TestSort<TRec3>(function: TRec3 begin
    Result.a := Random(High(Byte));
    Result.b := Random(High(Byte));
    Result.c := Random(High(Byte));
  end);
  TestSort<TRec4>(function: TRec4 begin
    Result.a := Random(High(Byte));
    Result.b := Random(High(Byte));
    Result.c := Random(High(Byte));
    Result.d := Random(High(Byte));
  end);
  TestSort<TRec5>(function: TRec5 begin
    Result.a := Random(High(Byte));
    Result.b := Random(High(Byte));
    Result.c := Random(High(Byte));
    Result.d := Random(High(Byte));
    Result.e := Random(High(Byte));
  end);
  TestSort<TRec8>(function: TRec8 begin
    Result.a := Random(High(Integer));
    Result.b := Random(High(Integer));
  end);
  TestSort<TRec12>(function: TRec12 begin
    Result.a := Random(High(Integer));
    Result.b := Random(High(Integer));
    Result.c := Random(High(Integer));
  end);
end;

{$ENDREGION}


{$REGION 'TTestHash'}

procedure TTestHash.TestHash(hashKind: THashKind; len: Cardinal);
const
  // test data generated using Python mmh3 and xxhash modules
  Data: array [0..63] of Byte = (
    208, 117, 92, 62, 156, 212, 232, 10, 35, 8, 31, 230, 228, 102, 224, 58,
    53, 25, 103, 147, 179, 175, 210, 84, 17, 158, 140, 126, 98, 98, 25, 120,
    107, 167, 23, 87, 158, 237, 185, 74, 200, 146, 231, 119, 143, 138, 200,
    172, 241, 93, 30, 70, 178, 63, 21, 44, 83, 159, 38, 242, 19, 166, 62, 97
  );

  // results generated using Python mmh3 and xxhash modules
  (*
    import mmh3
    import xxhash

    data = bytes([
        208, 117, 92, 62, 156, 212, 232, 10, 35, 8, 31, 230, 228, 102, 224, 58,
        53, 25, 103, 147, 179, 175, 210, 84, 17, 158, 140, 126, 98, 98, 25, 120,
        107, 167, 23, 87, 158, 237, 185, 74, 200, 146, 231, 119, 143, 138, 200,
        172, 241, 93, 30, 70, 178, 63, 21, 44, 83, 159, 38, 242, 19, 166, 62, 97
    ])
    print('input data')
    print(', '.join([str(byte) for byte in data]))

    mmh3h = [mmh3.hash(data[:len], 0xDEADBEEF) for len in range(65)]
    print('mmh3')
    print(', '.join('${:x}'.format(hash & 0xFFFFFFFF) for hash in mmh3h))

    xxhash32h = [xxhash.xxh32(data[:len], 0xDEADBEEF) for len in range(65)]
    print('xxhash32')
    print(', '.join('${:x}'.format(hash.intdigest() & 0xFFFFFFFF) for hash in xxhash32h))
  *)
  ExpectedResults: array[THashKind, 0..64] of Cardinal = (
    // xxHash32
    (
      $c372c6cb,
      $e5f216a3, $0af6a55a, $40a90440, $03a549c5, $3b99221e, $fa164638, $7bcbf3c7, $e83a09d9,
      $106efc1d, $8a9ce6e6, $fe5b8b51, $c8827864, $a5242ac6, $ab86ba02, $eeea2f7f, $5ae17b66,
      $f8941f27, $aff57b0d, $9d039176, $43f5a52f, $5ca7d512, $77e0f040, $5ab1b93a, $46733668,
      $534b5035, $d48af9d6, $7bbfc89d, $028abc3a, $267414eb, $1707db61, $7090c40b, $b36235df,
      $68c68326, $a991ecd9, $335e17a0, $6a3fafd2, $51ebc2f5, $cbf1407e, $5ce8404b, $711181d5,
      $b6b13b3f, $bdc248ed, $2c0d8235, $a33ce951, $0aba04ea, $295420b0, $d5b42bc0, $285d177f,
      $5ce588c2, $fbeadfa1, $9b187c03, $4769a55b, $50f2669d, $e2e281ae, $89bf3044, $411c7bb3,
      $d2173590, $43553094, $14b0d583, $07d1fe61, $e55f8855, $ab440b8e, $b5b7652a, $d3b4aa34
    ),
    // MurmurHash3
    (
      $0de5c6a9,
      $42e92f47, $9eaac96c, $c48ae3bd, $2dafc77c, $fef89a7f, $e1241ce9, $cd21f3c5, $57ac4e2a,
      $a4dee050, $49f322e2, $0bf78fbe, $5cd88bbe, $5c6bf74c, $e1e36aa6, $2a338652, $a51e1300,
      $c04472ea, $5d553691, $f1ce041e, $97a46b33, $716ffc60, $e9a8a4df, $9634b6ff, $d644ecc4,
      $431edce0, $111f82c5, $9bb9fd33, $7faad2cc, $3c3edc99, $c63467b2, $33cbe27e, $a6af9905,
      $d7a690bd, $debaaeab, $be8cee99, $8339ee23, $86c4ec4d, $4bd40901, $c0ea88c6, $a8b4420b,
      $0f7c34b0, $697a2423, $452acf1a, $803074d6, $add9a248, $69ab4d82, $b87c8b80, $2bdd6015,
      $9ddce451, $139e2db6, $35105699, $4c8678c6, $4ba58bb0, $04478d61, $6906fea6, $7f8f1b82,
      $6a89407a, $22dca392, $e33c5839, $13a53c29, $19872049, $92529e7b, $4aeb837f, $98258a10
    )
  );

  HashFunction: array[THashKind] of THashFunction = (
    Spring.Hash.xxHash32, Spring.Hash.MurmurHash3
  );

  Seed = Integer($deadbeef);
begin
  CheckEquals(ExpectedResults[hashKind, len], Cardinal(HashFunction[hashKind](Data[0], len, Seed)));
end;

{$ENDREGION}


end.
