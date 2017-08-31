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

unit Spring.Tests.Base;

{$I Spring.inc}
{$IFDEF UNSAFE_NULLABLE}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

{$IF Defined(MACOS) AND NOT Defined(IOS)}
  {$DEFINE LogConsole}
{$IFEND MACOS}

interface

uses
  Classes,
  TypInfo,
  TestFramework,
  Spring.TestUtils,
  Spring,
  Spring.Events,
  Spring.Utils;

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
{$IFNDEF DELPHI2010}
    procedure TestFromVariantSQLTimestampOffset;
{$ENDIF}
  end;

  TTestNullableInt64 = class(TTestCase)
  private
    fInt64: Nullable<Int64>;
  protected
    procedure TearDown; override;
  published
    procedure TestFromVariantFmtBcd;
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
  end;

  {$M+}
  TProc<T1, T2> = reference to procedure(arg1: T1; arg2: T2);
  {$M-}

  TTestMulticastEvent = class(TTestCase)
  strict private
    type
      TEventInt64 = procedure(const Value: Int64) of object;
      TEventSingle = procedure(const Value: Single) of object;
      TEventDouble = procedure(const Value: Double) of object;
      TEventExtended = procedure(const Value: Extended) of object;
    const
      CNumber = 5;
      CText = 'test';
  strict private
    fEvent: IMulticastNotifyEvent;
    fASender: TObject;
    fAInvoked: Boolean;
    fBSender: TObject;
    fBInvoked: Boolean;
    fHandlerInvokeCount: Integer;
    fProc: TProc<Integer, string>;
  strict protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure HandlerA(sender: TObject);
    procedure HandlerB(sender: TObject);

    procedure HandlerInt64(const value: Int64);
    procedure HandlerSingle(const value: Single);
    procedure HandlerDouble(const value: Double);
    procedure HandlerExtended(const value: Extended);

    procedure HandleChanged(Sender: TObject);
  published
    procedure TestInvoke;
    procedure TestOneHandler;
    procedure TestTwoHandlers;
    procedure TestRecordType;
    procedure TestIssue58;
    procedure TestDelegate;
    procedure TestIssue60;
    procedure TestNotify;
    procedure TestNotifyDelegate;
    procedure TestRemove;

    procedure TestClassProcedureHandler;
    procedure TestInstanceProcedureHandler;
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
{$IFNDEF NEXTGEN}
    procedure Test_GetTypeSize_AnsiChar;
    procedure Test_GetTypeSize_AnsiString;
{$ENDIF}
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
{$IFNDEF NEXTGEN}
    procedure Test_GetTypeSize_PAnsiChar;
{$ENDIF}
    procedure Test_GetTypeSize_PChar;
    procedure Test_GetTypeSize_Pointer;
    procedure Test_GetTypeSize_Proc;
    procedure Test_GetTypeSize_Procedure;
    procedure Test_GetTypeSize_PWideChar;
    procedure Test_GetTypeSize_Real;
    procedure Test_GetTypeSize_Set;
    procedure Test_GetTypeSize_SetOfByte;
    procedure Test_GetTypeSize_ShortInt;
{$IFNDEF NEXTGEN}
    procedure Test_GetTypeSize_ShortString;
    procedure Test_GetTypeSize_ShortString0;
    procedure Test_GetTypeSize_ShortString1;
    procedure Test_GetTypeSize_ShortString2;
    procedure Test_GetTypeSize_ShortString255;
    procedure Test_GetTypeSize_ShortString7;
{$ENDIF}
    procedure Test_GetTypeSize_Single;
    procedure Test_GetTypeSize_SmallInt;
    procedure Test_GetTypeSize_string;
    procedure Test_GetTypeSize_UnicodeString;
    procedure Test_GetTypeSize_Variant;
    procedure Test_GetTypeSize_WideChar;
{$IFNDEF NEXTGEN}
    procedure Test_GetTypeSize_WideString;
{$ENDIF}
    procedure Test_GetTypeSize_Word;
    procedure Test_GetTypeSize_WordBool;
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

  TTestOwned = class(TTestCase)
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

    procedure FromVariantProperlyHandlesVariantArrays;

    procedure ConvertStringToIntegerFailsForInvalidString;
    procedure ConvertStringToFloatFailsForInvalidString;

    procedure FromCustomVariantFmtBcd;

    procedure TryToType_ConvertIntToStr;
    procedure TryToType_ConvertStrToInt;
    procedure TryToType_ConvertStringToNullableString;
    procedure TryToType_ConvertIntegerToNullableEnum;

    procedure GetNullableValue_ValueIsEmpty_ReturnsEmpty;

    procedure ImplicitOperators;

    procedure NullableToString;
  end;

  TEnumeration = (teFirst, teSecond, teLast);

{$IFNDEF DELPHI2010}
  TTestManagedObject = class(TTestCase)
  published
    procedure TestInitialization;
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
  {$IFNDEF NEXTGEN}
    [Default('x')]
    fAnsiCharValue: AnsiChar;
  {$ENDIF}
    [Default('y')]
    fWideCharValue: WideChar;
    [Default('z')]
    fCharValue: Char;
  {$IFNDEF DELPHI2010}
    [Managed(TInterfacedObject)]
    fIntfValue: IInterface;
  {$ENDIF}

    fIntValue_Prop: Integer;
    fStrValue_Prop: string;
    procedure SetStrValue_Prop(const Value: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    [Default(43)]
    property IntValue: Integer read fIntValue_Prop write fIntValue_Prop;

    [Default('hello')]
    property StrValue: string read fStrValue_Prop write SetStrValue_Prop;
  end;
{$ENDIF}

  TArrayTest = class(TTestCase)
  published
    procedure TestBinarySearchUpperBound;
    procedure TestBinarySearchUpperBoundSubRange;

    procedure TestLastIndexOf;
    procedure TestLastIndexOfSubRange;
  end;

  TWeakTest = class(TTestCase)
  published
    procedure TestIsAlive;
  end;

  TTestVirtualClass = class(TTestCase)
  published
    procedure TestIntegrity;
  end;

implementation

uses
  DateUtils,
  FmtBcd,
  Generics.Defaults,
  SqlTimSt,
  SysUtils,
  Variants,
  Rtti,
  StrUtils,
  TimeSpan,
  Types,
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
  fInteger := Nullable.Null;
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
{$IFDEF UNSAFE_NULLABLE}
  fInteger := 99.9;
{$ELSE}
  fInteger := Nullable<Integer>(99.9);
{$ENDIF}
end;

procedure TTestNullableInteger.TestAssignNull;
var
  n: Nullable<Integer>;
begin
  n := 5;
  n := Nullable.Null;
  Check(not n.HasValue);
end;

procedure TTestNullableInteger.TestAssignStringInt;
begin
  // Nullable does NOT do a variant type conversion but is strict about the underlying type
  ExpectedException := EInvalidCast;
{$IFDEF UNSAFE_NULLABLE}
  fInteger := '5';
{$ELSE}
  fInteger := Nullable<Integer>('5');
{$ENDIF}
end;

procedure TTestNullableInteger.TestAssignStringNonInt;
begin
  ExpectedException := EInvalidCast;
{$IFDEF UNSAFE_NULLABLE}
  fInteger := '5x';
{$ELSE}
  fInteger := Nullable<Integer>('5x');
{$ENDIF}
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
  dirtyValue := 5;
end;

procedure TTestNullableInteger.TestNullableNull;
begin
  fInteger := 42;
  CheckEquals(42, fInteger.Value);
  fInteger := Nullable.Null;
  Check(not fInteger.HasValue);
  Check(fInteger = Nullable.Null);
  CheckFalse(fInteger <> Nullable.Null);
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

{$IFDEF UNSAFE_NULLABLE}
  fInteger := value;
{$ELSE}
  fInteger := Nullable<Integer>(value);
{$ENDIF}
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
  fBoolean := Nullable.Null;
end;

procedure TTestNullableBoolean.TestIssue55;
var
  v: Variant;
begin
  fBoolean := True;
{$IFDEF UNSAFE_NULLABLE}
  v := fBoolean;
{$ELSE}
  v := fBoolean.ToVariant;
{$ENDIF}
  CheckTrue(v);
end;

{$ENDREGION}


{$REGION 'TTestMulticastEvent'}

procedure TTestMulticastEvent.SetUp;
begin
  inherited;
  fEvent := TMulticastNotifyEvent.Create();
  fProc :=
    procedure(i: Integer; s: string)
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

procedure TTestMulticastEvent.TestClassProcedureHandler;
var
  e: Event<TEventInt64>;
begin
  e.Add(TEventHandler.HandleInt64Static);
  e.Invoke(42);
  e.Remove(TEventHandler.HandleInt64Static);
  CheckTrue(TEventHandler.fClassHandlerInvoked);
end;

procedure TTestMulticastEvent.TestDelegate;
var
  e: Event<TProc<Integer, string>>;
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
  e.Add(t.HandleInt64);
  e.Invoke(42);
  CheckTrue(t.fClassHandlerInvoked);
  t.Free;
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

  eventInt64 := Event<TEventInt64>.Create();
  eventSingle := Event<TEventSingle>.Create();
  eventDouble := Event<TEventDouble>.Create();
  eventExtended := Event<TEventExtended>.Create();

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
  event2: Event<TProc<Integer, string>>;
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
  factory: TFunc<Integer>;
begin
  factory :=
    function: Integer
    begin
      Result := CExpectedBalance;
    end;
  fBalance := TLazy<Integer>.Create(factory);

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
  CheckFalse(IsLazyType(TypeInfo(TFunc<string,Integer>)));
  CheckTrue(IsLazyType(TypeInfo(TFunc<TFunc<string,Integer>>)));
end;

procedure TTestLazy.Test_Initializer_RaisesArgumentException_NotReferenceType;
var
  i: Integer;
begin
  ExpectedException := EArgumentException;
  TLazyInitializer.EnsureInitialized<Integer>(i, function: Integer begin Exit(42) end);
end;

procedure TTestLazy.TearDown;
begin
  inherited;
  fBalance := nil;
end;

procedure TTestLazy.TestByValue;
begin
  fBalance := TLazy<Integer>.CreateFrom(CExpectedBalance);

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
{$IFNDEF NEXTGEN}
  e: TNotifyEvent;
{$ENDIF}
begin
  obj := nil;
  CheckTrue(Guard.IsNullReference(obj, TypeInfo(TObject)));
  CheckTrue(Guard.IsNullReference(intf, TypeInfo(IInterface)));
{$IFNDEF NEXTGEN}
  e := nil;
  CheckTrue(Guard.IsNullReference(e, TypeInfo(TNotifyEvent)));
  TMethod(e).Data := Self;
  CheckFalse(Assigned(e));
  CheckFalse(Guard.IsNullReference(e, TypeInfo(TNotifyEvent)));
{$ELSE}
//  {$MESSAGE WARN 'Delphi problem'}
{$ENDIF}
end;

procedure TTestGuard.TestNotNull;
var
  intf: IInterface;
begin
  StartExpectingException(EArgumentNullException);
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
  eventDouble := Event<TEventDouble>.Create();
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
  eventDouble := Event<TEventDouble>.Create();
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
  eventExtended := Event<TEventExtended>.Create();
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
  eventExtended := Event<TEventExtended>.Create();
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
  eventExtended := Event<TEventGuid>.Create();
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
  eventInt64 := Event<TEventInt64>.Create();
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
  eventInt64 := Event<TEventInt64>.Create();
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
  eventSingle := Event<TEventSingle>.Create();
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
  eventSingle := Event<TEventSingle>.Create();
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

{$IFNDEF NEXTGEN}
type
  TShortString0 = String[0];
  TShortString1 = String[1];
  TShortString2 = String[2];
  TShortString255 = String[255];
  TShortString7 = String[7];
{$ENDIF}

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
{$IFDEF NEXTGEN}
const NextGenExcludedTypeKinds = [tkChar, tkString, tkLString, tkWString];
{$ENDIF}
begin
  inherited;
  fRemainingTypeKinds := tkAny - [tkUnknown];
{$IFDEF NEXTGEN}
  // NextGen does not support these types by default (unless DCU hacking is used)
  fRemainingTypeKinds := fRemainingTypeKinds - NextGenExcludedTypeKinds;
{$ENDIF}
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

{$IFNDEF NEXTGEN}
procedure TTestSpringEventsMethods.Test_GetTypeSize_AnsiChar;
begin
  MatchType(TypeInfo(AnsiChar), tkChar, SizeOf(AnsiChar));
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_AnsiString;
begin
  MatchType(TypeInfo(AnsiString), tkLString, PointerSize);
end;
{$ENDIF}

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

{$IFNDEF NEXTGEN}
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
{$ENDIF}

procedure TTestSpringEventsMethods.Test_GetTypeSize_Interface;
begin
  MatchType(TypeInfo(IInterface), tkInterface, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_Method;
begin
  MatchType(TypeInfo(TNotifyEvent), tkMethod, PointerSize * 2);
end;

{$IFNDEF NEXTGEN}
procedure TTestSpringEventsMethods.Test_GetTypeSize_PAnsiChar;
begin
  MatchType(TypeInfo(PAnsiChar), tkPointer, PointerSize);
end;
{$ENDIF}

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

{$IFNDEF NEXTGEN}
procedure TTestSpringEventsMethods.Test_GetTypeSize_ShortString255;
begin
  MatchType(TypeInfo(TShortString255), tkString, PointerSize);
end;

procedure TTestSpringEventsMethods.Test_GetTypeSize_ShortString7;
begin
  MatchType(TypeInfo(TShortString7), tkString, PointerSize);
end;
{$ENDIF}

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

{$IFNDEF NEXTGEN}
procedure TTestSpringEventsMethods.Test_GetTypeSize_WideString;
begin
  MatchType(TypeInfo(WideString), tkWString, PointerSize);
end;
{$ENDIF}

procedure TTestSpringEventsMethods.Test_GetTypeSize_WordBool;
begin
  MatchType(TypeInfo(WordBool), tkEnumeration, SizeOf(WordBool)); // not tkInteger !!
end;

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

procedure TTestOwned.TestInterfaceType_Instance_Gets_Created;
var
  p: IManaged<TTestClass>;
begin
  p := TManaged<TTestClass>.Create();
  CheckTrue(p.CreateCalled);
end;

procedure TTestOwned.TestInterfaceType_Instance_Gets_Destroyed_When_Created;
var
  p: IManaged<TTestClass>;
  t: TTestClass;
  destroyCalled: Boolean;
begin
  p := TManaged<TTestClass>.Create();
  t := p;
  t.DestroyCalled := @destroyCalled;
{$IFDEF AUTOREFCOUNT}
  t := nil;
{$ENDIF}
  destroyCalled := False;
  p := nil;
  CheckTrue(destroyCalled);
end;

procedure TTestOwned.TestInterfaceType_Instance_Gets_Destroyed_When_Injected;
var
  t: TTestClass;
  p: IManaged<TTestClass>;
  destroyCalled: Boolean;
begin
  t := TTestClass.Create;
  t.DestroyCalled := @destroyCalled;
  p := TManaged<TTestClass>.Create(t);
{$IFDEF AUTOREFCOUNT}
  t := nil;
{$ENDIF}
  destroyCalled := False;
  p := nil;
  CheckTrue(destroyCalled);
end;

procedure TTestOwned.TestRecordType_Implicit_FromInstance_Works;
var
  p: Managed<TTestClass>;
  t: TTestClass;
begin
  t := TTestClass.Create;
  p := t;
  CheckSame(t, p.Value);
end;

procedure TTestOwned.TestRecordType_Implicit_ToInstance_Works;
var
  p: Managed<TTestClass>;
  t, t2: TTestClass;
begin
  t := TTestClass.Create;
  p := t;
  t2 := p;
  CheckSame(t, t2);
end;

procedure TTestOwned.TestRecordType_Instance_Gets_Destroyed;
var
  p: Managed<TTestClass>;
  t: TTestClass;
  destroyCalled: Boolean;
begin
  t := TTestClass.Create;
  t.DestroyCalled := @destroyCalled;
  p := t;
{$IFDEF AUTOREFCOUNT}
  t := nil;
{$ENDIF}
  destroyCalled := False;
  p := Default(Managed<TTestClass>);
  CheckTrue(destroyCalled);
end;

type
  PMyRecord = ^TMyRecord;
  TMyRecord = record
    x, y: Integer;
    s: string;
  end;

procedure TTestOwned.TestRecordType_Manage_Typed_Pointer;
var
  p: IManaged<PMyRecord>;
begin
  p := TManaged<PMyRecord>.Create();
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
  CheckFalse(fSUT.TryConvert<Double>(f));
end;

procedure TTestValueHelper.ConvertStringToIntegerFailsForInvalidString;
var
  i: Integer;
begin
  fSUT := 'foo';
  CheckFalse(fSUT.TryConvert<Integer>(i));
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
begin
  v := VarFMTBcdCreate('999999999999999999', 19, 0);
  fSUT := TValue.FromVariant(v);
  Check(fSUT.Kind = tkInt64);
  CheckEquals(999999999999999999, fSUT.AsInt64);

  v := VarFMTBcdCreate(12.5);
  fSUT := TValue.FromVariant(v);
  Check(fSUT.Kind = tkFloat);
  CheckEquals(12.5, fSUT.AsType<Double>);
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
  fSUT := TValue.From(Nullable<Integer>(Nullable.Null));
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

{$ENDREGION}


{$REGION 'TTestNullableDateTime'}

procedure TTestNullableDateTime.TearDown;
begin
  fDateTime := Nullable.Null;
  inherited;
end;

procedure TTestNullableDateTime.TestFromVariantSQLTimestamp;
var
  dt: TDateTime;
  v: Variant;
begin
  dt := EncodeDateTime(2015, 1, 1, 12, 0, 0, 0);
  v := VarSQLTimeStampCreate(dt);
{$IFDEF UNSAFE_NULLABLE}
  fDateTime := v;
{$ELSE}
  fDateTime := Nullable<TDateTime>(v);
{$ENDIF}
  CheckEquals(dt, fDateTime.Value);
end;

{$IFNDEF DELPHI2010}
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
{$IFDEF UNSAFE_NULLABLE}
  fDateTime := v;
{$ELSE}
  fDateTime := Nullable<TDateTime>(v);
{$ENDIF}
  CheckEquals(dt, fDateTime.Value);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTestNullableInt64'}

procedure TTestNullableInt64.TearDown;
begin
  fInt64 := Nullable.Null;
  inherited;
end;

procedure TTestNullableInt64.TestFromVariantFmtBcd;
var
  v: Variant;
begin
  v := VarFMTBcdCreate('8123456789012345678', 19, 0);
{$IFDEF UNSAFE_NULLABLE}
  fInt64 := v;
{$ELSE}
  fInt64 := Nullable<Int64>(v);
{$ENDIF}
  CheckEquals(8123456789012345678, fInt64.Value);
end;

{$ENDREGION}


{$REGION 'TTestManagedObject'}

{$IFNDEF DELPHI2010}
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
  {$IFNDEF NEXTGEN}
    CheckEquals('x', Char(obj.fAnsiCharValue));
  {$ENDIF}
    CheckEquals('y', Char(obj.fWideCharValue));
    CheckEquals('z', Char(obj.fCharValue));
  {$IFNDEF DELPHI2010}
    CheckNotNull(obj.fIntfValue);
    CheckIs(obj.fIntfValue as TObject, TInterfacedObject);
  {$ENDIF}

    // check property initializations
    CheckEquals(43, obj.fIntValue_Prop);
    CheckEquals('hello', obj.fStrValue_Prop);
  finally
    obj.Free;
  end;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TArrayTest'}

procedure TArrayTest.TestBinarySearchUpperBound;
var
  values: TArray<Integer>;
  index: Integer;
begin
  values := TArray<Integer>.Create(1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9);
  CheckTrue(TArray.BinarySearchUpperBound<Integer>(values, 5, index));
  CheckEquals(6, index);
end;

procedure TArrayTest.TestBinarySearchUpperBoundSubRange;
var
  values: TArray<Integer>;
  index: Integer;
begin
  values := TArray<Integer>.Create(1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9);
  CheckTrue(TArray.BinarySearchUpperBound<Integer>(values, 5, index,
    TComparer<Integer>.Default(), 0, 6));
  CheckEquals(5, index);
end;

procedure TArrayTest.TestLastIndexOf;
var
  values: TArray<Integer>;
  index: Integer;
begin
  values := TArray<Integer>.Create(1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9);
  index := TArray.LastIndexOf<Integer>(values, 5);
  CheckEquals(6, index);
end;

procedure TArrayTest.TestLastIndexOfSubRange;
var
  values: TArray<Integer>;
  index: Integer;
begin
  values := TArray<Integer>.Create(1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9);
  index := TArray.LastIndexOf<Integer>(values, 5, 0, 6);
  CheckEquals(5, index);
end;

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


end.
