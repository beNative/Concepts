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

unit Spring.Tests.Logging.Serializers;

{$I Spring.Tests.inc}

interface

uses
  SysUtils,
  TypInfo,
  Rtti,
  TestFramework,
  Generics.Defaults,
  Spring,
  Spring.Logging,
  Spring.Logging.Extensions,
  Spring.Logging.Controller,
  Spring.Logging.Serializers,
  Spring.Tests.Logging.Types;

type
  {$REGION 'TSerializerTestCase'}

  TSerializerTestCase = class(TTestCase)
  strict private
    fSerializer: TSerializerBase;
    procedure SetSerializer(const value: TSerializerBase);
  strict protected
    property Serializer: TSerializerBase read fSerializer write SetSerializer;
  protected
    procedure TearDown; override;
    procedure CheckUnsupported(supportedTypeKinds: TTypeKinds);
  end;

  {$ENDREGION}


  {$REGION 'TTestSimpleTypeSerializer'}

  TTestSimpleTypeSerializer = class(TSerializerTestCase)
  private
    procedure CheckValue(const expected: string; const value: TValue;
      kind: TTypeKind);
  protected
    procedure SetUp; override;
  published
    procedure TestInteger;
    procedure TestEnumeration;
    procedure TestFloat;
{$IFNDEF NEXTGEN}
    procedure TestChar;
    procedure TestString;
    procedure TestLString;
    procedure TestWString;
{$ENDIF}
    procedure TestSet;
    procedure TestWChar;
    procedure TestInt64;
    procedure TestUString;
    procedure TestClassRef;
    procedure TestPointer;
    procedure TestUnsupported;
  end;

  {$ENDREGION}


  {$REGION 'TTestReflectionTypeSerializer'}

  TTestReflectionTypeSerializer = class(TSerializerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestClass;
    procedure TestNestedClass;
    procedure TestNestingClass;
    procedure TestRecord;
    procedure TestUnsupported;
  end;

  {$ENDREGION}


  {$REGION 'TTestInterfaceSerializer'}

  TTestInterfaceSerializer = class(TSerializerTestCase)
  private
    fController: TLoggerController;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInterfacedObject;
    procedure TestInterfacedNoObject;
    procedure TestUnsupported;
  end;

  {$ENDREGION}


  {$REGION 'TTestArrayOfValueSerializer'}

  TTestArrayOfValueSerializer = class(TSerializerTestCase)
  published
    procedure TestSimple;
    procedure TestNested;
    procedure TestUnsupported;
  end;

  {$ENDREGION}


implementation


{$REGION 'Helpers'}

type
  TInterfacedObjectAccess = class(TInterfacedObject);

{$ENDREGION}


{$REGION 'TSerializerTestCase'}

procedure TSerializerTestCase.SetSerializer(const value: TSerializerBase);
begin
  if value <> fSerializer then
  begin
    if Assigned(fSerializer) then
      TInterfacedObjectAccess(fSerializer)._Release;
    fSerializer := value;
    if Assigned(fSerializer) then
      TInterfacedObjectAccess(fSerializer)._AddRef;
  end;
end;

procedure TSerializerTestCase.TearDown;
begin
  Serializer := nil;
  inherited;
end;

procedure TSerializerTestCase.CheckUnsupported(supportedTypeKinds: TTypeKinds);
var
  typeInfo: TTypeInfo;
  kind: TTypeKind;
begin
  typeInfo := Default(TTypeInfo);

  for kind in tkAny - supportedTypeKinds do
  begin
    typeInfo.Kind := kind;
    CheckFalse(fSerializer.CanHandleType(@typeInfo));
  end;
end;

{$ENDREGION}


{$REGION 'TTestSimpleTypeSerializer'}

procedure TTestSimpleTypeSerializer.CheckValue(const expected: string;
  const value: TValue; kind: TTypeKind);
var
  result: string;
begin
  Assert(kind = value.Kind);

  CheckTrue(Serializer.CanHandleType(value.TypeInfo));
  result := Serializer.Serialize(nil, value);

  CheckEquals(expected, result);
end;

procedure TTestSimpleTypeSerializer.SetUp;
begin
  inherited;
  Serializer := TSimpleTypeSerializer.Create;
end;

procedure TTestSimpleTypeSerializer.TestClassRef;
begin
  CheckValue('(class ''TSampleObject'' @ ' +
    IntToHex(NativeInt(TSampleObject), SizeOf(Pointer) * 2) + ')',
    TValue.From(TClass(TSampleObject)), tkClassRef);
end;

procedure TTestSimpleTypeSerializer.TestEnumeration;
begin
  CheckValue('Warn', TValue.From(TLogLevel.Warn), tkEnumeration);
  CheckValue('True', TValue.From(True), tkEnumeration);
end;

procedure TTestSimpleTypeSerializer.TestFloat;
begin
{$IFDEF DELPHI2010}
  CheckValue('3' + DecimalSeparator + '14',
    TValue.From<Double>(3.14), tkFloat);
{$ELSE}
  CheckValue('3' + FormatSettings.DecimalSeparator + '14',
    TValue.From<Double>(3.14), tkFloat);
{$ENDIF}
end;

procedure TTestSimpleTypeSerializer.TestInt64;
begin
  CheckValue('12345678901234567890',
    TValue.From<UInt64>(12345678901234567890), tkInt64);
end;

procedure TTestSimpleTypeSerializer.TestInteger;
begin
  CheckValue('1', TValue.From(Integer(1)), tkInteger);
end;

procedure TTestSimpleTypeSerializer.TestPointer;
begin
{$IF SizeOf(Pointer) = 4}
  CheckValue('(pointer @ 12345678)', TValue.From(Pointer($12345678)), tkPointer);
{$ELSEIF SizeOf(Pointer) = 8}
  CheckValue('(pointer @ 1234567890ABCDEF)',
    TValue.From(Pointer($1234567890ABCDEF)), tkPointer);
{$ELSE}
  {$MESSAGE FATAL 'Unsupported CPU'}
{$IFEND}
end;

procedure TTestSimpleTypeSerializer.TestSet;
begin
  CheckValue('[SerializedData]', TValue.From([TLogEventType.SerializedData]), tkSet);
end;

{$IFNDEF NEXTGEN}
procedure TTestSimpleTypeSerializer.TestChar;
begin
  CheckValue('A', TValue.From(AnsiChar('A')), tkChar);
end;

procedure TTestSimpleTypeSerializer.TestString;
begin
  CheckValue('test', TValue.From(ShortString('test')), tkString);
end;

procedure TTestSimpleTypeSerializer.TestLString;
begin
  CheckValue('test', TValue.From(AnsiString('test')), tkLString);
end;

procedure TTestSimpleTypeSerializer.TestWString;
begin
  CheckValue('test', TValue.From(WideString('test')), tkWString);
end;
{$ENDIF}

procedure TTestSimpleTypeSerializer.TestUnsupported;
begin
  CheckUnsupported([tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      tkSet, tkWChar, tkLString, tkWString, tkInt64, tkUString, tkClassRef,
      tkPointer]);
end;

procedure TTestSimpleTypeSerializer.TestUString;
begin
  CheckValue('test', TValue.From(UnicodeString('test')), tkUString);
end;

procedure TTestSimpleTypeSerializer.TestWChar;
begin
  CheckValue('A', TValue.From(WideChar('A')), tkWChar);
end;

{$ENDREGION}


{$REGION 'TTestReflectionTypeSerializer'}

procedure TTestReflectionTypeSerializer.SetUp;
begin
  inherited;
  Serializer := TReflectionTypeSerializer.Create;
end;

procedure TTestReflectionTypeSerializer.TestClass;
var
  o: TSampleObject;
  result: string;
  s: string;
begin
  CheckTrue(Serializer.CanHandleType(TypeInfo(TSampleObject)));

  o := TSampleObject.Create;
  try
    o.fString := 'test';
    result := Serializer.Serialize(nil, o);
    s :=
      '(TSampleObject @ ' + IntToHex(NativeInt(o), SizeOf(Pointer) * 2) + ')('#$A +
      '  fObject = (empty)'#$A +
      '  fString = test'#$A +
      '  PObject = (empty)'#$A +
      '  PString = test'#$A +
      '  RefCount = ' +
{$IFDEF NEXTGEN}
      '3'#$A +
      '  Disposed = False' +
{$ELSE}
      '0' +
{$ENDIF}
      ')';
  finally
    o.Free;
  end;

  CheckEquals(s, result);
end;

procedure TTestReflectionTypeSerializer.TestNestedClass;
var
  controller: Shared<TLoggerController>;
  o: TSampleObject;
  result: string;
  s, c: string;
  value: TValue;
  parent: ITypeSerializer;
begin
  controller := TLoggerController.Create;
  controller.Value.AddEventConverter(Serializer);
  parent := TReflectionTypeSerializer.Create([mvPublic], True);
  o := TSampleObject.Create;
  try
{$IFDEF AUTOREFCOUNT}
    //We're also testing how WeakRefs work
    CheckEquals(1, TSampleObject.Instances);
{$ENDIF}
    o.fString := 'test';
    o.fObject := o;
    value := o;
    result := parent.Serialize(controller, value);

    s := IntToHex(NativeInt(o), SizeOf(Pointer) * 2);
    s := '(TSampleObject @ ' + s + ')';
    c := s + '('#$A +
      '    fObject = ' + s + #$A +
      '    fString = test'#$A +
      '    PObject = ' + s + #$A +
      '    PString = test'#$A +
      '    RefCount = ' +
{$IFDEF AUTOREFCOUNT}
      '5'#$A +
      '    Disposed = False' +
{$ELSE}
      '0' +
{$ENDIF}
      ')';
    s :=
      s+'('#$A +
      '  fObject = ' + c + #$A +
      '  fString = test'#$A +
      '  PObject = ' + c + #$A +
      '  PString = test'#$A +
      '  RefCount = ' +
{$IFDEF AUTOREFCOUNT}
      '3'#$A +
      '  Disposed = False' +
{$ELSE}
      '0' +
{$ENDIF}
      ')';

      value := nil; //Release the RefCount on ARC
  finally
    o.Free;
  end;

  CheckEquals(s, result);
{$IFDEF AUTOREFCOUNT}
  //Lets make sure we're not leaking (ie. the instance was freed and all
  //references removed)
  CheckEquals(0, TSampleObject.Instances, 'Leak detected!');
{$ENDIF}
end;

procedure TTestReflectionTypeSerializer.TestNestingClass;
var
  controller: Shared<TLoggerController>;
  o: TSampleObject;
  result: string;
begin
  Serializer := TReflectionTypeSerializer.Create([mvPublic], True);
  controller := TLoggerController.Create;
  controller.Value.AddEventConverter(Serializer);
  o := TSampleObject.Create;
  try
    o.fObject := o;
    result := Serializer.Serialize(controller, o);
  finally
    o.Free;
  end;

  //We generate rather big blob of data
  //If nesting doesn't work stack overflow would be raised
  Check(Length(result) > 1000);
end;

procedure TTestReflectionTypeSerializer.TestRecord;
var
  r: TSampleRecord;
  result: string;
  s: string;
begin
  CheckTrue(Serializer.CanHandleType(TypeInfo(TSampleRecord)));

  r.fObject := nil;
  r.fString := 'test';
  result := Serializer.Serialize(nil, TValue.From(r));

  s :=
    '(TSampleRecord)('#$A +
    '  fObject = (empty)'#$A +
    '  fString = test)';

  CheckEquals(s, result);
end;

procedure TTestReflectionTypeSerializer.TestUnsupported;
begin
  CheckUnsupported([tkRecord, tkClass]);
end;

{$ENDREGION}


{$REGION 'TTestInterfaceSerializer'}

procedure TTestInterfaceSerializer.SetUp;
begin
  fController := TLoggerController.Create;
  TInterfacedObjectAccess(fController)._AddRef;
  fController.AddEventConverter(TReflectionTypeSerializer.Create);
  Serializer := TInterfaceSerializer.Create;
  inherited;
end;

procedure TTestInterfaceSerializer.TearDown;
begin
  inherited;
  TInterfacedObjectAccess(fController)._Release;
  fController := nil;
end;

procedure TTestInterfaceSerializer.TestInterfacedNoObject;
var
  i: IComparer<string>;
  result: string;
begin
  i := TComparer<string>.Default;

  result := Serializer.Serialize(fController, TValue.From(i));

  CheckEquals('(IComparer<System.string>)', result);
end;

procedure TTestInterfaceSerializer.TestInterfacedObject;
var
  o: TSampleObject;
  i: IInterface;
  value: TValue;
  result: string;
  s: string;
begin
  CheckTrue(Serializer.CanHandleType(TypeInfo(IInterface)));

  o := TSampleObject.Create;
  i := o;
  o.fString := 'test';

  value := TValue.From(i);
  result := Serializer.Serialize(fController, value);

  s :=
    '(IInterface): (TSampleObject @ ' + IntToHex(NativeInt(o), SizeOf(Pointer) * 2) + ')('#$A +
    '  fObject = (empty)'#$A +
    '  fString = test'#$A +
    '  PObject = (empty)'#$A +
    '  PString = test'#$A +
    '  RefCount = ' +
{$IFDEF NEXTGEN}
      '6'#$A +
      '  Disposed = False' +
{$ELSE}
      '3' +
{$ENDIF}
      ')';

{$IFDEF AUTOREFCOUNT}
  o := nil;
  i := nil;
{$ENDIF}
  value := nil; //Release the RefCount on ARC

  CheckEquals(s, result);
end;

procedure TTestInterfaceSerializer.TestUnsupported;
begin
  CheckUnsupported([tkInterface]);
end;

{$ENDREGION}


{$REGION 'TTestArrayOfValueSerializer'}

procedure TTestArrayOfValueSerializer.TestNested;
var
  controller: Shared<TLoggerController>;
  values: TArray<TValue>;
  result: string;
  i: IInterface;
  s: string;
begin
  Serializer := TArrayOfValueSerializer.Create(True);
  controller := TLoggerController.Create;
  controller.Value.AddEventConverter(TReflectionTypeSerializer.Create);
  controller.Value.AddEventConverter(TInterfaceSerializer.Create);

  i := TSampleObject.Create;
  values := TArray.Copy<TValue>([1, 'text', TValue.From(i)]);

  result := Serializer.Serialize(controller, TValue.From(values));

  s :=
    '(IInterface): (TSampleObject @ ' + IntToHex(NativeInt(TObject(i)), SizeOf(Pointer) * 2) + ')('#$A +
    '  fObject = (empty)'#$A +
    '  fString = '#$A +
    '  PObject = (empty)'#$A +
    '  PString = '#$A +
    '  RefCount = ' +
{$IFDEF NEXTGEN}
      '5'#$A +
      '  Disposed = False' +
{$ELSE}
{$IFDEF DELPHI2010}
      '5' +
{$ELSE}
      '3' +
{$ENDIF}
{$ENDIF}
      ')';
  values := nil;
  CheckEquals('[1, text, ' + s + ']', result);
end;

procedure TTestArrayOfValueSerializer.TestSimple;
var
  values: TArray<TValue>;
  result: string;
  r: TSampleRecord;
begin
  Serializer := TArrayOfValueSerializer.Create;

  CheckTrue(Serializer.CanHandleType(TypeInfo(TArray<TValue>)));

  values := TArray.Copy<TValue>([1, 'text', TValue.From(r)]);

  result := Serializer.Serialize(nil, TValue.From(values));

  CheckEquals('[1, text, (TSampleRecord)]', result);
end;

procedure TTestArrayOfValueSerializer.TestUnsupported;
begin
  Serializer := TArrayOfValueSerializer.Create;
  CheckUnsupported([tkInterface]);
end;

{$ENDREGION}


end.
