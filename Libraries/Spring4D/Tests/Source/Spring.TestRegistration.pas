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

unit Spring.TestRegistration; // should be platform neutral

{$I Spring.Tests.inc}

interface

procedure RegisterTestCases();

implementation

uses
  TestFramework,
  TestExtensions,
  Spring.TestUtils,
  Spring.Tests.Base,
  Spring.Tests.Collections,
  Spring.Tests.Collections.Extensions,
  Spring.Tests.SystemUtils,
  Spring.Tests.DesignPatterns,
  Spring.Tests.Helpers,
  Spring.Tests.Reflection,
  Spring.Tests.ValueConverters,
  Spring.Tests.Logging,
  Spring.Tests.Logging.Serializers,
  Spring.Tests.Container,
  Spring.Tests.Container.LifetimeManager,
  Spring.Tests.Container.Logging,
{$IFNDEF DELPHI2010}
  Spring.Tests.Interception,
  Spring.Tests.Mocking,
{$ENDIF}
  Spring.Tests.Pool,
  Spring.Tests.Utils,
  Spring.Tests.Cryptography;

procedure RegisterTestCases();
begin
  RegisterTests('Spring.Base', [
    TRepeatedTest.Create(TTestNullableInteger.Suite, 3),
    TTestNullableBoolean.Suite,
    TTestNullableDateTime.Suite,
    TTestNullableInt64.Suite,
    TTestGuard.Suite,
    TTestLazy.Suite,
    TTestMulticastEvent.Suite,
    TTestMulticastEventStackSize.Suite,
    TTestSpringEventsMethods.Suite,
    TTestTuplesDouble.Suite,
    TTestTuplesTriple.Suite,
    TTestTuplesQuadruple.Suite,
    TTestOwned.Suite,
    TTestVector.Suite,
    TTestValueHelper.Suite,
    TArrayTest.Suite,
    TWeakTest.Suite,
    TTestVirtualClass.Suite
  ]);

{$IFNDEF DELPHI2010}
  RegisterTests('Spring.Base', [
    TTestManagedObject.Suite
  ]);
{$ENDIF}

  RegisterTests('Spring.Base.Collections', [
    TTestEmptyHashSet.Suite,
    TTestNormalHashSet.Suite,
    TTestIntegerList.Suite,
    TTestSortedList.Suite,
    TTestStringIntegerDictionary.Suite,
    TTestEmptyStringIntegerDictionary.Suite,
    TTestEmptyStackOfStrings.Suite,
    TTestStackOfInteger.Suite,
    TTestStackOfTBytes.Suite,
    TTestStackOfIntegerChangedEvent.Suite,
    TTestEmptyQueueOfInteger.Suite,
    TTestQueueOfInteger.Suite,
    TTestQueueOfTBytes.Suite,
    TTestQueueOfIntegerChangedEvent.Suite,
    TTestListOfIntegerAsIEnumerable.Suite,
    TTestLinkedList.Suite,
    TTestObjectList.Suite,
    TTestInterfaceList.Suite,
    TTestCollectionList.Suite,
    TTestEnumerable.Suite,
    TTestListAdapter.Suite,
    TTestMultiMap.Suite,
    TTestBidiDictionary.Suite,
    TTestObjectStack.Suite,
    TTestObjectQueue.Suite
  ]);

  RegisterTests('Spring.Base.Collections.Extensions', [
    TTestWhere.Suite,
    TTestSelect.Suite,
    TTestRange.Suite,
    TTestRepeated.Suite,
    TTestConcat.Suite,
    TTestSelectMany.Suite,
    TTestAny.Suite,
    TTestAll.Suite,
    TTestFirst.Suite,
    TTestSingle.Suite,
    TTestLast.Suite,
    TTestFirstOrDefault.Suite,
    TTestSingleOrDefault.Suite,
    TTestLastOrDefault.Suite,
    TTestDefaultIfEmpty.Suite,
    TTestDistinct.Suite,
    TTestUnion.Suite,
    TTestIntersect.Suite,
    TTestExcept.Suite,
    TTestToLookup.Suite,
    TTestJoin.Suite,
    TTestGroupBy.Suite,
    TTestGroupJoin.Suite,
    TTestTake.Suite,
    TTestSkip.Suite,
    TTestTakeWhile.Suite,
    TTestSkipWhile.Suite,
    TTestOrderBy.Suite,
    TTestOrderByDescending.Suite,
    TTestReverse.Suite,
    TTestMaxBy.Suite
  ]);

  RegisterTests('Spring.Base.DesignPatterns', [
    TTestSingleton.Suite
  ]);

  RegisterTests('Spring.Base.Helpers', [
    TTestRttiTypeHelper.Suite
  ]);

  RegisterTests('Spring.Base.Reflection', [
    TTestType.Suite,
    TTestMethodHelper.Suite
  ]);

  RegisterTests('Spring.Base.Reflection.ValueConverters', [
    TTestFromString.Suite,
{$IFNDEF NEXTGEN}
    TTestFromWideString.Suite,
{$ENDIF}
    TTestFromInteger.Suite,
    TTestFromCardinal.Suite,
    TTestFromSmallInt.Suite,
    TTestFromShortInt.Suite,
    TTestFromBoolean.Suite,
    TTestFromEnum.Suite,
    TTestFromFloat.Suite,
{$IFNDEF SPRING_DISABLE_GRAPHICS}
    TTestFromColor.Suite,
{$ENDIF}
    TTestFromCurrency.Suite,
    TTestFromDateTime.Suite,
    TTestFromDate.Suite,
    TTestFromTime.Suite,
    TTestFromObject.Suite,
    TTestFromNullable.Suite,
    TTestFromInterface.Suite,
    TTestCustomTypes.Suite
  ]);

  RegisterTests('Spring.Base.SystemUtils', [
    TTestSplitString.Suite,
    TTestTryConvertStrToDateTime.Suite,
    TTestSplitNullTerminatedStrings.Suite,
    TTestEnum.Suite
  ]);

  RegisterTests('Spring.Base.Logging', [
    TTestLoggerController.Suite,
    TTestLogger.Suite,
    TTestLogAppenderBase.Suite,
    TTestStreamLogAppender.Suite
  ]);

  RegisterTests('Spring.Base.Logging.Serializers', [
    TTestSimpleTypeSerializer.Suite,
    TTestReflectionTypeSerializer.Suite,
    TTestInterfaceSerializer.Suite,
    TTestArrayOfValueSerializer.Suite
  ]);

//  RegisterTests('Spring.Base.Reflection.ValueExpression', [
//    TTestValueExpression.Suite
//  ]);

  RegisterTests('Spring.Core.Container', [
{$IFDEF AUTOREFCOUNT}
    TTestGlobalContainer.Suite,
{$ENDIF}
    TTestEmptyContainer.Suite,
    TTestSimpleContainer.Suite,
    TTestDifferentServiceImplementations.Suite,
    TTestImplementsDifferentServices.Suite,
    TTestActivatorDelegate.Suite,
    TTestTypedInjectionByCoding.Suite,
    TTestTypedInjectionsByAttribute.Suite,
    TTestNamedInjectionsByCoding.Suite,
    TTestNamedInjectionsByAttribute.Suite,
    TTestDirectCircularDependency.Suite,
    TTestCrossedCircularDependency.Suite,
    TTestPerResolve.Suite,
    TTestImplementsAttribute.Suite,
    TTestRegisterInterfaces.Suite,
    TTestSingletonLifetimeManager.Suite,
    TTestTransientLifetimeManager.Suite,
    TTestCustomLifetimeManager.Suite,
    TTestRefCounting.Suite,
    TTestDefaultResolve.Suite,
    TTestInjectionByValue.Suite,
    TTestObjectPool.Suite,
    TTestResolverOverride.Suite,
    TTestRegisterInterfaceTypes.Suite,
    TTestLazyDependencies.Suite,
    TTestLazyDependenciesDetectRecursion.Suite,
    TTestManyDependencies.Suite,
    TTestDecorators.Suite
  ]);

  RegisterTests('Spring.Core.Logging', [
    TTestLogInsideContainer.Suite,
    TTestLogSubResolverAndConfiguration.Suite,
    TTestLoggingConfiguration.Suite,
    TTestLoggingConfigurationBuilder.Suite
  ]);

{$IFNDEF DELPHI2010}
  RegisterTests('Spring.Interception', [
    TFreezableTest.Suite,
    TProxyTest.Suite,
    TStorageTests.Suite,
    TTestInterception.Suite,
    TTestAutoMockingExtension.Suite,
    TParameterMatchingTests.Suite,
    ReceivedChecksForInputValueOfVarParams.Suite,
    MockReturnsOtherMockInDynamicMode.Suite,
    MockDynamicallySupportsOtherInterfaces.Suite,
    MockSequenceTest.Suite
  ]);
{$ENDIF}

  RegisterTests('Spring.Extensions.Utils', [
    TTestVersion.Suite,
    TTestEnvironment.Suite
  ]);

  RegisterTests('Spring.Extensions.Cryptography', [
//    TTestBuffer.Suite,
//    TTestEmptyBuffer.Suite,
//    TTestFiveByteBuffer.Suite,
    TTestCRC16.Suite,
    TTestCRC32.Suite,
    TTestMD5.Suite,
    TTestSHA1.Suite,
    TTestSHA256.Suite,
    TTestSHA384.Suite,
    TTestSHA512.Suite,
    TTestPaddingModeIsNone.Suite,
    TTestPaddingModeIsPKCS7.Suite,
    TTestPaddingModeIsZeros.Suite,
    TTestPaddingModeIsANSIX923.Suite,
    TTestPaddingModeIsISO10126.Suite,
    TTestDES.Suite,
    TTestTripleDES.Suite
  ]);

// Stefan Glienke - 2011/11/20:
// removed configuration and logging tests because they break other tests in Delphi 2010
// due to some bug in Rtti.TRttiPackage.MakeTypeLookupTable
// see https://forums.embarcadero.com/thread.jspa?threadID=54471
//
//  RegisterTests('Spring.Core.Configuration', [
//    TTestConfiguration.Suite
//  ]);
//
//  RegisterTests('Spring.Core.Logging', [
//     TTestLoggingConfig.Suite
//  ]);
end;

end.
