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

unit Spring.TestRegistration; // should be platform neutral

{$I Spring.Tests.inc}

interface

procedure RegisterTestCases;

implementation

uses
  TestFramework,
  Spring.TestUtils,
  Spring.Tests.Base,
  Spring.Tests.Collections,
  Spring.Tests.Collections.Dictionaries,
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
  Spring.Tests.Interception,
  Spring.Tests.Mocking,
  Spring.Tests.Pool,
  Spring.Tests.Testing,
  Spring.Tests.Utils,
  Spring.Tests.Cryptography;

procedure RegisterTestCases;
type
  TTestKind = (None,
    BaseCommon, BaseCollections, BaseLogging,
    CoreContainer, CoreInterception, CoreLogging,
    Aux);
const
  TestKinds = [
    BaseCommon,
    BaseCollections,
    BaseLogging,
    CoreContainer,
    CoreInterception,
    CoreLogging,
    Aux,
    None
  ];
begin
  if BaseCommon in TestKinds then
  begin
    RegisterTests('Spring.Base', [
      TTestNullableInteger.Suite,
      TTestNullableBoolean.Suite,
      TTestNullableDateTime.Suite,
      TTestNullableInt64.Suite,
    {$IFDEF DELPHIXE5_UP}
      TTestNullableCustomRecord.Suite,
    {$ENDIF}
      TTestGuard.Suite,
      TTestLazy.Suite,
      TTestMulticastEvent.Suite,
      TTestMulticastEventStackSize.Suite,
      TTestSpringEventsMethods.Suite,
      TTestTuplesDouble.Suite,
      TTestTuplesTriple.Suite,
      TTestTuplesQuadruple.Suite,
      TTestShared.Suite,
      TTestVector.Suite,
      TTestValueHelper.Suite,
      TArrayTest.Suite,
      TWeakTest.Suite,
      TTestVirtualClass.Suite,
      TTestEnum.Suite,
      TTestBaseRoutines.Suite,
      TTestManagedObject.Suite,
      TSortTest.Suite,
      TTestHash.Suite
    ]);
  end;

  if BaseCollections in TestKinds then
  begin
    RegisterTests('Spring.Base.Collections', [
      TTestEmptyHashSet.Suite,
      TTestNormalHashSet.Suite,
      TTestIntegerList.Suite,
      TTestStringList.Suite,
      TTestSortedList.Suite,
      TTestEmptyStackOfStrings.Suite,
      TTestStackOfInteger.Suite,
      TTestStackOfTBytes.Suite,
      TTestStackOfIntegerChangedEvent.Suite,
      TTestEmptyQueueOfInteger.Suite,
      TTestEmptyDequeOfInteger.Suite,
      TTestQueueOfInteger.Suite,
      TTestDequeOfInteger.Suite,
      TTestBoundedDeque.Suite,
      TTestEvictingDeque.Suite,
      TTestQueueOfTBytes.Suite,
      TTestQueueOfIntegerChangedEvent.Suite,
      TTestDequeOfIntegerChangedEvent.Suite,
      TTestListOfIntegerAsIEnumerable.Suite,
      TTestLinkedList.Suite,
      TTestObjectList.Suite,
      TTestInterfaceList.Suite,
      TTestCollectionList.Suite,
      TTestEnumerable.Suite,
      TTestListMultiMap.Suite,
      TTestHashMultiMap.Suite,
      TTestTreeMultiMap.Suite,
      TTestSortedListMultiMap.Suite,
      TTestSortedHashMultiMap.Suite,
      TTestSortedTreeMultiMap.Suite,
      TTestObjectStack.Suite,
      TTestObjectQueue.Suite,
      TTestObjectDeque.Suite,
      TTestRedBlackTreeInteger.Suite,
      TTestRedBlackTreeIntegerString.Suite,
      TTestSet.Suite,
      TTestSortedSet.Suite,

      TSkipTests.Suite,
      TSkipLastTests.Suite,
      TTakeTests.Suite,
      TTakeLastTests.Suite,

      TAtLeastTests.Suite,
      TAtMostTests.Suite,
      TBetweenTests.Suite,
      TExactlyTests.Suite,

      TMemoizeTests.Suite,

      TTestChunk.Suite
    ]);

    RegisterTests('Spring.Base.Collections.Dictionaries', [
      TTestBidiDictionary.Suite,
      TTestBidiDictionaryChangedEvent.Suite,
      TTestBidiDictionaryChangedEventInverse.Suite,
      TTestBidiDictionaryInverse.Suite,
      TTestBidiDictionaryInverseKeyComparer.Suite,
      TTestBidiDictionaryInverseOwnership.Suite,
      TTestBidiDictionaryInverseValueComparer.Suite,
      TTestBidiDictionaryKeyComparer.Suite,
      TTestBidiDictionaryOwnership.Suite,
      TTestBidiDictionaryValueComparer.Suite,
      TTestDictionary.Suite,
      TTestDictionaryChangedEvent.Suite,
      TTestDictionaryCreationExceptions.Suite,
      TTestDictionaryKeyComparer.Suite,
      TTestDictionaryOwnership.Suite,
      TTestDictionaryValueComparer.Suite,
      TTestSortedDictionary.Suite,
      TTestSortedDictionaryChangedEvent.Suite
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

    RegisterTests('Spring.Base.Collections.MultiMaps', [
      TTestListMultiMapChangedEvent.Suite,
      TTestHashMultiMapChangedEvent.Suite,
      TTestTreeMultiMapChangedEvent.Suite,
      TTestSortedListMultiMapChangedEvent.Suite,
      TTestSortedHashMultiMapChangedEvent.Suite,
      TTestSortedTreeMultiMapChangedEvent.Suite
    ]);

    RegisterTests('Spring.Base.Collections.MultiSets', [
      TTestHashMultiSet.Suite,
      TTestTreeMultiSet.Suite,
      TTestHashMultiSetChangedEvent.Suite,
      TTestTreeMultiSetChangedEvent.Suite
    ]);
  end;

  if BaseCommon in TestKinds then
  begin
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
      TTestFromWideString.Suite,
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
      TTestTryStrToDateTimeFmt.Suite,
      TTestSplitNullTerminatedStrings.Suite
    ]);
  end;

  if BaseLogging in TestKinds then
  begin
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
  end;

  if CoreContainer in TestKinds then
  begin
    RegisterTests('Spring.Core.Container', [
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
      TTestCircularReferenceLazySingleton.Suite,
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
  end;

  if CoreLogging in TestKinds then
  begin
    RegisterTests('Spring.Core.Logging', [
      TTestLogInsideContainer.Suite,
      TTestLogSubResolverAndConfiguration.Suite,
      TTestLoggingConfiguration.Suite,
      TTestLoggingConfigurationBuilder.Suite
    ]);
  end;

  if CoreInterception in TestKinds then
  begin
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
  end;

  if Aux in TestKinds then
  begin
    RegisterTests('Spring.Extensions.Utils', [
      TTestVersion.Suite,
      TTestEnvironment.Suite
    ]);

    RegisterTests('Spring.Extensions.Cryptography', [
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

    RegisterTests('Spring.Testing', [
      TSelfTest.Suite,
      TDataDrivenTest.Suite,
      TSuiteSetUpTearDownTest.Suite
    ]);
  end;
end;

end.
