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

unit Spring.ResourceStrings;

interface

resourcestring

  {$REGION 'Spring'}

  STypeConversionError         = '%s cannot be converted to %s';
  SNotSupportedException       = '%s is not supported.';
  SNotImplementedException     = '%s is not implemented.';
  SNotSupportedOperation       = 'Not supported operation.';
  SNoDelegateAssigned          = 'No delegate has been assigned.';
  SNotClassType                = '"%s" is not a class type.';
  STypeNotSupported            = 'Type is not supported: %s';
  SValueFactoryReturnedNil     = 'ValueFactory returned nil.';
  STypeInfoNotFound            = 'TypeInfo not found for type: %s';

  SArgumentOutOfRangeException = 'Specified argument was out of the range of valid values: %s';
  SArgumentNilException        = 'Specified argument cannot be nil: %s';
  SInvalidEnumArgument         = 'Invalid enum argument: %s';
  SInvalidSetArgument          = 'Invalid set argument: %s';
  SUnexpectedTypeKindArgument  = 'Unexpected type kind %s for the argument %s.';
  SInvalidOperationBufferSizeShouldBeSame = 'Buffer size should be the same.';
  SInvalidArgumentFormat = 'Invalid format for argument "%s".';
  SInvalidTypeCast             = 'Invalid cast: %s to %s.';

  SCannotAccessRegistryKey     = 'Cannot access the registry key: %s.';

  SAbstractClassCreation       = 'Cannot create the abstract class: %s.';

  SFileNotFoundException          = 'File not found: %s';
  SDirectoryNotFoundException     = 'Directory not found: %s';
  SNullableHasNoValue             = 'Nullable must have a value.';
  SValueDoesNotContainNullable    = 'Value does not contain a nullable value.';
  STypeNotRegistered              = '%s was not registered.';
  SServiceNotExists               = 'The service "%s" does not exist.';
  STimeoutException               = 'Timeout';
  SInsufficientMemoryException    = 'Insufficient memory.';

  SUnexpectedArgumentLength = 'Unexpected parameter length.';

  SCriticalSectionNotInitialized = 'Critical section was not initialized';

  SServiceLocatorNotInitialized = 'The global ServiceLocator has not been initialized.';

  SNoTypeInfo          = 'No type information found.';
  SUnsupportedCallingConvention = 'Unsupported calling convention, only standard calling convention is supported';
  STypeParameterShouldBeMethod = 'The type parameter "%s" should be an event or anonymous method type.';
  STypeParameterContainsNoRtti = 'The type parameter "%s" contains no RTTI. Please check for {$M+}.';
  SUnexpectedTypeKind  = 'Unexpected type kind: %s.';
  SNotEnumeratedType   = 'Type "%s" is not enumerated type.';
  SIncorrectFormat     = 'Unable to convert %s.';
  SInvalidDateTime     = '"%s" is not a valid date and time.';
  SIllegalFieldCount   = 'fieldCount is more than the number of components defined in the current Version object.';

  SBadObjectInheritance = 'Argument %s of type %s does not inherit from type %s.';

  SUnknownDescription  = 'Unknown';
  SVersionDescription  = 'Version';
//  SOSVersionStringFormat = '%s Version %s %s';

  SSizeStringFormat    = '%s %s';   // e.g. '20.5 MB'

//  SByteDescription     = 'byte';
  SBytesDescription    = 'bytes';
  SKBDescription       = 'KB';
  SMBDescription       = 'MB';
  SGBDescription       = 'GB';
  STBDescription       = 'TB';

  SArraysIdentical = 'Source and target array must not be identical';

  SIncompatibleTypes = 'Incompatible types: "%s" and "%s"';

  {$ENDREGION}


  {$REGION 'Spring.Collections'}

  SSequenceContainsNoElements = 'Sequence contains no elements.';
  SSequenceContainsNoMatchingElement = 'Sequence contains no matching element.';
  SSequenceContainsMoreThanOneMatchingElement = 'Sequence contains more than one matching element.';
  SSequenceContainsMoreThanOneElement = 'Sequence contains more than one element.';

  SLinkedListNodeIsAttached = 'The LinkedList node already belongs to a LinkedList.';

  SArgument_DuplicateKey = 'An item with the same key has already been added.';
  SArgument_KeyNotFound        = 'The given key was not present in the dictionary.';
  SArgument_InvalidIndexCount = 'Index and count were out of bounds for the array or count is greater than the number of elements from index to the end of the source collection.';

  SArgumentOutOfRange_NeedNonNegNum = 'Non-negative number required. Parameter name: %s';
  SArgumentOutOfRange_Index = 'Index must be non-negative and less than the size of the collection.';
  SArgumentOutOfRange_Count = 'Count must be non-negative and refer to a location within the collection.';
  SArgumentOutOfRange_Capacity = 'Capacity must be greater than the current size of the collection.';

  SInvalidOperation_EnumFailedVersion = 'Collection was modified; enumeration operation may not execute.';

  {$ENDREGION}


  {$REGION 'Spring.Helpers'}

  SNotGenericType = 'Type "%s" is not a generic type.';

  SInvalidOperation_GetValue = 'The GetValue method works only for properties/fields.';
  SInvalidOperation_SetValue = 'The SetValue method works only for properties/fields.';
  SInvalidGuidArray = 'Byte array for GUID must be exactly %d bytes long';

  {$ENDREGION}


  {$REGION 'Spring.Reflection'}

  SNoConstructorFound = 'No constructor with matching signature found for type: %s';

  // Value Converters
  SCouldNotConvertValue = 'Could not convert value: %s to %s.';
  SEmptySourceTypeKind = 'Empty source TypeKind argument set.';
  SEmptyTargetTypeKind = 'Empty target TypeKind argument set.';

  // Value Expressions
  SCannotModifyReadOnlyValue      = 'Cannot modify read-only value.';
  SInvalidExpressionPath          = 'Invalid expression path %s.';
  SUnexpectedToken                = 'Unexpected expression part %s.';
  SInvalidExpressionSyntax        = 'Invalid expression syntax %s.';
  SCouldNotFindPath               = 'Could not find expression path %s.';

  {$ENDREGION}


  {$REGION 'Spring.Times'}

  SNoMatchAny = 'anytime, but was %2:d times';
  SNoMatchAtLeast = 'at least %0:d times, but was %2:d times';
  SNoMatchAtLeastOnce = 'at least once, but was never';
  SNoMatchAtMost = 'at most %1:d times, but was %2:d times';
  SNoMatchAtMostOnce = 'at most once, but was %2:d times';
  SNoMatchBetween = 'between %0:d and %1:d times, but was %2:d times';
  SNoMatchExactly = 'exactly %0:d times, but was %2:d times';
  SNoMatchNever = 'never, but was %2:d times';
  SNoMatchOnce = 'once, but was %2:d times';

  {$ENDREGION}


  {$REGION 'Spring.Cryptography'}

  SIllegalBlockSize = 'Illegal block size: %d.';
  SIllegalKeySize = 'Illegal key size: %d.';
  SIllegalIVSize = 'Illegal IV size: %d.';
  SPaddingModeMissing = 'Padding mode is missing';
  SInvalidCipherText = 'Illegal cipher text.';
  SNotSupportedCipherMode = 'The cipher mode "%s" is not supported.';

  {$ENDREGION}


  {$REGION 'Spring.Utils'}

  SDriveNotReady              = 'Drive "%s" is not ready.';

  SUnknownDriveDescription    = 'Unknown Drive';
  SNoRootDirectoryDescription = 'No Root Directory';
  SRemovableDescription       = 'Removable Drive';
  SFixedDescription           = 'Fixed Drive';
  SNetworkDescription         = 'Network Drive';
  SCDRomDescription           = 'CD-Rom Drive';
  SRamDescription             = 'Ram Drive';

  SUnknownOSDescription       = 'Unknown Operating System';
  SWin95Description           = 'Microsoft Windows 95';
  SWin98Description           = 'Microsoft Windows 98';
  SWinMEDescription           = 'Microsoft Windows ME';
  SWinNT351Description        = 'Microsoft Windows NT 3.51';
  SWinNT40Description         = 'Microsoft Windows NT 4';
  SWinServer2000Description   = 'Microsoft Windows Server 2000';
  SWinXPDescription           = 'Microsoft Windows XP';
  SWinServer2003Description   = 'Microsoft Windows Server 2003';
  SWinVistaDescription        = 'Microsoft Windows Vista';
  SWinServer2008Description   = 'Microsoft Windows Server 2008';
  SWin7Description            = 'Microsoft Windows 7';
  SWinServer2008R2Description = 'Microsoft Windows Server 2008 R2';
  SWin8Description            = 'Microsoft Windows 8';
  SWinServer2012Description   = 'Microsoft Windows Server 2012';
  SWin81Description           = 'Microsoft Windows 8.1';
  SWinServer2012R2Description = 'Microsoft Windows Server 2012 R2';


  SFileVersionInfoFormat =
    'File:             %s' + #13#10 +
    'InternalName:     %s' + #13#10 +
    'OriginalFilename: %s' + #13#10 +
    'FileVersion:      %s' + #13#10 +
    'FileDescription:  %s' + #13#10 +
    'Product:          %s' + #13#10 +
    'ProductVersion:   %s' + #13#10 +
    'Debug:            %s' + #13#10 +
    'Patched:          %s' + #13#10 +
    'PreRelease:       %s' + #13#10 +
    'PrivateBuild:     %s' + #13#10 +
    'SpecialBuild:     %s' + #13#10 +
    'Language:         %s' + #13#10;


  SAtLeastTwoElements = 'There must be at least two elements.';
  SIllegalArgumentQuantity = 'Invalid argument "quantity": %d.';

  SInvalidOperationCurrent = 'Invalid operation. The enumerable collection is empty.';

  SEnumNotStarted = 'Enum not started.';
  SEnumEnded = 'Enum ended.';

  {$ENDREGION}


implementation

end.
