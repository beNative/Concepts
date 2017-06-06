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

{$IFNDEF SPRING_INC}
{$DEFINE SPRING_INC}

// Import JEDI Development environment directives
// Will assert correct CPU type and defaults to CPU386 if CPU type is unknown
// (Delphi older than XE3).
{$I jedi.inc}

{$T-,X+,H+,B-}

{$IFDEF DELPHIXE4_UP}
  {$LEGACYIFEND ON}
{$ENDIF}

// JEDI does not handle CPUARM
{$IFDEF CPUARM}
  {$UNDEF CPU386}
  {$UNDEF CPU32}
  {$UNDEF CPUASM}
  {$DEFINE PUREPASCAL}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE PUREPASCAL}
{$ENDIF}

// JEDI will assert that at least CPU386 is defined
{$IFDEF CPU386}
  // Keep this for older delphi compatibility (older than XE3)
  {$IFNDEF CPUX86}
    {$DEFINE CPUX86}
  {$ENDIF}
{$ENDIF}

{$IFNDEF DELPHIXE8_UP}
  {$IFDEF CPUX64}
    {$DEFINE CPU64BITS}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  // Use the unused vmtAutoTable slot in the class VMT to store the init table
  // otherwise a global dictionary will be used which is slightly slower
  {$DEFINE USE_VMTAUTOTABLE}
{$ELSE}
  // cannot use the vmtAutoTable slot on any platform except Windows (yet)
  {$UNDEF USE_VMTAUTOTABLE}
{$ENDIF}

// This will enable implicit assignment compatibility of Variant and Nullable<T>
// This reduces type safety as the compiler might try to turn types into Variants
// first to then assign them to a nullable
// Example: n of type Nullable<Integer> might be assigned a string because the
// compiler tries to turn the string into an Integer via VarToInt.
// This is against the type safety provided by Nullable<T> where it only accepts
// values of the type T.
// Use this define to provide backwards compatibility. It is enabled by default
// for now to find possible issues rather than changing the behavior without notice
{$DEFINE UNSAFE_NULLABLE}
{$IFDEF UNSAFE_NULLABLE}
  // Raise deprecated warning in places where implicit operators are being used
  {$DEFINE UNSAFE_NULLABLE_WARN}
{$ENDIF}
// This will enable the implicit operator for unboxing a nullable value
// Be aware that this may possibly raise an exception if the nullable does not
// contain a value, i.e. is null
{$DEFINE IMPLICIT_NULLABLE}
{$IFDEF IMPLICIT_NULLABLE}
  // Raise deprecated warning in places where implicit operators are being used
  {.$DEFINE IMPLICIT_NULLABLE_WARN}
{$ENDIF}

// This will disable the execution of guard code when assertions are turned off
{$IFNDEF ASSERTIONS_ON}
  {$IFNDEF SPRING_ENABLE_GUARD}
    {$DEFINE SPRING_DISABLE_GUARD}
  {$ENDIF}
{$ENDIF}
{$IFNDEF RELEASE}
  {$IFNDEF SPRING_DISABLE_GUARD}
    {$DEFINE SPRING_ENABLE_GUARD}
  {$ENDIF}
{$ENDIF}

{$IFNDEF DELPHIXE2_UP}
  {$DEFINE SPRING_DISABLE_GRAPHICS}
{$ENDIF}

// If we are running on Android or iOS, we are sure we are running using FMX.
// If FMX is to be used on x86/x64 FMX conditional should be defined
// in the project settings by hand
{$IFDEF ANDROID}
  {$DEFINE FMX}
{$ENDIF}
{$IFDEF IOS}
  {$DEFINE FMX}
{$ENDIF}
{$IFDEF MACOS}
  {$DEFINE FMX}
{$ENDIF}

// This option is needed to enable code browsing (aka Ctrl+Click)
// It does not affect the binary size or generated code
{$DEFINITIONINFO ON}

{$IFNDEF DELPHI2010_UP}
  {$MESSAGE ERROR 'This framework requires Delphi 2010 or higher.'}
{$ENDIF}

{$IFNDEF NEXTGEN}
  {$WARN UNIT_PLATFORM OFF}     // Turn off UNIT_PLATFORM warning
  {$WARN SYMBOL_PLATFORM OFF}   // Turn off SYMBOL_PLATFORM warning
{$ELSE}
  {$WARN SYMBOL_DEPRECATED OFF} // Turn off SYMBOL_DEPRECATED warning
{$ENDIF}

{$ENDIF ~SPRING_INC}
