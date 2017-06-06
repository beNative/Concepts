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

program SpringTestsMobile;

{$I Spring.Tests.inc}

// Spring.TestBootstrap be the first unit in the uses clause since it specifies
// some units that need to be set in given order for FMX
uses
  Spring.TestBootstrap in '..\..\Source\Spring.TestBootstrap.pas',
  TestFramework,
  FMXTestRunner in '..\..\Source\dUnit\FMXTestRunner.pas' {FMXTestRunner},
  Spring.TestRegistration in '..\..\Source\Spring.TestRegistration.pas',
  Spring.TestRunner in '..\..\Source\Spring.TestRunner.pas',
  Spring.TestUtils in '..\..\Source\Spring.TestUtils.pas',
  Spring.Tests.Base in '..\..\Source\Base\Spring.Tests.Base.pas',
  Spring.Tests.Collections in '..\..\Source\Base\Spring.Tests.Collections.pas',
  Spring.Tests.Collections.Extensions in '..\..\Source\Base\Spring.Tests.Collections.Extensions.pas',
  Spring.Tests.DesignPatterns in '..\..\Source\Base\Spring.Tests.DesignPatterns.pas',
  Spring.Tests.Helpers in '..\..\Source\Base\Spring.Tests.Helpers.pas',
  Spring.Tests.Reflection in '..\..\Source\Base\Spring.Tests.Reflection.pas',
  Spring.Tests.ValueConverters in '..\..\Source\Base\Spring.Tests.ValueConverters.pas',
  Spring.Tests.SystemUtils in '..\..\Source\Base\Spring.Tests.SystemUtils.pas',
  Spring.Tests.Logging in '..\..\Source\Base\Spring.Tests.Logging.pas',
  Spring.Tests.Logging.Serializers in '..\..\Source\Base\Spring.Tests.Logging.Serializers.pas',
  Spring.Tests.Logging.Types in '..\..\Source\Base\Spring.Tests.Logging.Types.pas',
  Spring.Tests.Container.Components in '..\..\Source\Core\Spring.Tests.Container.Components.pas',
  Spring.Tests.Container.Interfaces in '..\..\Source\Core\Spring.Tests.Container.Interfaces.pas',
  Spring.Tests.Container.LifetimeManager in '..\..\Source\Core\Spring.Tests.Container.LifetimeManager.pas',
  Spring.Tests.Container in '..\..\Source\Core\Spring.Tests.Container.pas',
  Spring.Tests.Container.Logging in '..\..\Source\Core\Spring.Tests.Container.Logging.pas',
{$IFNDEF DELPHI2010}
  Spring.Tests.Interception in '..\..\Source\Core\Spring.Tests.Interception.pas',
  Spring.Tests.Interception.Types in '..\..\Source\Core\Spring.Tests.Interception.Types.pas',
{$ENDIF}
  Spring.Tests.Pool in '..\..\Source\Core\Spring.Tests.Pool.pas',
  Spring.Tests.Cryptography in '..\..\Source\Extensions\Spring.Tests.Cryptography.pas',
  Spring.Tests.Utils in '..\..\Source\Extensions\Spring.Tests.Utils.pas',
  Spring.Container;

begin
  RegisterTestCases();
  ReportMemoryLeaksOnShutdown := True;
  RunRegisteredTests();
  CleanupGlobalContainer;
  TestFramework.ClearRegistry;
end.
