{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.TestRunner;

{$I Spring.Tests.inc}

interface

procedure RunRegisteredTests;

implementation

uses
{$IFDEF CONSOLE_TESTRUNNER}
  SysUtils,
  Spring.TestUtils,
  {$IFDEF XMLOUTPUT}
  VSoft.DUnit.XMLTestRunner,
  TestFramework,
  {$ENDIF}
  TextTestRunner;
{$ELSE}
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnit;
  {$ELSE}
  {$IFNDEF FMX}
  Forms,
  GUITestRunner;
  {$ELSE}
  FMXTestRunner;
  {$ENDIF}
  {$ENDIF}
{$ENDIF CONSOLE_TESTRUNNER}

var
  OutputFile: string = 'Spring.Tests.Reports.xml';

procedure RunRegisteredTests;
begin
{$IFDEF CONSOLE_TESTRUNNER}
  {$IFDEF XMLOUTPUT}
  if ParamCount > 0 then
    OutputFile := ParamStr(1);
  WriteLn('Writing output to ' + OutputFile);
  WriteLn(Format('Running %d of %d test cases', [RegisteredTests.CountEnabledTestCases, RegisteredTests.CountTestCases]));
  ProcessTestResult(VSoft.DUnit.XMLTestRunner.RunRegisteredTests(OutputFile));
  {$ELSE}
  TextTestRunner.RunRegisteredTests{$IFNDEF AUTOREFCOUNT}.Free{$ENDIF};
  {$ENDIF}

  {$IFDEF DEBUG}
  if DebugHook <> 0 then
  begin
    Write('Press <Enter>');
    Readln;
  end;
  {$ENDIF}
{$ELSE}
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnit.RunRegisteredTests;
  {$ELSE}
  {$IFNDEF FMX}
  Application.Initialize;
  TGUITestRunner.RunRegisteredTests;
  {$ELSE}
  TFMXTestRunner.RunRegisteredTests;
  {$ENDIF}
  {$ENDIF}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
end;

end.
