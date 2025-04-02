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

unit Spring.TestRunner;

{$I Spring.Tests.inc}

interface

procedure RunRegisteredTests;

implementation

uses
{$IFDEF LEAKCHECK}
  LeakCheck,
  SysUtils,
  StrUtils,
  Classes,
  DateUtils,
  Rtti,
  TestFramework,
  LeakCheck.Utils,
  LeakCheck.Cycle,
  LeakCheck.DUnit,
  LeakCheck.DUnitCycle,
  Spring,
  Spring.Reflection,
  Spring.ValueConverters,
  Spring.VirtualClass,
    {$IFNDEF ORM_TESTS}
    Spring.Tests.Base,
    {$ELSE}
    LeakCheck.Cycle.Utils,
    Spring.Persistence.Core.EntityCache,
    Spring.Persistence.Mapping.Attributes,
      {$IFDEF DELPHIXE5_UP}
      FireDAC.Comp.Client,
      {$ENDIF}
      {$IFNDEF FMX} // non mobile
      DBXCommon,
      {$ENDIF}
    {$ENDIF}
{$ENDIF}

{$IFDEF TESTINSIGHT}
  TestInsight.DUnit;
{$ELSE}
  {$IFDEF MSWINDOWS}
  VSoft.DUnit.XMLTestRunner;
  {$ELSE}
  TextTestRunner;
  {$ENDIF}

{$APPTYPE CONSOLE}

var
  OutputFile: string = 'Spring.Tests.Reports.xml';
{$ENDIF}

{$IFDEF LEAKCHECK}
function IgnoreValueConverters(const Instance: TObject; ClassType: TClass): Boolean;
begin
  Result := ClassType.InheritsFrom(TValueConverter);
  if Result then
    IgnoreManagedFields(Instance, ClassType);
end;

function IgnoreTimeZoneCache(const Instance: TObject; ClassType: TClass): Boolean;
begin
  Result := ClassType.ClassName = 'TLocalTimeZone.TYearlyChanges';
  if Result then
    IgnoreManagedFields(Instance, ClassType);
end;

function IgnoreEmptyEnumerable(const Instance: TObject; ClassType: TClass): Boolean;
begin
  Result := StartsStr('TEmptyEnumerable<', ClassType.ClassName);
end;

type
  TVirtualClasses = class(Spring.VirtualClass.TVirtualClasses);

procedure InitializeLeakCheck;
var
  intfType: TRttiInterfaceType;
{$IFDEF ORM_TESTS}
  lType: TRttiType;
  {$IFDEF DELPHIXE5_UP}
  fdConnection: TFDConnection;
  fdQuery: TFDQuery;
  {$ENDIF}
{$ENDIF}
begin
  MemLeakMonitorClass := TLeakCheckGraphMonitor;//TLeakCheckCycleGraphMonitor;
  // For ORM ignore strings as well since it assignes them to global attributes
  // which creates unignorable leaks.
  TLeakCheck.IgnoredLeakTypes := [tkUnknown, tkUString];
  TLeakCheck.InstanceIgnoredProc := IgnoreMultipleObjects;
  AddIgnoreObjectProc([
{$IFDEF ANDROID}
    IgnoreJNIBridgeClasses,
{$ENDIF}
    IgnoreRttiObjects,
    IgnoreValueConverters,
    IgnoreEmptyEnumerable,
    IgnoreAnonymousMethodPointers,
    IgnoreCustomAttributes,
    IgnoreTimeZoneCache,
    TIgnore<TInitTable>.Any,
    TIgnore<TInitTable>.AnyNestedClasses
  ]);
  // Initialize few things so they dont't leak in the tests
  TThread.CurrentThread;
  TType.FindType('System.TObject'); // Initialize RTTI package maps
  TType.TryGetInterfaceType(IUnknown, intfType); // Initialize Spring.TType interface map
  intfType := nil;
  TVirtualClasses.Default.GetVirtualClass(TInterfacedObject);
  TTimeZone.Local.ID;
  StrToBool('True'); // Initialize StrToBool array cache
{$IFDEF DELPHIXE5_UP}
  TEncoding.ANSI;
{$ENDIF}
  TEncoding.ASCII;
  TEncoding.Default;
  TEncoding.UTF7;
  TEncoding.UTF8;
  TEncoding.Unicode;
{$IFDEF ORM_TESTS}
  // Initialize global Entity cache
  for lType in TType.Context.GetTypes do
    if lType.IsClass and lType.HasCustomAttribute(TableAttribute, True) then
  begin
    TEntityCache.Get(lType.AsInstance.MetaclassType);
  end;
  {$IFDEF DELPHIXE5_UP}
  // Initialize FireDAC singletons
  fdConnection := TFDConnection.Create(nil);
  try
    fdConnection.DriverName := 'SQLite';
    fdConnection.Connected := True;
    fdQuery := TFDQuery.Create(nil);
    try
      fdQuery.Connection := fdConnection;
      fdQuery.SQL.Text := 'SELECT COUNT(*) FROM sqlite_master';
      fdQuery.OpenOrExecute;
    finally
      fdQuery.Free;
    end;
  finally
    fdConnection.Free;
  end;
  {$ENDIF}
  {$IFNDEF FMX} // non mobile
  // Initialize DBX singletons
  TDBXConnectionFactory.GetConnectionFactory;
  {$ENDIF}
{$ENDIF}
end;
{$ENDIF}

procedure RunRegisteredTests;
begin
{$IFDEF LEAKCHECK}
  InitializeLeakCheck;
{$ENDIF}

{$IFDEF TESTINSIGHT}
  TestInsight.DUnit.RunRegisteredTests;
  ReportMemoryLeaksOnShutdown := True;
{$ELSE}
  {$IFDEF MSWINDOWS}
  if ParamCount > 0 then
    OutputFile := ParamStr(1);
  VSoft.DUnit.XMLTestRunner.RunRegisteredTests(OutputFile).Free;
  {$ELSE}
  TextTestRunner.RunRegisteredTests.Free;
  {$ENDIF}
{$ENDIF}
end;

end.
