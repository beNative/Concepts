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

program SpringORMTestsMobile;

{$I Spring.Tests.inc}

// Spring.TestBootstrap be the first unit in the uses clause since it specifies
// some units that need to be set in given order for FMX
uses
  Spring.TestBootstrap in '..\..\Source\Spring.TestBootstrap.pas',
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  FMX.Forms,
  FMX.Types,
  TestFramework,
  FMXTestRunner in '..\..\Source\dUnit\FMXTestRunner.pas' {FMXTestRunner},
  TestInsight.DUnitFMXRunner in '..\..\Source\TestInsight.DUnitFMXRunner.pas' {frmTestInsightDUnitFMXRunner},
  Spring.TestRunner in '..\..\Source\Spring.TestRunner.pas',
  Spring.Tests.Utils in '..\..\Source\Extensions\Spring.Tests.Utils.pas',
  Spring.Persistence.Core.Graphics in '..\..\..\Source\Persistence\Core\Spring.Persistence.Core.Graphics.pas',
  TestCodeGenerator in '..\..\..\Marshmallow\Test\TestCodeGenerator.pas',
  TestEntities in '..\..\..\Marshmallow\Test\TestEntities.pas',
  TestConsts in '..\..\..\Marshmallow\Test\TestConsts.pas',
  //-TestAdapterMongoDB in 'TestAdapterMongoDB.pas',
  //-TestAdaptersASA in '..\..\..\Marshmallow\Test\TestAdaptersASA.pas',
  //-TestAdaptersOracle in '..\..\..\Marshmallow\Test\TestAdaptersOracle.pas',
  //-TestSQLServerSQLGenerator in '..\..\..\Marshmallow\Test\TestSQLServerSQLGenerator.pas',
  TestAnsiSQLGenerator in '..\..\..\Marshmallow\Test\TestAnsiSQLGenerator.pas',
  Spring.Persistence.Core.EntityWrapper in '..\..\..\Source\Persistence\Core\Spring.Persistence.Core.EntityWrapper.pas',
  TestEntityWrapper in '..\..\..\Marshmallow\Test\TestEntityWrapper.pas',
  TestCoreEntityMap in '..\..\..\Marshmallow\Test\TestCoreEntityMap.pas',
  TestCoreUtils in '..\..\..\Marshmallow\Test\TestCoreUtils.pas',
  TestPersistence in '..\..\..\Marshmallow\Test\TestPersistence.pas',
  TestSession in '..\..\..\Marshmallow\Test\TestSession.pas',
  TestCommands in '..\..\..\Marshmallow\Test\TestCommands.pas',
  TestConnectionFactory in '..\..\..\Marshmallow\Test\TestConnectionFactory.pas',
  TestCoreCriteria in '..\..\..\Marshmallow\Test\TestCoreCriteria.pas',
  TestDatabaseManager in '..\..\..\Marshmallow\Test\TestDatabaseManager.pas',
  {$IFDEF DELPHIXE5_UP}
  TestFireDACConnection in '..\..\..\Marshmallow\Test\TestFireDACConnection.pas',
  TestAdaptersFireDAC in '..\..\..\Marshmallow\Test\TestAdaptersFireDAC.pas',
  {$ENDIF }
  TestObjectDataSet in '..\..\..\Marshmallow\Test\TestObjectDataSet.pas',
  //+ViewTestObjectDataSet in '..\..\Source\Persistence\ViewTestObjectDataSet.pas' {frmObjectDataSetTest},
  TestSimpleRepository in '..\..\..\Marshmallow\Test\TestSimpleRepository.pas',
  TestAdaptersSQLite in '..\..\..\Marshmallow\Test\TestAdaptersSQLite.pas',
  Spring.Tests.Persistence.FmxRefForm in '..\..\Source\Persistence\Spring.Tests.Persistence.FmxRefForm.pas' {PersistenceFmxRefForm};

{$R *.res}

begin
  //GlobalUseGPUCanvas := True;
  TThread.NameThreadForDebugging('Main');
{$IF Defined(MSWINDOWS)}
  OutputDir := IncludeTrailingPathDelimiter(ExpandFileName(ExtractFileDir(ParamStr(0))));
  PictureFilename := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir + '..\..\..\..\Marshmallow\Test')) + 'DelphiOOP.png';
  ScannerFilename := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir + '..\..\..\..\Marshmallow\Test')) + 'DelphiOOP.pdf';
{$ELSEIF Defined(ANDROID)}
  // Must be deployed to assets\internal
  OutputDir := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetDocumentsPath);
  PictureFilename := OutputDir + 'DelphiOOP.png';
  ScannerFilename := OutputDir + 'DelphiOOP.pdf';
  OutputDir := '/storage/emulated/0/';
{$ELSE}
  {$MESSAGE ERROR 'Unsupported platform'}
{$IFEND}
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmTestInsightDUnitFMXRunner, frmTestInsightDUnitFMXRunner);
  Application.CreateForm(TPersistenceFmxRefForm, PersistenceFmxRefForm);
  //Application.CreateForm(TfrmObjectDataSetTest, frmObjectDataSetTest);
  Application.Run;
  //RunRegisteredTests();
  TestFramework.ClearRegistry;
end.
