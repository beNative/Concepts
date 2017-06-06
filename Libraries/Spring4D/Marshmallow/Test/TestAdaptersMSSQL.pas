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

{$I Spring.inc}

unit TestAdaptersMSSQL;

interface

uses
  TestFramework,
  TestEntities,
  SysUtils,
  Windows,
  Spring.Persistence.Adapters.ADO,
  Spring.Persistence.Adapters.MSSQL,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session,
  Spring.Persistence.SQL.Generators.Oracle,
  Spring.TestUtils,
  TestAdaptersADO;

type
  TMSSQLConnectionAdapterTest = class(TBaseADOAdapterTest)
  strict private
    fConnectionAdapter: TMSSQLConnectionAdapter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetQueryLanguage;
    procedure TestTransaction;
    procedure TestConnectException;
    procedure TestBeginTransactionException;
  end;

implementation

uses
  ADODB,
  Spring.Persistence.SQL.Interfaces;


{$REGION 'TMSSQLConnectionAdapterTest'}

procedure TMSSQLConnectionAdapterTest.SetUp;
begin
  inherited;
  fConnectionAdapter := TMSSQLConnectionAdapter.Create(TADOConnection.Create(nil));
  fConnectionAdapter.AutoFreeConnection := True;
end;

procedure TMSSQLConnectionAdapterTest.TearDown;
begin
  fConnectionAdapter.Free;
  inherited;
end;

procedure TMSSQLConnectionAdapterTest.TestBeginTransactionException;
begin
  ExpectedException := EADOAdapterException;
  fConnectionAdapter.BeginTransaction;
end;

procedure TMSSQLConnectionAdapterTest.TestConnectException;
begin
  ExpectedException := EADOAdapterException;
  fConnectionAdapter.Connect;
end;

procedure TMSSQLConnectionAdapterTest.TestGetQueryLanguage;
begin
  CheckEquals(qlMSSQL, fConnectionAdapter.QueryLanguage);
end;

procedure TMSSQLConnectionAdapterTest.TestTransaction;
begin
  fConnectionAdapter.Connection.ConnectionObject := fMockConnectionObject;

  // Test connect exception
  CheckException(EADOAdapterException,
    procedure begin fConnectionAdapter.BeginTransaction end);

  SetupOpen;

  // Test commit - with exception
  SetupExecute(['BEGIN TRANSACTION T1', 'COMMIT TRANSACTION T1']);
  with fConnectionAdapter.BeginTransaction do
  begin
    Commit;
    // Next commit is not allowed, simulates exception in the driver
    CheckException(EADOAdapterException,
      procedure begin Commit end);
  end;

  // Test rollback - with exception
  SetupExecute(['BEGIN TRANSACTION T2', 'ROLLBACK TRANSACTION T2']);
  with fConnectionAdapter.BeginTransaction do
  begin
    Rollback;
    // Next rollback is not allowed, simulates exception in the driver
    CheckException(EADOAdapterException,
      procedure begin Rollback end);
  end;
end;

{$ENDREGION}


initialization
  RegisterTest('Spring.Persistence.Adapters',
    TMSSQLConnectionAdapterTest.Suite);

end.
