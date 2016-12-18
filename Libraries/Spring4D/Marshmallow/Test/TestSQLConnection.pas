{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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

unit TestSQLConnection;

interface

uses
  Classes,
  DB,
  DBXCommon,
  SqlExpr,
  Spring,
  Spring.Reflection;

type
  // Since default RTTI generation is set not to generate info for protected
  // methods, we need to enabled it manually and override some methods so
  // info is re-generated for them. Also make them public to allow the test
  // to access them. We also need to skip the destruction code that does not
  // expect empty instance.
  {$RTTI EXPLICIT
    METHODS([vcPrivate..vcPublished])
    PROPERTIES(DefaultPropertyRttiVisibility)
    FIELDS(DefaultFieldRttiVisibility)}
  TTestSQLConnection = class(TSQLConnection)
  public
    destructor Destroy; override;

    function GetConnected: Boolean; override; abstract;
    function GetDataSetCount: Integer; override; abstract;
    procedure RegisterClient(Client: TObject;
      Event: TConnectChangeEvent); override; abstract;
    procedure SetConnected(Value: Boolean); override; abstract;
    procedure UnRegisterClient(Client: TObject); override; abstract;
  end;

  TTestDBXConnection = class(TDBXConnection)
  public
    destructor Destroy; override;

    function CreateAndBeginTransaction(const Isolation: Integer): TDBXTransaction;
      override; abstract;
  end;

  TTestDBXTransaction = class(TDBXTransaction)
  public
    destructor Destroy; override;

    constructor Create(Connection: TDBXConnection);
  end;

  TTestSQLQuery = class(TSQLQuery)
  public
    constructor Create(const connection: TSQLConnection); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Rtti;

{ TTestSQLConnection }

destructor TTestSQLConnection.Destroy;
begin
  // no-inherited
end;

{ TTestDBXTransaction }

constructor TTestDBXTransaction.Create(Connection: TDBXConnection);
begin
  inherited;
  TType.SetFieldValue(Self, 'FConnection', Connection);
end;

destructor TTestDBXTransaction.Destroy;
begin
  // no-inherited
end;

{ TTestDBXConnection }

destructor TTestDBXConnection.Destroy;
begin
  // no-inherited
end;

{ TTestSQLQuery }

constructor TTestSQLQuery.Create(const connection: TSQLConnection);
begin
  inherited Create(nil);
  SQLConnection := connection;
end;

destructor TTestSQLQuery.Destroy;
begin
  inherited;
end;

end.
