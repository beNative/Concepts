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

unit Spring.Persistence.SQL.Generators.Register;

interface

// register all implemented generators
uses
  Spring.Persistence.SQL.Generators.SQLite3,
  Spring.Persistence.SQL.Generators.PostgreSQL,
  Spring.Persistence.SQL.Generators.Oracle,
  Spring.Persistence.SQL.Generators.MSSQL,
  Spring.Persistence.SQL.Generators.MySQL,
  Spring.Persistence.SQL.Generators.Firebird,
  //Spring.Persistence.SQL.Generators.MongoDB,
  Spring.Persistence.SQL.Generators.ASA;

implementation

end.
