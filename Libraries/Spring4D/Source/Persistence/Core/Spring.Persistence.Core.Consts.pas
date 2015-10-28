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

{$I Spring.inc}

unit Spring.Persistence.Core.Consts;

interface

const
  METHODNAME_CONTAINER_ADD = 'Add';
  METHODNAME_CONTAINER_OWNSOBJECTS = 'OwnsObjects';

  SQL_BEGIN_TRAN = 'BEGIN TRANSACTION ';
  SQL_BEGIN_SAVEPOINT = 'SAVEPOINT ';
  SQL_COMMIT_TRAN = 'COMMIT TRANSACTION ';
  SQL_ROLLBACK_TRAN = 'ROLLBACK TRANSACTION ';
  SQL_ROLLBACK_SAVEPOINT = 'ROLLBACK TO SAVEPOINT ';

resourcestring
  EXCEPTION_CANNOT_COMMIT = 'Cannot commit unstarted transaction';
  EXCEPTION_CANNOT_ROLLBACK = 'Cannot rollback unstarted transaction';
  EXCEPTION_PRIMARYKEY_NOTFOUND = 'Primary key column "%S" not found.';
  EXCEPTION_COLUMN_NOTFOUND = 'Column "%S" not found.';
  EXCEPTION_CONTAINER_DOESNOTHAVE_ADD = 'Container does not have Add method';
  EXCEPTION_CONTAINER_ADD_ONE_PARAM = 'Container''s Add method must have only one parameter.';
  EXCEPTION_CONTAINER_ITEM_TYPE_NOTSUPPORTED = 'Container''s items type not supported';
  EXCEPTION_UNSUPPORTED_LAZY_TYPE = 'Unsupported type for lazy value: %S.';
  EXCEPTION_UNSUPPORTED_CONTAINER_TYPE = 'List must be Spring interface IList<T>.';
  EXCEPTION_QUERY_NO_RECORDS = 'Query returned 0 records.';
  EXCEPTION_CANNOT_OPEN_QUERY = 'Cannot open query. Error Message: ' + sLineBreak + '%S';
  EXCEPTION_CANNOT_CONVERT_TYPE = 'Cannot convert from type "%0:S" into type "%1:S"';

implementation

end.
