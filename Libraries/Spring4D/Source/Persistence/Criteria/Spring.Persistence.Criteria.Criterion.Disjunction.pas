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

unit Spring.Persistence.Criteria.Criterion.Disjunction;

interface

uses
  Spring.Persistence.Criteria.Criterion.Junction,
  Spring.Persistence.SQL.Types;

type
  TDisjunction = class(TJunction)
  protected
    function GetWhereOperator: TWhereOperator; override;
  end;

implementation


{$REGION 'TDisjunction'}

function TDisjunction.GetWhereOperator: TWhereOperator;
begin
  Result := woOr;
end;

{$ENDREGION}


end.
