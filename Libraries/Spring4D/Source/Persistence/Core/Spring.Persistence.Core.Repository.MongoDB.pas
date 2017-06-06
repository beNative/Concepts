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

unit Spring.Persistence.Core.Repository.MongoDB;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Repository.Simple,
  Spring.Persistence.Core.Session,
  Spring.Persistence.Core.Session.MongoDB;

type
  TMongoDBRepository<T: class, constructor; TID> = class(TSimpleRepository<T,TID>)
  private
    {$IFDEF WEAKREF}[Weak]{$ENDIF}
    fSession: TMongoDBSession;
  public
    procedure Insert(const entities: IEnumerable<T>); override;
    function Query(const query: string;
      const params: array of TValue): IList<T>; override;
  public
    constructor Create(const session: TSession); override;
  end;

implementation

uses
  SysUtils;


{$REGION 'TMongoDBRepository<T, TID>'}

constructor TMongoDBRepository<T, TID>.Create(const session: TSession);
begin
  inherited Create(session);
  fSession := session as TMongoDBSession;
end;

procedure TMongoDBRepository<T, TID>.Insert(const entities: IEnumerable<T>);
begin
  fSession.BulkInsert<T>(entities);
end;

function TMongoDBRepository<T, TID>.Query(const query: string;
  const params: array of TValue): IList<T>;
var
  LQuery: string;
begin
  LQuery := Format('S[%s]%s', [Namespace, query]);
  Result := inherited Query(LQuery, params);
end;

end.
