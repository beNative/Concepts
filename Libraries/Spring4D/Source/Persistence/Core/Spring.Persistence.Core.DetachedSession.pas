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

unit Spring.Persistence.Core.DetachedSession;

interface

uses
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session;

type
  /// <summary>
  ///   Represents detached session. Detached session doesn't hold any history
  ///   of loaded, saved, deleted entities, so Save method always inserts new
  ///   entity, because it doesn't know the state of an entity. Detached
  ///   session could be useful in those scenarios where you always know what
  ///   action should be done (insert, update, or delete). Also it is faster
  ///   than ordinary session (no need to save entities state).
  /// </summary>
  TDetachedSession = class(TSession)
  protected
    procedure AttachEntity(const entity: TObject); override;
    procedure DetachEntity(const entity: TObject); override;
  end;

implementation


{$REGION 'TDetachedSession'}

procedure TDetachedSession.AttachEntity(const entity: TObject);
begin
  // do nothing
end;

procedure TDetachedSession.DetachEntity(const entity: TObject);
begin
  // do nothing
end;

{$ENDREGION}


end.
