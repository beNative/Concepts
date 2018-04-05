{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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

unit Spring.Persistence.Core.ListSession;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session;

type
  TListSession<T: class, constructor> = class(TInterfacedObject, IListSession<T>)
  private
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fSession: TSession;
    fList: IList<T>;
    fPrimaryKeys: ISet<TValue>;
  protected
    procedure DoOnListChanged(Sender: TObject; const item: T; action: TCollectionChangedAction);

    procedure CommitListSession; virtual;
    procedure RollbackListSession; virtual;

    procedure DeleteEntities;
  public
    constructor Create(const session: TSession; const list: IList<T>); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  Generics.Collections,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.SQL.Commands.Delete;


{$REGION 'TListSession'}

constructor TListSession<T>.Create(const session: TSession; const list: IList<T>);
begin
  inherited Create;
  fPrimaryKeys := TCollections.CreateSet<TValue>;
  fSession := session;
  fList := list;
  fList.OnChanged.Add(DoOnListChanged);
end;

destructor TListSession<T>.Destroy;
begin
  fList.OnChanged.Remove(DoOnListChanged);
  inherited Destroy;
end;

procedure TListSession<T>.CommitListSession;
begin
  //delete first
  DeleteEntities;
  fSession.SaveList<T>(fList);
end;

procedure TListSession<T>.DeleteEntities;
var
  deleter: IDeleteCommand;
  primaryKey: TValue;
begin
  deleter := TDeleteExecutor.Create(fSession.Connection);
  deleter.Build(T);
  for primaryKey in fPrimaryKeys do
    deleter.ExecuteById(primaryKey);
end;

procedure TListSession<T>.DoOnListChanged(Sender: TObject; const Item: T; Action: TCollectionChangedAction);
var
  value: TValue;
  entityDetails: TEntityData;
begin
  case Action of
    caAdded: ;
    caRemoved:
    begin
      if not fSession.IsNew(Item) then
      begin
        entityDetails := TEntityCache.Get(T);
        value := entityDetails.PrimaryKeyColumn.GetValue(Item);
        if fPrimaryKeys.Add(value) then
          fSession.OldStateEntities.Remove(Item);
      end;
    end;
    caReplaced: ;
    caMoved: ;
    caReseted: ;
  end;
end;

procedure TListSession<T>.RollbackListSession;
begin
  fPrimaryKeys.Clear;
end;

{$ENDREGION}


end.
