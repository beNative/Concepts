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

unit Spring.Collections.ReadOnlyLists;

{$I Spring.inc}

interface

uses
//  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  TAbstractReadOnlyList<T> = class(TEnumerableBase<T>, IReadOnlyList<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TAbstractReadOnlyList<T>;
        fIndex: Integer;
        fCurrent: T;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TAbstractReadOnlyList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
        procedure Reset; override;
      end;
  protected
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): T; virtual; abstract;
  {$ENDREGION}
  {$REGION 'Implements IInterface'}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  {$ENDREGION}
  public
    function GetEnumerator: IEnumerator<T>; override;

    function IndexOf(const item: T): Integer; virtual; abstract;

    function Reversed: IReadOnlyList<T>; reintroduce;

    property Items[index: Integer]: T read GetItem; default;
  end;

  TReadOnlyList<T> = class(TAbstractReadOnlyList<T>)
  private
    fList: IList<T>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(index: Integer): T; override;
  {$ENDREGION}
  public
    constructor Create(const list: IList<T>);

    function GetEnumerator: IEnumerator<T>; override;

    function IndexOf(const item: T): Integer; override;
  end;

  TAnonymousReadOnlyList<T> = class(TAbstractReadOnlyList<T>)
  private
    fCounter: TFunc<Integer>;
    fGetter: TFunc<Integer, T>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(index: Integer): T; override;
  {$ENDREGION}
  public
    constructor Create(const counter: TFunc<Integer>; const getter: TFunc<Integer, T>);

    function IndexOf(const item: T): Integer; override;
  end;

  TReadOnlyList = record
  public
    class function AsReadOnlyList<T>(const source: IList<T>): IReadOnlyList<T>; static;
    class function AsList<T>(const source: IReadOnlyList<T>): IList<T>; static;
  end;

implementation

uses
  TypInfo;


{$REGION 'TAbstractReadOnlyList<T>'}

function TAbstractReadOnlyList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TAbstractReadOnlyList<T>.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IID = IObjectReadOnlyList then
  begin
    if ElementType.Kind = tkClass then
      Result := inherited QueryInterface(IReadOnlyList<TObject>, Obj)
    else
      Result := E_NOINTERFACE;
  end
  else
    Result := inherited;
end;

function TAbstractReadOnlyList<T>.Reversed: IReadOnlyList<T>;
begin
  Result := TAnonymousReadOnlyList<T>.Create(
    function: Integer
    begin
      Result := Count;
    end,
    function(index: Integer): T
    begin
      Result := Items[Count - 1 - index];
    end);
end;

{$ENDREGION}


{$REGION 'TAbstractReadOnlyList<T>.TEnumerator'}

constructor TAbstractReadOnlyList<T>.TEnumerator.Create(const list: TAbstractReadOnlyList<T>);
begin
  inherited Create;
  fList := list;
  fList._AddRef;
end;

destructor TAbstractReadOnlyList<T>.TEnumerator.Destroy;
begin
  fList._Release;
  inherited Destroy;
end;

function TAbstractReadOnlyList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TAbstractReadOnlyList<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fIndex < fList.Count then
  begin
    fCurrent := fList[fIndex];
    Inc(fIndex);
    Result := True;
  end
  else
    fCurrent := Default(T);
end;

procedure TAbstractReadOnlyList<T>.TEnumerator.Reset;
begin
  fIndex := 0;
  fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TAdapterReadOnlyList<T>'}

constructor TReadOnlyList<T>.Create(const list: IList<T>);
begin
  Guard.CheckNotNull(Assigned(list), 'list');

  inherited Create;

  fList := list;
end;

function TReadOnlyList<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TReadOnlyList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fList.GetEnumerator;
end;

function TReadOnlyList<T>.GetItem(index: Integer): T;
begin
  Result := fList[index];
end;

function TReadOnlyList<T>.IndexOf(const item: T): Integer;
begin
  Result := fList.IndexOf(item);
end;

{$ENDREGION}


{$REGION 'TAnonymousReadOnlyList<T>'}

constructor TAnonymousReadOnlyList<T>.Create(const counter: TFunc<Integer>;
  const getter: TFunc<Integer, T>);
begin
  Guard.CheckNotNull(Assigned(counter), 'counter');
  Guard.CheckNotNull(Assigned(getter), 'getter');

  inherited Create;

  fCounter := counter;
  fGetter := getter;
end;

function TAnonymousReadOnlyList<T>.GetCount: Integer;
begin
  Result := fCounter();
end;

function TAnonymousReadOnlyList<T>.GetItem(index: Integer): T;
begin
  Guard.CheckRange((index >= 0) and (index < Count), 'index');

  Result := fGetter(index);
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T): Integer;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := EqualityComparer;
  for i := 0 to Count - 1 do
    if comparer.Equals(Items[i], item) then
      Exit(i);
  Result := -1;
end;

{$ENDREGION}


end.
