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

unit Spring.Collections.Stacks;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Events;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  /// <summary>
  ///   Represents a last-in, first-out (LIFO) collection of items.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the stack.
  /// </typeparam>
  TAbstractStack<T> = class(TEnumerableBase<T>)
  private
  {$REGION 'Nested Types'}
    type
      PEnumerator = ^TEnumerator;
      TEnumerator = record
        Vtable: Pointer;
        RefCount: Integer;
        TypeInfo: PTypeInfo;
        fSource: TAbstractStack<T>;
        fIndex, fVersion: Integer;
        function GetCurrent: T;
        function MoveNext: Boolean;
        class var Enumerator_Vtable: TEnumeratorVtable;
      end;
  {$ENDREGION}
  private
    fOnChanged: TCollectionChangedEventImpl<T>;
    fItems: TArray<T>;
    fCapacity: Integer;
    fCount: Integer;
    fVersion: Integer;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetNonEnumeratedCount: Integer;
    function GetOnChanged: ICollectionChangedEvent<T>;
    function GetOwnsObjects: Boolean; inline;
    procedure SetCapacity(value: Integer);
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
    procedure PopInternal(var item: T; action: TCollectionChangedAction); inline;
    procedure PushInternal(const item: T); inline;
    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create(capacity: Integer = 0; ownsObjects: Boolean = False);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
  {$ENDREGION}

  {$REGION 'Implements IStack<T>'}
    function Pop: T;
    function Extract: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryPop(var item: T): Boolean;
    function TryExtract(var item: T): Boolean;
    function TryPeek(var item: T): Boolean;

    procedure Clear;
    procedure TrimExcess;
  {$ENDREGION}
  end;

  TStack<T> = class(TAbstractStack<T>, IInterface, IEnumerable<T>, IStack<T>)
  private
    procedure Grow;
  public
    constructor Create(const values: array of T); overload;
    constructor Create(const values: IEnumerable<T>); overload;
    procedure Clear;
    function Push(const item: T): Boolean;
  end;

  TBoundedStack<T> = class(TAbstractStack<T>, IInterface, IEnumerable<T>, IStack<T>)
  public
    function Push(const item: T): Boolean;
  end;

  TFoldedStack<T> = class(TStack<T>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const elementType: PTypeInfo;
      const comparer: IComparer<T>; ownsObjects: Boolean = False);
  end;

implementation

uses
{$IFDEF DELPHIXE4}
  Rtti, // suppress hint about inlining
{$ENDIF}
  TypInfo,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TAbstractStack<T>'}

constructor TAbstractStack<T>.Create(capacity: Integer; ownsObjects: Boolean);
begin
  inherited Create;
  SetCapacity(capacity);
  SetOwnsObjects(ownsObjects);
end;

procedure TAbstractStack<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

procedure TAbstractStack<T>.BeforeDestruction;
begin
  IStack<T>(this).Clear;
  fOnChanged.Free;
end;

function TAbstractStack<T>.GetCapacity: Integer;
begin
  Result := fCapacity;
end;

function TAbstractStack<T>.GetCount: Integer;
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) <> tkClass then
    Result := fCount
  else
  {$ENDIF}
  Result := fCount and CountMask;
end;

function TAbstractStack<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fVersion;
    fIndex := -1;
  end;
end;

function TAbstractStack<T>.GetNonEnumeratedCount: Integer;
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) <> tkClass then
    Result := fCount
  else
  {$ENDIF}
  Result := fCount and CountMask;
end;

function TAbstractStack<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

function TAbstractStack<T>.GetOwnsObjects: Boolean;
begin
  Result := {$IFDEF DELPHIXE7_UP}(GetTypeKind(T) = tkClass) and {$ENDIF}(fCount < 0);
end;

procedure TAbstractStack<T>.Clear;
var
  stackCount, i: Integer;
begin
  stackCount := Count;
  if stackCount > 0 then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    Dec(fCount, stackCount);

    if fOnChanged.CanInvoke then
      if OwnsObjects then
        for i := stackCount - 1 downto 0 do
        begin
          fOnChanged.Invoke(Self, fItems[i], caRemoved);
          TArray<TObject>(fItems)[i].Free;
        end
      else
        for i := stackCount - 1 downto 0 do
          fOnChanged.Invoke(Self, fItems[i], caRemoved)
    else
      if OwnsObjects then
        for i := stackCount - 1 downto 0 do
          TArray<TObject>(fItems)[i].Free;

    if TType.IsManaged<T> then
      System.Finalize(fItems[0], stackCount)
    else
      System.FillChar(fItems[0], SizeOf(T) * stackCount, 0);
  end;
end;

procedure TAbstractStack<T>.PopInternal(var item: T; action: TCollectionChangedAction);
var
  stackItem: ^T;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);

  stackItem := @fItems[Count];
  item := stackItem^;
  stackItem^ := Default(T);

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
  if OwnsObjects then
    if action = caRemoved then
    begin
      PObject(@item).Free;
      item := Default(T);
    end;
end;

procedure TAbstractStack<T>.PushInternal(const item: T);
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[Count] := item;
  Inc(fCount);

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, caAdded);
end;

function TAbstractStack<T>.Pop: T; //FI:W521
begin
  if Count > 0 then
    PopInternal(Result, caRemoved)
  else
    RaiseHelper.NoElements;
end;

function TAbstractStack<T>.Extract: T; //FI:W521
begin
  if Count > 0 then
    PopInternal(Result, caExtracted)
  else
    RaiseHelper.NoElements;
end;

function TAbstractStack<T>.Peek: T; //FI:W521
begin
  if Count > 0 then
    Exit(fItems[Count - 1]);
  RaiseHelper.NoElements;
  __SuppressWarning(Result);
end;

function TAbstractStack<T>.PeekOrDefault: T;
begin
  if Count > 0 then
    Result := fItems[Count - 1]
  else
    Result := Default(T);
end;

procedure TAbstractStack<T>.SetCapacity(value: Integer);
begin
  if value < Count then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.value, ExceptionResource.ArgumentOutOfRange_Capacity);

  fCapacity := value;
  SetLength(fItems, value);
end;

procedure TAbstractStack<T>.SetOwnsObjects(const value: Boolean);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) = tkClass then
  {$ELSE}
  if TType.Kind<T> = tkClass then
  {$ENDIF}
    fCount := (fCount and CountMask) or (Ord(value) shl OwnsObjectsBitIndex);
end;

procedure TAbstractStack<T>.TrimExcess;
begin
  fCapacity := Count;
  SetLength(fItems, fCapacity);
end;

function TAbstractStack<T>.TryPop(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    PopInternal(item, caRemoved);
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

function TAbstractStack<T>.TryExtract(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    PopInternal(item, caExtracted);
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

function TAbstractStack<T>.TryPeek(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    item := fItems[Count - 1];
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TAbstractStack<T>.TEnumerator'}

function TAbstractStack<T>.TEnumerator.GetCurrent: T;
begin
  Result := fSource.fItems[fIndex];
end;

function TAbstractStack<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion = fSource.fVersion then
  begin
    if fIndex < 0 then
      fIndex := fSource.Count;
    Result := fIndex > 0;
    Dec(fIndex, Ord(Result));
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TStack<T>'}

constructor TStack<T>.Create(const values: array of T);
var
  i: Integer;
begin
  inherited Create(Length(values));
  for i := 0 to High(values) do
    Push(values[i]);
end;

constructor TStack<T>.Create(const values: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  inherited Create;
  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    Push(item);
  end;
end;

procedure TStack<T>.Clear;
begin
  inherited Clear;
  fCapacity := 0;
  SetLength(fItems, 0);
end;

function TStack<T>.Push(const item: T): Boolean;
begin
  if Count = Capacity then
    Grow;
  PushInternal(item);
  Result := True;
end;

procedure TStack<T>.Grow;
begin
  fCapacity := GrowCapacity(fCapacity);
  SetLength(fItems, fCapacity);
end;

{$ENDREGION}


{$REGION 'TBoundedStack<T>'}

function TBoundedStack<T>.Push(const item: T): Boolean;
begin
  if Count = Capacity then
    Exit(False);
  PushInternal(item);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TFoldedStack<T>'}

constructor TFoldedStack<T>.Create(const elementType: PTypeInfo;
  const comparer: IComparer<T>; ownsObjects: Boolean);
begin
  fElementType := elementType;
  fComparer := comparer;
  inherited Create;
  SetOwnsObjects(ownsObjects);
end;

function TFoldedStack<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


end.
