(*
  Copyright (c) 2011-2012, DSharp team
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Core.DependencyProperty;

interface

uses
  Classes,
  Generics.Collections,
  Rtti,
  Spring,
  SyncObjs,
  TypInfo;

type
  TDependencyProperty = class;

  {$SCOPEDENUMS ON}
  TFrameworkPropertyMetadataOption = (
    ///	<summary>
    ///	  No options are specified; the dependency property uses the default
    ///	  behavior of the property system.
    ///	</summary>
    None,

    ///	<summary>
    ///	  The values of this dependency property are inherited by child elements.
    ///	</summary>
    Inherits
  );
  {$SCOPEDENUMS OFF}
  TFrameworkPropertyMetadataOptions = set of TFrameworkPropertyMetadataOption;

  ///	<summary>
  ///	  Provides data for various property changed events. Typically these
  ///	  events report effective value changes in the value of a read-only
  ///	  dependency property. Another usage is as part of a
  ///	  PropertyChangedCallback implementation.
  ///	</summary>
  IDependencyPropertyChangedEventArgs = interface(IEventArgs)
    ['{33B767D0-6ECC-4430-BB61-6E926DB86CF4}']
    function GetOldValue: TValue;
    function GetNewValue: TValue;
    function GetProp: TDependencyProperty;
    property OldValue: TValue read GetOldValue;
    property NewValue: TValue read GetNewValue;
    property Prop: TDependencyProperty read GetProp;
  end;

  TDependencyPropertyChangedEventArgs = class(TEventArgs, IDependencyPropertyChangedEventArgs)
  private
    FNewValue: TValue;
    FOldValue: TValue;
    FProp: TDependencyProperty;

    function GetOldValue: TValue;
    function GetNewValue: TValue;
    function GetProp: TDependencyProperty;
  public
    constructor Create(Prop: TDependencyProperty; const OldValue, NewValue: TValue);
  end;

  {$M+}
  TPropertyChangedEvent = reference to procedure(Sender: TComponent;
    EventArgs: IDependencyPropertyChangedEventArgs);

  TPropertyMetadata = class
  private
    FDefaultValue: TValue;
    FFlags: TFrameworkPropertyMetadataOptions;
    FOnPropertyChanged: Event<TPropertyChangedEvent>;

    function GetInherits: Boolean;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
  public
    constructor Create(DefaultValue: TValue;
      Flags: TFrameworkPropertyMetadataOptions = [];
      PropertyChanged: TPropertyChangedEvent = nil); overload;
    constructor Create(PropertyChanged: TPropertyChangedEvent); overload;
    constructor Create(Flags: TFrameworkPropertyMetadataOptions = [];
      PropertyChanged: TPropertyChangedEvent = nil); overload;
    constructor Create(DefaultValue: TValue;
      PropertyChanged: TPropertyChangedEvent); overload;

    property DefaultValue: TValue read FDefaultValue;
    property Inherits: Boolean read GetInherits;
    property OnPropertyChanged: IEvent<TPropertyChangedEvent>
      read GetOnPropertyChanged;
  end;

  TDependencyProperty = class(TComponent)
  private
    FDefaultMetadata: TPropertyMetadata;
    FName: string;
    FOwnerType: TClass;
    FPropertyType: PTypeInfo;
    class var FLock: TCriticalSection;
    class var FRegister: TDictionary<TDependencyProperty, TDictionary<TComponent, TValue>>;

    procedure DoPropertyChanged(Sender: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create(const Name: string; PropertyType: PTypeInfo;
      OwnerType: TClass; DefaultMetadata: TPropertyMetadata); reintroduce;
    destructor Destroy; override;

    procedure ClearValue(Target: TComponent);
    function GetValue(Target: TComponent): TValue;
    procedure SetValue(Target: TComponent; const Value: TValue);

    class function FindByName(const FullPropertyName: string): TArray<TDependencyProperty>;
    class function RegisterAttached(const Name: string; PropertyType: PTypeInfo;
      OwnerType: TClass; DefaultMetadata: TPropertyMetadata = nil): TDependencyProperty; overload;

    property DefaultMetadata: TPropertyMetadata read FDefaultMetadata;
    property Name: string read FName;
    property OwnerType: TClass read FOwnerType;
    property PropertyType: PTypeInfo read FPropertyType;
  end;

  DependencyProperty<T> = record
  private
    FInstance: TDependencyProperty;
  public
    procedure ClearValue(Target: TComponent);
    function GetValue(Target: TComponent): T;
    procedure SetValue(Target: TComponent; const Value: T);

    class operator Implicit(const Value: TDependencyProperty): DependencyProperty<T>;
  end;

implementation

uses
  DSharp.Core.Framework,
  DSharp.Core.Reflection,
  StrUtils,
  SysConst,
  SysUtils;

resourcestring
  RUnknownDependencyProp = 'The specified dependency property "%s" does not exist.';

{ TDependencyPropertyChangedEventArgs }

constructor TDependencyPropertyChangedEventArgs.Create(Prop: TDependencyProperty;
  const OldValue, NewValue: TValue);
begin
  FProp := Prop;
  FOldValue := OldValue;
  FNewValue := NewValue;
end;

function TDependencyPropertyChangedEventArgs.GetNewValue: TValue;
begin
  Result := FNewValue;
end;

function TDependencyPropertyChangedEventArgs.GetOldValue: TValue;
begin
  Result := FOldValue;
end;

function TDependencyPropertyChangedEventArgs.GetProp: TDependencyProperty;
begin
  Result := FProp;
end;

{ TPropertyMetadata }

constructor TPropertyMetadata.Create(DefaultValue: TValue;
  Flags: TFrameworkPropertyMetadataOptions;
  PropertyChanged: TPropertyChangedEvent);
begin
  FDefaultValue := DefaultValue;
  FFlags := Flags;
  if Assigned(PropertyChanged) then
  begin
    FOnPropertyChanged.Add(PropertyChanged);
  end;
end;

constructor TPropertyMetadata.Create(PropertyChanged: TPropertyChangedEvent);
begin
  Create(TValue.Empty, [], PropertyChanged);
end;

constructor TPropertyMetadata.Create(Flags: TFrameworkPropertyMetadataOptions;
  PropertyChanged: TPropertyChangedEvent);
begin
  Create(TValue.Empty, Flags, PropertyChanged);
end;

constructor TPropertyMetadata.Create(DefaultValue: TValue;
  PropertyChanged: TPropertyChangedEvent);
begin
  Create(DefaultValue, [], PropertyChanged);
end;

function TPropertyMetadata.GetInherits: Boolean;
begin
  Result := TFrameworkPropertyMetadataOption.Inherits in FFlags;
end;

function TPropertyMetadata.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged;
end;

{ TDependencyProperty }

class constructor TDependencyProperty.Create;
begin
  FLock := TCriticalSection.Create;
  FRegister := TObjectDictionary<TDependencyProperty, TDictionary<TComponent, TValue>>.Create([Generics.Collections.doOwnsKeys, Generics.Collections.doOwnsValues]);
end;

class destructor TDependencyProperty.Destroy;
begin
  FRegister.Free;
  FLock.Free;
end;

constructor TDependencyProperty.Create(const Name: string; PropertyType: PTypeInfo;
  OwnerType: TClass; DefaultMetadata: TPropertyMetadata);
begin
  inherited Create(nil);

  FDefaultMetadata := DefaultMetadata;
  FName := Name;
  FOwnerType := OwnerType;
  FPropertyType := PropertyType;

  FLock.Acquire;
  try
    FRegister.Add(Self, TDictionary<TComponent, TValue>.Create);
  finally
    FLock.Release;
  end;

  TRttiDependencyProperty.Create(Self);
end;

procedure TDependencyProperty.ClearValue(Target: TComponent);
var
  LStore: TDictionary<TComponent, TValue>;
  LOldValue: TValue;
begin
  FLock.Acquire;
  try
    if FRegister.TryGetValue(Self, LStore) then
    begin
      if LStore.ContainsKey(Target) then
      begin
        LStore.TryGetValue(Target, LOldValue);
        LStore.Remove(Target);

        DoPropertyChanged(Target, TDependencyPropertyChangedEventArgs.Create(
          Self, LOldValue, TValue.Empty));
      end;
    end;
  finally
    FLock.Release;
  end;
end;

destructor TDependencyProperty.Destroy;
begin
  FDefaultMetadata.Free;
  inherited;
end;

procedure TDependencyProperty.DoPropertyChanged(Sender: TComponent;
  EventArgs: IDependencyPropertyChangedEventArgs);
var
  LElement: TObject;
begin
  TFramework.NotifyPropertyChanged(Sender, EventArgs.Prop.Name);

  //FDefaultMetadata.OnPropertyChanged.Invoke(Sender, EventArgs);
  //FDefaultMetadata.OnPropertyChanged(Sender, EventArgs);

  if FDefaultMetadata.Inherits then
  begin
    for LElement in TFramework.GetChildren(Sender) do
    begin
      DoPropertyChanged(TComponent(LElement), EventArgs);
    end;
  end;
end;

class function TDependencyProperty.FindByName(
  const FullPropertyName: string): TArray<TDependencyProperty>;
var
  LScope: string;
  LName: string;
  LProp: TDependencyProperty;
begin
  Result := nil;
  LScope := Copy(FullPropertyName, 1, LastDelimiter('.', FullPropertyName) - 1);
  LName := Copy(FullPropertyName, LastDelimiter('.', FullPropertyName) + 1);
  for LProp in FRegister.Keys do
  begin
    if SameText(LProp.Name, LName)
      and EndsText(LScope, LProp.OwnerType.QualifiedClassName) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := LProp;
    end;
  end;
end;

function TDependencyProperty.GetValue(Target: TComponent): TValue;
var
  LStore: TDictionary<TComponent, TValue>;
  LElement: TComponent;
  LValue: TValue;
begin
  FLock.Acquire;
  try
    Result := FDefaultMetadata.DefaultValue;

    if not FRegister.TryGetValue(Self, LStore) then
    begin
      raise EInvalidOperation.CreateFmt(RUnknownDependencyProp, [Name]);
    end;

    if FDefaultMetadata.Inherits then
    begin
      LElement := Target;

      while Assigned(LElement) do
      begin
        if LStore.TryGetValue(TComponent(LElement), LValue) then
        begin
          Result := LValue;
          Break;
        end;
        LElement := TFramework.GetParent(LElement);
      end;
    end
    else
    begin
      if LStore.TryGetValue(Target, LValue) then
      begin
        Result := LValue;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TDependencyProperty.Notification(AComponent: TComponent; Operation: TOperation);
var
  LStore: TDictionary<TComponent, TValue>;
begin
  inherited;
  case Operation of
    opRemove:
    begin
      if FRegister.TryGetValue(Self, LStore) then
      begin
        FLock.Acquire;
        try
          if LStore.Count > 0 then
          begin
            LStore.Remove(AComponent);
          end;
        finally
          FLock.Release;
        end;
      end;
    end;
  end;
end;

class function TDependencyProperty.RegisterAttached(const Name: string;
  PropertyType: PTypeInfo; OwnerType: TClass;
  DefaultMetadata: TPropertyMetadata = nil): TDependencyProperty;
begin
  if not Assigned(DefaultMetadata) then
  begin
    DefaultMetadata := TPropertyMetadata.Create;
  end;
  Result := TDependencyProperty.Create(Name, PropertyType, OwnerType, DefaultMetadata);
end;

procedure TDependencyProperty.SetValue(Target: TComponent; const Value: TValue);
var
  LStore: TDictionary<TComponent, TValue>;
  LOldValue: TValue;
begin
  FLock.Acquire;
  try
    if FRegister.TryGetValue(Self, LStore) then
    begin
      if Value.IsObject then
      begin
        if not Value.IsEmpty and not Value.IsInstanceOf(GetTypeData(PropertyType).ClassType) then
        begin
          raise EInvalidCast.CreateRes(@SInvalidCast);
        end;
      end else
      if (PropertyType <> TypeInfo(TValue)) and not Value.IsType(PropertyType) then
      begin
        raise EInvalidCast.CreateRes(@SInvalidCast);
      end;

      Target.FreeNotification(Self);
      LStore.TryGetValue(Target, LOldValue);

      if not SameValue(LOldValue, Value) then
      begin
        LStore.AddOrSetValue(Target, Value);

        DoPropertyChanged(Target, TDependencyPropertyChangedEventArgs.Create(Self, LOldValue, Value));
      end;
    end
    else
    begin
      raise EInvalidOperation.CreateResFmt(@RUnknownDependencyProp, [Name]);
    end;
  finally
    FLock.Release;
  end;
end;

{ DependencyProperty<T> }

procedure DependencyProperty<T>.ClearValue(Target: TComponent);
begin
  FInstance.ClearValue(Target);
end;

function DependencyProperty<T>.GetValue(Target: TComponent): T;
begin
  Result := FInstance.GetValue(Target).AsType<T>;
end;

procedure DependencyProperty<T>.SetValue(Target: TComponent; const Value: T);
begin
  FInstance.SetValue(Target, TValue.From<T>(Value));
end;

class operator DependencyProperty<T>.Implicit(
  const Value: TDependencyProperty): DependencyProperty<T>;
begin
  Result.FInstance := Value;
end;

end.

