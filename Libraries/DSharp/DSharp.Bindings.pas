(*
  Copyright (c) 2011-2012, Stefan Glienke
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

unit DSharp.Bindings;

interface

uses
  Classes,
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  DSharp.Core.Collections,
  DSharp.Core.DataConversion,
  DSharp.Core.Editable,
  DSharp.Core.Expressions,
  DSharp.Core.Validations,
  Rtti,
  Spring,
  Spring.Collections,
  SysUtils;

type
  TBindingMode = (bmOneWay, bmTwoWay, bmOneWayToSource, bmOneTime);
  TTriggerMode = (tmOneWay, tmTwoWay);
  TUpdateTrigger = DSharp.Bindings.Notifications.TUpdateTrigger;

const
  BindingModeDefault = bmTwoWay;
  TriggerModeDefault = tmOneWay;
  utPropertyChanged = DSharp.Bindings.Notifications.utPropertyChanged;
  utLostFocus = DSharp.Bindings.Notifications.utLostFocus;
  utExplicit = DSharp.Bindings.Notifications.utExplicit;

type
  BindingAttribute = class(TCustomAttribute)
  private
    FBindingMode: TBindingMode;
    FSourcePropertyName: string;
    FTargetPropertyName: string;
  public
    constructor Create(const ASourcePropertyName, ATargetPropertyName: string;
      ABindingMode: TBindingMode = BindingModeDefault);
    property BindingMode: TBindingMode read FBindingMode;
    property SourcePropertyName: string read FSourcePropertyName;
    property TargetPropertyName: string read FTargetPropertyName;
  end;

  TBindingGroup = class;

  TBinding = class(TCollectionItem, INotifyPropertyChanged, IValidatable)
  private
    FActive: Boolean;
    FBindingGroup: TBindingGroup;
    FBindingMode: TBindingMode;
    FConverter: IValueConverter;
    FEnabled: Boolean;
    FManaged: Boolean;
    FNotificationHandler: TNotificationHandler;
    FNotifyOnSourceUpdated: Boolean;
    FNotifyOnTargetUpdated: Boolean;
    FOnPropertyChanged: Event<TPropertyChangedEvent>;
    FOnSourceUpdated: TPropertyChangedEvent;
    FOnTargetUpdated: TPropertyChangedEvent;
    FOnValidation: Event<TValidationEvent>;
    FPreventFocusChange: Boolean;
    FSource: TObject;
    FSourceCollectionChanged: INotifyCollectionChanged;
    FSourceProperty: IMemberExpression;
    FSourcePropertyName: string;
    FSourceUpdateTrigger: TUpdateTrigger;
    FTarget: TObject;
    FTargetProperty: IMemberExpression;
    FTargetPropertyName: string;
    FTargetUpdateTrigger: TUpdateTrigger;
    FTriggerMode: TTriggerMode;
    FUpdateSourceCount: Integer;
    FUpdateSourceExpression: TCompiledExpression;
    FUpdateTargetCount: Integer;
    FUpdateTargetExpression: TCompiledExpression;
    FValidateCount: Integer;
    FValidatesOnDataErrors: Boolean;
    FValidationErrors: IList<IValidationResult>;
    FValidationRules: IList<IValidationRule>;

    class var DataErrorValidationRule: IValidationRule;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure BeginUpdateSource;
    procedure BeginUpdateTarget;
    procedure BeginValidate;

    function CanUpdateSource(const AEventArgs: IPropertyChangedEventArgs): Boolean;
    function CanUpdateTarget(const AEventArgs: IPropertyChangedEventArgs): Boolean;

    procedure CompileExpressions;

    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure DoSourceCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoSourcePropertyChanged(ASender: TObject;
      const AEventArgs: IPropertyChangedEventArgs);
    procedure DoSourceUpdated(ASender: TObject;
      const AEventArgs: IPropertyChangedEventArgs);
    procedure DoTargetPropertyChanged(ASender: TObject;
      const AEventArgs: IPropertyChangedEventArgs);
    procedure DoTargetUpdated(ASender: TObject;
      const AEventArgs: IPropertyChangedEventArgs);
    procedure DoValidationErrorsChanged(Sender: TObject;
      const Item: IValidationResult; Action: TCollectionChangedAction);
    procedure DoValidationRulesChanged(Sender: TObject;
      const Item: IValidationRule; Action: TCollectionChangedAction);

    procedure EndUpdateSource;
    procedure EndUpdateTarget;
    procedure EndValidate;

    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    function GetOnValidation: IEvent<TValidationEvent>;
    function GetValidationErrors: IList<IValidationResult>;
    function GetValidationRules: IList<IValidationRule>;

    function IsUpdatingSource: Boolean;
    function IsUpdatingTarget: Boolean;
    function IsValidating: Boolean;

    procedure Notification(AComponent: TComponent; AOperation: TOperation);
    procedure RaiseValidationError;

    procedure SetActive(const Value: Boolean);
    procedure SetBindingGroup(const Value: TBindingGroup);
    procedure SetBindingMode(const Value: TBindingMode);
    procedure SetConverter(const Value: IValueConverter);
    procedure SetEnabled(const Value: Boolean);
    procedure SetSource(const Value: TObject);
    procedure SetSourceProperty;
    procedure SetSourcePropertyName(const Value: string);
    procedure SetTarget(const Value: TObject);
    procedure SetTargetProperty;
    procedure SetTargetPropertyName(const Value: string);

    function ValidateCommitted: Boolean;
    function ValidateOnTargetUpdated: Boolean;
    procedure SetValidatesOnDataErrors(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    class constructor Create;
    constructor Create(Collection: TCollection); overload; override;
    constructor Create(ASource: TObject = nil; ASourcePropertyName: string = '';
      ATarget: TObject = nil; ATargetPropertyName: string = '';
      ABindingMode: TBindingMode = BindingModeDefault;
      AConverter: IValueConverter = nil); reintroduce; overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure UpdateSource(IgnoreBindingMode: Boolean = True);
    procedure UpdateTarget(IgnoreBindingMode: Boolean = True);
    function Validate: Boolean;

    procedure BeginEdit;
    procedure CancelEdit;
    procedure CommitEdit;

    property Active: Boolean read FActive write SetActive;
    property BindingGroup: TBindingGroup read FBindingGroup write SetBindingGroup;
    property Converter: IValueConverter read FConverter write SetConverter;
    property OnPropertyChanged: IEvent<TPropertyChangedEvent>
      read GetOnPropertyChanged;
    property OnValidation: IEvent<TValidationEvent> read GetOnValidation;
    property SourceProperty: IMemberExpression read FSourceProperty;
    property TargetProperty: IMemberExpression read FTargetProperty;
    property ValidationErrors: IList<IValidationResult> read GetValidationErrors;
    property ValidationRules: IList<IValidationRule> read GetValidationRules;
  published
    property BindingMode: TBindingMode read FBindingMode write SetBindingMode
      default BindingModeDefault;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Managed: Boolean read FManaged write FManaged default True;
    property NotifyOnSourceUpdated: Boolean read FNotifyOnSourceUpdated
      write FNotifyOnSourceUpdated default False;
    property NotifyOnTargetUpdated: Boolean read FNotifyOnTargetUpdated
      write FNotifyOnTargetUpdated default False;
    property OnSourceUpdated: TPropertyChangedEvent read FOnSourceUpdated
      write FOnSourceUpdated;
    property OnTargetUpdated: TPropertyChangedEvent read FOnTargetUpdated
      write FOnTargetUpdated;
    property PreventFocusChange: Boolean
      read FPreventFocusChange write FPreventFocusChange default False;
    property Source: TObject read FSource write SetSource;
    property SourcePropertyName: string read FSourcePropertyName
      write SetSourcePropertyName;
    property SourceUpdateTrigger: TUpdateTrigger read FSourceUpdateTrigger
      write FSourceUpdateTrigger default UpdateTriggerDefault;
    property Target: TObject read FTarget write SetTarget;
    property TargetPropertyName: string read FTargetPropertyName
      write SetTargetPropertyName;
    property TargetUpdateTrigger: TUpdateTrigger read FTargetUpdateTrigger
      write FTargetUpdateTrigger default UpdateTriggerDefault;
    property TriggerMode: TTriggerMode read FTriggerMode
      write FTriggerMode default TriggerModeDefault;
    property ValidatesOnDataErrors: Boolean
      read FValidatesOnDataErrors write SetValidatesOnDataErrors default False;
  end;

  IBindable = interface
    function GetBinding: TBinding;
    property Binding: TBinding read GetBinding;
  end;

  TBindingCollection = class(TOwnedCollection<TBinding>)
  public
    constructor Create(AOwner: TPersistent); override;
  end;

  TBindingGroup = class(TComponent, INotifyPropertyChanged, IValidatable)
  private
    FBindings: TBindingCollection;
    FEditing: Boolean;
    FItems: IList<TObject>;
    FOnPropertyChanged: Event<TPropertyChangedEvent>;
    FValidationErrors: IList<IValidationResult>;
    FValidationRules: IList<IValidationRule>;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    procedure SetBindings(const Value: TBindingCollection);
    procedure SetEditing(const Value: Boolean);
    function GetItems: IList<TObject>;
    function GetValidationErrors: IList<IValidationResult>;
    function GetValidationRules: IList<IValidationRule>;
  protected
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure DoValidationErrorsChanged(Sender: TObject;
      const Item: IValidationResult; Action: TCollectionChangedAction);
    procedure DoValidationRulesChanged(Sender: TObject;
      const Item: IValidationRule; Action: TCollectionChangedAction);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure ReadBindings(AReader: TReader);
    procedure WriteBindings(AWriter: TWriter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddBinding(ASource: TObject = nil; ASourcePropertyName: string = '';
      ATarget: TObject = nil; ATargetPropertyName: string = '';
      ABindingMode: TBindingMode = BindingModeDefault;
      AConverter: IValueConverter = nil): TBinding;
    procedure Bind(ASource: TObject; ATarget: TObject = nil);

    function GetBindingForTarget(ATarget: TObject): TBinding;

    procedure BeginEdit;
    procedure CancelEdit;
    procedure CommitEdit;

    procedure NotifyPropertyChanged(ASender: TObject; APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    function TryGetValue(AItem: TObject; APropertyName: string;
      out AValue: TValue): Boolean;
    procedure UpdateSources(IgnoreBindingMode: Boolean = True);
    procedure UpdateTargets(IgnoreBindingMode: Boolean = True);
    function Validate: Boolean;

    property Bindings: TBindingCollection read FBindings write SetBindings;
    property Editing: Boolean read FEditing;
    property Items: IList<TObject> read GetItems;
    property OnPropertyChanged: IEvent<TPropertyChangedEvent>
      read GetOnPropertyChanged;
    property ValidationErrors: IList<IValidationResult> read GetValidationErrors;
    property ValidationRules: IList<IValidationRule> read GetValidationRules;
  end;

  EBindingException = class(Exception);

function FindBindingGroup(AComponent: TPersistent): TBindingGroup;
function GetBindingForComponent(AComponent: TPersistent): TBinding;
procedure NotifyPropertyChanged(ASender: TObject; const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger = utPropertyChanged); overload;

implementation

uses
  System.Types,
  DSharp.Bindings.Exceptions,
  DSharp.Bindings.Validations,
  DSharp.Core.Reflection,
  DSharp.Core.Utils,
  Spring.Collections.Lists,
  StrUtils,
  TypInfo;

var
  BindingGroups: TList;

function FindBindingGroup(AComponent: TPersistent): TBindingGroup;
var
  i: Integer;
  LOwner: TPersistent;
  LType: TRttiType;
begin
  Result := nil;
  if AComponent is TCollectionItem then
  begin
    LOwner := GetUltimateOwner(TCollectionItem(AComponent));
  end
  else
  if AComponent is TCollection then
  begin
    LOwner := GetUltimateOwner(TCollection(AComponent));
  end
  else
  if TryGetRttiType(AComponent.ClassInfo, LType) and (LType.IsInheritedFrom('TForm')
    or LType.IsInheritedFrom('TFrame') or LType.IsInheritedFrom('TDataModule')) then
  begin
    LOwner := AComponent;
  end
  else
  begin
    LOwner := GetUltimateOwner(AComponent);
  end;
  if Assigned(LOwner) and (LOwner is TComponent) then
  begin
    for i := 0 to Pred(TComponent(LOwner).ComponentCount) do
    begin
      if TComponent(LOwner).Components[i] is TBindingGroup then
      begin
        Result := TBindingGroup(TComponent(LOwner).Components[i]);
        Break;
      end;
    end;
  end
end;

function GetBindingForComponent(AComponent: TPersistent): TBinding;
var
  LBindingGroup: TBindingGroup;
begin
  Result := nil;
  LBindingGroup := FindBindingGroup(AComponent);
  if Assigned(LBindingGroup) then
  begin
    Result := LBindingGroup.GetBindingForTarget(AComponent);
  end;
end;

function IsPropertyExtension(const Name: string): Boolean;
var
  LPos: Integer;
  LPropertyName: string;
begin
  LPropertyName := Name;

  repeat
    LPos := PosEx('[', LPropertyName, LastDelimiter('.', LPropertyName));
    if LPos > 0 then
    begin
      LPropertyName := LeftStr(LPropertyName, Pred(LPos));
    end;
    Result := TRttiPropertyExtension.FindByName(LPropertyName) <> nil;
    if not Result then
    begin
      LPropertyName := Copy(LPropertyName, 1, LastDelimiter('.', LPropertyName) - 1);
    end;
  until Result or not ContainsText(LPropertyName, '.');
end;

function IsRootProperty(const Name: string; Expression: IParameterExpression): Boolean;
var
  LRoot: string;
begin
  LRoot := Expression.Name;
  if ContainsText(LRoot, '.') then
  begin
    LRoot := LeftStr(LRoot, Pred(Pos('.', LRoot)));
  end;
  if ContainsText(LRoot, '[') then
  begin
    LRoot := LeftStr(LRoot, Pred(Pos('[', LRoot)));
  end;
  Result := SameText(Name, LRoot);

  if not Result and ContainsStr(Name, '.') then
    Result := IsPropertyExtension(Name);
end;

procedure NotifyPropertyChanged(ASender: TObject; const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
var
  i: Integer;
begin
  for i := 0 to Pred(BindingGroups.Count) do
  begin
    TBindingGroup(BindingGroups[i]).NotifyPropertyChanged(
      ASender, APropertyName, AUpdateTrigger);
  end;
end;

{ BindingAttribute }

constructor BindingAttribute.Create(const ASourcePropertyName,
  ATargetPropertyName: string; ABindingMode: TBindingMode);
begin
  FBindingMode := ABindingMode;
  FSourcePropertyName := ASourcePropertyName;
  FTargetPropertyName := ATargetPropertyName;
end;

{ TBinding }

class constructor TBinding.Create;
begin
  DataErrorValidationRule := TDataErrorValidationRule.Create();
end;

constructor TBinding.Create(Collection: TCollection);
begin
  inherited;

  FNotificationHandler := TNotificationHandler.Create(nil);
  FNotificationHandler.OnNotification := Notification;
  FValidationErrors := TCollections.CreateInterfaceList<IValidationResult>;
  FValidationErrors.OnChanged.Add(DoValidationErrorsChanged);
  FValidationRules := TCollections.CreateInterfaceList<IValidationRule>;
  FValidationRules.OnChanged.Add(DoValidationRulesChanged);

  FBindingMode := BindingModeDefault;
  FEnabled := True;

  if Assigned(Collection) then
  begin
    FBindingGroup := TBindingGroup(Collection.Owner);
    FManaged := Assigned(FBindingGroup);
    FActive := FEnabled and Assigned(FBindingGroup)
      and not (csDesigning in FBindingGroup.ComponentState);
  end;
end;

constructor TBinding.Create(ASource: TObject; ASourcePropertyName: string;
  ATarget: TObject; ATargetPropertyName: string; ABindingMode: TBindingMode;
  AConverter: IValueConverter);
begin
  Create(nil);
  FActive := False;

  FBindingMode := ABindingMode;

  FSourcePropertyName := ASourcePropertyName;
  SetSource(ASource);
  FTargetPropertyName := ATargetPropertyName;
  SetTarget(ATarget);

  SetConverter(AConverter);
  FActive := True;

  UpdateTarget(True);
end;

destructor TBinding.Destroy;

  function IsValid(AObject: TObject): Boolean;
  {$IFDEF VER210}
  type
    PNativeInt = ^NativeInt;
  {$ENDIF}
  begin
    Result := False;
    if Assigned(AObject) then
    try
      if PNativeInt(AObject)^ > $FFFF then  // "hotfix" to prevent some access violations (no clue if this works) :)
        Result := PNativeInt(AObject)^ = PNativeInt(PNativeInt(AObject)^ + vmtSelfPtr)^;
    except
    end;
  end;

begin
  if IsValid(FSource) then  // workaround for already freed non TComponent source
  begin
    SetSource(nil);
  end;

  SetBindingGroup(nil);
  SetTarget(nil);

  FNotificationHandler.Free();
  FValidationErrors.OnChanged.Remove(DoValidationErrorsChanged);
  FValidationRules.OnChanged.Remove(DoValidationRulesChanged);

  inherited;
end;

procedure TBinding.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBinding) then
  begin
    Self.Active := TBinding(Source).Active;
//    Self.BindingGroup := TBinding(Source).BindingGroup;
    Self.Converter := TBinding(Source).Converter;
    Self.Enabled := TBinding(Source).Enabled;
    Self.Source := TBinding(Source).Source;
    Self.SourcePropertyName := TBinding(Source).SourcePropertyName;
    Self.SourceUpdateTrigger := TBinding(Source).SourceUpdateTrigger;
    Self.Target := TBinding(Source).Target;
    Self.TargetPropertyName := TBinding(Source).TargetPropertyName;
    Self.TargetUpdateTrigger := TBinding(Source).TargetUpdateTrigger;
  end;
end;

procedure TBinding.BeginEdit;
var
  LEditable: IEditable;
begin
  if FActive and Assigned(FSource)
    and Supports(FSource, IEditable, LEditable) then
  begin
    LEditable.BeginEdit();
  end;
end;

procedure TBinding.BeginUpdateSource;
begin
  Inc(FUpdateSourceCount);
end;

procedure TBinding.BeginUpdateTarget;
begin
  Inc(FUpdateTargetCount);
end;

procedure TBinding.BeginValidate;
begin
  Inc(FValidateCount);
end;

procedure TBinding.CancelEdit;
var
  LEditable: IEditable;
begin
  if FActive and Assigned(FSource)
    and Supports(FSource, IEditable, LEditable) then
  begin
    LEditable.CancelEdit();
  end;
end;

procedure TBinding.CommitEdit;
var
  LEditable: IEditable;
begin
  if FActive and Assigned(FSource)
    and Supports(FSource, IEditable, LEditable) then
  begin
    LEditable.EndEdit();
  end;
end;

function TBinding.CanUpdateSource(const AEventArgs: IPropertyChangedEventArgs): Boolean;
var
  LEventArgs: TPropertyChangedEventArgsEx;
  LUpdateTrigger: TUpdateTrigger;
begin
  LEventArgs := AEventArgs as TPropertyChangedEventArgsEx;
  if Assigned(LEventArgs) then
    LUpdateTrigger := LEventArgs.UpdateTrigger
  else
    LUpdateTrigger := utPropertyChanged;

  Result := FActive and not IsUpdatingSource
    and (FBindingMode in [bmTwoWay..bmOneWayToSource])
    and (LUpdateTrigger = FSourceUpdateTrigger)
    and ((FTriggerMode = tmTwoWay) or not IsUpdatingTarget);
end;

function TBinding.CanUpdateTarget(const AEventArgs: IPropertyChangedEventArgs): Boolean;
var
  LEventArgs: TPropertyChangedEventArgsEx;
  LUpdateTrigger: TUpdateTrigger;
begin
  LEventArgs := AEventArgs as TPropertyChangedEventArgsEx;
  if Assigned(LEventArgs) then
    LUpdateTrigger := LEventArgs.UpdateTrigger
  else
    LUpdateTrigger := utPropertyChanged;

  Result := FActive and not IsUpdatingTarget
    and (FBindingMode in [bmOneWay..bmTwoWay])
    and (LUpdateTrigger = FTargetUpdateTrigger)
    and ((FTriggerMode = tmTwoWay) or not IsUpdatingSource);
end;

procedure TBinding.CompileExpressions;
begin
  if Assigned(FSourceProperty) and Assigned(FTargetProperty) then
  begin
    if Assigned(FConverter) then
    begin
      FUpdateSourceExpression :=
        Expression.Convert(
          FSourceProperty,
          FTargetProperty,
          function(const Value: TValue): TValue
          begin
            Result := FConverter.ConvertBack(Value);
          end).Compile();

      FUpdateTargetExpression :=
        Expression.Convert(
          FTargetProperty,
          FSourceProperty,
          function(const Value: TValue): TValue
          begin
            Result := FConverter.Convert(Value);
          end).Compile();
    end
    else
    begin
      FUpdateSourceExpression :=
        Expression.Convert(
          FSourceProperty,
          FTargetProperty).Compile();

      FUpdateTargetExpression :=
        Expression.Convert(
          FTargetProperty,
          FSourceProperty).Compile();
    end;
  end
  else
  begin
    FUpdateSourceExpression := Expression.Empty.Compile();
    FUpdateTargetExpression := Expression.Empty.Compile();
  end;
end;

procedure TBinding.DoTargetUpdated(ASender: TObject;
  const AEventArgs: IPropertyChangedEventArgs);
begin
  if FNotifyOnTargetUpdated and Assigned(FOnTargetUpdated) then
  begin
    FOnTargetUpdated(ASender, AEventArgs);
  end;
end;

procedure TBinding.DoValidationErrorsChanged(Sender: TObject;
  const Item: IValidationResult; Action: TCollectionChangedAction);
begin
  if Target is TComponent then
  begin
    // set to nil first to trigger changed event
    Validation.Errors.SetValue(TComponent(Target), nil);
    Validation.Errors.SetValue(TComponent(Target), FValidationErrors);
  end;

  DoPropertyChanged('ValidationErrors');
end;

procedure TBinding.DoValidationRulesChanged(Sender: TObject;
  const Item: IValidationRule; Action: TCollectionChangedAction);
begin
  DoPropertyChanged('ValidationRules');
end;

procedure TBinding.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, TPropertyChangedEventArgsEx.Create(
    APropertyName, AUpdateTrigger) as IPropertyChangedEventArgs);
end;

procedure TBinding.DoSourceCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LCollectionView: ICollectionView;
  LEvent: IEvent<TCollectionChangedEvent>;
begin
  if Supports(FTarget, ICollectionView, LCollectionView) then
  begin
    LEvent := LCollectionView.OnCollectionChanged;
    LEvent.Invoke(Sender, Item, Action);
  end;
end;

procedure TBinding.DoSourcePropertyChanged(ASender: TObject;
  const AEventArgs: IPropertyChangedEventArgs);
begin
  if CanUpdateTarget(AEventArgs)
    and IsRootProperty(AEventArgs.PropertyName, FSourceProperty) then
  begin
    BeginUpdateTarget();
    try
      DoTargetUpdated(ASender, AEventArgs);

      UpdateTarget(True);

      ValidateOnTargetUpdated();
    finally
      EndUpdateTarget();
    end;
  end;
end;

procedure TBinding.DoSourceUpdated(ASender: TObject;
  const AEventArgs: IPropertyChangedEventArgs);
begin
  if FNotifyOnSourceUpdated and Assigned(FOnSourceUpdated) then
  begin
    FOnSourceUpdated(ASender, AEventArgs);
  end;
end;

procedure TBinding.DoTargetPropertyChanged(ASender: TObject;
  const AEventArgs: IPropertyChangedEventArgs);
begin
  if CanUpdateSource(AEventArgs) and not IsValidating
    and IsRootProperty(AEventArgs.PropertyName, FTargetProperty) then
  begin
    BeginUpdateSource();
    try
      if Validate() then
      begin
        DoSourceUpdated(ASender, AEventArgs);

        UpdateSource(True);
      end
      else
      begin
        if FPreventFocusChange then
        begin
          RaiseValidationError();
        end;
      end;
    finally
      EndUpdateSource();
    end;

    if not ValidateCommitted()
      and (FSourceUpdateTrigger = utLostFocus) and FPreventFocusChange then
    begin
      RaiseValidationError();
    end;
  end;
end;

procedure TBinding.EndUpdateSource;
begin
  Dec(FUpdateSourceCount);
end;

procedure TBinding.EndUpdateTarget;
begin
  Dec(FUpdateTargetCount);
end;

procedure TBinding.EndValidate;
begin
  Dec(FValidateCount);
end;

function TBinding.GetDisplayName: string;
const
  BindingModeNames: array[TBindingMode] of string = ('-->', '<->', '<--', '*->');
begin
  Result := inherited;
  if Assigned(FSource) and (FSource is TComponent)
    and Assigned(FTarget) and (FTarget is TComponent)
    and not SameText(Trim(FSourcePropertyName), EmptyStr)
    and not SameText(Trim(FTargetPropertyName), EmptyStr) then
  begin
    Result := Result + Format(' (%s.%s %s %s.%s)', [
      TComponent(FSource).Name, FSourcePropertyName, BindingModeNames[FBindingMode],
      TComponent(FTarget).Name, FTargetPropertyName]);
  end
  else
  begin
    Result := Result + ' (definition uncomplete)';
  end;
end;

function TBinding.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged;
end;

function TBinding.GetOnValidation: IEvent<TValidationEvent>;
begin
  Result := FOnValidation;
end;

function TBinding.GetValidationErrors: IList<IValidationResult>;
begin
  Result := FValidationErrors;
end;

function TBinding.GetValidationRules: IList<IValidationRule>;
begin
  Result := FValidationRules;
end;

function TBinding.IsUpdatingSource: Boolean;
begin
  Result := FUpdateSourceCount > 0;
end;

function TBinding.IsUpdatingTarget: Boolean;
begin
  Result := FUpdateTargetCount > 0;
end;

function TBinding.IsValidating: Boolean;
begin
  Result := FValidateCount > 0;
end;

procedure TBinding.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  if AOperation = opRemove then
  begin
    if AComponent = FSource then
    begin
      FSource := nil;
      if not FManaged then
      begin
        Free;
      end;
    end;

    if AComponent = FTarget then
    begin
      FTarget := nil;
      if not FManaged then
      begin
        Free;
      end;
    end;
  end;
end;

function TBinding.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TBinding.RaiseValidationError;
begin
  raise EValidationError.Create(Self);
end;

procedure TBinding.SetActive(const Value: Boolean);
begin
  if FEnabled and (FActive <> Value) then
  begin
    FActive := Value;

    if FActive then
    begin
      UpdateTarget(True);

      ValidateOnTargetUpdated();
    end;
  end;
end;

procedure TBinding.SetBindingGroup(const Value: TBindingGroup);
begin
  if FBindingGroup <> Value then
  begin
    FBindingGroup := Value;

    if Assigned(FBindingGroup) then
    begin
      Collection := FBindingGroup.Bindings;
    end;
  end;
end;

procedure TBinding.SetBindingMode(const Value: TBindingMode);
begin
  if FBindingMode <> Value then
  begin
    FBindingMode := Value;

    SetTargetProperty();
    SetSourceProperty();
  end;
end;

procedure TBinding.SetConverter(const Value: IValueConverter);
begin
  if FConverter <> Value then
  begin
    FConverter := Value;
    CompileExpressions();
    UpdateTarget(True);
  end;
end;

procedure TBinding.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if not FEnabled and FActive then
  begin
    FActive := False;
  end;
end;

procedure TBinding.SetSource(const Value: TObject);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: IEvent<TPropertyChangedEvent>;
  LSourceCollectionChanged: IEvent<TCollectionChangedEvent>;
begin
  if FSource <> Value then
  begin
    if Assigned(FSource) then
    begin
      if FSource is TComponent then
      begin
        TComponent(FSource).RemoveFreeNotification(FNotificationHandler);
      end;

      if Supports(FSource, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        if Assigned(LPropertyChanged) then
        begin
          LPropertyChanged.Remove(DoSourcePropertyChanged);
        end;
      end;

      if Assigned(FSourceCollectionChanged) then
      begin
        LSourceCollectionChanged := FSourceCollectionChanged.OnCollectionChanged;
        LSourceCollectionChanged.Remove(DoSourceCollectionChanged);
      end;
    end;

    FSource := Value;
    SetSourceProperty();

    if Assigned(FSource) then
    begin
      if FSource is TComponent then
      begin
        TComponent(FSource).FreeNotification(FNotificationHandler);
      end;

      if Supports(FSource, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        LPropertyChanged.Add(DoSourcePropertyChanged);
      end;

      // maybe the source itself is a collection?
      if not Assigned(FSourceCollectionChanged)
        and Supports(FSource, INotifyCollectionChanged, FSourceCollectionChanged) then
      begin
        LSourceCollectionChanged := FSourceCollectionChanged.OnCollectionChanged;
        LSourceCollectionChanged.Add(DoSourceCollectionChanged);
      end;
    end;

    UpdateTarget(True);
  end;
end;

procedure TBinding.SetSourceProperty;
var
  LSourceCollectionChanged: IEvent<TCollectionChangedEvent>;
begin
  if not Assigned(BindingGroup) or not (csDesigning in BindingGroup.ComponentState) then
  begin
    if Assigned(FSourceCollectionChanged) then
    begin
      LSourceCollectionChanged := FSourceCollectionChanged.OnCollectionChanged;
      LSourceCollectionChanged.Remove(DoSourceCollectionChanged);
    end;

    FSourceProperty := Expression.PropertyAccess(
      Expression.Constant(FSource), FSourcePropertyName);

    CompileExpressions();

    if Assigned(FSourceProperty) and FSourceProperty.Member.RttiType.IsInstance
      and Supports(FSourceProperty.Value.AsObject,
      INotifyCollectionChanged, FSourceCollectionChanged) then
    begin
      LSourceCollectionChanged := FSourceCollectionChanged.OnCollectionChanged;
      LSourceCollectionChanged.Add(DoSourceCollectionChanged);
    end;
  end;
end;

procedure TBinding.SetSourcePropertyName(const Value: string);
begin
  if not SameText(FSourcePropertyName, Value) then
  begin
    FSourcePropertyName := Value;
    if Assigned(FSource) then
    begin
      SetSourceProperty;

      UpdateTarget(True);
    end;
  end;
end;

procedure TBinding.SetTarget(const Value: TObject);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  if FTarget <> Value then
  begin
    if Assigned(FTarget) then
    begin
      if FTarget is TComponent then
      begin
        TComponent(FTarget).RemoveFreeNotification(FNotificationHandler);
      end;

      if Supports(FTarget, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        LPropertyChanged.Remove(DoTargetPropertyChanged);
      end;
    end;

    FTarget := Value;
    SetTargetProperty();

    if Assigned(FTarget) then
    begin
      if FTarget is TComponent then
      begin
        TComponent(FTarget).FreeNotification(FNotificationHandler);
      end;

      if Supports(FTarget, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        LPropertyChanged.Add(DoTargetPropertyChanged);
      end;
    end;

    UpdateTarget(True);
  end;
end;

procedure TBinding.SetTargetProperty;
begin
  if not Assigned(BindingGroup) or not (csDesigning in BindingGroup.ComponentState) then
  begin
    FTargetProperty := Expression.PropertyAccess(
      Expression.Constant(FTarget), FTargetPropertyName);

    CompileExpressions();
  end;
end;

procedure TBinding.SetTargetPropertyName(const Value: string);
begin
  if not SameText(FTargetPropertyName, Value) then
  begin
    FTargetPropertyName := Value;
    if Assigned(FTarget) then
    begin
      SetTargetProperty();

      UpdateTarget(True);
    end;
  end;
end;

procedure TBinding.SetValidatesOnDataErrors(const Value: Boolean);
begin
  if FValidatesOnDataErrors <> Value then
  begin
    FValidatesOnDataErrors := Value;
    if FValidatesOnDataErrors then
    begin
      ValidateOnTargetUpdated;
    end;
  end;
end;

procedure TBinding.UpdateSource(IgnoreBindingMode: Boolean = True);
begin
  if Assigned(Self) and FActive
    and (IgnoreBindingMode or (FBindingMode in [bmTwoWay..bmOneWayToSource]))
    and Assigned(FTarget) and Assigned(FTargetProperty)
    and Assigned(FSource) and Assigned(FSourceProperty)
    and FTargetProperty.Member.IsReadable and FSourceProperty.Member.IsWritable then
  begin
    BeginUpdateSource();
    try
      FUpdateSourceExpression();
      FValidationErrors.Clear();

      ValidateCommitted();
    finally
      EndUpdateSource();
    end;
  end;
end;

procedure TBinding.UpdateTarget(IgnoreBindingMode: Boolean = True);
var
  LSourceCollectionChanged: IEvent<TCollectionChangedEvent>;
begin
  if Assigned(Self) and FActive
    and (IgnoreBindingMode or (FBindingMode in [bmOneWay..bmTwoWay]))
    and (not Assigned(FBindingGroup) or not (csLoading in FBindingGroup.ComponentState))
    and Assigned(FTarget) and Assigned(FTargetProperty)
    and Assigned(FSource) and Assigned(FSourceProperty)
    and FTargetProperty.Member.IsWritable and FSourceProperty.Member.IsReadable then
  begin
    BeginUpdateTarget();
    try
      if Assigned(FSourceCollectionChanged) then
      begin
        LSourceCollectionChanged := FSourceCollectionChanged.OnCollectionChanged;
        LSourceCollectionChanged.Remove(DoSourceCollectionChanged);
      end;

      FSourceCollectionChanged := nil;

      if FSourceProperty.Member.RttiType.IsInstance
        and Supports(FSourceProperty.Value.AsObject,
        INotifyCollectionChanged, FSourceCollectionChanged) then
      begin
        LSourceCollectionChanged := FSourceCollectionChanged.OnCollectionChanged;
        LSourceCollectionChanged.Add(DoSourceCollectionChanged);
      end;

      FUpdateTargetExpression();
      FValidationErrors.Clear();
    finally
      EndUpdateTarget();
    end;
  end;
end;

function TBinding.Validate: Boolean;
var
  LConverted: Boolean;
  LSourceValue: TValue;
  LTargetValue: TValue;
  LValidationResult: IValidationResult;
  LValidationRule: IValidationRule;
begin
  FValidationErrors.Clear();
  Result := True;

  LValidationRule := nil;
  BeginValidate();
  try
    try
      if Assigned(FTarget) and Assigned(FTargetProperty)
        and FTargetProperty.Member.IsReadable then
      begin
        LTargetValue := FTargetProperty.Value;

        for LValidationRule in FValidationRules do
        begin
          if LValidationRule.ValidationStep = vsRawProposedValue then
          begin
            LValidationResult := LValidationRule.Validate(LTargetValue);
            if Assigned(LValidationResult) then
            begin
              FOnValidation.Invoke(Self, LValidationRule, LValidationResult);
              if not LValidationResult.IsValid then
              begin
                FValidationErrors.Add(LValidationResult);
                Result := False;
                Break;
              end;
            end;
          end;
        end;

        if Result then
        begin
          LConverted := False;

          for LValidationRule in FValidationRules do
          begin
            if LValidationRule.ValidationStep = vsConvertedProposedValue then
            begin
              if not LConverted then
              begin
                if Assigned(FConverter) then
                begin
                  LSourceValue := FConverter.ConvertBack(LTargetValue);
                end
                else
                begin
                  LSourceValue := LTargetValue;
                end;

                LConverted := True;
              end;

              LValidationResult := LValidationRule.Validate(LSourceValue);
              FOnValidation.Invoke(Self, LValidationRule, LValidationResult);
              if not LValidationResult.IsValid then
              begin
                FValidationErrors.Add(LValidationResult);
                Result := False;
                Break;
              end;
            end;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        FOnValidation.Invoke(Self, LValidationRule, TValidationResult.Create(False, E.Message));
        Result := False;
      end;
    end;
  finally
    EndValidate();
  end;
end;

function TBinding.ValidateCommitted: Boolean;
var
  LValidationResult: IValidationResult;
begin
  Result := True;

  if FValidatesOnDataErrors then
  begin
    BeginValidate();
    try
      LValidationResult := DataErrorValidationRule.Validate(TValue.From<TBinding>(Self));
      if not LValidationResult.IsValid then
      begin
        FValidationErrors.Add(LValidationResult);
        Result := False;
      end;
    finally
      EndValidate();
    end;
  end;
end;

function TBinding.ValidateOnTargetUpdated: Boolean;
var
  LTargetValue: TValue;
  LValidationRule: IValidationRule;
  LValidationResult: IValidationResult;
begin
  Result := True;

  BeginValidate();
  try
    if Assigned(FTarget) and Assigned(FTargetProperty)
      and FTargetProperty.Member.IsReadable then
    begin
      LTargetValue := FTargetProperty.Value;

      for LValidationRule in FValidationRules do
      begin
        if LValidationRule.ValidatesOnTargetUpdated then
        begin
          LValidationResult := LValidationRule.Validate(LTargetValue);
          if Assigned(LValidationResult) then
          begin
            FOnValidation.Invoke(Self, LValidationRule, LValidationResult);
            if not LValidationResult.IsValid then
            begin
              FValidationErrors.Add(LValidationResult);
              Result := False;
              Break;
            end;
          end;
        end;
      end;
    end;
  finally
    EndValidate();
  end;
end;

function TBinding._AddRef: Integer;
begin
  Result := -1;
end;

function TBinding._Release: Integer;
begin
  Result := -1;
end;

{ TBindingCollection }

constructor TBindingCollection.Create(AOwner: TPersistent);
begin
  inherited;
  PropName := 'Bindings';
end;

{ TBindingGroup }

function TBindingGroup.AddBinding(ASource: TObject; ASourcePropertyName: string;
  ATarget: TObject; ATargetPropertyName: string; ABindingMode: TBindingMode;
  AConverter: IValueConverter): TBinding;
begin
  Result := Bindings.Add();
  Result.Source := ASource;
  Result.SourcePropertyName := ASourcePropertyName;
  Result.Target := ATarget;
  Result.TargetPropertyName := ATargetPropertyName;
  Result.BindingMode := ABindingMode;
  Result.Converter := AConverter;
end;

procedure TBindingGroup.BeginEdit;
var
  LBinding: TBinding;
begin
  if not FEditing then
  begin
    for LBinding in FBindings do
    begin
      LBinding.BeginEdit();
    end;

    SetEditing(True);
  end;
end;

procedure TBindingGroup.Bind(ASource, ATarget: TObject);
var
  LField: TRttiField;
  LBindingAttribute: BindingAttribute;
  LTarget: TObject;
begin
  if not Assigned(ASource) then
  begin
    raise EBindingException.Create('Binding source not specified');
  end;

  LTarget := ATarget;
  if not Assigned(LTarget) then
  begin
    LTarget := Owner;
    if not Assigned(LTarget) then
    begin
      raise EBindingException.Create('Binding target not specified');
    end;
  end;

  for LField in LTarget.GetFields do
  begin
    for LBindingAttribute in LField.GetCustomAttributes<BindingAttribute> do
    begin
      if LField.RttiType.IsInstance then
      begin
        AddBinding(ASource, LBindingAttribute.SourcePropertyName,
          LField.GetValue(LTarget).AsObject, LBindingAttribute.TargetPropertyName,
          LBindingAttribute.BindingMode);
      end
      else
      begin
        raise EBindingException.CreateFmt('Could not bind to %s', [LField.Name]);
      end;
    end;
  end;
end;

procedure TBindingGroup.CancelEdit;
var
  LBinding: TBinding;
begin
  if FEditing then
  begin
    for LBinding in FBindings do
    begin
      LBinding.CancelEdit();
    end;

    SetEditing(False);
  end;
end;

procedure TBindingGroup.CommitEdit;
var
  LBinding: TBinding;
begin
  if FEditing then
  begin
    for LBinding in FBindings do
    begin
      LBinding.CommitEdit();
    end;

    SetEditing(False);
  end;
end;

constructor TBindingGroup.Create(AOwner: TComponent);
var
  i: Integer;
begin
  FBindings := TBindingCollection.Create(Self);
  FItems := TList<TObject>.Create();
  FValidationErrors := TCollections.CreateInterfaceList<IValidationResult>;
  FValidationErrors.OnChanged.Add(DoValidationErrorsChanged);
  FValidationRules := TCollections.CreateInterfaceList<IValidationRule>;
  FValidationRules.OnChanged.Add(DoValidationRulesChanged);

  if Assigned(AOwner) then
  begin
    for i := 0 to AOwner.ComponentCount - 1 do
    begin
      if AOwner.Components[i] is TBindingGroup then
      begin
        raise EBindingException.Create('Only one binding group allowed');
      end;
    end;
  end;
  inherited;
  BindingGroups.Add(Self);
end;

destructor TBindingGroup.Destroy;
begin
  BindingGroups.Remove(Self);
  FBindings.Free();
  FValidationErrors.OnChanged.Remove(DoValidationErrorsChanged);
  FValidationRules.OnChanged.Remove(DoValidationRulesChanged);
  inherited;
end;

procedure TBindingGroup.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, TPropertyChangedEventArgsEx.Create(
    APropertyName, AUpdateTrigger) as IPropertyChangedEventArgs);
end;

procedure TBindingGroup.DoValidationErrorsChanged(Sender: TObject;
  const Item: IValidationResult; Action: TCollectionChangedAction);
begin
  DoPropertyChanged('ValidationErrors');
end;

procedure TBindingGroup.DoValidationRulesChanged(Sender: TObject;
  const Item: IValidationRule; Action: TCollectionChangedAction);
begin
  DoPropertyChanged('ValidationRules');
end;

function TBindingGroup.GetBindingForTarget(ATarget: TObject): TBinding;
var
  LBinding: TBinding;
begin
  Result := nil;
  for LBinding in Bindings do
  begin
    if LBinding.Target = ATarget then
    begin
      Result := LBinding;
      Break;
    end;
  end;
  if not Assigned(Result) then
  begin
    Result := Bindings.Add;
    Result.Active := False;
    Result.Target := ATarget;
  end;
end;

function TBindingGroup.GetItems: IList<TObject>;
var
  LBinding: TBinding;
begin
  // TODO: trigger this when bindings change
  FItems.Clear();

  for LBinding in FBindings do
  begin
    if Assigned(LBinding.Source) and not FItems.Contains(LBinding.Source) then
    begin
      FItems.Add(LBinding.Source);
    end;
  end;

  Result := FItems;
end;

function TBindingGroup.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged;
end;

function TBindingGroup.GetValidationErrors: IList<IValidationResult>;
begin
  Result := FValidationErrors;
end;

function TBindingGroup.GetValidationRules: IList<IValidationRule>;
begin
  Result := FValidationRules;
end;

procedure TBindingGroup.Loaded;
var
  LBinding: TBinding;
begin
  inherited;

  for LBinding in FBindings do
  begin
    if LBinding.Active then
    begin
      LBinding.UpdateTarget(True);
    end;
  end;
end;

procedure TBindingGroup.NotifyPropertyChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger);
var
  LBinding: TBinding;
begin
  if Assigned(Self) then
  begin
    for LBinding in FBindings do
    begin
      if LBinding.Source = ASender then
      begin
        LBinding.DoSourcePropertyChanged(ASender, TPropertyChangedEventArgsEx.Create(
          APropertyName, AUpdateTrigger) as IPropertyChangedEventArgs);
      end;
      if LBinding.Target = ASender then
      begin
        LBinding.DoTargetPropertyChanged(ASender, TPropertyChangedEventArgsEx.Create(
          APropertyName, AUpdateTrigger) as IPropertyChangedEventArgs);
      end;
    end;
  end;
end;

procedure TBindingGroup.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Bindings', ReadBindings, WriteBindings, True);
end;

procedure TBindingGroup.ReadBindings(AReader: TReader);
begin
  AReader.ReadValue();
  AReader.ReadCollection(FBindings);
end;

procedure TBindingGroup.SetBindings(const Value: TBindingCollection);
begin
  FBindings.Assign(Value);
end;

procedure TBindingGroup.SetEditing(const Value: Boolean);
begin
  FEditing := Value;
  DoPropertyChanged('Editing');
end;

function TBindingGroup.TryGetValue(AItem: TObject; APropertyName: string;
  out AValue: TValue): Boolean;
var
  LBinding: TBinding;
begin
  Result := False;

  for LBinding in FBindings do
  begin
    if (LBinding.Source = AItem)
      and SameText(LBinding.SourcePropertyName, APropertyName) then
    begin
      AValue := LBinding.SourceProperty.Value;
      Result := True;
      Break;
    end;
  end;
end;

procedure TBindingGroup.UpdateSources(IgnoreBindingMode: Boolean = True);
var
  LBinding: TBinding;
begin
  if Validate() then
  begin
    for LBinding in FBindings do
    begin
      LBinding.UpdateSource(IgnoreBindingMode);
    end;
  end;
end;

procedure TBindingGroup.UpdateTargets(IgnoreBindingMode: Boolean = True);
var
  LBinding: TBinding;
begin
  for LBinding in FBindings do
  begin
    LBinding.UpdateTarget(IgnoreBindingMode);
  end;
end;

function TBindingGroup.Validate: Boolean;
var
  LBinding: TBinding;
  LValidationResult: IValidationResult;
  LValidationRule: IValidationRule;
begin
  FValidationErrors.Clear();
  Result := True;

  for LValidationRule in FValidationRules do
  begin
    if LValidationRule.ValidationStep in [vsRawProposedValue, vsConvertedProposedValue] then
    begin
      LValidationResult := LValidationRule.Validate(Self);
      if not LValidationResult.IsValid then
      begin
        FValidationErrors.Add(LValidationResult);
        Result := False;
      end;
    end;
  end;

  for LBinding in FBindings do
  begin
    Result := LBinding.Validate() and Result;

    FValidationErrors.AddRange(LBinding.ValidationErrors.ToArray);
  end;
end;

procedure TBindingGroup.WriteBindings(AWriter: TWriter);
var
  i: Integer;
begin
  for i := Pred(FBindings.Count) downto 0 do
  begin
    if not Assigned(FBindings[i].Source) or not Assigned(FBindings[i].Target)
      or (Trim(FBindings[i].SourcePropertyName) = '')
      or (Trim(FBindings[i].TargetPropertyName) = '') then
    begin
      FBindings.Delete(i);
    end;
  end;
  AWriter.WriteCollection(FBindings);
end;

initialization
  BindingGroups := TList.Create();

finalization
  BindingGroups.Free();

end.
