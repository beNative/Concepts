unit zValueManager;

interface

uses
  System.Classes, System.TypInfo, System.Rtti, System.SysUtils, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  FloatConv,

  zObjInspTypes;

type
  TzFloatPreference = class(TPersistent)
  private
    FExpPrecision: Integer;
    FMaxDigits: Integer;
    FFormatOptions: TFloatFormatOptions;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property MaxDigits: Integer read FMaxDigits write FMaxDigits;
    property ExpPrecision: Integer read FExpPrecision write FExpPrecision;
    property FormatOptions: TFloatFormatOptions read FFormatOptions write FFormatOptions;
  end;

  TzCustomValueManager = class
  private
    FFloatPreference: TzFloatPreference;

    function GetFloatPreference: TzFloatPreference;
    procedure SetFloatPreference(const Value: TzFloatPreference);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    /// <summary> Use custom ListBox .
    /// </summary>
    function GetListClass(const PItem: PPropItem): TClass;
    procedure SetValue(const PItem: PPropItem; var Value: TValue); virtual;
    function StrToValue<T>(const PItem: PPropItem; const s: string): T;
    function GetValue(const PItem: PPropItem; const Value): TValue; virtual;
    function GetValueAs<T>(const Value: TValue): T;
    function GetValueName(const PItem: PPropItem): string; virtual;
    /// <summary> Check if item can assign value that is not listed in ListBox .
    /// </summary>
    function ValueHasOpenProbabilities(const PItem: PPropItem): Boolean; virtual;
    /// <summary> Check if value has an ExtraRect like (Color,Boolean)type .
    /// </summary>
    /// <returns> non zero to indicate that value must use an ExtraRect .
    /// </returns>
    function GetExtraRectWidth(const PItem: PPropItem): Integer; virtual;
    function GetValueType(const PItem: PPropItem): Integer; virtual;
    /// <summary> Paint item value name .
    /// </summary>
    procedure PaintValue(Canvas: TCanvas; Index: Integer; const PItem: PPropItem; R: TRect); virtual;
    /// <summary> Check if the current item can have button .
    /// </summary>
    function HasButton(const PItem: PPropItem): Boolean; virtual;
    /// <summary> Check if the current item can drop ListBox .
    /// </summary>
    function HasList(const PItem: PPropItem): Boolean; virtual;
    /// <summary> Check if the current item have customized dialog .
    /// </summary>
    function HasDialog(const PItem: PPropItem): Boolean; virtual;
    /// <summary> Get customized dialog for current item .
    /// </summary>
    function GetDialog(const PItem: PPropItem): TComponentClass; virtual;
    procedure DialogCode(const PItem: PPropItem; Dialog: TComponent; Code: Integer); virtual;
    /// <summary> Get the value returned after editing from the dialog .
    /// </summary>
    function DialogResultValue(const PItem: PPropItem; Dialog: TComponent): TValue; virtual;
    /// <summary> Return ListBox items for the current item .
    /// </summary>
    procedure GetListItems(const PItem: PPropItem; Items: TStrings); virtual;
    /// <summary> Get the value when the user click the ExtraRect .
    /// </summary>
    function GetExtraRectResultValue(const PItem: PPropItem): TValue; virtual;

    property FloatPreference : TzFloatPreference
      read GetFloatPreference write SetFloatPreference;
  end;

function IsPropTypeDerivedFromClass(const PropType: TRttiType; BaseClass: TClass): Boolean;
function IsValueSigned(const Value: TValue): Boolean;

implementation

uses
  Winapi.Windows,
  System.UITypes,
  Vcl.Dialogs, Vcl.Themes,

  zObjInspList, zObjInspector, zUtils, zObjInspDialogs, zStringsDialog,
  zGraphicDialog, zCollectionEditor;

const
  cDefaultMaxDigits = 2;
  cDefaultExpPrecision = 6;
  cDefaultFormatOptions: TFloatFormatOptions = [];
  cColorWidth = 13;

  vtUnknown = 0;
  vtEnum = 1;
  vtSet = 2;
  vtSetElement = 3;
  vtObj = 4;
  vtMethod = 5;
  vtBool = 6;
  vtString = 7;
  vtChar = 8;
  vtColor = 9;
  vtCursor = 10;
  vtFont = 11;
  vtIcon = 12;
  vtShortCut = 13;
  vtSingle = 14;
  vtDouble = 15;
  vtExtended = 16;

function GetMethodName(Value: TValue; Obj: TObject): String;
begin
  Result := '';
  if (not Assigned(Obj)) or (Value.Kind <> tkMethod) or (Value.IsEmpty) then
    Exit;
  Result := Obj.MethodName(TValueData(Value).FAsMethod.Code);
end;

function SetEnumToBoolean(SetValue, Value: Integer): Boolean;
var
  s: TIntegerSet;
begin
  Integer(s) := SetValue;
  Result := Value in s;
end;

function BooleanToStr(B: Boolean): string;
const
  BoolStrs: array [Boolean] of String = ('False', 'True');
begin
  Result := BoolStrs[B];
end;

function IsValueSigned(const Value: TValue): Boolean;
begin
  Result := (Value.TypeData.MinValue < 0) or (Value.TypeData.MinInt64Value < 0);
end;

function IsClassDerivedFromClass(const AClassType: TClass; BaseClass: TClass): Boolean;
var
  C: TClass;
begin
  Result := False;
  C := AClassType;
  while C <> nil do
  begin
    if C = BaseClass then
      Exit(True);
    C := C.ClassParent;
  end;
end;

function IsPropTypeDerivedFromClass(const PropType: TRttiType; BaseClass: TClass): Boolean;
var
  T: TRttiType;
begin
  Result := False;

  if PropType.TypeKind <> tkClass then
    Exit;
  T := PropType;
  while T <> nil do
  begin
    if T.Handle = PTypeInfo(BaseClass.ClassInfo) then
      Exit(True);
    T := T.BaseType; // Parent base class .
  end;
end;

function PropSameMethodType(AProp: TRttiType; AMethod: TRttiMethod): Boolean;
var
  LPType: TRttiInvokableType;
  LPropParamsList, LMethodParamsList: TArray<TRttiParameter>;
  LSPropParamType, LSMethodParamType: String;
  LPropParam, LMethodParam: TRttiParameter;
  L, i: Integer;
begin
  Result := False;
  if AProp is TRttiMethodType then
    if TRttiMethodType(AProp).MethodKind <> AMethod.MethodKind then
      Exit;
  if AProp is TRttiMethodType then
  begin
    LPType := TRttiInvokableType(AProp);
    { Check if had same method type ( Procedure or function) }
    Result := (AMethod.ReturnType = nil) and (LPType.ReturnType = nil);
    if not Result then
    begin
      Result := Assigned(AMethod.ReturnType);
      if not Result then
        Exit;
      Result := Assigned(LPType.ReturnType);
      if not Result then
        Exit;
      { Check if had same ReturnType if its a function }
      Result := LPType.ReturnType.Handle = AMethod.Handle;
      if not Result then
        Exit;
    end;
    { Check if had same Calling Convention }
    Result := AMethod.CallingConvention = LPType.CallingConvention;
    if not Result then
      Exit;

    { Check if had same parameters }
    LMethodParamsList := AMethod.GetParameters;
    LPropParamsList := LPType.GetParameters;
    Result := Length(LMethodParamsList) = Length(LPropParamsList);
    if not Result then
      Exit;
    L := Length(LMethodParamsList);
    for i := 0 to L - 1 do
    begin
      LSPropParamType := '';
      LSMethodParamType := '';
      LPropParam := LPropParamsList[i];
      LMethodParam := LMethodParamsList[i];
      if not SameText(LMethodParam.Name, LPropParam.Name) then
        Exit(False);
      if Assigned(LPropParam.ParamType) then
        LSPropParamType := LPropParam.ParamType.ToString;
      if Assigned(LMethodParam.ParamType) then
        LSMethodParamType := LMethodParam.ParamType.ToString;
      if not SameText(LSPropParamType, LSMethodParamType) then
        Exit(False);
    end;
  end;
end;

function GetEnumOrdValue(const Value: TValue): Integer;
var
  vd: TValueData;
begin
  Result := 0;
  vd := TValueData(Value);
  case Value.TypeData.OrdType of
    otSByte: Result := vd.FAsSByte;
    otUByte: Result := vd.FAsUByte;
    otSWord: Result := vd.FAsSWord;
    otUWord: Result := vd.FAsUWord;
    otSLong: Result := vd.FAsSLong;
    otULong: Result := vd.FAsULong;
  end;
end;

procedure TzFloatPreference.Assign(Source: TPersistent);
begin
  if Source is TzFloatPreference then
  begin
    MaxDigits := TzFloatPreference(Source).MaxDigits;
    ExpPrecision := TzFloatPreference(Source).ExpPrecision;
    FormatOptions := TzFloatPreference(Source).FormatOptions;
  end
  else
    inherited Assign(Source);
end;

procedure TzCustomValueManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FFloatPreference := TzFloatPreference.Create;
  FFloatPreference.ExpPrecision  := cDefaultExpPrecision;
  FFloatPreference.MaxDigits     := cDefaultMaxDigits;
  FFloatPreference.FormatOptions := cDefaultFormatOptions;
end;

procedure TzCustomValueManager.BeforeDestruction;
begin
  FFloatPreference.Free;
  inherited BeforeDestruction;
end;

procedure TzCustomValueManager.DialogCode(const PItem: PPropItem; Dialog: TComponent; Code: Integer);
begin

end;

function TzCustomValueManager.DialogResultValue(const PItem: PPropItem; Dialog: TComponent): TValue;
begin
  Result := PItem.Value;
  case GetValueType(PItem) of
    vtColor: Result := GetValue(PItem, TColorDialog(Dialog).Color);
    vtFont: Result := GetValue(PItem, TFontDialog(Dialog).Font);
  end;
end;

function TzCustomValueManager.GetDialog(const PItem: PPropItem): TComponentClass;
begin
  Result := nil;
  case GetValueType(PItem) of
    vtObj, vtIcon:
      begin
        if PItem.Value.AsObject is TStrings then
          Exit(TStringsDialog)
        else if PItem.Value.AsObject is TGraphic then
          Exit(TGraphicDialog)
        else if PItem.Value.AsObject is TCollection then
          Result := TzCollectionEditorDialog
      end;
    vtColor: Result := TColorDialog;
    vtFont: Result := TFontDialog;
  end;
end;

function TzCustomValueManager.GetExtraRectResultValue(const PItem: PPropItem): TValue;
var
  Value: TValue;
  BoolVal: Boolean;
  s: TIntegerSet;
  vOrd: Integer;
begin
  Value := PItem.Value;
  Result := Value;
  if PItem.IsSetElement then
  begin
    Integer(s) := GetEnumOrdValue(Value);
    vOrd := PItem.SetElementValue;
    if (vOrd in s) then
      BoolVal := False
    else
      BoolVal := True;
    Result := GetValue(PItem, BoolVal);
    Exit;
  end;
  case GetValueType(PItem) of
    vtBool:
      begin
        BoolVal := not(GetValueAs<Boolean>(Value));
        Result := GetValue(PItem, BoolVal);
      end;
  end;
end;

function TzCustomValueManager.GetExtraRectWidth(const PItem: PPropItem): Integer;
var
  LDetails: TThemedElementDetails;
  Size: TSize;
begin
  Result := 0;
  case GetValueType(PItem) of
    vtBool, vtSetElement:
      begin
        LDetails := StyleServices.GetElementDetails(tbCheckBoxUnCheckedNormal);
        StyleServices.GetElementSize(0, LDetails, esActual, Size);
        Result := Size.Width;
      end;
    vtColor: Result := cColorWidth + 1;
  end;
end;

function TzCustomValueManager.GetFloatPreference: TzFloatPreference;
begin
  Result := FFloatPreference;
end;

function TzCustomValueManager.GetListClass(const PItem: PPropItem): TClass;
var
  Value: TValue;
begin
  Value := PItem.Value;
  if Value.TypeInfo = TypeInfo(TColor) then
    Exit(TzPopupColorListBox)
  else if Value.TypeInfo = TypeInfo(TCursor) then
    Exit(TzPopupCursorListBox)
  else if Value.TypeInfo = TypeInfo(TShortCut) then
    Exit(TzPopupShortCutListBox);

  Result := TzPopupListBox;
end;

procedure TzCustomValueManager.GetListItems(const PItem: PPropItem; Items: TStrings);
var
  Value: TValue;
  i: Integer;
  Root: TCustomForm;
  procedure GetMethodsItems;
  var
    LCtx: TRttiContext;
    LType: TRttiType;
    LMethods: TArray<TRttiMethod>;
    LMethod: TRttiMethod;
  begin
    LCtx := TRttiContext.Create;
    LType := LCtx.GetType(Root.ClassInfo);
    LMethods := LType.GetDeclaredMethods;
    for LMethod in LMethods do
    begin
      if PropSameMethodType(PItem.Prop.PropertyType, LMethod) then
      begin
        Items.AddObject(LMethod.Name, TObject(LMethod.CodeAddress));
      end;
    end;
  end;

begin
  Value := PItem.Prop.GetValue(PItem.Instance);
  Root := PItem.ComponentRoot;
  if Value.TypeInfo = TypeInfo(TColor) then
    Exit;
  if Value.TypeInfo = TypeInfo(TCursor) then
    Exit;

  if PItem.Prop.PropertyType.TypeKind = tkClass then
  begin
    if Assigned(Root) then
      for i := 0 to Root.ComponentCount - 1 do
        if IsClassDerivedFromClass(Root.Components[i].ClassType, TRttiInstanceType(PItem.Prop.PropertyType).MetaclassType) then
          Items.AddObject(Root.Components[i].Name, TObject(Root.Components[i]));
    Exit;
  end else if PItem.Prop.PropertyType.TypeKind = tkMethod then
  begin
    if Assigned(Root) then
      GetMethodsItems;
    Exit;
  end;

  if PItem.IsSetElement then
  begin
    Items.AddObject('False', TObject(0));
    Items.AddObject('True', TObject(1));
    Exit;
  end;

  if PItem.Prop.PropertyType.IsOrdinal then
  begin
    for i := Value.TypeData.MinValue to Value.TypeData.MaxValue do
      Items.AddObject(GetEnumName(Value.TypeInfo, i), TObject(i));
    Exit;
  end;
end;

function TzCustomValueManager.GetValue(const PItem: PPropItem; const Value): TValue;
var
  Val: TValue;
  s: TIntegerSet;
begin
  Result := TValue.Empty;
  Val := PItem.Value;
  case GetValueType(PItem) of
    vtMethod:
      begin
        TValueData(Val).FAsMethod := TMethod(Value);
        Exit(Val);
      end;
    vtObj:
      begin
        TValueData(Val).FAsObject := TObject(Value);
        Exit(Val);
      end;
    vtString:
      begin
        Val := Val.From(String(Value));
        Exit(Val);
      end;
    vtChar:
      begin
        TValueData(Val).FAsUByte := Byte(Value);
        Exit(Val);
      end;
    vtSingle:
      begin
        TValueData(Val).FAsSingle := Single(Value);
        Exit(Val);
      end;
    vtDouble:
      begin
        TValueData(Val).FAsDouble := Double(Value);
        Exit(Val);
      end;
    vtExtended:
      begin
        TValueData(Val).FAsExtended := Extended(Value);
        Exit(Val);
      end;
  end;

  if PItem.IsSetElement then
  begin
    Integer(s) := GetEnumOrdValue(Val);
    if Boolean(Value) then
      Include(s, PItem.SetElementValue)
    else
      Exclude(s, PItem.SetElementValue);
    TValueData(Val).FAsSLong := Integer(s);
  end else begin
    if IsValueSigned(Val) then
      TValueData(Val).FAsSInt64 := Int64(Value)
    else
      TValueData(Val).FAsUInt64 := UInt64(Value);
  end;
  Result := Val;
end;

function TzCustomValueManager.GetValueAs<T>(const Value: TValue): T;
var
  sValue: Int64; // Signed Value !
  uValue: UInt64; // UnSigned Value !
  sR: T absolute sValue; // Signed Result !
  uR: T absolute uValue; // UnSigned Result !
  ValSign: Boolean;
begin

  { Just to avoid : E2506 Method of parameterized type declared in interface section must not use local symbol 'IsValueSigned'
    => We can not call IsValueSigned !
  }
  { To Avoid Range check error we must check ValSign }
  ValSign := (Value.TypeData.MinValue < 0) or (Value.TypeData.MinInt64Value < 0);

  { Always use 64 bit data !
    => Delphi automatically will cast the result !
  }
  if ValSign then
  begin
    sValue := (TValueData(Value).FAsSInt64);
    Result := sR;
  end else begin
    uValue := (TValueData(Value).FAsUInt64);
    Result := uR;
  end;
end;

function TzCustomValueManager.GetValueName(const PItem: PPropItem): string;
var
  Value: TValue;
  LObj: TObject;
  function GetComponentDisplayName(Prop: TRttiProperty; Comp: TComponent; Root: TCustomForm): String;
  var
    LCtx: TRttiContext;
    LType: TRttiType;
    LFields: TArray<TRttiField>;
    LField: TRttiField;
  begin
    LCtx := TRttiContext.Create;
    LType := LCtx.GetType(Root.ClassInfo);
    LFields := LType.GetDeclaredFields;
    for LField in LFields do
      if LField.FieldType.TypeKind = tkClass then
      begin
        if SameText(Comp.Name, LField.Name) then
          Exit(Comp.Name);
        // Component is declared in the designer form .
      end;
  end;

begin
  Value := PItem.Value;

  if PItem.IsSetElement then
  begin
    Result := BooleanToStr(SetEnumToBoolean(GetEnumOrdValue(Value), PItem.SetElementValue));
    Exit;
  end else if (Value.TypeInfo = TypeInfo(TColor)) then
  begin
    Result := ColorToString(GetValueAs<TColor>(Value));
    Exit;
  end else if (Value.TypeInfo = TypeInfo(TCursor)) then
  begin
    Result := CursorToString(GetValueAs<TCursor>(Value));
    Exit;
  end else if (Value.TypeInfo = TypeInfo(TShortCut)) then
  begin
    Result := fShortCutToText(GetValueAs<TShortCut>(Value));
    Exit;
  end else if Value.IsObject then
  begin
    Result := '';
    if not Value.IsEmpty then
    begin
      LObj := GetValueAs<TObject>(Value);
      Result := Format('(%s)', [LObj.ToString]);
      if (LObj is TComponent) then
        if TComponent(LObj).Name <> '' then
          Result := TComponent(LObj).Name;
    end;
    Exit;
  end else if PItem.Prop.PropertyType.TypeKind = tkMethod then
  begin
    Result := GetMethodName(Value, PItem.ComponentRoot);
    Exit;
  end else if PItem.Value.TypeInfo = TypeInfo(Single) then
  begin
    with FFloatPreference do
      Result := MyFormatFloat(TValueData(Value).FAsSingle, MaxDigits, ExpPrecision, FormatOptions);
    Exit;
  end else if PItem.Value.TypeInfo = TypeInfo(Double) then
  begin
    with FFloatPreference do
      Result := MyFormatFloat(TValueData(Value).FAsDouble, MaxDigits, ExpPrecision, FormatOptions);
    Exit;
  end else if PItem.Value.TypeInfo = TypeInfo(Extended) then
  begin
    with FFloatPreference do
      Result := MyFormatFloat(Value.AsExtended, MaxDigits, ExpPrecision, FormatOptions);
    Exit;
  end;

  Result := Value.ToString;
end;

function TzCustomValueManager.GetValueType(const PItem: PPropItem): Integer;
var
  Value: TValue;
begin
  Value := PItem.Value;
  if Value.TypeInfo = nil then
    Exit(vtUnknown);

  if Assigned(PItem.Prop) then
  begin
    case PItem.Prop.PropertyType.TypeKind of
      tkMethod: Exit(vtMethod);
      tkString, tkWString, tkUString: Exit(vtString);
      tkWChar, tkChar: Exit(vtChar);
    end;
  end;
  if Value.TypeInfo = TypeInfo(TColor) then
    Exit(vtColor)
  else if Value.TypeInfo = TypeInfo(TCursor) then
    Exit(vtCursor)
  else if Value.TypeInfo = TypeInfo(TShortCut) then
    Exit(vtShortCut)
  else if Value.TypeInfo = TypeInfo(TFont) then
    Exit(vtFont)
  else if Value.TypeInfo = TypeInfo(TIcon) then
    Exit(vtIcon)
  else if Value.TypeInfo = TypeInfo(Boolean) then
    Exit(vtBool)
  else if Value.TypeInfo = TypeInfo(Bool) then
    Exit(vtBool)
  else if Value.TypeInfo = TypeInfo(Single) then
    Exit(vtSingle)
  else if Value.TypeInfo = TypeInfo(Double) then
    Exit(vtDouble)
  else if Value.TypeInfo = TypeInfo(Extended) then
    Exit(vtExtended);

  if PItem.IsEnum then
    Exit(vtEnum);

  if PItem.IsSetElement then
    Exit(vtSetElement)
  else if PItem.IsSet then
    Exit(vtSet);

  if PItem.IsClass then
    Exit(vtObj);

  Result := vtUnknown;
end;

function TzCustomValueManager.HasButton(const PItem: PPropItem): Boolean;
begin
  Result := False;
  case GetValueType(PItem) of
    vtFont: Exit(True);
  end;
  if HasList(PItem) or HasDialog(PItem) then
    Result := True;
end;

function TzCustomValueManager.HasDialog(const PItem: PPropItem): Boolean;
begin
  Result := False;
  case GetValueType(PItem) of
    vtObj:
      begin
        if PItem.Value.AsObject is TStrings then
          Exit(True)
        else if PItem.Value.AsObject is TGraphic then
          Exit(True)
        else if PItem.Value.AsObject is TCollection then
          Result := True;
      end;
      vtString:
      begin
        Result := True;
      end;
      vtColor, vtFont, vtIcon:
      begin
        Exit(True);
      end;
  end;
end;

function TzCustomValueManager.HasList(const PItem: PPropItem): Boolean;
begin
  Result := False;

  case GetValueType(PItem) of
    vtObj:
      begin
        Result := IsPropTypeDerivedFromClass(PItem.Prop.PropertyType, TComponent);
        Exit;
      end;
    vtMethod, vtBool, vtColor, vtCursor, vtShortCut, vtSetElement, vtEnum:
      begin
        Exit(True);
      end;
  end;
end;

procedure TzCustomValueManager.PaintValue(Canvas: TCanvas; Index: Integer; const PItem: PPropItem; R: TRect);
var
  Value: TValue;
  ExtRect: TRect;
  ValueName: String;
  C: TColor;
  DC: HDC;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  VT: Integer;
  BoolVal: Boolean;
  LInspector: TzCustomObjInspector;
  LQName: String;
  LColor: TColor;
begin
  LStyle := StyleServices;
  LInspector := TzCustomObjInspector(PItem.Insp);
  Value := PItem.Value;
  VT := GetValueType(PItem);
  DC := Canvas.Handle;
  BoolVal := False;

  LInspector.CanvasStack.Push(Canvas);

  if (VT = vtBool) or (VT = vtSetElement) then
  begin
    case VT of
      vtBool: BoolVal := GetValueAs<Boolean>(Value);
      vtSetElement:
        BoolVal := SetEnumToBoolean(GetEnumOrdValue(Value), PItem.SetElementValue);
    end;
    if BoolVal then
      LDetails := LStyle.GetElementDetails(tbCheckBoxCheckedNormal)
    else
      LDetails := LStyle.GetElementDetails(tbCheckBoxUnCheckedNormal);
    ExtRect := LInspector.ExtraRect[Index];
    LStyle.DrawElement(DC, LDetails, ExtRect);
  end else if VT = vtColor then
  begin
    C := GetValueAs<TColor>(Value);
    ExtRect := LInspector.ExtraRect[Index]; { Delphi Berlin color style }
    if LInspector.SelectedIndex = Index then
      InflateRect(ExtRect, -1, -1);
    zFillRect(DC, ExtRect, ColorToRGB(C), clBlack, 1, 1);
  end;

  LInspector.CanvasStack.Pop;

  LQName := PItem^.QualifiedName;
  ValueName := PItem^.ValueName;

  LColor := LInspector.ValueColor;
  if LInspector.UseStyleColor then
    LColor := StyleServices.GetSystemColor(clBtnText);
  Canvas.Font.Color := LColor;

  Canvas.Font.Style := Canvas.Font.Style - [fsBold];
  if LInspector.IsValueNoDefault(LQName, ValueName) then
  begin
    if not LInspector.UseStyleColor then
      Canvas.Font.Color := LInspector.NonDefaultValueColor;
    if LInspector.BoldNonDefaultValue then
      Canvas.Font.Style := [fsBold];
  end;

  R := LInspector.ValueTextRect[Index];
  DrawText(Canvas.Handle, ValueName, -1, R, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
end;

procedure TzCustomValueManager.SetFloatPreference(
  const Value: TzFloatPreference);
begin
  FFloatPreference.Assign(Value);
end;

procedure TzCustomValueManager.SetValue(const PItem: PPropItem; var Value: TValue);
begin
  PItem^.Prop.SetValue(PItem.Instance, Value);
end;

function TzCustomValueManager.StrToValue<T>(const PItem: PPropItem; const s: string): T;
var
  vSInt: Int64;
  vUInt: UInt64;
  vSingle: Single;
  vDouble: Double;
  vExtended: Extended;
  Value: TValue;
  ValSign: Boolean;
  sR: T absolute vSInt;
  uR: T absolute vUInt;
  fsR: T absolute vSingle;
  fdR: T absolute vDouble;
  feR: T absolute vExtended;
  E: Integer;
begin
  vUInt := 0;
  vSInt := 0;
  vSingle := 0.0;
  vDouble := 0.0;
  vExtended := 0.0;
  Value := PItem.Value;
  ValSign := (Value.TypeData.MinValue < 0) or (Value.TypeData.MinInt64Value < 0);
  case GetValueType(PItem) of
    vtString:
      begin
        vUInt := UInt64(Pointer(s));
        Result := uR;
        Exit;
      end;
    vtChar:
      begin { IsOrdinal = True , but TryStrToInt64 will fail ! }
        if s <> '' then
          vUInt := Ord(s[1]);
        Result := uR;
        Exit;
      end;
    vtSingle:
      begin
        MyTryStrToFloat(s, vSingle);
        Result := fsR;
        Exit;
      end;
    vtDouble:
      begin
        MyTryStrToFloat(s, vDouble);
        Result := fdR;
        Exit;
      end;
    vtExtended:
      begin
        MyTryStrToFloat(s, vExtended);
        Result := feR;
        Exit;
      end;
  end;

  if PItem.Prop.PropertyType.IsOrdinal then
  begin
    TryStrToInt64(s, vSInt);
    Val(s, vUInt, E);
  end;

  if ValSign then
    Result := sR
  else
    Result := uR;
end;

function TzCustomValueManager.ValueHasOpenProbabilities(const PItem: PPropItem): Boolean;
begin
  Result := False;
  case GetValueType(PItem) of
    vtColor, vtString, vtUnknown: Exit(True);
  end;
end;

end.
