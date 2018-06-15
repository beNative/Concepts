unit zObjInspTypes;

interface

uses
  System.Rtti, System.TypInfo,
  Vcl.Controls, Vcl.Forms,

  zRecList;

type
  TPropList = class;
  TzRttiType = class;
  PPropItem = ^TPropItem;

  TPropItem = record
    Parent: PPropItem;
    Prop: TRttiProperty;
    Component: TObject;
    SetElementValue: Integer;
    Insp: TControl;
    Instance: TObject;
    FIsCategory: Boolean;
    CategoryIndex: Integer;
  private
    FVisible: Boolean;
    FQName: String;
    FItems: TPropList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): PPropItem;
    function GetHasChild: Boolean;
    function GetExpanded: Boolean;
    function GetValue: TValue;
    function GetName: String;
    function GetValueName: String;
    function GetVisible: Boolean;
    procedure CheckItemsList;
    function GetComponentRoot: TCustomForm;
  public
    procedure SetItems(AItems: TPropList);
    function EqualTo(A: PPropItem): Boolean;
    function IsEmpty: Boolean;
    class function Empty: TPropItem; static;
    function IsEnum: Boolean;
    function IsCategory: Boolean;
    function IsClass: Boolean;
    function IsSet: Boolean;
    function IsSetElement: Boolean;
    function MayHaveChild: Boolean;
    property ComponentRoot: TCustomForm read GetComponentRoot;
    property Count: Integer read GetCount;
    property Items[const index: Integer]: PPropItem read GetItem;
    property HasChild: Boolean read GetHasChild;
    property Expanded: Boolean read GetExpanded;
    property QualifiedName: String read FQName write FQName;
    property Value: TValue read GetValue;
    property Name: String read GetName;
    property ValueName: String read GetValueName;
    property Visible: Boolean read GetVisible write FVisible;
  end;

  TPropList = class(TzRecordList<TPropItem, PPropItem>)
    procedure FreeRecord(P: Pointer); override;
    function IndexOfQName(QName: String): Integer;
    procedure Sort;
  end;

  TzRttiType = class(TRttiType)
    function GetUsedProperties: TArray<TRttiProperty>;
  end;

function GetFormRoot(Comp: TObject): TCustomForm;

function IsPropVisible(const Prop: TRttiProperty; const PropOwner: TObject; ObjectVisibility: TMemberVisibility): Boolean;

implementation

uses
  Winapi.Windows,
  System.SysUtils, System.Classes,

  zObjInspector;

function GetFormRoot(Comp: TObject): TCustomForm;
begin
  Result := nil;
  if Comp is TCustomForm then
    Result := TCustomForm(Comp)
  else if Comp is TControl then
    Result := GetParentForm(TControl(Comp));
end;

function IsPropVisible(const Prop: TRttiProperty; const PropOwner: TObject; ObjectVisibility: TMemberVisibility): Boolean;
begin
  if PropOwner is TPersistent then
    Result := Prop.Visibility = mvPublished
  else // if PropOwner is TObject then
    Result := Prop.Visibility >= ObjectVisibility;
end;

function ObjHasAtLeastOneChild(Obj: TObject; ObjVisibility: TMemberVisibility): Boolean;
var
  LCtx: TRttiContext;
  LType: TRttiType;
  LPropList: TArray<TRttiProperty>;
  LProp: TRttiProperty;
begin
  Result := False;
  if not Assigned(Obj) then
    Exit;
  LCtx := TRttiContext.Create;
  LType := LCtx.GetType(Obj.ClassInfo);
  LPropList := TzRttiType(LType).GetUsedProperties;
  for LProp in LPropList do
    if IsPropVisible(LProp, Obj, ObjVisibility) then
      Exit(True);
end;

function TzRttiType.GetUsedProperties: TArray<TRttiProperty>;
var
  LBasePropList { ,LDecPropList } : TArray<TRttiProperty>;
  Prop: TRttiProperty;
  L: Integer;
  function Contains(PropArray: TArray<TRttiProperty>; AProp: TRttiProperty): Boolean;
  var
    LProp: TRttiProperty;
  begin
    Result := False;
    for LProp in PropArray do
      if LProp.Name = AProp.Name then
        Exit(True);
  end;

begin
  Result := GetDeclaredProperties;
  LBasePropList := GetProperties;
  // LDecPropList := GetDeclaredProperties;

  for Prop in LBasePropList do
  begin
    if Assigned(Prop) and (not Contains( { LDecPropList } Result, Prop)) then
    begin
      L := Length(Result) + 1;
      SetLength(Result, L);
      Result[L - 1] := Prop;
    end;
  end;

end;

procedure TPropList.FreeRecord(P: Pointer);
begin
  inherited;
end;

function TPropList.IndexOfQName(QName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if SameText(Items[i].FQName, QName) then
        Exit(i);
end;

procedure TPropList.Sort;
  function Compare(Item1, Item2: Pointer): Integer;
  begin
    Result := CompareText(PPropItem(Item1)^.FQName, PPropItem(Item2)^.FQName);
  end;

begin
  SortOrgList(@Compare);
end;

procedure TPropItem.CheckItemsList;
begin
  if not Assigned(FItems) then
    raise Exception.Create('Items list cannot be nil.');
end;

class function TPropItem.Empty: TPropItem;
begin
  ZeroMemory(@Result, SizeOf(TPropItem));
end;

function TPropItem.EqualTo(A: PPropItem): Boolean;
begin
  Result := @Self = A;
end;

function TPropItem.GetComponentRoot: TCustomForm;
begin
  Result := GetFormRoot(Component);
end;

function TPropItem.GetCount: Integer;
var
  L: Integer;
  i: Integer;
  P: PPropItem;
begin
  CheckItemsList;
  Result := 0;
  if not MayHaveChild then
    Exit;
  L := FItems.IndexOf(@Self);
  if L > -1 then
  begin
    for i := L + 1 to FItems.Count - 1 do
    begin
      P := FItems.Items[i];
      if IsCategory then
      begin
        if CategoryIndex = P^.CategoryIndex then
          Inc(Result)
        else if P^.CategoryIndex > -1 then
          Break;
      end else begin
        if P^.Parent = @Self then
          Inc(Result);
        if Parent = P^.Parent then
          Break;
      end;
    end;
  end;
end;

function TPropItem.GetExpanded: Boolean;
begin
  CheckItemsList;
  Result := Count > 0;
  if Result then
    Result := Assigned(Items[0]);
  if Result then
    Result := Items[0].FVisible;
end;

function TPropItem.GetHasChild: Boolean;
begin
  if IsCategory then
    Exit(True);
  if not MayHaveChild then
    Exit(False);
  Result := not Value.IsEmpty;
  if Result and IsClass then
    Result := ObjHasAtLeastOneChild(Value.AsObject, TzObjInspectorBase(Insp).ObjectVisibility);
end;

function TPropItem.GetItem(const Index: Integer): PPropItem;
var
  L, J, CC, i: Integer;
  LList: TList;
begin
  CheckItemsList;
  Result := nil;
  if not(IsCategory or IsClass or IsSet) then
    Exit;
  LList := TList.Create;
  try
    L := FItems.IndexOf(@Self);
    J := 0;
    if L > -1 then
    begin
      CC := Count; // Childs count.
      for i := L + 1 to FItems.Count - 1 do
      begin
        if IsCategory then
        begin
          if FItems.Items[i].CategoryIndex = CategoryIndex then
          begin
            Inc(J);
            LList.Add(FItems.Items[i]);
          end;
        end else if FItems.Items[i].Parent = @Self then
        begin
          Inc(J);
          LList.Add(FItems.Items[i]);
        end;
        if J = CC then
          Break;
      end;
      Result := LList.Items[Index];
    end;
  finally
    LList.Free;
  end;
end;

function TPropItem.GetName: String;
begin
  if IsCategory then
  begin
    Result := TzObjInspectorBase(Insp).Category[CategoryIndex];
    Exit;
  end;
  if IsSetElement then
    Result := GetEnumName(Prop.PropertyType.AsSet.ElementType.Handle, SetElementValue)
  else
    Result := Prop.Name;
end;

function TPropItem.GetValue: TValue;
var
  LInstance: TObject;
begin
  Result := TValue.Empty;
  if IsCategory then
    Exit();
  LInstance := Instance;
  if Assigned(LInstance) then
  begin
    try
      Result := Prop.GetValue(LInstance);
    except
      Result := TValue.Empty;
    end;
  end;
end;

function TPropItem.GetValueName: String;
begin
  Result :=  TzObjInspectorBase(Insp).ValueManager.GetValueName(@Self);
end;

function TPropItem.GetVisible: Boolean;
begin
  Result := FVisible;
  if Result then
    Result := Assigned(Instance);
end;

function TPropItem.IsCategory: Boolean;
begin
  Result := FIsCategory;
end;

function TPropItem.IsClass: Boolean;
begin
  if (not Assigned(Prop)) or (IsCategory) then
    Exit(False);
  Result := Prop.PropertyType.TypeKind = tkClass;
end;

function TPropItem.IsEmpty: Boolean;
var
  PropItem: TPropItem;
begin
  PropItem := TPropItem.Empty;
  Result := CompareMem(@Self, @PropItem, SizeOf(TPropItem));
end;

function TPropItem.IsEnum: Boolean;
begin
  if (not Assigned(Prop)) or (IsCategory) then
    Exit(False);
  Result := Prop.PropertyType.TypeKind = tkEnumeration;
end;

function TPropItem.IsSet: Boolean;
begin
  if (not Assigned(Prop)) or (IsCategory) then
    Exit(False);
  Result := (Prop.PropertyType.TypeKind = tkSet) and (SetElementValue = -1);
end;

function TPropItem.IsSetElement: Boolean;
begin
  if (not Assigned(Prop)) or (IsCategory) then
    Exit(False);
  Result := (Prop.PropertyType.TypeKind = tkSet) and (SetElementValue > -1);
end;

function TPropItem.MayHaveChild: Boolean;
begin
  Result := IsCategory or IsClass or IsSet;
end;

procedure TPropItem.SetItems(AItems: TPropList);
begin
  FItems := AItems;
end;

end.
