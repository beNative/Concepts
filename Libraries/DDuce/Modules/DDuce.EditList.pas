{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{$I DDuce.inc}

unit DDuce.EditList;

{ A user configurable list of key-value pairs. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  System.Actions, System.Rtti,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.ImgList, Vcl.ToolWin,

  Spring,

  VirtualTrees, VirtualTrees.Types,

  DDuce.DynamicRecord, DDuce.Components.ValueList, DDuce.Logger,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection,
  SVGIconImageCollection;

{$REGION 'documentation'}
{  TODO :
     - support for drag and drop
     - Multiselect move
}
{$ENDREGION}

type
  TEditListItemEvent = procedure(
    ASender    : TObject;
    var AName  : string;
    var AValue : TValue
  ) of object;

type
  TEditList = class(TForm)
    {$REGION 'designer controls'}
    aclMain      : TActionList;
    actAdd       : TAction;
    actDelete    : TAction;
    actDuplicate : TAction;
    actEdit      : TAction;
    actExecute   : TAction;
    actMoveDown  : TAction;
    actMoveUp    : TAction;
    actRefresh   : TAction;
    btn1         : TToolButton;
    btnAdd       : TToolButton;
    btnDelete    : TToolButton;
    btnDuplicate : TToolButton;
    btnExecute   : TToolButton;
    btnMoveDown  : TToolButton;
    btnMoveUp    : TToolButton;
    btnRefresh   : TToolButton;
    btnSpacer1   : TToolButton;
    btnSpacer2   : TToolButton;
    mniAdd       : TMenuItem;
    mniDelete    : TMenuItem;
    mniDuplicate : TMenuItem;
    mniExecute   : TMenuItem;
    mniMoveDown  : TMenuItem;
    mniMoveUp    : TMenuItem;
    mniN1        : TMenuItem;
    mniN2        : TMenuItem;
    mniN3        : TMenuItem;
    mniRefresh   : TMenuItem;
    pnlMain      : TPanel;
    ppmMain      : TPopupMenu;
    tlbMain      : TToolBar;
    imcMain: TSVGIconImageCollection;
    imlMain: TVirtualImageList;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDuplicateExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    {$ENDREGION}

  private
    FValueList       : TValueList;
    FOnItemAdd       : Event<TEditListItemEvent>;
    FOnItemDuplicate : Event<TEditListItemEvent>;
    FOnItemDelete    : Event<TEditListItemEvent>;
    FOnItemExecute   : Event<TEditListItemEvent>;
    FOnItemMoveUp    : Event<TEditListItemEvent>;
    FOnItemMoveDown  : Event<TEditListItemEvent>;
    FOnDuplicate     : Event<TEditListItemEvent>;
    FOnDelete        : Event<TEditListItemEvent>;
    FOnExecute       : Event<TEditListItemEvent>;
    FUpdate          : Boolean;

    {$REGION 'property access methods'}
    function GetOnAdd: IEvent<TEditListItemEvent>;
    function GetOnDelete: IEvent<TEditListItemEvent>;
    function GetOnItemDelete: IEvent<TEditListItemEvent>;
    function GetOnExecute: IEvent<TEditListItemEvent>;
    function GetOnItemExecute: IEvent<TEditListItemEvent>;
    function GetOnDuplicate: IEvent<TEditListItemEvent>;
    function GetOnItemMoveUp: IEvent<TEditListItemEvent>;
    function GetOnItemDuplicate: IEvent<TEditListItemEvent>;
    function GetOnItemMoveDown: IEvent<TEditListItemEvent>;
    function GetValueList: TValueList;
    function GetData: IDynamicRecord;
    function GetActionAdd: TAction;
    function GetActionDelete: TAction;
    function GetActionDuplicate: TAction;
    function GetActionExecute: TAction;
    function GetActionMoveDown: TAction;
    function GetActionMoveUp: TAction;
    function GetActionRefresh: TAction;
    function GetMultiSelect: Boolean;
    procedure SetMultiSelect(const Value: Boolean);
    {$ENDREGION}

    procedure FValueListDataChanged(ASender: TObject);

  protected
    function CanMoveUp: Boolean; virtual;
    function CanMoveDown: Boolean; virtual;
    procedure UpdateActions; override;
    procedure Modified;

    procedure DoItemAdd(
      var AName  : string;
      var AValue : TValue
    );
    procedure DoDuplicate(
      var AName  : string;
      var AValue : TValue
    );
    procedure DoItemDuplicate(
      var AName  : string;
      var AValue : TValue
    );
    procedure DoExecute(
      var AName  : string;
      var AValue : TValue
    );
    procedure DoItemExecute(
      var AName  : string;
      var AValue : TValue
    );
    procedure DoDelete(
      var AName  : string;
      var AValue : TValue
    );
    procedure DoItemDelete(
      var AName  : string;
      var AValue : TValue
    );
    procedure DoItemMoveUp(
      var AName  : string;
      var AValue : TValue
    );
    procedure DoItemMoveDown(
      var AName  : string;
      var AValue : TValue
    );

  public
    procedure AfterConstruction; override;
    constructor Create(
      AOwner  : TComponent;
      AParent : TWinControl
    ); reintroduce; virtual;

    procedure Refresh; virtual;

    property ValueList: TValueList
      read GetValueList;

    property Data: IDynamicRecord
      read GetData;

    property MultiSelect: Boolean
      read GetMultiSelect write SetMultiSelect;

    property ActionMoveUp: TAction
      read GetActionMoveUp;

    property ActionMoveDown: TAction
      read GetActionMoveDown;

    property ActionExecute: TAction
      read GetActionExecute;

    property ActionAdd: TAction
      read GetActionAdd;

    property ActionDelete: TAction
      read GetActionDelete;

    property ActionDuplicate: TAction
      read GetActionDuplicate;

    property ActionRefresh: TAction
      read GetActionRefresh;

    property OnAdd: IEvent<TEditListItemEvent>
      read GetOnAdd;

    property OnDuplicate: IEvent<TEditListItemEvent>
      read GetOnDuplicate;

    property OnItemDuplicate: IEvent<TEditListItemEvent>
      read GetOnItemDuplicate;

    property OnDelete: IEvent<TEditListItemEvent>
      read GetOnDelete;

    property OnItemDelete: IEvent<TEditListItemEvent>
      read GetOnItemDelete;

    property OnExecute: IEvent<TEditListItemEvent>
      read GetOnExecute;

    property OnItemExecute: IEvent<TEditListItemEvent>
      read GetOnItemExecute;

    property OnItemMoveUp: IEvent<TEditListItemEvent>
      read GetOnItemMoveUp;

    property OnItemMoveDown: IEvent<TEditListItemEvent>
      read GetOnItemMoveDown;
  end;

implementation

uses
  VirtualTrees.BaseTree,

  DDuce.Utils;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TEditList.AfterConstruction;
begin
  inherited AfterConstruction;
  FValueList             := TValueList.Create(Self);
  FValueList.Parent      := pnlMain;
  FValueList.Align       := alClient;
  FValueList.Data        := DynamicRecord.CreateDynamicRecord;
  FValueList.Data.OnChanged.Add(FValueListDataChanged);
  FValueList.BorderStyle := bsNone;
  FValueList.ShowGutter  := False;
  actAdd.Enabled         := True;
  actRefresh.Enabled     := True;
end;

constructor TEditList.Create(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);
  if Assigned(AParent) then
    AssignFormParent(Self, AParent);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TEditList.actAddExecute(Sender: TObject);
var
  I      : Integer;
  S      : string;
  LName  : string;
  LValue : TValue;
begin
  LName  := 'New';
  LValue := '';
  I := 0;
  S := LName;
  while Data.ContainsField(S) do
  begin
    Inc(I);
    S := Format('%s_%d', [LName, I]);
  end;
  LName := S;
  DoItemAdd(LName, LValue);
  FValueList.Data[LName] := LValue;
  if  FValueList.Data.ContainsField(LName) then
  begin
    Guard.CheckNotNull(FValueList.Data.Fields[LName], LName);
    FValueList.SelectNode(Data.Count - 1);
    Abort;
    FValueList.FocusedField := FValueList.Data.Fields[LName];
  end;
  Modified;
end;

procedure TEditList.actDeleteExecute(Sender: TObject);
var
  LName  : string;
  LNode  : TValueListNode;
  LValue : TValue;
begin
  for LNode in FValueList.GetSelectedData<TValueListNode> do
  begin
    LValue := LNode.Data.Value;
    LName  := LNode.Data.Name;
    if FValueList.SelectedCount >= 1 then
      DoDelete(LName, LValue);
    DoItemDelete(LName, LValue);
    Data.DeleteField(LName);
    FValueList.DeleteNode(LNode.VNode);
  end;
end;

procedure TEditList.actDuplicateExecute(Sender: TObject);
var
  I      : Integer;
  S      : string;
  LName  : string;
  LNode  : TValueListNode;
  LValue : TValue;
begin
  for LNode in FValueList.GetSelectedData<TValueListNode> do
  begin
    LValue := LNode.Data.Value;
    LName  := LNode.Data.Name;
    I      := 0;
    S      := LName;
    while Data.ContainsField(S) do
    begin
      Inc(I);
      S := Format('%s_%d', [LName, I]);
    end;
    LName := S;
    DoItemDuplicate(LName, LValue);
    FValueList.Data[LName] := LValue;
  end;
  Modified;
end;

procedure TEditList.actEditExecute(Sender: TObject);
begin
  FValueList.EditNode(FValueList.FocusedNode, FValueList.FocusedColumn);
end;

procedure TEditList.actExecuteExecute(Sender: TObject);
var
  LName  : string;
  LNode  : TValueListNode;
  LValue : TValue;
begin
  for LNode in FValueList.GetSelectedData<TValueListNode> do
  begin
    LValue := LNode.Data.Value;
    LName  := LNode.Data.Name;
    DoItemExecute(LName, LValue);
  end;
end;

procedure TEditList.actMoveDownExecute(Sender: TObject);
var
  LName  : string;
  LNode  : TValueListNode;
  LValue : TValue;
begin
  for LNode in FValueList.GetSelectedData<TValueListNode> do
  begin
    LValue := LNode.Data.Value;
    LName  := LNode.Data.Name;
    LNode.Data.Index := LNode.Data.Index + 1;
    FValueList.MoveTo(LNode.VNode, LNode.VNode.NextSibling, amInsertAfter, False);
    FValueList.FocusedNode := LNode.VNode;
    LValue := LNode.Data.Value;
    LName  := LNode.Data.Name;
    DoItemMoveDown(LName, LValue);
  end;
end;

procedure TEditList.actMoveUpExecute(Sender: TObject);
var
  LName  : string;
  LNode  : TValueListNode;
  LValue : TValue;
begin
  if Assigned(FValueList.FocusedField) then
  begin
    LNode := FValueList.GetFirstSelectedNodeData<TValueListNode>;
    LNode.Data.Index := LNode.Data.Index - 1;
    FValueList.MoveTo(LNode.VNode, LNode.VNode.PrevSibling, amInsertBefore, False);
    FValueList.FocusedNode := LNode.VNode;
    LValue := LNode.Data.Value;
    LName  := LNode.Data.Name;
    DoItemMoveUp(LName, LValue);
  end;
end;

procedure TEditList.actRefreshExecute(Sender: TObject);
begin
  Refresh;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TEditList.FValueListDataChanged(ASender: TObject);
begin
  Logger.Track(Self, 'FValueListDataChanged');
  Modified;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TEditList.DoItemAdd(var AName: string; var AValue: TValue);
begin
  Logger.Track(Self, 'DoItemAdd');
  if FOnItemAdd.CanInvoke then
    FOnItemAdd.Invoke(Self, AName, AValue);
  Modified;
end;

procedure TEditList.DoDelete(var AName: string; var AValue: TValue);
begin
  if FOnDelete.CanInvoke then
    FOnDelete.Invoke(Self, AName, AValue);
end;

{ Called for every selected item if MutiSelect is enabled }

procedure TEditList.DoItemDelete(var AName: string; var AValue: TValue);
begin
  if FOnItemDelete.CanInvoke then
    FOnItemDelete.Invoke(Self, AName, AValue);
end;

procedure TEditList.DoItemDuplicate(var AName: string; var AValue: TValue);
begin
  if FOnItemDuplicate.CanInvoke then
    FOnItemDuplicate.Invoke(Self, AName, AValue);
  Modified;
end;

procedure TEditList.DoDuplicate(var AName: string; var AValue: TValue);
begin
  if FOnDuplicate.CanInvoke then
    FOnDuplicate.Invoke(Self, AName, AValue);
  Modified;
end;

procedure TEditList.DoExecute(var AName: string; var AValue: TValue);
begin
  if FOnExecute.CanInvoke then
    FOnExecute.Invoke(Self, AName, AValue);
end;

{ Called for every selected item if MutiSelect is enabled }

procedure TEditList.DoItemExecute(var AName: string; var AValue: TValue);
begin
  if FOnItemExecute.CanInvoke then
    FOnItemExecute.Invoke(Self, AName, AValue);
  Modified;
end;

procedure TEditList.DoItemMoveDown(var AName: string; var AValue: TValue);
begin
  if FOnItemMoveDown.CanInvoke then
    FOnItemMoveDown.Invoke(Self, AName, AValue);
  Modified;
end;

procedure TEditList.DoItemMoveUp(var AName: string; var AValue: TValue);
begin
  if FOnItemMoveUp.CanInvoke then
    FOnItemMoveUp.Invoke(Self, AName, AValue);
  Modified;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TEditList.GetActionAdd: TAction;
begin
  Result := actAdd;
end;

function TEditList.GetActionDelete: TAction;
begin
  Result := actDelete;
end;

function TEditList.GetActionDuplicate: TAction;
begin
  Result := actDuplicate;
end;

function TEditList.GetActionExecute: TAction;
begin
  Result := actExecute;
end;

function TEditList.GetActionMoveDown: TAction;
begin
  Result := actMoveDown;
end;

function TEditList.GetActionMoveUp: TAction;
begin
  Result := actMoveUp;
end;

function TEditList.GetActionRefresh: TAction;
begin
  Result := actRefresh;
end;

function TEditList.GetData: IDynamicRecord;
begin
  Result := FValueList.Data;
end;

function TEditList.GetMultiSelect: Boolean;
begin
  Result := FValueList.MultiSelect;
end;

procedure TEditList.SetMultiSelect(const Value: Boolean);
begin
  FValueList.MultiSelect := Value;
end;

function TEditList.GetOnAdd: IEvent<TEditListItemEvent>;
begin
  Result := FOnItemAdd;
end;

function TEditList.GetOnDelete: IEvent<TEditListItemEvent>;
begin
  Result := FOnDelete;
end;

function TEditList.GetOnItemDelete: IEvent<TEditListItemEvent>;
begin
  Result := FOnItemDelete;
end;

function TEditList.GetOnItemDuplicate: IEvent<TEditListItemEvent>;
begin
  Result := FOnItemDuplicate;
end;

function TEditList.GetOnDuplicate: IEvent<TEditListItemEvent>;
begin
  Result := FOnDuplicate;
end;

function TEditList.GetOnExecute: IEvent<TEditListItemEvent>;
begin
  Result := FOnExecute;
end;

function TEditList.GetOnItemExecute: IEvent<TEditListItemEvent>;
begin
  Result := FOnItemExecute;
end;

function TEditList.GetOnItemMoveDown: IEvent<TEditListItemEvent>;
begin
  Result := FOnItemMoveDown;
end;

function TEditList.GetOnItemMoveUp: IEvent<TEditListItemEvent>;
begin
  Result := FOnItemMoveUp;
end;

function TEditList.GetValueList: TValueList;
begin
  Result := FValueList;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TEditList.CanMoveDown: Boolean;
begin
  if Assigned(FValueList.FocusedNode) and (FValueList.SelectedCount = 1) then
    Result := Assigned(FValueList.FocusedNode.NextSibling)
  else
    Result := False;
end;

function TEditList.CanMoveUp: Boolean;
begin
  if Assigned(FValueList.FocusedNode) and (FValueList.SelectedCount = 1) then
    Result := Assigned(FValueList.FocusedNode.PrevSibling)
  else
    Result := False;
end;

procedure TEditList.Modified;
begin
  FUpdate := True;
end;

procedure TEditList.UpdateActions;
begin
  inherited UpdateActions;
  actMoveUp.Enabled    := CanMoveUp;
  actMoveDown.Enabled  := CanMoveDown;
  actDuplicate.Enabled := not Data.IsEmpty;
  actDelete.Enabled    := not Data.IsEmpty;
  actExecute.Enabled   := not Data.IsEmpty and FOnExecute.CanInvoke;
  actEdit.Enabled      := ValueList.SelectedCount = 1;
  if FUpdate then
  begin
    Refresh;
    FUpdate := False;
  end;
  // temp workaround for toolbar painting black in LogViewer.
  tlbMain.Invalidate;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TEditList.Refresh;
begin
  if Assigned(FValueList) then
    FValueList.Refresh;
end;
{$ENDREGION}

end.
