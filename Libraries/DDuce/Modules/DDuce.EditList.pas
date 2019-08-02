{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.EditList;

{ A user configurable list of key-value pairs. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  System.Actions, System.Rtti,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Menus,

  Spring,

  VirtualTrees,

  DDuce.DynamicRecord, DDuce.Components.ValueList;

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
    actExecute   : TAction;
    actMoveDown  : TAction;
    actMoveUp    : TAction;
    btn1         : TToolButton;
    btnAdd       : TToolButton;
    btnDelete    : TToolButton;
    btnDuplicate : TToolButton;
    btnExecute   : TToolButton;
    btnMoveDown  : TToolButton;
    btnMoveUp    : TToolButton;
    btnSpacer1   : TToolButton;
    btnSpacer2   : TToolButton;
    imlMain      : TImageList;
    mniAdd       : TMenuItem;
    mniDelete    : TMenuItem;
    mniDuplicate : TMenuItem;
    mniExecute   : TMenuItem;
    mniMoveDown  : TMenuItem;
    mniMoveUp    : TMenuItem;
    mniN1        : TMenuItem;
    mniN2        : TMenuItem;
    mniN3        : TMenuItem;
    pnlMain      : TPanel;
    ppmMain      : TPopupMenu;
    tlbMain      : TToolBar;
    actRefresh   : TAction;
    btnRefresh   : TToolButton;
    mniRefresh   : TMenuItem;
    actEdit: TAction;
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
    FValueList     : TValueList;
    FOnAdd         : Event<TEditListItemEvent>;
    FOnDuplicate   : Event<TEditListItemEvent>;
    FOnDelete      : Event<TEditListItemEvent>;
    FOnDeleteItem  : Event<TEditListItemEvent>;
    FOnExecute     : Event<TEditListItemEvent>;
    FOnExecuteItem : Event<TEditListItemEvent>;
    FUpdate        : Boolean;

    {$REGION 'property access methods'}
    function GetOnAdd: IEvent<TEditListItemEvent>;
    function GetOnDelete: IEvent<TEditListItemEvent>;
    function GetOnDeleteItem: IEvent<TEditListItemEvent>;
    function GetOnExecute: IEvent<TEditListItemEvent>;
    function GetOnExecuteItem: IEvent<TEditListItemEvent>;
    function GetOnDuplicate: IEvent<TEditListItemEvent>;
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

    procedure DoAdd(
      var AName  : string;
      var AValue : TValue
    );

    procedure DoDuplicate(
      var AName  : string;
      var AValue : TValue
    );

    procedure DoExecute(
      var AName  : string;
      var AValue : TValue
    );

    procedure DoExecuteItem(
      var AName  : string;
      var AValue : TValue
    );

    procedure DoDelete(
      var AName  : string;
      var AValue : TValue
    );

    procedure DoDeleteItem(
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

    property OnDelete: IEvent<TEditListItemEvent>
      read GetOnDelete;

    property OnDeleteItem: IEvent<TEditListItemEvent>
      read GetOnDeleteItem;

    property OnExecute: IEvent<TEditListItemEvent>
      read GetOnExecute;

    property OnExecuteItem: IEvent<TEditListItemEvent>
      read GetOnExecuteItem;
  end;

implementation

uses
  DDuce.Utils;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TEditList.AfterConstruction;
begin
  inherited AfterConstruction;
  FValueList             := TValueList.Create(Self);
  FValueList.Parent      := pnlMain;
  FValueList.Align       := alClient;
  FValueList.ShowGutter  := False;
  FValueList.MultiSelect := True;
  FValueList.Data        := DynamicRecord.CreateDynamicRecord;
  FValueList.Data.OnChanged.Add(FValueListDataChanged);
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
  LName  : string;
  LValue : TValue;
begin
  DoAdd(LName, LValue);
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
    if FValueList.SelectedCount = 1 then
      DoDelete(LName, LValue);
    DoDeleteItem(LName, LValue);
    Data.DeleteField(LName);
    FValueList.DeleteNode(LNode.VNode);
  end;
end;

procedure TEditList.actDuplicateExecute(Sender: TObject);
begin
//
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
    DoExecuteItem(LName, LValue);
  end;
end;

procedure TEditList.actMoveDownExecute(Sender: TObject);
var
  LNode : TValueListNode;
begin
  if Assigned(FValueList.FocusedField) then
  begin
    LNode := FValueList.GetFirstSelectedNodeData<TValueListNode>;
    LNode.Data.Index := LNode.Data.Index + 1;
    FValueList.MoveTo(LNode.VNode, LNode.VNode.NextSibling, amInsertAfter, False);
    FValueList.FocusedNode := LNode.VNode;
  end;
end;

procedure TEditList.actMoveUpExecute(Sender: TObject);
var
  LNode : TValueListNode;
begin
  if Assigned(FValueList.FocusedField) then
  begin
    LNode := FValueList.GetFirstSelectedNodeData<TValueListNode>;
    LNode.Data.Index := LNode.Data.Index - 1;
    FValueList.MoveTo(LNode.VNode, LNode.VNode.PrevSibling, amInsertBefore, False);
    FValueList.FocusedNode := LNode.VNode;
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
  Modified;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TEditList.DoAdd(var AName: string; var AValue: TValue);
var
  I : Integer;
  S : string;
begin
  if FOnAdd.CanInvoke then
  begin
    AName := 'New';
    AValue := '';
    I := 0;
    S := AName;
    while Data.ContainsField(S) do
    begin
      Inc(I);
      S := Format('%s_%d', [AName, I]);
    end;
    AName := S;
    FOnAdd.Invoke(Self, AName, AValue);
    FValueList.Data[AName] := AValue;
    Modified;
  end;
end;

procedure TEditList.DoDelete(var AName: string; var AValue: TValue);
begin
  if FOnDelete.CanInvoke then
    FOnDelete.Invoke(Self, AName, AValue);
end;

{ Called for every selected item if MutiSelect is enabled }

procedure TEditList.DoDeleteItem(var AName: string; var AValue: TValue);
begin
  if FOnDeleteItem.CanInvoke then
    FOnDeleteItem.Invoke(Self, AName, AValue);
end;

procedure TEditList.DoDuplicate(var AName: string; var AValue: TValue);
begin
  if FOnDuplicate.CanInvoke then
    FOnDuplicate.Invoke(Self, AName, AValue);
  FValueList.Repaint;
end;

procedure TEditList.DoExecute(var AName: string; var AValue: TValue);
begin
  if FOnExecute.CanInvoke then
    FOnExecute.Invoke(Self, AName, AValue);
end;

{ Called for every selected item if MutiSelect is enabled }

procedure TEditList.DoExecuteItem(var AName: string; var AValue: TValue);
begin
  if FOnExecuteItem.CanInvoke then
    FOnExecuteItem.Invoke(Self, AName, AValue);
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
  Result := FOnAdd;
end;

function TEditList.GetOnDelete: IEvent<TEditListItemEvent>;
begin
  Result := FOnDelete;
end;

function TEditList.GetOnDeleteItem: IEvent<TEditListItemEvent>;
begin
  Result := FOnDeleteItem;
end;

function TEditList.GetOnDuplicate: IEvent<TEditListItemEvent>;
begin
  Result := FOnDuplicate;
end;

function TEditList.GetOnExecute: IEvent<TEditListItemEvent>;
begin
  Result := FOnExecute;
end;

function TEditList.GetOnExecuteItem: IEvent<TEditListItemEvent>;
begin
  Result := FOnExecuteItem;
end;

function TEditList.GetValueList: TValueList;
begin
  Result := FValueList;
end;

procedure TEditList.Modified;
begin
  FUpdate := True;
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

procedure TEditList.UpdateActions;
begin
  inherited UpdateActions;
  actMoveUp.Enabled    := CanMoveUp;
  actMoveDown.Enabled  := CanMoveDown;
  actDuplicate.Enabled := not Data.IsEmpty  and FOnDuplicate.CanInvoke;
  actDelete.Enabled    := not Data.IsEmpty;
  actExecute.Enabled   := not Data.IsEmpty and FOnExecute.CanInvoke;
  if FUpdate then
  begin
    Refresh;
    FUpdate := False;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TEditList.Refresh;
begin
  FValueList.Refresh;
end;
{$ENDREGION}

end.
