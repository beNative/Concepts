{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.RTTEye;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Actions, System.Classes, System.Rtti, System.ImageList,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Controls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Forms, Vcl.ImgList;

type
  TfrmRTTEye = class(TForm)
    {$REGION 'designer controls'}
    aclMain     : TActionList;
    actCollapse : TAction;
    actExpand   : TAction;
    actRefresh  : TAction;
    actSearch   : TAction;
    btnCollapse : TButton;
    btnExpand   : TButton;
    btnLoad     : TButton;
    btnSearch   : TButton;
    EditSearch  : TEdit;
    imlMain     : TImageList;
    lvRtti      : TListView;
    pnlBottom   : TPanel;
    pnlMain     : TPanel;
    splVertical : TSplitter;
    tvRtti      : TTreeView;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure tvRttiCustomDrawItem(
      Sender          : TCustomTreeView;
      Node            : TTreeNode;
      State           : TCustomDrawState;
      var DefaultDraw : Boolean
    );
    procedure tvRttiChange(Sender: TObject; Node: TTreeNode);
    procedure tvRttiDblClick(Sender: TObject);
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actRefreshExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actExpandExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    {$ENDREGION}

  private
    FRttiContext: TRttiContext;

    procedure LoadTree;

    function FindTreeViewText(
      const AText : string;
      ATreeView   : TTreeView
    ): TTreeNode;
    procedure CollapseTree;
    procedure ExpandTree;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.SysUtils, System.TypInfo,
  Vcl.Dialogs, Vcl.Graphics;

const
  LEVEL_PACKAGE = 0;
  LEVEL_UNIT    = 1;
  LEVEL_TYPE    = 2;
  LEVEL_FIELD   = 3;

resourcestring
  SNotFound = '%s Not found';

{$REGION 'construction and destruction'}
procedure TfrmRTTEye.AfterConstruction;
begin
  inherited AfterConstruction;
  //HourGlass(LoadTree);
  LoadTree;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmRTTEye.actCollapseExecute(Sender: TObject);
begin
  //HourGlass(CollapseTree);
  CollapseTree;
end;

procedure TfrmRTTEye.actExpandExecute(Sender: TObject);
begin
  //HourGlass(ExpandTree);
  ExpandTree;
end;

procedure TfrmRTTEye.actRefreshExecute(Sender: TObject);
begin
  //HourGlass(LoadTree);
  LoadTree;
end;

procedure TfrmRTTEye.actSearchExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  if Trim(EditSearch.Text) <> '' then
  begin
    Node := FindTreeViewText(Trim(EditSearch.Text), tvRTTI);
    if Node <> nil then
    begin
      Node.MakeVisible;
      tvRTTI.Selected := Node
    end
    else
      ShowMessage(Format(SNotFound, [Trim(EditSearch.Text)]));
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}

{ Builds listview items for selected treenode. }

procedure TfrmRTTEye.tvRttiChange(Sender: TObject; Node: TTreeNode);
var
  lTyp  : TRttiType;
  T     : TRttiType;
  M     : TRttiMethod;
  P     : TRttiProperty;
  lProp : TRttiProperty;
  F     : TRttiField;
  O     : TRttiObject;
  Item  : TListItem;
begin
  if Assigned(Node.Data) then
  begin
    lvRtti.Items.BeginUpdate;
    try
      lvRtti.Items.Clear;
      O := TRttiObject(Node.Data);
      case Node.Level of
        LEVEL_TYPE:
        begin
          lTyp := Node.Data;
          T := FRttiContext.GetType(TRttiType);
          for P in T.GetProperties do
          begin
          // Ugly hack to prevent calling properties in the RTTI type which
          // start with 'As' like AsInstance, AsOrdinal, AsRecord, AsSet
            if Copy(P.Name, 1, 2) = 'As' then
              Continue;

            Item := lvRtti.Items.Add;
            Item.Caption := P.Name;
            Item.SubItems.Add(P.GetValue(lTyp).ToString);
          end;
        end;

        LEVEL_FIELD:
        begin
          if O.ClassNameIs('TRttiInstanceMethodEx')
            or O.ClassNameIs('TRttiIntfMethod')
            or O.ClassNameIs('TRttiRecordMethod') then
          begin
            M := Node.Data;
            T := FRttiContext.GetType(TRttiMethod);
            for P in T.GetProperties do
            begin
              Item := lvRtti.Items.Add;
              Item.Caption := P.Name;
              Item.SubItems.Add(P.GetValue(M).ToString);
            end;
          end
          else if O.ClassNameIs('TRttiInstancePropertyEx') then
          begin
            lProp := Node.Data;
            T := FRttiContext.GetType(TRttiProperty);
            for P in T.GetProperties do
            begin
              Item := lvRtti.Items.Add;
              Item.Caption := P.Name;
              Item.SubItems.Add(P.GetValue(lProp).ToString);
            end;
          end
          else if O.ClassNameIs('TRttiIndexedProperty') then
          begin
            F := Node.Data;
            T := FRttiContext.GetType(TRttiIndexedProperty);
            for P in T.GetProperties do
            begin
              Item := lvRtti.Items.Add;
              Item.Caption := P.Name;
              Item.SubItems.Add(P.GetValue(F).ToString);
            end;
          end
          else if O.ClassNameIs('TRttiInstanceFieldEx')
            or O.ClassNameIs('TRttiRecordField') then
          begin
            F := Node.Data;
            T := FRttiContext.GetType(TRttiField);
            for P in T.GetProperties do
            begin
              Item := lvRtti.Items.Add;
              Item.Caption := P.Name;
              Item.SubItems.Add(P.GetValue(F).ToString);
            end;
          end
          else
          begin
            // TS: debug
            ShowMessage(O.ClassName);
          end;
        end;
      end;
    finally
      lvRtti.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmRTTEye.tvRttiCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  LFontColor : TColor;
  LBackColor : TColor;
  T         : TRttiType;
begin
  LBackColor := clWindow;
  LFontColor := clWindowText;
  T := Node.Data;
  if Assigned(T) then
  begin
    case Node.Level of
      LEVEL_PACKAGE:
      begin
        LFontColor := clBlack;
        Sender.Canvas.Font.Style := [fsBold];
      end;

      LEVEL_UNIT:
      begin
        LFontColor := clRed;
      end;

      LEVEL_TYPE:
      begin
        case T.TypeKind of
          tkClass:
            begin
              LFontColor := clGreen;
              Sender.Canvas.Font.Style := [fsBold];
            end;
        else
          LFontColor := clBlue;
        end;
      end;

      LEVEL_FIELD:
      begin
        if TRttiObject(T).ClassNameIs('TRttiInstanceMethodEx') then
        begin
          LFontColor := clGray;
        end
        else if TRttiObject(T).ClassNameIs('TRttiInstancePropertyEx')
          then
        begin
          LFontColor := clNavy;
        end;
      end;
    end; // case

    if Node.Selected then
    begin
      LBackColor := clHighlight;
      LFontColor := clWindow;
    end;
  end;
  Sender.Canvas.Brush.Color := LBackColor;
  Sender.Canvas.Font.Color  := LFontColor;
  DefaultDraw := True;
end;

{ Jump to referenced type. }

procedure TfrmRTTEye.tvRttiDblClick(Sender: TObject);

  function FindRttiType(T: TRttiType): TTreeNode;
  var
    I : Integer;
  begin
    Result := nil;
    for I := 0 to tvRTTI.Items.Count - 1 do
      if tvRTTI.Items[I].Level = LEVEL_TYPE then
      begin
        Result := tvRTTI.Items[I];
        if Assigned(Result.Data) then
          if T.QualifiedName = TRttiType(Result.Data).QualifiedName then
            Break;
      end;
  end;

var
  LNode : TTreeNode;
  T     : TRttiType;
  M     : TRttiMethod;
  P     : TRttiProperty;
  F     : TRttiField;
  O     : TRttiObject;
begin
  LNode := tvRTTI.Selected;
  O := TRttiObject(LNode.Data);
  case LNode.Level of
    LEVEL_FIELD:
      begin
        if O.ClassNameIs('TRttiInstanceFieldEx') then
        begin
          F := LNode.Data;
          T := FRttiContext.FindType(F.FieldType.QualifiedName);
          if Assigned(T) then
          begin
            LNode := FindRttiType(T);
            if LNode <> nil then
            begin
              LNode.MakeVisible;
              tvRTTI.Selected := LNode;
            end;
          end;
        end
        else if O.ClassNameIs('TRttiInstancePropertyEx') then
        begin
          P := LNode.Data;
          T := FRttiContext.FindType(P.PropertyType.QualifiedName);
          if Assigned(T) then
          begin
            LNode := FindRttiType(T);
            if LNode <> nil then
            begin
              LNode.MakeVisible;
              tvRTTI.Selected := LNode;
            end;
          end;
        end
        else if O.ClassNameIs('TRttiInstanceMethodEx') then
        begin
          M := LNode.Data;
          if M.HasExtendedInfo
            and (M.MethodKind in [mkFunction, mkClassFunction]) then
          begin
            T := FRttiContext.FindType(M.ReturnType.QualifiedName);
            if Assigned(T) then
            begin
              LNode := FindRttiType(T);
              if LNode <> nil then
              begin
                LNode.MakeVisible;
                tvRTTI.Selected := LNode;
              end;
            end;
          end;
        end;
      end;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
function TfrmRTTEye.FindTreeViewText(const AText: string; ATreeView: TTreeView):
  TTreeNode;
var
  Node: TTreeNode;
begin
  Result := nil;
  if ATreeView.Items.Count > 0 then
  begin
    ATreeView.Items.BeginUpdate;
    try
      Node := ATreeView.Selected;
      if Node = nil then
        Node := ATreeView.Items[0]
      else
        Node := Node.GetNext;

      while Assigned(Node) do
      begin
        if Node.Text.Contains(AText) then
        begin
          Result := Node;
          Exit;
        end
        else
          Node := Node.GetNext;
      end;
    finally
      ATreeView.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmRTTEye.CollapseTree;
begin
  tvRTTI.Items.BeginUpdate;
  try
    tvRTTI.FullCollapse;
  finally
    tvRTTI.Items.EndUpdate;
  end;
end;

procedure TfrmRTTEye.ExpandTree;
begin
  tvRTTI.Items.BeginUpdate;
  try
    tvRTTI.FullExpand;
  finally
    tvRTTI.Items.EndUpdate;
  end;
end;

procedure TfrmRTTEye.LoadTree;

  function GetUnitName(T: TRttiType): string;
  begin
    Result := T.QualifiedName;
    StringReplace(T.QualifiedName, '.' + T.Name, '', [rfReplaceAll]);
  end;

var
  T            : TRttiType;
  M            : TRttiMethod;
  P            : TRttiProperty;
  IP           : TRttiIndexedProperty;
  F            : TRttiField;
  PNode        : TTreeNode;
  LNode        : TTreeNode;
  LTypeList    : TArray<TRttiType>;
  LUnits       : TStrings;
  LUnitName    : string;
  LPackage     : TRttiPackage;
  LPackageNode : TTreeNode;
begin
  tvRTTI.Items.BeginUpdate;
  tvRTTI.Items.Clear;
  LUnits := TStringList.Create;
  try
    for LPackage in FRttiContext.GetPackages do
    begin
      LUnits.Clear;
      LPackageNode := tvRTTI.Items.Add(nil, LPackage.Name);
      try
        LTypeList := LPackage.GetTypes;
      except
        raise Exception.CreateFmt(
          'Package.GetTypes failed on %s',
          [LPackage.Name]
        );
      end;
      PNode := nil;
      for T in LTypeList do
      begin
        LUnitName := GetUnitName(T);
        if LUnits.IndexOf(LUnitName) < 0 then
        begin
          LUnits.Add(LUnitName);
          PNode := tvRTTI.Items.AddChild(LPackageNode, LUnitName);
        end;

        LNode := tvRTTI.Items.AddChildObject(PNode, T.ToString, T);

        for F in T.GetDeclaredFields do
          tvRTTI.Items.AddChildObject(LNode, F.ToString, F);

        for M in T.GetDeclaredMethods do
          tvRTTI.Items.AddChildObject(LNode, M.ToString, M);

        for P in T.GetDeclaredProperties do
          tvRTTI.Items.AddChildObject(LNode, P.ToString, P);

        for IP in T.GetDeclaredIndexedProperties do
          tvRTTI.Items.AddChildObject(LNode, IP.ToString, IP);
      end;
    end;
  finally
    tvRTTI.Items.EndUpdate;
    LUnits.Free;
  end;
end;
{$ENDREGION}

end.
