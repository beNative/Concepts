{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.RTTEye.Form;

interface

uses
  System.Actions, System.Classes, System.Rtti,
  Winapi.Windows, Winapi.Messages,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Controls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Forms;

type
  TfrmRTTEye = class(TForm)
    {$REGION 'designer controls'}
    aclMain      : TActionList;
    actCollapse  : TAction;
    actExpand    : TAction;
    actLoad      : TAction;
    actSearch    : TAction;
    btnCollapse1 : TButton;
    btnExpand1   : TButton;
    btnLoad      : TButton;
    btnSearch    : TButton;
    EditSearch   : TEdit;
    lvRtti       : TListView;
    pnlBottom    : TPanel;
    pnlMain      : TPanel;
    splVertical  : TSplitter;
    tvRtti       : TTreeView;
    {$ENDREGION}

    procedure tvRttiCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvRttiChange(Sender: TObject; Node: TTreeNode);
    procedure tvRttiDblClick(Sender: TObject);

    procedure actLoadExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actExpandExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);

  private
    FRttiContext: TRttiContext;

    procedure LoadTree;

    function FindTreeViewText(AText: string; ATreeView: TTreeView): TTreeNode;
    procedure CollapseTree;
    procedure ExpandTree;

  public
    procedure AfterConstruction; override;

  end;

procedure ShowRTTEye;

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

{$REGION 'interfaced routines'}
procedure ShowRTTEye;
var
  F: TfrmRTTEye;
begin
  F := TfrmRTTEye.Create(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmRTTEye.AfterConstruction;
begin
  inherited;
  LoadTree;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmRTTEye.actCollapseExecute(Sender: TObject);
begin
  CollapseTree;
end;

procedure TfrmRTTEye.actExpandExecute(Sender: TObject);
begin
  ExpandTree;
end;

procedure TfrmRTTEye.actLoadExecute(Sender: TObject);
begin
  LoadTree;
end;

procedure TfrmRTTEye.actSearchExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  if Trim(EditSearch.Text) = '' then
    Exit;

  Node := FindTreeViewText(Trim(EditSearch.Text), tvRTTI);
  if Node <> nil then
  begin
    Node.MakeVisible;
    tvRTTI.Selected := Node
  end
  else
    ShowMessage(Format('%s Not found', [Trim(EditSearch.Text)]));
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmRTTEye.tvRttiChange(Sender: TObject; Node: TTreeNode);
var
  aNode : TTreeNode;
  lTyp  : TRttiType;
  T     : TRttiType;
  M     : TRttiMethod;
  P     : TRttiProperty;
  lProp : TRttiProperty;
  F     : TRttiField;
  Item  : TListItem;
begin
  aNode := tvRTTI.Selected;
  if aNode <> nil then
  begin
    lvRtti.Items.BeginUpdate;
    try
      lvRtti.Items.Clear;
    finally
      lvRtti.Items.EndUpdate;
    end;

    case aNode.Level of
      LEVEL_TYPE:
        begin
          lTyp := aNode.Data;
          T := FRttiContext.GetType(TRttiType);

          lvRtti.Items.BeginUpdate;
          try
            for P in T.GetProperties do
            begin
            // Ugly hack to prevent calling properties in the RTTI type wich
            // start with 'As' like AsInstance, AsOrdinal, AsRecord, AsSet
              if Copy(P.Name, 1, 2) = 'As' then
                Continue;

              Item := lvRtti.Items.Add;
              Item.Caption := P.Name;
              Item.SubItems.Add(P.GetValue(lTyp).ToString);
            end;
          finally
            lvRtti.Items.EndUpdate;
          end;
        end;

      LEVEL_FIELD:
        begin
          if aNode.Data = nil then
            Exit;

          if TRttiObject(aNode.Data).ClassNameIs('TRttiInstanceMethodEx') or
            TRttiObject(aNode.Data).ClassNameIs('TRttiIntfMethod') or      // TS
            TRttiObject(aNode.Data).ClassNameIs('TRttiRecordMethod') then  // TS

          begin
            M := aNode.Data;
            T := FRttiContext.GetType(TRttiMethod);
            lvRtti.Items.BeginUpdate;
            try
              for P in T.GetProperties do
              begin
                Item := lvRtti.Items.Add;
                Item.Caption := P.Name;
// Another uggly hack due to RTTI limitations, wich raise an exception when the
// ReturnType, MethodKind or CallingConvention property is called and
// lMethod.HasExtendedInfo is false, because one of the types has no RTTI
// information.
//                if ((P.Name = 'ReturnType') or
//                    (P.Name = 'CallingConvention') or
//                    (P.Name = 'MethodKind'))
//                  and not M.HasExtendedInfo then
//                  Item.SubItems.Add('Not supported by RTTI')
//                else
                  Item.SubItems.Add(P.GetValue(M).ToString);
              end;
            finally
              lvRtti.Items.EndUpdate;
            end;

          end
          else if TRttiObject(Node.Data).ClassNameIs('TRttiInstancePropertyEx')

            then
          begin
            lProp := aNode.Data;
            T := FRttiContext.GetType(TRttiProperty);
            lvRtti.Items.BeginUpdate;
            try
              for P in T.GetProperties do
              begin
                Item := lvRtti.Items.Add;
                Item.Caption := P.Name;
                Item.SubItems.Add(P.GetValue(lProp).ToString);
              end;
            finally
              lvRtti.Items.EndUpdate;
            end;
          end
          else if TRttiObject(Node.Data).ClassNameIs('TRttiIndexedProperty') then
          begin
            F := aNode.Data;
            T := FRttiContext.GetType(TRttiIndexedProperty);

            lvRtti.Items.BeginUpdate;
            try
              for P in T.GetProperties do
              begin
                Item := lvRtti.Items.Add;
                Item.Caption := P.Name;
                Item.SubItems.Add(P.GetValue(F).ToString);
              end;
            finally
              lvRtti.Items.EndUpdate;
            end;
          end
          else if TRttiObject(Node.Data).ClassNameIs('TRttiInstanceFieldEx')
            or TRttiObject(Node.Data).ClassNameIs('TRttiRecordField')  // TS
            then
          begin
            F := aNode.Data;
            T := FRttiContext.GetType(TRttiField);

            lvRtti.Items.BeginUpdate;
            try
              for P in T.GetProperties do
              begin
                Item := lvRtti.Items.Add;
                Item.Caption := P.Name;
                Item.SubItems.Add(P.GetValue(F).ToString);
              end;
            finally
              lvRtti.Items.EndUpdate;
            end;
          end
          else
          begin
            ShowMessage(TRttiObject(Node.Data).ClassName);
          end;
        end;
    end;
  end;
end;

procedure TfrmRTTEye.tvRttiCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  FontColor : TColor;
  BackColor : TColor;
  T         : TRttiType;
  M         : TRttiMethod;
  P         : TRttiProperty;
  F         : TRttiField;
begin
  FontColor := Sender.Canvas.Font.Color;
  Sender.Canvas.Font.Color := clNavy;
  Sender.Canvas.Font.Color := FontColor;
  BackColor := clWindow;
  FontColor := clWindowText;

  case Node.Level of
    LEVEL_PACKAGE:
      begin
        FontColor := clBlack;
        Sender.Canvas.Font.Style := [fsBold];
      end;

    LEVEL_UNIT:
      begin
        FontColor := clRed;
      end;

    LEVEL_TYPE:
      begin
        T := Node.Data;

        if T <> nil then
          case T.TypeKind of
            tkClass:
              begin
                FontColor := clGreen;
                Sender.Canvas.Font.Style := [fsBold]
              end;
          else
            FontColor := clBlue;
          end;
      end;

    LEVEL_FIELD:
      begin
        if Node.Data = nil then
          Exit;

        if TRttiObject(Node.Data).ClassNameIs('TRttiInstanceMethodEx') then
        begin
          M := Node.Data;
          FontColor := clGray;
        end
        else if TRttiObject(Node.Data).ClassNameIs('TRttiInstancePropertyEx')
          then
        begin
          P := Node.Data;
          FontColor := clNavy;
        end;
      end;
  end;

  if (Node.Selected) then
  begin
    BackColor := clHighlight;
    FontColor := clWindow;
  end;

  Sender.Canvas.Brush.Color := BackColor;
  Sender.Canvas.Font.Color := FontColor;
  DefaultDraw := True;
end;

{ Jump to referenced type. }

procedure TfrmRTTEye.tvRttiDblClick(Sender: TObject);

  function FindRttiType(T: TRttiType): TTreeNode;
  var
    I: Integer;
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
  Node : TTreeNode;
  T    : TRttiType;
  M    : TRttiMethod;
  P    : TRttiProperty;
  F    : TRttiField;
begin
  Node := tvRTTI.Selected;
  case Node.Level of
    LEVEL_FIELD:
      begin
        if TRttiObject(Node.Data).ClassNameIs('TRttiInstanceFieldEx') then
        begin
          F := Node.Data;
          T := FRttiContext.FindType(F.FieldType.QualifiedName);
          if Assigned(T) then
          begin
            Node := FindRttiType(T);
            if Node <> nil then
            begin
              Node.MakeVisible;
              tvRTTI.Selected := Node;
            end;
          end;
        end
        else if TRttiObject(Node.Data).ClassNameIs('TRttiInstancePropertyEx')
          then
        begin
          P := Node.Data;
          T := FRttiContext.FindType(P.PropertyType.QualifiedName);
          if Assigned(T) then
          begin
            Node := FindRttiType(T);
            if Node <> nil then
            begin
              Node.MakeVisible;
              tvRTTI.Selected := Node;
            end;
          end;
        end
        else if TRttiObject(Node.Data).ClassNameIs('TRttiInstanceMethodEx') then
        begin
          M := Node.Data;
          if M.HasExtendedInfo and (M.MethodKind in [mkFunction,
            mkClassFunction]) then
          begin
            T := FRttiContext.FindType(M.ReturnType.QualifiedName);
            if Assigned(T) then
            begin
              Node := FindRttiType(T);
              if Node <> nil then
              begin
                Node.MakeVisible;
                tvRTTI.Selected := Node;
              end;
            end;
          end;
        end;
      end;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
function TfrmRTTEye.FindTreeViewText(AText: string; ATreeView: TTreeView):
  TTreeNode;
var
  Node: TTreeNode;
begin
  Result := nil;
  ATreeView.Items.BeginUpdate;
  try
    if ATreeView.Items.Count = 0 then
      Exit;
    Node := ATreeView.Selected;
    if Node = nil then
      Node := ATreeView.Items[0]
    else
      Node := Node.GetNext;

    while Node <> nil do
    begin
      if Pos(UpperCase(AText), UpperCase(Node.Text)) > 0 then
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
  T           : TRttiType;
  M           : TRttiMethod;
  P           : TRttiProperty;
  IP          : TRttiIndexedProperty;
  F           : TRttiField;
  PNode       : TTreeNode;
  Node        : TTreeNode;
  TypeList    : TArray<TRttiType>;
  Units       : TStrings;
  UnitName    : string;
  Package     : TRttiPackage;
  PackageNode : TTreeNode;
  B           : Boolean;
begin
  tvRTTI.Items.BeginUpdate;
  tvRTTI.Items.Clear;
  Units := TStringList.Create;
  try
    for Package in FRttiContext.GetPackages do
    begin
      Units.Clear;
      PackageNode := tvRTTI.Items.Add(nil, Package.Name);
      B := True;
      try
        TypeList := Package.GetTypes;
      except
        ShowMessageFmt('Package.GetTypes failed on %s', [Package.Name]);
        B := False;
      end;

      if B then
      begin
        PNode := nil;
        for T in TypeList do
        begin
          UnitName := GetUnitName(T);
          if Units.IndexOf(UnitName) < 0 then
          begin
            Units.Add(UnitName);
            PNode := tvRTTI.Items.AddChild(PackageNode, UnitName);
          end;

          Node := tvRTTI.Items.AddChildObject(PNode, T.ToString, T);

          for F in T.GetDeclaredFields do
            tvRTTI.Items.AddChildObject(Node, F.ToString, F);

          for M in T.GetDeclaredMethods do
            tvRTTI.Items.AddChildObject(Node, M.ToString, M);

          for P in T.GetDeclaredProperties do
            tvRTTI.Items.AddChildObject(Node, P.ToString, P);

          for IP in T.GetDeclaredIndexedProperties do
            tvRTTI.Items.AddChildObject(Node, IP.ToString, IP);
        end;
      end;
    end;
  finally
    tvRTTI.Items.EndUpdate;
    Units.Free;
  end;
end;
{$ENDREGION}

end.

