{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I Concepts.inc}

unit Concepts.ChromeTabs.Form;

{ Demonstrates the TChromeTabs component. }

interface

uses
  System.Classes,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms,

  zObjInspector, zObjInspTypes,

  ChromeTabs, ChromeTabsClasses, ChromeTabsTypes;

type
  TfrmChromeTabs = class(TForm)
    {$REGION 'designer controls'}
    pnlMain       : TPanel;
    pnlLeft       : TPanel;
    pnlRight      : TPanel;
    sbrStatusBar  : TStatusBar;
    splSplitter   : TSplitter;
    pnlDrag       : TPanel;
    pnlHeader     : TPanel;
    lblHeader     : TLabel;
    {$ENDREGION}

    procedure FCTTopNeedDragImageControl(
      Sender          : TObject;
      ATab            : TChromeTab;
      var DragControl : TWinControl
    );
    procedure FCTTopTabDragDrop(
      Sender             : TObject;
      X, Y               : Integer;
      DragTabObject      : IDragTabObject;
      Cancelled          : Boolean;
      var TabDropOptions : TTabDropOptions
    );
    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

  private
    FObjectInspector : TzObjectInspector;
    FCTTop           : TChromeTabs;

  protected
    procedure ProcessDroppedTab(
      Sender             : TObject;
      X, Y               : Integer;
      DragTabObject      : IDragTabObject;
      Cancelled          : Boolean;
      var TabDropOptions : TTabDropOptions
    );
  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  System.TypInfo, System.Rtti,

  DDuce.Factories.zObjInspector;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmChromeTabs.AfterConstruction;
begin
  inherited AfterConstruction;
  FCTTop                        := TChromeTabs.Create(Self);
  FCTTop.Parent                 := pnlRight;
  FCTTop.Align                  := alTop;
  FCTTop.OnTabDragDrop          := FCTTopTabDragDrop;
  FCTTop.OnNeedDragImageControl := FCTTopNeedDragImageControl;

  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlLeft);
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublic;
  FObjectInspector.Component := FCTTop;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmChromeTabs.FCTTopNeedDragImageControl(Sender: TObject;
  ATab: TChromeTab; var DragControl: TWinControl);
begin
  DragControl := pnlDrag;
end;

procedure TfrmChromeTabs.FCTTopTabDragDrop(Sender: TObject; X, Y: Integer;
  DragTabObject: IDragTabObject; Cancelled: Boolean;
  var TabDropOptions: TTabDropOptions);
begin
  ProcessDroppedTab(Sender, X, Y, DragTabObject, Cancelled, TabDropOptions);
end;

function TfrmChromeTabs.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not (PItem.Prop.PropertyType is TRttiMethodType);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmChromeTabs.ProcessDroppedTab(Sender: TObject; X,
  Y: Integer; DragTabObject: IDragTabObject; Cancelled: Boolean;
  var TabDropOptions: TTabDropOptions);
var
  WinX, WinY: Integer;
  NewForm   : TfrmChromeTabs;
begin
  // Make sure that the drag drop hasn't been cancelled and that
  // we are not dropping on a TChromeTab control
  if (not Cancelled) and
    (DragTabObject.SourceControl <> DragTabObject.DockControl) and
    (DragTabObject.DockControl = nil) then
  begin
    // Find the drop position
    WinX := Mouse.CursorPos.X - DragTabObject.DragCursorOffset.X -
      ((Width - ClientWidth) div 2);
    WinY := Mouse.CursorPos.Y - DragTabObject.DragCursorOffset.Y -
      (Height - ClientHeight) + ((Width - ClientWidth) div 2);

    // Create a new form
    NewForm := TfrmChromeTabs.Create(Application);

    // Set the new form position
    NewForm.Position := poDesigned;
    NewForm.Left     := WinX;
    NewForm.Top      := WinY;

    // Show the form
    NewForm.Show;

    // Remove the original tab
    TabDropOptions := [tdDeleteDraggedTab];
  end;
end;
{$ENDREGION}

end.
