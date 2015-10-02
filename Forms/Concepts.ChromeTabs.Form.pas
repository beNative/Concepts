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

unit Concepts.ChromeTabs.Form;

{$I ..\Source\DDuce.inc}

interface

uses
  System.Classes,
  Vcl.ComCtrls, Vcl.ButtonGroup, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.Forms,

  DDuce.Components.PropertyInspector,

  ChromeTabs, ChromeTabsClasses, ChromeTabsTypes;

type
  TfrmChromeTabs = class(TForm)
    {$REGION 'designer controls'}
    pnlMain       : TPanel;
    pnlLeft       : TPanel;
    pnlRight      : TPanel;
    cbxControls   : TComboBox;
    sbrStatusBar  : TStatusBar;
    splSplitter   : TSplitter;
    ctTop         : TChromeTabs;
    ctBottom      : TChromeTabs;
    pnlDrag       : TPanel;
    {$ENDREGION}

    procedure cbxControlsChange(Sender: TObject);
    procedure ctTopNeedDragImageControl(Sender: TObject; ATab: TChromeTab;
      var DragControl: TWinControl);
    procedure ctTopTabDragDrop(Sender: TObject; X, Y: Integer;
      DragTabObject: IDragTabObject; Cancelled: Boolean;
      var TabDropOptions: TTabDropOptions);

  private
    FPropertyInspector: TPropertyInspector;

  public
    procedure AfterConstruction; override;

    procedure ProcessDroppedTab(Sender: TObject; X, Y: Integer;
      DragTabObject: IDragTabObject; Cancelled: Boolean;
      var TabDropOptions: TTabDropOptions);

  end;

implementation

uses
  Concepts.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmChromeTabs.AfterConstruction;
var
  I: Integer;
  C: TWinControl;
begin
  inherited;
  FPropertyInspector := TConceptFactories.CreateInspector(
    Self,
    pnlLeft,
    ctTop
  );
  FPropertyInspector.Name := 'PropertyInspector';
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TWinControl then
    begin
      C := TWinControl(Components[I]);
      cbxControls.AddItem(C.Name, C);
    end;
  end;
  cbxControls.ItemIndex := 0;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmChromeTabs.cbxControlsChange(Sender: TObject);
var
  C: TWinControl;
begin
  C := cbxControls.Items.Objects[cbxControls.ItemIndex] as TWinControl;
  FPropertyInspector.Objects[0] := C;
end;
{$ENDREGION}

procedure TfrmChromeTabs.ctTopNeedDragImageControl(Sender: TObject;
  ATab: TChromeTab; var DragControl: TWinControl);
begin
  DragControl := pnlDrag;
end;

procedure TfrmChromeTabs.ProcessDroppedTab(Sender: TObject; X,
  Y: Integer; DragTabObject: IDragTabObject; Cancelled: Boolean;
  var TabDropOptions: TTabDropOptions);
var
  WinX, WinY: Integer;
  NewForm: TfrmChromeTabs;
begin
  // Make sure that the drag drop hasn't been cancelled and that
  // we are not dropping on a TChromeTab control
  if (not Cancelled) and
     (DragTabObject.SourceControl <> DragTabObject.DockControl) and
     (DragTabObject.DockControl = nil) then
  begin
    // Find the drop position
    WinX := Mouse.CursorPos.X - DragTabObject.DragCursorOffset.X - ((Width - ClientWidth) div 2);
    WinY := Mouse.CursorPos.Y - DragTabObject.DragCursorOffset.Y - (Height - ClientHeight) + ((Width - ClientWidth) div 2);

    // Create a new form
    NewForm := TfrmChromeTabs.Create(Application);

    // Set the new form position
    NewForm.Position := poDesigned;
    NewForm.Left := WinX;
    NewForm.Top := WinY;

    // Show the form
    NewForm.Show;

    // Remove the original tab
    TabDropOptions := [tdDeleteDraggedTab];
  end;
end;

procedure TfrmChromeTabs.ctTopTabDragDrop(Sender: TObject; X, Y: Integer;
  DragTabObject: IDragTabObject; Cancelled: Boolean;
  var TabDropOptions: TTabDropOptions);
begin
  ProcessDroppedTab(Sender, X, Y, DragTabObject, Cancelled, TabDropOptions);
end;

end.
