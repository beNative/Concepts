{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kcontrolsdesign;

{$include ..\..\source\kcontrols.inc}

interface

uses
{$IFDEF FPC}
  ComponentEditors, PropEdits,
{$ELSE}
  DesignIntf, DesignEditors,
 {$IFDEF COMPILER17_UP}
  Actions,
 {$ENDIF}
{$ENDIF}
  KPictureEditor
  ;

type
  TKOpenDialogEditor = class(TClassProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

  TKFileNameEditor = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TKFolderEditor = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TKGraphicEditor = class(TClassProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TKPageControlEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function FindUniqueName(const Name: string): string;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

{$IFNDEF FPC}
  {$R *.dcr}
{$ENDIF}

uses
  Classes, Dialogs, Controls, SysUtils, Forms, Graphics, ActnList,
  KGraphics, KControls, KButtons, KDialogs, KGrids, KHexEditor,
  KEdits, KLabels, KLog, KMemo, KMemoFrm, KProgress, KSplitter,
  KPageControl, KRes
{$IFDEF TKDBGRID_USE}
  , KDBGrids
{$ENDIF}
{$IFDEF FPC}
  , LResources
{$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('TK', [
    TKGrid,
{$IFDEF TKDBGRID_USE}
    TKDBGrid,
{$ENDIF}
    TKMemo,
{$IFnDEF FPC}
    TKMemoFrame,
{$ENDIF}
    TKHexEditor,
    TKBitBtn,
    TKColorButton,
    TKSpeedButton,
    TKNumberEdit,
    TKFileNameEdit,
    TKLog,
    TKPercentProgressBar,
    TKLinkLabel,
    TKGradientLabel,
    TKSplitter,
    TKPageControl,
    TKPrintPreview,
    TKPrintSetupDialog,
    TKPrintPreviewDialog,
    TKBrowseFolderDialog
  ]);

  // editors
  RegisterPropertyEditor(TypeInfo(TKFileNameEditDlgProperties), nil, '', TKOpenDialogEditor);
  RegisterPropertyEditor(TypeInfo(TFileName), nil, 'FileName', TKFileNameEditor);
  RegisterPropertyEditor(TypeInfo(TFolder), nil, '', TKFolderEditor);
  RegisterPropertyEditor(TypeInfo(TKAlphaBitmap), nil, '', TKGraphicEditor);

  RegisterComponentEditor(TKPageControl, TKPageControlEditor);
  RegisterComponentEditor(TKTabSheet, TKPageControlEditor);
  RegisterNoIcon([TKTabSheet]);
  RegisterClasses([TKTabSheet]);

  // actions
  RegisterActions('KMemo', [TKMemoEditCopyAction, TKMemoEditCutAction,
    TKMemoEditPasteAction, TKMemoEditSelectAllAction], nil);

  // images
{$IFDEF REGISTER_PICTURE_FORMATS}
  TPicture.RegisterFileFormat('BMA', 'KControls alpha bitmap', TKAlphaBitmap);
{$ENDIF}
end;

{ TKOpenDialogEditor }

function TKOpenDialogEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

{ TFileNameEditor }

function TKFileNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paRevertable, paDialog];
end;

procedure TKFileNameEditor.Edit;

  procedure DoOpenDlg;
  var
    OD: TOpenDialog;
  begin
    OD := TOpenDialog.Create(Application);
    try
      OD.Filter := sEDAllFiles;
      OD.FileName := GetValue;
      if OD.Execute then SetValue(OD.FileName);
    finally
      OD.Free;
    end;
  end;

var
  BF: TKBrowseFolderDialog;
  P: TPersistent;
begin
  inherited;
  P := GetComponent(0);
  if P is TKFileNameEdit then with TKFileNameEdit(P) do
  begin
    if foFolderOnly in Options then
    begin
      BF := TKBrowseFolderDialog.Create(Application);
      try
        BF.Position := poScreenCenter;
        BF.Folder := GetValue;
        if BF.Execute then SetValue(BF.Folder);
      finally
        BF.Free;
      end;
    end else
      DoOpenDlg;
  end else
    DoOpenDlg;
end;

{ TKFolderEditor }

function TKFolderEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paRevertable, paDialog, paMultiSelect];
end;

procedure TKFolderEditor.Edit;
var
  BF: TKBrowseFolderDialog;
begin
  inherited;
  BF := TKBrowseFolderDialog.Create(Application);
  try
    BF.Position := poScreenCenter;
    if BF.Execute then
      SetValue(BF.Folder);
  finally
    BF.Free;
  end;
end;

{ TKGraphicEditor }

function TKGraphicEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paRevertable, paDialog];
end;

procedure TKGraphicEditor.Edit;
var
  PictureEditor: TKPictureEditForm;
  P: TObject;
  Graphic: TKGraphic;
begin
{$IFDEF FPC}
  P := GetObjectValue(TGraphic);
{$ELSE}
  P := TObject(GetOrdValue);
{$ENDIF}
  if P is TKGraphic then
  begin
    Graphic := TKGraphic(P);
    PictureEditor := TKPictureEditForm.Create(nil);
    try
      PictureEditor.IMMain.Picture.Graphic := Graphic;
      PictureEditor.ODMain.Filter := Format('%s (%s)|%1:s', [Graphic.Description, Graphic.FileFilter]);
      PictureEditor.SDMain.Filter := PictureEditor.ODMain.Filter;
      if PictureEditor.ShowModal = mrOk then
      {$IFDEF FPC}
        SetPtrValue(PictureEditor.IMMain.Picture.Graphic);
      {$ELSE}
        SetOrdValue(Integer(PictureEditor.IMMain.Picture.Graphic));
      {$ENDIF}
    finally
      PictureEditor.Free;
    end;
  end else
    raise Exception.CreateRes(@SInvalidGraphicFormat);
end;

{ TKPageControlEditor }

procedure TKPageControlEditor.ExecuteVerb(Index: Integer);
var
  PC: TKPageControl;
  Sheet: TKTabSheet;
{$IFDEF FPC}
  Hook: TPropertyEditorHook;
{$ENDIF}
begin
  if Component is TKPageControl then
    PC := TKPageControl(Component)
  else if Component is TKTabSheet then
    PC := TKPageControl(TKTabSheet(Component).PageControl)
  else
    PC := nil;
  if PC <> nil then
  begin
{$IFDEF FPC}
    Hook:=nil;
    if not GetHook(Hook) then Exit;
{$ENDIF}
    case Index of
      1:
      begin
        Sheet := PC.ActivePage;
        PC.DeletePage(Sheet);
{$IFDEF FPC}
        Hook.PersistentDeleting(Sheet);
{$ENDIF}
      end;
      2:
      begin
        PC.ActivePage := PC.FindNextPage(PC.ActivePage, True);
      end;
      3:
      begin
        PC.ActivePage := PC.FindNextPage(PC.ActivePage, False);
      end;
    else
{$IFDEF FPC}
      Sheet := PC.AddPage(Designer.Form);
{$ELSE}
      Sheet := PC.AddPage(Designer.GetRoot);
{$ENDIF}
      Sheet.Name := FindUniqueName('KTabSheet');
      Sheet.Caption := Sheet.Name;
{$IFDEF FPC}
      Hook.PersistentAdded(Sheet,true);
{$ENDIF}
    end;
    if Designer <> nil then
      Designer.Modified;
  end;
end;

function TKPageControlEditor.FindUniqueName(const Name: string): string;
var
{$IFDEF FPC}
  FFormDesigner: TComponentEditorDesigner;
{$ELSE}
  FFormDesigner: IDesigner;
{$ENDIF}
begin
  FFormDesigner := GetDesigner;
  Result := FFormDesigner.UniqueName(Name);
end;

function TKPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    1: Result := 'Delete page';
    2: Result := 'Next page';
    3: Result := 'Previous page';
  else
    Result := 'New page';
  end;
end;

function TKPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

{$IFDEF FPC}
initialization
  {$i kcontrolsdesign.lrs}
{$ENDIF}

end.
