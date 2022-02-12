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

{$I Concepts.inc}

unit Concepts.System.Interfaces.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ActnList, Vcl.Buttons, Vcl.ComCtrls,

  Concepts.System.Interfaces.Interfaces,
  Concepts.System.Interfaces.InterfacedObject,
  Concepts.System.Interfaces.WeakReferences;

type
  TInterfacedObject =
    Concepts.System.Interfaces.InterfacedObject.TMyInterfacedObject;

type
  TfrmInterfaces = class(TForm)
    {$REGION 'designer controls'}
    aclMain                          : TActionList;
    actAssignInterfaceVarToObjectVar : TAction;
    actAssignObjectVarToInterfaceVar : TAction;
    actCallInnerMethod               : TAction;
    actCreateInnerInterfaceInstance: TAction;
    actCreateInterfaceInstance: TAction;
    actCreateObjectInstance: TAction;
    actCreateOuterInterfaceInstance: TAction;
    actFreeInterfaceInstance: TAction;
    actFreeObjectInstance: TAction;
    actRun                           : TAction;
    actShowInterfaceGUID             : TAction;
    btnAssignInterfaceVarToObjectVar : TBitBtn;
    btnAssignObjectVarToInterfaceVar : TBitBtn;
    btnCallInnerMethod               : TButton;
    btnCreateInnerInterface          : TBitBtn;
    btnCreateInterfaceVar            : TBitBtn;
    btnCreateObjectVar               : TBitBtn;
    btnCreateOuterInterface          : TBitBtn;
    btnExecute                       : TBitBtn;
    btnFreeInterfaceVar              : TBitBtn;
    btnFreeObjectVar                 : TBitBtn;
    btnShowInterfaceGUID             : TButton;
    chkRaiseException                : TCheckBox;
    chkRefCounted                    : TCheckBox;
    chkReportMemoryLeaksOnShutdown   : TCheckBox;
    lblInterfaceDeclatation          : TLabel;
    lblObjectDeclaration             : TLabel;
    lblObjectRefCount                : TLabel;
    lbxLog                           : TListBox;
    pgcMain: TPageControl;
    pnlOperations                    : TPanel;
    tsGUIDs                          : TTabSheet;
    tsImplementationByAggregation    : TTabSheet;
    tsInterfaceInternals             : TTabSheet;
    tsReferenceCounting              : TTabSheet;
    txtObjectRefCount                : TStaticText;
    tsVirtualInterface: TTabSheet;
    actGo: TAction;
    actStop: TAction;
    lblDefinition: TLabel;
    btnGo: TButton;
    btnStop: TButton;
    actCreateIGoStopInstance: TAction;
    btnGo1: TButton;
    actCallOuterMethod: TAction;
    btnCallOuterMethod: TButton;
    lbl1: TLabel;
    actClearLog: TAction;
    btnClearLog: TButton;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    tsWeakReferences: TTabSheet;
    actCreateObject1: TAction;
    actCreateObject2: TAction;
    actAssignReferences: TAction;
    btnAssignReferences: TButton;
    btnCreateObject1: TButton;
    btnCreateObject2: TButton;
    lblRefCount1: TLabel;
    lblRefCount2: TLabel;
    actAssignNilToInterface1: TAction;
    actAssignNilToInterface2: TAction;
    btnAssignNilToInterface1: TButton;
    btnAssignNilToInterface2: TButton;
    lbl5: TLabel;
    chkUseWeakReference1: TCheckBox;
    chkUseWeakReference2: TCheckBox;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure btnCallExecuteClick(Sender: TObject);
    procedure chkReportMemoryLeaksOnShutdownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure aclMainExecute(Action: TBasicAction; var Handled: Boolean);
    procedure actAssignObjectVarToInterfaceVarExecute(Sender: TObject);
    procedure actAssignInterfaceVarToObjectVarExecute(Sender: TObject);
    procedure actFreeInterfaceInstanceExecute(Sender: TObject);
    procedure actCreateInterfaceInstanceExecute(Sender: TObject);
    procedure actCreateObjectInstanceExecute(Sender: TObject);
    procedure actFreeObjectInstanceExecute(Sender: TObject);
    procedure actCreateOuterInterfaceInstanceExecute(Sender: TObject);
    procedure actCreateInnerInterfaceInstanceExecute(Sender: TObject);
    procedure actCallInnerMethodExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actShowInterfaceGUIDExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actGoExecute(Sender: TObject);
    procedure actCreateIGoStopInstanceExecute(Sender: TObject);
    procedure actCallOuterMethodExecute(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actCreateObject1Execute(Sender: TObject);
    procedure actCreateObject2Execute(Sender: TObject);
    procedure actAssignReferencesExecute(Sender: TObject);
    procedure actAssignNilToInterface1Execute(Sender: TObject);
    procedure actAssignNilToInterface2Execute(Sender: TObject);
    {$ENDREGION}

  private
    FObject         : TInterfacedObject;
    FInterface      : ITrackRefCount;
    FInnerInterface : IInnerInterface;
    FOuterInterface : IOuterInterface;
    FGoStop         : IGoStop;
    FInterface1     : IInterface1;
    FInterface2     : IInterface2;

  protected
    {$REGION 'event handlers'}
    procedure OnObjectAddRef(Sender: TObject; ARefCount: Integer);
    procedure OnObjectRelease(Sender: TObject; ARefCount: Integer);
    procedure OnObjectDestroy(Sender: TObject);
    procedure OnQueryInterface(Sender: TObject);
    {$ENDREGION}

    procedure UpdateActions; override;

    procedure AddToLog(const AString: string);

  public
    procedure CreateInterfaceInstance;
    procedure CreateObjectInstance;
    procedure CreateIGoStopInstance;

    procedure Run(const AInterface: IInterface);
  end;

var
  frmInterfaces: TfrmInterfaces;

implementation

{$R *.dfm}

uses
  System.Rtti,

  Concepts.System.Interfaces.DelegatedImplementation;

{$REGION 'construction and destruction'}
procedure TfrmInterfaces.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := chkReportMemoryLeaksOnShutdown.Checked;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmInterfaces.OnObjectAddRef(Sender: TObject; ARefCount: Integer);
begin
  AddToLog(Format('%s._AddRef %d', [Sender.ClassName, ARefCount]));
end;

procedure TfrmInterfaces.OnObjectDestroy(Sender: TObject);
begin
  AddToLog('Object destroyed');
end;

procedure TfrmInterfaces.OnObjectRelease(Sender: TObject; ARefCount: Integer);
begin
  AddToLog(Format('%s._Release %d', [Sender.ClassName, ARefCount]));
end;

procedure TfrmInterfaces.OnQueryInterface(Sender: TObject);
begin
  AddToLog(Format('%s.QueryInterface', [Sender.ClassName]));
end;

procedure TfrmInterfaces.btnCallExecuteClick(Sender: TObject);
begin
  Run(FInterface);
end;

procedure TfrmInterfaces.chkReportMemoryLeaksOnShutdownClick(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := (Sender as TCheckBox).Checked;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmInterfaces.actCallOuterMethodExecute(Sender: TObject);
begin
  if Assigned(FOuterInterface) then
    FOuterInterface.OuterMethod;
end;

procedure TfrmInterfaces.actClearLogExecute(Sender: TObject);
begin
  lbxLog.Clear;
end;

procedure TfrmInterfaces.actAssignInterfaceVarToObjectVarExecute(Sender: TObject);
begin
  FObject := FInterface as TInterfacedObject;
end;

procedure TfrmInterfaces.actAssignNilToInterface1Execute(Sender: TObject);
begin
  FInterface1 := nil;
end;

procedure TfrmInterfaces.actAssignNilToInterface2Execute(Sender: TObject);
begin
  FInterface2 := nil;
end;

procedure TfrmInterfaces.actAssignObjectVarToInterfaceVarExecute(Sender: TObject);
begin
  FInterface := FObject;
end;

procedure TfrmInterfaces.actAssignReferencesExecute(Sender: TObject);
begin
  FInterface1.InterfaceReference := FInterface2;
  FInterface2.InterfaceReference := FInterface1;
end;

procedure TfrmInterfaces.actCallInnerMethodExecute(Sender: TObject);
begin
  if Assigned(FInnerInterface) then
    FInnerInterface.InnerMethod;
end;

procedure TfrmInterfaces.actCreateIGoStopInstanceExecute(Sender: TObject);
begin
  CreateIGoStopInstance;
end;

procedure TfrmInterfaces.actCreateInnerInterfaceInstanceExecute(Sender: TObject);
begin
  FInnerInterface := TOuterObject.Create(
    chkRefCounted.Checked,
    OnObjectAddRef,
    OnObjectRelease,
    OnQueryInterface,
    OnObjectDestroy
  );
end;

procedure TfrmInterfaces.actCreateInterfaceInstanceExecute(Sender: TObject);
begin
  CreateInterfaceInstance;
end;

procedure TfrmInterfaces.actCreateObject1Execute(Sender: TObject);
begin
  FInterface1 := TObject1.Create(
    True,
    chkUseWeakReference2.Checked,
    OnObjectAddRef,
    OnObjectRelease,
    OnQueryInterface,
    OnObjectDestroy
  );
end;

procedure TfrmInterfaces.actCreateObject2Execute(Sender: TObject);
begin
  FInterface2 := TObject2.Create(
    True,
    chkUseWeakReference1.Checked,
    OnObjectAddRef,
    OnObjectRelease,
    OnQueryInterface,
    OnObjectDestroy
  );
end;

procedure TfrmInterfaces.actCreateObjectInstanceExecute(Sender: TObject);
begin
  CreateObjectInstance;
end;

procedure TfrmInterfaces.actCreateOuterInterfaceInstanceExecute(Sender: TObject);
begin
  FOuterInterface := TOuterObject.Create(
    chkRefCounted.Checked,
    OnObjectAddRef,
    OnObjectRelease,
    OnQueryInterface,
    OnObjectDestroy
  );
end;

procedure TfrmInterfaces.actFreeInterfaceInstanceExecute(Sender: TObject);
begin
  FInterface := nil;
end;

procedure TfrmInterfaces.actFreeObjectInstanceExecute(Sender: TObject);
begin
  FreeAndNil(FObject);
end;

procedure TfrmInterfaces.actGoExecute(Sender: TObject);
begin
  if Assigned(FGoStop) then
    FGoStop.Go;
end;

procedure TfrmInterfaces.actRunExecute(Sender: TObject);
var
  E : IRunnable;
begin
  E := TRunnable.Create(
    chkRefCounted.Checked,
    OnObjectAddRef,
    OnObjectRelease,
    OnQueryInterface,
    OnObjectDestroy
  );
  E.Run(chkRaiseException.Checked);
end;

procedure TfrmInterfaces.actShowInterfaceGUIDExecute(Sender: TObject);
var
  GUID: TGUID;
begin
  GUID := TGUID(IRunnable);
  ShowMessage(GUID.ToString);
end;

procedure TfrmInterfaces.actStopExecute(Sender: TObject);
begin
  if Assigned(FGoStop) then
    FGoStop.Stop;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmInterfaces.UpdateActions;
begin
  inherited UpdateActions;
  if Assigned(FInterface) then
  begin
    txtObjectRefCount.Caption := FInterface.RefCount.ToString;
  end
  else
  begin
    txtObjectRefCount.Caption := '0';
  end;
  if Assigned(FInterface1) then
  begin
    lblRefCount1.Caption := FInterface1.RefCount.ToString;
  end;
  if Assigned(FInterface2) then
  begin
    lblRefCount2.Caption := FInterface2.RefCount.ToString;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmInterfaces.aclMainExecute(Action: TBasicAction; var Handled: Boolean);
begin
  AddToLog(
    Format('Execute action (%s): %s',
           [Action.Name, (Action as TContainedAction).Caption])
  );
end;

procedure TfrmInterfaces.AddToLog(const AString: string);
begin
  if Assigned(lbxLog) then
  begin
    lbxLog.Items.Add(Format('%3d. %s', [lbxLog.Count, AString]));
    lbxLog.ItemIndex := lbxLog.Items.Count - 1;
    lbxLog.ClearSelection;
  end;
end;

procedure TfrmInterfaces.CreateIGoStopInstance;
var
 VIIE : TVirtualInterfaceInvokeEvent; // anonymous callback procedure
begin
  VIIE := procedure(Method: TRttiMethod; const Args: TArray<TValue>;
    out Result: TValue)
    const
      MSG = 'Method declaration: %s; ' + sLineBreak +
            'Argument values:' + sLineBreak + '%s; ' +
            'Return value: %s';
    var
      S : string;
      V : TValue;
    begin
      for V in Args do
        S := S + V.ToString + sLineBreak;
      S := Format(MSG, [Method.ToString, S, Result.ToString]);
      AddToLog(S);
    end;
  FGoStop := TVirtualInterface.Create(TypeInfo(IGoStop), VIIE) as IGoStop;
end;

procedure TfrmInterfaces.CreateInterfaceInstance;
begin
  FInterface := TInterfacedObject.Create(
    chkRefCounted.Checked,
    OnObjectAddRef,
    OnObjectRelease,
    OnQueryInterface,
    OnObjectDestroy
  );
end;

procedure TfrmInterfaces.CreateObjectInstance;
begin
  FObject := TInterfacedObject.Create(
    chkRefCounted.Checked,
    OnObjectAddRef,
    OnObjectRelease,
    OnQueryInterface,
    OnObjectDestroy
  );
end;

{
  The SysUtils unit provides an overloaded function called Supports that returns
  true or false when class types and instances support a particular interface
  represented by a GUID. The Supports function is used in the manner of the
  Delphi is and as operators. The significant difference is that the Supports
  function can take as the right operand either a GUID or an interface type
  associated with a GUID, whereas is and as take the name of a type. For more
  information about is and as, see Class References.
}

procedure TfrmInterfaces.Run(const AInterface: IInterface);
var
  GUID: TGUID;
begin
  GUID := TGUID(IRunnable);
  ShowMessage(GUID.ToString);
  //if AInterface is IExecutable then    -> not supported
  //if Supports(AInterface, TGuid.Create(IID_IRunnable)) then
  //if Supports(AInterface, IRunnable_GUID) then
  if Supports(AInterface, IRunnable) then
  begin
     (AInterface as IRunnable).Run;
  end
  else
    ShowMessage('Interface IRunnable not supported');
end;
{$ENDREGION}

end.
