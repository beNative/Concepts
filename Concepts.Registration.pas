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

unit Concepts.Registration;

{ Handles registration of all supported concept forms. }

interface

uses
  Vcl.Graphics;

{$I Concepts.inc}

type
  TConcepts = record
  strict private
    class var FCategoryColor : TColor;

    class procedure RegisterSpringConcepts; static;
    class procedure RegisterDSharpConcepts; static;
    class procedure RegisterDevExpressConcepts; static;
    class procedure RegisterKControlsConcepts; static;
    class procedure RegisterSystemConcepts; static;
    class procedure RegisterVclConcepts; static;
    class procedure RegisterWinApiConcepts; static;
    class procedure RegisterIndyConcepts; static;
  public
    class procedure RegisterConcepts; static;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils, System.Classes,

  {$REGION 'Concept form units'}
  {$IFDEF WINAPI}
  Concepts.Winapi.LockPaint.Form,
  {$ENDIF}

  {$IFDEF VCL}
  Concepts.Vcl.GridPanel.Form,
  {$IFDEF DELPHIX_SEATTLE_UP}
  Concepts.Vcl.RelativePanel.Form,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FIREDAC}
  Concepts.FireDAC.Form,
  {$ENDIF}

  {$IFDEF SYSTEM}
  Concepts.System.RTTI.Form,
  Concepts.System.Variants.Form,
  Concepts.System.Threads.Form,
  Concepts.System.RegularExpressions.Form,
  Concepts.System.LiveBindings.Form,
  Concepts.System.Threading.Form,
  Concepts.System.Libraries.Form,
  Concepts.System.AnonymousMethods.Form,
  Concepts.System.InterfaceImplementationByAggregation.Form,
  Concepts.System.StringList.Form,
  Concepts.System.VirtualMethodInterceptor.Form,
  Concepts.System.VirtualInterface.Form,
  Concepts.System.PublishedFields.Form,
  Concepts.System.Interfaces.Form,
  {$ENDIF}

  {$IFDEF DEVEXPRESS}
  Concepts.DevExpress.cxEditors.Form,
  Concepts.DevExpress.cxGridViewPresenter.Form,
  {$ENDIF}

  {$IFDEF DSHARP}
  Concepts.DSharp.TreeViewPresenter.Tree.Form,
  Concepts.DSharp.TreeViewPresenter.List.Form,
  {$ENDIF}

  {$IFDEF SPRING}
  Concepts.Spring.ClassProxy.Form,
  Concepts.Spring.Interception.Form,
  Concepts.Spring.Collections.Form,
  Concepts.Spring.LazyInstantiation.Form,
  Concepts.Spring.MulticastEvents.Form,
  Concepts.Spring.ObjectDataSet.Form,
  Concepts.Spring.Logging.Form,
  Concepts.Spring.Types.Form,
  Concepts.Spring.Utils.Form,
  Concepts.Spring.Persistence.Form,
  {$ENDIF}

  {$IFDEF BTMEMORYMODULE}
  Concepts.BTMemoryModule.Form,
  {$ENDIF}

  {$IFDEF SQLBUILDER4D}
  Concepts.SQLBuilder4D.Form,
  {$ENDIF}

  {$IFDEF CHROMETABS}
  Concepts.ChromeTabs.Form,
  {$ENDIF}

  {$IFDEF BCEDITOR}
  Concepts.BCEditor.Form,
  {$ENDIF}

  {$IFDEF DDETOURS}
  Concepts.DDetours.Form,
  {$ENDIF}

  {$IFDEF DELPHIZMQ}
  Concepts.ZeroMQ.Form,
  {$ENDIF}

  {$IFDEF ZOBJECTINSPECTOR}
  Concepts.zObjectInspector.Form,
  {$ENDIF}
  {$ENDREGION}

  {$IFDEF SYNAPSE}
  Concepts.Synapse.Serial.Form,
  {$ENDIF}

  {$IFDEF INDY}
  Concepts.Indy.TCP.Form,
  Concepts.Indy.Telnet.Form,
  {$ENDIF}

  {$IFDEF SYNEDIT}
  Concepts.SynEdit.Form,
  {$ENDIF}

  {$IFDEF VIRTUALTREES}
  Concepts.VirtualTreeView.Form,
  {$ENDIF}

  {$IFDEF KCONTROLS}
  Concepts.KControls.KMemo.Form,
  {$ENDIF}

  Concepts.SynMemoEx.Form,
  Concepts.MQTT.Form,

  Concepts.FMXContainer.Form,

  Concepts.Manager;

const
  SPRING_CATEGORY_COLOR     = $00DDFFDD;
  DSHARP_CATEGORY_COLOR     = $00B0FFFF;
  INDY_CATEGORY_COLOR       = $00C1D6FF;
  KCONTROLS_CATEGORY_COLOR  = $00FEFAC5;
  DEVEXPRESS_CATEGORY_COLOR = $00C1E0FF;
  SYSTEM_CATEGORY_COLOR     = $00E1E1FF;
  VCL_CATEGORY_COLOR        = $00FFD9D9;
  WINAPI_CATEGORY_COLOR     = $00C7E2E2;

procedure EnsureZMQLibExists;
const
  LIBZMQ = 'libzmq';
var
  LResStream  : TResourceStream;
  LFileStream : TFileStream;
  LPath       : string;
begin
  LPath := Format('%s\%s.dll', [ExtractFileDir(ParamStr(0)), LIBZMQ]);
  if not FileExists(LPath) then
  begin
    LResStream := TResourceStream.Create(HInstance, LIBZMQ, RT_RCDATA);
    try
      LFileStream := TFileStream.Create(LPath, fmCreate);
      try
        LFileStream.CopyFrom(LResStream, 0);
      finally
        LFileStream.Free;
      end;
    finally
      LResStream.Free;
    end;
  end;
end;

{$REGION 'private methods'}
class procedure TConcepts.RegisterSpringConcepts;
begin
  {$IFDEF SPRING}
  FCategoryColor := SPRING_CATEGORY_COLOR;
  ConceptManager.Register(
    TfrmCollections,
    'Collections',
    'Spring4D',
    'Spring4D collections',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmMultiCastEvents,
    'Multicast events',
    'Spring4D',
    'Multicast events (Event<T> - IEvent<T)',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmLazyInstantiation,
    'Lazy instantiation',
    'Spring4D',
    'Lazy instantiation (Lazy<T> - ILazy<T>)',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmSpringInterception,
    'Interception',
    'Spring4D',
    'Aspect Oriented Programming',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmObjectDataSet,
    'TObjectDataSet control',
    'Spring4D',
    'TObjectDataSet control',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmSpringPersistence,
    'ORM',
    'Spring4D',
    'Spring4D Entities',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmSpringLogging,
    'Logging',
    'Spring4D',
    'Spring4D Logging system',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmSpringTypes,
    'Types',
    'Spring4D',
    'Spring4D Utility types',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmSpringUtils,
    'Utils',
    'Spring4D',
    'Utillity classes and routines',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmClassProxy,
    'ClassProxy',
    'Spring4D',
    'Interception',
    FCategoryColor
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterDSharpConcepts;
begin
  {$IFDEF DSHARP}
  FCategoryColor := DSHARP_CATEGORY_COLOR;
  ConceptManager.Register(
    TfrmTreeViewPresenterList,
    'TreeViewPresenter list',
    'DSharp',
    'TreeViewPresenter list',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmTreeViewPresenterTree,
    'TreeViewPresenter tree',
    'DSharp',
    'TreeViewPresenter tree',
    FCategoryColor
  );
  {$IFDEF DEVEXPRESS}
  ConceptManager.Register(
    TfrmcxGridViewPresenter,
    'Presenters',
    'DSharp',
    'Specialized presenters',
    FCategoryColor
  );
  {$ENDIF}
  {$ENDIF}
end;

class procedure TConcepts.RegisterIndyConcepts;
begin
  {$IFDEF INDY}
  FCategoryColor := INDY_CATEGORY_COLOR;
  ConceptManager.Register(
    TfrmIndyTCP,
    'TCP',
    'Indy',
    'Indy TCP client',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmIndyTelnet,
    'Telnet',
    'Indy',
    'Indy Telnet client',
    FCategoryColor
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterKControlsConcepts;
begin
  {$IFDEF KCONTROLS}
  FCategoryColor := KCONTROLS_CATEGORY_COLOR;
  ConceptManager.Register(
    TfrmKMemo,
    'TKMemo control',
    'KControls',
    'Demonstrates the TKMemo control',
    FCategoryColor
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterDevExpressConcepts;
begin
  {$IFDEF DEVEXPRESS}
  FCategoryColor := DEVEXPRESS_CATEGORY_COLOR;
  ConceptManager.Register(
    TfrmcxEditors,
    'cxEditors',
    'DevExpress',
    'Demonstrates the DevExpress edit controls',
    FCategoryColor
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterSystemConcepts;
begin
  {$IFDEF SYSTEM}
  FCategoryColor := SYSTEM_CATEGORY_COLOR;
  ConceptManager.Register(
    TfrmLibraries,
    'Libraries',
    'System',
    'Demonstrates dynamic loading of libraries',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmRTTI,
    'RTTI',
    'System',
    'Demonstrates the extended RTTI',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmVariants,
    'Variants',
    'System',
    'Demonstrates some operations on Variants',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmLiveBindings,
    'LiveBindings',
    'System',
    'Demonstrates Delphi''s LiveBindings system',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmRegularExpressions,
    'Regular Expressions',
    'System',
    'Demonstrates support for regular expressions in the RTL',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmThreading,
    'Parallel Programming Library',
    'System',
    'Demonstrates the Parallel Programming Library (PPL) introduced in Delphi XE7',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmThreads,
    'Threads',
    'System',
    'Demonstrates some basic threading scenarios and primitives',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmAnonymousMethods,
    'Anonymous methods',
    'System',
    'Demonstrates anonymous methods and features like variable capturing',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmInterfaceImplementationByAggregation,
    'Interface implementation by aggregation',
    'System',
    'Demonstrates how an object can implement an interface by delegating ' +
    'the implementation to an object property',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmVirtualInterfaceDemo,
    'TVirtualInterface class',
    'System',
    'Demonstrated how a TVirtualInterface object can be created that implements ' +
    'any IInvokable ($M+) descendant at runtime',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmVirtualMethodInterceptor,
    'TVirtualMethodInterceptor class',
    'System',
    'TODO',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmStringList,
    'TStringList class',
    'System',
    'Demonstrates some TStringList features',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmPublishedFields,
    'Published fields',
    'System',
    'Demonstrates a form with components without published fields',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmInterfaces,
    'Object interfaces',
    'System',
    'Demonstrates how object interfaces work.',
    FCategoryColor
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterVclConcepts;
begin
  {$IFDEF VCL}
  FCategoryColor := VCL_CATEGORY_COLOR;
  ConceptManager.Register(
    TfrmGridPanel,
    'TGridPanel control',
    'Vcl',
    'Demonstrates the TGridPanel VCL control.',
    FCategoryColor
  );
  {$IFDEF DELPHIX_SEATTLE_UP}
  ConceptManager.Register(
    TfrmRelativePanel,
    'TRelativePanel control',
    'Vcl',
    'Demonstrates the TRelativePanel VCL control',
    FCategoryColor
  );
  {$ENDIF}
  {$ENDIF}
end;

class procedure TConcepts.RegisterWinApiConcepts;
begin
  {$IFDEF WINAPI}
  FCategoryColor := WINAPI_CATEGORY_COLOR;
  ConceptManager.Register(
    TfrmLockPaint,
    'LockPaint',
    'WinApi',
    'Demonstrates LockPaint/UnlockPaint routines',
    FCategoryColor
  );
  {$ENDIF}
end;
{$ENDREGION}

{$REGION 'public methods'}
class procedure TConcepts.RegisterConcepts;
begin
  {$IFDEF ZOBJECTINSPECTOR}
  ConceptManager.Register(
    TfrmzObjectInspector,
    'TzObjectInspector control',
    'TzObjectInspector',
    'TzObjectInspector control demo'
  );
  {$ENDIF}

  {$IFDEF DELPHIZMQ}
  ConceptManager.Register(
    TfrmZMQConcept,
    'ZeroMQ',
    'ZeroMQ',
    'ZeroMQ demo'
  );
  {$ENDIF}

  ConceptManager.Register(
    TfrmMQTTNode,
    'MQTT',
    'MQTT',
    'MQTT demo'
  );

  {$IFDEF DDETOURS}
  ConceptManager.Register(
    TfrmDDetours,
    'DDetours',
    'DDetours',
    'DDetours library demo'
  );
  {$ENDIF}

  {$IFDEF BCEDITOR}
  ConceptManager.Register(
    TfrmBCEditor,
    'TBCEditor control',
    'BCEditor',
    'TBCEditor control'
  );
  {$ENDIF}

  {$IFDEF SYNEDIT}
  ConceptManager.Register(
    TfrmSynEdit,
    'TSynEdit control',
    'SynEdit',
    'Demonstrates the TSynEdit control'
  );
  {$ENDIF}

  {$IFDEF BTMEMORYMODULE}
  ConceptManager.Register(
    TfrmBTMemoryModule,
    'BTMemoryModule',
    'BTMemoryModule',
    'Demonstrates how to load a DLL direct from memory.'
  );
  {$ENDIF}

  {$IFDEF SQLBUILDER4D}
  ConceptManager.Register(
    TfrmSQLBuilder4D,
    'SQLBuilder4D',
    'TfrmSQLBuilder4D',
    'Demonstrates the SQLBuilder4D library'
  );
  {$ENDIF}

  {$IFDEF CHROMETABS}
  ConceptManager.Register(
    TfrmChromeTabs,
    'TChromeTabs control',
    'TChromeTabs',
    'TChromeTabs control demo.'
  );
  {$ENDIF}

//  ConceptManager.Register(
//    TfrmRTTEye,
//    'RTTEye',
//    'System',
//    'Reflection-like overview using the extended RTTI.'
//  );

  {$IFDEF SYNAPSE}
  ConceptManager.Register(
    TfrmSynapseSerial,
    'Serial',
    'Synapse',
    'Serial communication with the Synapse library'
  );
  {$ENDIF}

  {$IFDEF FMXCONTAINER}
  ConceptManager.Register(
    TfrmFMXContainer,
    'TFireMonkeyContainer contol',
    'FMXContainer',
    'Demonstrates the Parnassus TFireMonkeyContainer control'
  );
  {$ENDIF}

  {$IFDEF VIRTUALTREES}
  ConceptManager.Register(
    TfrmVirtualTreeView,
    'Virtual treeview control',
    'TVirtualStringTree',
    'Demonstrates the TVirtualStringTree control'
  );
  {$ENDIF}

  ConceptManager.Register(
    TfrmSynMemoEx,
    'SynMemoEx control',
    'TSynMemoEx',
    'TSynMemoEx control'
  );

  RegisterSpringConcepts;
  RegisterDSharpConcepts;
  RegisterKControlsConcepts;
  RegisterIndyConcepts;
  RegisterDevExpressConcepts;
  RegisterSystemConcepts;
  RegisterVclConcepts;
  RegisterWinApiConcepts;
 end;
{$ENDREGION}

initialization
  EnsureZMQLibExists;

end.
