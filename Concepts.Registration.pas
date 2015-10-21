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

unit Concepts.Registration;

interface

{$I Concepts.inc}

type
  TConcepts = record
  strict private
    class procedure RegisterSpringConcepts; static;
    class procedure RegisterDSharpConcepts; static;
    class procedure RegisterDevExpressConcepts; static;
    class procedure RegisterSystemConcepts; static;
    class procedure RegisterVclConcepts; static;
  public
    class procedure RegisterConcepts; static;
  end;

implementation

uses
  System.SysUtils,

  {$REGION 'Concept form units'}
  {$IFDEF DEVEXPRESS}
  Concepts.DevExpress.cxEditors.Form,
  Concepts.DevExpress.cxGridViewPresenter.Form,
  {$ENDIF}

  {$IFDEF DSHARP}
  Concepts.DSharp.TreeViewPresenter.Form,
  Concepts.DSharp.Bindings.Form,
  {$ENDIF}

  {$IFDEF SPRING}
  Concepts.Spring.Interception.Form,
  Concepts.Spring.Collections.Form,
  Concepts.Spring.LazyInstantiation.Form,
  Concepts.Spring.MulticastEvents.Form,
  Concepts.Spring.ObjectDataSet.Form,
  Concepts.Spring.Logging.Form,
  Concepts.Spring.Types.Form,
  {$ENDIF}

  {$IFDEF ASYNCCALLS}
  Concepts.AsyncCalls.Form,
  {$ENDIF}

  {$IFDEF BTMEMORYMODULE}
  Concepts.BTMemoryModule.Form,
  {$ENDIF}

  {$IFDEF RTTEYE}
  Concepts.RTTEye.Form,
  {$ENDIF}

  {$IFDEF WINAPI}
  Concepts.Winapi.LockPaint.Form,
  {$ENDIF}

  {$IFDEF VCL}
  Concepts.Vcl.GridPanels.Form,
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
  Concepts.System.VirtualMethodInterceptor.Form,
  Concepts.System.VirtualInterface.Form,
  {$ENDIF}

  {$IFDEF SQLBUILDER4D}
  Concepts.SQLBuilder4D.Form,
  {$ENDIF}

  {$IFDEF CHROMETABS}
  Concepts.ChromeTabs.Form,
  {$ENDIF}
  {$ENDREGION}

  Concepts.Manager;

{$REGION 'private methods'}
class procedure TConcepts.RegisterSpringConcepts;
begin
  {$IFDEF SPRING}
  ConceptManager.Register(
    TfrmCollections,
    'Collections',
    'Spring',
    'Spring collections demo'
  );
  ConceptManager.Register(
    TfrmMultiCastEvents,
    'Multicast events',
    'Spring',
    'Spring multicast events'
  );
  ConceptManager.Register(
    TfrmLazyInstantiation,
    'Lazy instantiation',
    'Spring',
    'Spring lazy instantiation demo'
  );
  ConceptManager.Register(
    TfrmSpringInterception,
    'Interception',
    'Spring',
    'Aspect Oriented Programming'
  );
//  ConceptManager.Register(
//    TfrmSpringObjectDataSet,
//    'Persistence',
//    'Spring',
//    'TObjectDataSet'
//  );
  ConceptManager.Register(
    TfrmSpringLogging,
    'Logging',
    'Spring',
    'Spring Logging system'
  );
  ConceptManager.Register(
    TfrmSpringTypes,
    'Types',
    'Spring',
    'Spring types'
  );
//  ConceptManager.Register(
//    TfrmSpringUtils,
//    'Utils',
//    'Spring',
//    'Utillity classes and routines'
//  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterDSharpConcepts;
begin
  {$IFDEF DSHARP}
  ConceptManager.Register(
    TfrmBindings,
    'Bindings',
    'DSharp',
    'DSharp bindings'
  );
  ConceptManager.Register(
    TfrmTreeViewPresenter,
    'TreeViewPresenter',
    'DSharp',
    'DSharp TreeViewPresenter'
  );
  ConceptManager.Register(
    TfrmcxGridViewPresenter,
    'Presenters',
    'DSharp',
    'Specialized presenters'
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterDevExpressConcepts;
begin
  {$IFDEF DEVEXPRESS}
  ConceptManager.Register(
    TfrmcxEditors,
    'cxEditors',
    'DevExpress',
    'Demonstrates the DevExpress edit controls'
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterSystemConcepts;
begin
  {$IFDEF SYSTEM}
  ConceptManager.Register(
    TfrmLibraries,
    'Libraries',
    'System',
    ''
  );
  ConceptManager.Register(
    TfrmRTTI,
    'RTTI',
    'System',
    'Demonstrates the extended RTTI'
  );
  ConceptManager.Register(
    TfrmVariants,
    'Variants',
    'System',
    'Demonstrates some operations on Variants'
  );
  ConceptManager.Register(
    TfrmRTTEye,
    'RTTEye',
    'System',
    'Reflection-like overview using the enhanced RTTI'
  );
  ConceptManager.Register(
    TfrmLiveBindings,
    'LiveBindings',
    'System',
    'Demonstrates Delphi''s LiveBindings system'
  );
  ConceptManager.Register(
    TfrmRegularExpressions,
    'Regular Expressions',
    'System',
    'Demonstrates support for regular expressions in the RTL'
  );
  ConceptManager.Register(
    TfrmThreading,
    'Parallel library',
    'System',
    'Demonstrates the System.Threading library introduced in Delphi XE7'
  );
  ConceptManager.Register(
    TfrmThreads,
    'Threads',
    'System',
    'Demonstrates some basic threading scenarios and primitives'
  );
  ConceptManager.Register(
    TfrmAnonymousMethods,
    'Anonymous methods',
    'System',
    'Anonymous methods - variable capturing'
  );
  ConceptManager.Register(
    TfrmInterfaceImplementationByAggregation,
    'Interface implementation by aggregation',
    'System',
    'using the ''implements'' keyword'
  );
  ConceptManager.Register(
    TfrmVirtualInterfaceDemo,
    'TVirtualInterface demo',
    'System',
    ''
  );
  ConceptManager.Register(
    TfrmVirtualMethodInterceptor,
    'TVirtualMethodInterceptor demo',
    'System',
    ''
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterVclConcepts;
begin
  {$IFDEF VCL}
  ConceptManager.Register(
    TfrmGridPanels,
    'Grid panels',
    'Vcl',
    'Demonstrates TGridPanel component'
  );
  ConceptManager.Register(
    TfrmLockPaint,
    'LockPaint',
    'Vcl',
    'Demonstrates LockPaint/UnlockPaint routines'
  );
  {$ENDIF}
end;
{$ENDREGION}

{$REGION 'public methods'}
class procedure TConcepts.RegisterConcepts;
begin
  RegisterSpringConcepts;
  RegisterDSharpConcepts;
  RegisterDevExpressConcepts;
  RegisterSystemConcepts;
  RegisterVclConcepts;

  {$IFDEF BTMEMORYMODULE}
  ConceptManager.Register(
    TfrmBTMemoryModule,
    'BTMemoryModule',
    'BTMemoryModule',
    'Demonstrates how to load a DLL direct from memory.'
  );
  {$ENDIF}

  {$IFDEF ASYNCCALLS}
  ConceptManager.Register(
    TfrmAsyncCalls,
    'AsyncCalls',
    'AsyncCalls',
    'Demonstrates how to use AsyncCalls.pas'
  );
  {$ENDIF}

  {$IFDEF SQLBUILDER4D}
  ConceptManager.Register(
    TfrmSQLBuilder4D,
    'TfrmSQLBuilder4D demo',
    'TfrmSQLBuilder4D',
    ''
  );
  {$ENDIF}

  {$IFDEF CHROMETABS}
  ConceptManager.Register(
    TfrmChromeTabs,
    'ChromeTabs',
    'TChromeTabs',
    'TChromeTabs demo'
  );
  {$ENDIF}
 {
    ConceptManager.Register(
    TfrmInterfacedComponent,
    'InterfacedComponent trick',
    'System',
    'Demonstrates how TComponent descendents can be reference counted'
  ); }
end;
{$ENDREGION}

end.
