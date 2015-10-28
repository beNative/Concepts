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

{$I Concepts.inc}

unit Concepts.Registration;

interface

uses
  Vcl.Graphics;

type
  TConcepts = record
  strict private
    class var FCategoryColor : TColor;

    class procedure RegisterSpringConcepts; static;
    class procedure RegisterDSharpConcepts; static;
    class procedure RegisterDevExpressConcepts; static;
    class procedure RegisterSystemConcepts; static;
    class procedure RegisterVclConcepts; static;
    class procedure RegisterWinApiConcepts; static;
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
  {$IFDEF DELPHIXE7UP}
  Concepts.System.Threading.Form,
  {$ENDIF}
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
  FCategoryColor := $00DDFFDD;
  ConceptManager.Register(
    TfrmCollections,
    'Collections',
    'Spring',
    'Spring collections demo',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmMultiCastEvents,
    'Multicast events',
    'Spring',
    'Spring multicast events',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmLazyInstantiation,
    'Lazy instantiation',
    'Spring',
    'Spring lazy instantiation demo',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmSpringInterception,
    'Interception',
    'Spring',
    'Aspect Oriented Programming',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmObjectDataSet,
    'Persistence',
    'Spring',
    'TObjectDataSet',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmSpringLogging,
    'Logging',
    'Spring',
    'Spring Logging system',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmSpringTypes,
    'Types',
    'Spring',
    'Spring types',
    FCategoryColor
  );
//  ConceptManager.Register(
//    TfrmSpringUtils,
//    'Utils',
//    'Spring',
//    'Utillity classes and routines',
//    FCategoryColor
//  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterDSharpConcepts;
begin
  {$IFDEF DSHARP}
  FCategoryColor := $00B0FFFF;
  ConceptManager.Register(
    TfrmBindings,
    'Bindings',
    'DSharp',
    'DSharp bindings',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmTreeViewPresenter,
    'TreeViewPresenter',
    'DSharp',
    'DSharp TreeViewPresenter',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmcxGridViewPresenter,
    'Presenters',
    'DSharp',
    'Specialized presenters',
    FCategoryColor
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterDevExpressConcepts;
begin
  {$IFDEF DEVEXPRESS}
  FCategoryColor := $00C1E0FF;
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
  FCategoryColor := $00E1E1FF;
  ConceptManager.Register(
    TfrmLibraries,
    'Libraries',
    'System',
    '',
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
  {$IFDEF DELPHIXE7UP}
  ConceptManager.Register(
    TfrmThreading,
    'Parallel library',
    'System',
    'Demonstrates the System.Threading library introduced in Delphi XE7',
    FCategoryColor
  );
  {$ENDIF}
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
    'Anonymous methods - variable capturing',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmInterfaceImplementationByAggregation,
    'Interface implementation by aggregation',
    'System',
    'using the ''implements'' keyword',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmVirtualInterfaceDemo,
    'TVirtualInterface demo',
    'System',
    '',
    FCategoryColor
  );
  ConceptManager.Register(
    TfrmVirtualMethodInterceptor,
    'TVirtualMethodInterceptor demo',
    'System',
    '',
    FCategoryColor
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterVclConcepts;
begin
  {$IFDEF VCL}
  FCategoryColor := $00FFD9D9;
  ConceptManager.Register(
    TfrmGridPanels,
    'Grid panels',
    'Vcl',
    'Demonstrates TGridPanel component',
    FCategoryColor
  );
  {$ENDIF}
end;

class procedure TConcepts.RegisterWinApiConcepts;
begin
  {$IFDEF WINAPI}
  ConceptManager.Register(
    TfrmLockPaint,
    'LockPaint',
    'Vcl',
    'Demonstrates LockPaint/UnlockPaint routines',
    FCategoryColor
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
  RegisterWinApiConcepts;

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

  {$IFDEF RTTEYE}
  ConceptManager.Register(
    TfrmRTTEye,
    'RTTEye',
    'System',
    'Reflection-like overview using the enhanced RTTI'
  );
  {$ENDIF}
 end;
{$ENDREGION}

end.
