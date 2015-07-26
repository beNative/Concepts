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

procedure RegisterConcepts;

implementation

uses
  System.SysUtils, System.StrUtils,

  {$IFDEF DEVEXPRESS}
  Concepts.DevExpress.cxEditors.Form,
  Concepts.DevExpress.cxDragAndDrop.Form,
    //Concepts.DevExpress.cxGridViewPresenter.Form,
  {$ENDIF}

  {$IFDEF DSHARP}
  Concepts.DSharp.TreeViewPresenter.Form,
  Concepts.DSharp.Bindings.Form,
  Concepts.DSharp.VSTPresenter.Form,
  {$ENDIF}

  {$IFDEF SPRING}
  Concepts.Spring.Interception.Form,
  Concepts.Spring.Collections.Form,
  Concepts.Spring.LazyInstantiation.Form,
  Concepts.Spring.MulticastEvents.Form,
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
  Concepts.System.InterfacedComponent.Form,
  {$ENDIF}

  Concepts.Manager;

procedure RegisterConcepts;
begin
//  ConceptManager.Register(TfrmcxGridViewPresenter, 'cxGridViewPresenter');

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
    TfrmVSTPresenter,
    'VSTPresenter',
    'DSharp',
    'DSharp TVirtualStringTreePresenter'
  );
{$ENDIF}
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
{$ENDIF}
{$IFDEF DEVEXPRESS}
  ConceptManager.Register(
    TfrmcxEditors,
    'cxEditors',
    'DevExpress',
    'Demonstrates the DevExpress edit controls'
  );
  ConceptManager.Register(
    TfrmcxDragAndDrop,
    'Drag and drop',
    'DevExpress',
    ''
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
{$IFDEF ASYNCCALLS}
  ConceptManager.Register(
    TfrmAsyncCalls,
    'AsyncCalls',
    'AsyncCalls',
    'Demonstrates how to use AsyncCalls.pas'
  );
{$ENDIF}
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
    'Demonstrates the new System.Threading library.'
  );
  ConceptManager.Register(
    TfrmThreads,
    'Threads',
    'System',
    'Demonstrates some basic threading scenarios and primitives.'
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
{$ENDIF}
 {
    ConceptManager.Register(
    TfrmInterfacedComponent,
    'InterfacedComponent trick',
    'System',
    'Demonstrates how TComponent descendents can be reference counted'
  ); }
end;

end.

