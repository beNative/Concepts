program Concepts;


//{$RTTI EXPLICIT
//  METHODS([vcPrivate,vcProtected, vcPublic, vcPublished])
//  PROPERTIES([vcPrivate,vcProtected, vcPublic, vcPublished])
//  FIELDS([vcPrivate,vcProtected, vcPublic, vcPublished])
//}
//{$STRONGLINKTYPES ON}

uses
  Forms,
  Concepts.Registration in 'Concepts.Registration.pas',
  Concepts.Utils in 'Concepts.Utils.pas',
  Concepts.Types.ValidationRules in 'Types\Concepts.Types.ValidationRules.pas',
  Concepts.Types.Contact in 'Types\Concepts.Types.Contact.pas',
  Concepts.Manager in 'Concepts.Manager.pas',
  AsyncCalls in 'Libraries\AsyncCalls.pas',
  Concepts.System.Variants.Form in 'Forms\Concepts.System.Variants.Form.pas' {frmVariants},
  Concepts.DSharp.TreeViewPresenter.Form in 'Forms\Concepts.DSharp.TreeViewPresenter.Form.pas' {frmTreeViewPresenter},
  Concepts.System.RTTI.Form in 'Forms\Concepts.System.RTTI.Form.pas' {frmRTTI},
  Concepts.Spring.MultiCastEvents.Form in 'Forms\Concepts.Spring.MultiCastEvents.Form.pas' {frmMulticastEvents},
  Concepts.MainForm in 'Concepts.MainForm.pas' {frmMain},
  Concepts.WinApi.LockPaint.Form in 'Forms\Concepts.WinApi.LockPaint.Form.pas' {frmLockPaint},
  Concepts.Vcl.GridPanels.Form in 'Forms\Concepts.Vcl.GridPanels.Form.pas' {frmGridPanels},
  Concepts.AsyncCalls.Form in 'Forms\Concepts.AsyncCalls.Form.pas' {frmAsyncCalls},
  Concepts.Spring.Collections.Form in 'Forms\Concepts.Spring.Collections.Form.pas' {frmCollections},
  Concepts.DevExpress.cxEditors.Form in 'Forms\cx\Concepts.DevExpress.cxEditors.Form.pas' {frmcxEditors},
  Concepts.DevExpress.cxDragAndDrop.Form in 'Forms\cx\Concepts.DevExpress.cxDragAndDrop.Form.pas' {frmcxDragAndDrop},
  Concepts.BTMemoryModule.Form in 'Forms\Concepts.BTMemoryModule.Form.pas' {frmBTMemoryModule},
  BTMemoryModule in 'Libraries\BTMemoryModule.pas',
  Concepts.System.Libraries.Form in 'Forms\Concepts.System.Libraries.Form.pas' {frmLibraries},
  Concepts.DSharp.VSTPresenter.Form in 'Forms\Concepts.DSharp.VSTPresenter.Form.pas' {frmVSTPresenter},
  Concepts.Spring.MultiCastEvents.Data in 'Forms\Concepts.Spring.MultiCastEvents.Data.pas',
  Concepts.RTTEye.Form in 'Forms\Concepts.RTTEye.Form.pas' {frmRTTEye},
  Concepts.System.LiveBindings.Form in 'Forms\Concepts.System.LiveBindings.Form.pas' {frmLiveBindings},
  Concepts.Spring.LazyInstantiation.Form in 'Forms\Concepts.Spring.LazyInstantiation.Form.pas' {frmLazyInstantiation},
  Concepts.System.Threads.Form in 'Forms\Concepts.System.Threads.Form.pas' {frmThreads},
  Concepts.RTTEye.Templates in 'Types\Concepts.RTTEye.Templates.pas',
  Concepts.Factories in 'Concepts.Factories.pas',
  Concepts.Helpers in 'Concepts.Helpers.pas',
  Concepts.Types.ThreadSafe in 'Types\Concepts.Types.ThreadSafe.pas',
  Concepts.System.RegularExpressions.Form in 'Forms\Concepts.System.RegularExpressions.Form.pas' {frmRegularExpressions},
  Concepts.System.Threading.Form in 'Forms\Concepts.System.Threading.Form.pas' {frmThreading},
  System.Translator in 'System.Translator.pas',
  System.InterfacedComponent in 'System.InterfacedComponent.pas',
  Concepts.TestForm in 'Concepts.TestForm.pas' {frmTest},
  Concepts.System.AnonymousMethods.Form in 'Forms\Concepts.System.AnonymousMethods.Form.pas' {frmAnonymousMethods},
  Concepts.System.InterfaceImplementationByAggregation.Form in 'Forms\Concepts.System.InterfaceImplementationByAggregation.Form.pas' {frmIterFaceImplementationByAggregation},
  Concepts.Spring.Interception.Form in 'Forms\Concepts.Spring.Interception.Form.pas' {frmSpringInterception},
  Concepts.System.VirtualMethodInterceptor.Form in 'Forms\Concepts.System.VirtualMethodInterceptor.Form.pas' {frmVirtualMethodInterceptor},
  Concepts.System.VirtualInterface.Form in 'Forms\Concepts.System.VirtualInterface.Form.pas' {frmVirtualInterfaceDemo},
  Concepts.System.InterfacedComponent.Form in 'Forms\Concepts.System.InterfacedComponent.Form.pas' {frmInterfacedComponent},
  PasZip in 'Libraries\PasZip.pas',
  gnugettext in 'Libraries\gnugettext.pas',
  Concepts.DSharp.Bindings.Form in 'Forms\Concepts.DSharp.Bindings.Form.pas' {frmBindings},
  Concepts.Spring.ObjectDataSet.Form in 'Forms\Concepts.Spring.ObjectDataSet.Form.pas' {frmSpringObjectDataSet},
  Concepts.Spring.Logging.Form in 'Forms\Concepts.Spring.Logging.Form.pas' {frmSpringLogging},
  Concepts.Spring.Utils.Form in 'Forms\Concepts.Spring.Utils.Form.pas' {frmSpringUtils};

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  RegisterConcepts;
  if ConceptManager.ItemList.Count = 1 then
  begin
    ConceptManager.Execute(ConceptManager.ItemList.First);
  end
  else
  begin
    Application.Title := 'Concepts';
    Application.CreateForm(TfrmMain, frmMain);
  end;
  Application.Run;
end.

