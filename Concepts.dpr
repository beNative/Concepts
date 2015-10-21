program Concepts;

uses
  Forms,
  PasZip in 'Libraries\PasZip\PasZip.pas',
  AsyncCalls in 'Libraries\AsyncCalls\AsyncCalls.pas',
  gnugettext in 'Libraries\gnugettext\gnugettext.pas',
  BTMemoryModule in 'Libraries\BTMemoryModule\BTMemoryModule.pas',
  Vcl.Themes,
  Vcl.Styles,
  Concepts.Registration in 'Concepts.Registration.pas',
  Concepts.Utils in 'Concepts.Utils.pas',
  Concepts.Types.ValidationRules in 'Types\Concepts.Types.ValidationRules.pas',
  Concepts.Types.Contact in 'Types\Concepts.Types.Contact.pas',
  Concepts.Manager in 'Concepts.Manager.pas',
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
  Concepts.System.Libraries.Form in 'Forms\Concepts.System.Libraries.Form.pas' {frmLibraries},
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
  Concepts.TestForm in 'Concepts.TestForm.pas' {frmTest},
  Concepts.System.AnonymousMethods.Form in 'Forms\Concepts.System.AnonymousMethods.Form.pas' {frmAnonymousMethods},
  Concepts.System.InterfaceImplementationByAggregation.Form in 'Forms\Concepts.System.InterfaceImplementationByAggregation.Form.pas' {frmIterFaceImplementationByAggregation},
  Concepts.Spring.Interception.Form in 'Forms\Concepts.Spring.Interception.Form.pas' {frmSpringInterception},
  Concepts.System.VirtualMethodInterceptor.Form in 'Forms\Concepts.System.VirtualMethodInterceptor.Form.pas' {frmVirtualMethodInterceptor},
  Concepts.System.VirtualInterface.Form in 'Forms\Concepts.System.VirtualInterface.Form.pas' {frmVirtualInterfaceDemo},
  Concepts.System.InterfacedComponent.Form in 'Forms\Concepts.System.InterfacedComponent.Form.pas' {frmInterfacedComponent},
  Concepts.DSharp.Bindings.Form in 'Forms\Concepts.DSharp.Bindings.Form.pas' {frmBindings},
  Concepts.Spring.Logging.Form in 'Forms\Concepts.Spring.Logging.Form.pas' {frmSpringLogging},
  Concepts.Spring.Utils.Form in 'Forms\Concepts.Spring.Utils.Form.pas' {frmSpringUtils},
  Concepts.SQLBuilder4D.Form in 'Forms\Concepts.SQLBuilder4D.Form.pas' {frmSQLBuilder4D},
  Concepts.ChromeTabs.Form in 'Forms\Concepts.ChromeTabs.Form.pas' {frmChromeTabs},
  Concepts.ComponentInspectorTemplate.Form in 'Forms\Concepts.ComponentInspectorTemplate.Form.pas' {frmPropertyInspector},
  Concepts.Spring.ObjectDataSet.Form in 'Forms\Concepts.Spring.ObjectDataSet.Form.pas' {frmSpringObjectDataSet},
  Concepts.Resources in 'Concepts.Resources.pas' {dmResources: TDataModule},
  Concepts.Spring.Types.Form in 'Forms\Concepts.Spring.Types.Form.pas' {frmSpringTypes},
  DSharp.Bindings.Collections in 'Libraries\DSharp\DSharp.Bindings.Collections.pas',
  DSharp.Bindings.CollectionView.Adapters in 'Libraries\DSharp\DSharp.Bindings.CollectionView.Adapters.pas',
  DSharp.Bindings.CollectionView in 'Libraries\DSharp\DSharp.Bindings.CollectionView.pas',
  DSharp.Bindings.CollectionView.VCLAdapters in 'Libraries\DSharp\DSharp.Bindings.CollectionView.VCLAdapters.pas',
  DSharp.Bindings.Exceptions in 'Libraries\DSharp\DSharp.Bindings.Exceptions.pas',
  DSharp.Bindings.Notifications in 'Libraries\DSharp\DSharp.Bindings.Notifications.pas',
  DSharp.Bindings in 'Libraries\DSharp\DSharp.Bindings.pas',
  DSharp.Bindings.Validations in 'Libraries\DSharp\DSharp.Bindings.Validations.pas',
  DSharp.Bindings.VCLControls.Extensions in 'Libraries\DSharp\DSharp.Bindings.VCLControls.Extensions.pas',
  DSharp.Bindings.VCLControls in 'Libraries\DSharp\DSharp.Bindings.VCLControls.pas',
  DSharp.Core.Collections in 'Libraries\DSharp\DSharp.Core.Collections.pas',
  DSharp.Core.CopyOperator in 'Libraries\DSharp\DSharp.Core.CopyOperator.pas',
  DSharp.Core.DataConversion in 'Libraries\DSharp\DSharp.Core.DataConversion.pas',
  DSharp.Core.DataTemplates.Default in 'Libraries\DSharp\DSharp.Core.DataTemplates.Default.pas',
  DSharp.Core.DataTemplates in 'Libraries\DSharp\DSharp.Core.DataTemplates.pas',
  DSharp.Core.DependencyProperty in 'Libraries\DSharp\DSharp.Core.DependencyProperty.pas',
  DSharp.Core.Editable in 'Libraries\DSharp\DSharp.Core.Editable.pas',
  DSharp.Core.Expressions in 'Libraries\DSharp\DSharp.Core.Expressions.pas',
  DSharp.Core.Framework in 'Libraries\DSharp\DSharp.Core.Framework.pas',
  DSharp.Core.Properties in 'Libraries\DSharp\DSharp.Core.Properties.pas',
  DSharp.Core.PropertyChangedBase in 'Libraries\DSharp\DSharp.Core.PropertyChangedBase.pas',
  DSharp.Core.Reflection in 'Libraries\DSharp\DSharp.Core.Reflection.pas',
  DSharp.Core.Utils in 'Libraries\DSharp\DSharp.Core.Utils.pas',
  DSharp.Core.Validations in 'Libraries\DSharp\DSharp.Core.Validations.pas',
  DSharp.Windows.ColumnDefinitions.ControlTemplate in 'Libraries\DSharp\DSharp.Windows.ColumnDefinitions.ControlTemplate.pas',
  DSharp.Windows.ColumnDefinitions in 'Libraries\DSharp\DSharp.Windows.ColumnDefinitions.pas',
  DSharp.Windows.ColumnDefinitions.RttiDataTemplate in 'Libraries\DSharp\DSharp.Windows.ColumnDefinitions.RttiDataTemplate.pas',
  DSharp.Windows.ColumnDefinitions.XmlDataTemplate in 'Libraries\DSharp\DSharp.Windows.ColumnDefinitions.XmlDataTemplate.pas',
  DSharp.Windows.ControlTemplates in 'Libraries\DSharp\DSharp.Windows.ControlTemplates.pas',
  DSharp.Windows.CustomPresenter in 'Libraries\DSharp\DSharp.Windows.CustomPresenter.pas',
  DSharp.Windows.CustomPresenter.Types in 'Libraries\DSharp\DSharp.Windows.CustomPresenter.Types.pas',
  DSharp.Windows.TreeViewPresenter in 'Libraries\DSharp\DSharp.Windows.TreeViewPresenter.pas',
  DSharp.ComponentModel.DataAnnotations in 'Libraries\DSharp\DSharp.ComponentModel.DataAnnotations.pas',
  DSharp.Core.XNode in 'Libraries\DSharp\DSharp.Core.XNode.pas',
  DSharp.Collections.ObservableCollection in 'Libraries\DSharp\DSharp.Collections.ObservableCollection.pas',
  DSharp.DevExpress.GridViewPresenter in 'Libraries\DSharp\DSharp.DevExpress.GridViewPresenter.pas',
  DSharp.DevExpress.PresenterDataSource in 'Libraries\DSharp\DSharp.DevExpress.PresenterDataSource.pas',
  DSharp.DevExpress.TreeListPresenter in 'Libraries\DSharp\DSharp.DevExpress.TreeListPresenter.pas',
  Concepts.DevExpress.cxGridViewPresenter.Form in 'Forms\cx\Concepts.DevExpress.cxGridViewPresenter.Form.pas' {frmcxGridViewPresenter};

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
  Application.CreateForm(TdmResources, dmResources);
  end;
  Application.Run;
end.

