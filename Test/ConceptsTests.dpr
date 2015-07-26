program ConceptsTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  ExceptionLog,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  CIM.Entity.WorkstationDevice in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.WorkstationDevice.pas',
  CIM.Entity.Workstation in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Workstation.pas',
  CIM.Entity.Workstation.Lists in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Workstation.Lists.pas',
  CIM.Entity.WorkcenterType in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.WorkcenterType.pas',
  CIM.Entity.Workcenter in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Workcenter.pas',
  CIM.Entity.Workcenter.Lists in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Workcenter.Lists.pas',
  CIM.Entity.Verfbad in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Verfbad.pas',
  CIM.Entity.UserRole in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.UserRole.pas',
  CIM.Entity.UserLanguage in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.UserLanguage.pas',
  CIM.Entity.UserActiviteitCode in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.UserActiviteitCode.pas',
  CIM.Entity.UserActiviteit in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.UserActiviteit.pas',
  CIM.Entity.User in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.User.pas',
  CIM.Entity.User.Lists in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.User.Lists.pas',
  CIM.Entity.TuftMROLNummer in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.TuftMROLNummer.pas',
  CIM.Entity.TuftMachine in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.TuftMachine.pas',
  CIM.Entity.ToestandCodeType in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.ToestandCodeType.pas',
  CIM.Entity.ToestandCode in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.ToestandCode.pas',
  CIM.Entity.TFDefinition in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.TFDefinition.pas',
  CIM.Entity.Stilstand in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Stilstand.pas',
  CIM.Entity.Role in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Role.pas',
  CIM.Entity.Role.Lists in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Role.Lists.pas',
  CIM.Entity.ProductieStap in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.ProductieStap.pas',
  CIM.Entity.Param in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Param.pas',
  CIM.Entity.OrderOperation in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.OrderOperation.pas',
  CIM.Entity.OrderItem in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.OrderItem.pas',
  CIM.Entity.OrderComponent in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.OrderComponent.pas',
  CIM.Entity.Order in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Order.pas',
  CIM.Entity.Order.Lists in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Order.Lists.pas',
  CIM.Entity.Nummer in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Nummer.pas',
  CIM.Entity.MachineToestand in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.MachineToestand.pas',
  CIM.Entity.MachineToestand.Lists in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.MachineToestand.Lists.pas',
  CIM.Entity.MachineTellerType in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.MachineTellerType.pas',
  CIM.Entity.MachineTeller in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.MachineTeller.pas',
  CIM.Entity.MachineStilstandSettings in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.MachineStilstandSettings.pas',
  CIM.Entity.Machinebezetting in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Machinebezetting.pas',
  CIM.Entity.LabelFolder in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.LabelFolder.pas',
  CIM.Entity.Label_ in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Label_.pas',
  CIM.Entity.Kwaliteit in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Kwaliteit.pas',
  CIM.Entity.KreelOperatieSoort in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.KreelOperatieSoort.pas',
  CIM.Entity.KreelOperatie in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.KreelOperatie.pas',
  CIM.Entity.KostenPlaats in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.KostenPlaats.pas',
  CIM.Entity.HandlingUnitType in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.HandlingUnitType.pas',
  CIM.Entity.HandlingUnitAfmelding in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.HandlingUnitAfmelding.pas',
  CIM.Entity.HandlingUnit in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.HandlingUnit.pas',
  CIM.Entity.HandlingUnit.Lists in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.HandlingUnit.Lists.pas',
  CIM.Entity.ControleWaarde in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.ControleWaarde.pas',
  CIM.Entity.ControleTijdstip in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.ControleTijdstip.pas',
  CIM.Entity.ControlePlan in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.ControlePlan.pas',
  CIM.Entity.ControleCode in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.ControleCode.pas',
  CIM.Entity.Controle in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Controle.pas',
  CIM.Entity.CommentType in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.CommentType.pas',
  CIM.Entity.Comment in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Comment.pas',
  CIM.Entity.Code in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Code.pas',
  CIM.Entity.BatchVerbruik in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.BatchVerbruik.pas',
  CIM.Entity.BatchRegistratie in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.BatchRegistratie.pas',
  CIM.Entity.BatchHandlingUnit in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.BatchHandlingUnit.pas',
  CIM.Entity.BatchAfmelding in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.BatchAfmelding.pas',
  CIM.Entity.Batch in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Batch.pas',
  CIM.Entity.ArtikelPlant in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.ArtikelPlant.pas',
  CIM.Entity.Artikel in '..\..\..\..\..\Sources\CIM\Entity\CIM.Entity.Artikel.pas',
  CIMTools.Tests.Entity in '..\..\CIMTools\CIMTools.Tests.Entity.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

