unit Spring.Tests.Persistence.FmxRefForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.UI.Intf, FireDAC.FMXUI.Wait, FireDAC.Stan.Intf, FireDAC.Comp.UI;

type
  /// <summary>
  ///   Form that only acts as a reference for needed FireDAC components.
  /// </summary>
  TPersistenceFmxRefForm = class(TForm)
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PersistenceFmxRefForm: TPersistenceFmxRefForm;

implementation

{$R *.fmx}

end.
