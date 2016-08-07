unit FMXForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Effects, FMX.Colors, FMX.TabControl, FMX.Menus,
  FMX.Filter.Effects, FMX.Edit, FMX.Controls.Presentation, FMX.ExtCtrls,
  FMX.EditBox, FMX.SpinBox;

type
  TFireMonkeyForm = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    Button2: TButton;
    Edit1: TEdit;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    AlphaTrackBar1: TAlphaTrackBar;
    TabItem2: TTabItem;
    Switch1: TSwitch;
    trb1: TTrackBar;
    spnbx1: TSpinBox;
    pltgrd1: TPlotGrid;
    procedure ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  FireMonkeyForm: TFireMonkeyForm;

implementation

{$R *.fmx}

procedure TFireMonkeyForm.ButtonClick(Sender: TObject);
begin
  MessageDlg('Hello from ' + (Sender as TComponent).Name, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

end.
