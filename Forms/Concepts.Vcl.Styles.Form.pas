unit Concepts.Vcl.Styles.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    btnactCnPrefixWizard: TButton;
    lv1: TListView;
    lbl1: TLabel;
    pnl1: TPanel;
    actmgr1: TActionManager;
    actApplyStyle: TAction;
  private

  protected


  public
    procedure AfterConstruction; override;
    destructor Destroy; override;


  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$REGION 'MyRegion'}
procedure TForm1.AfterConstruction;
begin
  inherited AfterConstruction;
//
end;

destructor TForm1.Destroy;
begin

  inherited Destroy;
end;
{$ENDREGION}

end.
