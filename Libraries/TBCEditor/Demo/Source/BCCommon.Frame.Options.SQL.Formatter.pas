unit BCCommon.Frame.Options.SQL.Formatter;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, BCCommon.Options.Container.SQL.Formatter,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BCControl.ComboBox, BCCommon.Frame.Options.Base, sComboBox, BCControl.Panel, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsSQLFormatterFrame = class(TBCOptionsBaseFrame)
    ComboBoxDatabase: TBCComboBox;
    Panel: TBCPanel;
  protected
    procedure GetData; override;
    procedure Init; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsSQLFormatterFrame(AOwner: TComponent): TOptionsSQLFormatterFrame;

implementation

{$R *.dfm}



var
  FOptionsSQLFormatterFrame: TOptionsSQLFormatterFrame;

function OptionsSQLFormatterFrame(AOwner: TComponent): TOptionsSQLFormatterFrame;
begin
  if not Assigned(FOptionsSQLFormatterFrame) then
    FOptionsSQLFormatterFrame := TOptionsSQLFormatterFrame.Create(AOwner);
  Result := FOptionsSQLFormatterFrame;
end;

destructor TOptionsSQLFormatterFrame.Destroy;
begin
  inherited;
  FOptionsSQLFormatterFrame := nil;
end;

procedure TOptionsSQLFormatterFrame.Init;
begin
  { 0 = MSSql; 1 = Oracle; 2 = MySQL; 3 = Access; 4 = Generic; 5 = DB2; 6 = Sybase; 7 = Informix; 8 = PostgreSQL;
    9 = Firebird; 10 = Mdx }
  with ComboBoxDatabase.Items do
  begin
    Add('MSSQL');
    Add('Oracle');
    Add('MySQL');
    Add('MSAccess');
    Add('Generic');
    Add('DB2');
    //Add('Sybase');
    //Add('Informix');
    //Add('PostgreSQL');
    //Add('Firebird');
    //Add('Mdx');
  end;
end;

procedure TOptionsSQLFormatterFrame.GetData;
begin
  ComboBoxDatabase.ItemIndex := SQLFormatterOptionsContainer.SQLDatabase;
end;

procedure TOptionsSQLFormatterFrame.PutData;
begin
  SQLFormatterOptionsContainer.SQLDatabase := ComboBoxDatabase.ItemIndex;
end;

end.
