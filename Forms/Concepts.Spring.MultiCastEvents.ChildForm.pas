unit Concepts.Spring.MultiCastEvents.ChildForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls,

  Concepts.Spring.MultiCastEvents.Data;

type
  TfrmMulticastEventsChild = class(TForm)
    aclMain       : TActionList;
    actTest       : TAction;
    trbImageIndex : TTrackBar;
    procedure trbImageIndexChange(Sender: TObject);

  private
    FPosition : TPosition;

    procedure FPositionOnChange(Sender: TObject);

  public
    constructor Create(
       AOwner    : TComponent;
       APosition : TPosition
    ); reintroduce; virtual;
  end;


implementation

{$R *.dfm}

uses
  Concepts.Resources;


constructor TfrmMulticastEventsChild.Create(AOwner: TComponent;
  APosition: TPosition);
begin
  inherited Create(AOwner);
  trbImageIndex.Max := aclMain.Images.Count;
  FPosition := APosition;
  FPosition.OnChange.Add(FPositionOnChange);
end;

procedure TfrmMulticastEventsChild.FPositionOnChange(Sender: TObject);
begin
  trbImageIndex.Position := FPosition.Position;
  aclMain.Images.GetIcon(FPosition.Position, Icon);
end;

procedure TfrmMulticastEventsChild.trbImageIndexChange(Sender: TObject);
begin
  FPosition.Position := trbImageIndex.Position;
end;

end.
