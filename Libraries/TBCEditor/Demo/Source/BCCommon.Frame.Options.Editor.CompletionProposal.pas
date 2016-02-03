unit BCCommon.Frame.Options.Editor.CompletionProposal;

interface

uses
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BCControl.ComboBox, BCCommon.Options.Container, BCCommon.Frame.Options.Base, sComboBox, BCControl.Panel,
  acSlider, sLabel, Vcl.StdCtrls, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsEditorCompletionProposalFrame = class(TBCOptionsBaseFrame)
    ComboBoxShortcut: TBCComboBox;
    Panel: TBCPanel;
    StickyLabelCaseSensitive: TsStickyLabel;
    SliderCaseSensitive: TsSlider;
    StickyLabelEnabled: TsStickyLabel;
    SliderEnabled: TsSlider;
    SliderAutoInvoke: TsSlider;
    StickyLabelAutoInvoke: TsStickyLabel;
  protected
    procedure Init; override;
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorCompletionProposalFrame(AOwner: TComponent): TOptionsEditorCompletionProposalFrame;

implementation

{$R *.dfm}

uses
  Vcl.Menus, BCCommon.Utils, BCCommon.Consts;

var
  FOptionsEditorCompletionProposalFrame: TOptionsEditorCompletionProposalFrame;

function OptionsEditorCompletionProposalFrame(AOwner: TComponent): TOptionsEditorCompletionProposalFrame;
begin
  if not Assigned(FOptionsEditorCompletionProposalFrame) then
    FOptionsEditorCompletionProposalFrame := TOptionsEditorCompletionProposalFrame.Create(AOwner);
  Result := FOptionsEditorCompletionProposalFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsEditorCompletionProposalFrame.Destroy;
begin
  inherited;
  FOptionsEditorCompletionProposalFrame := nil;
end;

procedure TOptionsEditorCompletionProposalFrame.Init;
var
  i: Integer;
begin
  for i := 1 to High(ShortCuts) do
    ComboBoxShortcut.Items.Add(ShortCutToText(ShortCuts[i]));
end;

procedure TOptionsEditorCompletionProposalFrame.PutData;
begin
  with OptionsContainer do
  begin
    CompletionProposalEnabled := SliderEnabled.SliderOn;
    CompletionProposalCaseSensitive := SliderCaseSensitive.SliderOn;
    CompletionProposalAutoInvoke := SliderAutoInvoke.SliderOn;
    CompletionProposalShortcut := ComboBoxShortcut.Text;
  end;
end;

procedure TOptionsEditorCompletionProposalFrame.GetData;
begin
  with OptionsContainer do
  begin
    SliderEnabled.SliderOn := CompletionProposalEnabled;
    SliderCaseSensitive.SliderOn := CompletionProposalCaseSensitive;
    SliderAutoInvoke.SliderOn := CompletionProposalAutoInvoke;
    ComboBoxShortcut.ItemIndex := ComboBoxShortcut.Items.IndexOf(CompletionProposalShortcut);
  end;
end;

end.
