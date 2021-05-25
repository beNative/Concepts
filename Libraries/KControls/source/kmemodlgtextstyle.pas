{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kmemodlgtextstyle; // lowercase name because of Lazarus/Linux

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KEdits, KControls, KButtons, KMemo;

type
  TKMemoTextStyleForm = class(TForm)
    BUOk: TButton;
    BUCancel: TButton;
    GBFont: TGroupBox;
    LBFontName: TLabel;
    GBStyle: TGroupBox;
    EDFont: TEdit;
    LiBFont: TListBox;
    LiBFontSize: TListBox;
    EDFontSize: TKNumberEdit;
    LBFontSize: TLabel;
    CBBold: TCheckBox;
    CBItalic: TCheckBox;
    CBUnderline: TCheckBox;
    CBStrikeout: TCheckBox;
    CBCaps: TCheckBox;
    CBSmallCaps: TCheckBox;
    GBColors: TGroupBox;
    LBTextShading: TLabel;
    LBFontColor: TLabel;
    CLBTextShading: TKColorButton;
    CLBFontColor: TKColorButton;
    CBSubscript: TCheckBox;
    CBSuperscript: TCheckBox;
    procedure LiBFontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure LiBFontClick(Sender: TObject);
    procedure EDFontChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EDFontSizeChange(Sender: TObject);
    procedure LiBFontSizeClick(Sender: TObject);
    procedure CBCapsClick(Sender: TObject);
    procedure CBSmallCapsClick(Sender: TObject);
    procedure CBSubscriptClick(Sender: TObject);
    procedure CBSuperscriptClick(Sender: TObject);
  private
    { Private declarations }
    FFontChanging: Boolean;
    FFontSizeChanging: Boolean;
    procedure FillFontNames;
    procedure FillFontSizes;
  public
    { Public declarations }
    procedure Load(AStyle: TKMemoTextStyle);
    procedure Save(AStyle: TKMemoTextStyle);
  end;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  Math, Types, KGraphics;

{ TKMemoTextStyleForm }

procedure TKMemoTextStyleForm.FormCreate(Sender: TObject);
begin
  FFontChanging := False;
  FFontSizeChanging := False;
  FillFontSizes;
end;

procedure TKMemoTextStyleForm.CBCapsClick(Sender: TObject);
begin
  if CBCaps.Checked then
    CBSmallCaps.Checked := False;
end;

procedure TKMemoTextStyleForm.CBSmallCapsClick(Sender: TObject);
begin
  if CBSmallCaps.Checked then
    CBCaps.Checked := False;
end;

procedure TKMemoTextStyleForm.CBSubscriptClick(Sender: TObject);
begin
  if CBSubscript.Checked then
    CBSuperscript.Checked := False;
end;

procedure TKMemoTextStyleForm.CBSuperscriptClick(Sender: TObject);
begin
  if CBSuperscript.Checked then
    CBSubscript.Checked := False;
end;

procedure TKMemoTextStyleForm.EDFontChange(Sender: TObject);
var
  I, Index: Integer;
  S: string;
begin
  if not FFontChanging then
  begin
    LiBFont.ItemIndex := LiBFont.Items.IndexOf(EDFont.Text);
    if LiBFont.ItemIndex < 0 then
    begin
      // search for best match
      Index := -1;
      S := LowerCase(EDFont.Text);
      for I := 0 to LiBFont.Count - 1 do
      begin
        if Pos(S, LowerCase(LiBFont.Items[I])) = 1 then
        begin
          Index := I;
          Break;
        end;
      end;
    end else
      Index := LiBFont.ItemIndex;
    LiBFOnt.TopIndex := Max(Index, 0);
  end;
end;

procedure TKMemoTextStyleForm.EDFontSizeChange(Sender: TObject);
var
  I, Index: Integer;
  S: string;
begin
  if not FFontSizeChanging then
  begin
    LiBFontSize.ItemIndex := LiBFontSize.Items.IndexOf(IntToStr(EDFontSize.ValueAsInt));
    if LiBFontSize.ItemIndex < 0 then
    begin
      // search for best match
      Index := -1;
      S := LowerCase(EDFontSize.Text);
      for I := 0 to LiBFontSize.Count - 1 do
      begin
        if Pos(S, LowerCase(LiBFontSize.Items[I])) = 1 then
        begin
          Index := I;
          Break;
        end;
      end;
    end else
      Index := LiBFontSize.ItemIndex;
    LiBFontSize.TopIndex := Max(Index, 0);
  end;
end;

procedure TKMemoTextStyleForm.FillFontNames;
var
  I: Integer;
  FontName: string;
begin
  LiBFont.Clear;
  for I := 0 to Screen.Fonts.Count - 1 do
  begin
    FontName := Screen.Fonts[I];
    if FontName[1] <> '@' then // disable vertical fonts
      LiBFont.Items.Add(FontName);
  end;
  if LiBFont.Count > 0 then
    LiBFont.ItemIndex := 0;
end;

procedure TKMemoTextStyleForm.FillFontSizes;
begin
  // fill standard font sizes
  LiBFontSize.Clear;
  with LiBFontSize.Items do
  begin
    Add('8');
    Add('9');
    Add('10');
    Add('11');
    Add('12');
    Add('14');
    Add('16');
    Add('18');
    Add('20');
    Add('22');
    Add('24');
    Add('26');
    Add('28');
    Add('36');
    Add('48');
    Add('72');
  end;
  LiBFontSize.ItemIndex := 0;
end;

procedure TKMemoTextStyleForm.LiBFontClick(Sender: TObject);
begin
  FFontChanging := True;
  try
    if LiBFont.ItemIndex >= 0 then
      EDFont.Text := LiBFont.Items[LiBFont.ItemIndex]
    else
      EDFont.Text := '';
  finally
    FFontChanging := False;
  end;
end;

procedure TKMemoTextStyleForm.LiBFontDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Size: TSize;
  FontName: string;
  FontColor, BrushColor: TColor;
begin
  if odSelected in State then
  begin
    FontColor := clHighlightText;
    BrushColor := clHighlight;
  end else
  begin
    FontColor := clWindowText;
    BrushColor := clWindow;
  end;
  FontName := LiBFont.Items[Index];
  LiBFont.Canvas.Font.Color := FontColor;
  LiBFont.Canvas.Font.Name := FontName;
  LiBFont.Canvas.Font.Height := 18;
  Size := LiBFont.Canvas.TextExtent(FontName);
  LiBFont.Canvas.Brush.Style := bsClear;
  LiBFont.Canvas.Brush.Color := BrushColor;
  LiBFont.Canvas.FillRect(Rect);
  LiBFont.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + (LiBFont.ItemHeight - Size.cy) div 2, LiBFont.Canvas.Font.Name);
end;

procedure TKMemoTextStyleForm.LiBFontSizeClick(Sender: TObject);
begin
  FFontSizeChanging := True;
  try
    if LiBFontSize.ItemIndex >= 0 then
      EDFontSize.Text := LiBFontSize.Items[LiBFontSize.ItemIndex]
    else
      EDFontSize.Text := '';
  finally
    FFontSizeChanging := False;
  end;
end;

procedure TKMemoTextStyleForm.Load(AStyle: TKMemoTextStyle);
begin
  if AStyle <> nil then
  begin
    FillFontNames;
    EDFont.Text := ''; // force the change
    EDFont.Text := AStyle.Font.Name;
    EDFontSize.Text := ''; // force the change
    EDFontSize.ValueAsInt := AStyle.Font.Size;
    CLBFontColor.DlgColor := AStyle.Font.Color;
    if AStyle.Brush.Style <> bsClear then
      CLBTextShading.DlgColor := AStyle.Brush.Color
    else
      CLBTextShading.DlgColor := clNone;
    CBBold.Checked := fsBold in AStyle.Font.Style;
    CBItalic.Checked := fsItalic in AStyle.Font.Style;
    CBUnderline.Checked := fsUnderline in AStyle.Font.Style;
    CBStrikeout.Checked := fsStrikeout in AStyle.Font.Style;
    CBCaps.Checked := AStyle.Capitals = tcaNormal;
    CBSmallCaps.Checked := AStyle.Capitals = tcaSmall;
    CBSubscript.Checked := AStyle.ScriptPosition = tpoSubscript;
    CBSuperscript.Checked := AStyle.ScriptPosition = tpoSuperscript;
  end;
end;

procedure TKMemoTextStyleForm.Save(AStyle: TKMemoTextStyle);
var
  FontStyles: TFontStyles;
begin
  if AStyle <> nil then
  begin
    AStyle.Font.Name := EDFont.Text;
    AStyle.Font.Size := EDFontSize.ValueAsInt;
    AStyle.Font.Color := CLBFontColor.DlgColor;
    if CLBTextShading.DlgColor <> clNone then
      AStyle.Brush.Color := CLBTextShading.DlgColor;
    FontStyles := [];
    if CBBold.Checked then Include(FontStyles, fsBold);
    if CBItalic.Checked then Include(FontStyles, fsItalic);
    if CBUnderline.Checked then Include(FontStyles, fsUnderline);
    if CBStrikeout.Checked then Include(FontStyles, fsStrikeout);
    AStyle.Font.Style := FontStyles;
    if CBCaps.Checked then AStyle.Capitals := tcaNormal
    else if CBSmallCaps.Checked then AStyle.Capitals := tcaSmall
    else AStyle.Capitals := tcaNone;
    if CBSubscript.Checked then AStyle.ScriptPosition := tpoSubscript
    else if CBSuperscript.Checked then AStyle.ScriptPosition := tpoSuperscript
    else AStyle.ScriptPosition := tpoNormal;
  end;
end;

end.
