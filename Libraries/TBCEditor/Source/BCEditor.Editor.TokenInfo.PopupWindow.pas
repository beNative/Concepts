unit BCEditor.Editor.TokenInfo.PopupWindow;

{
  Supported tags in contents:

  <a href="Reference">Link text</a>
  <b>Bold text</b>
  <i>Italic text</i>
}

interface

uses
  System.Classes, System.Types, Vcl.Graphics, BCEditor.Types, BCEditor.Lines, BCEditor.Editor.PopupWindow,
  BCEditor.Editor.TokenInfo;

type
  TBCEditorTokenInfoEvent = procedure(ASender: TObject; const ATextPosition: TBCEditorTextPosition;
    const AToken: string; AContent: TBCEditorLines; ATitleContent: TBCEditorLines; var AShowInfo: Boolean) of object;

  TBCEditorTokenInfoTextStyle = (tsBold, tsItalic, tsReference);
  TBCEditorTokenInfoTextStyles = set of TBCEditorTokenInfoTextStyle;

  TBCEditorTokenInfoPopupWindow = class(TBCEditorPopupWindow)
  strict private
    FBitmapBuffer: Vcl.Graphics.TBitmap;
    FContent: TBCEditorLines;
    FContentTextTokensList: TList;
    FMaxHeight: Integer;
    FMaxWidth: Integer;
    FTitleContent: TBCEditorLines;
    FTitleContentTextTokensList: TList;
    FTokenInfo: TBCEditorTokenInfo;
    procedure ParseText(AText: TBCEditorLines; ATokens: TList; AFont: TFont);
    procedure SetStyles(const AStyles: TBCEditorTokenInfoTextStyles);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    procedure Execute(const APoint: TPoint);
    property Content: TBCEditorLines read FContent write FContent;
    property TitleContent: TBCEditorLines read FTitleContent write FTitleContent;
  end;

implementation

uses
  Winapi.Windows, BCEditor.Consts, BCEditor.Utils, System.UITypes;

const
  MARGIN_LEFT = 3;

type
  TBCEditorTokenInfoTextToken = record
    Value: string;
    Styles: TBCEditorTokenInfoTextStyles;
    Rect: TRect;
    Reference: string;
  end;
  PBCEditorTokenInfoTextToken = ^TBCEditorTokenInfoTextToken;

{ TBCEditorTokenInfoPopupWindow }

constructor TBCEditorTokenInfoPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FContent := TBCEditorLines.Create(nil);
  FContent.Clear;
  FContentTextTokensList := TList.Create;

  FTitleContent := TBCEditorLines.Create(nil);
  FTitleContent.Clear;
  FTitleContentTextTokensList := TList.Create;

  FBitmapBuffer := Vcl.Graphics.TBitmap.Create;
end;

destructor TBCEditorTokenInfoPopupWindow.Destroy;
var
  LIndex: Integer;
begin
  FContent.Free;

  for LIndex := FContentTextTokensList.Count - 1 downto 0 do
    Dispose(PBCEditorTokenInfoTextToken(FContentTextTokensList.Items[LIndex]));
  FContentTextTokensList.Clear;
  FContentTextTokensList.Free;

  FTitleContent.Free;

  for LIndex := FTitleContentTextTokensList.Count - 1 downto 0 do
    Dispose(PBCEditorTokenInfoTextToken(FTitleContentTextTokensList.Items[LIndex]));
  FTitleContentTextTokensList.Clear;
  FTitleContentTextTokensList.Free;

  FBitmapBuffer.Free;

  inherited Destroy;
end;

procedure TBCEditorTokenInfoPopupWindow.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTokenInfo then
  begin
    FTokenInfo := ASource as TBCEditorTokenInfo;
    with FTokenInfo do
    begin
      if not (tioAutoSize in Options) then
      begin
        Self.Width := Width;
        Self.Height := Height;
      end;
    end
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTokenInfoPopupWindow.Execute(const APoint: TPoint);
begin
  FMaxHeight := 0;
  FMaxWidth := 0;

  if FTokenInfo.Title.Visible then
    ParseText(FTitleContent, FTitleContentTextTokensList, FTokenInfo.Title.Font);
  ParseText(FContent, FContentTextTokensList, FTokenInfo.Font);

  if tioAutoSize in FTokenInfo.Options then
  begin
    // TODO if height goes over the bottom, show scroll bar
    Height := FMaxHeight + 2;
    Width := FMaxWidth + 2;
  end;

  Height := Height;
  Width := Width + 2 * MARGIN_LEFT;

  Show(APoint);
end;

procedure TBCEditorTokenInfoPopupWindow.SetStyles(const AStyles: TBCEditorTokenInfoTextStyles);
begin
  FBitmapBuffer.Canvas.Font.Style := [];
  if tsBold in AStyles then
    FBitmapBuffer.Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  if tsItalic in AStyles then
    FBitmapBuffer.Canvas.Font.Style := Canvas.Font.Style + [fsItalic];
  if tsReference in AStyles then
    FBitmapBuffer.Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
end;

procedure TBCEditorTokenInfoPopupWindow.Paint;
var
  LIndex: Integer;
  LPTextToken: PBCEditorTokenInfoTextToken;
  LPreviousStyles: TBCEditorTokenInfoTextStyles;
  LLeft, LTop, LPreviousTop: Integer;
  LText: string;
  LRect: TRect;

  procedure PaintToken;
  begin
    SetStyles(LPreviousStyles);

    if tsReference in LPreviousStyles then
      FBitmapBuffer.Canvas.Font.Color := FTokenInfo.Title.Colors.Reference;

    FBitmapBuffer.Canvas.TextOut(LLeft, LTop, LText);
  end;

begin
  with FBitmapBuffer do
  begin
    Canvas.Brush.Color := FTokenInfo.Colors.Background;
    Height := 0;
    Width := ClientWidth;
    Height := ClientHeight;
    LLeft := MARGIN_LEFT;
    LTop := 0;
    LRect := Rect(0, 0, 0, 0);

    if FTitleContentTextTokensList.Count > 0 then
    begin
      Canvas.Brush.Color := FTokenInfo.Title.Colors.Background;

      LRect.Width := ClientWidth;
      LPTextToken := PBCEditorTokenInfoTextToken(FTitleContentTextTokensList[FTitleContentTextTokensList.Count - 1]);
      LRect.Height := LPTextToken^.Rect.Bottom;

      Winapi.Windows.ExtTextOut(Canvas.Handle, 0, 0, ETO_OPAQUE, LRect, '', 0, nil);

      LPTextToken := PBCEditorTokenInfoTextToken(FTitleContentTextTokensList[0]);
      LPreviousStyles := LPTextToken^.Styles;
      LPreviousTop := LPTextToken^.Rect.Top;
      for LIndex := 0 to FTitleContentTextTokensList.Count - 1 do
      begin
        Canvas.Font.Assign(FTokenInfo.Title.Font);

        LPTextToken := PBCEditorTokenInfoTextToken(FTitleContentTextTokensList[LIndex]);

        if (LPreviousStyles <> LPTextToken^.Styles) or (LPreviousTop <> LPTextToken^.Rect.Top) then
        begin
          PaintToken;
          LText := '';
          LLeft := LPTextToken^.Rect.Left;
          LTop := LPTextToken^.Rect.Top;
        end;
        LPreviousStyles := LPTextToken^.Styles;
        LPreviousTop :=  LPTextToken^.Rect.Top;
        LText := LText + LPTextToken^.Value;
      end;
      if LText <> '' then
        PaintToken;
    end;

    if FContentTextTokensList.Count > 0 then
    begin
      LText := '';
      LLeft := MARGIN_LEFT;

      Canvas.Brush.Color := FTokenInfo.Colors.Background;

      LPTextToken := PBCEditorTokenInfoTextToken(FContentTextTokensList[0]);
      LPreviousStyles := LPTextToken^.Styles;
      LPreviousTop := LPTextToken^.Rect.Top;
      LTop := LPTextToken^.Rect.Top;
      for LIndex := 0 to FContentTextTokensList.Count - 1 do
      begin
        Canvas.Font.Assign(FTokenInfo.Font);

        LPTextToken := PBCEditorTokenInfoTextToken(FContentTextTokensList[LIndex]);

        if (LPreviousStyles <> LPTextToken^.Styles) or (LPreviousTop <> LPTextToken^.Rect.Top) then
        begin
          PaintToken;
          LText := '';
          LLeft := LPTextToken^.Rect.Left;
          LTop := LPTextToken^.Rect.Top;
        end;
        LPreviousStyles := LPTextToken^.Styles;
        LPreviousTop :=  LPTextToken^.Rect.Top;
        LText := LText + LPTextToken^.Value;
      end;
      if LText <> '' then
        PaintToken;
    end;
  end;
  Canvas.Draw(0, 0, FBitmapBuffer);
end;

procedure TBCEditorTokenInfoPopupWindow.ParseText(AText: TBCEditorLines; ATokens: TList; AFont: TFont);
const
  CTOKEN_REFERENCE = 0;
  CTOKEN_BOLD = 1;
  CTOKEN_ITALIC = 2;
var
  LIndex: Integer;
  LPText, LPToken, LPBookmark: PChar;
  LPTextToken: PBCEditorTokenInfoTextToken;
  LCurrentValue: string;
  LCurrentStyles: TBCEditorTokenInfoTextStyles;
  LCurrentRect: TRect;
  LCurrentReference: string;
  LTextHeight: Integer;
  LOpenTokens: array [0..2] of string;
  LCloseTokens: array [0..2] of string;

  procedure AddTokens;
  begin
    LOpenTokens[CTOKEN_REFERENCE] := '<A HREF="';
    LOpenTokens[CTOKEN_BOLD] := '<B>';
    LOpenTokens[CTOKEN_ITALIC] := '<I>';
    LCloseTokens[CTOKEN_REFERENCE] := '</A>';
    LCloseTokens[CTOKEN_BOLD] := '</B>';
    LCloseTokens[CTOKEN_ITALIC] := '</I>';
  end;

  procedure ClearCurrentValue;
  begin
    LCurrentValue := '';
    LCurrentStyles := [];
    LCurrentReference := '';
  end;

  procedure AddTextToken;
  begin
    New(LPTextToken);
    LPTextToken^.Value := LCurrentValue;
    LPTextToken^.Styles := LCurrentStyles;
    SetStyles(LCurrentStyles);
    LCurrentRect.Right := LCurrentRect.Left + FBitmapBuffer.Canvas.TextWidth(LCurrentValue);
    if LCurrentRect.Right > FMaxWidth then
      FMaxWidth := LCurrentRect.Right;
    LPTextToken^.Rect := LCurrentRect;
    LPTextToken^.Reference := LCurrentReference;
    ATokens.Add(LPTextToken);
    ClearCurrentValue;
    LCurrentRect.Left := LCurrentRect.Right;
  end;

  procedure NextLine;
  begin
    AddTextToken;
    LCurrentRect.Left := MARGIN_LEFT;
    Inc(FMaxHeight, LTextHeight);
    Inc(LCurrentRect.Top, LTextHeight);
    Inc(LCurrentRect.Bottom, LTextHeight);
  end;

begin
  if AText.Text = '' then
    Exit;

  AddTokens;

  FBitmapBuffer.Canvas.Font.Assign(AFont);
  LTextHeight := FBitmapBuffer.Canvas.TextHeight('X') + 2;
  LCurrentRect.Left := MARGIN_LEFT;
  LCurrentRect.Top := FMaxHeight;
  LCurrentRect.Bottom := LTextHeight;
  Inc(FMaxHeight, LTextHeight);
  LPText := PChar(AText.Text);
  ClearCurrentValue;
  while LPText^ <> BCEDITOR_NONE_CHAR do
  begin
    if LPText^ = '<' then
    for LIndex := 0 to Length(LOpenTokens) - 1 do
    begin
      LPToken := PChar(LOpenTokens[LIndex]);
      LPBookmark := LPText;
      while (LPText^ <> BCEDITOR_NONE_CHAR) and (LPToken^ <> BCEDITOR_NONE_CHAR) and (CaseUpper(LPText^) = LPToken^) do
      begin
        Inc(LPText);
        Inc(LPToken);
      end;
      if LPToken^ = BCEDITOR_NONE_CHAR then
      begin
        if LCurrentValue <> '' then
          AddTextToken;

        case LIndex of
          CTOKEN_REFERENCE:
            begin
              while (LPText^ <> BCEDITOR_NONE_CHAR) and (LPText^ <> '"') do
              begin
                LCurrentReference := LCurrentReference + LPText^;
                Inc(LPText);
              end;
              Inc(LPText, 2); // '">'
              Include(LCurrentStyles, tsReference);
            end;
          CTOKEN_BOLD:
            Include(LCurrentStyles, tsBold);
          CTOKEN_ITALIC:
            Include(LCurrentStyles, tsItalic);
        end;
        Break;
      end
      else
        LPText := LPBookmark;
    end;

    if LPText^ = '<' then
    for LIndex := 0 to Length(LCloseTokens) - 1 do
    begin
      LPToken := PChar(LCloseTokens[LIndex]);
      LPBookmark := LPText;
      while (LPText^ <> BCEDITOR_NONE_CHAR) and (LPToken^ <> BCEDITOR_NONE_CHAR) and (CaseUpper(LPText^) = LPToken^) do
      begin
        Inc(LPText);
        Inc(LPToken);
      end;
      if LPToken^ = BCEDITOR_NONE_CHAR then
      begin
        if LCurrentValue <> '' then
          AddTextToken;
        Break;
      end
      else
        LPText := LPBookmark;
    end;

    if LPText^ = BCEDITOR_CARRIAGE_RETURN then
    begin
      if LCurrentValue <> '' then
        AddTextToken;
      NextLine;
      Inc(LPText, 2);
      Continue;
    end;

    if LPText^ <> BCEDITOR_NONE_CHAR then
    begin
      LCurrentValue := LCurrentValue + LPText^;
      Inc(LPText);
    end;
  end;
  if LCurrentValue <> '' then
    AddTextToken;
end;

end.
