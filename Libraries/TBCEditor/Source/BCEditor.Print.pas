unit BCEditor.Print;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Printers, BCEditor.Editor.Base, BCEditor.Types,
  BCEditor.Print.Types, BCEditor.Print.HeaderFooter, BCEditor.Print.PrinterInfo, BCEditor.Print.Margins,
  BCEditor.Utils, BCEditor.Highlighter, BCEditor.Editor.Selection, BCEditor.TextDrawer;

type
  TBCEditorPageLine = class
  private
    FFirstLine: Integer;
  public
    property FirstLine: Integer read FFirstLine write FFirstLine;
  end;

  TBCEditorPrint = class(TComponent)
  strict private
    FAbort: Boolean;
    FBlockBeginPosition: TBCEditorTextPosition;
    FBlockEndPosition: TBCEditorTextPosition;
    FCanvas: TCanvas;
    FCharWidth: Integer;
    FColors: Boolean;
    FCopies: Integer;
    FDefaultBackground: TColor;
    FDocumentTitle: string;
    FEditor: TBCBaseEditor;
    FFont: TFont;
    FFontDummy: TFont;
    FFontColor: TColor;
    FFooter: TBCEditorPrintFooter;
    FHeader: TBCEditorPrintHeader;
    FHighlight: Boolean;
    FHighlighter: TBCEditorHighlighter;
    FHighlighterRangesSet: Boolean;
    FLineHeight: Integer;
    FLineNumber: Integer;
    FLineNumbers: Boolean;
    FLineNumbersInMargin: Boolean;
    FLineOffset: Integer;
    FLines: TStrings;
    FMargins: TBCEditorPrintMargins;
    FMaxColumn: Integer;
    FMaxLeftChar: Integer;
    FMaxWidth: Integer;
    FOldFont: TFont;
    FOnPrintLine: TBCEditorPrintLineEvent;
    FOnPrintStatus: TBCEditorPrintStatusEvent;
    FPageCount: Integer;
    FPageOffset: Integer;
    FPages: TList;
    FPagesCounted: Boolean;
    FPrinterInfo: TBCEditorPrinterInfo;
    FPrinting: Boolean;
    FSelectionAvailable: Boolean;
    FSelectedOnly: Boolean;
    FSelectionMode: TBCEditorSelectionMode;
    FTabWidth: Integer;
    FTextDrawer: TBCEditorTextDrawer;
    FTitle: string;
    FWrap: Boolean;
    FYPos: Integer;
    function ClipLineToRect(var ALine: string): string;
    function GetPageCount: Integer;
    procedure CalculatePages;
    procedure HandleWrap(const AText: string);
    procedure InitHighlighterRanges;
    procedure InitPrint;
    procedure PrintPage(APageNumber: Integer);
    procedure RestoreCurrentFont;
    procedure SaveCurrentFont;
    procedure SetCharWidth(const AValue: Integer);
    procedure SetEditor(const AValue: TBCBaseEditor);
    procedure SetFont(const AValue: TFont);
    procedure SetFooter(const AValue: TBCEditorPrintFooter);
    procedure SetHeader(const AValue: TBCEditorPrintHeader);
    procedure SetHighlighter(const AValue: TBCEditorHighlighter);
    procedure SetLines(const AValue: TStrings);
    procedure SetMargins(const AValue: TBCEditorPrintMargins);
    procedure SetMaxLeftChar(const aValue: Integer);
    procedure SetPixelsPerInch;
    procedure SetWrap(const AValue: Boolean);
    procedure TextOut(const AText: string; AList: TList);
    procedure WriteLine(const AText: string);
    procedure WriteLineNumber;
  protected
    procedure PrintLine(ALineNumber, APageNumber: Integer); virtual;
    procedure PrintStatus(AStatus: TBCEditorPrintStatus; APageNumber: Integer; var AAbort: Boolean); virtual;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    property MaxLeftChar: Integer read FMaxLeftChar write SetMaxLeftChar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);
    procedure Print;
    procedure PrintRange(AStartPage, AEndPage: Integer);
    procedure PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
    procedure SaveToStream(AStream: TStream);
    procedure UpdatePages(ACanvas: TCanvas);
    property Editor: TBCBaseEditor read FEditor write SetEditor;
    property PageCount: Integer read GetPageCount;
    property PrinterInfo: TBCEditorPrinterInfo read FPrinterInfo;
  published
    property Color: TColor read FDefaultBackground write FDefaultBackground;
    property Colors: Boolean read FColors write FColors default False;
    property Copies: Integer read FCopies write FCopies;
    property DocumentTitle: string read FDocumentTitle write FDocumentTitle;
    property Font: TFont read FFont write SetFont;
    property Footer: TBCEditorPrintFooter read FFooter write SetFooter;
    property Header: TBCEditorPrintHeader read FHeader write SetHeader;
    property Highlight: Boolean read FHighlight write FHighlight default True;
    property Highlighter: TBCEditorHighlighter read FHighlighter write SetHighlighter;
    property LineNumbers: Boolean read FLineNumbers write FLineNumbers default False;
    property LineNumbersInMargin: Boolean read FLineNumbersInMargin write FLineNumbersInMargin default False;
    property LineOffset: Integer read FLineOffset write FLineOffset default 0;
    property Margins: TBCEditorPrintMargins read FMargins write SetMargins;
    property OnPrintLine: TBCEditorPrintLineEvent read FOnPrintLine write FOnPrintLine;
    property OnPrintStatus: TBCEditorPrintStatusEvent read FOnPrintStatus write FOnPrintStatus;
    property PageOffset: Integer read FPageOffset write FPageOffset default 0;
    property SelectedOnly: Boolean read FSelectedOnly write FSelectedOnly default False;
    property TabWidth: Integer read FTabWidth write FTabWidth;
    property Title: string read FTitle write FTitle;
    property Wrap: Boolean read FWrap write SetWrap default True;
  end;

implementation

uses
  System.UITypes, BCEditor.Highlighter.Attributes, BCEditor.Consts;

{ TBCEditorPrint }

constructor TBCEditorPrint.Create(AOwner: TComponent);
begin
  inherited;
  FCopies := 1;
  FFooter := TBCEditorPrintFooter.Create;
  FHeader := TBCEditorPrintHeader.Create;
  FLines := TStringList.Create;
  FMargins := TBCEditorPrintMargins.Create;
  FPrinterInfo := TBCEditorPrinterInfo.Create;
  FFont := TFont.Create;
  FOldFont := TFont.Create;
  MaxLeftChar := 1024;
  FWrap := True;
  FHighlight := True;
  FColors := False;
  FLineNumbers := False;
  FLineOffset := 0;
  FPageOffset := 0;
  FLineNumbersInMargin := False;
  FPages := TList.Create;
  FTabWidth := 8;
  FDefaultBackground := clWhite;
  FFontDummy := TFont.Create;
  with FFontDummy do
  begin
    Name := 'Courier New';
    Size := 10;
  end;
  FTextDrawer := TBCEditorTextDrawer.Create([fsBold], FFontDummy);
end;

destructor TBCEditorPrint.Destroy;
var
  i: Integer;
begin
  FFooter.Free;
  FHeader.Free;
  FLines.Free;
  FMargins.Free;
  FPrinterInfo.Free;
  FFont.Free;
  FOldFont.Free;
  for i := 0 to FPages.Count - 1 do
    TBCEditorPageLine(FPages[i]).Free;
  FPages.Free;
  FTextDrawer.Free;
  FFontDummy.Free;
  inherited;
end;

procedure TBCEditorPrint.SetLines(const AValue: TStrings);
var
  i, j: Integer;
  LTabConvertProc: TBCEditorTabConvertProc;
  LLine: string;
  LHasTabs: Boolean;
begin
  LTabConvertProc := GetTabConvertProc(FTabWidth);
  with FLines do
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to AValue.Count - 1 do
      begin
        LLine := LTabConvertProc(AValue[i], FTabWidth, LHasTabs);
        j := Pos(BCEDITOR_TAB_CHAR, LLine);
        while j > 0 do
        begin
          LLine[j] := ' ';
          j := Pos(BCEDITOR_TAB_CHAR, LLine);
        end;
        Add(LLine);
      end;
    finally
      EndUpdate;
    end;
  end;
  FHighlighterRangesSet := False;
  FPagesCounted := False;
end;

procedure TBCEditorPrint.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
  FPagesCounted := False;
end;

procedure TBCEditorPrint.SetCharWidth(const AValue: Integer);
begin
  if FCharWidth <> AValue then
    FCharWidth := AValue;
end;

procedure TBCEditorPrint.SetMaxLeftChar(const AValue: Integer);
begin
  if FMaxLeftChar <> AValue then
    FMaxLeftChar := AValue;
end;

procedure TBCEditorPrint.SetHighlighter(const AValue: TBCEditorHighlighter);
begin
  FHighlighter := AValue;
  FHighlighterRangesSet := False;
  FPagesCounted := False;
end;

procedure TBCEditorPrint.SetWrap(const AValue: Boolean);
begin
  if AValue <> FWrap then
  begin
    FWrap := AValue;
    if FPages.Count > 0 then
    begin
      CalculatePages;
      FHeader.NumberOfPages := FPageCount;
      FFooter.NumberOfPages := FPageCount;
   end;
  end;
end;

procedure TBCEditorPrint.InitPrint;
var
  LSize: Integer;
  LTextMetric: TTextMetric;
begin
  FFontColor := FFont.Color;

  FCanvas.Font.Assign(FFont);
  if not FPrinting then
  begin
    SetPixelsPerInch;
    LSize := FCanvas.Font.Size;
    FCanvas.Font.PixelsPerInch := FFont.PixelsPerInch;
    FCanvas.Font.Size := LSize;
  end;
  FCanvas.Font.Style := [fsBold, fsItalic, fsUnderline, fsStrikeOut];

  GetTextMetrics(FCanvas.Handle, LTextMetric);
  CharWidth := LTextMetric.tmAveCharWidth;
  FLineHeight := LTextMetric.tmHeight + LTextMetric.tmExternalLeading;

  FTextDrawer.SetBaseFont(FFont);
  FTextDrawer.Style := FFont.Style;

  FMargins.InitPage(FCanvas, 1, FPrinterInfo, FLineNumbers, FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
  CalculatePages;
  FHeader.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
  FFooter.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
end;

procedure TBCEditorPrint.SetPixelsPerInch;
var
  LSize: Integer;
begin
  FHeader.SetPixelsPerInch(FPrinterInfo.YPixPerInch);
  FFooter.SetPixelsPerInch(FPrinterInfo.YPixPerInch);
  LSize := FFont.Size;
  FFont.PixelsPerInch := FPrinterInfo.YPixPerInch;
  FFont.Size := LSize;
end;

procedure TBCEditorPrint.InitHighlighterRanges;
var
  i: Integer;
begin
  if not FHighlighterRangesSet and Assigned(FHighlighter) and (FLines.Count > 0) then
  begin
    FHighlighter.ResetCurrentRange;
    FLines.Objects[0] := FHighlighter.GetCurrentRange;
    i := 1;
    while i < FLines.Count do
    begin
      FHighlighter.SetCurrentLine(FLines[i - 1]);
      FHighlighter.NextToEndOfLine;
      FLines.Objects[i] := FHighlighter.GetCurrentRange;
      Inc(i);
    end;
    FHighlighterRangesSet := True;
  end;
end;

procedure TBCEditorPrint.CalculatePages;
var
  LTempText, LText: string;
  i, j: Integer;
  LList: TList;
  LYPos: Integer;
  LPageLine: TBCEditorPageLine;
  LStartLine, LEndLine: Integer;
  LSelectionStart, LSelectionLength: Integer;

  procedure CountWrapped;
  var
    j: Integer;
  begin
    for j := 0 to LList.Count - 1 do
      LYPos := LYPos + FLineHeight;
  end;

begin
  InitHighlighterRanges;
  for i := 0 to FPages.Count - 1 do
    TBCEditorPageLine(FPages[i]).Free;
  FPages.Clear;
  FMaxWidth := FMargins.PixelRight - FMargins.PixelLeft;
  LTempText := '';
  FMaxColumn := 0;
  while TextWidth(FCanvas, LTempText) < FMaxWidth do
  begin
    LTempText := LTempText + 'W';
    FMaxColumn := FMaxColumn + 1;
  end;
  FMaxColumn := FMaxColumn - 1;
  LTempText := StringOfChar('W', FMaxColumn);
  FMaxWidth := TextWidth(FCanvas, LTempText);
  FPageCount := 1;
  LPageLine := TBCEditorPageLine.Create;
  LPageLine.FirstLine := 0;
  FPages.Add(LPageLine);
  LYPos := FMargins.PixelTop;
  if SelectedOnly then
  begin
    LStartLine := FBlockBeginPosition.Line - 1;
    LEndLine := FBlockEndPosition.Line - 1;
  end
  else
  begin
    LStartLine := 0;
    LEndLine := FLines.Count - 1;
  end;
  for i := LStartLine to LEndLine do
  begin
    if not FSelectedOnly then
      LText := FLines[i]
    else
    begin
      if (FSelectionMode = smColumn) or (i = FBlockBeginPosition.Line - 1) then
        LSelectionStart := FBlockBeginPosition.Char
      else
        LSelectionStart := 1;
      if (FSelectionMode = smColumn) or (i = FBlockEndPosition.Line - 1) then
        LSelectionLength := FBlockEndPosition.Char - LSelectionStart
      else
        LSelectionLength := MaxInt;
      LText := Copy(FLines[i], LSelectionStart, LSelectionLength);
    end;

    if LYPos + FLineHeight > FMargins.PixelBottom then
    begin
      LYPos := FMargins.PixelTop;
      FPageCount := FPageCount + 1;
      LPageLine := TBCEditorPageLine.Create;
      LPageLine.FirstLine := i;
      FPages.Add(LPageLine);
    end;

    if Wrap and (TextWidth(FCanvas, LText) > FMaxWidth) then
    begin
      LList := TList.Create;
      try
        if WrapTextEx(LText, [' ', '-', BCEDITOR_TAB_CHAR, ','], FMaxColumn, LList) then
          CountWrapped
        else
        begin
          if WrapTextEx(LText, [';', ')', '.'], FMaxColumn, LList) then
            CountWrapped
          else
            while Length(LText) > 0 do
            begin
              LTempText := Copy(LText, 1, FMaxColumn);
              Delete(LText, 1, FMaxColumn);
              if Length(LText) > 0 then
                LYPos := LYPos + FLineHeight;
            end;
        end;
        for j := 0 to LList.Count - 1 do
          TBCEditorWrapPosition(LList[j]).Free;
      finally
        LList.Free;
      end;
    end;

    LYPos := LYPos + FLineHeight;
  end;
  FPagesCounted := True;
end;

procedure TBCEditorPrint.WriteLineNumber;
var
  LLineNumber: string;
begin
  SaveCurrentFont;
  LLineNumber := (FLineNumber + FLineOffset).ToString + ': ';
  FCanvas.Brush.Color := FDefaultBackground;
  FCanvas.Font.Style := [];
  FCanvas.Font.Color := clBlack;
  FCanvas.TextOut(FMargins.PixelLeft - FCanvas.TextWidth(LLineNumber), FYPos, LLineNumber);
  RestoreCurrentFont;
end;

procedure TBCEditorPrint.HandleWrap(const AText: string);
var
  S: string;
  LList: TList;
  j: Integer;

  procedure WrapPrimitive;
  var
    i: Integer;
    LWrapPosition: TBCEditorWrapPosition;
  begin
    i := 1;
    while i <= Length(AText) do
    begin
      S := '';
      while (Length(S) < FMaxColumn) and (i <= Length(AText)) do
      begin
        S := S + AText[i];
        i := i + 1;
      end;
      LWrapPosition := TBCEditorWrapPosition.Create;
      LWrapPosition.Index := i - 1;
      LList.Add(LWrapPosition);
      if (Length(S) - i) <= FMaxColumn then
        Break;
    end;
  end;

begin
  S := '';
  LList := TList.Create;
  try
    if WrapTextEx(AText, [' ', '-', BCEDITOR_TAB_CHAR, ','], FMaxColumn, LList) then
      TextOut(AText, LList)
    else
    begin
      if WrapTextEx(AText, [';', ')', '.'], FMaxColumn, LList) then
        TextOut(AText, LList)
      else
      begin
        WrapPrimitive;
        TextOut(AText, LList)
      end;
    end;
    for j := 0 to LList.Count - 1 do
      TBCEditorWrapPosition(LList[j]).Free;
  finally
    LList.Free;
  end;
end;

procedure TBCEditorPrint.SaveCurrentFont;
begin
  FOldFont.Assign(FCanvas.Font);
end;

procedure TBCEditorPrint.RestoreCurrentFont;
begin
  FCanvas.Font.Assign(FOldFont);
end;

function TBCEditorPrint.ClipLineToRect(var ALine: string): string;
begin
  while FCanvas.TextWidth(ALine) > FMaxWidth do
    SetLength(ALine, Length(ALine) - 1);

  Result := ALine;
end;

procedure TBCEditorPrint.TextOut(const AText: string; AList: TList);
var
  i: Integer;
  LToken: string;
  LTokenPosition: Integer;
  LHighlighterAttribute: TBCEditorHighlighterAttribute;
  LColor: TColor;
  LTokenStart: Integer;
  LCount: Integer;
  LHandled: Boolean;
  LWrapPosition, LOldWrapPosition: Integer;
  LLines: TStringList;
  LClipRect: TRect;

  procedure ClippedTextOut(X, Y: Integer; AText: string);
  begin
    AText := ClipLineToRect(AText);
    if Highlight and Assigned(FHighlighter) and (FLines.Count > 0) then
    begin
      SetBkMode(FCanvas.Handle, TRANSPARENT);
      FTextDrawer.ExtTextOut(X, Y, 0, LClipRect, PChar(AText), Length(AText));
      SetBkMode(FCanvas.Handle, OPAQUE);
    end
    else
      Winapi.Windows.ExtTextOut(FCanvas.Handle, X, Y, 0, nil, PChar(AText), Length(AText), nil);
  end;

  procedure SplitToken;
  var
    LTempText: string;
    LLast: Integer;
    LFirstPosition: Integer;
    LTokenEnd: Integer;
  begin
    LLast := LTokenPosition;
    LFirstPosition := LTokenPosition;
    LTokenEnd := LTokenPosition + Length(LToken);
    while (LCount < AList.Count) and (LTokenEnd > TBCEditorWrapPosition(AList[LCount]).Index) do
    begin
      LTempText := Copy(AText, LLast + 1, TBCEditorWrapPosition(AList[LCount]).Index - LLast);
      LLast := TBCEditorWrapPosition(AList[LCount]).Index;
      ClippedTextOut(FMargins.PixelLeft + LFirstPosition * FTextDrawer.CharWidth, FYPos, LTempText);
      LFirstPosition := 0;
      LCount := LCount + 1;
      FYPos := FYPos + FLineHeight;
    end;
    LTempText := Copy(AText, LLast + 1, LTokenEnd - LLast);
    ClippedTextOut(FMargins.PixelLeft + LFirstPosition * FTextDrawer.CharWidth, FYPos, LTempText);
    LTokenStart := LTokenPosition + Length(LToken) - Length(LTempText);
  end;

var
  LTempText: string;
begin
  FTextDrawer.BeginDrawing(FCanvas.Handle);
  with FMargins do
    LClipRect := Rect(PixelLeft, PixelTop, PixelRight, PixelBottom);

  if Highlight and Assigned(FHighlighter) and (FLines.Count > 0) then
  begin
    SaveCurrentFont;
    FHighlighter.SetCurrentRange(FLines.Objects[FLineNumber - 1]);
    FHighlighter.SetCurrentLine(AText);
    LToken := '';
    LTokenStart := 0;
    LCount := 0;
    while not FHighlighter.GetEndOfLine do
    begin
      FHighlighter.GetToken(LToken);
      LTokenPosition := FHighlighter.GetTokenPosition;
      LHighlighterAttribute := FHighlighter.GetTokenAttribute;

      if Assigned(LHighlighterAttribute) then
      begin
        FCanvas.Font.Style := LHighlighterAttribute.Style;
        if FColors then
        begin
          LColor := LHighlighterAttribute.Foreground;
          if LColor = clNone then
            LColor := FFont.Color;
          FCanvas.Font.Color := LColor;
          LColor := LHighlighterAttribute.Background;
          if LColor = clNone then
            LColor := FDefaultBackground;
          FCanvas.Brush.Color := LColor;
        end
        else
        begin
          FCanvas.Font.Color := FFontColor;
          FCanvas.Brush.Color := FDefaultBackground;
        end;
      end
      else
      begin
        FCanvas.Font.Color := FFontColor;
        FCanvas.Brush.Color := FDefaultBackground;
      end;
      LHandled := False;
      if Assigned(AList) then
        if LCount < AList.Count then
        begin
          if LTokenPosition >= TBCEditorWrapPosition(AList[LCount]).Index then
          begin
            LCount := LCount + 1;
            LTokenStart := LTokenPosition;
            FYPos := FYPos + FLineHeight;
          end
          else
          if LTokenPosition + Length(LToken) > TBCEditorWrapPosition(AList[LCount]).Index then
          begin
            LHandled := True;
            SplitToken;
          end;
        end;
      if not LHandled then
        ClippedTextOut(FMargins.PixelLeft + (LTokenPosition - LTokenStart) * FTextDrawer.CharWidth, FYPos, LToken);
      FHighlighter.Next;
    end;
    RestoreCurrentFont;
  end
  else
  begin
    LLines := TStringList.Create;
    try
      LOldWrapPosition := 0;
      if Assigned(AList) then
        for i := 0 to AList.Count - 1 do
        begin
          LWrapPosition := TBCEditorWrapPosition(AList[i]).Index;
          if i = 0 then
            LTempText := Copy(AText, 1, LWrapPosition)
          else
            LTempText := Copy(AText, LOldWrapPosition + 1, LWrapPosition - LOldWrapPosition);
          LLines.Add(LTempText);
          LOldWrapPosition := LWrapPosition;
        end;
      if Length(AText) > 0 then
        LLines.Add(Copy(AText, LOldWrapPosition + 1, MaxInt));

      for i := 0 to LLines.Count - 1 do
      begin
        ClippedTextOut(FMargins.PixelLeft, FYPos, LLines[i]);
        if i < LLines.Count - 1 then
          FYPos := FYPos + FLineHeight;
      end;
    finally
      LLines.Free;
    end
  end;
  FTextDrawer.EndDrawing;
end;

procedure TBCEditorPrint.WriteLine(const AText: string);
begin
  if FLineNumbers then
    WriteLineNumber;
  if Wrap and (FCanvas.TextWidth(AText) > FMaxWidth) then
    HandleWrap(AText)
  else
    TextOut(AText, nil);
  FYPos := FYPos + FLineHeight;
end;

procedure TBCEditorPrint.PrintPage(APageNumber: Integer);
var
  i, LEndLine: Integer;
  LSelectionStart, LSelectionLength: Integer;
begin
  PrintStatus(psNewPage, APageNumber, FAbort);
  if not FAbort then
  begin
    FCanvas.Brush.Color := Color;
    with FMargins do
      PatBlt(FCanvas.Handle, PixelLeft, PixelTop, PixelRight - PixelLeft, PixelBottom - PixelTop, PATCOPY);
    FMargins.InitPage(FCanvas, APageNumber, FPrinterInfo, FLineNumbers, FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
    FHeader.Print(FCanvas, APageNumber + FPageOffset);
    if FPages.Count > 0 then
    begin
      FYPos := FMargins.PixelTop;
      if APageNumber = FPageCount then
        LEndLine := FLines.Count - 1
      else
        LEndLine := TBCEditorPageLine(FPages[APageNumber]).FirstLine - 1;
      for i := TBCEditorPageLine(FPages[APageNumber - 1]).FirstLine to LEndLine do
      begin
        FLineNumber := i + 1;
        if (not FSelectedOnly or ((i >= FBlockBeginPosition.Line - 1) and (i <= FBlockEndPosition.Line - 1))) then
        begin
          if not FSelectedOnly then
            WriteLine(FLines[i])
          else
          begin
            if (FSelectionMode = smColumn) or (i = FBlockBeginPosition.Line - 1) then
              LSelectionStart := FBlockBeginPosition.Char
            else
              LSelectionStart := 1;
            if (FSelectionMode = smColumn) or (i = FBlockEndPosition.Line - 1) then
              LSelectionLength := FBlockEndPosition.Char - LSelectionStart
            else
              LSelectionLength := MaxInt;
            WriteLine(Copy(FLines[i], LSelectionStart, LSelectionLength));
          end;
          PrintLine(i + 1, APageNumber);
        end;
      end;
    end;
    FFooter.Print(FCanvas, APageNumber + FPageOffset);
  end;
end;

procedure TBCEditorPrint.UpdatePages(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  FPrinterInfo.UpdatePrinter;
  InitPrint;
end;

procedure TBCEditorPrint.PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
begin
  FAbort := False;
  FPrinting := False;
  FCanvas := ACanvas;
  PrintPage(PageNumber);
end;

procedure TBCEditorPrint.Print;
begin
  PrintRange(1, -1);
end;

procedure TBCEditorPrint.PrintRange(AStartPage, AEndPage: Integer);
var
  i, j: Integer;
begin
  if FSelectedOnly and not FSelectionAvailable then
    Exit;

  FPrinting := True;
  FAbort := False;
  if FDocumentTitle <> '' then
    Printer.Title := FDocumentTitle
  else
    Printer.Title := FTitle;
  Printer.BeginDoc;
  PrintStatus(psBegin, AStartPage, FAbort);
  UpdatePages(Printer.Canvas);

  for i := 1 to Copies do
  begin
    j := AStartPage;
    if AEndPage < 0 then
      AEndPage := FPageCount;
    while (j <= AEndPage) and (not FAbort) do
    begin
      PrintPage(j);
      if ((j < AEndPage) or (i < Copies)) and not FAbort then
        Printer.NewPage;
      Inc(j);
    end;
  end;
  if not FAbort then
    PrintStatus(psEnd, AEndPage, FAbort);
  Printer.EndDoc;
  FPrinting := False;
end;

procedure TBCEditorPrint.PrintLine(ALineNumber, APageNumber: Integer);
begin
  if Assigned(FOnPrintLine) then
    FOnPrintLine(Self, ALineNumber, APageNumber);
end;

procedure TBCEditorPrint.PrintStatus(AStatus: TBCEditorPrintStatus; APageNumber: Integer; var AAbort: Boolean);
begin
  AAbort := False;
  if Assigned(FOnPrintStatus) then
    FOnPrintStatus(Self, AStatus, APageNumber, AAbort);
  if AAbort then
    if FPrinting then
      Printer.Abort;
end;

function TBCEditorPrint.GetPageCount: Integer;
var
  LCanvas: TCanvas;
  LHandle: HDC;
begin
  Result := 0;
  if FPagesCounted then
    Result := FPageCount
  else
  begin
    LCanvas := TCanvas.Create;
    LHandle := GetDC(0);
    try
      if LHandle <> 0 then
      begin
        LCanvas.Handle := LHandle;
        UpdatePages(LCanvas);
        LCanvas.Handle := 0;
        Result := FPageCount;
        FPagesCounted := True;
      end;
    finally
      ReleaseDC(0, LHandle);
      LCanvas.Free;
    end;
  end;
end;

procedure TBCEditorPrint.SetEditor(const AValue: TBCBaseEditor);
begin
  FEditor := AValue;
  Highlighter := AValue.Highlighter;
  Font := AValue.Font;
  CharWidth := AValue.CharWidth;
  FTabWidth := AValue.Tabs.Width;
  SetLines(AValue.Lines);
  FSelectionAvailable := AValue.SelectionAvailable;
  FBlockBeginPosition := AValue.SelectionBeginPosition;
  FBlockEndPosition := AValue.SelectionEndPosition;
  FSelectionMode := AValue.Selection.Mode;
end;

procedure TBCEditorPrint.LoadFromStream(AStream: TStream);
var
  LLength, LBufferSize: Integer;
  LBuffer: PChar;
begin
  FHeader.LoadFromStream(AStream);
  FFooter.LoadFromStream(AStream);
  FMargins.LoadFromStream(AStream);
  with AStream do
  begin
    Read(LLength, SizeOf(LLength));
    LBufferSize := LLength * SizeOf(Char);
    GetMem(LBuffer, LBufferSize + SizeOf(Char));
    try
      Read(LBuffer^, LBufferSize);
      LBuffer[LBufferSize div SizeOf(Char)] := BCEDITOR_NONE_CHAR;
      FTitle := LBuffer;
    finally
      FreeMem(LBuffer);
    end;
    Read(LLength, SizeOf(LLength));
    LBufferSize := LLength * SizeOf(Char);
    GetMem(LBuffer, LBufferSize + SizeOf(Char));
    try
      Read(LBuffer^, LBufferSize);
      LBuffer[LBufferSize div SizeOf(Char)] := BCEDITOR_NONE_CHAR;
      FDocumentTitle := LBuffer;
    finally
      FreeMem(LBuffer);
    end;
    Read(FWrap, SizeOf(FWrap));
    Read(FHighlight, SizeOf(FHighlight));
    Read(FColors, SizeOf(FColors));
    Read(FLineNumbers, SizeOf(FLineNumbers));
    Read(FLineOffset, SizeOf(FLineOffset));
    Read(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TBCEditorPrint.SaveToStream(AStream: TStream);
var
  LLength: Integer;
begin
  FHeader.SaveToStream(AStream);
  FFooter.SaveToStream(AStream);
  FMargins.SaveToStream(AStream);
  with AStream do
  begin
    LLength := Length(FTitle);
    Write(LLength, SizeOf(LLength));
    Write(PChar(FTitle)^, LLength * SizeOf(Char));
    LLength := Length(FDocumentTitle);
    Write(LLength, SizeOf(LLength));
    Write(PChar(FDocumentTitle)^, LLength * SizeOf(Char));
    Write(FWrap, SizeOf(FWrap));
    Write(FHighlight, SizeOf(FHighlight));
    Write(FColors, SizeOf(FColors));
    Write(FLineNumbers, SizeOf(FLineNumbers));
    Write(FLineOffset, SizeOf(FLineOffset));
    Write(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TBCEditorPrint.SetFooter(const AValue: TBCEditorPrintFooter);
begin
  FFooter.Assign(AValue);
end;

procedure TBCEditorPrint.SetHeader(const AValue: TBCEditorPrintHeader);
begin
  FHeader.Assign(AValue);
end;

procedure TBCEditorPrint.SetMargins(const AValue: TBCEditorPrintMargins);
begin
  FMargins.Assign(AValue);
end;

end.
