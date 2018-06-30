unit BCEditor.Export.HTML;

interface

uses
  System.Classes, System.SysUtils, Vcl.Graphics, BCEditor.Lines, BCEditor.Highlighter;

type
  TBCEditorExportHTML = class(TObject)
  private
    FCharSet: string;
    FFont: TFont;
    FHighlighter: TBCEditorHighlighter;
    FLines: TBCEditorLines;
    FStringList: TStrings;
    procedure CreateHTMLDocument;
    procedure CreateHeader;
    procedure CreateInternalCSS;
    procedure CreateLines;
    procedure CreateFooter;
  public
    constructor Create(ALines: TBCEditorLines; AHighlighter: TBCEditorHighlighter; AFont: TFont; const ACharSet: string); overload;
    destructor Destroy; override;

    procedure SaveToStream(AStream: TStream; AEncoding: System.SysUtils.TEncoding);
  end;

implementation

uses
  Winapi.Windows, System.UITypes, BCEditor.Highlighter.Attributes, BCEditor.Highlighter.Colors, BCEditor.Consts,
  BCEditor.Utils;

constructor TBCEditorExportHTML.Create(ALines: TBCEditorLines; AHighlighter: TBCEditorHighlighter; AFont: TFont; const ACharSet: string);
begin
  inherited Create;

  FStringList := TStringList.Create;

  FCharSet := ACharSet;
  if FCharSet = '' then
    FCharSet := 'utf-8';
  FLines := ALines;
  FHighlighter := AHighlighter;
  FFont := AFont;
end;

destructor TBCEditorExportHTML.Destroy;
begin
  FStringList.Free;

  inherited Destroy;
end;

procedure TBCEditorExportHTML.CreateHTMLDocument;
begin
  if not Assigned(FHighlighter) then
    Exit;
  if FLines.Count = 0 then
    Exit;

  CreateHeader;
  CreateLines;
  CreateFooter;
end;

procedure TBCEditorExportHTML.CreateHeader;
begin
  FStringList.Add('<!DOCTYPE HTML>');
  FStringList.Add('');
  FStringList.Add('<html>');
  FStringList.Add('<head>');
	FStringList.Add('  <meta charset="' + FCharSet + '">');

  CreateInternalCSS;

  FStringList.Add('</head>');
  FStringList.Add('');
  FStringList.Add('<body class="Editor">');
end;

procedure TBCEditorExportHTML.CreateInternalCSS;
var
  LIndex: Integer;
  LStyles: TList;
  LElement: PBCEditorHighlighterElement;
begin
  FStringList.Add('  <style>');

  FStringList.Add('    body {');
  FStringList.Add('      font-family: ' + FFont.Name + ';');
  FStringList.Add('      font-size: ' + IntToStr(FFont.Size) + 'px;');
  FStringList.Add('    }');

  LStyles := FHighlighter.Colors.Styles;
  for LIndex := 0 to LStyles.Count - 1 do
  begin
    LElement := LStyles.Items[LIndex];

    FStringList.Add('    .' + LElement^.Name + ' { ');
    FStringList.Add('      color: #' + ColorToHex(LElement^.Foreground) + ';');
    FStringList.Add('      background-color: #' + ColorToHex(LElement^.Background) + ';');

    if TFontStyle.fsBold in LElement^.FontStyles then
      FStringList.Add('      font-weight: bold;');

    if TFontStyle.fsItalic in LElement^.FontStyles then
      FStringList.Add('      font-style: italic;');

    if TFontStyle.fsUnderline in LElement^.FontStyles then
      FStringList.Add('      text-decoration: underline;');

    if TFontStyle.fsStrikeOut in LElement^.FontStyles then
      FStringList.Add('      text-decoration: line-through;');

    FStringList.Add('    }');
    FStringList.Add('');
  end;
  FStringList.Add('  </style>');
end;

procedure TBCEditorExportHTML.CreateLines;
var
  LIndex: Integer;
  LTextLine, LToken: string;
  LHighlighterAttribute: TBCEditorHighlighterAttribute;
  LPreviousElement: string;
begin
  LPreviousElement := '';
  for LIndex := 0 to FLines.Count - 1 do
  begin
    if LIndex = 0 then
      FHighlighter.ResetCurrentRange
    else
      FHighlighter.SetCurrentRange(FLines.Ranges[LIndex - 1]);
    FHighlighter.SetCurrentLine(FLines.ExpandedStrings[LIndex]);
    LTextLine := '';
    while not FHighlighter.GetEndOfLine do
    begin
      LHighlighterAttribute := FHighlighter.GetTokenAttribute;
      FHighlighter.GetToken(LToken);
      if LToken = BCEDITOR_SPACE_CHAR then
        LTextLine := LTextLine + '&nbsp;'
      else
      if LToken = '&' then
        LTextLine := LTextLine + '&amp;'
      else
      if LToken = '<' then
        LTextLine := LTextLine + '&lt;'
      else
      if LToken = '>' then
        LTextLine := LTextLine + '&gt;'
      else
      if LToken = '"' then
        LTextLine := LTextLine + '&quot;'
      else
      if Assigned(LHighlighterAttribute) then
      begin
        if (LPreviousElement <> '') and (LPreviousElement <> LHighlighterAttribute.Element) then
          LTextLine := LTextLine + '</span>';
        if LPreviousElement <> LHighlighterAttribute.Element then
          LTextLine := LTextLine + '<span class="' + LHighlighterAttribute.Element + '">';
        LTextLine := LTextLine + LToken;
        LPreviousElement := LHighlighterAttribute.Element;
      end
      else
        LTextLine := LTextLine + LToken;
      FHighlighter.Next;
    end;
    FStringList.Add(LTextLine + '<br>');
  end;
  if LPreviousElement <> '' then
    FStringList.Add('</span>');
end;

procedure TBCEditorExportHTML.CreateFooter;
begin
  FStringList.Add('</body>');
  FStringList.Add('</html>');
end;

procedure TBCEditorExportHTML.SaveToStream(AStream: TStream; AEncoding: System.SysUtils.TEncoding);
begin
  CreateHTMLDocument;
  if not Assigned(AEncoding) then
    AEncoding := TEncoding.UTF8;
  FStringList.SaveToStream(AStream, AEncoding);
end;

end.
