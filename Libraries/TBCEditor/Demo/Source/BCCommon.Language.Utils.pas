unit BCCommon.Language.Utils;

interface

uses
  Vcl.Forms;

function GetSelectedLanguage(Default: string = ''): string;
procedure UpdateLanguage(Form: TForm; SelectedLanguage: string = ''); overload;

implementation

uses
  BigIni, BCCommon.FileUtils, System.SysUtils, System.IniFiles, Vcl.StdCtrls, Vcl.ActnList, Vcl.Menus, Vcl.ComCtrls,
  Vcl.ExtCtrls, VirtualTrees, sPageControl, Vcl.Consts,
  BCControl.GroupBox, BCControl.RadioButton, BCControl.Panel, BCControl.Edit, sLabel, acSlider,
  BCControl.DateEdit, BCControl.ComboBox;

function GetSelectedLanguage(Default: string): string;
begin
  with TBigIniFile.Create(GetIniFilename) do
  try
    Result := ReadString('Options', 'Language', Default);
  finally
    Free;
  end;
end;

procedure UpdateLanguage(Form: TForm; SelectedLanguage: string);
var
  i, j, LLeft: Integer;
  s: string;
  LanguagePath: string;
begin
  if SelectedLanguage = '' then
    SelectedLanguage := GetSelectedLanguage;
  if SelectedLanguage = '' then
    Exit;
  LanguagePath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'Languages');
  if not DirectoryExists(LanguagePath) then
    Exit;

  with TMemIniFile.Create(LanguagePath + SelectedLanguage + '.lng', TEncoding.Unicode) do
  try
    s := ReadString(Form.Name, 'Caption', '');
    if s <> '' then
      Form.Caption := s;
    for i := 0 to Form.ComponentCount - 1 do
      if Form.Components[i] is TFrame then
        UpdateLanguage(TForm(Form.Components[i]), SelectedLanguage)
      else
      if Form.Components[i] is TMenuItem then
        Continue
      else
      if Form.Components[i] is TButton then
      begin
        s := ReadString(Form.Name, TButton(Form.Components[i]).Name, '');
        if s <> '' then
          TButton(Form.Components[i]).Caption := s
      end
      else
      if Form.Components[i] is TLabel then
      begin
        s := ReadString(Form.Name, TLabel(Form.Components[i]).Name, '');
        if s <> '' then
          TLabel(Form.Components[i]).Caption := s
      end
      else
      if Form.Components[i] is TsStickyLabel then
      begin
        s := ReadString(Form.Name, TsStickyLabel(Form.Components[i]).Name, '');
        if s <> '' then
          TsStickyLabel(Form.Components[i]).Caption := s
      end
      else
      if Form.Components[i] is TsSlider then
      begin
        LLeft := TsSlider(Form.Components[i]).Left;
        TsSlider(Form.Components[i]).SliderCaptionOn := StringReplace(SMsgDlgYes, '&', '', [rfReplaceAll]);
        TsSlider(Form.Components[i]).SliderCaptionOff := StringReplace(SMsgDlgNo, '&', '', [rfReplaceAll]);
        TsSlider(Form.Components[i]).Left := LLeft;
      end
      else
      if Form.Components[i] is TBCRadioButton then
      begin
        s := ReadString(Form.Name, TBCRadioButton(Form.Components[i]).Name, '');
        if s <> '' then
          TBCRadioButton(Form.Components[i]).Caption := ' ' + s;
      end
      else
      if Form.Components[i] is TBCGroupBox then
      begin
        s := ReadString(Form.Name, TGroupBox(Form.Components[i]).Name, '');
        if s <> '' then
          TGroupBox(Form.Components[i]).Caption := s;
      end
      else
      if Form.Components[i] is TBCPanel then
      begin
        s := ReadString(Form.Name, TBCPanel(Form.Components[i]).Name, '');
        if s <> '' then
          TBCPanel(Form.Components[i]).Caption := ' ' + s + ' '
      end
      else
      if Form.Components[i] is TBCComboBox then
      begin
        s := ReadString(Form.Name, TBCComboBox(Form.Components[i]).Name, '');
        if s <> '' then
          TBCComboBox(Form.Components[i]).BoundLabel.Caption := s;
        s := ReadString(Form.Name, TBCComboBox(Form.Components[i]).Name + ':h', '');
        if s <> '' then
          TBCComboBox(Form.Components[i]).Hint := s
      end
      else
      if Form.Components[i] is TBCFontComboBox then
      begin
        s := ReadString(Form.Name, TBCFontComboBox(Form.Components[i]).Name, '');
        if s <> '' then
          TBCFontComboBox(Form.Components[i]).BoundLabel.Caption := s
      end
      else
      if Form.Components[i] is TBCColorComboBox then
      begin
        s := ReadString(Form.Name, TBCColorComboBox(Form.Components[i]).Name, '');
        if s <> '' then
          TBCColorComboBox(Form.Components[i]).BoundLabel.Caption := s
      end
      else
      if Form.Components[i] is TBCDateEdit then
      begin
        s := ReadString(Form.Name, TBCDateEdit(Form.Components[i]).Name, '');
        if s <> '' then
          TBCDateEdit(Form.Components[i]).BoundLabel.Caption := s
      end
      else
      if Form.Components[i] is TBCEdit then
      begin
        s := ReadString(Form.Name, TBCEdit(Form.Components[i]).Name, '');
        if s <> '' then
          TBCEdit(Form.Components[i]).BoundLabel.Caption := s
      end
      else
      if Form.Components[i] is TAction then
      begin
        s := ReadString(Form.Name, TAction(Form.Components[i]).Name, '');
        if (TAction(Form.Components[i]).Caption <> '') and (s <> '') then
          TAction(Form.Components[i]).Caption := s;
        s := ReadString(Form.Name, TAction(Form.Components[i]).Name + ':h', '');
        if s <> '' then
          TAction(Form.Components[i]).Hint := s;
        s := ReadString(Form.Name, TAction(Form.Components[i]).Name + ':s', '');
        if s <> '' then
        begin
          TAction(Form.Components[i]).Hint := TAction(Form.Components[i]).Hint + ' (' + s + ')';
          TAction(Form.Components[i]).ShortCut := TextToShortCut(s);
        end;
      end
      else
      if Form.Components[i] is TsTabSheet then
      begin
        s := ReadString(Form.Name, TsTabSheet(Form.Components[i]).Name, '');
        if s <> '' then
          TsTabSheet(Form.Components[i]).Caption := s
      end
      else
      if Form.Components[i] is TRadioGroup then
      begin
        s := ReadString(Form.Name, TRadioGroup(Form.Components[i]).Name, '');
        if s <> '' then
        begin
          TRadioGroup(Form.Components[i]).Caption := s;
          TRadioGroup(Form.Components[i]).Items.Clear;
          j := 0;
          repeat
            s := ReadString(Form.Name, Format('%s%d', [TRadioGroup(Form.Components[i]).Name, j]), '');
            if s <> '' then
              begin
              TRadioGroup(Form.Components[i]).Items.Add(s);
              Inc(j);
            end;
          until s = '';
        end;
      end
      else
      if Form.Components[i] is TVirtualDrawTree then
      begin
        s := ReadString(Form.Name, TVirtualDrawTree(Form.Components[i]).Name + ':h', '');
        if s <> '' then
          TVirtualDrawTree(Form.Components[i]).Hint := s;
        for j := 0 to TVirtualDrawTree(Form.Components[i]).Header.Columns.Count - 1 do
        begin
          s := ReadString(Form.Name, Format('%s:%d', [TVirtualDrawTree(Form.Components[i]).Name, j]), '');
          if s <> '' then
            TVirtualDrawTree(Form.Components[i]).Header.Columns[j].Text := s;
        end;
      end
  finally
    Free;
  end;
end;

end.

