unit TBCEditorDemo.Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BCEditor.Editor.Base, BCEditor.Editor, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Editor: TBCEditor;
    ListBoxColors: TListBox;
    ListBoxHighlighters: TListBox;
    PanelLeft: TPanel;
    SplitterVertical: TSplitter;
    SplitterHorizontal: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxHighlightersClick(Sender: TObject);
    procedure ListBoxColorsClick(Sender: TObject);
  private
    { Private declarations }
    procedure SetSelectedColor;
    procedure SetSelectedHighlighter;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure AddFileNamesFromPathIntoListBox(const APath: string; AListBox: TListBox);
var
  LSearchRec: TSearchRec;
begin
  if FindFirst(APath + '*.json', faNormal, LSearchRec) = 0 then
  try
    repeat
      AListBox.AddItem(LSearchRec.Name, nil);
    until FindNext(LSearchRec) <> 0;
  finally
    FindClose(LSearchRec);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  LApplicationPath, LHighlighterPath, LColorsPath: string;
begin
  LApplicationPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  with Editor.Directories do
  begin
    LHighlighterPath := IncludeTrailingPathDelimiter(LApplicationPath + Highlighters);
    LColorsPath := IncludeTrailingPathDelimiter(LApplicationPath + Colors);
  end;

  AddFileNamesFromPathIntoListBox(LHighlighterPath, ListBoxHighlighters);
  AddFileNamesFromPathIntoListBox(LColorsPath, ListBoxColors);

  with ListBoxHighlighters do
    Selected[Items.IndexOf('Object Pascal.json')] := True;

  with ListBoxColors do
    Selected[Items.IndexOf('Default.json')] := True;

  SetSelectedHighlighter;
  SetSelectedColor;
end;

procedure TMainForm.SetSelectedColor;
begin
  with ListBoxColors do
    Editor.Highlighter.Colors.LoadFromFile(Items[ItemIndex]);
end;

procedure TMainForm.SetSelectedHighlighter;
begin
  with ListBoxHighlighters do
    Editor.Highlighter.LoadFromFile(Items[ItemIndex]);
  Editor.Lines.Text := Editor.Highlighter.Info.General.Sample;
end;

procedure TMainForm.ListBoxColorsClick(Sender: TObject);
begin
  SetSelectedColor;
end;

procedure TMainForm.ListBoxHighlightersClick(Sender: TObject);
begin
  SetSelectedHighlighter;
end;

end.
