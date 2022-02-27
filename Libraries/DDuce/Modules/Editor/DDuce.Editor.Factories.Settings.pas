{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit DDuce.Editor.Factories.Settings;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Controls,

  DDuce.Editor.Tools.Settings, DDuce.Editor.Highlighters, DDuce.Editor.Interfaces;

type
  TEditorSettingsFactory = class(TInterfacedObject, IEditorSettingsFactory)
  public
    procedure RegisterToolSettings(ASettings: TEditorToolSettings);
    procedure RegisterHighlighters(AHighlighters: THighlighters);

    function CreateInstance(
      AOwner          : TComponent = nil;
      const AFileName : string = ''
    ): IEditorSettings;
  end;

implementation

uses
  Vcl.Forms,

  DDuce.Editor.CodeFormatters,
//  DDuce.Editor.CodeFormatters.SQL,
  DDuce.Editor.AlignLines.Settings,
//  DDuce.Editor.CodeFilter.Settings,
//  DDuce.Editor.CodeShaper.Settings,
//  DDuce.Editor.HexEditor.Settings,
//  DDuce.Editor.HTMLView.Settings,
//  DDuce.Editor.SortStrings.Settings,
  DDuce.Editor.Search.Engine.Settings,
  DDuce.Editor.Settings,
  DDuce.Editor.Resources;

{$REGION 'private methods'}

procedure TEditorSettingsFactory.RegisterToolSettings(
  ASettings: TEditorToolSettings);
begin
  ASettings.RegisterSettings(TAlignLinesSettings, 'AlignLinesSettings');
//  ASettings.RegisterSettings(TCodeFilterSettings, 'CodeFilterSettings');
//  ASettings.RegisterSettings(THTMLViewSettings, 'HTMLViewSettings');
//  ASettings.RegisterSettings(TSortStringsSettings, 'SortStringsSettings');
  //ASettings.RegisterSettings(THexEditorSettings, 'HexEditorSettings');
  ASettings.RegisterSettings(TSearchEngineSettings, 'SearchSettings');
//  ASettings.RegisterSettings(TCodeShaperSettings, 'CodeShaperSettings');
end;
//
procedure TEditorSettingsFactory.RegisterHighlighters(
  AHighlighters: THighlighters);
  procedure Reg(
    const ALayoutName     : string = '';
    const AName           : string = '';
    const ADescription    : string = '';
    const AFileExtensions : string = '';
    const ACodeFormatter  : ICodeFormatter = nil
  );
  var
    LPath : string;
    LExt  : string;
  begin
    LPath := '.\Highlighters';
    LExt  := '.json';
    AHighlighters.RegisterHighlighter(
      Format('%s\%s%s', [LPath, ALayoutName, LExt]),
      AName,
      ADescription,
      AFileExtensions,
      ACodeFormatter
    );
  end;

begin
//  HL := TCollections.CreateList<string>;
//  HL.AddRange(TDirectory.GetFiles('.\Highlighters', '*.json'));
//  Reg('ActionScript',
//  Reg('ASP',
//  Reg('Assembler - 68HC11',
//  Reg('AutoIt v3',

  Reg('C#',                 HL_CS,   SCSDescription,   FILE_EXTENSIONS_CS);
  Reg('C++',                HL_CPP,  SCPPDescription,  FILE_EXTENSIONS_CPP);
//  Reg('CoffeeScript',
  Reg('CSS',                HL_CSS,  SCSSDescription,  FILE_EXTENSIONS_CSS);
//  Reg('D',
  Reg('Delphi Form Module', HL_DFM,  SDFMDescription,  FILE_EXTENSIONS_DFM);
  Reg('Go',                 HL_GO,   SGODescription,   FILE_EXTENSIONS_GO);
//  Reg('Groovy',
  Reg('HTML with Scripts',  HL_HTML, SHTMLDescription, FILE_EXTENSIONS_HTML);
  Reg('INI',                HL_INI,  SINIDescription,  FILE_EXTENSIONS_INI);
  //Reg('Inno Setup',
  Reg('Java',               HL_JAVA, SJavaDescription, FILE_EXTENSIONS_JAVA);
  Reg('JavaScript',         HL_JS,   SJSDescription,   FILE_EXTENSIONS_JS);
  Reg('JSON',               HL_JSON, SJSONDescription, FILE_EXTENSIONS_JSON);
  Reg('LaTex',              HL_TEX,  STEXDescription,  FILE_EXTENSIONS_TEX);
  Reg('Flux', 'Flux', '', '.flux');
//  Reg('Lisp',
  Reg('Lua',                HL_LUA,  SLUADescription,  FILE_EXTENSIONS_LUA);
//  Reg('MATLAB',
  Reg('MS-DOS Batch',       HL_BAT,  SBATDescription,  FILE_EXTENSIONS_BAT);
  Reg('Object Pascal',      HL_PAS,  SPASDescription,  FILE_EXTENSIONS_PAS);
//  Reg('Objective-C',
//  Reg('OCaml',
  Reg('Perl',               HL_PERL, SPERLDescription, FILE_EXTENSIONS_PERL);
  Reg('PHP',                HL_PHP,  SPHPDescription,  FILE_EXTENSIONS_PHP);
//  Reg('PowerShell',
  Reg('Python',             HL_PY,   SPYDescription,   FILE_EXTENSIONS_PY);
  Reg('Ruby',               HL_RUBY, SRUBYDescription, FILE_EXTENSIONS_RUBY);
//  Reg('Rust',
//  Reg('Scala',
//  Reg('SQL - Firebird',
//  Reg('SQL - Oracle',
//  Reg('SQL - PostgreSQL',
//  Reg('SQL - SQLite',
  Reg('SQL - Standard',    HL_SQL,   SSQLDescription, FILE_EXTENSIONS_SQL);
//  Reg('SQL - Sybase',
//  Reg('TclTk',
  Reg('Text',              HL_TXT,   STXTDescription, FILE_EXTENSIONS_TXT);
//  Reg('UnrealScript',
  //Reg('Visual Basic',      HL_
  Reg('XML',               HL_XML,   SXMLDescription, FILE_EXTENSIONS_XML);
  Reg('XSL',               HL_XSL,   SXSLDescription, FILE_EXTENSIONS_XSL);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TEditorSettingsFactory.CreateInstance(AOwner: TComponent;
  const AFileName: string): IEditorSettings;
var
  ES : IEditorSettings;
begin
  ES := TEditorSettings.Create(AOwner);
  RegisterToolSettings(ES.ToolSettings);
  RegisterHighlighters(ES.Highlighters);
  if AFileName <> '' then
  begin
    ES.FileName := AFileName;
    ES.Load;
  end;
  Result := ES;
end;
{$ENDREGION}

end.

