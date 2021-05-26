{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.Resources;

{ Shared resources. }

interface

uses
  System.Classes, System.SysUtils, System.ImageList,
  Vcl.Controls, Vcl.ImgList;

const
  HL_BAT  = 'BAT';
  HL_CPP  = 'CPP';
  HL_CS   = 'CS';
  HL_CSS  = 'CSS';
  HL_DIFF = 'DIFF';
  HL_GO   = 'GO';
  HL_HTML = 'HTML';
  HL_INI  = 'INI';
  HL_JAVA = 'JAVA';
  HL_JS   = 'JS';
  HL_JSON = 'JSON';
  HL_DFM  = 'DFM';
  HL_LOG  = 'LOG';
  HL_LUA  = 'LUA';
  HL_PAS  = 'PAS';
  HL_PERL = 'PERL';
  HL_PHP  = 'PHP';
  HL_PO   = 'PO';
  HL_PY   = 'PY';
  HL_RES  = 'RES';
  HL_RTF  = 'RTF';
  HL_RUBY = 'RUBY';
  HL_SH   = 'SH';
  HL_SQL  = 'SQL';
  HL_TEX  = 'TEX';
  HL_TXT  = 'TXT';
  HL_XML  = 'XML';
  HL_XSL  = 'XSL';

  // comma separated lists of supported file extensions (no spaces)
  FILE_EXTENSIONS_GO   = 'go';
  FILE_EXTENSIONS_TXT  = 'txt,nfo,me';
  FILE_EXTENSIONS_PAS  = 'pas,dpr,pp,lpr,inc,dpk';
  FILE_EXTENSIONS_CPP  = 'cpp,hpp,' +            // C++
                         'c,h' +                 // regular C
                         'ino,pde';              // Arduino sketch
  FILE_EXTENSIONS_JAVA = 'java';                 // Java source file
  FILE_EXTENSIONS_SQL  = 'sql';                  // SQL script source file
  FILE_EXTENSIONS_XML  = 'xml,' +                // General XML file
                         'dtd,' +                // Document Type Definition
                         'xsd,' +                // XML Schema Definition
                         'xslt,' +               // XSL Transform file
                         'hgl,' +                // Highlighter (synunisyn)
                         'lpi,lps,lpk,' +        // Lazarus
                         'fpc,compiled' +        // FPC Make
                         'dproj,groupproj,' +    // Delphi
                         'template,' +           // Java templates
                         'svg,' +                // Scalable Vector Graphics
                         'docx,' +               // Open XML document
                         'docm,' +               // Word Macro-Enabled Document
                         'dotx,' +               // Word Template
                         'dotm,' +               // Word Macro-Enabled Template
                         'xlsx,' +               // Open XML Spreadsheet
                         'xlsm,' +               // Excel Macro-Enabled Workbook
                         'xltx,' +               // Excel Template
                         'xltm,' +               // Excel Macro-Enabled Template
                         'xlam,' +               // Excel Add-In
                         'pptx,' +               // Open XML Presentation
                         'pptm,' +               // PowerPoint Macro-Enabled Presentation
                         'potx,'  +              // PowerPoint Template
                         'potm,'  +              // PowerPoint Macro-Enabled Template
                         'ppam,'  +              // PowerPoint Add-In
                         'ppsx,'  +              // PowerPoint Show
                         'ppsm,' +               // PowerPoint Macro-Enabled Show
                         'vsdx,' +               // Visio Drawing
                         'vsdm,' +               // Visio Macro-Enabled Drawing
                         'vssx,' +               // Visio Stencil
                         'vssm,' +               // Visio Macro-Enabled Stencil
                         'vstx,' +               // Visio Template
                         'vstm,' +               // Visio Macro-Enabled Template
                         'sgml,' +               // Standard Generalized Markup Language
                         'xmmap,' +              // Mindjet MindManager
                         'vcxproj,' +            // Visual C++ Project file
                         'vcxproj.filters,' +    // Visual C++ Project filters
                         'cpppjoj';              // C++ Project file
  FILE_EXTENSIONS_XSL  = 'xsl';
  FILE_EXTENSIONS_DFM  = 'dfm,' +                // Delphi VCL form
                         'lfm,' +                // Lazarus LCL form
                         'fmx';                  // Delphi Firemonkey form
  FILE_EXTENSIONS_INI  = 'ini,' +
                         'fpd,' +                // FastReport definition
                         'reg,' +                // Windows registry file
                         'lrt,' +
                         'msg,' +
                         'prop,properties,' +
                         'desktop';
  FILE_EXTENSIONS_BAT  = 'bat,cmd';
  FILE_EXTENSIONS_RTF  = 'rtf';
  FILE_EXTENSIONS_RES  = 'res';
  FILE_EXTENSIONS_PHP  = 'php';
  FILE_EXTENSIONS_PERL = 'pl';
  FILE_EXTENSIONS_PY   = 'py';
  FILE_EXTENSIONS_HTML = 'html,htm';
  FILE_EXTENSIONS_PO   = 'po';
  FILE_EXTENSIONS_JS   = 'js';
  FILE_EXTENSIONS_CSS  = 'css';
  FILE_EXTENSIONS_CS   = 'cs';
  FILE_EXTENSIONS_DIFF = 'diff';
  FILE_EXTENSIONS_TEX  = 'tex';
  FILE_EXTENSIONS_SH   = 'sh';
  FILE_EXTENSIONS_RUBY = 'rb,rbw';
  FILE_EXTENSIONS_LUA  = 'lua';
  FILE_EXTENSIONS_JSON = 'json';

resourcestring
  STextNotFound = 'Text not found';
  SSearching    = 'Searching...';

  SBATDescription  = 'Windows batch script';
  SCPPDescription  = 'C++ source file';
  SCSDescription   = 'C# source file';
  SCSSDescription  = 'Cascading Style Sheet';
  SDFMDescription  = 'Object Pascal form definition';
  SDIFFDescription = 'Diff';
  SGODescription   = 'Go source file';
  SHTMLDescription = 'HTML document';
  SINIDescription  = 'Settings file';
  SJavaDescription = 'Java source file';
  SJSDescription   = 'JavaScript';
  SJSONDescription = 'JSON document';
  SLOGDescription  = 'Log';
  SLUADescription  = 'Lua script';
  SPASDescription  = 'Object Pascal source file';
  SPERLDescription = 'Perl script';
  SPHPDescription  = 'PHP script';
  SPODescription   = 'Gettext translation strings';
  SPYDescription   = 'Python script';
  SRESDescription  = 'Windows resources';
  SRTFDescription  = 'Rich Text Format document';
  SRUBYDescription = 'Ruby script';
  SSHDescription   = 'Shell script';
  SSQLDescription  = 'SQL script';
  STEXDescription  = 'TeX document';
  STXTDescription  = 'Text document';
  SXMLDescription  = 'XML document';
  SXSLDescription  = 'XML stylesheet document';

  SAskSaveChanges = 'File %s is modified. Do you want to save changes?';

  SNewEditorViewFileName   = '<new>';
  SFileMenuCaption         = '&File';
  SSeLectionMenuCaption    = 'Se&lection';
  SSearchMenuCaption       = '&Search';
  SInsertMenuCaption       = '&Insert';
  SViewMenuCaption         = '&View';
  SToolsMenuCaption        = '&Tools';
  SSettingsMenuCaption     = '&Settings';
  SHighlightersMenuCaption = '&Highlighters';
  SHelpMenuCaption         = '&Help';
  SApplicationMenuCaption  = '&Application';

  // CodeFilterDialog
  SOneLineWithMatchFound = '1 line with match found.';
  SLinesWithMatchFound   = '%d lines with match found.';

  // IEditorManager
  SNotImplementedYet     = 'This feature is not implemented yet';

const
  DEFAULT_BLEND_FACTOR = 128;

const
  ALineBreakStyles : array[TTextLineBreakStyle] of string = (
    'LF',
    'CRLF'
  );

  AHighlighterLayouts : array[0..47] of string = (
   'ActionScript',
   'ASP',
   'Assembler - 68HC11',
   'AutoIt v3',
   'AWK',
   'C',
   'C#',
   'C++',
   'CoffeeScript',
   'CSS',
   'D',
   'Delphi Form Module',
   'Free Pascal',
   'Go',
   'Groovy',
   'HTML with Scripts',
   'INI',
   'Inno Setup',
   'Java',
   'JavaScript',
   'JSON',
   'LaTex',
   'Lisp',
   'Lua',
   'MATLAB',
   'MS-DOS Batch',
   'Object Pascal',
   'Objective-C',
   'OCaml',
   'Perl',
   'PHP',
   'PowerShell',
   'Python',
   'Ruby',
   'Rust',
   'Scala',
   'SQL - Firebird',
   'SQL - Oracle',
   'SQL - PostgreSQL',
   'SQL - SQLite',
   'SQL - Standard',
   'SQL - Sybase',
   'TclTk',
   'Text',
   'UnrealScript',
   'Visual Basic',
   'XML',
   'XSL'
  );

type
  TResourcesDataModule = class(TDataModule)
    imlFunctionKeys : TImageList;
  end;

implementation

{$R *.dfm}

end.

