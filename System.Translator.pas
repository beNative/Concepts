{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

{
  Unit for automatic dxgettext-based translation support.

  This module simplifies translation support by elliminating the need to use
  the standard gnugettext API functions from code.

  Add this unit to your program's uses clause to automatically translate all
  forms and datamodules in your application when they are created. It also will
  scan all the components based on ownership to find texts that need
  localization (published string properties).
  Dynamically created components with no Owner won't be translated unless you
  call Translator.Translate explicitely.

  By default all resource strings are localizable. The hooks defined in
  gnugettext.pas redirect the native calls to fetch these resources.

  The default language depends on the Windows regional settings. This setting
  can be overruled by setting an environment variable "lang" to the corres-
  ponding language code.
  At runtime the language can be changed in code to any supported language by
  changing the LanguageCode property.

  Author: Tim Sinaeve (2005-2015).

  TODO:
    - Use extended RTTI for adding translation ignores.
    - Use thread safe hooks (DDetours). This is probably not needed as all
      visual controls and its data are always handled in the main thread, and
      hooks are only created on objects that are not thread safe themselves
      anyway (TDataModule, TForm).
 }

unit System.Translator;

interface

uses
  System.Classes, System.Rtti;

const
  DEFAULT_TEXTDOMAIN = 'default'; // gnugettext assumes this domain name to
                                  // use in its search for translation files.

type
  Translator = record
  private
    FVMIForm       : TVirtualMethodInterceptor;
    FVMIDataModule : TVirtualMethodInterceptor;

    procedure FormDoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);

    procedure FormDoAfter(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; var Result: TValue);

    procedure DataModuleDoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);

    procedure DataModuleDoAfter(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; var Result: TValue);

    class function GetLanguageCode: string; static;
    class procedure SetLanguageCode(const Value: string); static;
    class function GetTextDomain: string; static;
    class procedure SetTextDomain(const Value: string); static;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure AddTranslationIgnores; static;
    class function GetLogFileName: string; static;
    class procedure GetSupportedLanguageCodes(
            ALanguageCodes : TStrings;
      const ATextDomain    : string = DEFAULT_TEXTDOMAIN
    ); static;
    class procedure Translate(AComponent : TComponent = nil); static;
    class procedure Refresh; static;

    { Path to the gnugettext log file. This file will only be created when
      the conditional variable DXGETTEXTDEBUG is defined. }
    class property LogFileName: string
      read GetLogFileName;

    { Current application locale. }
    class property LanguageCode: string
      read GetLanguageCode write SetLanguageCode;

    { Switches to another textdomain that will be used for translation. }
    class property TextDomain: string
      read GetTextDomain write SetTextDomain;

  end;

implementation

uses
  gnugettext,

  System.RTLConsts, System.SysUtils, System.TypInfo, System.IOUtils,
  Winapi.Windows, SHDocVw,
  Vcl.ActnList, Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,
  Vcl.DBCtrls, Vcl.StdCtrls,
  Data.DB, Data.Win.ADODB,
  DataSnap.DBClient;

const
  LOG_FILENAME = 'dxgettext.log';

function GetdxGettextLogFileName: string;
var
  S : string;
begin
  S := ExtractFilePath(Application.ExeName) + '\' + LOG_FILENAME;
  Result := S;
end;

type
  TCharArray5 = array[0..4] of AnsiChar;

  THook = class
  private
    Patch         : TCharArray5;
    Original      : TCharArray5;
    PatchPosition : PAnsiChar;

  public
    constructor Create(OldProcedure, NewProcedure: Pointer);
    destructor Destroy; override; // Restores unhooked state

    procedure Disable;
    procedure Enable;
  end;

  TGGForm = class(TCustomForm)
    procedure GGDoCreate;
  end;

  TGGDataModule = class(TDataModule)
    procedure GGDoCreate;
  end;

var
  HookFormOnCreate       : THook;
  HookDataModuleOnCreate : THook;

{$REGION 'construction and destruction'}
constructor THook.Create(OldProcedure, NewProcedure: Pointer);
var
  Offset : Integer;
  OV     : Cardinal;
begin
  PatchPosition := PAnsiChar(OldProcedure);
  Offset := Integer(NewProcedure) - Integer(OldProcedure) - 5;

  Patch[0] := AnsiChar($E9);
  Patch[1] := AnsiChar(Offset and 255);
  Patch[2] := AnsiChar((Offset shr 8) and 255);
  Patch[3] := AnsiChar((Offset shr 16) and 255);
  Patch[4] := AnsiChar((Offset shr 24) and 255);

  Original[0] := PatchPosition[0];
  Original[1] := PatchPosition[1];
  Original[2] := PatchPosition[2];
  Original[3] := PatchPosition[3];
  Original[4] := PatchPosition[4];

  if not VirtualProtect(Pointer(PatchPosition), 5, PAGE_EXECUTE_READWRITE, @OV)
    then
    RaiseLastOSError;

  Enable;
end;

destructor THook.Destroy;
begin
  Disable;
  inherited;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure THook.Enable;
begin
  PatchPosition[0] := Patch[0];
  PatchPosition[1] := Patch[1];
  PatchPosition[2] := Patch[2];
  PatchPosition[3] := Patch[3];
  PatchPosition[4] := Patch[4];
end;

procedure THook.Disable;
begin
  PatchPosition[0] := Original[0];
  PatchPosition[1] := Original[1];
  PatchPosition[2] := Original[2];
  PatchPosition[3] := Original[3];
  PatchPosition[4] := Original[4];
end;
{$ENDREGION}

procedure TGGForm.GGDoCreate;
begin
  HookFormOnCreate.Disable;
  try
    DoCreate;
  finally
    HookFormOnCreate.Enable;
  end;
  DisableAlign;
  DisableAutoRange;
  try
    TranslateComponent(Self);
  finally
    EnableAlign;
    EnableAutoRange;
  end;
end;

procedure TGGDataModule.GGDoCreate;
begin
  HookDataModuleOnCreate.Disable;
  try
    DoCreate;
  finally
    HookDataModuleOnCreate.Enable;
  end;
  TranslateComponent(Self);
end;

{$REGION 'Translator'}
class constructor Translator.Create;
begin
//  FVMIForm       := TVirtualMethodInterceptor(TCustomForm);
//  FVMIDataModule := TVirtualMethodInterceptor(TDataModule);
//  FVMIForm.OnBefore := FormDoBefore;
//  FVMIForm.OnAfter  := FormDoAfter;
//  FVMIDataModule.OnBefore := DataModuleDoBefore;
//  FVMIDataModule.OnAfter  := DataModuleDoAfter;

// translates VCL resourcestrings if the translation file is included
  Translator.AddTranslationIgnores;
  // if delphi RTL translations are available, these will be used too
  //AddDomainForResourceString('delphi');
end;

procedure Translator.DataModuleDoAfter(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; var Result: TValue);
begin
//
end;

procedure Translator.DataModuleDoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
begin
//
end;

class destructor Translator.Destroy;
begin
//  FVMIForm := nil;
//  FVMIDataModule := nil;
end;

procedure Translator.FormDoAfter(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; var Result: TValue);
begin
//
end;

procedure Translator.FormDoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
begin
//
end;

class function Translator.GetLanguageCode: string;
begin
  Result := DefaultInstance.GetCurrentLanguage;
end;

class procedure Translator.SetLanguageCode(const Value: string);
begin
  if not SameText(Value, LanguageCode) then
  begin
    UseLanguage(Value);
    Refresh;
  end;
end;

class function Translator.GetTextDomain: string;
begin
  Result := DefaultInstance.GetCurrentTextDomain;
end;

class procedure Translator.Refresh;
var
  I : Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    Translate(Screen.Forms[I]);
  for I := 0 to Screen.DataModuleCount - 1 do
    Translate(Screen.DataModules[I]);
end;

class procedure Translator.SetTextDomain(const Value: string);
begin
  if Value <> TextDomain then
    DefaultInstance.TextDomain(Value);
end;

class function Translator.GetLogFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + '\' + LOG_FILENAME;
end;

{ Specifies which classes/objects and properties should be excluded from
  translation. }

class procedure Translator.AddTranslationIgnores;
begin
  if TFile.Exists(GetLogFileName) then
    TFile.Delete(GetLogFileName);
  DefaultInstance.DebugLogToFile(GetLogFileName);

  // VCL
  // (Vcl.ActnList.pas)
  TP_GlobalIgnoreClassProperty(TAction,'Category');
  // (Vcl.Controls.pas)
  TP_GlobalIgnoreClassProperty(TControl,'HelpKeyword');
  TP_GlobalIgnoreClassProperty(TControl,'ImeName');
  // (Vcl.StdCtrls.pas)
  TP_GlobalIgnoreClassProperty(TCustomEdit,'Text');
  TP_GlobalIgnoreClassProperty(TCustomMemo,'Lines');
  // (Vcl.ExtCtrls.pas)
  TP_GlobalIgnoreClassProperty(TNotebook,'Pages');
  // (Vcl.Graphics.pas)
  TP_GlobalIgnoreClass(TFont);

  // Database controls (Vcl.DBCtrls.pas)
  TP_GlobalIgnoreClassProperty(TControl,'DataField');
  TP_GlobalIgnoreClassProperty(TDBComboBox,'DataField');
  TP_GlobalIgnoreClassProperty(TDBCheckBox,'DataField');
  TP_GlobalIgnoreClassProperty(TDBEdit,'DataField');
  TP_GlobalIgnoreClassProperty(TDBImage,'DataField');
  TP_GlobalIgnoreClassProperty(TDBListBox,'DataField');
  TP_GlobalIgnoreClassProperty(TDBLookupControl,'DataField');
  TP_GlobalIgnoreClassProperty(TDBLookupControl,'KeyField');
  TP_GlobalIgnoreClassProperty(TDBLookupControl,'ListField');
  TP_GlobalIgnoreClassProperty(TDBMemo,'DataField');
  TP_GlobalIgnoreClassProperty(TDBRadioGroup,'DataField');
  TP_GlobalIgnoreClassProperty(TDBRichEdit,'DataField');
  TP_GlobalIgnoreClassProperty(TDBText,'DataField');

  // Database (Data.DB.pas)
  TP_GlobalIgnoreClassProperty(TField,'DefaultExpression');
  TP_GlobalIgnoreClassProperty(TField,'FieldName');
  TP_GlobalIgnoreClassProperty(TField,'KeyFields');
  TP_GlobalIgnoreClassProperty(TField,'LookupKeyFields');
  TP_GlobalIgnoreClassProperty(TField,'LookupResultField');
  TP_GlobalIgnoreClassProperty(TField,'Origin');
  TP_GlobalIgnoreClassProperty(TFieldDef,'Name');
  TP_GlobalIgnoreClass(TParam);

  // ADO components (Data.ADODB.pas)
  TP_GlobalIgnoreClassProperty(TCustomADODataset, 'CommandText');
  TP_GlobalIgnoreClassProperty(TCustomADODataset, 'ConnectionString');
  TP_GlobalIgnoreClassProperty(TCustomADODataset, 'DatasetField');
  TP_GlobalIgnoreClassProperty(TCustomADODataset, 'Filter');
  TP_GlobalIgnoreClassProperty(TCustomADODataset, 'IndexFieldNames');
  TP_GlobalIgnoreClassProperty(TCustomADODataset, 'IndexName');
  TP_GlobalIgnoreClassProperty(TCustomADODataset, 'MasterFields');
  TP_GlobalIgnoreClassProperty(TADOStoredProc, 'ProcedureName');
  TP_GlobalIgnoreClassProperty(TADOTable, 'TableName');
  TP_GlobalIgnoreClassProperty(TADOCommand, 'CommandText');
  TP_GlobalIgnoreClass(TADOConnection);
  TP_GlobalIgnoreClass(TParameter);

  // Web browser (SHDocVw.pas)
  TP_GlobalIgnoreClass(TWebBrowser);

  // MIDAS/Datasnap (DataSnap.DBClient.pas)
  TP_GlobalIgnoreClassProperty(TClientDataset,'CommandText');
  TP_GlobalIgnoreClassProperty(TClientDataset,'Filename');
  TP_GlobalIgnoreClassProperty(TClientDataset,'Filter');
  TP_GlobalIgnoreClassProperty(TClientDataset,'IndexFieldnames');
  TP_GlobalIgnoreClassProperty(TClientDataset,'IndexName');
  TP_GlobalIgnoreClassProperty(TClientDataset,'MasterFields');
  TP_GlobalIgnoreClassProperty(TClientDataset,'Params');
  TP_GlobalIgnoreClassProperty(TClientDataset,'ProviderName');
end;

class procedure Translator.GetSupportedLanguageCodes(ALanguageCodes: TStrings;
  const ATextDomain: string);
begin
  if Assigned(ALanguageCodes) then
  begin
    ALanguageCodes.Clear;
    DefaultInstance.GetListOfLanguages(ATextDomain, ALanguageCodes);
  end;
end;

{ Translates all localizable properties of a given component to the current
  language. }

class procedure Translator.Translate(AComponent: TComponent);
begin
  if Assigned(AComponent.FindComponent('GNUgettextMarker')) then
    ReTranslateComponent(AComponent)
  else
    TranslateComponent(AComponent);
end;
{$ENDREGION}

initialization
  HookFormOnCreate := THook.Create(@TGGForm.DoCreate, @TGGForm.GGDoCreate);
  HookDataModuleOnCreate :=
    THook.Create(@TGGDataModule.DoCreate, @TGGDataModule.GGDoCreate);

finalization
  FreeAndNil(HookDataModuleOnCreate);
  FreeAndNil(HookFormOnCreate);

end.
