object frmSynEdit: TfrmSynEdit
  Left = 0
  Top = 0
  Caption = 'SynEdit component demo'
  ClientHeight = 716
  ClientWidth = 1164
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 329
    Top = 29
    Width = 8
    Height = 668
    ExplicitTop = 0
    ExplicitHeight = 580
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 697
    Width = 1164
    Height = 19
    Panels = <>
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 29
    Width = 329
    Height = 668
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlMain: TPanel
    Left = 337
    Top = 29
    Width = 827
    Height = 668
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object seMain: TSynEdit
      Left = 0
      Top = 0
      Width = 827
      Height = 668
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Pitch = fpFixed
      Font.Style = []
      TabOrder = 0
      CodeFolding.GutterShapeSize = 11
      CodeFolding.CollapsedLineColor = clGrayText
      CodeFolding.FolderBarLinesColor = clGrayText
      CodeFolding.IndentGuidesColor = clGray
      CodeFolding.IndentGuides = True
      CodeFolding.ShowCollapsedLine = False
      CodeFolding.ShowHintMark = True
      UseCodeFolding = False
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Highlighter = synPAS
      Lines.Strings = (
        '{'
        '  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com'
        ''
        
          '  Licensed under the Apache License, Version 2.0 (the "License")' +
          ';'
        
          '  you may not use this file except in compliance with the Licens' +
          'e.'
        '  You may obtain a copy of the License at'
        ''
        '      http://www.apache.org/licenses/LICENSE-2.0'
        ''
        
          '  Unless required by applicable law or agreed to in writing, sof' +
          'tware'
        
          '  distributed under the License is distributed on an "AS IS" BAS' +
          'IS,'
        
          '  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express o' +
          'r implied.'
        
          '  See the License for the specific language governing permission' +
          's and'
        '  limitations under the License.'
        '}'
        ''
        '{$I Concepts.inc}'
        ''
        'unit Concepts.System.Threading.Form;'
        ''
        
          '{ Demonstration of the new Delphi XE7 - System.Threading unit, w' +
          'hich is also'
        '  referenced to as the PPL (Parallel Programming Library). }'
        ''
        'interface'
        ''
        'uses'
        '  Winapi.Windows, Winapi.Messages,'
        
          '  System.SysUtils, System.Variants, System.Classes, System.Actio' +
          'ns,'
        '  System.ImageList, System.Threading,'
        
          '  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnLi' +
          'st, Vcl.ImgList,'
        '  Vcl.StdCtrls, Vcl.ExtCtrls,'
        ''
        '  Spring, Spring.Collections,'
        ''
        '  DDuce.Components.LogTree;'
        ''
        'type'
        '  TfrmThreading = class(TForm)'
        '    {$REGION '#39'designer controls'#39'} '
        '    aclMain                     : TActionList;'
        '    actCancelTask               : TAction;'
        '    actExecuteParallel          : TAction;'
        '    actExecuteSequential        : TAction;'
        '    actStartTask                : TAction;'
        '    actTestIterationsAndStrides : TAction;'
        '    btnCancelTask               : TButton;'
        '    btnExecute                  : TButton;'
        '    btnExecuteSequential        : TButton;'
        '    btnStartTask                : TButton;'
        '    btnTestIterationsAndStrides : TButton;'
        '    edtIterations               : TLabeledEdit;'
        '    edtStrides                  : TLabeledEdit;'
        '    imlMain                     : TImageList;'
        '    pnlLog                      : TPanel;'
        '    {$ENDREGION}'
        ''
        '    procedure actExecuteParallelExecute(Sender: TObject);'
        '    procedure actExecuteSequentialExecute(Sender: TObject);'
        '    procedure actStartTaskExecute(Sender: TObject);'
        '    procedure actCancelTaskExecute(Sender: TObject);'
        
          '    procedure actTestIterationsAndStridesExecute(Sender: TObject' +
          ');'
        ''
        '  strict private'
        '    FProc      : TProc<Integer>;'
        '    FCount     : Integer;'
        '    FLog       : TLogTree;'
        '    FTasks     : IList<ITask>;'
        ''
        '    function GetIterations: Integer;'
        '    function GetStrides: Integer;'
        ''
        '  protected'
        '    procedure ExecuteParallel('
        '      const AProc : TProc<Integer>;'
        '      ACount      : Integer;'
        '      AStrides    : Integer'
        '    );'
        '    procedure ExecuteSequential('
        '      const AProc : TProc<Integer>;'
        '      ACount      : Integer'
        '    );'
        ''
        '  public'
        '    procedure AfterConstruction; override;'
        '    procedure BeforeDestruction; override;'
        ''
        '    property Iterations: Integer'
        '      read GetIterations;'
        ''
        
          '    { Amount of executions that should be executed by each threa' +
          'd. }'
        '    property Strides: Integer'
        '      read GetStrides;'
        '  end;'
        ''
        'implementation'
        ''
        '{$R *.dfm}'
        ''
        'uses'
        '  System.Diagnostics,'
        ''
        '  DDuce.Components.Factories, DDuce.RandomData;'
        ''
        'resourcestring'
        '  STaskStarted                  = '#39'Task %d started.'#39';'
        '  STaskFinished                 = '#39'Task %d finished.'#39';'
        
          '  SFinishedParallelExecutions   = '#39'Finished %d/%d parallel execu' +
          'tions in %dms.'#39';'
        
          '  SFinishedSequentialExecutions = '#39'Finished %d sequential execut' +
          'ions in %dms.'#39';'
        ''
        '{$REGION '#39'construction and destruction'#39'}'
        'procedure TfrmThreading.AfterConstruction;'
        'begin'
        '  inherited AfterConstruction;'
        '  FProc := procedure(AIndex: Integer)'
        '    begin'
        '      Sleep(1);'
        '    end;'
        '  FLog := TDDuceComponents.CreateLogTree(Self, pnlLog);'
        '  FLog.DateTimeFormat := '#39'hh:nn:ss.zzz'#39';'
        '  FLog.Images := imlMain;'
        '  FTasks := TCollections.CreateInterfaceList<ITask>;'
        'end;'
        ''
        'procedure TfrmThreading.BeforeDestruction;'
        'begin'
        '  FLog.Free;'
        '  inherited BeforeDestruction;'
        'end;'
        '{$ENDREGION}'
        ''
        '{$REGION '#39'property access methods'#39'}'
        'function TfrmThreading.GetIterations: Integer;'
        'begin'
        '  Result := StrToIntDef(edtIterations.Text, 0);'
        'end;'
        ''
        'function TfrmThreading.GetStrides: Integer;'
        'begin'
        '  Result := StrToIntDef(edtStrides.Text, 0);'
        'end;'
        '{$ENDREGION}'
        ''
        '{$REGION '#39'action handlers'#39'}'
        'procedure TfrmThreading.actCancelTaskExecute(Sender: TObject);'
        'begin'
        '  if FTasks.Last.Status = TTaskStatus.Running then'
        '    FTasks.Last.Cancel;'
        'end;'
        ''
        
          'procedure TfrmThreading.actExecuteParallelExecute(Sender: TObjec' +
          't);'
        'begin'
        '  TTask.Run(procedure'
        '    begin'
        '      ExecuteParallel(FProc, Iterations, Strides);'
        '    end'
        '  );'
        'end;'
        ''
        
          'procedure TfrmThreading.actExecuteSequentialExecute(Sender: TObj' +
          'ect);'
        'begin'
        '  TTask.Run(procedure'
        '    begin'
        '      ExecuteSequential(FProc, Iterations);'
        '    end'
        '  );'
        'end;'
        ''
        'procedure TfrmThreading.actStartTaskExecute(Sender: TObject);'
        'var'
        '  C : Integer;'
        'begin'
        '  C := FTasks.Count + 1;'
        '  FLog.LogFmt(STaskStarted, [C]);'
        '  FTasks.Add('
        '    TTask.Run(procedure'
        '      begin'
        '        Sleep(5000);'
        '        TThread.Queue(TThread.Current, procedure'
        '          begin'
        '            FLog.LogFmt(STaskFinished, [C]);'
        '          end'
        '        );'
        '      end'
        '    )'
        '  );'
        'end;'
        ''
        
          'procedure TfrmThreading.actTestIterationsAndStridesExecute(Sende' +
          'r: TObject);'
        'var'
        '  N: Integer;'
        'begin'
        '  N := Iterations;'
        '  TTask.Run(procedure'
        '    begin'
        '      ExecuteParallel(FProc, N, 10);'
        '      ExecuteParallel(FProc, N, 20);'
        '      ExecuteParallel(FProc, N, 50);'
        '      ExecuteParallel(FProc, N, 100);'
        '      ExecuteParallel(FProc, N, 200);'
        '      ExecuteParallel(FProc, N, 500);'
        '      ExecuteParallel(FProc, N, 1000);'
        '      ExecuteParallel(FProc, N, 2000);'
        '      ExecuteParallel(FProc, N, 5000);'
        '    end'
        '  );'
        'end;'
        '{$ENDREGION}'
        ''
        '{$REGION '#39'protected methods'#39'}'
        
          '{ Executes the given anonymous procedure ACount times in a paral' +
          'lel for loop.'
        
          '  AStrides indicates how many executions of the loop are execute' +
          'd per thread. }'
        ''
        
          'procedure TfrmThreading.ExecuteParallel(const AProc: TProc<Integ' +
          'er>;'
        '  ACount: Integer; AStrides: Integer);'
        'var'
        '  SW : TStopwatch;'
        'begin'
        '  SW := TStopwatch.Create;'
        '  SW.Start;'
        '  FCount := 0;'
        '  TParallel.For(AStrides, 1, ACount, AProc);'
        '  SW.Stop;'
        '  TThread.Queue(TThread.CurrentThread, procedure'
        '    begin'
        '      FLog.LogFmt('
        '        SFinishedParallelExecutions,'
        '        [ACount, AStrides, SW.ElapsedMilliseconds]'
        '      );'
        '    end'
        '  );'
        'end;'
        ''
        
          '{ Executes the given anonymous procedure ACount times using a no' +
          'rmal for loop. }'
        ''
        
          'procedure TfrmThreading.ExecuteSequential(const AProc: TProc<Int' +
          'eger>;'
        '  ACount: Integer);'
        'var'
        '  SW : TStopwatch;'
        '  I  : Integer;'
        'begin'
        '  SW := TStopwatch.Create;'
        '  SW.Start;'
        '  FCount := 0;'
        '  for I := 1 to ACount do'
        '  begin'
        '    FProc(I);'
        '  end;'
        '  SW.Stop;'
        '  TThread.Queue(TThread.CurrentThread, procedure'
        '    begin'
        '      FLog.LogFmt('
        '        SFinishedSequentialExecutions,'
        '        [ACount, SW.ElapsedMilliseconds]'
        '      );'
        '    end'
        '  );'
        'end;'
        '{$ENDREGION}'
        ''
        'end.')
      SearchEngine = sesSearch
      FontSmoothing = fsmNone
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 1164
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    Caption = 'This form demonstrates the TSynEdit control.'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
  end
  object scpMain: TSynCompletionProposal
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    ShortCut = 16416
    Editor = seMain
    Left = 777
    Top = 440
  end
  object sacMain: TSynAutoComplete
    EndOfTokenChr = '()[]. '
    Editor = seMain
    ShortCut = 8224
    Options = []
    Left = 833
    Top = 504
  end
  object suoMain: TSynURIOpener
    Editor = seMain
    Left = 761
    Top = 376
  end
  object sodMain: TSynEditOptionsDialog
    UseExtendedStrings = False
    Left = 913
    Top = 304
  end
  object synINI: TSynIniSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 929
    Top = 456
  end
  object synURI: TSynURISyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 993
    Top = 560
  end
  object synPAS: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clMedGray
    DirectiveAttri.Foreground = clOlive
    DirectiveAttri.Style = [fsBold]
    KeyAttri.Foreground = clBlue
    NumberAttri.Foreground = clRed
    NumberAttri.Style = [fsBold, fsItalic]
    FloatAttri.Foreground = clFuchsia
    FloatAttri.Style = [fsBold, fsItalic]
    StringAttri.Foreground = clGreen
    StringAttri.Style = [fsBold, fsItalic]
    CharAttri.Foreground = clGreen
    CharAttri.Style = [fsBold, fsItalic]
    SymbolAttri.Foreground = clRed
    SymbolAttri.Style = [fsBold]
    Left = 1009
    Top = 416
  end
  object sesSearch: TSynEditSearch
    Left = 873
    Top = 248
  end
end
