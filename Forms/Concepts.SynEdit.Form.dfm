object frmSynEdit: TfrmSynEdit
  Left = 0
  Top = 0
  Caption = 'SynEdit component demo'
  ClientHeight = 716
  ClientWidth = 1164
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 329
    Top = 35
    Width = 8
    Height = 662
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
    Top = 35
    Width = 329
    Height = 662
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 29
    ExplicitHeight = 668
  end
  object pnlMain: TPanel
    Left = 337
    Top = 35
    Width = 827
    Height = 662
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 29
    ExplicitHeight = 668
    object seMain: TSynEdit
      Left = 0
      Top = 0
      Width = 827
      Height = 662
      Align = alClient
      ActiveLineColor = clYellow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
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
      BorderStyle = bsNone
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Highlighter = synPAS
      Lines.Strings = (
        '{'
        '  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com'
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
        'unit Concepts.ZeroMQ.Data;'
        ''
        'interface'
        ''
        'uses'
        '  System.Classes, System.SysUtils,'
        ''
        '  ZeroMQ;'
        ''
        'const'
        '  ZMQ_DEFAULT_PORT = 5555;'
        ''
        'type'
        '  TZeroMQDevice = class(TInterfacedObject)'
        '  private'
        '    FZMQ  : IZeroMQ;'
        '    FPair : IZMQPair;'
        '    FPort : Integer;'
        '    FName : string;'
        ''
        '    {$REGION '#39'property access methods'#39'}'
        '    function GetPort: Integer;'
        '    procedure SetPort(const Value: Integer);'
        '    function GetName: string;'
        '    procedure SetName(const Value: string);'
        '    {$ENDREGION}'
        ''
        '  public'
        '    procedure AfterConstruction; override;'
        '    constructor Create(const AZMQ: IZeroMQ);'
        ''
        '    function ToString: string; override;'
        ''
        '    property ZMQ: IZeroMQ'
        '      read FZMQ write FZMQ;'
        ''
        '    property Pair: IZMQPair'
        '      read FPair write FPair;'
        ''
        '    property Port: Integer'
        '      read GetPort write SetPort default ZMQ_DEFAULT_PORT;'
        ''
        '    property Name: string'
        '      read GetName write SetName;'
        '  end;'
        ''
        'const'
        '  ZMQTransports : array[0..4] of string = ('
        '    '#39'tcp'#39','
        
          '    '#39'inproc'#39', // every connection needs to share the same IZeroM' +
          'Q'
        '    '#39'ipc'#39','
        '    '#39'pgm'#39','
        '    '#39'egm'#39
        '  );'
        '  ZMQEventNames : array[ZMQEvent] of string = ('
        '    '#39'Connected'#39','
        '    '#39'Delayed'#39','
        '    '#39'Retried'#39','
        '    '#39'Listening'#39','
        '    '#39'BindFailed'#39','
        '    '#39'Accepted'#39','
        '    '#39'AcceptFailed'#39','
        '    '#39'Closed'#39','
        '    '#39'CloseFailed'#39','
        '    '#39'Disconnected'#39','
        '    '#39'MonitorStopped'#39
        '  );'
        ''
        'implementation'
        ''
        '{$REGION '#39'construction and destruction'#39'}'
        'procedure TZeroMQDevice.AfterConstruction;'
        'begin'
        '  inherited AfterConstruction;'
        '  FPort := ZMQ_DEFAULT_PORT;'
        'end;'
        ''
        'constructor TZeroMQDevice.Create(const AZMQ: IZeroMQ);'
        'begin'
        '  FZMQ := AZMQ;'
        'end;'
        '{$ENDREGION}'
        ''
        '{$REGION '#39'property access methods'#39'}'
        'function TZeroMQDevice.GetName: string;'
        'begin'
        '  Result := FName;'
        'end;'
        ''
        'procedure TZeroMQDevice.SetName(const Value: string);'
        'begin'
        '  FName := Value;'
        'end;'
        ''
        'function TZeroMQDevice.GetPort: Integer;'
        'begin'
        '  Result := FPort;'
        'end;'
        ''
        'procedure TZeroMQDevice.SetPort(const Value: Integer);'
        'begin'
        '  FPort := Value;'
        'end;'
        '{$ENDREGION}'
        ''
        '{$REGION '#39'public methods'#39'}'
        'function TZeroMQDevice.ToString: string;'
        'begin'
        '//'
        'end;'
        '{$ENDREGION}'
        ''
        'end.')
      SearchEngine = sesSearch
      FontSmoothing = fsmClearType
      ExplicitHeight = 668
    end
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1158
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
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 1164
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
