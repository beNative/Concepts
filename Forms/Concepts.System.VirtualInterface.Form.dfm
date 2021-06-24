object frmVirtualInterfaceDemo: TfrmVirtualInterfaceDemo
  Left = 0
  Top = 0
  Caption = 'TVirtualInterface demo'
  ClientHeight = 120
  ClientWidth = 323
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object lblDefinition: TLabel
    Left = 4
    Top = 3
    Width = 314
    Height = 78
    BiDiMode = bdLeftToRight
    Caption = 
      'type'#13#10'  IGoStop = interface(IInvokable)'#13#10'  ['#39'{DAA28BA6-2243-41BE' +
      '-BC61-2A548999753A}'#39']'#13#10'    procedure Go(AInteger: Integer = 5);'#13 +
      #10'    procedure Stop(const AString: string = '#39'TEST'#39');'#13#10'  end;'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentBiDiMode = False
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object btnGo: TButton
    Left = 8
    Top = 89
    Width = 146
    Height = 25
    Action = actGo
    Constraints.MaxHeight = 25
    Constraints.MaxWidth = 146
    Constraints.MinHeight = 25
    Constraints.MinWidth = 146
    TabOrder = 0
  end
  object btnStop: TButton
    Left = 160
    Top = 89
    Width = 154
    Height = 25
    Action = actStop
    TabOrder = 1
  end
  object aclMain: TActionList
    Left = 232
    Top = 40
    object actGo: TAction
      Caption = 'Go'
      OnExecute = actGoExecute
    end
    object actStop: TAction
      Caption = 'Stop'
      OnExecute = actStopExecute
    end
  end
end
