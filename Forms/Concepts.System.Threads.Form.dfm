object frmThreads: TfrmThreads
  Left = 0
  Top = 0
  Caption = 'Threads'
  ClientHeight = 178
  ClientWidth = 395
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    395
    178)
  PixelsPerInch = 96
  TextHeight = 13
  object lbxCounters: TListBox
    Left = 8
    Top = 130
    Width = 379
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    Columns = 20
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ItemHeight = 13
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 0
  end
  object btnCreateAnonymousThreads: TButton
    Left = 8
    Top = 8
    Width = 252
    Height = 25
    Action = actCreateAnonymousThreads
    TabOrder = 1
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 159
    Width = 395
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 200
      end>
    StyleElements = [seFont, seClient]
  end
  object btnTerminateThreads: TButton
    Left = 8
    Top = 39
    Width = 252
    Height = 25
    Action = actTerminateThreads
    TabOrder = 3
  end
  object btnMonitorEnter: TButton
    Left = 8
    Top = 70
    Width = 116
    Height = 25
    Action = actMonitorEnter
    TabOrder = 4
  end
  object btnMonitorExit: TButton
    Left = 130
    Top = 70
    Width = 130
    Height = 25
    Action = actMonitorExit
    TabOrder = 5
  end
  object btnMonitorPulse: TButton
    Left = 8
    Top = 101
    Width = 116
    Height = 25
    Action = actMonitorPulse
    TabOrder = 6
  end
  object btnMonitorPulseAll: TButton
    Left = 130
    Top = 101
    Width = 130
    Height = 25
    Action = actMonitorPulseAll
    TabOrder = 7
  end
  object lbxThreads: TCheckListBox
    Left = 266
    Top = 8
    Width = 121
    Height = 118
    Anchors = [akLeft, akTop, akBottom]
    Columns = 3
    ItemHeight = 13
    TabOrder = 8
  end
  object aclMain: TActionList
    Left = 312
    Top = 32
    object actCreateAnonymousThreads: TAction
      Caption = 'Create anonymous threads'
      Hint = 
        'Creates 20 threads which start counting down from 10 and stop wh' +
        'en they reach 0.'
      OnExecute = actCreateAnonymousThreadsExecute
    end
    object actTerminateThreads: TAction
      Caption = 'Terminate all threads'
      OnExecute = actTerminateThreadsExecute
    end
    object actMonitorEnter: TAction
      Caption = 'TMonitor.Enter(FLock)'
      OnExecute = actMonitorEnterExecute
    end
    object actMonitorExit: TAction
      Caption = 'TMonitor.Exit(FLock)'
      OnExecute = actMonitorExitExecute
    end
    object actMonitorPulse: TAction
      Caption = 'TMonitor.Pulse(FLock)'
      OnExecute = actMonitorPulseExecute
    end
    object actMonitorPulseAll: TAction
      Caption = 'TMonitor.PulseAll(FLock)'
      OnExecute = actMonitorPulseAllExecute
    end
  end
end
