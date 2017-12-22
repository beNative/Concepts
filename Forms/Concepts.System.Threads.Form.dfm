object frmThreads: TfrmThreads
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Threads'
  ClientHeight = 203
  ClientWidth = 459
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
    459
    203)
  PixelsPerInch = 96
  TextHeight = 13
  object lbxCounters: TListBox
    Left = 8
    Top = 155
    Width = 443
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
    Top = 33
    Width = 306
    Height = 25
    Action = actCreateAnonymousThreads
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 1
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 184
    Width = 459
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
    Top = 64
    Width = 306
    Height = 25
    Action = actTerminateThreads
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 3
  end
  object btnMonitorEnter: TButton
    Left = 8
    Top = 95
    Width = 150
    Height = 25
    Action = actMonitorEnter
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 4
  end
  object btnMonitorExit: TButton
    Left = 164
    Top = 95
    Width = 150
    Height = 25
    Action = actMonitorExit
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 5
  end
  object btnMonitorPulse: TButton
    Left = 8
    Top = 126
    Width = 150
    Height = 25
    Action = actMonitorPulse
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 6
  end
  object btnMonitorPulseAll: TButton
    Left = 164
    Top = 126
    Width = 150
    Height = 25
    Action = actMonitorPulseAll
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 7
  end
  object lbxThreads: TCheckListBox
    Left = 320
    Top = 33
    Width = 131
    Height = 118
    Anchors = [akLeft, akTop, akBottom]
    Columns = 3
    ItemHeight = 13
    TabOrder = 8
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 459
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 9
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 459
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 
        'This form demonstrates how TMonitor works in a multithreaded sce' +
        'nario.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 414
      ExplicitHeight = 13
    end
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 392
    Top = 80
    object actCreateAnonymousThreads: TAction
      Caption = 'Create anonymous threads'
      Hint = 
        'Creates 20 threads which start counting down from 10 and stop wh' +
        'en they reach 0.'
      ImageIndex = 61
      OnExecute = actCreateAnonymousThreadsExecute
    end
    object actTerminateThreads: TAction
      Caption = 'Terminate all threads'
      ImageIndex = 177
      OnExecute = actTerminateThreadsExecute
    end
    object actMonitorEnter: TAction
      Caption = 'TMonitor.Enter(FLock)'
      ImageIndex = 530
      OnExecute = actMonitorEnterExecute
    end
    object actMonitorExit: TAction
      Caption = 'TMonitor.Exit(FLock)'
      ImageIndex = 532
      OnExecute = actMonitorExitExecute
    end
    object actMonitorPulse: TAction
      Caption = 'TMonitor.Pulse(FLock)'
      ImageIndex = 247
      OnExecute = actMonitorPulseExecute
    end
    object actMonitorPulseAll: TAction
      Caption = 'TMonitor.PulseAll(FLock)'
      ImageIndex = 251
      OnExecute = actMonitorPulseAllExecute
    end
  end
end
