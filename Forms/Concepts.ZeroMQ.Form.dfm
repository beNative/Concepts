object frmZMQConcept: TfrmZMQConcept
  Left = 0
  Top = 0
  ActiveControl = edtAddress
  Caption = 'TP'
  ClientHeight = 733
  ClientWidth = 917
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    917
    733)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlClient: TPanel
    Left = 8
    Top = 8
    Width = 420
    Height = 697
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      420
      697)
    object edtAddress: TLabeledEdit
      Left = 168
      Top = 32
      Width = 89
      Height = 21
      Alignment = taCenter
      EditLabel.Width = 43
      EditLabel.Height = 13
      EditLabel.Caption = 'Address:'
      LabelPosition = lpLeft
      TabOrder = 0
      Text = 'localhost'
    end
    object mmoSend: TMemo
      Left = 127
      Top = 276
      Width = 293
      Height = 124
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'Test')
      ParentFont = False
      TabOrder = 1
    end
    object edtPort: TLabeledEdit
      Left = 170
      Top = 63
      Width = 88
      Height = 21
      Alignment = taCenter
      EditLabel.Width = 24
      EditLabel.Height = 13
      EditLabel.Caption = 'Port:'
      LabelPosition = lpLeft
      TabOrder = 2
      Text = '5555'
    end
    object btnClientConnect: TButton
      Left = 263
      Top = 32
      Width = 75
      Height = 25
      Action = actConnect
      TabOrder = 3
    end
    object btnSend: TButton
      Left = 127
      Top = 245
      Width = 131
      Height = 25
      Action = actSendMemoText
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
    object btnReceive: TButton
      Left = 127
      Top = 406
      Width = 75
      Height = 25
      Action = actReceive
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object btnServerBind: TButton
      Left = 344
      Top = 32
      Width = 75
      Height = 25
      Action = actBind
      TabOrder = 6
    end
    object edtFilter: TLabeledEdit
      Left = 169
      Top = 90
      Width = 89
      Height = 21
      Alignment = taCenter
      EditLabel.Width = 28
      EditLabel.Height = 13
      EditLabel.Caption = 'Filter:'
      LabelPosition = lpLeft
      TabOrder = 7
    end
    object mmoReceive: TMemo
      Left = 127
      Top = 437
      Width = 285
      Height = 122
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 8
    end
    object lbxEvents: TCheckListBox
      Left = 4
      Top = 242
      Width = 117
      Height = 153
      OnClickCheck = lbxEventsClickCheck
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      Items.Strings = (
        'Connected'
        'Delayed'
        'Retried'
        'Listening'
        'BindFailed'
        'Accepted'
        'AcceptFailed'
        'Closed'
        'CloseFailed'
        'Disconnected'
        'MonitorStopped')
      ParentFont = False
      TabOrder = 9
    end
    object pnlConnectionString: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 414
      Height = 23
      Align = alTop
      BevelKind = bkFlat
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 10
    end
    object btnSubscribe: TButton
      Left = 264
      Top = 227
      Width = 131
      Height = 25
      Action = actSubscribe
      TabOrder = 11
    end
    object rgpTransport: TRadioGroup
      Left = 264
      Top = 63
      Width = 156
      Height = 96
      Caption = 'Transport'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'tcp'
        'inproc'
        'ipc'
        'pgm'
        'epgm')
      TabOrder = 12
    end
    object btnCreateNew: TButton
      Left = 264
      Top = 165
      Width = 156
      Height = 25
      Action = actCreateNew
      TabOrder = 13
    end
    object rgpZMQSocket: TRadioGroup
      Left = 4
      Top = 32
      Width = 102
      Height = 204
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      Items.Strings = (
        'Pair'
        'Publisher'
        'Subscriber'
        'Requester'
        'Responder'
        'Dealer'
        'Router'
        'Pull'
        'Push'
        'XPublisher'
        'XSubscriber'
        'Stream')
      TabOrder = 14
    end
    object edtPollTimeout: TLabeledEdit
      Left = 208
      Top = 119
      Width = 50
      Height = 21
      Alignment = taCenter
      EditLabel.Width = 79
      EditLabel.Height = 13
      EditLabel.Caption = 'Poll timeout (ms)'
      LabelPosition = lpLeft
      NumbersOnly = True
      TabOrder = 15
      Text = '10'
    end
    object edtCounter: TLabeledEdit
      Left = 208
      Top = 146
      Width = 50
      Height = 21
      Alignment = taCenter
      EditLabel.Width = 43
      EditLabel.Height = 13
      EditLabel.Caption = 'Counter:'
      LabelPosition = lpLeft
      NumbersOnly = True
      TabOrder = 16
      Text = '0'
      OnExit = edtCounterExit
    end
    object btnSendCounterValue: TButton
      Left = 127
      Top = 173
      Width = 131
      Height = 25
      Action = actSendCounterValue
      TabOrder = 17
    end
    object mmoIPs: TMemo
      Left = 127
      Top = 565
      Width = 285
      Height = 122
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 18
    end
  end
  object mmoLog: TMemo
    Left = 434
    Top = 8
    Width = 475
    Height = 717
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitWidth = 296
    ExplicitHeight = 559
  end
  object aclMain: TActionList
    Left = 496
    Top = 80
    object actConnect: TAction
      Caption = 'Connect'
      OnExecute = actConnectExecute
    end
    object actSendMemoText: TAction
      Caption = 'Send memotext'
      OnExecute = actSendMemoTextExecute
    end
    object actBind: TAction
      Caption = 'Bind'
      OnExecute = actBindExecute
    end
    object actReceive: TAction
      Caption = 'Receive'
      OnExecute = actReceiveExecute
    end
    object actSubscribe: TAction
      Caption = 'Subscribe'
      OnExecute = actSubscribeExecute
    end
    object actCreateNew: TAction
      Caption = 'Create new ZeroMQ node'
      OnExecute = actCreateNewExecute
    end
    object actClose: TAction
      Caption = 'Close'
      Hint = 'Close connection'
      OnExecute = actCloseExecute
    end
    object actSendCounterValue: TAction
      Caption = 'Send counter value'
      OnExecute = actSendCounterValueExecute
    end
    object actResetCounter: TAction
      Caption = 'Reset counter'
      OnExecute = actResetCounterExecute
    end
  end
end
