object frmZMQConcept: TfrmZMQConcept
  Left = 0
  Top = 0
  Caption = 'TP'
  ClientHeight = 653
  ClientWidth = 738
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    738
    653)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlClient: TPanel
    Left = 8
    Top = 8
    Width = 420
    Height = 521
    BevelOuter = bvNone
    TabOrder = 0
    object edtAddress: TLabeledEdit
      Left = 56
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
      Top = 199
      Width = 249
      Height = 89
      Lines.Strings = (
        'Test')
      TabOrder = 1
    end
    object edtPort: TLabeledEdit
      Left = 186
      Top = 32
      Width = 71
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
      Left = 124
      Top = 168
      Width = 75
      Height = 25
      Action = actSend
      TabOrder = 4
    end
    object btnReceive: TButton
      Left = 124
      Top = 294
      Width = 75
      Height = 25
      Action = actReceive
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
    object pnlZMQSocket: TPanel
      Left = 7
      Top = 148
      Width = 98
      Height = 374
      BevelOuter = bvNone
      TabOrder = 7
      DesignSize = (
        98
        374)
      object shpRequesterResponder: TShape
        AlignWithMargins = True
        Left = 3
        Top = 104
        Width = 90
        Height = 54
        Margins.Top = 1
        Margins.Right = 5
        Margins.Bottom = 1
        Align = alTop
        Brush.Color = clYellow
        Pen.Color = clWhite
        Pen.Style = psClear
        ExplicitLeft = 29
        ExplicitTop = 165
        ExplicitWidth = 87
      end
      object shpStream: TShape
        AlignWithMargins = True
        Left = 3
        Top = 328
        Width = 90
        Height = 36
        Margins.Top = 1
        Margins.Right = 5
        Margins.Bottom = 1
        Align = alTop
        Brush.Color = clBtnFace
        Pen.Color = clWhite
        Pen.Style = psClear
        ExplicitLeft = 4
        ExplicitTop = 329
      end
      object shpPullPush: TShape
        AlignWithMargins = True
        Left = 3
        Top = 216
        Width = 90
        Height = 54
        Margins.Top = 1
        Margins.Right = 5
        Margins.Bottom = 1
        Align = alTop
        Brush.Color = 16761343
        Pen.Color = clWhite
        Pen.Style = psClear
        ExplicitLeft = 29
        ExplicitTop = 179
        ExplicitWidth = 87
      end
      object shpPublisherSubscriber: TShape
        AlignWithMargins = True
        Left = 3
        Top = 48
        Width = 90
        Height = 54
        Margins.Top = 1
        Margins.Right = 5
        Margins.Bottom = 1
        Align = alTop
        Brush.Color = 16764622
        Pen.Color = clWhite
        Pen.Style = psClear
        ExplicitLeft = 30
        ExplicitTop = 109
        ExplicitWidth = 87
      end
      object shpDealerRouter: TShape
        AlignWithMargins = True
        Left = 3
        Top = 160
        Width = 90
        Height = 54
        Margins.Top = 1
        Margins.Right = 5
        Margins.Bottom = 1
        Align = alTop
        Pen.Color = clWhite
        Pen.Style = psClear
        ExplicitLeft = 37
        ExplicitTop = 179
        ExplicitWidth = 87
      end
      object shpXPublisherXSubscriber: TShape
        AlignWithMargins = True
        Left = 3
        Top = 272
        Width = 90
        Height = 54
        Margins.Top = 1
        Margins.Right = 5
        Margins.Bottom = 1
        Align = alTop
        Brush.Color = 12910532
        Pen.Color = clWhite
        Pen.Style = psClear
        ExplicitLeft = 30
        ExplicitTop = 179
        ExplicitWidth = 87
      end
      object shpPair: TShape
        AlignWithMargins = True
        Left = 3
        Top = 19
        Width = 90
        Height = 27
        Margins.Top = 1
        Margins.Right = 5
        Margins.Bottom = 1
        Align = alTop
        Brush.Color = clBtnFace
        Pen.Color = clWhite
        Pen.Style = psClear
        ExplicitLeft = 4
        ExplicitTop = 27
      end
      object shpSpacer: TShape
        AlignWithMargins = True
        Left = 3
        Top = 1
        Width = 90
        Height = 16
        Margins.Top = 1
        Margins.Right = 5
        Margins.Bottom = 1
        Align = alTop
        Brush.Color = clBtnFace
        Pen.Color = clWhite
        Pen.Style = psClear
        ExplicitLeft = 6
      end
      object rgpZMQSocket: TRadioGroup
        Left = 4
        Top = 3
        Width = 91
        Height = 365
        Anchors = [akLeft, akTop, akRight]
        Ctl3D = True
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
        ParentCtl3D = False
        TabOrder = 0
      end
    end
    object edtFilter: TLabeledEdit
      Left = 56
      Top = 59
      Width = 89
      Height = 21
      Alignment = taCenter
      EditLabel.Width = 28
      EditLabel.Height = 13
      EditLabel.Caption = 'Filter:'
      LabelPosition = lpLeft
      TabOrder = 8
    end
    object mmoReceive: TMemo
      Left = 124
      Top = 325
      Width = 249
      Height = 89
      ReadOnly = True
      TabOrder = 9
    end
    object lbxEvents: TCheckListBox
      Left = 145
      Top = 117
      Width = 117
      Height = 153
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
      TabOrder = 10
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
      TabOrder = 11
      ExplicitLeft = 6
      ExplicitTop = 0
      ExplicitWidth = 406
    end
    object btnSubscribe: TButton
      Left = 182
      Top = 72
      Width = 75
      Height = 25
      Action = actSubscribe
      TabOrder = 12
    end
    object rgpTransport: TRadioGroup
      Left = 263
      Top = 56
      Width = 156
      Height = 65
      Caption = 'Transport'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'tcp'
        'inproc'
        'ipc'
        'pgm'
        'epgm')
      TabOrder = 13
    end
    object btnCreateNew: TButton
      Left = 268
      Top = 127
      Width = 144
      Height = 25
      Action = actCreateNew
      TabOrder = 14
    end
  end
  object mmoLog: TMemo
    Left = 434
    Top = 8
    Width = 296
    Height = 637
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object aclMain: TActionList
    Left = 344
    Top = 176
    object actConnect: TAction
      Caption = 'Connect'
      OnExecute = actConnectExecute
    end
    object actSend: TAction
      Caption = 'Send'
      OnExecute = actSendExecute
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
  end
end
