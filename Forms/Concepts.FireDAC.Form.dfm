object frmFireDAC: TfrmFireDAC
  Left = 0
  Top = 0
  Caption = 'FireDAC'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    852
    411)
  PixelsPerInch = 96
  TextHeight = 13
  object grpConnectionSettings: TGroupBox
    Left = 315
    Top = 71
    Width = 489
    Height = 154
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Database connection &settings'
    TabOrder = 0
    TabStop = True
    DesignSize = (
      489
      154)
    object lblDriverID: TLabel
      Left = 12
      Top = 20
      Width = 47
      Height = 13
      Caption = '&Driver ID:'
      FocusControl = cbxDrivers
    end
    object lblDatabase: TLabel
      Left = 12
      Top = 47
      Width = 50
      Height = 13
      Caption = 'Data&base:'
      FocusControl = edtDatabase
    end
    object lblCatalog: TLabel
      Left = 12
      Top = 75
      Width = 41
      Height = 13
      Caption = '&Catalog:'
      FocusControl = edtCatalog
    end
    object lblConnectionDefinitionName: TLabel
      Left = 175
      Top = 20
      Width = 105
      Height = 13
      Caption = 'Connection definition:'
      FocusControl = cbxConnectionDefs
    end
    object cbxDrivers: TComboBox
      Left = 76
      Top = 17
      Width = 93
      Height = 21
      DropDownCount = 30
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object btnConnectionString: TButton
      Left = 441
      Top = 15
      Width = 40
      Height = 25
      Anchors = [akTop, akRight]
      ImageAlignment = iaCenter
      ImageIndex = 9
      TabOrder = 1
      WordWrap = True
    end
    object edtDatabase: TButtonedEdit
      Left = 76
      Top = 44
      Width = 405
      Height = 21
      Hint = 'Database (server or path).'
      Anchors = [akLeft, akTop, akRight]
      RightButton.ImageIndex = 10
      RightButton.Visible = True
      TabOrder = 2
    end
    object edtCatalog: TButtonedEdit
      Left = 76
      Top = 72
      Width = 405
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      RightButton.Enabled = False
      RightButton.ImageIndex = 10
      TabOrder = 3
    end
    object grpDBMSUserLogin: TGroupBox
      Left = 8
      Top = 99
      Width = 474
      Height = 47
      Anchors = [akLeft, akTop, akRight]
      Caption = 'DBMS user &login'
      TabOrder = 4
      DesignSize = (
        474
        47)
      object chkOSAuthent: TCheckBox
        Left = 13
        Top = 21
        Width = 130
        Height = 17
        Hint = 
          'Enable this use Windows authentication. If not checked, DBMS aut' +
          'hentication is used.'
        Caption = 'Use &OS authentication'
        TabOrder = 0
      end
      object pnlLogin: TGridPanel
        Left = 155
        Top = 17
        Width = 314
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        Caption = 'pnlLogin'
        Color = clWindow
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 65.000000000000000000
          end
          item
            Value = 54.248366013071890000
          end
          item
            SizeStyle = ssAbsolute
            Value = 60.000000000000000000
          end
          item
            Value = 45.751633986928110000
          end>
        ControlCollection = <
          item
            Column = 1
            Control = edtUserName
            Row = 0
          end
          item
            Column = 2
            Control = lblPassword
            Row = 0
          end
          item
            Column = 3
            Control = edtPassword
            Row = 0
          end
          item
            Column = 0
            Control = lblUserName
            Row = 0
          end>
        ParentBackground = False
        RowCollection = <
          item
            Value = 100.000000000000000000
          end
          item
            SizeStyle = ssAuto
          end>
        ShowCaption = False
        TabOrder = 1
        object edtUserName: TEdit
          AlignWithMargins = True
          Left = 66
          Top = 1
          Width = 100
          Height = 21
          Hint = 'The DBMS server login name.'
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          TabOrder = 0
        end
        object lblPassword: TLabel
          AlignWithMargins = True
          Left = 169
          Top = 2
          Width = 56
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          AutoSize = False
          Caption = '&Password:'
          Layout = tlCenter
          ExplicitLeft = 184
          ExplicitTop = -1
          ExplicitWidth = 64
        end
        object edtPassword: TEdit
          AlignWithMargins = True
          Left = 228
          Top = 1
          Width = 85
          Height = 21
          Hint = 'The DBMS server login password.'
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          TabOrder = 1
        end
        object lblUserName: TLabel
          Left = 0
          Top = 0
          Width = 65
          Height = 23
          Align = alClient
          Caption = '&Username:'
          Layout = tlCenter
          ExplicitWidth = 52
          ExplicitHeight = 13
        end
      end
    end
    object cbxConnectionDefs: TComboBox
      Left = 286
      Top = 17
      Width = 147
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 30
      TabOrder = 5
    end
  end
  object grpClientSettings: TGroupBox
    Left = 315
    Top = 231
    Width = 488
    Height = 135
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Client settings'
    TabOrder = 1
    TabStop = True
    object lblPacketrecords: TLabel
      Left = 139
      Top = 23
      Width = 75
      Height = 13
      Caption = 'Packet &records:'
      FocusControl = edtPacketRecords
    end
    object edtPacketRecords: TEdit
      Left = 220
      Top = 17
      Width = 58
      Height = 21
      Alignment = taCenter
      TabOrder = 0
      Text = '100'
    end
    object chkFetchOnDemand: TCheckBox
      Left = 10
      Top = 21
      Width = 100
      Height = 17
      Caption = '&Fetch on demand'
      Checked = True
      DoubleBuffered = False
      ParentDoubleBuffered = False
      State = cbChecked
      TabOrder = 1
    end
    object chkAutoReconnect: TCheckBox
      Left = 10
      Top = 39
      Width = 337
      Height = 17
      Hint = 
        'When enabled, the automatic connection recovery will detect when' +
        ' a '#13#10'connection has been lost and will try to recover from this ' +
        'situation.'
      Caption = 'Automatically restore database connection after connection loss.'
      TabOrder = 2
    end
    object chkMultipleResultSets: TCheckBox
      Left = 10
      Top = 58
      Width = 425
      Height = 17
      Caption = 
        'Enable support for multiple resultsets (all fetched data will be' +
        ' readonly) '
      TabOrder = 3
    end
    object chkReadOnlyResultSets: TCheckBox
      Left = 10
      Top = 77
      Width = 393
      Height = 17
      Caption = 'Readonly resultsets'
      TabOrder = 4
    end
    object chkDisconnectedMode: TCheckBox
      Left = 10
      Top = 95
      Width = 137
      Height = 19
      Caption = 'Disconnected mode'
      TabOrder = 5
    end
    object btnTestConnection: TButton
      Left = 368
      Top = 16
      Width = 107
      Height = 25
      TabOrder = 6
    end
  end
  object conMain: TFDConnection
    Left = 72
    Top = 16
  end
  object qryMain: TFDQuery
    Connection = conMain
    Left = 184
    Top = 16
  end
  object aclMain: TActionList
    Left = 104
    Top = 168
    object actEditConnectionDefinition: TAction
      Caption = 'actEditConnectionDefinition'
      OnExecute = actEditConnectionDefinitionExecute
    end
  end
end
