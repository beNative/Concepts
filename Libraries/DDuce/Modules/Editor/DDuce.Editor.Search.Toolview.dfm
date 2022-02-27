inherited frmSearchForm: TfrmSearchForm
  Left = 498
  Top = 33
  BorderIcons = [biSystemMenu]
  Caption = 'Find'
  ClientHeight = 645
  ClientWidth = 309
  Constraints.MinHeight = 312
  Constraints.MinWidth = 300
  PopupMode = pmAuto
  ShowHint = True
  OnHide = FormHide
  OnShow = FormShow
  ExplicitWidth = 325
  ExplicitHeight = 684
  PixelsPerInch = 96
  TextHeight = 13
  object pnlOperations: TPanel
    Left = 0
    Top = 0
    Width = 309
    Height = 372
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      309
      372)
    object grpSearchText: TGroupBox
      Left = 0
      Top = 0
      Width = 301
      Height = 51
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Text to find:'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      DesignSize = (
        301
        51)
      object cbxSearchText: TComboBox
        Left = 14
        Top = 20
        Width = 277
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnChange = cbxSearchTextChange
      end
    end
    object grpReplaceWith: TGroupBox
      Left = 0
      Top = 55
      Width = 301
      Height = 51
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Replace with:'
      TabOrder = 1
      DesignSize = (
        301
        51)
      object cbxReplaceWith: TComboBox
        Left = 14
        Top = 20
        Width = 277
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
    object grpOptions: TGroupBox
      Left = 0
      Top = 112
      Width = 301
      Height = 71
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Options'
      TabOrder = 2
      object chkWholeWordsOnly: TCheckBox
        Left = 20
        Top = 12
        Width = 120
        Height = 30
        Caption = 'Whole words only'
        Constraints.MinHeight = 30
        Constraints.MinWidth = 120
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = chkClick
      end
      object chkCaseSensitive: TCheckBox
        Left = 20
        Top = 44
        Width = 120
        Height = 18
        Caption = 'Case sensitive'
        Constraints.MinWidth = 120
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = chkClick
      end
      object chkRegularExpressions: TCheckBox
        Left = 138
        Top = 12
        Width = 135
        Height = 30
        Caption = 'Regular expressions'
        Constraints.MinWidth = 120
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = chkClick
      end
      object chkMultiLine: TCheckBox
        Left = 138
        Top = 44
        Width = 135
        Height = 18
        Caption = 'Multiline'
        Constraints.MinWidth = 120
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = chkClick
      end
    end
    object grpScope: TGroupBox
      Left = 0
      Top = 182
      Width = 301
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Scope'
      TabOrder = 3
      object rbSelection: TRadioButton
        Left = 20
        Top = 24
        Width = 90
        Height = 18
        Caption = '&Selection'
        Constraints.MinWidth = 90
        TabOrder = 0
        OnClick = rbSelectionClick
      end
      object rbActiveView: TRadioButton
        Left = 109
        Top = 24
        Width = 90
        Height = 18
        Caption = 'Active &view'
        Constraints.MinWidth = 90
        TabOrder = 1
        OnClick = rbActiveViewClick
      end
      object rbAllViews: TRadioButton
        Left = 201
        Top = 24
        Width = 90
        Height = 18
        Hint = 'Search in all views.'
        Caption = '&All views'
        Checked = True
        Constraints.MinWidth = 90
        TabOrder = 2
        TabStop = True
        OnClick = rbActiveViewClick
      end
    end
    object grpOrigin: TGroupBox
      Left = 0
      Top = 236
      Width = 301
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Origin'
      TabOrder = 4
      object rbFromCursor: TRadioButton
        Left = 20
        Top = 24
        Width = 127
        Height = 18
        Caption = '&From cursor'
        Constraints.MinWidth = 120
        TabOrder = 0
        OnClick = rbFromCursorClick
      end
      object rbEntireScope: TRadioButton
        Left = 147
        Top = 24
        Width = 126
        Height = 18
        Caption = '&Entire scope'
        Checked = True
        Constraints.MinWidth = 120
        TabOrder = 1
        TabStop = True
        OnClick = rbEntireScopeClick
      end
    end
    object grpDirection: TGroupBox
      Left = 0
      Top = 292
      Width = 301
      Height = 56
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Direction'
      TabOrder = 5
      object rbBackward: TRadioButton
        Left = 14
        Top = 24
        Width = 127
        Height = 18
        Caption = 'Backward'
        Constraints.MinWidth = 120
        TabOrder = 0
      end
      object rbForward: TRadioButton
        Left = 147
        Top = 24
        Width = 126
        Height = 18
        Caption = 'Forward'
        Checked = True
        Constraints.MinWidth = 120
        TabOrder = 1
        TabStop = True
        OnClick = rbForwardClick
      end
    end
    object pnlButtons: TPanel
      Left = 0
      Top = 349
      Width = 297
      Height = 23
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 6
      object btnFind: TBitBtn
        Left = 0
        Top = 0
        Width = 98
        Height = 23
        Action = actFind
        Caption = 'Find'
        Constraints.MinWidth = 90
        Default = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Spacing = 0
        TabOrder = 0
      end
      object btnReplace: TBitBtn
        Left = 101
        Top = 0
        Width = 98
        Height = 23
        Action = actReplace
        Caption = 'Replace'
        Constraints.MinWidth = 90
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Spacing = 0
        TabOrder = 1
      end
      object btnReplaceAll: TBitBtn
        Left = 200
        Top = 0
        Width = 97
        Height = 23
        Action = actReplaceAll
        Caption = 'Replace all'
        Constraints.MinWidth = 90
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Spacing = 0
        TabOrder = 2
      end
    end
  end
  object pnlResultList: TPanel
    Left = 0
    Top = 372
    Width = 309
    Height = 250
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 411
    ExplicitHeight = 211
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 622
    Width = 309
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
  end
  object aclMain: TActionList
    Left = 16
    Top = 424
    object actFocusSearchText: TAction
      Caption = 'actFocusSearchText'
      ImageIndex = 1
      ShortCut = 113
      OnExecute = actFocusSearchTextExecute
    end
    object actFind: TAction
      Caption = 'Find'
      OnExecute = actFindExecute
    end
    object actReplace: TAction
      Caption = 'Replace'
      OnExecute = actReplaceExecute
    end
    object actReplaceAll: TAction
      Caption = 'Replace all'
      OnExecute = actReplaceAllExecute
    end
  end
end
