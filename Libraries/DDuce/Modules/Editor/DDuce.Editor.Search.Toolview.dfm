inherited frmSearchForm: TfrmSearchForm
  Left = 266
  Top = 266
  Margins.Left = 1
  Margins.Top = 1
  Margins.Right = 1
  Margins.Bottom = 1
  BorderIcons = [biSystemMenu]
  Caption = 'Find'
  ClientHeight = 413
  ClientWidth = 298
  Constraints.MinHeight = 139
  Constraints.MinWidth = 133
  PopupMode = pmAuto
  StyleElements = [seFont, seClient, seBorder]
  OnHide = FormHide
  OnShow = FormShow
  ExplicitWidth = 314
  ExplicitHeight = 452
  TextHeight = 15
  object pnlOperations: TPanel
    Left = 0
    Top = 0
    Width = 298
    Height = 241
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      298
      241)
    object grpSearchText: TGroupBox
      Left = 0
      Top = 0
      Width = 293
      Height = 27
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Text to find:'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      ExplicitWidth = 287
      DesignSize = (
        293
        27)
      object cbxSearchText: TComboBox
        Left = 9
        Top = 11
        Width = 282
        Height = 14
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -5
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnChange = cbxSearchTextChange
        ExplicitWidth = 276
      end
    end
    object grpReplaceWith: TGroupBox
      Left = 0
      Top = 25
      Width = 293
      Height = 22
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Replace with:'
      TabOrder = 1
      ExplicitWidth = 287
      DesignSize = (
        293
        22)
      object cbxReplaceWith: TComboBox
        Left = 6
        Top = 9
        Width = 282
        Height = 23
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        ExplicitWidth = 276
      end
    end
    object grpOptions: TGroupBox
      Left = 0
      Top = 50
      Width = 293
      Height = 31
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Options'
      TabOrder = 2
      ExplicitWidth = 287
      object chkWholeWordsOnly: TCheckBox
        Left = 9
        Top = 5
        Width = 53
        Height = 14
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = 'Whole words only'
        Constraints.MinHeight = 13
        Constraints.MinWidth = 53
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = chkClick
      end
      object chkCaseSensitive: TCheckBox
        Left = 9
        Top = 19
        Width = 53
        Height = 8
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = 'Case sensitive'
        Constraints.MinWidth = 53
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = chkClick
      end
      object chkRegularExpressions: TCheckBox
        Left = 61
        Top = 5
        Width = 60
        Height = 14
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = 'Regular expressions'
        Constraints.MinWidth = 53
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = chkClick
      end
      object chkMultiLine: TCheckBox
        Left = 61
        Top = 19
        Width = 60
        Height = 8
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = 'Multiline'
        Constraints.MinWidth = 53
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = chkClick
      end
    end
    object grpScope: TGroupBox
      Left = 0
      Top = 81
      Width = 293
      Height = 26
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Scope'
      TabOrder = 3
      ExplicitWidth = 287
      object rbSelection: TRadioButton
        Left = 9
        Top = 11
        Width = 40
        Height = 8
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = '&Selection'
        Constraints.MinWidth = 40
        TabOrder = 0
        OnClick = rbSelectionClick
      end
      object rbActiveView: TRadioButton
        Left = 49
        Top = 11
        Width = 40
        Height = 8
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = 'Active &view'
        Constraints.MinWidth = 40
        TabOrder = 1
        OnClick = rbActiveViewClick
      end
      object rbAllViews: TRadioButton
        Left = 89
        Top = 11
        Width = 40
        Height = 8
        Hint = 'Search in all views.'
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = '&All views'
        Checked = True
        Constraints.MinWidth = 40
        TabOrder = 2
        TabStop = True
        OnClick = rbActiveViewClick
      end
    end
    object grpOrigin: TGroupBox
      Left = 0
      Top = 105
      Width = 293
      Height = 40
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Origin'
      TabOrder = 4
      ExplicitWidth = 287
      object rbFromCursor: TRadioButton
        Left = 9
        Top = 11
        Width = 56
        Height = 8
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = '&From cursor'
        Constraints.MinWidth = 53
        TabOrder = 0
        OnClick = rbFromCursorClick
      end
      object rbEntireScope: TRadioButton
        Left = 65
        Top = 11
        Width = 56
        Height = 8
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = '&Entire scope'
        Checked = True
        Constraints.MinWidth = 53
        TabOrder = 1
        TabStop = True
        OnClick = rbEntireScopeClick
      end
    end
    object grpDirection: TGroupBox
      Left = 0
      Top = 180
      Width = 298
      Height = 61
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alBottom
      Caption = 'Direction'
      TabOrder = 5
      ExplicitTop = 179
      object rbBackward: TRadioButton
        Left = 6
        Top = 30
        Width = 57
        Height = 8
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = 'Backward'
        Constraints.MinWidth = 53
        TabOrder = 0
      end
      object rbForward: TRadioButton
        Left = 65
        Top = 11
        Width = 56
        Height = 8
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Caption = 'Forward'
        Checked = True
        Constraints.MinWidth = 53
        TabOrder = 1
        TabStop = True
        OnClick = rbForwardClick
      end
    end
    object pnlButtons: TPanel
      Left = 0
      Top = 155
      Width = 167
      Height = 12
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 6
      object btnFind: TBitBtn
        Left = 0
        Top = 0
        Width = 43
        Height = 10
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Action = actFind
        Caption = 'Find'
        Constraints.MinWidth = 40
        Default = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Spacing = 0
        TabOrder = 0
      end
      object btnReplace: TBitBtn
        Left = 45
        Top = 0
        Width = 44
        Height = 10
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Action = actReplace
        Caption = 'Replace'
        Constraints.MinWidth = 40
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Spacing = 0
        TabOrder = 1
      end
      object btnReplaceAll: TBitBtn
        Left = 111
        Top = 2
        Width = 56
        Height = 10
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Action = actReplaceAll
        Caption = 'Replace all'
        Constraints.MinWidth = 40
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
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
    Top = 241
    Width = 298
    Height = 162
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 256
    ExplicitHeight = 147
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 403
    Width = 298
    Height = 10
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alBottom
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -5
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    ExplicitTop = 386
    ExplicitWidth = 292
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
