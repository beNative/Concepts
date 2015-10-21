object frmcxEditors: TfrmcxEditors
  Left = 308
  Top = 261
  Caption = 'cxEdit'
  ClientHeight = 601
  ClientWidth = 791
  Color = clBtnFace
  Constraints.MinHeight = 601
  Constraints.MinWidth = 799
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 233
    Top = 0
    Width = 7
    Height = 581
    ExplicitHeight = 554
  end
  object pnlRight: TPanel
    Left = 240
    Top = 0
    Width = 551
    Height = 581
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object splHorizontal: TSplitter
      Left = 0
      Top = 311
      Width = 551
      Height = 7
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 185
    end
    object ispTextEdit: TcxRTTIInspector
      Left = 0
      Top = 0
      Width = 551
      Height = 311
      BorderStyle = cxcbsNone
      Align = alClient
      InspectedObject = edtMaskEdit
      LookAndFeel.Kind = lfUltraFlat
      LookAndFeel.NativeStyle = False
      OptionsView.CellAutoHeight = True
      OptionsView.CellEndEllipsis = True
      OptionsView.CategoryExplorerStyle = True
      OptionsView.RowHeaderWidth = 280
      OptionsView.ShowEmptyRowImage = True
      OptionsView.GridLines = vglVertical
      OptionsView.ValueWidth = 150
      OptionsView.ValueMinWidth = 100
      OptionsBehavior.GoToNextCellOnEnter = True
      OptionsBehavior.GoToNextCellOnTab = True
      OptionsBehavior.BandSizing = False
      OptionsBehavior.RowSizing = True
      TabOrder = 0
      Version = 1
    end
    object pnlRightBottom: TPanel
      Left = 0
      Top = 318
      Width = 551
      Height = 263
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object redLog: TcxRichEdit
        Left = 0
        Top = 0
        Align = alClient
        Properties.HideScrollBars = False
        Properties.ReadOnly = True
        Properties.ScrollBars = ssVertical
        Properties.SelectionBar = True
        Style.BorderStyle = ebs3D
        Style.StyleController = escMain
        TabOrder = 0
        Height = 263
        Width = 336
      end
      object pnlBottomRight: TPanel
        Left = 336
        Top = 0
        Width = 215
        Height = 263
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object lblVarType: TLabel
          Left = 24
          Top = 23
          Width = 65
          Height = 13
          Caption = 'Variant Type:'
        end
        object lblVariantValue: TLabel
          Left = 24
          Top = 50
          Width = 65
          Height = 13
          Caption = 'Variant Type:'
        end
      end
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 233
    Height = 581
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      233
      581)
    object lblEdit: TLabel
      Left = 8
      Top = 8
      Width = 63
      Height = 13
      Caption = 'TcxMaskEdit:'
    end
    object lblAssignText: TLabel
      Left = 8
      Top = 218
      Width = 58
      Height = 13
      Caption = 'Assign text:'
    end
    object lblEditMask: TLabel
      Left = 8
      Top = 242
      Width = 46
      Height = 13
      Caption = 'EditMask:'
    end
    object lblEditFormat: TLabel
      Left = 8
      Top = 266
      Width = 56
      Height = 13
      Caption = 'EditFormat:'
      Visible = False
    end
    object lblDisplayFormat: TLabel
      Left = 8
      Top = 290
      Width = 72
      Height = 13
      Caption = 'DisplayFormat:'
      Visible = False
    end
    object edtString: TcxTextEdit
      Left = 80
      Top = 215
      Anchors = [akLeft, akTop, akRight]
      Style.StyleController = escMain
      TabOrder = 7
      Width = 145
    end
    object pnlStatus: TPanel
      Left = 0
      Top = 385
      Width = 233
      Height = 196
      Align = alBottom
      BevelInner = bvLowered
      BevelOuter = bvNone
      Enabled = False
      TabOrder = 13
      DesignSize = (
        233
        196)
      object lblEditValue: TLabel
        Left = 7
        Top = 12
        Width = 48
        Height = 13
        Caption = 'EditValue:'
      end
      object lblText: TLabel
        Left = 7
        Top = 106
        Width = 62
        Height = 13
        Caption = 'EditingValue:'
      end
      object lblEditingValue: TLabel
        Left = 7
        Top = 59
        Width = 26
        Height = 13
        Caption = 'Text:'
      end
      object lblEditingText: TLabel
        Left = 7
        Top = 82
        Width = 58
        Height = 13
        Caption = 'EditingText:'
      end
      object lblEditText: TLabel
        Left = 7
        Top = 36
        Width = 40
        Height = 13
        Caption = 'EditText'
      end
      object edtEditingText: TcxTextEdit
        Left = 71
        Top = 79
        Anchors = [akLeft, akTop, akRight]
        Style.BorderStyle = ebsUltraFlat
        Style.StyleController = escMain
        TabOrder = 0
        Width = 153
      end
      object edtText: TcxTextEdit
        Left = 71
        Top = 56
        Anchors = [akLeft, akTop, akRight]
        Style.BorderStyle = ebsUltraFlat
        Style.StyleController = escMain
        TabOrder = 1
        Width = 153
      end
      object edtEditingValue: TcxTextEdit
        Left = 71
        Top = 103
        Anchors = [akLeft, akTop, akRight]
        Style.BorderStyle = ebsUltraFlat
        Style.StyleController = escMain
        TabOrder = 2
        Width = 153
      end
      object edtEditValue: TcxTextEdit
        Left = 71
        Top = 9
        Anchors = [akLeft, akTop, akRight]
        Style.BorderStyle = ebsUltraFlat
        Style.StyleController = escMain
        TabOrder = 3
        Width = 153
      end
      object chkEditModified: TcxCheckBox
        Left = 8
        Top = 132
        Caption = 'EditModified'
        TabOrder = 4
        Width = 97
      end
      object chkModifiedAfterEnter: TcxCheckBox
        Left = 8
        Top = 157
        Caption = 'ModifiedAfterEnter'
        TabOrder = 5
        Width = 129
      end
      object edtEditText: TcxTextEdit
        Left = 71
        Top = 33
        Anchors = [akLeft, akTop, akRight]
        Style.BorderStyle = ebsUltraFlat
        Style.StyleController = escMain
        TabOrder = 6
        Width = 153
      end
    end
    object edtMaskEdit: TcxMaskEdit
      Left = 80
      Top = 4
      OnFocusChanged = edtMaskEditFocusChanged
      Anchors = [akLeft, akTop, akRight]
      Properties.MaskKind = emkRegExprEx
      Properties.MaxLength = 0
      Properties.OnChange = edtMaskEditPropertiesChange
      Properties.OnEditValueChanged = edtMaskEditPropertiesEditValueChanged
      Properties.OnNewLookupDisplayText = edtMaskEditPropertiesNewLookupDisplayText
      Properties.OnValidate = edtMaskEditPropertiesValidate
      Style.StyleController = escMain
      TabOrder = 0
      OnEditing = edtMaskEditEditing
      OnEnter = edtMaskEditEnter
      OnExit = edtMaskEditExit
      Width = 145
    end
    object edtEditMask: TcxTextEdit
      Left = 80
      Top = 239
      Anchors = [akLeft, akTop, akRight]
      Properties.OnEditValueChanged = edtEditMaskPropertiesEditValueChanged
      Style.StyleController = escMain
      TabOrder = 8
      Width = 145
    end
    object edtEditFormat: TcxTextEdit
      Left = 80
      Top = 263
      Anchors = [akLeft, akTop, akRight]
      Properties.OnEditValueChanged = edtEditFormatPropertiesEditValueChanged
      Style.StyleController = escMain
      TabOrder = 9
      Visible = False
      Width = 145
    end
    object edtDisplayFormat: TcxTextEdit
      Left = 80
      Top = 287
      Anchors = [akLeft, akTop, akRight]
      Properties.OnEditValueChanged = edtDisplayFormatPropertiesEditValueChanged
      Style.StyleController = escMain
      TabOrder = 10
      Visible = False
      Width = 145
    end
    object chkUseDisplayFormatWhenEditing: TCheckBox
      Left = 8
      Top = 314
      Width = 193
      Height = 17
      Caption = 'UseDisplayFormatWhenEditing'
      TabOrder = 11
      Visible = False
      OnClick = chkUseDisplayFormatWhenEditingClick
    end
    object btnAssignEditValue: TcxButton
      Left = 8
      Top = 31
      Width = 217
      Height = 25
      Action = actAssignEditValue
      Anchors = [akLeft, akTop, akRight]
      LookAndFeel.Kind = lfStandard
      LookAndFeel.NativeStyle = True
      TabOrder = 1
    end
    object btnAssignText: TcxButton
      Left = 8
      Top = 93
      Width = 217
      Height = 25
      Action = actAssignText
      Anchors = [akLeft, akTop, akRight]
      LookAndFeel.Kind = lfStandard
      LookAndFeel.NativeStyle = True
      TabOrder = 3
    end
    object btnAssignEditingText: TcxButton
      Left = 8
      Top = 123
      Width = 217
      Height = 25
      Action = actAssignEditingText
      Anchors = [akLeft, akTop, akRight]
      LookAndFeel.Kind = lfStandard
      LookAndFeel.NativeStyle = True
      TabOrder = 4
    end
    object btnClearText: TcxButton
      Left = 8
      Top = 185
      Width = 217
      Height = 25
      Action = actClearText
      Anchors = [akLeft, akTop, akRight]
      LookAndFeel.Kind = lfStandard
      LookAndFeel.NativeStyle = True
      TabOrder = 6
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnClearLog: TcxButton
      Left = 8
      Top = 358
      Width = 217
      Height = 25
      Action = actClearLog
      Anchors = [akLeft, akRight, akBottom]
      Default = True
      LookAndFeel.Kind = lfStandard
      LookAndFeel.NativeStyle = True
      TabOrder = 12
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnAssignEditText: TcxButton
      Left = 8
      Top = 62
      Width = 217
      Height = 25
      Action = actAssignEditText
      Anchors = [akLeft, akTop, akRight]
      LookAndFeel.Kind = lfStandard
      LookAndFeel.NativeStyle = True
      TabOrder = 2
    end
    object btnValidateEdit: TcxButton
      Left = 8
      Top = 154
      Width = 217
      Height = 25
      Action = actValidateEdit
      Anchors = [akLeft, akTop, akRight]
      LookAndFeel.Kind = lfStandard
      LookAndFeel.NativeStyle = True
      TabOrder = 5
    end
  end
  object sbrMain: TdxStatusBar
    Left = 0
    Top = 581
    Width = 791
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        PanelStyle.Alignment = taCenter
        Bevel = dxpbNone
        Text = 'Focus:'
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        PanelStyle.Alignment = taCenter
        Width = 200
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object escMain: TcxEditStyleController
    Style.LookAndFeel.Kind = lfStandard
    Style.LookAndFeel.NativeStyle = True
    Style.Shadow = False
    Style.PopupBorderStyle = epbsSingle
    StyleDisabled.LookAndFeel.Kind = lfStandard
    StyleDisabled.LookAndFeel.NativeStyle = True
    StyleFocused.LookAndFeel.Kind = lfStandard
    StyleFocused.LookAndFeel.NativeStyle = True
    StyleHot.LookAndFeel.Kind = lfStandard
    StyleHot.LookAndFeel.NativeStyle = True
    Left = 568
    Top = 8
    PixelsPerInch = 96
  end
  object descMain: TcxDefaultEditStyleController
    Left = 568
    Top = 48
    PixelsPerInch = 96
  end
  object aclMain: TActionList
    Left = 576
    Top = 112
    object actAssignEditValue: TAction
      Caption = 'Assign EditValue'
      OnExecute = actAssignEditValueExecute
    end
    object actAssignText: TAction
      Caption = 'Assign Text'
      OnExecute = actAssignTextExecute
    end
    object actAssignEditingText: TAction
      Caption = 'Assign EditingText'
      OnExecute = actAssignEditingTextExecute
    end
    object actClearLog: TAction
      Caption = 'Clear logging'
      OnExecute = actClearLogExecute
    end
    object actClearText: TAction
      Caption = 'Clear text'
      OnExecute = actClearTextExecute
    end
    object actAssignEditText: TAction
      Caption = 'Assign EditText'
      OnExecute = actAssignEditTextExecute
    end
    object actValidateEdit: TAction
      Caption = 'Validate Edit'
      OnExecute = actValidateEditExecute
    end
  end
end
