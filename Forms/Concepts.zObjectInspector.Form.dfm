object frmzObjectInspector: TfrmzObjectInspector
  Left = 0
  Top = 0
  Caption = 'TzObjectInspector'
  ClientHeight = 716
  ClientWidth = 799
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 28
    Width = 799
    Height = 688
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 0
    ExplicitHeight = 716
    object splSplitter: TSplitter
      Left = 313
      Top = 0
      Width = 8
      Height = 669
      ResizeStyle = rsUpdate
      ExplicitLeft = 273
      ExplicitTop = 1
      ExplicitHeight = 362
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 313
      Height = 669
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 697
      object cbxControls: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 307
        Height = 21
        Margins.Bottom = 0
        Align = alTop
        Style = csDropDownList
        DropDownCount = 20
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        TextHint = 'Select a component on this form'
        OnChange = cbxControlsChange
      end
    end
    object pnlRight: TPanel
      Left = 321
      Top = 0
      Width = 478
      Height = 669
      Align = alClient
      BevelOuter = bvNone
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 1
      ExplicitHeight = 697
      DesignSize = (
        478
        669)
      object btnButton: TButton
        Left = 223
        Top = 6
        Width = 109
        Height = 25
        Action = actTest1
        ImageMargins.Left = 3
        ImageMargins.Top = 3
        ImageMargins.Right = 3
        ImageMargins.Bottom = 3
        TabOrder = 1
      end
      object chkCheckBox: TCheckBox
        Left = 22
        Top = 69
        Width = 218
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'TCheckBox'
        State = cbGrayed
        TabOrder = 2
      end
      object edtEdit: TEdit
        Left = 6
        Top = 6
        Width = 107
        Height = 21
        TabOrder = 0
        Text = 'TEdit'
      end
      object bgMain: TButtonGroup
        Left = 6
        Top = 104
        Width = 467
        Height = 559
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvNone
        BevelKind = bkFlat
        BorderStyle = bsNone
        ButtonOptions = [gboAllowReorder, gboShowCaptions]
        Images = dmResources.imlMain
        Items = <>
        TabOrder = 3
      end
      object trbTrackBar: TTrackBar
        Left = 6
        Top = 33
        Width = 467
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        PositionToolTip = ptTop
        ShowSelRange = False
        TabOrder = 4
      end
      object edtButtonedEdit: TButtonedEdit
        Left = 119
        Top = 6
        Width = 98
        Height = 21
        RightButton.Hint = 'Hint'
        RightButton.HotImageIndex = 114
        RightButton.ImageIndex = 115
        RightButton.PressedImageIndex = 116
        RightButton.Visible = True
        TabOrder = 5
        Text = 'TButtonedEdit'
      end
    end
    object sbrStatusBar: TStatusBar
      Left = 0
      Top = 669
      Width = 799
      Height = 19
      Panels = <>
      ParentShowHint = False
      ShowHint = True
      ExplicitTop = 697
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 799
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 799
      Height = 28
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'This form demonstrates the TzObjectInspector control. You can ad' +
        'just properties of any control shown on this form.'
      EllipsisPosition = epWordEllipsis
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
      ExplicitHeight = 26
    end
  end
  object aclMain: TActionList
    Left = 520
    Top = 96
    object actTest1: TAction
      Caption = 'Test 1 action'
    end
    object actTest2: TAction
      Caption = 'actTest2'
    end
  end
end
