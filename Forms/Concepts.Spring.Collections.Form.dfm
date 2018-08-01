object frmCollections: TfrmCollections
  Left = 0
  Top = 0
  Caption = 'Spring collections'
  ClientHeight = 443
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    495
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object lblRecordCount: TLabel
    Left = 265
    Top = 43
    Width = 67
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'RecordCount:'
    FocusControl = trbRecordCount
  end
  object btnFirstNameIs: TButton
    Left = 8
    Top = 69
    Width = 250
    Height = 25
    Action = actFirstNameIs
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 0
  end
  object mmoList: TMemo
    Left = 8
    Top = 160
    Width = 479
    Height = 258
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnCreateList: TButton
    Left = 8
    Top = 38
    Width = 250
    Height = 25
    Action = actPopulateList
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 2
  end
  object btnLastNameIs: TButton
    Left = 8
    Top = 100
    Width = 250
    Height = 25
    Action = actLastNameIs
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 3
  end
  object edtFirstName: TEdit
    Left = 264
    Top = 102
    Width = 223
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = 'John'
  end
  object edtLastName: TEdit
    Left = 264
    Top = 133
    Width = 223
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    Text = 'Roberts'
  end
  object btnBoth: TButton
    Left = 8
    Top = 131
    Width = 250
    Height = 25
    Action = actBoth
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 6
  end
  object trbRecordCount: TTrackBar
    Left = 338
    Top = 38
    Width = 149
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    Max = 6000000
    Min = 1000
    PageSize = 10000
    Frequency = 500000
    Position = 1000000
    PositionToolTip = ptTop
    ShowSelRange = False
    TabOrder = 7
    OnChange = trbRecordCountChange
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 424
    Width = 495
    Height = 19
    Panels = <>
  end
  object edtRecordCount: TEdit
    Left = 264
    Top = 71
    Width = 223
    Height = 21
    Alignment = taCenter
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = True
    ParentFont = False
    TabOrder = 9
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 495
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
    TabOrder = 10
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 372
      Height = 13
      Align = alClient
      Alignment = taCenter
      Caption = 
        'This form demonstrates some basic features of Spring collections' +
        '.'
      Layout = tlCenter
      WordWrap = True
    end
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 288
    Top = 96
    object actPopulateList: TAction
      Caption = 'Populate contactlist'
      ImageIndex = 967
      OnExecute = actPopulateListExecute
    end
    object actEnumerate: TAction
      Caption = 'actEnumerate'
      ImageIndex = 235
    end
    object actFirstNameIs: TAction
      Caption = 'Find all contacts with first name = '#39'?'#39' '
      ImageIndex = 954
      OnExecute = actFirstNameIsExecute
    end
    object actLastNameIs: TAction
      Caption = 'Find contacts with  last name = '#39'?'#39
      ImageIndex = 962
      OnExecute = actLastNameIsExecute
    end
    object actBoth: TAction
      Caption = 'Combine both conditions'
      ImageIndex = 454
      OnExecute = actBothExecute
    end
  end
  object lstBindings: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 240
    Top = 224
    object BindExpression1: TBindExpression
      Category = 'Binding Expressions'
      ControlComponent = edtRecordCount
      SourceComponent = trbRecordCount
      SourceMemberName = 'Position'
      SourceExpression = 'Position'
      ControlExpression = 'Text'
      NotifyOutputs = True
      Direction = dirSourceToControl
    end
  end
end
