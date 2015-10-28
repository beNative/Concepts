object frmCollections: TfrmCollections
  Left = 0
  Top = 0
  Caption = 'Spring collections'
  ClientHeight = 443
  ClientWidth = 494
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    494
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object lblRecordCount: TLabel
    Left = 265
    Top = 7
    Width = 53
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'RecordCount:'
    FocusControl = trbRecordCount
    ExplicitWidth = 67
  end
  object btnExecuteQuery: TButton
    Left = 8
    Top = 33
    Width = 250
    Height = 25
    Action = actFirstNameIs
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 0
  end
  object mmoList: TMemo
    Left = 8
    Top = 126
    Width = 478
    Height = 309
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 492
  end
  object btnCreateList: TButton
    Left = 8
    Top = 2
    Width = 250
    Height = 25
    Action = actPopulateList
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 2
  end
  object btnLastNameRoberts: TButton
    Left = 8
    Top = 64
    Width = 250
    Height = 25
    Action = actLastNameIs
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 3
  end
  object edtFirstName: TEdit
    Left = 264
    Top = 35
    Width = 215
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = 'John'
    ExplicitWidth = 229
  end
  object edtLastName: TEdit
    Left = 264
    Top = 66
    Width = 215
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    Text = 'Roberts'
    ExplicitWidth = 229
  end
  object btnBoth: TButton
    Left = 8
    Top = 95
    Width = 250
    Height = 25
    Action = actBoth
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 6
  end
  object trbRecordCount: TTrackBar
    Left = 338
    Top = 2
    Width = 141
    Height = 19
    Anchors = [akLeft, akTop, akRight]
    Max = 2000000
    Min = 1000
    Position = 100000
    PositionToolTip = ptTop
    ShowSelRange = False
    TabOrder = 7
    ExplicitWidth = 157
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 288
    Top = 96
    object actPopulateList: TAction
      Caption = 'Populate list with random contacts'
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
end
