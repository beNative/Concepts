object frmCollections: TfrmCollections
  Left = 0
  Top = 0
  Caption = 'Spring collections'
  ClientHeight = 443
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    508
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExecuteQuery: TButton
    Left = 8
    Top = 33
    Width = 250
    Height = 25
    Action = actFirstNameIs
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Find all contacts with first name = '#39'?'#39' '
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 0
  end
  object mmoList: TMemo
    Left = 8
    Top = 136
    Width = 492
    Height = 299
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnCreateList: TButton
    Left = 8
    Top = 2
    Width = 250
    Height = 25
    Action = actCreateList
    Anchors = [akLeft, akTop, akRight]
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
    Anchors = [akLeft, akTop, akRight]
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 3
  end
  object edtFirstName: TEdit
    Left = 264
    Top = 35
    Width = 101
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = 'John'
    OnChange = edtFirstNameChange
  end
  object edtLastName: TEdit
    Left = 264
    Top = 66
    Width = 101
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 5
    Text = 'Roberts'
  end
  object btnBoth: TButton
    Left = 8
    Top = 95
    Width = 250
    Height = 25
    Action = actBoth
    Anchors = [akLeft, akTop, akRight]
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 6
  end
  object trbRecordCount: TTrackBar
    Left = 343
    Top = 2
    Width = 157
    Height = 19
    Max = 2000000
    Min = 1000
    Position = 100000
    PositionToolTip = ptTop
    ShowSelRange = False
    TabOrder = 7
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 72
    Top = 152
    object actCreateList: TAction
      Caption = 'Create a random contact list'
      ImageIndex = 967
      OnExecute = actCreateListExecute
    end
    object actEnumerate: TAction
      Caption = 'actEnumerate'
      ImageIndex = 235
    end
    object actFirstNameIs: TAction
      Caption = 'Execute Query: find all contacts with first name = '#39'?'#39' '
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
