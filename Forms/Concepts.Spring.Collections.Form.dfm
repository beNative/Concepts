object frmCollections: TfrmCollections
  Left = 0
  Top = 0
  Caption = 'Spring collections'
  ClientHeight = 443
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    394
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExecuteQuery: TButton
    Left = 8
    Top = 33
    Width = 271
    Height = 25
    Action = actFirstNameJohn
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Find all contacts with first name = '#39'?'#39' '
    TabOrder = 0
    ExplicitWidth = 257
  end
  object mmoList: TMemo
    Left = 8
    Top = 95
    Width = 378
    Height = 340
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 364
  end
  object btnCreateList: TButton
    Left = 8
    Top = 2
    Width = 271
    Height = 25
    Action = actCreateList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitWidth = 257
  end
  object btnLastNameRoberts: TButton
    Left = 8
    Top = 64
    Width = 271
    Height = 25
    Action = actLastNameRoberts
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Find contacts with  last name = '#39'?'#39
    TabOrder = 3
    ExplicitWidth = 257
  end
  object edtFirstName: TEdit
    Left = 285
    Top = 35
    Width = 101
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = 'John'
    ExplicitLeft = 271
  end
  object edtLastName: TEdit
    Left = 285
    Top = 66
    Width = 101
    Height = 22
    Anchors = [akTop, akRight]
    TabOrder = 5
    Text = 'Roberts'
    ExplicitLeft = 271
  end
  object aclMain: TActionList
    Left = 72
    Top = 152
    object actCreateList: TAction
      Caption = 'Create a random contact list'
      OnExecute = actCreateListExecute
    end
    object actEnumerate: TAction
      Caption = 'actEnumerate'
    end
    object actFirstNameJohn: TAction
      Caption = 'Execute Query: find all contacts with first name = '#39'John'#39' '
      OnExecute = actFirstNameJohnExecute
    end
    object actLastNameRoberts: TAction
      Caption = 'Find contacts with  last name = '#39'Roberts'#39
      OnExecute = actLastNameRobertsExecute
    end
  end
end
