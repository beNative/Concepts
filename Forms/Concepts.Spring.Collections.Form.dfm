object frmCollections: TfrmCollections
  Left = 0
  Top = 0
  Caption = 'Spring collections'
  ClientHeight = 443
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    380
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExecuteQuery: TButton
    Left = 8
    Top = 33
    Width = 364
    Height = 25
    Action = actFirstNameJohn
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Find all contacts with first name = '#39'John'#39' '
    TabOrder = 0
  end
  object mmoList: TMemo
    Left = 8
    Top = 95
    Width = 364
    Height = 340
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnCreateList: TButton
    Left = 8
    Top = 2
    Width = 364
    Height = 25
    Action = actCreateList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object btnLastNameRoberts: TButton
    Left = 8
    Top = 64
    Width = 364
    Height = 25
    Action = actLastNameRoberts
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object aclMain: TActionList
    Left = 72
    Top = 152
    object actCreateList: TAction
      Caption = 'Create contact list'
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
