object frmInterfacedComponent: TfrmInterfacedComponent
  Left = 0
  Top = 0
  Caption = 'Interfaced TComponent demo'
  ClientHeight = 286
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnCreateNormal: TButton
    Left = 8
    Top = 8
    Width = 122
    Height = 25
    Action = actCreateNormal
    TabOrder = 0
  end
  object btnCreateReferenceCounted: TButton
    Left = 136
    Top = 8
    Width = 129
    Height = 25
    Action = actCreateReferenceCounted
    TabOrder = 1
  end
  object btnCreateBoth: TButton
    Left = 271
    Top = 8
    Width = 143
    Height = 25
    Action = actCreateBoth
    TabOrder = 2
  end
  object aclMain: TActionList
    Left = 72
    Top = 96
    object actCreateNormal: TAction
      Caption = 'Normal'
      OnExecute = actCreateNormalExecute
    end
    object actCreateReferenceCounted: TAction
      Caption = 'Reference counted'
      OnExecute = actCreateReferenceCountedExecute
    end
    object actCreateBoth: TAction
      Caption = 'Both'
      OnExecute = actCreateBothExecute
    end
  end
end
