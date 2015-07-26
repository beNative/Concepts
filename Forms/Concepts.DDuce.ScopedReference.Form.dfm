object frmScopedReferences: TfrmScopedReferences
  Left = 0
  Top = 0
  Caption = 'Scoped references'
  ClientHeight = 293
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnShowClassNames: TButton
    Left = 8
    Top = 16
    Width = 217
    Height = 25
    Action = actShowClassNames
    TabOrder = 0
  end
  object aclMain: TActionList
    Left = 232
    Top = 24
    object actShowClassNames: TAction
      Caption = 'Show classnames of embedded objects'
      OnExecute = actShowClassNamesExecute
    end
  end
end
