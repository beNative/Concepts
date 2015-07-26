object frmAsyncCalls: TfrmAsyncCalls
  Left = 0
  Top = 0
  ClientHeight = 293
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    516
    293)
  PixelsPerInch = 96
  TextHeight = 13
  object sbrMain: TStatusBar
    Left = 0
    Top = 274
    Width = 516
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object mmoFiles: TMemo
    Left = 8
    Top = 8
    Width = 130
    Height = 260
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 1
  end
  object btnGetFiles: TButton
    Left = 442
    Top = 8
    Width = 74
    Height = 25
    Action = actGetFiles
    Anchors = [akTop, akRight]
    TabOrder = 2
  end
  object mmoFiles2: TMemo
    Left = 144
    Top = 8
    Width = 145
    Height = 260
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 3
  end
  object mmoFiles3: TMemo
    Left = 295
    Top = 8
    Width = 141
    Height = 260
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 4
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 240
    Top = 72
    object actGetFiles: TAction
      Caption = 'actGetFiles'
      OnExecute = actGetFilesExecute
    end
  end
  object imlMain: TImageList
    Left = 288
    Top = 144
  end
end
