object frmKMemo: TfrmKMemo
  Left = 0
  Top = 0
  Caption = 'KMemo'
  ClientHeight = 566
  ClientWidth = 845
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnAddTextBlock: TButton
    Left = 12
    Top = 2
    Width = 150
    Height = 25
    Action = actAddTextBlock
    TabOrder = 0
  end
  object btnClear: TButton
    Left = 176
    Top = 2
    Width = 150
    Height = 25
    Action = actClear
    TabOrder = 1
  end
  object btnAddContainer: TButton
    Left = 12
    Top = 29
    Width = 150
    Height = 25
    Action = actAddContainer
    TabOrder = 2
  end
  object btnAddHyperlink: TButton
    Left = 12
    Top = 56
    Width = 150
    Height = 25
    Action = actAddHyperLink
    TabOrder = 3
  end
  object btnAddImageBlock: TButton
    Left = 12
    Top = 83
    Width = 150
    Height = 25
    Action = actAddImageBlock
    TabOrder = 4
  end
  object btnAddParagraph: TButton
    Left = 12
    Top = 110
    Width = 150
    Height = 25
    Action = actAddParagraph
    TabOrder = 5
  end
  object btnAddTable: TButton
    Left = 12
    Top = 137
    Width = 150
    Height = 25
    Action = actAddTable
    TabOrder = 6
  end
  object btnDeleteObject: TButton
    Left = 176
    Top = 32
    Width = 150
    Height = 25
    Action = actDeleteObject
    TabOrder = 7
  end
  object pnlTreeView: TPanel
    Left = 0
    Top = 176
    Width = 390
    Height = 845
    BevelOuter = bvNone
    TabOrder = 8
  end
  object btnactCnPrefixWizard: TButton
    Left = 176
    Top = 64
    Width = 150
    Height = 25
    Action = actRebuildTree
    TabOrder = 9
  end
  object btnactCnPrefixWizard1: TButton
    Left = 176
    Top = 96
    Width = 150
    Height = 25
    Action = actInspectKMemo
    TabOrder = 10
  end
  object pnlRichEditor: TPanel
    Left = 408
    Top = 0
    Width = 437
    Height = 566
    Align = alRight
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 11
    ExplicitTop = 2
    object KMemo: TKMemo
      Left = 0
      Top = 0
      Width = 437
      Height = 566
      Align = alClient
      ContentPadding.Left = 5
      ContentPadding.Top = 5
      ContentPadding.Right = 5
      ContentPadding.Bottom = 5
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Visible = True
      ExplicitLeft = 112
      ExplicitTop = 96
      ExplicitWidth = 300
      ExplicitHeight = 200
    end
  end
  object aclMain: TActionList
    Left = 471
    Top = 250
    object actAddTextBlock: TAction
      Caption = 'Add text block'
      OnExecute = actAddTextBlockExecute
    end
    object actClear: TAction
      Caption = 'Clear'
      OnExecute = actClearExecute
    end
    object actAddParagraph: TAction
      Caption = 'Add paragraph'
      OnExecute = actAddParagraphExecute
    end
    object actAddImageBlock: TAction
      Caption = 'Add image block'
      OnExecute = actAddImageBlockExecute
    end
    object actAddTable: TAction
      Caption = 'Add table'
      OnExecute = actAddTableExecute
    end
    object actAddHyperLink: TAction
      Caption = 'Add hyperlink'
      OnExecute = actAddHyperLinkExecute
    end
    object actAddContainer: TAction
      Caption = 'Add container'
      OnExecute = actAddContainerExecute
    end
    object actDeleteObject: TAction
      Caption = 'Delete object'
      OnExecute = actDeleteObjectExecute
    end
    object actRebuildTree: TAction
      Caption = 'Rebuild tree'
      OnExecute = actRebuildTreeExecute
    end
    object actInspectKMemo: TAction
      Caption = 'Inspect KMemo'
      OnExecute = actInspectKMemoExecute
    end
  end
end
