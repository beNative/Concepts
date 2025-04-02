object frmSelectionInfo: TfrmSelectionInfo
  Left = 304
  Top = 304
  ClientHeight = 615
  ClientWidth = 446
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 13
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 446
    Height = 615
    ActivePage = tsSelectionInfo
    Align = alClient
    TabOrder = 0
    object tsSelectionInfo: TTabSheet
      Caption = 'Selection info'
      object pnlSelectionInfo: TPanel
        Left = 0
        Top = 0
        Width = 441
        Height = 590
        Align = alClient
        TabOrder = 0
        DesignSize = (
          438
          587)
        object lblStoredBlockBegin: TLabel
          Left = 8
          Top = 8
          Width = 93
          Height = 13
          Margins.Bottom = 2
          Caption = 'StoredBlockBegin:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblStoredBlockEnd: TLabel
          Left = 8
          Top = 33
          Width = 84
          Height = 13
          Margins.Bottom = 2
          Caption = 'StoredBlockEnd:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblStoredBlockBeginValue: TLabel
          Left = 156
          Top = 8
          Width = 131
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblStoredBlockBeginValue'
          Color = clBtnFace
          ParentColor = False
        end
        object lblStoredBlockEndValue: TLabel
          Left = 156
          Top = 33
          Width = 122
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblStoredBlockEndValue'
          Color = clBtnFace
          ParentColor = False
        end
        object lblStoredBlockLines: TLabel
          Left = 10
          Top = 304
          Width = 114
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblStoredBlockLines'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object lblStoredBlockSelectionMode: TLabel
          Left = 8
          Top = 83
          Width = 141
          Height = 13
          Margins.Bottom = 2
          Caption = 'StoredBlockSelectionMode:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblStoredBlockSelectionModeValue: TLabel
          Left = 156
          Top = 83
          Width = 179
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblStoredBlockSelectionModeValue'
          Color = clBtnFace
          ParentColor = False
        end
        object lblBlockBegin: TLabel
          Left = 8
          Top = 108
          Width = 59
          Height = 13
          Margins.Bottom = 2
          Caption = 'BlockBegin:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblBlockEnd: TLabel
          Left = 8
          Top = 133
          Width = 50
          Height = 13
          Margins.Bottom = 2
          Caption = 'BlockEnd:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblBlockBeginValue: TLabel
          Left = 156
          Top = 108
          Width = 97
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblBlockBeginValue'
          Color = clBtnFace
          ParentColor = False
        end
        object lblBlockEndValue: TLabel
          Left = 156
          Top = 133
          Width = 88
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblBlockEndValue'
          Color = clBtnFace
          ParentColor = False
        end
        object lblCaretXY: TLabel
          Left = 8
          Top = 158
          Width = 41
          Height = 13
          Margins.Bottom = 2
          Caption = 'CaretXY:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblCaretXYValue: TLabel
          Left = 156
          Top = 158
          Width = 79
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblCaretXYValue'
          Color = clBtnFace
          ParentColor = False
        end
        object lblLogicalCaretXY: TLabel
          Left = 8
          Top = 183
          Width = 77
          Height = 13
          Margins.Bottom = 2
          Caption = 'LogicalCaretXY:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblLogicalCaretXYValue: TLabel
          Left = 156
          Top = 183
          Width = 115
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblLogicalCaretXYValue'
          Color = clBtnFace
          ParentColor = False
        end
        object lblStoredCaretXY: TLabel
          Left = 8
          Top = 58
          Width = 75
          Height = 13
          Margins.Bottom = 2
          Caption = 'StoredCaretXY:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblStoredCaretXYValue: TLabel
          Left = 156
          Top = 58
          Width = 113
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblStoredCaretXYValue'
          Color = clBtnFace
          ParentColor = False
        end
        object lblLineCount: TLabel
          Left = 8
          Top = 208
          Width = 56
          Height = 13
          Margins.Bottom = 2
          Caption = 'LineCount:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblLineCountValue: TLabel
          Left = 156
          Top = 208
          Width = 94
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblLineCountValue'
          Color = clBtnFace
          ParentColor = False
        end
        object lblSelStart: TLabel
          Left = 8
          Top = 336
          Width = 39
          Height = 13
          Margins.Bottom = 2
          Caption = 'SelStart'
        end
        object lblSelEnd: TLabel
          Left = 8
          Top = 376
          Width = 35
          Height = 13
          Margins.Bottom = 2
          Caption = 'SelEnd'
        end
        object lblSelStartValue: TLabel
          Left = 144
          Top = 336
          Width = 52
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblSelStart'
        end
        object lblSelEndValue: TLabel
          Left = 144
          Top = 376
          Width = 48
          Height = 13
          Margins.Bottom = 2
          Caption = 'lblSelEnd'
        end
        object btnStore: TButton
          Left = 8
          Top = 232
          Width = 100
          Height = 25
          Caption = 'Store'
          TabOrder = 0
          OnClick = btnStoreClick
        end
        object btnRestore: TButton
          Left = 8
          Top = 264
          Width = 100
          Height = 25
          Caption = 'Restore'
          TabOrder = 1
          OnClick = btnRestoreClick
        end
        object chkLockUpdates: TCheckBox
          Left = 120
          Top = 235
          Width = 88
          Height = 19
          Caption = 'LockUpdates'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object chkExcludeEmptyLines: TCheckBox
          Left = 224
          Top = 235
          Width = 121
          Height = 19
          Caption = 'ExcludeEmptyLines'
          TabOrder = 3
        end
        object mmoBlock: TMemo
          Left = 0
          Top = 437
          Width = 432
          Height = 143
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          TabOrder = 4
          OnChange = mmoBlockChange
          ExplicitWidth = 435
          ExplicitHeight = 146
        end
      end
    end
    object tsReflectedProperties: TTabSheet
      Caption = 'Reflected properties'
      ImageIndex = 1
      object mmoReflected: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 420
        Height = 581
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
  end
end
