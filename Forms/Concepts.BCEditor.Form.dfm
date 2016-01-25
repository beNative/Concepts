object frmBCEditor: TfrmBCEditor
  Left = 0
  Top = 0
  ClientHeight = 721
  ClientWidth = 1008
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 1008
    Height = 721
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object splSplitter: TSplitter
      Left = 433
      Top = 0
      Width = 8
      Height = 702
      ResizeStyle = rsUpdate
      ExplicitLeft = 273
      ExplicitTop = 1
      ExplicitHeight = 362
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 433
      Height = 702
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlRight: TPanel
      Left = 441
      Top = 0
      Width = 567
      Height = 702
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 321
      ExplicitWidth = 687
      object spl1: TSplitter
        Left = 0
        Top = 289
        Width = 567
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 0
        ExplicitWidth = 608
      end
      object pnlRightTop: TPanel
        Left = 0
        Top = 0
        Width = 567
        Height = 289
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 687
        object pgcMain: TPageControl
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 561
          Height = 283
          ActivePage = tsHighlighter
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 681
          object tsHighlighter: TTabSheet
            Caption = 'Highlighter definition'
            ExplicitWidth = 673
            object tlbHighlighter: TToolBar
              Left = 0
              Top = 0
              Width = 553
              Height = 29
              Caption = 'tlbHighlighter'
              TabOrder = 0
              ExplicitWidth = 673
              object btnSaveHighlighter: TToolButton
                Left = 0
                Top = 0
                Action = actSaveHighlighter
              end
            end
            object pnlHighlighter: TPanel
              Left = 153
              Top = 29
              Width = 400
              Height = 226
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 1
              ExplicitWidth = 520
            end
            object pnlHLLeft: TPanel
              Left = 0
              Top = 29
              Width = 153
              Height = 226
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 2
            end
            object pnlHLRight: TPanel
              Left = 153
              Top = 29
              Width = 400
              Height = 226
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 3
              ExplicitWidth = 520
            end
          end
          object tsColors: TTabSheet
            Caption = 'Color mappings'
            ImageIndex = 1
            ExplicitWidth = 673
            object tlbColors: TToolBar
              Left = 0
              Top = 0
              Width = 553
              Height = 29
              Caption = 'tlbColors'
              TabOrder = 0
              ExplicitWidth = 673
              object btnSaveColorMap: TToolButton
                Left = 0
                Top = 0
                Action = actSaveColorMap
              end
            end
            object pnlColors: TPanel
              Left = 153
              Top = 29
              Width = 400
              Height = 226
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 1
              ExplicitWidth = 520
            end
            object pnlCMLeft: TPanel
              Left = 0
              Top = 29
              Width = 153
              Height = 226
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 2
            end
            object pnlCMRight: TPanel
              Left = 153
              Top = 29
              Width = 400
              Height = 226
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 3
              ExplicitWidth = 520
            end
          end
          object tsHighlighters: TTabSheet
            Caption = 'Highlighters'
            ImageIndex = 2
            ExplicitWidth = 673
          end
        end
      end
      object pnlRightBottom: TPanel
        Left = 0
        Top = 295
        Width = 567
        Height = 407
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitWidth = 687
      end
    end
    object sbrStatusBar: TStatusBar
      Left = 0
      Top = 702
      Width = 1008
      Height = 19
      Panels = <>
      ParentShowHint = False
      ShowHint = True
    end
  end
  object aclMain: TActionList
    Left = 488
    Top = 320
    object actSaveHighlighter: TAction
      Caption = 'actSaveHighlighter'
      OnExecute = actSaveHighlighterExecute
    end
    object actSaveColorMap: TAction
      Caption = 'actSaveColorMap'
      OnExecute = actSaveColorMapExecute
    end
  end
end
