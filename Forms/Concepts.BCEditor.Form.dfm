object frmBCEditor: TfrmBCEditor
  Left = 0
  Top = 0
  ClientHeight = 627
  ClientWidth = 990
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 990
    Height = 627
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object splSplitter: TSplitter
      Left = 313
      Top = 0
      Width = 8
      Height = 608
      ResizeStyle = rsUpdate
      ExplicitLeft = 273
      ExplicitTop = 1
      ExplicitHeight = 362
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 313
      Height = 608
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object cbxControls: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 307
        Height = 21
        Margins.Bottom = 0
        Align = alTop
        Style = csDropDownList
        DropDownCount = 20
        TabOrder = 0
        OnChange = cbxControlsChange
      end
    end
    object pnlRight: TPanel
      Left = 321
      Top = 0
      Width = 669
      Height = 608
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object spl1: TSplitter
        Left = 0
        Top = 289
        Width = 669
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 0
        ExplicitWidth = 608
      end
      object pnlRightTop: TPanel
        Left = 0
        Top = 0
        Width = 669
        Height = 289
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object pgcMain: TPageControl
          AlignWithMargins = True
          Left = 175
          Top = 3
          Width = 491
          Height = 283
          ActivePage = tsHighlighter
          Align = alRight
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
          ExplicitLeft = 168
          ExplicitWidth = 497
          ExplicitHeight = 280
          object tsHighlighter: TTabSheet
            Caption = 'tsHighlighter'
            ExplicitWidth = 281
            ExplicitHeight = 165
            object tlbHighlighter: TToolBar
              Left = 0
              Top = 0
              Width = 483
              Height = 29
              Caption = 'tlbHighlighter'
              TabOrder = 0
              ExplicitLeft = 168
              ExplicitTop = 112
              ExplicitWidth = 150
              object btnSaveHighlighter: TToolButton
                Left = 0
                Top = 0
                Action = actSaveHighlighter
              end
            end
            object pnlHighlighter: TPanel
              Left = 0
              Top = 29
              Width = 483
              Height = 226
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 1
              ExplicitLeft = 152
              ExplicitTop = 104
              ExplicitWidth = 185
              ExplicitHeight = 41
            end
          end
          object tsColors: TTabSheet
            Caption = 'tsColors'
            ImageIndex = 1
            ExplicitWidth = 281
            ExplicitHeight = 165
            object tlbColors: TToolBar
              Left = 0
              Top = 0
              Width = 483
              Height = 29
              Caption = 'tlbColors'
              TabOrder = 0
              ExplicitLeft = 168
              ExplicitTop = 112
              ExplicitWidth = 150
              object btnSaveColorMap: TToolButton
                Left = 0
                Top = 0
                Action = actSaveColorMap
              end
            end
            object pnlColors: TPanel
              Left = 0
              Top = 29
              Width = 483
              Height = 226
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 1
              ExplicitLeft = 152
              ExplicitTop = 104
              ExplicitWidth = 185
              ExplicitHeight = 41
            end
          end
        end
      end
      object pnlRightBottom: TPanel
        Left = 0
        Top = 295
        Width = 669
        Height = 313
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 240
        ExplicitTop = 376
        ExplicitWidth = 185
        ExplicitHeight = 41
      end
    end
    object sbrStatusBar: TStatusBar
      Left = 0
      Top = 608
      Width = 990
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
