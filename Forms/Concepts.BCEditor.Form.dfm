object frmBCEditor: TfrmBCEditor
  Left = 0
  Top = 0
  ClientHeight = 721
  ClientWidth = 1191
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
    Width = 1191
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
      object splLeftHorizontal: TSplitter
        Left = 0
        Top = 403
        Width = 433
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitLeft = 428
        ExplicitTop = 25
        ExplicitWidth = 383
      end
      object tlbComponentInspector: TToolBar
        Left = 0
        Top = 0
        Width = 433
        Height = 25
        Caption = 'tlbHighlighter'
        Images = dmResources.imlMain
        TabOrder = 0
        object btnCollapseAll: TToolButton
          Left = 0
          Top = 0
          Action = actCollapseAll
        end
        object btnExpandAll: TToolButton
          Left = 23
          Top = 0
          Action = actExpandAll
        end
      end
      object pnlLeftBottom: TPanel
        Left = 0
        Top = 408
        Width = 433
        Height = 294
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object pgcLeftBottoù: TPageControl
          Left = 0
          Top = 0
          Width = 433
          Height = 294
          ActivePage = tsSampleCode
          Align = alClient
          TabOrder = 0
          object tsSampleCode: TTabSheet
            Caption = 'tsSampleCode'
            object pnlExampleCodeHeader: TPanel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 419
              Height = 22
              Align = alTop
              BevelKind = bkFlat
              BevelOuter = bvNone
              Caption = 'Sample code'
              Color = clSilver
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentBackground = False
              ParentFont = False
              TabOrder = 0
            end
          end
          object TabSheet1: TTabSheet
            Caption = 'TabSheet1'
            ImageIndex = 1
            object mmo1: TMemo
              Left = 0
              Top = 0
              Width = 425
              Height = 266
              Align = alClient
              Lines.Strings = (
                'mmo1')
              TabOrder = 0
            end
          end
        end
      end
    end
    object pnlRight: TPanel
      Left = 441
      Top = 0
      Width = 750
      Height = 702
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object spl1: TSplitter
        Left = 0
        Top = 385
        Width = 750
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 0
        ExplicitWidth = 608
      end
      object pnlRightTop: TPanel
        Left = 0
        Top = 0
        Width = 750
        Height = 385
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object pgcMain: TPageControl
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 744
          Height = 379
          ActivePage = tsHighlighter
          Align = alClient
          TabOrder = 0
          object tsHighlighter: TTabSheet
            Caption = 'Highlighter definition'
            object pnlHighlighter: TPanel
              Left = 153
              Top = 0
              Width = 583
              Height = 351
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
            end
            object pnlHLLeft: TPanel
              Left = 0
              Top = 0
              Width = 153
              Height = 351
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 1
              object tlbHighlighter: TToolBar
                Left = 0
                Top = 0
                Width = 153
                Height = 23
                Caption = 'tlbHighlighter'
                Images = dmResources.imlMain
                TabOrder = 0
                object btnSaveHighlighter: TToolButton
                  Left = 0
                  Top = 0
                  Action = actSaveHighlighter
                end
              end
            end
            object pnlHLRight: TPanel
              Left = 153
              Top = 0
              Width = 583
              Height = 351
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 2
            end
          end
          object tsColors: TTabSheet
            Caption = 'Color mappings'
            ImageIndex = 1
            object splVerticalRight: TSplitter
              Left = 475
              Top = 0
              Width = 5
              Height = 351
              Align = alRight
              ExplicitLeft = 8
            end
            object pnlColors: TPanel
              Left = 153
              Top = 0
              Width = 322
              Height = 351
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
            end
            object pnlCMLeft: TPanel
              Left = 0
              Top = 0
              Width = 153
              Height = 351
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 1
              object tlbColors: TToolBar
                Left = 0
                Top = 0
                Width = 153
                Height = 23
                Caption = 'tlbColors'
                Images = dmResources.imlMain
                TabOrder = 0
                object btnSaveColorMap: TToolButton
                  Left = 0
                  Top = 0
                  Action = actSaveColorMap
                end
              end
            end
            object pnlCMRight: TPanel
              Left = 153
              Top = 0
              Width = 322
              Height = 351
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 2
            end
            object pnlCMRightRight: TPanel
              Left = 480
              Top = 0
              Width = 256
              Height = 351
              Align = alRight
              BevelOuter = bvNone
              TabOrder = 3
            end
          end
        end
      end
      object pnlRightBottom: TPanel
        Left = 0
        Top = 391
        Width = 750
        Height = 311
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
    object sbrStatusBar: TStatusBar
      Left = 0
      Top = 702
      Width = 1191
      Height = 19
      Panels = <>
      ParentShowHint = False
      ShowHint = True
    end
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 488
    Top = 320
    object actSaveHighlighter: TAction
      Caption = 'actSaveHighlighter'
      ImageIndex = 336
      ShortCut = 16467
      OnExecute = actSaveHighlighterExecute
    end
    object actSaveColorMap: TAction
      Caption = 'actSaveColorMap'
      ImageIndex = 336
      ShortCut = 16467
      OnExecute = actSaveColorMapExecute
    end
    object actCollapseAll: TAction
      Caption = 'Collapse all'
      Hint = 'Collapse tree.'
      ImageIndex = 44
      OnExecute = actCollapseAllExecute
    end
    object actExpandAll: TAction
      Caption = 'Expand all'
      Hint = 'Expand tree.'
      ImageIndex = 48
      OnExecute = actExpandAllExecute
    end
    object actTest: TAction
      Caption = 'actTest'
      ShortCut = 49239
      OnExecute = actTestExecute
    end
  end
end
