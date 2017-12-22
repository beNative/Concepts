object frmVirtualTreeView: TfrmVirtualTreeView
  Left = 0
  Top = 0
  Caption = 'Virtual treeview'
  ClientHeight = 678
  ClientWidth = 1342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 329
    Top = 28
    Width = 8
    Height = 631
    ExplicitTop = -19
    ExplicitHeight = 697
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 659
    Width = 1342
    Height = 19
    Panels = <>
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 28
    Width = 329
    Height = 631
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlMain: TPanel
    Left = 337
    Top = 28
    Width = 1005
    Height = 631
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object vstMain: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 1005
      Height = 631
      Align = alClient
      Header.AutoSizeIndex = -1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = [fsBold]
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoAutoSpring]
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRestoreSelection]
      OnAfterCellPaint = vstMainAfterCellPaint
      OnChange = vstMainChange
      OnDblClick = vstMainDblClick
      OnExpanded = vstMainExpanded
      OnExpanding = vstMainExpanding
      OnFocusChanged = vstMainFocusChanged
      OnFocusChanging = vstMainFocusChanging
      OnFreeNode = vstMainFreeNode
      OnGetText = vstMainGetText
      OnGetImageIndex = vstMainGetImageIndex
      OnGetHint = vstMainGetHint
      OnGetNodeDataSize = vstMainGetNodeDataSize
      OnHeaderDraw = vstMainHeaderDraw
      OnHeaderDrawQueryElements = vstMainHeaderDrawQueryElements
      OnInitChildren = vstMainInitChildren
      OnInitNode = vstMainInitNode
      OnMouseUp = vstMainMouseUp
      Columns = <
        item
          Color = clWhite
          MaxWidth = 200
          MinWidth = 100
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus, coEditable]
          Position = 0
          Width = 200
          WideText = 'FirstName'
        end
        item
          MaxWidth = 200
          MinWidth = 100
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coEditable]
          Position = 1
          Width = 100
          WideText = 'LastName'
        end
        item
          MaxWidth = 200
          MinWidth = 100
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coEditable]
          Position = 2
          Width = 100
          WideText = 'Email'
        end
        item
          MaxWidth = 400
          MinWidth = 100
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coEditable]
          Position = 3
          Width = 100
          WideText = 'CompanyName'
        end
        item
          MaxWidth = 400
          MinWidth = 100
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coEditable]
          Position = 4
          Width = 100
          WideText = 'Address'
        end
        item
          MaxWidth = 200
          MinWidth = 70
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coEditable]
          Position = 5
          Width = 70
          WideText = 'Number'
        end
        item
          MaxWidth = 200
          MinWidth = 70
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coEditable]
          Position = 6
          Width = 200
          WideText = 'Active'
        end>
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 1342
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 1342
      Height = 28
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      Caption = 'This form demonstrates the TVirtualStringTree control.'
      EllipsisPosition = epWordEllipsis
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 799
      ExplicitHeight = 26
    end
  end
end
