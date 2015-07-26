object frmTreeViewPresenter: TfrmTreeViewPresenter
  Left = 441
  Top = 465
  Caption = 'frmTreeViewPresenter'
  ClientHeight = 352
  ClientWidth = 667
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  DesignSize = (
    667
    352)
  PixelsPerInch = 96
  TextHeight = 13
  object lblChange: TLabel
    Left = 216
    Top = 326
    Width = 225
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Change FirstName property of item with index:'
  end
  object grdMain: TVirtualStringTree
    Left = 8
    Top = 8
    Width = 651
    Height = 180
    Anchors = [akLeft, akTop, akRight, akBottom]
    DrawSelectionMode = smBlendedRectangle
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    NodeDataSize = 4
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
    Columns = <>
  end
  object grdMainDetail: TVirtualStringTree
    Left = 8
    Top = 194
    Width = 651
    Height = 121
    Anchors = [akLeft, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoAutoSpring]
    NodeDataSize = 4
    TabOrder = 1
    Columns = <>
  end
  object btnFilter: TButton
    Left = 135
    Top = 321
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Filter'
    TabOrder = 2
    OnClick = btnFilterClick
  end
  object edtFilter: TEdit
    Left = 8
    Top = 323
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
  end
  object btnEvent: TButton
    Left = 606
    Top = 322
    Width = 53
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Change'
    TabOrder = 4
    OnClick = btnEventClick
  end
  object edtIndex: TcxSpinEdit
    Left = 446
    Top = 323
    Anchors = [akLeft, akBottom]
    Properties.Alignment.Horz = taCenter
    Style.BorderStyle = ebs3D
    TabOrder = 5
    Width = 41
  end
  object edtName: TEdit
    Left = 490
    Top = 323
    Width = 111
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 6
  end
end
