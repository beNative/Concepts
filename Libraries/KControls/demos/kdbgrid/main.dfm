object MainForm: TMainForm
  Left = 613
  Top = 261
  Caption = 'KDBGrid demo'
  ClientHeight = 557
  ClientWidth = 696
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  DesignSize = (
    696
    557)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 88
    Height = 13
    Caption = 'Connection string:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 30
    Height = 13
    Caption = 'Table:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label3: TLabel
    Left = 184
    Top = 48
    Width = 90
    Height = 13
    Caption = 'First column value:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object EDConnectionString: TEdit
    Left = 8
    Top = 24
    Width = 630
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object EDTable: TEdit
    Left = 8
    Top = 64
    Width = 156
    Height = 21
    TabOrder = 1
  end
  object EDFirstCol: TDBEdit
    Left = 184
    Top = 64
    Width = 121
    Height = 21
    DataSource = DSMain
    TabOrder = 2
  end
  object BUOpen: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Action = ACOpen
    TabOrder = 3
  end
  object BUClose: TButton
    Left = 89
    Top = 104
    Width = 75
    Height = 25
    Action = ACClose
    TabOrder = 4
  end
  object DBNav: TDBNavigator
    Left = 184
    Top = 104
    Width = 240
    Height = 25
    DataSource = DSMain
    TabOrder = 5
    BeforeAction = DBNavBeforeAction
  end
  object BUPrint: TButton
    Left = 539
    Top = 62
    Width = 99
    Height = 25
    Action = ACPrint
    TabOrder = 6
  end
  object BUAutoSize: TButton
    Left = 328
    Top = 62
    Width = 89
    Height = 25
    Caption = 'Autosize row'
    TabOrder = 7
    OnClick = BUAutoSizeClick
  end
  object DBGrid: TKDBGrid
    Left = 0
    Top = 142
    Width = 696
    Height = 415
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    DBOptions = [dboAutoMoveRecord, dboAutoSizeBooleanCells, dboDontClearFixedCells, dboImageHint, dboIndexFixedCol, dboIndicateActiveRecord]
    ColCount = 6
    Columns = <
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clFuchsia
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        HorzAlign = halCenter
        Title = 'Index'
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        FieldName = 'id'
        Title = 'ID'
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        HorzAlign = halRight
        HorzPadding = 5
        FieldName = 'thumb_width'
        Title = 'Width'
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        HorzAlign = halRight
        HorzPadding = 5
        FieldName = 'thumb_height'
        Title = 'Height'
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        FieldName = 'thumb_data'
        Title = 'Image'
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      item
        Extent = 200
        CellHint = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        FieldName = 'image_text'
        Title = 'Description'
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end>
    DataSource = DSMain
    Options = [goAlwaysShowEditor, goClippedCells, goColMoving, goColSizing, goDrawFocusSelected, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goIndicateHiddenCells, goMouseCanHideCells, goMouseOverCells, goRowSelect, goRowSizing, goRowSorting, goTabs, goThemes, goThemedCells, goVertLine]
    OptionsEx = [gxEnterWraps, gxFixedCellClickSelect, gxTabWraps, gxMouseWheelScroll]
    RowCount = 2
    TabOrder = 8
    OnCustomSortRows = DBGridCustomSortRows
    OnDrawCell = DBGridDrawCell
    OnEditorCreate = DBGridEditorCreate
    ColWidths = (
      64
      64
      64
      64
      64
      200)
    RowHeights = (
      21
      21)
  end
  object BUAppend: TButton
    Left = 432
    Top = 62
    Width = 89
    Height = 25
    Action = ACAppend
    TabOrder = 9
  end
  object ALMain: TActionList
    Left = 440
    Top = 56
    object ACOpen: TAction
      Caption = 'Open table'
      OnExecute = ACOpenExecute
      OnUpdate = ACOpenUpdate
    end
    object ACClose: TAction
      Caption = 'Close table'
      OnExecute = ACCloseExecute
      OnUpdate = ACCloseUpdate
    end
    object ACPrint: TAction
      Caption = 'Print...'
      OnExecute = ACPrintExecute
      OnUpdate = ACPrintUpdate
    end
    object ACAppend: TAction
      Caption = 'Append Row'
      OnExecute = ACAppendExecute
      OnUpdate = ACAppendUpdate
    end
  end
  object DSMain: TDataSource
    Left = 480
    Top = 56
  end
  object PSDMain: TKPrintSetupDialog
    Control = DBGrid
    SelAvail = False
    Left = 416
    Top = 96
  end
end
