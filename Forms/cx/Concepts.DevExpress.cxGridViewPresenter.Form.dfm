object frmcxGridViewPresenter: TfrmcxGridViewPresenter
  Left = 0
  Top = 0
  Caption = 'Developer express presenters'
  ClientHeight = 587
  ClientWidth = 582
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 582
    Height = 568
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      582
      568)
    object pgcMain: TPageControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 576
      Height = 526
      ActivePage = tsGridView
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      object tsGridView: TTabSheet
        Caption = 'Gridview'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object grdMain: TcxGrid
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 562
          Height = 492
          Align = alClient
          TabOrder = 0
          object tvwMain: TcxGridTableView
            Navigator.Buttons.CustomButtons = <>
            DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoFocusTopRowAfterSorting]
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            OptionsBehavior.CellHints = True
            OptionsBehavior.DragDropText = True
            OptionsBehavior.GoToNextCellOnEnter = True
            OptionsBehavior.ImmediateEditor = False
            OptionsBehavior.EditAutoHeight = eahEditor
            OptionsData.Appending = True
            OptionsSelection.MultiSelect = True
            OptionsView.GridLineColor = clSilver
            object cxgrdclmnMainColumn1: TcxGridColumn
            end
            object cxgrdclmnMainColumn2: TcxGridColumn
            end
            object cxgrdclmnMainColumn3: TcxGridColumn
            end
            object cxgrdclmnMainColumn4: TcxGridColumn
            end
            object cxgrdclmnMainColumn5: TcxGridColumn
            end
          end
          object grlMain: TcxGridLevel
            GridView = tvwMain
          end
        end
      end
      object tsTreelist: TTabSheet
        Caption = 'Treelist'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lstMain: TcxVirtualTreeList
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 562
          Height = 492
          Align = alClient
          Bands = <
            item
            end>
          Navigator.Buttons.CustomButtons = <>
          OptionsBehavior.ImmediateEditor = False
          OptionsView.GridLineColor = clSilver
          TabOrder = 0
          object cxtrlstclmnMainColumn1: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Position.ColIndex = 0
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object cxtrlstclmnMainColumn2: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Position.ColIndex = 1
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object cxtrlstclmnMainColumn3: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Position.ColIndex = 2
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object cxtrlstclmnMainColumn4: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Position.ColIndex = 3
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object cxtrlstclmnMainColumn5: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Position.ColIndex = 4
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object cxtrlstclmnMainColumn6: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Position.ColIndex = 5
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
        end
      end
      object tsVirtualTree: TTabSheet
        Caption = 'VirtualStringTree'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object vstMain: TVirtualStringTree
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 562
          Height = 492
          Align = alClient
          Header.AutoSizeIndex = -1
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'Tahoma'
          Header.Font.Style = []
          Header.MainColumn = -1
          Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
          NodeDataSize = 4
          TabOrder = 0
          Columns = <>
        end
      end
    end
    object btnFillList: TButton
      Left = 497
      Top = 535
      Width = 75
      Height = 25
      Action = actFillList
      Anchors = [akRight, akBottom]
      TabOrder = 1
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 568
    Width = 582
    Height = 19
    Panels = <>
  end
  object aclMain: TActionList
    Left = 96
    Top = 72
    object actFillList: TAction
      Caption = 'Fill list'
      OnExecute = actFillListExecute
    end
  end
  object tvpMain: TTreeViewPresenter
    SelectionMode = smMulti
    ListMode = True
    TreeView = vstMain
    Left = 32
    Top = 176
    ColumnDefinitions = <
      item
        Caption = 'FirstName'
        ValuePropertyName = 'Firstname'
      end
      item
        Caption = 'LastName'
        ValuePropertyName = 'Lastname'
      end
      item
        Caption = 'CompanyName'
        ValuePropertyName = 'CompanyName'
      end
      item
        Caption = 'Email'
        ValuePropertyName = 'Email'
      end
      item
        Caption = 'Address'
        ValuePropertyName = 'Address'
      end>
  end
end
