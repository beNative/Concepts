object frmcxGridViewPresenter: TfrmcxGridViewPresenter
  Left = 0
  Top = 0
  Caption = 'Specialized presenters'
  ClientHeight = 556
  ClientWidth = 792
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
    Width = 792
    Height = 537
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      792
      537)
    object pgcMain: TPageControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 786
      Height = 495
      ActivePage = tsGridView
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      object tsGridView: TTabSheet
        Caption = 'DevExpress TcxGrid'
        object grdMain: TcxGrid
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 772
          Height = 461
          Align = alClient
          TabOrder = 0
          object tvwMain: TcxGridTableView
            Navigator.Buttons.CustomButtons = <>
            Navigator.Buttons.Delete.Enabled = False
            Navigator.Buttons.Delete.Visible = False
            Navigator.Buttons.Edit.Enabled = False
            Navigator.Buttons.Edit.Visible = False
            Navigator.InfoPanel.Visible = True
            Navigator.Visible = True
            FindPanel.UseExtendedSyntax = True
            DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoFocusTopRowAfterSorting]
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            DataController.Summary.Options = [soMultipleSelectedRecords]
            Filtering.ColumnFilteredItemsList = True
            FilterRow.Visible = True
            OptionsBehavior.CellHints = True
            OptionsBehavior.DragDropText = True
            OptionsBehavior.GoToNextCellOnEnter = True
            OptionsBehavior.NavigatorHints = True
            OptionsBehavior.EditAutoHeight = eahEditor
            OptionsBehavior.ImmediateEditor = False
            OptionsCustomize.ColumnHiding = True
            OptionsCustomize.ColumnsQuickCustomization = True
            OptionsData.Appending = True
            OptionsSelection.MultiSelect = True
            OptionsView.ColumnAutoWidth = True
            OptionsView.Footer = True
            OptionsView.FooterAutoHeight = True
            OptionsView.FooterMultiSummaries = True
            OptionsView.GridLineColor = clSilver
            OptionsView.GridLines = glVertical
            OptionsView.GroupFooters = gfVisibleWhenExpanded
          end
          object grlMain: TcxGridLevel
            GridView = tvwMain
          end
        end
      end
      object tsTreelist: TTabSheet
        Caption = 'DevExpress TcxVirtualTreelist'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 821
        ExplicitHeight = 498
        object lstMain: TcxVirtualTreeList
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 772
          Height = 461
          Align = alClient
          Bands = <
            item
            end>
          Navigator.Buttons.CustomButtons = <>
          OptionsBehavior.ImmediateEditor = False
          OptionsView.ColumnAutoWidth = True
          OptionsView.GridLineColor = clSilver
          TabOrder = 0
          ExplicitWidth = 815
          ExplicitHeight = 492
          object tlcMainColumn1: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Width = 100
            Position.ColIndex = 0
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object tlcMainColumn2: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Width = 100
            Position.ColIndex = 1
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object tlcMainColumn3: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Width = 100
            Position.ColIndex = 2
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object tlcMainColumn4: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Width = 100
            Position.ColIndex = 3
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object tlcMainColumn5: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Width = 100
            Position.ColIndex = 4
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object tlcMainColumn6: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Width = 100
            Position.ColIndex = 5
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
          object tlcMainColumn7: TcxTreeListColumn
            DataBinding.ValueType = 'String'
            Width = 100
            Position.ColIndex = 6
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
        end
      end
      object tsVirtualTree: TTabSheet
        Caption = 'Virtual Treeview - TVirtualStringTree'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 821
        ExplicitHeight = 498
        object vstMain: TVirtualStringTree
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 772
          Height = 461
          Align = alClient
          Header.AutoSizeIndex = -1
          Header.MainColumn = -1
          Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
          NodeDataSize = 4
          TabOrder = 0
          ExplicitWidth = 815
          ExplicitHeight = 492
          Columns = <>
        end
      end
    end
    object btnFillList: TButton
      Left = 632
      Top = 504
      Width = 150
      Height = 26
      Action = actFillList
      Anchors = [akRight, akBottom]
      Default = True
      Images = dmResources.imlMain
      TabOrder = 1
    end
    object btnInspectTreeListPresenter: TButton
      Left = 320
      Top = 504
      Width = 150
      Height = 25
      Action = actInspectTreeListPresenter
      Anchors = [akRight, akBottom]
      TabOrder = 2
    end
    object btnInspectGridViewPresenter: TButton
      Left = 476
      Top = 504
      Width = 150
      Height = 26
      Action = actInspectGridViewPresenter
      Anchors = [akRight, akBottom]
      TabOrder = 3
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 537
    Width = 792
    Height = 19
    Panels = <>
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 96
    Top = 72
    object actFillList: TAction
      Caption = 'Populate contactlist'
      ImageIndex = 518
      OnExecute = actFillListExecute
    end
    object actInspectGridViewPresenter: TAction
      Caption = 'Inspect GridView presenter'
      OnExecute = actInspectGridViewPresenterExecute
    end
    object actInspectTreeListPresenter: TAction
      Caption = 'Inspect TreeList presenter'
      OnExecute = actInspectTreeListPresenterExecute
    end
  end
end
