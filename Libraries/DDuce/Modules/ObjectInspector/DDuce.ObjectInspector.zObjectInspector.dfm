object frmComponentInspectorzObjectInspector: TfrmComponentInspectorzObjectInspector
  Left = 0
  Top = 0
  Width = 400
  Height = 686
  AlphaBlend = True
  AutoScroll = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poDefault
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 20
  OnActivate = FormActivate
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 647
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object cbxInspector: TComboBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 378
      Height = 21
      Margins.Bottom = 0
      Align = alTop
      TabOrder = 0
      OnChange = cbxInspectorChange
    end
  end
  object aclMain: TActionList
    Left = 192
    Top = 328
    object actExpandAll: TAction
      Caption = 'Expand all'
      OnExecute = actExpandAllExecute
    end
    object actCollapseAll: TAction
      Caption = 'Collapse all'
      OnExecute = actCollapseAllExecute
    end
  end
end
