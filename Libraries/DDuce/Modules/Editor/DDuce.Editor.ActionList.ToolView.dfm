object frmActionListView: TfrmActionListView
  Left = 506
  Top = 90
  ActiveControl = edtFilterActions
  Caption = 'Actions'
  ClientHeight = 526
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlEditorList: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 526
    Align = alClient
    BevelOuter = bvSpace
    TabOrder = 0
    object pgcMain: TPageControl
      Left = 1
      Top = 1
      Width = 798
      Height = 524
      ActivePage = tsActions
      Align = alClient
      TabOrder = 0
      object tsActions: TTabSheet
        Caption = 'Actions'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnlActions: TPanel
          Left = 0
          Top = 21
          Width = 790
          Height = 475
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
        end
        object edtFilterActions: TEdit
          Left = 0
          Top = 0
          Width = 790
          Height = 21
          Align = alTop
          TabOrder = 1
          OnChange = edtFilterActionsChange
          OnKeyDown = edtFilterActionsKeyDown
          OnKeyUp = edtFilterActionsKeyUp
        end
      end
      object tsCommands: TTabSheet
        Caption = 'Editor key commands'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
    end
  end
end
