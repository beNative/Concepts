object frmActionListView: TfrmActionListView
  Left = 506
  Top = 90
  ActiveControl = edtFilterActions
  Caption = 'Actions'
  ClientHeight = 526
  ClientWidth = 806
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  TextHeight = 13
  object pnlEditorList: TPanel
    Left = 0
    Top = 0
    Width = 806
    Height = 526
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pgcMain: TPageControl
      Left = 0
      Top = 0
      Width = 806
      Height = 526
      ActivePage = tsActions
      Align = alClient
      TabOrder = 0
      object tsActions: TTabSheet
        Caption = 'Actions'
        object pnlActions: TPanel
          Left = 0
          Top = 21
          Width = 801
          Height = 480
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
        end
        object edtFilterActions: TEdit
          Left = 0
          Top = 0
          Width = 801
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
      end
    end
  end
end
