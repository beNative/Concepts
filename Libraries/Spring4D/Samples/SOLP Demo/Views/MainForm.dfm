object CustomersView: TCustomersView
  Left = 0
  Top = 0
  Caption = 'CustomersView'
  ClientHeight = 289
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object VirtualStringTree1: TVirtualStringTree
    Left = 0
    Top = 41
    Width = 554
    Height = 248
    Align = alClient
    Colors.UnfocusedColor = clMedGray
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    TabOrder = 0
    Columns = <>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 10
      Width = 113
      Height = 25
      Caption = 'Load Customers'
      TabOrder = 0
    end
    object Edit1: TEdit
      Left = 136
      Top = 14
      Width = 185
      Height = 21
      TabOrder = 1
      TextHint = 'Search by customer id...'
    end
  end
  object CustomersPresenter: TTreeViewPresenter
    TreeView = VirtualStringTree1
    Left = 56
    Top = 64
    ColumnDefinitions = <
      item
        Caption = 'CustomerId'
        ValuePropertyName = 'CustomerId'
      end
      item
        Caption = 'CompanyName'
        ValuePropertyName = 'CompanyName'
        Width = 200
      end
      item
        Caption = 'City'
        ValuePropertyName = 'City'
      end>
  end
  object BindingGroup1: TBindingGroup
    Left = 176
    Top = 64
    Bindings = <
      item
        Source = Owner
        SourcePropertyName = 'DataContext.CustomerId'
        Target = Edit1
        TargetPropertyName = 'Text'
      end
      item
        BindingMode = bmOneWay
        Source = Owner
        SourcePropertyName = 'DataContext.Customers'
        Target = CustomersPresenter
        TargetPropertyName = 'View.ItemsSource'
      end
      item
        BindingMode = bmOneWay
        Source = Owner
        SourcePropertyName = 'DataContext.LoadCustomers'
        Target = Button1
        TargetPropertyName = 'OnClick'
      end>
  end
end
