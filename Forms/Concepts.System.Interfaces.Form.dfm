object frmInterfaces: TfrmInterfaces
  Left = 259
  Top = 141
  Caption = 'Interfaces Demo'
  ClientHeight = 711
  ClientWidth = 878
  Color = clBtnFace
  Constraints.MinWidth = 759
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    878
    711)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl5: TLabel
    Left = 326
    Top = 1
    Width = 542
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This demo uses a class derived from TInterfacedObject which enab' +
      'les us to track its method calls and reference count. Aside from' +
      ' logging what happens it behaves exactly like the stock implemen' +
      'tation would.'
    Color = clInfoBk
    EllipsisPosition = epEndEllipsis
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
    WordWrap = True
  end
  object chkReportMemoryLeaksOnShutdown: TCheckBox
    Left = 8
    Top = 8
    Width = 209
    Height = 17
    Caption = 'ReportMemoryLeaksOnShutdown'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = chkReportMemoryLeaksOnShutdownClick
  end
  object lbxLog: TListBox
    Left = 8
    Top = 247
    Width = 861
    Height = 463
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
    Constraints.MinHeight = 1
    ExtendedSelect = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Consolas'
    Font.Style = []
    ItemHeight = 18
    Items.Strings = (
      '')
    ParentFont = False
    TabOrder = 1
  end
  object pgcMain: TPageControl
    Left = 8
    Top = 31
    Width = 862
    Height = 210
    ActivePage = tsWeakReferences
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    object tsGUIDs: TTabSheet
      Caption = '1. GUIDs'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbl1: TLabel
        Left = 11
        Top = 34
        Width = 312
        Height = 78
        BiDiMode = bdLeftToRight
        Caption = 
          'type'#13#10'  IRunnable = interface'#13#10'  ['#39'{47F16423-1E0B-41C7-A9DA-925A' +
          'E066BA41}'#39']'#13#10#13#10'    procedure Run(ARaiseException: Boolean = Fals' +
          'e);'#13#10'  end;'
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentBiDiMode = False
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object btnShowInterfaceGUID: TButton
        Left = 11
        Top = 3
        Width = 238
        Height = 25
        Action = actShowInterfaceGUID
        TabOrder = 0
      end
    end
    object tsInterfaceInternals: TTabSheet
      Caption = '2. Interface internals'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object btnExecute: TBitBtn
        Left = 16
        Top = 39
        Width = 353
        Height = 26
        Action = actRun
        Caption = 'Call IRunnable.Run'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Consolas'
        Font.Style = []
        Margin = 1
        ParentDoubleBuffered = True
        ParentFont = False
        TabOrder = 0
      end
      object chkRaiseException: TCheckBox
        Left = 16
        Top = 16
        Width = 257
        Height = 17
        Caption = 'Raise exception in call to IRunnable.Run'
        TabOrder = 1
      end
    end
    object tsReferenceCounting: TTabSheet
      Caption = '3. Reference counting'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlOperations: TPanel
        Left = 0
        Top = 0
        Width = 854
        Height = 182
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object lblInterfaceDeclatation: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 45
          Width = 300
          Height = 18
          Hint = 
            '  IDemoInterface = interface'#13#10'    ['#39'{CFAEC497-C115-4BC9-B954-952' +
            '8B772E0A4}'#39']'#13#10'    function GetOnAddRef: TRefCountEvent;'#13#10'    fun' +
            'ction GetOnRelease: TRefCountEvent;'#13#10'    procedure SetOnAddRef(c' +
            'onst Value: TRefCountEvent);'#13#10'    procedure SetOnRelease(const V' +
            'alue: TRefCountEvent);'#13#10'    function GetRefCount: Integer;'#13#10'    ' +
            'function GetIsRefCounted: Boolean;'#13#10'    procedure SetIsRefCounte' +
            'd(const Value: Boolean);'#13#10#13#10'    property RefCount: Integer'#13#10'    ' +
            '  read GetRefCount;'#13#10#13#10'    property IsRefCounted : Boolean'#13#10'    ' +
            '  read GetIsRefCounted write SetIsRefCounted;'#13#10#13#10'    property On' +
            'AddRef : TRefCountEvent'#13#10'      read GetOnAddRef write SetOnAddRe' +
            'f;'#13#10#13#10'    property OnRelease : TRefCountEvent'#13#10'      read GetOnR' +
            'elease write SetOnRelease;'#13#10'  end;'
          Margins.Left = 10
          AutoSize = False
          Caption = 'FInterface : IInterface;'
          Color = clBlack
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object lblObjectDeclaration: TLabel
          AlignWithMargins = True
          Left = 375
          Top = 45
          Width = 300
          Height = 18
          Margins.Left = 10
          AutoSize = False
          Caption = 'FObject    : TInterfacedObject;'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object lblObjectRefCount: TLabel
          Left = 514
          Top = 267
          Width = 83
          Height = 13
          Caption = 'Object Refcount:'
        end
        object txtObjectRefCount: TStaticText
          Left = 617
          Top = 263
          Width = 65
          Height = 17
          Alignment = taCenter
          AutoSize = False
          BevelInner = bvNone
          BevelKind = bkSoft
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 6
        end
        object btnCreateInterfaceVar: TBitBtn
          Left = 16
          Top = 83
          Width = 353
          Height = 26
          Margins.Left = 0
          Action = actCreateInterfaceInstance
          Caption = 'FInterface := TInterfacedObject.Create;'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          Margin = 1
          ParentDoubleBuffered = True
          ParentFont = False
          TabOrder = 0
        end
        object btnCreateObjectVar: TBitBtn
          Left = 375
          Top = 83
          Width = 338
          Height = 26
          Action = actCreateObjectInstance
          Caption = 'FObject := TInterfacedObject.Create;'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          Margin = 1
          ParentDoubleBuffered = True
          ParentFont = False
          TabOrder = 1
        end
        object btnAssignObjectVarToInterfaceVar: TBitBtn
          Left = 16
          Top = 115
          Width = 353
          Height = 26
          Action = actAssignObjectVarToInterfaceVar
          Caption = 'FInterface := FObject;'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          Margin = 1
          ParentDoubleBuffered = True
          ParentFont = False
          TabOrder = 2
        end
        object btnFreeInterfaceVar: TBitBtn
          Left = 16
          Top = 147
          Width = 353
          Height = 26
          Action = actFreeInterfaceInstance
          Caption = 'FInterface := nil;'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          Margin = 1
          ParentDoubleBuffered = True
          ParentFont = False
          TabOrder = 3
        end
        object btnFreeObjectVar: TBitBtn
          Left = 375
          Top = 147
          Width = 338
          Height = 26
          Action = actFreeObjectInstance
          Caption = 'FreeAndNil(FObject);'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          Margin = 1
          ParentDoubleBuffered = True
          ParentFont = False
          TabOrder = 4
        end
        object btnAssignInterfaceVarToObjectVar: TBitBtn
          Left = 375
          Top = 115
          Width = 338
          Height = 26
          Action = actAssignInterfaceVarToObjectVar
          Caption = 'FObject := FInterface;'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          Margin = 1
          ParentDoubleBuffered = True
          ParentFont = False
          TabOrder = 5
        end
        object chkRefCounted: TCheckBox
          Left = 16
          Top = 16
          Width = 189
          Height = 17
          Caption = 'IsRefCounted := True;'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          State = cbChecked
          TabOrder = 7
        end
      end
    end
    object tsImplementationByAggregation: TTabSheet
      Caption = '4. Implementation by Aggregation'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        854
        182)
      object lbl2: TLabel
        Left = 273
        Top = 65
        Width = 264
        Height = 65
        BiDiMode = bdLeftToRight
        Caption = 
          'type'#13#10'  IInnerInterface = interface'#13#10'  ['#39'{F5D6310C-1F21-40D2-847' +
          'C-1E1D7FA8632D}'#39']'#13#10'    procedure InnerMethod;'#13#10'  end;'
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentBiDiMode = False
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object lbl3: TLabel
        Left = 5
        Top = 65
        Width = 264
        Height = 65
        BiDiMode = bdLeftToRight
        Caption = 
          'type'#13#10'  IOuterInterface = interface'#13#10'  ['#39'{C5AB5C8E-052D-4BC8-B75' +
          '1-4162BB36995F}'#39']'#13#10'    procedure OuterMethod;'#13#10'  end;'
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentBiDiMode = False
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object lbl4: TLabel
        Left = 543
        Top = 3
        Width = 306
        Height = 182
        Anchors = [akLeft, akTop, akRight]
        BiDiMode = bdLeftToRight
        Caption = 
          'type'#13#10'  TOuterObject = class(TInterfacedObject, '#13#10'    IOuterInte' +
          'rface, IInnerInterface)'#13#10'  private'#13#10'    FInnerObject    : TInner' +
          'Object;'#13#10'    FInnerInterface : IInnerInterface;'#13#10'  protected'#13#10'  ' +
          '  procedure OuterMethod;'#13#10'    property InnerObject: TInnerObject' +
          #13#10'      read FInnerObject implements IInnerInterface;'#13#10'  public'#13 +
          #10'    procedure BeforeDestruction; override;'#13#10'  end;'#13#10
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentBiDiMode = False
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object btnCreateInnerInterface: TBitBtn
        Left = 273
        Top = 3
        Width = 264
        Height = 25
        Action = actCreateInnerInterfaceInstance
        Caption = 'Create IInnerInterface'
        TabOrder = 0
      end
      object btnCreateOuterInterface: TBitBtn
        Left = 5
        Top = 3
        Width = 262
        Height = 25
        Action = actCreateOuterInterfaceInstance
        Caption = 'Create IOuterInterface instance'
        TabOrder = 1
      end
      object btnCallInnerMethod: TButton
        Left = 273
        Top = 34
        Width = 264
        Height = 25
        Action = actCallInnerMethod
        TabOrder = 2
      end
      object btnCallOuterMethod: TButton
        Left = 5
        Top = 34
        Width = 262
        Height = 25
        Action = actCallOuterMethod
        TabOrder = 3
      end
    end
    object tsVirtualInterface: TTabSheet
      Caption = '5. TVirtualInterface'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblDefinition: TLabel
        Left = 11
        Top = 34
        Width = 306
        Height = 78
        BiDiMode = bdLeftToRight
        Caption = 
          'type'#13#10'  IGoStop = interface(IInvokable)'#13#10'  ['#39'{DAA28BA6-2243-41BE' +
          '-BC61-2A548999753A}'#39']'#13#10'    procedure Go(AInteger: Integer = 5);'#13 +
          #10'    procedure Stop(const AString: string = '#39'TEST'#39');'#13#10'  end;'
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentBiDiMode = False
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object btnGo: TButton
        Left = 175
        Top = 3
        Width = 75
        Height = 25
        Action = actGo
        TabOrder = 0
      end
      object btnStop: TButton
        Left = 256
        Top = 3
        Width = 75
        Height = 25
        Action = actStop
        TabOrder = 1
      end
      object btnGo1: TButton
        Left = 11
        Top = 3
        Width = 158
        Height = 25
        Action = actCreateIGoStopInstance
        TabOrder = 2
      end
    end
    object tsWeakReferences: TTabSheet
      Caption = '6. Weak references'
      ImageIndex = 5
      object lblRefCount1: TLabel
        Left = 160
        Top = 14
        Width = 62
        Height = 13
        Caption = 'lblRefCount1'
      end
      object lblRefCount2: TLabel
        Left = 160
        Top = 45
        Width = 62
        Height = 13
        Caption = 'lblRefCount1'
      end
      object btnAssignReferences: TButton
        Left = 12
        Top = 71
        Width = 129
        Height = 25
        Action = actAssignReferences
        TabOrder = 0
      end
      object btnCreateObject1: TButton
        Left = 12
        Top = 9
        Width = 129
        Height = 25
        Action = actCreateObject1
        TabOrder = 1
      end
      object btnCreateObject2: TButton
        Left = 12
        Top = 40
        Width = 129
        Height = 25
        Action = actCreateObject2
        TabOrder = 2
      end
      object btnAssignNilToInterface1: TButton
        Left = 280
        Top = 9
        Width = 145
        Height = 25
        Action = actAssignNilToInterface1
        TabOrder = 3
      end
      object btnAssignNilToInterface2: TButton
        Left = 280
        Top = 40
        Width = 145
        Height = 25
        Action = actAssignNilToInterface2
        TabOrder = 4
      end
      object chkUseWeakReference1: TCheckBox
        Left = 24
        Top = 112
        Width = 265
        Height = 17
        Caption = 'Use weak reference for IInterface1 field'
        TabOrder = 5
      end
      object chkUseWeakReference2: TCheckBox
        Left = 24
        Top = 135
        Width = 233
        Height = 18
        Caption = 'Use weak reference for IInterface2 field'
        TabOrder = 6
      end
    end
  end
  object btnClearLog: TButton
    Left = 194
    Top = 3
    Width = 121
    Height = 25
    Action = actClearLog
    TabOrder = 3
  end
  object aclMain: TActionList
    OnExecute = aclMainExecute
    Left = 336
    Top = 296
    object actAssignObjectVarToInterfaceVar: TAction
      Caption = 'FInterface := FObject;'
      OnExecute = actAssignObjectVarToInterfaceVarExecute
    end
    object actAssignInterfaceVarToObjectVar: TAction
      Caption = 'FObject := FInterface;'
      OnExecute = actAssignInterfaceVarToObjectVarExecute
    end
    object actFreeInterfaceInstance: TAction
      Caption = 'FInterface := nil;'
      OnExecute = actFreeInterfaceInstanceExecute
    end
    object actFreeObjectInstance: TAction
      Caption = 'FreeAndNil(FObject);'
      OnExecute = actFreeObjectInstanceExecute
    end
    object actCreateInterfaceInstance: TAction
      Caption = 'FInterface := TInterfacedObject.Create;'
      OnExecute = actCreateInterfaceInstanceExecute
    end
    object actCreateObjectInstance: TAction
      Caption = 'FObject := TInterfacedObject.Create;'
      OnExecute = actCreateObjectInstanceExecute
    end
    object actCreateInnerInterfaceInstance: TAction
      Caption = 'Create IInnerInterface'
      OnExecute = actCreateInnerInterfaceInstanceExecute
    end
    object actCreateOuterInterfaceInstance: TAction
      Caption = 'Create IOuterInterface instance'
      OnExecute = actCreateOuterInterfaceInstanceExecute
    end
    object actCallInnerMethod: TAction
      Caption = 'Call inner method'
      OnExecute = actCallInnerMethodExecute
    end
    object actRun: TAction
      Caption = 'Call IRunnable.Run'
      OnExecute = actRunExecute
    end
    object actShowInterfaceGUID: TAction
      Caption = 'Show GUID for IRunnable'
      OnExecute = actShowInterfaceGUIDExecute
    end
    object actGo: TAction
      Caption = 'Go'
      OnExecute = actGoExecute
    end
    object actStop: TAction
      Caption = 'Stop'
      OnExecute = actStopExecute
    end
    object actCreateIGoStopInstance: TAction
      Caption = 'Create IGoStop instance'
      OnExecute = actCreateIGoStopInstanceExecute
    end
    object actCallOuterMethod: TAction
      Caption = 'Call outer method'
      OnExecute = actCallOuterMethodExecute
    end
    object actClearLog: TAction
      Caption = 'Clear log'
      OnExecute = actClearLogExecute
    end
    object actCreateObject1: TAction
      Caption = 'Create Interface1'
      OnExecute = actCreateObject1Execute
    end
    object actCreateObject2: TAction
      Caption = 'Create Interface2'
      OnExecute = actCreateObject2Execute
    end
    object actAssignReferences: TAction
      Caption = 'Assign mutual references'
      OnExecute = actAssignReferencesExecute
    end
    object actAssignNilToInterface1: TAction
      Caption = 'Interface1 := nil;'
      OnExecute = actAssignNilToInterface1Execute
    end
    object actAssignNilToInterface2: TAction
      Caption = 'Interface2 := nil;'
      OnExecute = actAssignNilToInterface2Execute
    end
  end
end
