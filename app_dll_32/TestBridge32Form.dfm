object FormTestBridge32: TFormTestBridge32
  Left = 0
  Top = 0
  Caption = 'Test CylinderBridge32 - Clean 32bit'#226#8224#8217'64bit Architecture'
  ClientHeight = 471
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 89
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 782
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 45
      Height = 13
      Caption = 'File Path:'
    end
    object Label2: TLabel
      Left = 16
      Top = 67
      Width = 35
      Height = 13
      Caption = 'Status:'
    end
    object LblStatus: TLabel
      Left = 72
      Top = 67
      Width = 45
      Height = 13
      Caption = 'Ready...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object BtnSelectFile: TButton
      Left = 696
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Select...'
      TabOrder = 0
      OnClick = BtnSelectFileClick
    end
    object EdtFilePath: TEdit
      Left = 72
      Top = 16
      Width = 618
      Height = 21
      TabOrder = 1
    end
    object BtnProcessMemory: TButton
      Left = 16
      Top = 43
      Width = 139
      Height = 25
      Caption = 'Process via Bridge32'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = BtnProcessMemoryClick
    end
    object BtnGetVersion: TButton
      Left = 161
      Top = 43
      Width = 75
      Height = 25
      Caption = 'Get Version'
      TabOrder = 3
      OnClick = BtnGetVersionClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 89
    Width = 784
    Height = 382
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 782
    ExplicitHeight = 374
  end
end
