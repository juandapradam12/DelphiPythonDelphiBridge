object FormTestDLL: TFormTestDLL
  Left = 0
  Top = 0
  Caption = 'TestDLL64 - CylinderCore64 DLL Test (Form-based like MainForm)'
  ClientHeight = 600
  ClientWidth = 800
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
    Width = 800
    Height = 121
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    ExplicitWidth = 798
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 45
      Height = 13
      Caption = 'File Path:'
    end
    object Label2: TLabel
      Left = 16
      Top = 88
      Width = 35
      Height = 13
      Caption = 'Status:'
    end
    object LblStatus: TLabel
      Left = 64
      Top = 88
      Width = 36
      Height = 13
      Caption = 'Ready'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object EdtFilePath: TEdit
      Left = 16
      Top = 35
      Width = 489
      Height = 21
      TabOrder = 0
    end
    object BtnSelectFile: TButton
      Left = 520
      Top = 33
      Width = 75
      Height = 25
      Caption = 'Select File...'
      TabOrder = 1
      OnClick = BtnSelectFileClick
    end
    object BtnInitialize: TButton
      Left = 616
      Top = 33
      Width = 75
      Height = 25
      Caption = 'Initialize'
      TabOrder = 2
      OnClick = BtnInitializeClick
    end
    object BtnProcessFile: TButton
      Left = 704
      Top = 33
      Width = 75
      Height = 25
      Caption = 'Process File'
      TabOrder = 3
      OnClick = BtnProcessFileClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 121
    Width = 800
    Height = 479
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitWidth = 798
    ExplicitHeight = 471
  end
end
