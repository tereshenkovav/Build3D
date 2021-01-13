object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = #1058#1088#1077#1093#1084#1077#1088#1085#1099#1081' '#1088#1077#1076#1072#1082#1090#1086#1088' '#1082#1091#1073#1080#1082#1086#1074
  ClientHeight = 715
  ClientWidth = 666
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTool: TPanel
    Left = 488
    Top = 0
    Width = 178
    Height = 715
    Align = alRight
    TabOrder = 0
    ExplicitLeft = 494
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 81
      Height = 13
      Caption = #1048#1085#1089#1090#1088#1091#1084#1077#1085#1090#1099
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 6
      Top = 324
      Width = 47
      Height = 13
      Caption = #1058#1077#1082#1089#1090#1091#1088#1072
    end
    object Label3: TLabel
      Left = 8
      Top = 416
      Width = 63
      Height = 13
      Caption = #1063#1080#1089#1083#1086' '#1082#1091#1073#1086#1074
      Visible = False
    end
    object Label4: TLabel
      Left = 6
      Top = 464
      Width = 137
      Height = 13
      Caption = #1056#1072#1079#1084#1077#1088#1099' '#1087#1072#1088#1072#1083#1083#1077#1083#1077#1087#1080#1087#1077#1076#1072
    end
    object Label5: TLabel
      Left = 6
      Top = 370
      Width = 98
      Height = 13
      Caption = #1058#1077#1082#1089#1090#1091#1088#1099' '#1074' '#1084#1086#1076#1077#1083#1080
    end
    object LabR: TLabel
      Left = 6
      Top = 416
      Width = 99
      Height = 13
      Caption = #1056#1072#1076#1080#1091#1089' '#1087#1088#1080#1084#1077#1085#1077#1085#1080#1103
    end
    object comboTexs: TComboBox
      Left = 6
      Top = 343
      Width = 163
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object EdCubeN: TEdit
      Left = 8
      Top = 432
      Width = 33
      Height = 21
      TabOrder = 1
      Text = '3'
      Visible = False
    end
    object EdParX: TEdit
      Left = 6
      Top = 480
      Width = 35
      Height = 21
      TabOrder = 2
      Text = '3'
    end
    object EdParY: TEdit
      Left = 47
      Top = 480
      Width = 35
      Height = 21
      TabOrder = 3
      Text = '2'
    end
    object EdParZ: TEdit
      Left = 88
      Top = 480
      Width = 35
      Height = 21
      TabOrder = 4
      Text = '1'
    end
    object Panel1: TPanel
      Left = 1
      Top = 577
      Width = 176
      Height = 137
      Align = alBottom
      TabOrder = 5
      object stModelSize: TStaticText
        Left = 5
        Top = 8
        Width = 164
        Height = 41
        AutoSize = False
        BorderStyle = sbsSingle
        Caption = 'stModelSize'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 0
      end
      object stFPS: TStaticText
        Left = 5
        Top = 55
        Width = 164
        Height = 41
        AutoSize = False
        BorderStyle = sbsSingle
        Caption = 'FPS'
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 1
      end
    end
    object ComboTexsInModel: TComboBox
      Left = 6
      Top = 389
      Width = 163
      Height = 21
      Style = csDropDownList
      TabOrder = 6
      OnChange = ComboTexsInModelChange
    end
    object rbFullBlock: TRadioButton
      Left = 6
      Top = 507
      Width = 91
      Height = 17
      Caption = #1055#1086#1083#1085#1099#1081' '#1073#1083#1086#1082
      Checked = True
      TabOrder = 7
      TabStop = True
    end
    object rbUpper: TRadioButton
      Left = 6
      Top = 530
      Width = 113
      Height = 17
      Caption = #1042#1077#1088#1093#1085#1080#1081' '#1087#1086#1083#1091#1073#1083#1086#1082
      TabOrder = 8
    end
    object rbLower: TRadioButton
      Left = 6
      Top = 553
      Width = 113
      Height = 17
      Caption = #1053#1080#1078#1085#1080#1081' '#1087#1086#1083#1091#1073#1083#1086#1082
      TabOrder = 9
    end
    object cbSetToEdge: TCheckBox
      Left = 56
      Top = 435
      Width = 113
      Height = 17
      Caption = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1082#1088#1072#1077#1084
      TabOrder = 10
      Visible = False
    end
  end
  object PanelGL: TPanel
    Left = 0
    Top = 0
    Width = 488
    Height = 715
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = PanelGLClick
    OnMouseMove = PanelGLMouseMove
    OnMouseUp = PanelGLMouseUp
  end
  object MainMenu1: TMainMenu
    OwnerDraw = True
    Left = 288
    Top = 264
    object N1: TMenuItem
      Caption = #1060#1072#1081#1083
      object NNew: TMenuItem
        Caption = #1053#1086#1074#1099#1081
        OnClick = NNewClick
      end
      object NOpen: TMenuItem
        Caption = #1054#1090#1082#1088#1099#1090#1100
        OnClick = NOpenClick
      end
      object NSave: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
        OnClick = NSaveClick
      end
      object NSaveAs: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1082#1072#1082
        OnClick = NSaveAsClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object NExit: TMenuItem
        Caption = #1042#1099#1093#1086#1076
        OnClick = NExitClick
      end
    end
    object N8: TMenuItem
      Caption = #1055#1088#1072#1074#1082#1072
      object NUndo: TMenuItem
        Caption = #1054#1090#1084#1077#1085#1080#1090#1100' '#1087#1086#1089#1083#1077#1076#1085#1077#1077' '#1076#1077#1081#1089#1090#1074#1080#1077
        OnClick = NUndoClick
      end
      object NRedo: TMenuItem
        Caption = #1055#1086#1074#1090#1086#1088#1080#1090#1100' '#1087#1086#1089#1083#1077#1076#1085#1077#1077' '#1076#1077#1081#1089#1090#1074#1080#1077
        OnClick = NRedoClick
      end
      object NSlice: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1087#1086' '#1089#1083#1086#1103#1084
        OnClick = NSliceClick
      end
      object NBlockReport: TMenuItem
        Caption = #1054#1090#1095#1077#1090' '#1086' '#1073#1083#1086#1082#1072#1093
        OnClick = NBlockReportClick
      end
    end
    object N3: TMenuItem
      Caption = #1059#1089#1090#1072#1085#1086#1074#1082#1080
      object NSetSizeLimit: TMenuItem
        Caption = #1054#1075#1088#1072#1085#1080#1095#1077#1085#1080#1103' '#1088#1072#1079#1084#1077#1088#1072
        OnClick = NSetSizeLimitClick
      end
      object NSetBackColor: TMenuItem
        Caption = #1062#1074#1077#1090' '#1092#1086#1085#1072
        OnClick = NSetBackColorClick
      end
      object NShowBorder: TMenuItem
        Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1088#1077#1073#1088#1072
        OnClick = NShowBorderClick
      end
      object NBridght: TMenuItem
        Caption = #1071#1088#1082#1086#1089#1090#1100' '#1075#1088#1072#1085#1077#1081
        OnClick = NBridghtClick
      end
    end
    object N5: TMenuItem
      Caption = #1056#1077#1078#1080#1084#1099
      object NDecart: TMenuItem
        Caption = #1057#1074#1086#1073#1086#1076#1085#1086#1077' '#1076#1074#1080#1078#1077#1085#1080#1077' '#1082#1072#1084#1077#1088#1099' - F6 ('#1076#1077#1082#1072#1088#1090#1086#1074#1099' '#1082#1086#1086#1088#1076#1080#1085#1072#1090#1099')'
        OnClick = NDecartClick
      end
      object NSphere: TMenuItem
        Caption = #1042#1088#1072#1097#1077#1085#1080#1077' '#1082#1072#1084#1077#1088#1099' '#1074#1086#1082#1088#1091#1075' '#1090#1086#1095#1082#1080' - F7 ('#1089#1092#1077#1088#1080#1095#1077#1089#1082#1080#1077' '#1082#1086#1086#1088#1076#1080#1085#1072#1090#1099') '
        OnClick = NSphereClick
      end
      object N7: TMenuItem
        Caption = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1085#1086#1074#1099#1081' '#1094#1077#1085#1090#1088' '#1074#1088#1072#1097#1077#1085#1080#1103' (F8)'
        OnClick = N7Click
      end
      object NSetDefaultCenter: TMenuItem
        Caption = #1042#1077#1088#1085#1091#1090#1100' '#1080#1079#1085#1072#1095#1072#1083#1100#1085#1099#1081' ('#1085#1091#1083#1077#1074#1086#1081') '#1094#1077#1085#1090#1088' '#1074#1088#1072#1097#1077#1085#1080#1103
        OnClick = NSetDefaultCenterClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object NPreview: TMenuItem
        Caption = #1042#1099#1076#1077#1083#1077#1085#1080#1077' '#1095#1077#1088#1077#1079' '#1087#1088#1077#1076#1087#1088#1086#1089#1084#1086#1090#1088' '#1080#1079#1084#1077#1085#1077#1085#1080#1081
        OnClick = NPreviewClick
      end
      object NGreenSide: TMenuItem
        Caption = #1042#1099#1076#1077#1083#1077#1085#1080#1077' '#1095#1077#1088#1077#1079' '#1079#1077#1083#1077#1085#1091#1102' '#1075#1088#1072#1085#1100
        OnClick = NGreenSideClick
      end
      object NNoSelection: TMenuItem
        Caption = #1054#1090#1082#1083#1102#1095#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1080#1077' '#1089#1086#1074#1089#1077#1084
        OnClick = NNoSelectionClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = #1052#1086#1076#1077#1083#1080' (*.model)|*.model|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    FilterIndex = 0
    Left = 136
    Top = 408
  end
  object SaveDialog1: TSaveDialog
    Filter = #1052#1086#1076#1077#1083#1080' (*.model)|*.model|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    FilterIndex = 0
    Left = 232
    Top = 408
  end
  object ColorDialog1: TColorDialog
    Left = 320
    Top = 416
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 160
    Top = 320
  end
end
