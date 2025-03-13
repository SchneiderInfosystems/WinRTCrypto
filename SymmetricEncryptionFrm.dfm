object FrmSymmetricEncryption: TFrmSymmetricEncryption
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Symmetric Encryption'
  ClientHeight = 774
  ClientWidth = 1099
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 168
  DesignSize = (
    1099
    774)
  TextHeight = 30
  object ShapeResult: TShape
    Left = 42
    Top = 646
    Width = 999
    Height = 114
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    Pen.Width = 2
    Shape = stRoundRect
  end
  object LabelAlgo: TLabel
    Left = 42
    Top = 56
    Width = 92
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Algorithm'
  end
  object LabelKey: TLabel
    Left = 42
    Top = 201
    Width = 33
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Key'
  end
  object LabelResult: TLabel
    Left = 96
    Top = 683
    Width = 903
    Height = 35
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 35
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object LabelKeySize: TLabel
    Left = 294
    Top = 133
    Width = 125
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Key size [bits]'
  end
  object LabelIV: TLabel
    Left = 200
    Top = 373
    Width = 19
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'IV'
  end
  object Label1: TLabel
    Left = 42
    Top = 243
    Width = 90
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Clear Text'
  end
  object Label2: TLabel
    Left = 42
    Top = 418
    Width = 62
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Chiffre'
  end
  object Label3: TLabel
    Left = 42
    Top = 558
    Width = 181
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Resulting Clear Text'
  end
  object cboAlgo: TComboBox
    Left = 196
    Top = 51
    Width = 607
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 1
    TabOrder = 0
    Text = 'AesCbcPkcs7'
    OnChange = cboAlgoChange
    Items.Strings = (
      'AesCbc'
      'AesCbcPkcs7'
      'AesEcb'
      'AesEcbPkcs7')
  end
  object btnCreateKey: TButton
    Left = 42
    Top = 126
    Width = 198
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Create Key'
    TabOrder = 1
    OnClick = btnCreateKeyClick
  end
  object EditKey: TEdit
    Left = 200
    Top = 196
    Width = 831
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EditKeyChange
  end
  object EditClear: TEdit
    Left = 42
    Top = 280
    Width = 989
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    TextHint = 'Clear text'
  end
  object btnEncrypt: TButton
    Left = 42
    Top = 364
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Encrypt'
    Enabled = False
    TabOrder = 4
    OnClick = btnEncryptClick
  end
  object EditEncrypted: TEdit
    Left = 42
    Top = 448
    Width = 989
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = EditEncryptedChange
  end
  object btnDecrypt: TButton
    Left = 42
    Top = 499
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Decrypt'
    Enabled = False
    TabOrder = 6
    OnClick = btnDecryptClick
  end
  object EditResult: TEdit
    Left = 42
    Top = 595
    Width = 999
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 7
    TextHint = 'resulting clear text'
  end
  object btnSaveEncrypt: TButton
    Left = 779
    Top = 364
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Save'
    Enabled = False
    TabOrder = 8
    OnClick = btnSaveEncryptClick
  end
  object btnLoadEncrypt: TButton
    Left = 921
    Top = 364
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Load'
    Enabled = False
    TabOrder = 9
    OnClick = btnLoadEncryptClick
  end
  object btnSaveKey: TButton
    Left = 770
    Top = 126
    Width = 119
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akTop, akRight]
    Caption = 'Save Key'
    Enabled = False
    TabOrder = 10
    OnClick = btnSaveKeyClick
  end
  object btnLoadKey: TButton
    Left = 900
    Top = 126
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akTop, akRight]
    Caption = 'Load Key'
    TabOrder = 11
    OnClick = btnLoadKeyClick
  end
  object EditKeySize: TEdit
    Left = 448
    Top = 128
    Width = 184
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ReadOnly = True
    TabOrder = 12
  end
  object cboKeySize: TComboBox
    Left = 910
    Top = 51
    Width = 121
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Style = csDropDownList
    Anchors = [akRight]
    ItemIndex = 2
    TabOrder = 13
    Text = '256'
    OnChange = cboAlgoChange
    Items.Strings = (
      '128'
      '192'
      '256')
  end
  object EditIV: TEdit
    Left = 238
    Top = 366
    Width = 355
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 14
    OnChange = EditIVChange
  end
  object chbAutoRenew: TCheckBox
    Left = 603
    Top = 366
    Width = 160
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Auto renew'
    Checked = True
    State = cbChecked
    TabOrder = 15
  end
end
