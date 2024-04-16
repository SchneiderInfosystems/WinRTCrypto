object frmAsymmetricEncryption: TfrmAsymmetricEncryption
  Left = 0
  Top = 0
  Caption = 'Asymmetric Encryption'
  ClientHeight = 505
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    622
    505)
  TextHeight = 15
  object LabelAlgo: TLabel
    Left = 24
    Top = 32
    Width = 54
    Height = 15
    Caption = 'Algorithm'
  end
  object ShapeResult: TShape
    Left = 24
    Top = 416
    Width = 557
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Shape = stRoundRect
    ExplicitWidth = 577
  end
  object LabelResult: TLabel
    Left = 55
    Top = 440
    Width = 502
    Height = 20
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 522
  end
  object LabelPublicKey: TLabel
    Left = 24
    Top = 115
    Width = 55
    Height = 15
    Caption = 'Public Key'
  end
  object LabelPrivateKey: TLabel
    Left = 24
    Top = 144
    Width = 58
    Height = 15
    Caption = 'Private Key'
  end
  object LabelKeySize: TLabel
    Left = 429
    Top = 32
    Width = 71
    Height = 15
    Anchors = [akTop, akRight]
    Caption = 'Key size [bits]'
    ExplicitLeft = 437
  end
  object lblChiffreLen: TLabel
    Left = 440
    Top = 293
    Width = 129
    Height = 15
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    ExplicitLeft = 448
  end
  object Label1: TLabel
    Left = 24
    Top = 167
    Width = 51
    Height = 15
    Caption = 'Clear Text'
  end
  object lblResult: TLabel
    Left = 24
    Top = 247
    Width = 32
    Height = 15
    Caption = 'Result'
  end
  object lblResultingClearText: TLabel
    Left = 24
    Top = 347
    Width = 103
    Height = 15
    Caption = 'Resulting Clear Text'
  end
  object EditClear: TEdit
    Left = 24
    Top = 187
    Width = 551
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'Clear text'
    ExplicitWidth = 549
  end
  object btnEncrypt: TButton
    Left = 24
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Encrypt'
    Enabled = False
    TabOrder = 1
    OnClick = btnEncryptClick
  end
  object EditEncrypted: TEdit
    Left = 24
    Top = 268
    Width = 551
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EditEncryptedChange
    ExplicitWidth = 549
  end
  object btnDecrypt: TButton
    Left = 24
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Decrypt'
    Enabled = False
    TabOrder = 3
    OnClick = btnDecryptClick
  end
  object cboAlgo: TComboBox
    Left = 112
    Top = 29
    Width = 281
    Height = 23
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 4
    Text = 'RsaPkcs1'
    OnChange = cboAlgoChange
    Items.Strings = (
      'RsaPkcs1'
      'RsaOaepSha1'
      'RsaOaepSha256'
      'RsaOaepSha384'
      'RsaOaepSha512'
      'EcdsaP256Sha256')
    ExplicitWidth = 279
  end
  object EditResult: TEdit
    Left = 24
    Top = 368
    Width = 557
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 5
    TextHint = 'resulting clear text'
    ExplicitWidth = 555
  end
  object btnCreateKeys: TButton
    Left = 24
    Top = 72
    Width = 113
    Height = 25
    Caption = 'Create Key Pair'
    TabOrder = 6
    OnClick = btnCreateKeysClick
  end
  object EditPublicKey: TEdit
    Left = 120
    Top = 112
    Width = 455
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    OnChange = EditKeyChange
    ExplicitWidth = 453
  end
  object EditPrivateKey: TEdit
    Left = 120
    Top = 139
    Width = 455
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    OnChange = EditKeyChange
    ExplicitWidth = 453
  end
  object btnSaveKeys: TButton
    Left = 345
    Top = 72
    Width = 68
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save Keys'
    Enabled = False
    TabOrder = 9
    OnClick = btnSaveKeysClick
    ExplicitLeft = 343
  end
  object btnSaveEncrypt: TButton
    Left = 419
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    Enabled = False
    TabOrder = 10
    OnClick = btnSaveEncryptClick
    ExplicitLeft = 417
  end
  object btnLoadEncrypt: TButton
    Left = 500
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    Enabled = False
    TabOrder = 11
    OnClick = btnLoadEncryptClick
    ExplicitLeft = 498
  end
  object btnLoadKeys: TButton
    Left = 419
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load Keys'
    TabOrder = 12
    OnClick = btnLoadKeysClick
    ExplicitLeft = 417
  end
  object btnLoadPubKey: TButton
    Left = 500
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load Public'
    TabOrder = 13
    OnClick = btnLoadPubKeyClick
    ExplicitLeft = 498
  end
  object cboKeySize: TComboBox
    Left = 506
    Top = 29
    Width = 69
    Height = 23
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemIndex = 0
    TabOrder = 14
    Text = '2048'
    Items.Strings = (
      '2048'
      '3072'
      '4096')
    ExplicitLeft = 504
  end
  object btnSign: TButton
    Left = 105
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Sign'
    Enabled = False
    TabOrder = 15
    OnClick = btnSignClick
  end
  object btnVerify: TButton
    Left = 105
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Verify'
    Enabled = False
    TabOrder = 16
    OnClick = btnVerifyClick
  end
end
