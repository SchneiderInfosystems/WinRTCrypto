object FrmMultiRecipientHybridEncryptionSign: TFrmMultiRecipientHybridEncryptionSign
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Hybrid Encryption for Multiple Recipients with Sender Signature '
  ClientHeight = 1229
  ClientWidth = 1778
  Color = clBtnFace
  Constraints.MinHeight = 741
  Constraints.MinWidth = 1032
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 168
  DesignSize = (
    1778
    1229)
  TextHeight = 30
  object ShapeResult: TShape
    Left = 28
    Top = 1125
    Width = 1687
    Height = 67
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akRight, akBottom]
    Pen.Width = 2
    Shape = stRoundRect
  end
  object LabelAlgo: TLabel
    Left = 28
    Top = 42
    Width = 210
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Symmetrical Algorithm'
  end
  object LabelResult: TLabel
    Left = 68
    Top = 1141
    Width = 1556
    Height = 35
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 35
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 826
    Top = 42
    Width = 223
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Asymmetrical Algorithm'
  end
  object cboSymAlgo: TComboBox
    Left = 266
    Top = 37
    Width = 366
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 4
    Text = 'AesCbcPkcs7'
    OnChange = cboAlgosChange
    Items.Strings = (
      'AesCbc'
      'AesCbcPkcs7'
      'AesEcb'
      'AesEcbPkcs7')
  end
  object cboSymKeySize: TComboBox
    Left = 653
    Top = 37
    Width = 121
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 1
    Text = '256'
    OnChange = CheckKeySizes
    Items.Strings = (
      '128'
      '192'
      '256')
  end
  object cboAsymAlgo: TComboBox
    Left = 1064
    Top = 37
    Width = 478
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 2
    Text = 'RsaOaepSha256'
    OnChange = cboAlgosChange
    Items.Strings = (
      'RsaPkcs1'
      'RsaOaepSha1'
      'RsaOaepSha256'
      'RsaOaepSha384'
      'RsaOaepSha512')
  end
  object PageControl: TPageControl
    Left = 32
    Top = 114
    Width = 1683
    Height = 609
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tabKeys
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object tabKeys: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Key Management'
      DesignSize = (
        1675
        564)
      object LabelPublicKey: TLabel
        Left = 33
        Top = 112
        Width = 95
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Public Key'
      end
      object LabelPrivateKey: TLabel
        Left = 33
        Top = 163
        Width = 102
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Private Key'
      end
      object Label7: TLabel
        Left = 33
        Top = 210
        Width = 91
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Key name'
      end
      object lblKeysSize: TLabel
        Left = 1111
        Top = 210
        Width = 516
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'Public key size / Private key size [bytes]'
        ExplicitLeft = 1106
      end
      object btnCreatePersonalKeys: TButton
        Left = 33
        Top = 32
        Width = 214
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Create Personal Key'
        TabOrder = 0
        OnClick = btnCreatePersonalKeysClick
      end
      object btnSavePersonalKeys: TButton
        Left = 33
        Top = 275
        Width = 221
        Height = 44
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Save Personal Keys'
        Enabled = False
        TabOrder = 1
        OnClick = btnSavePersonalKeysClick
      end
      object btnLoadKeys: TButton
        Left = 257
        Top = 32
        Width = 247
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Load Personal Keys'
        TabOrder = 2
        OnClick = btnLoadKeysClick
      end
      object btnLoadPubKey: TButton
        Left = 515
        Top = 32
        Width = 274
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Load Personal Public Key'
        TabOrder = 3
        OnClick = btnLoadPubKeyClick
      end
      object edtPublicKey: TEdit
        Left = 233
        Top = 107
        Width = 1408
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 4
      end
      object edtPrivateKey: TEdit
        Left = 233
        Top = 154
        Width = 1408
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 5
      end
      object edtKeyName: TEdit
        Left = 233
        Top = 205
        Width = 502
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 6
      end
    end
    object tabEncryption: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Message Encryption'
      ImageIndex = 1
      DesignSize = (
        1675
        564)
      object Label2: TLabel
        Left = 28
        Top = 21
        Width = 368
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Clear text as encryption input argument'
      end
      object Label8: TLabel
        Left = 28
        Top = 191
        Width = 156
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akBottom]
        Caption = 'List of Recipients'
        ExplicitTop = 206
      end
      object LabelKey: TLabel
        Left = 32
        Top = 490
        Width = 182
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akBottom]
        Caption = 'Created session key'
        ExplicitTop = 505
      end
      object LabelSymKeySize: TLabel
        Left = 1493
        Top = 488
        Width = 143
        Height = 27
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Alignment = taRightJustify
        Anchors = [akLeft, akRight, akBottom]
        AutoSize = False
        Caption = 'Key size [bits]'
        ExplicitTop = 481
        ExplicitWidth = 138
      end
      object Label13: TLabel
        Left = 257
        Top = 425
        Width = 186
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akBottom]
        Caption = 'Sender'#39's Private Key'
        ExplicitTop = 440
      end
      object btnEncrypt: TButton
        Left = 28
        Top = 417
        Width = 184
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akBottom]
        Caption = 'Encrypt and Sign'
        Enabled = False
        TabOrder = 0
        OnClick = btnEncryptClick
        ExplicitTop = 410
      end
      object MemoClear: TMemo
        Left = 28
        Top = 58
        Width = 1613
        Height = 112
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'MemoClear')
        TabOrder = 1
      end
      object lstRecipients: TListBox
        Left = 30
        Top = 228
        Width = 1613
        Height = 96
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akRight, akBottom]
        ItemHeight = 26
        TabOrder = 2
      end
      object btnAddRecipient: TButton
        Left = 404
        Top = 334
        Width = 352
        Height = 44
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akBottom]
        Caption = 'Add Recipient from Public Key File'
        TabOrder = 3
        OnClick = btnAddRecipientClick
        ExplicitTop = 327
      end
      object EditSessionKey: TEdit
        Left = 266
        Top = 481
        Width = 1176
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 4
        ExplicitTop = 474
        ExplicitWidth = 1171
      end
      object btnClearRecipients: TButton
        Left = 779
        Top = 334
        Width = 171
        Height = 44
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akBottom]
        Caption = 'Clear Recipients'
        TabOrder = 5
        OnClick = btnClearRecipientsClick
        ExplicitTop = 327
      end
      object btnAddCreatedKeyAsRecipient: TButton
        Left = 28
        Top = 334
        Width = 352
        Height = 44
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akBottom]
        Caption = 'Add created Public Key as Recipient'
        TabOrder = 6
        OnClick = btnAddCreatedKeyAsRecipientClick
        ExplicitTop = 327
      end
      object edtSenderPrivateKey: TEdit
        Left = 462
        Top = 418
        Width = 812
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 7
        ExplicitTop = 411
        ExplicitWidth = 807
      end
      object btnAddCreatedKeyAsSender: TButton
        Left = 1284
        Top = 417
        Width = 352
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akRight, akBottom]
        Caption = 'Add created Private Key as Sender'
        TabOrder = 8
        OnClick = btnAddCreatedKeyAsSenderClick
        ExplicitLeft = 1279
        ExplicitTop = 410
      end
    end
    object TabDecryption: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Message Decryption'
      ImageIndex = 2
      DesignSize = (
        1675
        564)
      object Label5: TLabel
        Left = 30
        Top = 333
        Width = 202
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Encrypted session key'
      end
      object Label6: TLabel
        Left = 30
        Top = 387
        Width = 343
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Clear text as decryption output result'
      end
      object LabelDecryptedSessionKeySize: TLabel
        Left = 1451
        Top = 333
        Width = 197
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'Key size [bits]'
        ExplicitLeft = 1446
      end
      object Label9: TLabel
        Left = 28
        Top = 152
        Width = 186
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Personal Private Key'
      end
      object Label10: TLabel
        Left = 28
        Top = 200
        Width = 56
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Name'
      end
      object Label11: TLabel
        Left = 28
        Top = 105
        Width = 179
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Personal Public Key'
      end
      object lblHintPublicKeyMissingInHeader: TLabel
        Left = 800
        Top = 200
        Width = 689
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 
          'The personal public key is missing in the header of the encrypte' +
          'd message'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label14: TLabel
        Left = 273
        Top = 270
        Width = 179
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Sender'#39's Public Key'
      end
      object btnDecrypt: TButton
        Left = 30
        Top = 263
        Width = 227
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Decrypt and Verify'
        Enabled = False
        TabOrder = 0
        OnClick = btnDecryptClick
      end
      object EditDecryptedSessionKey: TEdit
        Left = 278
        Top = 327
        Width = 1162
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 1
        ExplicitWidth = 1157
      end
      object MemoResult: TMemo
        Left = 30
        Top = 424
        Width = 1606
        Height = 112
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        TabOrder = 2
        ExplicitWidth = 1601
        ExplicitHeight = 105
      end
      object edtPersonalPrivateKey: TEdit
        Left = 228
        Top = 144
        Width = 1408
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        ExplicitWidth = 1403
      end
      object edtPersonalName: TEdit
        Left = 228
        Top = 194
        Width = 502
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 4
      end
      object btnLoadPersonalKey: TButton
        Left = 282
        Top = 32
        Width = 434
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Load Personal Keys from Private Key File'
        TabOrder = 5
        OnClick = btnLoadPersonalKeyClick
      end
      object edtPersonalPublicKey: TEdit
        Left = 228
        Top = 93
        Width = 1408
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        ExplicitWidth = 1403
      end
      object btnLoadCreatedKeys: TButton
        Left = 28
        Top = 32
        Width = 229
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Load created Keys'
        TabOrder = 7
        OnClick = btnLoadCreatedKeysClick
      end
      object edtSenderPublicKey: TEdit
        Left = 462
        Top = 264
        Width = 651
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 8
        ExplicitWidth = 646
      end
      object btnLoadSenderPublicKey: TButton
        Left = 1335
        Top = 263
        Width = 301
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akTop, akRight]
        Caption = 'Load Public Key from Sender'
        TabOrder = 9
        OnClick = btnLoadSenderPublicKeyClick
        ExplicitLeft = 1330
      end
      object edtSenderName: TEdit
        Left = 1123
        Top = 264
        Width = 202
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 10
        ExplicitLeft = 1118
      end
    end
  end
  object cboAsymKeySize: TComboBox
    Left = 1566
    Top = 37
    Width = 121
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 0
    Text = '3072'
    OnChange = CheckKeySizes
    Items.Strings = (
      '2048'
      '3072'
      '4096')
  end
  object gpbEncryptedMessage: TGroupBox
    Left = 32
    Top = 733
    Width = 1683
    Height = 361
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Encrypted Message'
    TabOrder = 5
    DesignSize = (
      1683
      361)
    object Label3: TLabel
      Left = 25
      Top = 189
      Width = 72
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Payload'
    end
    object Label4: TLabel
      Left = 25
      Top = 46
      Width = 509
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Public key with encrypted session keys for all recipients'
    end
    object lblHeaderSize: TLabel
      Left = 1068
      Top = 46
      Width = 580
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'Header Size [bytes]'
      ExplicitLeft = 1069
    end
    object lblPayloadSize: TLabel
      Left = 1068
      Top = 189
      Width = 587
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'Payload size [bytes]'
      ExplicitLeft = 1069
    end
    object LabelIV: TLabel
      Left = 23
      Top = 298
      Width = 19
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'IV'
    end
    object Label12: TLabel
      Left = 602
      Top = 294
      Width = 88
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Signature'
    end
    object btnLoadEncryptedMessage: TButton
      Left = 1385
      Top = 294
      Width = 263
      Height = 40
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'Load Encrypted Message'
      TabOrder = 0
      OnClick = btnLoadEncryptedMessageClick
    end
    object btnSaveEncryptedMsg: TButton
      Left = 1103
      Top = 294
      Width = 272
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'Save Encrypted Message'
      TabOrder = 1
      OnClick = btnSaveEncryptedMsgClick
    end
    object edtEncryptedPayload: TEdit
      Left = 25
      Top = 226
      Width = 1630
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
      Text = 'edtEncryptedPayload'
    end
    object lstHeader: TListBox
      Left = 25
      Top = 82
      Width = 1630
      Height = 97
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Pitch = fpVariable
      Font.Style = []
      ItemHeight = 30
      ParentFont = False
      TabOrder = 3
    end
    object edtIV: TEdit
      Left = 51
      Top = 291
      Width = 520
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 4
    end
    object edtSignature: TEdit
      Left = 700
      Top = 291
      Width = 380
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 5
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'crypt2'
    Filter = 'Encrypted and signed message (*.crypt2)|*.crypt2'
    Left = 472
    Top = 592
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'crypt2'
    Filter = 'Encrypted and signed message (*.crypt2)|*.crypt2'
    Left = 544
    Top = 592
  end
  object OpenDialogPrivateKey: TOpenDialog
    DefaultExt = 'PrivateKey'
    Filter = 'Private Key (*.PrivateKey)|*.PrivateKey'
    Left = 760
    Top = 592
  end
  object OpenDialogPublicKey: TOpenDialog
    DefaultExt = 'PublicKey'
    Filter = 'Public Key (*.PublicKey)|*.PublicKey'
    Left = 904
    Top = 592
  end
end
