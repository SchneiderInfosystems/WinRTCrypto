{******************************************************************************}
{                                                                              }
{  WinRT Crypto Sample Applications                                            }
{  Copyright (c) 2023 Christoph Schneider                                      }
{  Schneider Infosystems AG, Switzerland                                       }
{  https://github.com/SchneiderInfosystems/WinRTCrypto                         }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit MultiRecipientHybridEncryptionFrm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.SysUtils, System.StrUtils, System.Classes, System.Win.WinRT,
  System.Win.ComObj,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Winapi.CommonTypes, Winapi.WinRT,
  Winapi.Security.Cryptography;

type
  TFrmMultiRecipientHybridEncryption = class(TForm)
    ShapeResult: TShape;
    LabelAlgo: TLabel;
    LabelKey: TLabel;
    LabelResult: TLabel;
    LabelSymKeySize: TLabel;
    cboSymAlgo: TComboBox;
    EditSessionKey: TEdit;
    btnEncrypt: TButton;
    edtEncryptedPayload: TEdit;
    btnDecrypt: TButton;
    cboSymKeySize: TComboBox;
    MemoClear: TMemo;
    MemoResult: TMemo;
    Label1: TLabel;
    cboAsymAlgo: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EditDecryptedSessionKey: TEdit;
    LabelDecryptedSessionKeySize: TLabel;
    btnSaveEncryptedMsg: TButton;
    SaveDialog: TSaveDialog;
    btnLoadEncryptedMessage: TButton;
    OpenDialog: TOpenDialog;
    OpenDialogPrivateKey: TOpenDialog;
    OpenDialogPublicKey: TOpenDialog;
    Label2: TLabel;
    Label6: TLabel;
    PageControl: TPageControl;
    tabKeys: TTabSheet;
    tabEncryption: TTabSheet;
    TabDecryption: TTabSheet;
    btnCreatePersonalKeys: TButton;
    btnSavePersonalKeys: TButton;
    btnLoadKeys: TButton;
    btnLoadPubKey: TButton;
    LabelPublicKey: TLabel;
    edtPublicKey: TEdit;
    LabelPrivateKey: TLabel;
    edtPrivateKey: TEdit;
    edtKeyName: TEdit;
    Label7: TLabel;
    lstRecipients: TListBox;
    Label8: TLabel;
    btnAddRecipient: TButton;
    btnClearRecipients: TButton;
    lstHeader: TListBox;
    Label9: TLabel;
    edtPersonalPrivateKey: TEdit;
    Label10: TLabel;
    edtPersonalName: TEdit;
    btnLoadPersonalKey: TButton;
    edtPersonalPublicKey: TEdit;
    Label11: TLabel;
    lblHeaderSize: TLabel;
    lblPayloadSize: TLabel;
    lblKeysSize: TLabel;
    cboAsymKeySize: TComboBox;
    lblHintPublicKeyMissingInHeader: TLabel;
    btnAddCreatedKeyAsRecipient: TButton;
    gpbEncryptedMessage: TGroupBox;
    btnLoadCreatedKeys: TButton;
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnCreatePersonalKeysClick(Sender: TObject);
    procedure cboAlgosChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSavePersonalKeysClick(Sender: TObject);
    procedure btnLoadKeysClick(Sender: TObject);
    procedure btnLoadPubKeyClick(Sender: TObject);
    procedure btnSaveEncryptedMsgClick(Sender: TObject);
    procedure btnLoadEncryptedMessageClick(Sender: TObject);
    procedure btnAddRecipientClick(Sender: TObject);
    procedure btnClearRecipientsClick(Sender: TObject);
    procedure btnLoadPersonalKeyClick(Sender: TObject);
    procedure CheckKeySizes(Sender: TObject);
    procedure btnAddCreatedKeyAsRecipientClick(Sender: TObject);
    procedure btnLoadCreatedKeysClick(Sender: TObject);
  private
    function SelectedAsymAlgo: Core_IAsymmetricKeyAlgorithmProvider;
    function SelectAsymKeySizeInBits: integer;
    function SelectedSymAlgo: Core_ISymmetricKeyAlgorithmProvider;
    function SelectSymKeySizeInBytes: integer;
    function DataFolder: string;
    procedure CheckActions;
    procedure ShowError(const Msg: string);
    procedure ShowResult(ClearText: IBuffer);
    procedure ClearResult;
    procedure CalcMessageSize;
    function SearchEncryptedSessionKeyByPublicKeyInHeaders(const PublicKey: string): string;
    function ImportPublicKey(const PublicKeyAsBase64: string): Core_ICryptographicKey;
    function ImportPrivateKey(const PrivateKeyAsBase64: string): Core_ICryptographicKey;
    function AsymmetricEncrypt(PublicKey: Core_ICryptographicKey; SessionKey: IBuffer): IBuffer;
    function AsymmetricDecrypt(PrivatKey: Core_ICryptographicKey; Encrypted: IBuffer): IBuffer;
    function SymmetricEncrypt(AlgoProvider: Core_ISymmetricKeyAlgorithmProvider; SessionKey, ClearText: IBuffer): IBuffer;
    function SymmetricDecrypt(AlgoProvider: Core_ISymmetricKeyAlgorithmProvider; SessionKey, Encrypted: IBuffer): IBuffer;
    function IBufferToStr(Buf: IBuffer): string;
    function StrToIBuffer(Text: string): IBuffer;
    function DecodeFromBase64(Base64: string): IBuffer;
    function EncodeAsBase64(Buf: IBuffer): string;
  end;

var
  FrmMultiRecipientHybridEncryption: TFrmMultiRecipientHybridEncryption;

implementation

{$R *.dfm}

resourcestring
  rsTS =
    'This is a sample text for hybrid encryption based on symmetric algo %s and asymmetric algo %s.'#13#10 +
    'The creation date and time of this test text is %s %s.'#13#10 +
    'It contains also unicode letters Ж•₱₲₳‡₾ to test UTF8 encoding.';
  rsDefaultKeyName = 'Myself';
  rsKeySize = 'Key size %d [bits]';
  rsPublicAndPrivateKeySize = 'Public key size %d / Private key size %d [bytes]';
  rsHeaderSize = '%d Recipient(s) in header, size %d [~bytes]';
  rsPayloadSize = 'Payload size %d [bytes]';

{ TFrmHybridEncryption }

{$REGION 'GUI Handling'}
procedure TFrmMultiRecipientHybridEncryption.FormCreate(Sender: TObject);
begin
  lstHeader.Clear;
  edtEncryptedPayload.Text := '';
  ClearResult;
  cboAlgosChange(nil);
  CheckActions;
end;

procedure TFrmMultiRecipientHybridEncryption.cboAlgosChange(Sender: TObject);
begin
  MemoClear.Text :=
    Format(rsTS, [cboSymAlgo.Text, cboAsymAlgo.Text, DateToStr(now), TimeToStr(Now)]);
end;

procedure TFrmMultiRecipientHybridEncryption.CheckKeySizes(Sender: TObject);
begin
  // Asymetric key size starts with 1024 while symmetric key size starts with 512
  if cboAsymKeySize.ItemIndex < cboSymKeySize.ItemIndex then
    ShowError('Asymmetrical key size must be greater than symmetrical key size!')
  else
    ClearResult;
end;

procedure TFrmMultiRecipientHybridEncryption.CheckActions;
var
  IsClearTextDefined: boolean;
  IsPublicKeyDefined: boolean;
  IsPrivateKeyDefined: boolean;
  IsNameDefined: boolean;
  IsPersonalPublicKeyDefined: boolean;
  IsPersonalPrivateKeyDefined: boolean;
  IsPublicKeyInHeader: boolean;
  IsPayloaded: boolean;
  AreRecipientsDefined: boolean;
  AreHeaderItemsPresent: boolean;
begin
  IsClearTextDefined := length(MemoClear.Text) > 0;
  IsPublicKeyDefined := length(edtPublicKey.Text) > 0;
  IsPrivateKeyDefined := length(edtPrivateKey.Text) > 0;
  IsPersonalPublicKeyDefined := length(edtPersonalPublicKey.Text) > 0;
  IsPersonalPrivateKeyDefined := length(edtPersonalPrivateKey.Text) > 0;
  IsPublicKeyInHeader := not SearchEncryptedSessionKeyByPublicKeyInHeaders(
    edtPersonalPublicKey.Text).IsEmpty;
  IsNameDefined := length(edtKeyName.Text) > 0;
  IsPayloaded := length(edtEncryptedPayload.Text) > 0;
  AreRecipientsDefined := lstRecipients.Count > 0;
  AreHeaderItemsPresent := lstHeader.Count > 0;
  // Enable/disable button states
  btnAddCreatedKeyAsRecipient.Enabled := IsPublicKeyDefined and IsNameDefined;
  btnAddCreatedKeyAsRecipient.Enabled := IsPublicKeyDefined and IsNameDefined;
  btnLoadCreatedKeys.Enabled :=
    IsPublicKeyDefined and IsNameDefined and IsPrivateKeyDefined;
  btnSavePersonalKeys.Enabled := IsPublicKeyDefined;
  btnEncrypt.Enabled := AreRecipientsDefined and IsClearTextDefined;
  btnDecrypt.Enabled :=
    IsPersonalPrivateKeyDefined and IsPublicKeyInHeader and IsPayloaded;
  lblHintPublicKeyMissingInHeader.Visible := IsPersonalPublicKeyDefined and
    AreHeaderItemsPresent and not IsPublicKeyInHeader;
end;
{$ENDREGION}

{$REGION 'Result Panel'}
procedure TFrmMultiRecipientHybridEncryption.ShowError(const Msg: string);
begin
  ShapeResult.Brush.Color := clRed;
  LabelResult.Caption := Msg;
end;

procedure TFrmMultiRecipientHybridEncryption.ShowResult(ClearText: IBuffer);
begin
  MemoResult.Text := IBufferToStr(ClearText);
  ShapeResult.Brush.Color := clLime;
  LabelResult.Caption := 'Decryption passed';
end;

procedure TFrmMultiRecipientHybridEncryption.ClearResult;
begin
  ShapeResult.Brush.Color := clBtnFace;
  LabelResult.Caption := '';
end;
{$ENDREGION}

{$REGION 'Key Management'}
procedure TFrmMultiRecipientHybridEncryption.btnCreatePersonalKeysClick(Sender: TObject);
var
  PersonalKey: Core_ICryptographicKey;
  PublicKey, PrivateKey: IBuffer;
begin
  Screen.Cursor := crHourGlass;
  try
    PersonalKey := SelectedAsymAlgo.CreateKeyPair(SelectAsymKeySizeInBits);
  finally
    Screen.Cursor := crDefault;
  end;
  if assigned(PersonalKey) then
  begin
    PublicKey := PersonalKey.ExportPublicKey;
    PrivateKey := PersonalKey.Export(
      Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
    edtPublicKey.Text := EncodeAsBase64(PublicKey);
    edtPrivateKey.Text := EncodeAsBase64(PrivateKey);
    lblKeysSize.Caption := Format(rsPublicAndPrivateKeySize,
      [PublicKey.Length, PrivateKey.Length]);
    edtKeyName.Text := rsDefaultKeyName;
    // Transfer automatically to decrypt tab
    btnLoadCreatedKeysClick(nil);
  end;
  CheckActions;
end;

procedure TFrmMultiRecipientHybridEncryption.btnSavePersonalKeysClick(Sender: TObject);
var
  sl: TStringList;
begin
  if length(edtKeyName.Text) = 0 then
    edtKeyName.SetFocus
  else begin
    sl := TStringList.Create;
    try
      sl.Text := edtPublicKey.Text;
      sl.SaveToFile(DataFolder + edtKeyName.Text + '.PublicKey');
      sl.Text := edtPrivateKey.Text ;
      sl.SaveToFile(DataFolder + edtKeyName.Text + '.PrivateKey');
    finally
      sl.Free;
    end;
  end;
end;

procedure TFrmMultiRecipientHybridEncryption.btnLoadKeysClick(Sender: TObject);
var
  sl: TStringList;
  PersonalKey: Core_ICryptographicKey;
  PublicKey, PrivateKey: IBuffer;
begin
  OpenDialogPrivateKey.InitialDir := DataFolder;
  if OpenDialogPrivateKey.Execute(Handle) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OpenDialogPrivateKey.FileName);
      edtPrivateKey.Text := trim(sl.Text);
      PrivateKey := DecodeFromBase64(edtPrivateKey.Text);
    finally
      sl.Free;
    end;
    PersonalKey := ImportPrivateKey(edtPrivateKey.Text);
    if assigned(PersonalKey) then
    begin
      PublicKey := PersonalKey.ExportPublicKey;
      lblKeysSize.Caption := Format(rsPublicAndPrivateKeySize,
        [PublicKey.Length, PrivateKey.Length]);
      edtPublicKey.Text := EncodeAsBase64(PublicKey);
      edtKeyName.Text :=
        ChangeFileExt(ExtractFileName(OpenDialogPrivateKey.FileName), '');
    end;
    CheckActions;
  end;
end;

procedure TFrmMultiRecipientHybridEncryption.btnLoadPubKeyClick(Sender: TObject);
var
  sl: TStringList;
begin
  OpenDialogPublicKey.InitialDir := DataFolder;
  if OpenDialogPublicKey.Execute(Handle) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OpenDialogPublicKey.FileName);
      edtPublicKey.Text := trim(sl.Text);
      edtPrivateKey.Text := '';
      edtKeyName.Text :=
        ChangeFileExt(ExtractFileName(OpenDialogPublicKey.FileName), '');
      lblKeysSize.Caption := '';
    finally
      sl.Free;
    end;
    CheckActions;
  end;
end;
{$ENDREGION}

{$REGION 'Message Encryption'}
procedure TFrmMultiRecipientHybridEncryption.btnAddCreatedKeyAsRecipientClick(
  Sender: TObject);
begin
  lstRecipients.AddItem(edtKeyName.Text + ':' + edtPublicKey.Text, nil);
  CheckActions;
end;

procedure TFrmMultiRecipientHybridEncryption.btnAddRecipientClick(
  Sender: TObject);
var
  sl: TStringList;
  Name: string;
begin
  OpenDialogPublicKey.InitialDir := DataFolder;
  if OpenDialogPublicKey.Execute(Handle) then
  begin
    Name := ChangeFileExt(ExtractFileName(OpenDialogPublicKey.FileName), '');
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OpenDialogPublicKey.FileName);
      lstRecipients.AddItem(Name + ':' + trim(sl.Text), nil);
    finally
      sl.Free;
    end;
  end;
end;

procedure TFrmMultiRecipientHybridEncryption.btnClearRecipientsClick(
  Sender: TObject);
begin
  lstRecipients.Clear;
  CheckActions;
end;

procedure TFrmMultiRecipientHybridEncryption.btnEncryptClick(Sender: TObject);
var
  SessionKey: IBuffer;
  Recipient: string;
  RecipientNameAndKey: TStringDynArray;
  PublicKeyAsStr: string;
  PublicKey: Core_ICryptographicKey;
  EncryptedSessionKey: IBuffer;
  Payload: IBuffer;
begin
  lstHeader.Clear;
  SessionKey := TCryptographicBuffer.GenerateRandom(SelectSymKeySizeInBytes);
  EditSessionKey.Text := EncodeAsBase64(SessionKey);
  LabelSymKeySize.Caption :=
    Format(rsKeySize, [SelectedSymAlgo.CreateSymmetricKey(SessionKey).KeySize]);
  for Recipient in lstRecipients.Items do
  begin
    RecipientNameAndKey := SplitString(Recipient, ':');
    if length(RecipientNameAndKey) <> 2 then
      raise Exception.Create('Invalid recipient format');
    PublicKeyAsStr := RecipientNameAndKey[1];
    PublicKey := ImportPublicKey(PublicKeyAsStr);
    if assigned(PublicKey) then
    begin
      EncryptedSessionKey := AsymmetricEncrypt(PublicKey, SessionKey);
      if not assigned(EncryptedSessionKey) then
        exit;
      lstHeader.AddItem(PublicKeyAsStr + ':' + EncodeAsBase64(EncryptedSessionKey), nil);
    end;
  end;
  Payload := SymmetricEncrypt(SelectedSymAlgo, SessionKey, StrToIBuffer(MemoClear.Text));
  if assigned(Payload) then
  begin
    edtEncryptedPayload.Text := EncodeAsBase64(PayLoad);
    ClearResult;
  end;
  CalcMessageSize;
  CheckActions;
end;
{$ENDREGION}

{$REGION 'Message Decryption'}
procedure TFrmMultiRecipientHybridEncryption.btnLoadCreatedKeysClick(
  Sender: TObject);
begin
  // Load from key management tab
  edtPersonalPublicKey.Text := edtPublicKey.Text;
  edtPersonalPrivateKey.Text := edtPrivateKey.Text;
  edtPersonalName.Text := edtKeyName.Text;
  CheckActions;
end;

procedure TFrmMultiRecipientHybridEncryption.btnLoadPersonalKeyClick(Sender: TObject);
var
  sl: TStringList;
  PersonalKey: Core_ICryptographicKey;
begin
  OpenDialogPrivateKey.InitialDir := DataFolder;
  if OpenDialogPrivateKey.Execute(Handle) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OpenDialogPrivateKey.FileName);
      edtPersonalPrivateKey.Text := trim(sl.Text);
      PersonalKey := ImportPrivateKey(edtPersonalPrivateKey.Text);
      if assigned(PersonalKey) then
      begin
        edtPersonalPublicKey.Text := EncodeAsBase64(PersonalKey.ExportPublicKey);
        edtPersonalName.Text :=
          ChangeFileExt(ExtractFileName(OpenDialogPrivateKey.FileName), '');
      end;
    finally
      sl.Free;
    end;
    CheckActions;
  end;
end;

procedure TFrmMultiRecipientHybridEncryption.btnDecryptClick(Sender: TObject);
var
  EncryptedSessionKey: string;
  PrivateKey: Core_ICryptographicKey;
  SessionKey: IBuffer;
  ClearText: IBuffer;
begin
  ClearResult;
  EncryptedSessionKey :=
    SearchEncryptedSessionKeyByPublicKeyInHeaders(edtPersonalPublicKey.Text);
  if EncryptedSessionKey.IsEmpty then
    ShowError('This message is not encrypted for given public key')
  else begin
    PrivateKey := ImportPrivateKey(edtPersonalPrivateKey.Text);
    if assigned(PrivateKey) then
    begin
      SessionKey := AsymmetricDecrypt(PrivateKey, DecodeFromBase64(EncryptedSessionKey));
      if assigned(SessionKey) then
      begin
        EditDecryptedSessionKey.Text := EncodeAsBase64(SessionKey);
        LabelDecryptedSessionKeySize.Caption :=
          Format(rsKeySize, [SelectedSymAlgo.CreateSymmetricKey(SessionKey).KeySize]);
        ClearText := SymmetricDecrypt(SelectedSymAlgo, SessionKey,
          DecodeFromBase64(edtEncryptedPayload.Text));
        if assigned(ClearText) then
          ShowResult(ClearText);
      end;
    end;
  end;
end;

function TFrmMultiRecipientHybridEncryption.SearchEncryptedSessionKeyByPublicKeyInHeaders(
  const PublicKey: string): string;
var
  c: integer;
  header: string;
  publicKeyAndEncyptedSessionKey: TStringDynArray;
begin
  result := '';
  for c := 0 to pred(lstHeader.Items.Count) do
  begin
    header := lstHeader.Items[c];
    publicKeyAndEncyptedSessionKey := SplitString(header, ':');
    if length(publicKeyAndEncyptedSessionKey) <> 2 then
      raise Exception.Create('Invalid header format');
    if SameText(publicKeyAndEncyptedSessionKey[0], PublicKey) then
    begin
      lstHeader.ItemIndex := c;
      exit(publicKeyAndEncyptedSessionKey[1]);
    end;
  end;
  lstHeader.ItemIndex := -1;
end;
{$ENDREGION}

{$REGION 'Crypt helpers'}
function TFrmMultiRecipientHybridEncryption.ImportPrivateKey(const PrivateKeyAsBase64: string): Core_ICryptographicKey;
begin
  result := nil;
  try
    result := SelectedAsymAlgo.ImportKeyPair(DecodeFromBase64(PrivateKeyAsBase64),
      Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
  except
    on e: exception do
      ShowError('Import private key failed: ' + e.Message);
  end;
end;

function TFrmMultiRecipientHybridEncryption.ImportPublicKey(const PublicKeyAsBase64: string): Core_ICryptographicKey;
begin
  result := nil;
  try
    result := SelectedAsymAlgo.ImportPublicKey(DecodeFromBase64(PublicKeyAsBase64));
  except
    on e: exception do
      ShowError('Import public key failed: ' + e.Message);
  end;
end;

function TFrmMultiRecipientHybridEncryption.EncodeAsBase64(Buf: IBuffer): string;
begin
  result := TWindowsString.HStringToString(
              TCryptographicBuffer.EncodeToBase64String(Buf));
end;

function TFrmMultiRecipientHybridEncryption.DecodeFromBase64(Base64: string): IBuffer;
begin
  result := TCryptographicBuffer.DecodeFromBase64String(
    TWindowsString.Create(Base64));
end;

function TFrmMultiRecipientHybridEncryption.StrToIBuffer(Text: string): IBuffer;
var
  data: TBytes;
begin
  data := TEncoding.UTF8.GetBytes(Text);
  result := TCryptographicBuffer.CreateFromByteArray(length(data), @data[0]);
end;

function TFrmMultiRecipientHybridEncryption.IBufferToStr(Buf: IBuffer): string;
begin
  result := TCryptographicBuffer.ConvertBinaryToString(BinaryStringEncoding.Utf8, Buf).ToString;
end;
{$ENDREGION}

{$REGION 'Asymmetric Encrypt/Decryption'}
function TFrmMultiRecipientHybridEncryption.SelectedAsymAlgo: Core_IAsymmetricKeyAlgorithmProvider;
var
  Algo: HString;
begin
  case cboAsymAlgo.ItemIndex of
    0: Algo := TCore_AsymmetricAlgorithmNames.RsaPkcs1;
    1: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha1;
    2: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha256;
    3: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha384;
    4: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha512;
    else
      raise Exception.Create('Unknown Core_AsymmetricAlgorithmNames');
  end;
  result := TCore_AsymmetricKeyAlgorithmProvider.OpenAlgorithm(Algo);
  cboAsymAlgo.Enabled := false;
end;

function TFrmMultiRecipientHybridEncryption.SelectAsymKeySizeInBits: integer;
begin
  result := StrToInt(cboAsymKeySize.Text);
  cboAsymKeySize.Enabled := false;
end;

function TFrmMultiRecipientHybridEncryption.AsymmetricEncrypt(PublicKey: Core_ICryptographicKey; SessionKey: IBuffer): IBuffer;
begin
  result := nil;
  try
    result := TCore_CryptographicEngine.Encrypt(PublicKey, SessionKey, nil);
  except
    on e: EOleException do
      if e.ErrorCode = E_INVALIDARG then
        ShowError('Asymmetric encryption failed by invalid argument: Session key size must be smaller than recipient''s public key')
      else
        ShowError('Asymmetric encryption failed: ' + e.Message);
  end;
end;

function TFrmMultiRecipientHybridEncryption.AsymmetricDecrypt(PrivatKey: Core_ICryptographicKey; Encrypted: IBuffer): IBuffer;
begin
  result := nil;
  try
    result := TCore_CryptographicEngine.Decrypt(PrivatKey, Encrypted, nil);
  except
    on e: exception do
      ShowError('Asymmetric decryption failed: ' + e.Message);
  end;
end;
{$ENDREGION}

{$REGION 'Symmetric Encrypt/Decryption'}
function TFrmMultiRecipientHybridEncryption.SelectedSymAlgo: Core_ISymmetricKeyAlgorithmProvider;
var
  Algo: HString;
begin
  case cboSymAlgo.ItemIndex of
    0: Algo := TCore_SymmetricAlgorithmNames.AesCbc;
    1: Algo := TCore_SymmetricAlgorithmNames.AesCbcPkcs7;
    2: Algo := TCore_SymmetricAlgorithmNames.AesCcm;
    3: Algo := TCore_SymmetricAlgorithmNames.AesEcb;
    4: Algo := TCore_SymmetricAlgorithmNames.AesEcbPkcs7;
    5: Algo := TCore_SymmetricAlgorithmNames.AesGcm;
    else
      raise Exception.Create('Unknown Core_SymmetricAlgorithmNames');
  end;
  result := TCore_SymmetricKeyAlgorithmProvider.OpenAlgorithm(Algo);
  cboSymAlgo.Enabled := false;
end;

function TFrmMultiRecipientHybridEncryption.SelectSymKeySizeInBytes: integer;
begin
  result := StrToInt(cboSymKeySize.Text) div 8;
  cboSymKeySize.Enabled := false;
end;

function TFrmMultiRecipientHybridEncryption.SymmetricEncrypt(AlgoProvider: Core_ISymmetricKeyAlgorithmProvider;
  SessionKey, ClearText: IBuffer): IBuffer;
var
  key: Core_ICryptographicKey;
begin
  result := nil;
  key := AlgoProvider.CreateSymmetricKey(SessionKey);
  try
    result := TCore_CryptographicEngine.Encrypt(key, ClearText, nil);
  except
    on e: exception do
      ShowError('Symmetric encryption failed: ' + e.Message);
  end;
end;

function TFrmMultiRecipientHybridEncryption.SymmetricDecrypt(AlgoProvider: Core_ISymmetricKeyAlgorithmProvider;
  SessionKey, Encrypted: IBuffer): IBuffer;
var
  key: Core_ICryptographicKey;
begin
  result := nil;
  key := AlgoProvider.CreateSymmetricKey(SessionKey);
  try
    result := TCore_CryptographicEngine.Decrypt(key, Encrypted, nil);
  except
    on e: exception do
      ShowError('Symmetric decryption failed: ' + e.Message);
  end;
end;
{$ENDREGION}

{$REGION 'Encrypted Message Load/Save'}
function TFrmMultiRecipientHybridEncryption.DataFolder: string;
begin
  result := ExpandFileName(ExtractFileDir(Application.ExeName) + '\..\..\Data\');
  if not DirectoryExists(result, false) then
    ForceDirectories(result);
end;

procedure TFrmMultiRecipientHybridEncryption.btnSaveEncryptedMsgClick(Sender: TObject);
var
  sl: TStringList;
begin
  SaveDialog.InitialDir := DataFolder;
  SaveDialog.FileName := 'Message';
  if SaveDialog.Execute(Handle) then
  begin
    sl := TStringList.Create;
    try
      sl.AddStrings(lstHeader.Items);
      sl.Add(edtEncryptedPayload.Text);
      sl.SaveToFile(SaveDialog.FileName);
    finally
      sl.Free;
    end;
  end;
end;

procedure TFrmMultiRecipientHybridEncryption.btnLoadEncryptedMessageClick(Sender: TObject);
var
  sl: TStringList;
begin
  OpenDialog.InitialDir := DataFolder;
  if OpenDialog.Execute(Handle) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OpenDialog.FileName);
      if sl.Count >= 2 then
      begin
        edtEncryptedPayload.Text := sl[sl.Count - 1];
        sl.Delete(sl.Count - 1);
        lstHeader.Clear;
        lstHeader.Items.AddStrings(sl);
        CheckActions;
        ClearResult;
        CalcMessageSize;
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TFrmMultiRecipientHybridEncryption.CalcMessageSize;
begin
  lblHeaderSize.Caption :=
    Format(rsHeaderSize,
      [lstHeader.Items.Count, length(lstHeader.Items.Text) div 3]);
  lblPayloadSize.Caption :=
    Format(rsPayloadSize, [DecodeFromBase64(edtEncryptedPayload.Text).Length]);
end;
{$ENDREGION}

end.
