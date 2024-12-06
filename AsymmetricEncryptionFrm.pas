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

unit AsymmetricEncryptionFrm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Win.WinRT,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Winapi.CommonTypes, Winapi.WinRT,
  Winapi.Security.Cryptography;

type
  TfrmAsymmetricEncryption = class(TForm)
    EditClear: TEdit;
    btnEncrypt: TButton;
    EditEncrypted: TEdit;
    btnDecrypt: TButton;
    cboAlgo: TComboBox;
    LabelAlgo: TLabel;
    EditResult: TEdit;
    ShapeResult: TShape;
    btnCreateKeys: TButton;
    LabelResult: TLabel;
    EditPublicKey: TEdit;
    LabelPublicKey: TLabel;
    EditPrivateKey: TEdit;
    LabelPrivateKey: TLabel;
    btnSaveKeys: TButton;
    btnSaveEncrypt: TButton;
    btnLoadEncrypt: TButton;
    btnLoadKeys: TButton;
    btnLoadPubKey: TButton;
    cboKeySize: TComboBox;
    LabelKeySize: TLabel;
    lblChiffreLen: TLabel;
    Label1: TLabel;
    lblResult: TLabel;
    lblResultingClearText: TLabel;
    btnSign: TButton;
    btnVerify: TButton;
    btnSaveSign: TButton;
    btnLoadSign: TButton;
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnCreateKeysClick(Sender: TObject);
    procedure btnSaveKeysClick(Sender: TObject);
    procedure btnSaveEncryptClick(Sender: TObject);
    procedure btnLoadEncryptClick(Sender: TObject);
    procedure btnLoadKeysClick(Sender: TObject);
    procedure btnLoadPubKeyClick(Sender: TObject);
    procedure cboAlgoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditKeyChange(Sender: TObject);
    procedure EditEncryptedChange(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure btnSaveSignClick(Sender: TObject);
    procedure btnLoadSignClick(Sender: TObject);
  private
    fDataFolder: string;
    function SelectedAlgo: Core_IAsymmetricKeyAlgorithmProvider;
    function SelectedAsymSignAlgo: Core_IAsymmetricKeyAlgorithmProvider;
    function SelectedKeyLength: integer;
    procedure InitWithNewKeys;
    procedure CheckButtons;
    procedure ShowError(const Msg: string);
    procedure ShowWarning(const Msg: string);
    procedure ShowResult(const Msg: string);
    procedure ClearResult;
  end;

var
  frmAsymmetricEncryption: TfrmAsymmetricEncryption;

implementation

uses
  Winapi.Security.Helpers;

{$R *.dfm}

resourcestring
  rsTS = 'Test sample for asymmetrical encryption %s at %s with Unicode Ж•₱₲₳‡₾';

{$REGION 'GUI Handling'}
procedure TfrmAsymmetricEncryption.FormCreate(Sender: TObject);
begin
  cboAlgoChange(Sender);
end;

procedure TfrmAsymmetricEncryption.cboAlgoChange(Sender: TObject);
begin
  fDataFolder :=
    IncludeTrailingPathDelimiter(
      ExpandFileName(
        ExtractFileDir(Application.ExeName) + '\..\..\Data\' + cboAlgo.Text));
  if not DirectoryExists(fDataFolder, false) then
    ForceDirectories(fDataFolder);
  EditPublicKey.Text := '';
  EditPrivateKey.Text := '';
  btnEncrypt.Enabled := false;
  btnDecrypt.Enabled := false;
  btnSaveKeys.Enabled := false;
  EditClear.Text := Format(rsTS, [cboAlgo.Text, TimeToStr(Now)]);
  btnLoadKeys.Enabled := FileExists(fDataFolder + 'PrivateAndPublic.key');
  btnLoadPubKey.Enabled := FileExists(fDataFolder + 'Public.key');
  btnLoadEncrypt.Enabled := FileExists(fDataFolder + 'Encrypted.txt');
  btnLoadSign.Enabled := FileExists(fDataFolder + 'Signature.txt');
  if cboAlgo.ItemIndex = 5 then
  begin
    cboKeySize.Items.CommaText := '256';
    cboKeySize.ItemIndex := 0;
  end
  else if cboKeySize.Items.CommaText = '256' then
  begin
    cboKeySize.Items.CommaText := '2048,3072,4096';
    cboKeySize.ItemIndex := 0;
  end;
  cboKeySize.Enabled := cboAlgo.ItemIndex < 5;
end;

procedure TfrmAsymmetricEncryption.InitWithNewKeys;
begin
  btnEncrypt.Enabled := (length(EditPublicKey.Text) > 0) and
    (cboAlgo.ItemIndex < 5);
  btnLoadEncrypt.Enabled := true;
  btnSign.Enabled := length(EditPrivateKey.Text) > 0;
  btnDecrypt.Enabled := false;
  btnVerify.Enabled := false;
  btnSaveKeys.Enabled := true;
  EditEncrypted.Text := '';
  EditResult.Text := '';
  ClearResult;
end;
{$ENDREGION}

{$REGION 'Result Panel'}
procedure TfrmAsymmetricEncryption.ShowError(const Msg: string);
begin
  ShapeResult.Brush.Color := clRed;
  LabelResult.Caption := Msg;
end;

procedure TfrmAsymmetricEncryption.ShowWarning(const Msg: string);
begin
  ShapeResult.Brush.Color := clYellow;
  LabelResult.Caption := Msg;
end;

procedure TfrmAsymmetricEncryption.ShowResult(const Msg: string);
begin
  ShapeResult.Brush.Color := clLime;
  LabelResult.Caption := Msg;
end;

procedure TfrmAsymmetricEncryption.ClearResult;
begin
  ShapeResult.Brush.Color := clBtnFace;
  LabelResult.Caption := '';
end;

procedure TfrmAsymmetricEncryption.EditEncryptedChange(Sender: TObject);
begin
  CheckButtons;
end;

procedure TfrmAsymmetricEncryption.EditKeyChange(Sender: TObject);
begin
  TEdit(Sender).Color := clWindow;
  CheckButtons;
end;

procedure TfrmAsymmetricEncryption.CheckButtons;
begin
  btnDecrypt.Enabled := (length(EditPrivateKey.Text) > 0) and
    (length(EditEncrypted.Text) > 0);
end;

{$ENDREGION}

{$REGION 'Key Management'}
procedure TfrmAsymmetricEncryption.btnCreateKeysClick(Sender: TObject);
var
  Keys: Core_ICryptographicKey;
  PublicKey, PrivateKey: IBuffer;
begin
  Screen.Cursor := crHourGlass;
  try
    Keys := SelectedAlgo.CreateKeyPair(SelectedKeyLength);
    PublicKey := Keys.ExportPublicKey;
    EditPublicKey.Text := TWinRTCryptoHelpers.EncodeAsBase64(PublicKey);
    PrivateKey :=
      Keys.Export(Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
    EditPrivateKey.Text := TWinRTCryptoHelpers.EncodeAsBase64(PrivateKey);
    EditPublicKey.Color := clAqua;
    EditPrivateKey.Color := clAqua;
    InitWithNewKeys;
  finally
    Keys := nil;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmAsymmetricEncryption.btnSaveKeysClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := EditPublicKey.Text;
    sl.SaveToFile(fDataFolder + 'Public.key');
    sl.Text := EditPrivateKey.Text ;
    sl.SaveToFile(fDataFolder + 'PrivateAndPublic.key');
  finally
    sl.Free;
  end;
end;

procedure TfrmAsymmetricEncryption.btnLoadKeysClick(Sender: TObject);
var
  sl: TStringList;
  PersonalKey: Core_ICryptographicKey;
  Keys, PublicKeyAsStr: string;
  PublicKey, PrivateKey: IBuffer;
  NewKeys: boolean;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'PrivateAndPublic.key');
    Keys := sl.Text;
  finally
    sl.Free;
  end;
  PersonalKey := nil;
  try
    PersonalKey := SelectedAlgo.ImportKeyPair(
      TWinRTCryptoHelpers.DecodeFromBase64(Keys),
      Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
  except
    on e: exception do
      ShowError('Import private key failed: ' + e.Message);
  end;
  if assigned(PersonalKey) then
  begin
    PublicKey := PersonalKey.ExportPublicKey;
    PublicKeyAsStr := TWinRTCryptoHelpers.EncodeAsBase64(PublicKey);
    NewKeys := not SameText(EditPublicKey.Text, PublicKeyAsStr);
    if NewKeys then
      EditPublicKey.Text := PublicKeyAsStr;
    PrivateKey := PersonalKey.Export(
      Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
    EditPrivateKey.Text := TWinRTCryptoHelpers.EncodeAsBase64(PrivateKey);
    EditPublicKey.Color := clAqua;
    EditPrivateKey.Color := clAqua;
    if NewKeys then
      InitWithNewKeys
    else begin
      btnDecrypt.Enabled := (length(EditPrivateKey.Text) > 0) and
        (length(EditEncrypted.Text) > 0);
      btnVerify.Enabled := (length(EditPublicKey.Text) > 0) and
        (length(EditEncrypted.Text) > 0);
    end;
    cboKeySize.ItemIndex :=
      cboKeySize.Items.IndexOf(PersonalKey.KeySize.ToString);
  end;
end;

procedure TfrmAsymmetricEncryption.btnLoadPubKeyClick(Sender: TObject);
var
  sl: TStringList;
  PubKey: string;
  PublicKey: IBuffer;
  PersonalKey: Core_ICryptographicKey;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'Public.key');
    PubKey := sl.Text;
  finally
    sl.Free;
  end;
  PersonalKey := nil;
  try
    PersonalKey := SelectedAlgo.ImportPublicKey(
      TWinRTCryptoHelpers.DecodeFromBase64(PubKey));
  except
    on e: exception do
      ShowError('Import public key failed: ' + e.Message);
  end;
  if assigned(PersonalKey) then
  begin
    PublicKey := PersonalKey.ExportPublicKey;
    EditPublicKey.Text := TWinRTCryptoHelpers.EncodeAsBase64(PublicKey);
    EditPrivateKey.Text := '';
    EditPublicKey.Color := clAqua;
    EditPrivateKey.Color := clWindow;
    cboKeySize.ItemIndex :=
      cboKeySize.Items.IndexOf(PersonalKey.KeySize.ToString);
    InitWithNewKeys;
  end;
end;
{$ENDREGION}

{$REGION 'Asymmetric Encrypt/Decryption'}
procedure TfrmAsymmetricEncryption.btnEncryptClick(Sender: TObject);
var
  cleardata, encrypted: IBuffer;
  PersonalKey: Core_ICryptographicKey;
begin
  try
    PersonalKey := SelectedAlgo.ImportPublicKey(
      TWinRTCryptoHelpers.DecodeFromBase64(EditPublicKey.Text));
    EditPublicKey.Color := clAqua;
    EditPrivateKey.Color := clWindow;
  except
    on e: exception do
      ShowError('Import public key failed: ' + e.Message);
  end;
  ClearResult;
  clearData := TWinRTCryptoHelpers.StrToIBuffer(EditClear.Text);
  try
    encrypted := TCore_CryptographicEngine.Encrypt(PersonalKey, cleardata, nil);
    EditEncrypted.Text := TWinRTCryptoHelpers.EncodeAsBase64(encrypted);
    lblChiffreLen.Caption := Format('Size %d bytes', [encrypted.Length]);
    btnSaveEncrypt.Enabled := true;
    btnDecrypt.Enabled := length(EditPrivateKey.Text) > 0;
    btnVerify.Enabled := false;
    lblResult.Caption := 'Chiffre';
    EditResult.Text := '';
    EditResult.visible := true;
    lblResultingClearText.visible := true;
  except
    on e: exception do
      ShowError('Encryption failed: ' + e.Message);
  end;
end;

procedure TfrmAsymmetricEncryption.btnDecryptClick(Sender: TObject);
var
  cleardata, encrypted: IBuffer;
  PersonalKey: Core_ICryptographicKey;
begin
  Assert(lblResult.Caption = 'Chiffre', 'Decryption needs chiffre');
  try
    PersonalKey := SelectedAlgo.ImportKeyPair(
      TWinRTCryptoHelpers.DecodeFromBase64(EditPrivateKey.Text),
      Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
    EditPublicKey.Color := clWindow;
    EditPrivateKey.Color := clAqua;
  except
    on e: exception do
      ShowError('Import private key failed: ' + e.Message);
  end;
  encrypted := TWinRTCryptoHelpers.DecodeFromBase64(EditEncrypted.Text);
  try
    cleardata := TCore_CryptographicEngine.Decrypt(PersonalKey, encrypted, nil);
    EditResult.Text := TWinRTCryptoHelpers.IBufferToStr(cleardata);
    if SameText(EditClear.Text, EditResult.Text) then
      ShowResult(
        'Passed: Resulting clear text is identical to starting clear text')
    else
      ShowWarning(
        'Failed: Resulting clear text different than starting clear text');
    EditResult.visible := true;
    lblResultingClearText.visible := true;
  except
    on e: exception do
      ShowError('Decryption failed: ' + e.Message);
  end;
end;

function TfrmAsymmetricEncryption.SelectedAlgo:
  Core_IAsymmetricKeyAlgorithmProvider;
var
  Algo: HString;
begin
  case cboAlgo.ItemIndex of
    0: Algo := TCore_AsymmetricAlgorithmNames.RsaPkcs1;
    1: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha1;
    2: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha256;
    3: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha384;
    4: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha512;
    5: Algo := TCore_AsymmetricAlgorithmNames.EcdsaP256Sha256;
    else
      raise Exception.Create('Unknown Core_AsymmetricAlgorithmNames');
  end;
  result := TCore_AsymmetricKeyAlgorithmProvider.OpenAlgorithm(Algo);
  cboAlgo.Enabled := false;
end;

function TfrmAsymmetricEncryption.SelectedKeyLength: integer;
begin
  result := StrToInt(cboKeySize.Text);
  cboKeySize.Enabled := false;
end;
{$ENDREGION}

{$REGION 'Asymmetric Signing/Verifing'}
procedure TfrmAsymmetricEncryption.btnSignClick(Sender: TObject);
var
  cleardata, signature: IBuffer;
  PrivateKey: Core_ICryptographicKey;
begin
  try
    PrivateKey := SelectedAsymSignAlgo.ImportKeyPair(
      TWinRTCryptoHelpers.DecodeFromBase64(EditPrivateKey.Text),
      Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
    EditPublicKey.Color := clWindow;
    EditPrivateKey.Color := clAqua;
  except
    on e: exception do
      ShowError('Import private key failed: ' + e.Message);
  end;
  ClearResult;
  clearData := TWinRTCryptoHelpers.StrToIBuffer(EditClear.Text);
  try
    signature := TCore_CryptographicEngine.Sign(PrivateKey, cleardata);
    EditEncrypted.Text := TWinRTCryptoHelpers.EncodeAsBase64(signature);
    lblChiffreLen.Caption := Format('Size %d bytes', [signature.Length]);
    btnSaveSign.Enabled := true;
    btnDecrypt.Enabled := false;
    btnVerify.Enabled := length(EditPublicKey.Text) > 0;
    lblResult.Caption := 'Signature';
    EditResult.visible := false;
    lblResultingClearText.visible := false;
  except
    on e: exception do
      ShowError('Encryption failed: ' + e.Message);
  end;
end;

procedure TfrmAsymmetricEncryption.btnVerifyClick(Sender: TObject);
var
  cleardata, signature: IBuffer;
  PublicKey: Core_ICryptographicKey;
begin
  Assert(lblResult.Caption = 'Signature', 'Verification needs signature');
  try
    PublicKey := SelectedAsymSignAlgo.ImportPublicKey(
      TWinRTCryptoHelpers.DecodeFromBase64(EditPublicKey.Text));
    EditPublicKey.Color := clAqua;
    EditPrivateKey.Color := clWindow;
  except
    on e: exception do
      ShowError('Import public key failed: ' + e.Message);
  end;
  clearData := TWinRTCryptoHelpers.StrToIBuffer(EditClear.Text);
  signature := TWinRTCryptoHelpers.DecodeFromBase64(EditEncrypted.Text);
  try
    if TCore_CryptographicEngine.VerifySignature(PublicKey, clearData,
         signature) then
      ShowResult('Passed: Signature is valid')
    else
      ShowWarning('Failed: Signatur is invalid');
  except
    on e: exception do
      ShowError('Verify signature failed: ' + e.Message);
  end;
  EditResult.visible := false;
  lblResultingClearText.visible := false;
end;

function TfrmAsymmetricEncryption.SelectedAsymSignAlgo:
  Core_IAsymmetricKeyAlgorithmProvider;
var
  Algo: HString;
begin
  case cboAlgo.ItemIndex of
    0,
    1: Algo := TCore_AsymmetricAlgorithmNames.RsaSignPkcs1Sha1;
    2: Algo := TCore_AsymmetricAlgorithmNames.RsaSignPkcs1Sha256;
    3: Algo := TCore_AsymmetricAlgorithmNames.RsaSignPkcs1Sha384;
    4: Algo := TCore_AsymmetricAlgorithmNames.RsaSignPkcs1Sha512;
    5: Algo := TCore_AsymmetricAlgorithmNames.EcdsaSha256;
    6: Algo := TCore_AsymmetricAlgorithmNames.EcdsaP256Sha256;
    else
      raise Exception.Create('Unknown Core_AsymmetricAlgorithmNames');
  end;
  result := TCore_AsymmetricKeyAlgorithmProvider.OpenAlgorithm(Algo);
  cboAlgo.Enabled := false;
end;

{$ENDREGION}

{$REGION 'Encrypted Message Load/Save'}
procedure TfrmAsymmetricEncryption.btnSaveEncryptClick(Sender: TObject);
var
  sl: TStringList;
begin
  Assert(lblResult.Caption = 'Chiffre', 'Decryption needs chiffre');
  sl := TStringList.Create;
  try
    sl.Text := EditEncrypted.Text;
    sl.SaveToFile(fDataFolder + 'Encrypted.txt');
  finally
    sl.Free;
  end;
  btnSaveEncrypt.Enabled := false;
end;

procedure TfrmAsymmetricEncryption.btnLoadEncryptClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'Encrypted.txt');
    EditEncrypted.Text := sl.Text;
    lblResult.Caption := 'Chiffre';
    btnDecrypt.Enabled := length(EditPrivateKey.Text) > 0;
    btnVerify.Enabled := false;
  finally
    sl.Free;
  end;
end;

procedure TfrmAsymmetricEncryption.btnSaveSignClick(Sender: TObject);
var
  sl: TStringList;
begin
  Assert(lblResult.Caption = 'Signature', 'Verification needs signature');
  sl := TStringList.Create;
  try
    sl.Add(EditClear.Text);
    sl.Add(EditEncrypted.Text);
    sl.SaveToFile(fDataFolder + 'Signature.txt', TEncoding.UTF8);
  finally
    sl.Free;
  end;
  btnSaveSign.enabled := false;
end;

procedure TfrmAsymmetricEncryption.btnLoadSignClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'Signature.txt', TEncoding.UTF8);
    Assert(sl.Count = 2, 'Invalid signature file. Expected are 2 lines with clear text and signature.');
    EditClear.Text := sl[0];
    EditEncrypted.Text := sl[1];
    lblResult.Caption := 'Signature';
    btnDecrypt.Enabled := false;
    btnVerify.Enabled := length(EditPublicKey.Text) > 0;
  finally
    sl.Free;
  end;
end;
{$ENDREGION}

end.
