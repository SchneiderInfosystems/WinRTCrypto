﻿{******************************************************************************}
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

unit SymmetricEncryptionFrm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Win.WinRT,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Winapi.CommonTypes, Winapi.WinRT,
  Winapi.Security.Cryptography;

type
  TFrmSymmetricEncryption = class(TForm)
    LabelAlgo: TLabel;
    cboAlgo: TComboBox;
    btnCreateKey: TButton;
    LabelKey: TLabel;
    EditKey: TEdit;
    EditClear: TEdit;
    btnEncrypt: TButton;
    EditEncrypted: TEdit;
    btnDecrypt: TButton;
    EditResult: TEdit;
    LabelResult: TLabel;
    ShapeResult: TShape;
    btnSaveEncrypt: TButton;
    btnLoadEncrypt: TButton;
    btnSaveKey: TButton;
    btnLoadKey: TButton;
    LabelKeySize: TLabel;
    EditKeySize: TEdit;
    cboKeySize: TComboBox;
    EditIV: TEdit;
    LabelIV: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    chbAutoRenew: TCheckBox;
    procedure btnCreateKeyClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnSaveEncryptClick(Sender: TObject);
    procedure cboAlgoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoadEncryptClick(Sender: TObject);
    procedure btnSaveKeyClick(Sender: TObject);
    procedure btnLoadKeyClick(Sender: TObject);
    procedure EditKeyChange(Sender: TObject);
    procedure EditIVChange(Sender: TObject);
    procedure EditEncryptedChange(Sender: TObject);
  private
    fDataFolder: string;
    function SelectedAlgo: Core_ISymmetricKeyAlgorithmProvider;
    function SelectKeySizeInBytes: integer;
    function SelectAlgoRequiresIV: boolean;
    function SelectAlgoRequiresPadding: boolean;
    procedure CheckButtons;
    procedure InitWithNewKeys;
    procedure ShowError(const Msg: string);
    procedure ShowWarning(const Msg: string);
    procedure ShowResult(const Msg: string);
    procedure ClearResult;
  end;

var
  FrmSymmetricEncryption: TFrmSymmetricEncryption;

implementation

uses
  Winapi.Security.Helpers;

{$R *.dfm}

resourcestring
  rsTS = 'Test sample for symmetrical encryption %s at %s with Unicode Ж•₱₲₳‡₾';

{$REGION 'GUI Handling'}
procedure TFrmSymmetricEncryption.FormCreate(Sender: TObject);
begin
  cboAlgoChange(Sender);
end;

procedure TFrmSymmetricEncryption.cboAlgoChange(Sender: TObject);
begin
  fDataFolder :=
    IncludeTrailingPathDelimiter(
      ExpandFileName(
        ExtractFileDir(Application.ExeName) + '\..\..\Data\' + cboAlgo.Text));
  if not DirectoryExists(fDataFolder, false) then
    ForceDirectories(fDataFolder);
  EditKey.Text := '';
  btnEncrypt.Enabled := false;
  btnDecrypt.Enabled := false;
  btnSaveKey.Enabled := false;
  EditClear.Text := Format(rsTS, [cboAlgo.Text, TimeToStr(Now)]);
  btnLoadKey.Enabled := FileExists(fDataFolder + 'Private.key');
  btnLoadEncrypt.Enabled := FileExists(fDataFolder + 'Encrypted.txt');
  EditIV.visible := SelectAlgoRequiresIV;
  LabelIV.visible := EditIV.visible;
  chbAutoRenew.visible := EditIV.visible;
end;

procedure TFrmSymmetricEncryption.InitWithNewKeys;
begin
  btnEncrypt.Enabled := true;
  btnLoadEncrypt.Enabled := true;
  btnDecrypt.Enabled := false;
  btnSaveKey.Enabled := true;
  EditEncrypted.Text := '';
  EditResult.Text := '';
  ClearResult;
end;
{$ENDREGION}

{$REGION 'Result Panel'}
procedure TFrmSymmetricEncryption.ShowError(const Msg: string);
begin
  ShapeResult.Brush.Color := clRed;
  LabelResult.Caption := Msg;
end;

procedure TFrmSymmetricEncryption.ShowWarning(const Msg: string);
begin
  ShapeResult.Brush.Color := clYellow;
  LabelResult.Caption := Msg;
end;

procedure TFrmSymmetricEncryption.ShowResult(const Msg: string);
begin
  ShapeResult.Brush.Color := clLime;
  LabelResult.Caption := Msg;
end;

procedure TFrmSymmetricEncryption.ClearResult;
begin
  ShapeResult.Brush.Color := clBtnFace;
  LabelResult.Caption := '';
end;

procedure TFrmSymmetricEncryption.EditEncryptedChange(Sender: TObject);
begin
  CheckButtons;
end;

procedure TFrmSymmetricEncryption.EditIVChange(Sender: TObject);
begin
  CheckButtons;
end;

procedure TFrmSymmetricEncryption.EditKeyChange(Sender: TObject);
begin
  EditKey.Color := clWindow;
  CheckButtons;
end;

procedure TFrmSymmetricEncryption.CheckButtons;
begin
  btnDecrypt.Enabled := (length(EditEncrypted.Text) > 0) and
    (length(EditIV.Text) > 0) and (length(EditEncrypted.Text) > 0);
end;

{$ENDREGION}

{$REGION 'Key Management'}
procedure TFrmSymmetricEncryption.btnCreateKeyClick(Sender: TObject);
var
  KeyMat: IBuffer;
  Key: Core_ICryptographicKey;
begin
  KeyMat := TCryptographicBuffer.GenerateRandom(SelectKeySizeInBytes);
  EditKey.Text := TWinRTCryptoHelpers.EncodeAsBase64(KeyMat);
  Key := SelectedAlgo.CreateSymmetricKey(KeyMat);
  EditKey.Color := clAqua;
  EditKeySize.Text := Key.KeySize.ToString;
  InitWithNewKeys;
end;

procedure TFrmSymmetricEncryption.btnSaveKeyClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := EditKey.Text;
    sl.SaveToFile(fDataFolder + 'Private.key');
  finally
    sl.Free;
  end;
end;

procedure TFrmSymmetricEncryption.btnLoadKeyClick(Sender: TObject);
var
  sl: TStringList;
  KeyMat: IBuffer;
  Key: Core_ICryptographicKey;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'Private.key');
    EditKey.Text := sl.Text;
    EditKey.Color := clAqua;
    KeyMat := TWinRTCryptoHelpers.DecodeFromBase64(sl.Text);
    Key := SelectedAlgo.CreateSymmetricKey(KeyMat);
    EditKeySize.Text := Key.KeySize.ToString;
  finally
    sl.Free;
  end;
  InitWithNewKeys;
end;
{$ENDREGION}

{$REGION 'Symmetric Encrypt/Decryption'}
procedure TFrmSymmetricEncryption.btnEncryptClick(Sender: TObject);
var
  Key: Core_ICryptographicKey;
  ClearData, Encrypted, IV: IBuffer;
begin
  ClearData := TWinRTCryptoHelpers.StrToIBuffer(EditClear.Text);
  if SelectAlgoRequiresPadding then
  begin
    // Ensure that the message length is a multiple of the block length.
    // This is not necessary for PKCS #7 algorithms which automatically pad the
    // message to an appropriate length.
    if ClearData.Length mod SelectedAlgo.BlockLength <> 0 then
    begin
      ShowWarning(
        'Message length must be multiple of block length: Message is padded now');
      EditClear.Text := EditClear.Text + StringOfChar('_',
        SelectedAlgo.BlockLength - ClearData.Length mod SelectedAlgo.BlockLength);
      // A better solution would be to use a message length and a random pad
      ClearData := TWinRTCryptoHelpers.StrToIBuffer(EditClear.Text);
    end;
  end;
  try
    Key := SelectedAlgo.CreateSymmetricKey(
      TWinRTCryptoHelpers.DecodeFromBase64(EditKey.Text));
    if SelectAlgoRequiresIV then
    begin
      if chbAutoRenew.Checked or (EditIV.Text = TWinRTCryptoHelpers.EncodeAsBase64(IV)) then
      begin
        IV := TCryptographicBuffer.GenerateRandom(SelectedAlgo.BlockLength);
        EditIV.Text := TWinRTCryptoHelpers.EncodeAsBase64(IV);
      end else
        IV := TWinRTCryptoHelpers.DecodeFromBase64(EditIV.Text);
    end else
      IV := nil;
    Encrypted := TCore_CryptographicEngine.Encrypt(Key, ClearData, IV);
    EditEncrypted.Text := TWinRTCryptoHelpers.EncodeAsBase64(Encrypted);
    EditKey.Color := clAqua;
    btnSaveEncrypt.Enabled := true;
    btnDecrypt.Enabled := true;
  except
    on e: exception do
      ShowError('Encryption failed: ' + e.Message);
  end;
end;

procedure TFrmSymmetricEncryption.btnDecryptClick(Sender: TObject);
var
  Key: Core_ICryptographicKey;
  cleardata, encrypted, IV: IBuffer;
begin
  encrypted := TWinRTCryptoHelpers.DecodeFromBase64(EditEncrypted.Text);
  IV := TWinRTCryptoHelpers.DecodeFromBase64(EditIV.Text);
  try
    Key := SelectedAlgo.CreateSymmetricKey(
      TWinRTCryptoHelpers.DecodeFromBase64(EditKey.Text));
    cleardata := TCore_CryptographicEngine.Decrypt(Key, encrypted, IV);
    EditResult.Text := TWinRTCryptoHelpers.IBufferToStr(cleardata);
    EditKey.Color := clAqua;
    if SameText(EditClear.Text, EditResult.Text) then
      ShowResult(
        'Passed: Resulting clear text is identical to starting clear text')
    else
      ShowWarning(
        'Failed: Resulting clear text different than starting clear text');
  except
    on e: exception do
      ShowError('Decryption failed: ' + e.Message);
  end;
end;

function TFrmSymmetricEncryption.SelectedAlgo: Core_ISymmetricKeyAlgorithmProvider;
// Hint: AesCcm and AesGcm reuqires EncryptAndAuthenticate
var
  Algo: HString;
begin
  case cboAlgo.ItemIndex of
    0: Algo := TCore_SymmetricAlgorithmNames.AesCbc;
    1: Algo := TCore_SymmetricAlgorithmNames.AesCbcPkcs7;
    2: Algo := TCore_SymmetricAlgorithmNames.AesEcb;
    3: Algo := TCore_SymmetricAlgorithmNames.AesEcbPkcs7;
    else
      raise Exception.Create('Unknown Core_SymmetricAlgorithmNames');
  end;
  result := TCore_SymmetricKeyAlgorithmProvider.OpenAlgorithm(Algo);
end;

function TFrmSymmetricEncryption.SelectKeySizeInBytes: integer;
begin
  result := StrToInt(cboKeySize.Text) div 8;
end;

function TFrmSymmetricEncryption.SelectAlgoRequiresPadding: boolean;
begin
  result := cboAlgo.ItemIndex in [0, 2]; // Without PKCS #7
end;

function TFrmSymmetricEncryption.SelectAlgoRequiresIV: boolean;
begin
  result := cboAlgo.ItemIndex in [0, 1]; // Cbc block cipher mode
end;
{$ENDREGION}

{$REGION 'Encrypted Message Load/Save'}
procedure TFrmSymmetricEncryption.btnSaveEncryptClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add(EditEncrypted.Text);
    sl.Add(EditIV.Text);
    sl.SaveToFile(fDataFolder + 'Encrypted.txt');
  finally
    sl.Free;
  end;
end;

procedure TFrmSymmetricEncryption.btnLoadEncryptClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'Encrypted.txt');
    if sl.Count <> 2 then
      raise Exception.Create(
        'Format Error: Encrypted text and IV on two lines expected');
    EditEncrypted.Text := sl[0];
    EditIV.Text := sl[1];
    btnDecrypt.Enabled := true;
    chbAutoRenew.Checked := false;
  finally
    sl.Free;
  end;
end;
{$ENDREGION}

end.
