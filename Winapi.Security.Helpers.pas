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

unit Winapi.Security.Helpers;

interface

uses
  System.Classes, System.SysUtils, System.Win.WinRT,
  Winapi.Windows, Winapi.WinRT, Winapi.CommonTypes, Winapi.Security.Cryptography;

type
  TWinRTCryptoHelpers = class
    class function EncodeAsBase64(Buf: IBuffer): string;
    class function DecodeFromBase64(const Base64: string): IBuffer;
    class function StrToIBuffer(const Text: string): IBuffer;
    class function IBufferToStr(Buf: IBuffer): string;
  end;

implementation

resourcestring
  rsNotApplicable = 'n/a';

class function TWinRTCryptoHelpers.EncodeAsBase64(Buf: IBuffer): string;
begin
  if not assigned(Buf) then
    exit(rsNotApplicable);
  result := TWindowsString.HStringToString(
    TCryptographicBuffer.EncodeToBase64String(Buf));
end;

class function TWinRTCryptoHelpers.DecodeFromBase64(
  const Base64: string): IBuffer;
begin
  if SameText(Base64, rsNotApplicable) then
    exit(nil);
  result := TCryptographicBuffer.DecodeFromBase64String(
    TWindowsString.Create(Base64));
end;

class function TWinRTCryptoHelpers.StrToIBuffer(const Text: string): IBuffer;
var
  data: TBytes;
begin
  data := TEncoding.UTF8.GetBytes(Text);
  result := TCryptographicBuffer.CreateFromByteArray(length(data), @data[0]);
end;

class function TWinRTCryptoHelpers.IBufferToStr(Buf: IBuffer): string;
begin
  result := TCryptographicBuffer.ConvertBinaryToString(
    BinaryStringEncoding.Utf8, Buf).ToString;
end;

end.
