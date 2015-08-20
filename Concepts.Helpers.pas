{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit Concepts.Helpers;

{ Collection of helper routines. }

interface

uses
  Classes, Controls,

  Spring.Collections;

procedure LoadFromEmbedded(
        AImgList : TImageList;
  const AZipName : string
);

procedure FillListWithContacts(
  AList  : IList;
  ACount : Integer
);

//procedure ExecuteCommandLine(const commandLine: string;
//  var exitCode: Cardinal);

implementation

uses
  ImgList, Forms, Graphics,

  PasZip,

  DDuce.RandomData,

  Concepts.Types.Contact;

procedure FillListWithContacts(AList: IList; ACount: Integer);
var
  C : TContact;
  I : Integer;
begin
  if Assigned(AList) then
  begin
    AList.Clear;
    for I := 0 to ACount - 1 do
    begin
      C := TContact.Create;
      with C do
      begin
        Firstname   := RandomData.FirstName(gnMale);
        Lastname    := RandomData.LastName;
        CompanyName := RandomData.CompanyName;
        Email       := RandomData.Email(Firstname, Lastname);
        Address     := RandomData.Address;
        Number      := RandomData.Number(100);
        BirthDate   := RandomData.BirthDate(1920, 1988);
      end;
      AList.Add(C);
    end;
  end;
end;

/// uncompress an image list from a .zip embedded as a .res to the executable
procedure LoadFromEmbedded(AImgList: TImageList; const AZipName: string);
var
  I      : Integer;
  BMP    : TBitmap;
  Stream : TStringStream;
  BW     : Integer;
  BH     : Integer;
  W      : Integer;
  H      : Integer;
begin
  with TZipRead.Create(HInstance, 'Zip', 'ZIP') do
  begin
    try
      I := NameToIndex(AnsiString(AZipName));
      if I < 0 then
        exit;
      Stream := TStringStream.Create(UnZip(I)); // uncompress
      try
        BMP := TBitmap.Create;
        try
          BMP.LoadFromStream(Stream);
        // from multi-line (I.e. IDE export) into one-line (for AddMasked)
          BW := BMP.Width;
          BH := BMP.Height;
          W := (BW div AImgList.Width);
          H := (BH div AImgList.Height);
          BMP.Width := W * H * AImgList.Width;
          BH := AImgList.Height;
          for I := 2 to H do
            BMP.Canvas.CopyRect(Rect((I - 1) * BW, 0, I * BW, BH), BMP.Canvas,
              Rect(0, (I - 1) * BH, BW, I * BH));
          BMP.Height := BH;
        // add these images to the image list
          AImgList.AddMasked(BMP, clFuchsia);
        finally
          BMP.Free;
        end;
      finally
        Stream.Free;
      end;
    finally
      Free;
    end;
  end;
end;

//procedure ExecuteCommandLine(const commandLine: string;
//  var exitCode: Cardinal);
//var
//  localCommandLine: string;
//  startupInfo: TStartupInfo;
//  processInfo: TProcessInformation;
//begin
//  ZeroMemory(@startupInfo, SizeOf(startupInfo));
//  ZeroMemory(@processInfo, SizeOf(processInfo));
//  startupInfo.cb := SizeOf(startupInfo);
//  localCommandLine := commandLine;
//  UniqueString(localCommandLine);
//  if not CreateProcess(nil, PChar(localCommandLine), nil, nil, True,
//    CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS,
//    nil, nil, startupInfo, processInfo) then
//  begin
//    raise Exception.Create('Failed to create the process.');
//  end;
//  WaitForSingleObject(processInfo.hProcess, INFINITE);
//  GetExitCodeProcess(processInfo.hProcess, exitCode);
//  CloseHandle(processInfo.hProcess);
//  CloseHandle(processInfo.hThread);
//end;

end.
