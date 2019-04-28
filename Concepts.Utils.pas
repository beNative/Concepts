{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I Concepts.inc}

unit Concepts.Utils;

{ Somme commonly used routines. }

interface

uses
  Winapi.WinSock, Winapi.WinInet,
  System.Classes, System.Rtti, System.TypInfo, System.SysUtils,
  Vcl.Controls, Vcl.Graphics,

  Data.DB;

type
  TBitmap = Vcl.Graphics.TBitmap;

procedure AutoSizeDisplayWidths(
  ADataSet : TDataSet;
  ACount   : Integer = 100;
  AOffset  : Integer = 2
); overload;

procedure AutoSizeDisplayWidths(
  ADataSet : TDataSet;
  AFont    : TFont;
  ACount   : Integer = 100;
  AOffset  : Integer = 0
); overload;

procedure GetIPAddresses(AStrings: TStrings);

implementation

uses
  Winapi.Windows, Winapi.Messages,
  System.StrUtils, System.Variants, System.Character, System.UITypes,
  Vcl.Forms, Vcl.Dialogs;

{$REGION 'interfaced routines'}
procedure AutoSizeDisplayWidths(ADataSet: TDataSet; AFont: TFont;
  ACount: Integer; AOffset: Integer);
var
  BM : TBookmark;
  I  : Integer;
  J  : Integer;
  L  : Integer;

  function GetTextWidth(const AText: string; AFont: TFont): Integer;
  var
    Bitmap  : TBitmap;
    SL      : TStringList;
    I, W, R : Integer;
  begin
    SL := TStringList.Create;
    try
      SL.Text := AText;
      Bitmap := TBitmap.Create;
      try
        Bitmap.Canvas.Font.Assign(AFont);
        R := 0;
        for I := 0 to SL.Count - 1 do
        begin
          W := Bitmap.Canvas.TextWidth(SL[I]);
          if W > R then
            R := W;
        end;
        Result := R div AFont.Size;
      finally
        Bitmap.Free;
      end;
    finally
      SL.Free;
    end;
  end;

begin
  if not Assigned(ADataSet) then
    raise Exception.Create('ADataSet not assigned!');
  ADataSet.DisableControls;
  try
    BM := ADataSet.Bookmark;
    try
      for J := 0 to ADataSet.Fields.Count - 1 do
        ADataSet.Fields[J].DisplayWidth := Length(ADataSet.Fields[J].DisplayLabel);

      ADataSet.First;
      I := 0;
      while (I < ACount) and not ADataSet.Eof do
      begin
        for J := 0 to ADataSet.Fields.Count - 1 do
        begin
          if ADataSet.Fields[J].DataType in
            [ftMemo, ftWideMemo, ftString, ftWideString]  then
            L := GetTextWidth(ADataSet.Fields[J].DisplayText, AFont) + AOffset
          else
            L := Length(ADataSet.Fields[J].DisplayText) + AOffset;
          if L > ADataSet.Fields[J].DisplayWidth then
            ADataSet.Fields[J].DisplayWidth := L;
        end;
        ADataSet.Next;
        Inc(I);
      end;
    finally
      ADataSet.Bookmark := BM;
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

{
  REMARK : This method is not suitable for filtered datasets. For filtered
  datasets the records should be enumerated with the dataset's FindFirst and
  FindNext methods. We didn't use those methods because when used in combination
  with a ClientDataSet (with fetch on demand enabled) this causes the provider
  to fetch all the records from the server.
}

procedure AutoSizeDisplayWidths(ADataSet : TDataSet; ACount : Integer;
  AOffSet : Integer);
var
  BM : TBookmark;
  I  : Integer;
  J  : Integer;
  L  : Integer;

  function GetTextWidth(const AText: string): Integer;
  var
    SL      : TStringList;
    I, W, R : Integer;
  begin
    SL := TStringList.Create;
    try
      SL.Text := AText;
      R := 0;
      for I := 0 to SL.Count - 1 do
      begin
        W := Length(SL[I]);
        if W > R then
          R := W;
      end;
      Result := R;
    finally
      SL.Free;
    end;
  end;

begin
  if not Assigned(ADataSet) then
    raise Exception.Create('ADataSet not assigned!');
  ADataSet.DisableControls;
  try
    BM := ADataSet.Bookmark;
    try
      for J := 0 to ADataSet.Fields.Count - 1 do
        ADataSet.Fields[J].DisplayWidth := Length(ADataSet.Fields[J].DisplayLabel);
      ADataSet.First;
      I := 0;
      while (I < ACount) and not ADataSet.Eof do
      begin
        for J := 0 to ADataSet.Fields.Count - 1 do
        begin
          if ADataSet.Fields[J].DataType in
            [ftMemo, {ftWideMemo,} ftString, ftWideString]  then
            L := GetTextWidth(ADataSet.Fields[J].DisplayText) + AOffset
          else
            L := Length(ADataSet.Fields[J].DisplayText) + AOffset;
          if L > ADataSet.Fields[J].DisplayWidth then
            ADataSet.Fields[J].DisplayWidth := L;
        end;
        ADataSet.Next;
        Inc(I);
      end;
    finally
      ADataSet.Bookmark := BM;
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

procedure GetIPAddresses(AStrings: TStrings);
type
  TaPInAddr = array [0 .. 10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  PHE       : PHostEnt;
  PPtr      : PaPInAddr;
  Buffer    : array [0 .. 63] of AnsiChar;
  I         : Integer;
  GInitData : TWSAData;
begin
  WSAStartup($101, GInitData);
  AStrings.Clear;
  GetHostName(Buffer, SizeOf(Buffer));
  PHE := GetHostByName(Buffer);
  if PHE = nil then
    Exit;
  PPtr := PaPInAddr(PHE^.h_addr_list);
  I    := 0;
  while PPtr^[I] <> nil do
  begin
    AStrings.Add(string(inet_ntoa(PPtr^[I]^)));
    Inc(I);
  end;
  WSACleanup;
end;
{$ENDREGION}

end.
