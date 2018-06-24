{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Utils;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  Vcl.Graphics, Vcl.ExtCtrls;

function ExtractText(
  const AString : string;
  const ADelim1 : string;
  const ADelim2 : string
): string;

function GetTextWidth(const AText: string; AFont: TFont): Integer;

procedure OptimizeWidth(APanel: TPanel);

procedure OutputDebugString(const AString: string); overload;

procedure OutputDebugString(
  const AString : string;
  const AValues : array of const
); overload;

implementation

uses
  System.StrUtils;

{ Returns the total width for a given text and font. }

function GetTextWidth(const AText: string; AFont: TFont): Integer;
var
  Bitmap : TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(AFont);
    Result := Bitmap.Canvas.TextExtent(AText).cx;
  finally
    Bitmap.Free;
  end;
end;

{ Optimizes witdth according to its contents. }

procedure OptimizeWidth(APanel: TPanel);
var
  S: string;
begin
  S := APanel.Caption;
  if Trim(S) <> '' then
    APanel.Width := GetTextWidth(APanel.Caption, APanel.Font) + 10
  else
    APanel.Width := 0;
end;

{ Extracts the string between ADelim1 and ADelim2 from the given AString. }

function ExtractText(const AString: string; const ADelim1, ADelim2: string)
  : string;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := Pos(ADelim1, AString);
  if P1 > 0 then begin
    P2 := PosEx(ADelim2, AString, P1+1);
    if P2 > 0 then
      Result := Copy(AString, P1 + 1, P2 - P1 - 1);
  end;
end;

procedure OutputDebugString(const AString: string);
begin
  OutputDebugStringW(PChar(AString));
end;

procedure OutputDebugString(const AString : string; const AValues: array of const
); overload;
begin
  OutputDebugString(Format(AString, AValues));
end;

end.
