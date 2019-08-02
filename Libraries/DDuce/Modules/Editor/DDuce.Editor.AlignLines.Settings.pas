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

unit DDuce.Editor.AlignLines.Settings;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls,

  DDuce.Editor.Types;

const
  DEFAULT_WIDTH = 360;

type
  TAlignLinesSettings = class(TComponent)
  strict private
    FAlignInParagraphs    : Boolean;
    FAlignToToken         : TAlignToToken;
    FKeepSpaceAfterToken  : Boolean;
    FKeepSpaceBeforeToken : Boolean;
    FRemoveWhiteSpace     : Boolean;
    FSortAfterAlign       : Boolean;
    FSortDirection        : TSortDirection;
    FTokens               : TStringList;
    FWidth                : Integer;

    function GetTokens: TStrings;
    procedure SetTokens(AValue: TStrings);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property AlignInParagraphs: Boolean
      read FAlignInParagraphs write FAlignInParagraphs default False;

    property AlignToToken: TAlignToToken
      read FAlignToToken write FAlignToToken default atLeftMost;

    property SortAfterAlign: Boolean
      read FSortAfterAlign write FSortAfterAlign default False;

    property SortDirection: TSortDirection
      read FSortDirection write FSortDirection default sdAscending;

    property RemoveWhiteSpace: Boolean
      read FRemoveWhiteSpace write FRemoveWhiteSpace default False;

    property KeepSpaceBeforeToken: Boolean
      read FKeepSpaceBeforeToken write FKeepSpaceBeforeToken default False;

    property KeepSpaceAfterToken: Boolean
      read FKeepSpaceAfterToken write FKeepSpaceAfterToken default False;

    property Tokens: TStrings
      read GetTokens write SetTokens;

    property Width: Integer
      read FWidth write FWidth default DEFAULT_WIDTH;
  end;

implementation

{$REGION 'construction and destruction' /fold}
procedure TAlignLinesSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FSortDirection := sdAscending;
  FAlignToToken  := atLeftMost;
  FWidth         := DEFAULT_WIDTH;
  FTokens            := TStringList.Create;
  FTokens.Duplicates := dupIgnore;
  FTokens.Sorted     := True;
end;

procedure TAlignLinesSettings.BeforeDestruction;
begin
  FTokens.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods' /fold}
function TAlignLinesSettings.GetTokens: TStrings;
begin
  Result := FTokens;
end;

procedure TAlignLinesSettings.SetTokens(AValue: TStrings);
begin
  FTokens.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'public methods' /fold}
procedure TAlignLinesSettings.AssignTo(Dest: TPersistent);
var
  ALS: TAlignLinesSettings;
begin
  if Dest is TAlignLinesSettings then
  begin
    ALS := TAlignLinesSettings(Dest);
    ALS.KeepSpaceAfterToken  := KeepSpaceAfterToken;
    ALS.KeepSpaceBeforeToken := KeepSpaceBeforeToken;
    ALS.AlignInParagraphs    := AlignInParagraphs;
    ALS.RemoveWhiteSpace     := RemoveWhiteSpace;
    ALS.AlignToToken         := AlignToToken;
    ALS.Tokens               := Tokens;
    ALS.Width                := Width;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TAlignLinesSettings.Assign(Source: TPersistent);
var
  ALS: TAlignLinesSettings;
begin
  if Source is TAlignLinesSettings then
  begin
    ALS := TAlignLinesSettings(Source);
    KeepSpaceAfterToken  := ALS.KeepSpaceAfterToken;
    KeepSpaceBeforeToken := ALS.KeepSpaceBeforeToken;
    AlignInParagraphs    := ALS.AlignInParagraphs;
    AlignToToken         := ALS.AlignToToken;
    RemoveWhiteSpace     := ALS.RemoveWhiteSpace;
    Tokens               := ALS.Tokens;
    Width                := ALS.Width;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(TAlignLinesSettings);

end.

