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

unit DDuce.Editor.CodeTags;

interface

uses
  System.Classes, System.SysUtils;

type
  TCodeTagItem = class(TComponent)
  strict private
    FEndTag   : string;
    FStartTag : string;

    procedure SetEndTag(AValue: string);
    procedure SetStartTag(AValue: string);

  public
    procedure Assign(Source: TPersistent); override;

  published
    property StartTag: string
      read FStartTag write SetStartTag;

    property EndTag: string
      read FEndTag write SetEndTag;
  end;

  { TCodeTags }

  TCodeTags = class(TComponent)
  private
    function GetItem(Index: Integer): TCodeTagItem;
    procedure SetItem(Index: Integer; AValue: TCodeTagItem);
  public
    function Add: TCodeTagItem;

    property Items[Index: Integer]: TCodeTagItem
      read GetItem write SetItem; default;
  end;

implementation

function TCodeTags.GetItem(Index: Integer): TCodeTagItem;
begin
  Result := Components[Index] as TCodeTagItem;
end;

procedure TCodeTags.SetItem(Index: Integer; AValue: TCodeTagItem);
begin
  Components[Index].Assign(AValue);
end;

function TCodeTags.Add: TCodeTagItem;
begin
  Result := TCodeTagItem.Create(Self);
end;

{$REGION 'property access mehods'}
procedure TCodeTagItem.SetStartTag(AValue: string);
begin
  if FStartTag = AValue then
    Exit;
  FStartTag := AValue;
end;

procedure TCodeTagItem.Assign(Source: TPersistent);
var
  CTI : TCodeTagItem;
begin
 if (Source <> Self) and (Source is TCodeTagItem) then
 begin
   CTI := TCodeTagItem(Source);
   StartTag := CTI.StartTag;
   EndTag := CTI.EndTag;
 end
 else
   inherited Assign(Source);
end;

procedure TCodeTagItem.SetEndTag(AValue: string);
begin
  if FEndTag = AValue then
    Exit;
  FEndTag := AValue;
end;
{$ENDREGION}

initialization
  RegisterClass(TCodeTags);
  RegisterClass(TCodeTagItem);

end.

