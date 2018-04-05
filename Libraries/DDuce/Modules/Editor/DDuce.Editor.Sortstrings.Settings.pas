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

unit DDuce.Editor.SortStrings.Settings;

interface

uses
  System.Classes, System.SysUtils,

  DDuce.Editor.Types;

const
  DEFAULT_WIDTH = 360;

type
  TSortStringsSettings = class(TComponent)
  strict private
    FCaseSensitive : Boolean;
    FIgnoreSpaces  : Boolean;
    FSortDirection : TSortDirection;
    FSortScope     : TSortScope;
    FWidth         : Integer;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property SortDirection: TSortDirection
      read FSortDirection write FSortDirection default sdAscending;

    property SortScope: TSortScope
      read FSortScope write FSortScope default ssLines;

    property CaseSensitive: Boolean
      read FCaseSensitive write FCaseSensitive default False;

    property IgnoreSpaces: Boolean
      read FIgnoreSpaces write FIgnoreSpaces default False;

    property Width: Integer
      read FWidth write FWidth default DEFAULT_WIDTH;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TSortStringsSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FWidth := DEFAULT_WIDTH;
end;

procedure TSortStringsSettings.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TSortStringsSettings.AssignTo(Dest: TPersistent);
var
  S: TSortStringsSettings;
begin
  if Dest is TSortStringsSettings then
  begin
    S := TSortStringsSettings(Dest);
    S.Width := Width;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TSortStringsSettings.Assign(Source: TPersistent);
var
  S : TSortStringsSettings;
begin
  if Source is TSortStringsSettings then
  begin
    S := TSortStringsSettings(Source);
    Width := S.Width;
    SortDirection :=  S.SortDirection;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(TSortStringsSettings);

end.

