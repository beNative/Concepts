{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.ViewList.Data;

interface

uses
  System.Classes,

  DDuce.Editor.Interfaces;

type
  TEditorViewInfo = class
  private
    FView : TComponent; // TS: no interface reference here!

    {$REGION 'property access methods'}
    function GetFileName: string;
    function GetHighlighter: string;
    function GetModified: Boolean;
    function GetPath: string;
    function GetView: IEditorView;
    {$ENDREGION}

  public
    constructor Create(AView: IEditorView);
    procedure BeforeDestruction; override;

    property View: IEditorView
      read GetView;

    property FileName: string
      read GetFileName;

    property Path: string
      read GetPath;

    property Highlighter: string
      read GetHighlighter;

    property Modified: Boolean
      read GetModified;

  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
constructor TEditorViewInfo.Create(AView: IEditorView);
begin
  FView := AView.Form;
end;

procedure TEditorViewInfo.BeforeDestruction;
begin
  FView := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TEditorViewInfo.GetFileName: string;
begin
  Result := View.Form.Caption;
end;

function TEditorViewInfo.GetHighlighter: string;
begin
//  if Assigned(View.HighlighterItem) then
//    Result := View.HighlighterItem.Name
//  else
//    Result := '';
end;

function TEditorViewInfo.GetModified: Boolean;
begin
  Result := View.Modified;
end;

function TEditorViewInfo.GetPath: string;
begin
  Result :=  ExtractFilePath(View.FileName);
end;

function TEditorViewInfo.GetView: IEditorView;
begin
  Result := FView as IEditorView;
end;
{$ENDREGION}

end.

