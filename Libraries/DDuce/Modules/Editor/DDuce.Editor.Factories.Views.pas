{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.Factories.Views;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Controls,

  DDuce.Editor.Interfaces;

type
  TEditorViewFactory = class(TInterfacedObject, IEditorViewFactory)
    function CreateInstance(
       AParent            : TWinControl;
       AManager           : IEditorManager;
       const AName        : string = '';
       const AFileName    : string = '';
       const AHighlighter : string = 'TXT'
    ): IEditorView;
  end;

implementation

uses
  Vcl.Forms,

  Spring;

function TEditorViewFactory.CreateInstance(AParent: TWinControl;
  AManager: IEditorManager; const AName: string; const AFileName: string;
  const AHighlighter: string): IEditorView;
var
  V: IEditorView;
begin
  Guard.CheckNotNull(AParent, 'AParent');
  Guard.CheckNotNull(AManager, 'AManager');
  V := AManager.Views.Add(AName, AFileName, AHighlighter);
  V.Form.DisableAlign;
  V.Form.BorderStyle := bsNone;
  V.Form.Align := alClient;
  V.Form.Parent := AParent;
  V.PopupMenu := AManager.Menus.EditorPopupMenu;
  V.Form.Visible := True;
  V.Form.EnableAlign;
  Result := V;
end;

end.

