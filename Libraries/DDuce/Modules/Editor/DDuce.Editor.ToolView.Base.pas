{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.ToolView.Base;

{ Base toolview form that can be used to create descendants that implement
  IEditorToolView.
  It reacts to changes in the common settings (IEditorSettings) which are
  associated with the owning manager (IEditorManager) instance.

  This base class provides properties which are shortcuts to the following
  instances that are used by the editor manager:
    - Manager  : IEditorManager   - The owning editor manager instance
    - Settings : IEditorSettings  - All persistable settings
    - View     : IEditorView      - The currently active editor view
    - Views    : IEditorViews     - The list of available editor views

  It provides virtual event handlers which act as callback methods to let us
  respond to certain changes in the active editor view or to changes in the
  settings. It acts as an observer to react on any relevant changes in the
  observed instances.
}

interface

uses
  System.Classes,
  Vcl.Forms,

  DDuce.Editor.Interfaces;

type
  TCustomEditorToolView = class(TForm, IEditorToolView)
  strict private
    // this flag is set when there are pending updates.
    FUpdate: Boolean;

    function GetManager: IEditorManager;
    function GetSettings: IEditorSettings;
    function GetUpdate: Boolean;
    function GetView: IEditorView;
    function GetViews: IEditorViews;
    procedure SetUpdate(AValue: Boolean);

  strict protected
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean); virtual;

    // virtual event handlers
    procedure EditorCaretPositionChange(
      Sender : TObject;
      X, Y   : Integer
    ); virtual;
    procedure EditorSettingsChanged(Sender: TObject); virtual;
    procedure EditorActiveViewChanged(Sender: TObject); virtual;
    procedure EditorModified(Sender: TObject); virtual;
    procedure EditorChange(Sender: TObject); virtual;

    procedure Modified; virtual;
    procedure UpdateView; virtual;
    procedure SettingsChanged; virtual;

    property Update: Boolean
      read GetUpdate write SetUpdate;

    property Manager: IEditorManager
      read GetManager;

    property Settings: IEditorSettings
      read GetSettings;

    property View: IEditorView
      read GetView;

    property Views: IEditorViews
      read GetViews;

  public
    procedure AfterConstruction; override;
  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TCustomEditorToolView.AfterConstruction;
begin
  inherited AfterConstruction;
  Manager.Settings.OnChanged.Add(EditorSettingsChanged);
  Manager.Events.OnCaretPositionChange.Add(EditorCaretPositionChange);
  Manager.Events.OnActiveViewChange.Add(EditorActiveViewChanged);
  Manager.Events.OnChange.Add(EditorChange);
  Manager.Events.OnModified.Add(EditorModified);
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TCustomEditorToolView.GetUpdate: Boolean;
begin
  Result := FUpdate;
end;

procedure TCustomEditorToolView.SetUpdate(AValue: Boolean);
begin
  if AValue <> Update then
  begin
    FUpdate := AValue;
  end;
end;

procedure TCustomEditorToolView.SetVisible(AValue: Boolean);
begin
  inherited Visible := AValue;
end;

function TCustomEditorToolView.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TCustomEditorToolView.GetViews: IEditorViews;
begin
  Result := Owner as IEditorViews;
end;

function TCustomEditorToolView.GetForm: TForm;
begin
  Result := Self;
end;

function TCustomEditorToolView.GetName: string;
begin
  Result := inherited Name;
end;

function TCustomEditorToolView.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

function TCustomEditorToolView.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TCustomEditorToolView.GetSettings: IEditorSettings;
begin
  Result := Owner as IEditorSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TCustomEditorToolView.EditorCaretPositionChange(Sender: TObject; X,
  Y: Integer);
begin
  UpdateView;
end;

procedure TCustomEditorToolView.EditorSettingsChanged(Sender: TObject);
begin
  SettingsChanged;
end;

procedure TCustomEditorToolView.EditorActiveViewChanged(Sender: TObject);
begin
  UpdateView;
end;

procedure TCustomEditorToolView.EditorModified(Sender: TObject);
begin
  UpdateView;
end;

procedure TCustomEditorToolView.EditorChange(Sender: TObject);
begin
  UpdateView;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TCustomEditorToolView.UpdateView;
begin
  // to be overridden
end;

procedure TCustomEditorToolView.Modified;
begin
  FUpdate := True;
end;

{ Responds to changes in the global settings. }

procedure TCustomEditorToolView.SettingsChanged;
begin
  // to be overridden
end;
{$ENDREGION}

end.

