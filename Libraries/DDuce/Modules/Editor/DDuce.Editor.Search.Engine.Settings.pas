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

unit DDuce.Editor.Search.Engine.Settings;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls,

  TextEditor.Types;

type
  TSearchEngineSettings = class(TComponent)
  private
    FOptions        : TTextEditorSearchOptions;
    FSearchAllViews : Boolean;

  public
    procedure AssignTo(ADest: TPersistent); override;
    procedure Assign(ASource: TPersistent); override;

  published
    {
      TTextEditorSearchOption = (
      soBackwards,
      soBeepIfStringNotFound,
      soCaseSensitive,
      soEntireScope,
      soHighlightResults,
      soSearchOnTyping,
      soSelectedOnly,
      soShowStringNotFound,
      soShowSearchMatchNotFound,
      soWholeWordsOnly,
      soWrapAround
    );
    }
    property Options : TTextEditorSearchOptions
      read FOptions write FOptions;

    property SearchAllViews: Boolean
      read FSearchAllViews write FSearchAllViews default False;

  end;

implementation

{$REGION 'public methods'}
procedure TSearchEngineSettings.AssignTo(ADest: TPersistent);
var
  SES : TSearchEngineSettings;
begin
  if ADest is TSearchEngineSettings then
  begin
    SES := TSearchEngineSettings(ADest);
    SES.Options        := Options;
    SES.SearchAllViews := SearchAllViews;
  end
  else
    inherited AssignTo(ADest);
end;

procedure TSearchEngineSettings.Assign(ASource: TPersistent);
var
  SES : TSearchEngineSettings;
begin
  if ASource is TSearchEngineSettings then
  begin
    SES := TSearchEngineSettings(ASource);
    Options        := SES.Options;
    SearchAllViews := SES.SearchAllViews;
  end
  else
    inherited Assign(ASource);
end;
{$ENDREGION}

initialization
  RegisterClass(TSearchEngineSettings);

end.


