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

unit Concepts.System.PublishedFields.Form;

{ Designtime controls with no (automatically created) published field references
  to them. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

{  When you add components to a form at designtime, Delphi automatically adds
   published fields to the (implicit) published section of the class. These
   fields are automatically assigned to the corresponding components when they
   are streamed out from the DFM.
   The streaming system uses the classname from the RTTI generated for the
   published fields to create the components of the right class.

   It is possible to get rid of these published fields if you just register the
   corresponding classes. This is all that is needed to create the components
   on your form at runtime. }

type
  TfrmPublishedFields = class(TForm)
  public
    class constructor Create;
  end;

implementation

{$R *.dfm}

class constructor TfrmPublishedFields.Create;
begin
  RegisterClasses([TLabel, TButton, TEdit, TPanel]);
end;

end.
