{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.Spring.LazyInstantiation.Form;

{ Demonstrates lazy instantiation with Spring.Lazy<T>. }

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Winapi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ActnList,

  Spring;

type
  TMyType = class
  end;

type
  TfrmLazyInstantiation = class(TForm)
    aclMain               : TActionList;
    actAddTextToLazyMemo  : TAction;
    actShowClassName      : TAction;
    actToggleLazyCheckbox : TAction;
    btnAddTextToLazyMemo  : TButton;
    btnShowClassName      : TButton;
    btnToggleLazyCheckbox : TButton;
    lblDescription        : TLabel;
    pnlCheckBox           : TPanel;
    pnlMemo               : TPanel;

    procedure actToggleLazyCheckboxExecute(Sender: TObject);
    procedure actAddTextToLazyMemoExecute(Sender: TObject);
    procedure actShowClassNameExecute(Sender: TObject);

  private
    FLazyCheckBox : Lazy<TCheckBox>;
    FLazyMemo     : Lazy<TMemo>;
    FLazyMyType   : Lazy<TMyType>;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmLazyInstantiation.AfterConstruction;
begin
  inherited AfterConstruction;
  FLazyCheckBox.Create(
    function: TCheckBox
    begin
      pnlCheckBox.ShowCaption := False;
      Result := TCheckBox.Create(Self);
      Result.Parent  := pnlCheckBox;
      Result.Align   := alClient;
      Result.Caption := 'Lazy checkbox';
    end
  );
  FLazyMemo.Create(
    function: TMemo
    begin
      pnlMemo.ShowCaption := False;
      Result := TMemo.Create(Self);
      Result.Parent := pnlMemo;
      Result.Align  := alClient;
      Result.Text   := 'Lazy memo';
    end
  );
  FLazyMyType.Create(
    function: TMyType
    begin
      Result := TMyType.Create;
    end
  );
end;

procedure TfrmLazyInstantiation.BeforeDestruction;
begin
  if FLazyMyType.IsValueCreated then
    FLazyMyType.Value.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmLazyInstantiation.actShowClassNameExecute(Sender: TObject);
begin
  ShowMessage(FLazyMyType.Value.ClassName);
end;

procedure TfrmLazyInstantiation.actToggleLazyCheckboxExecute(Sender: TObject);
begin
  FLazyCheckBox.Value.Checked := not FLazyCheckBox.Value.Checked;
end;

procedure TfrmLazyInstantiation.actAddTextToLazyMemoExecute(Sender: TObject);
begin
  FLazyMemo.Value.Lines.Add('Added this line to lazy memo!');
end;
{$ENDREGION}

end.
