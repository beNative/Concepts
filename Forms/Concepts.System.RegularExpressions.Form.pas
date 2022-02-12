{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I Concepts.inc}

unit Concepts.System.RegularExpressions.Form;

{ Demonstrates the RTL's System.RegularExpressions unit and the use of
  anonymous threads to execute background tasks. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.RegularExpressions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmRegularExpressions = class(TForm)
    mmoInput         : TMemo;
    mmoOutput        : TMemo;
    edtRegExpression : TEdit;
    edtReplace       : TEdit;
    lblError         : TLabel;
    chkThreaded      : TCheckBox;
    lblWorking       : TLabel;

    {$REGION 'event handlers'}
    procedure edtRegExpressionChange(Sender: TObject);
    procedure edtReplaceChange(Sender: TObject);
    procedure mmoInputChange(Sender: TObject);
    {$ENDREGION}

  private
    FThreadRunning : Boolean; // read/write on byte values is atomic

    procedure ThreadTerminate(Sender: TObject);

    procedure UpdateView;
    procedure UpdateViewThreaded;
    procedure ExecuteUpdateView;

  protected
    procedure UpdateActions; override;

  end;

implementation

{$R *.dfm}

{$REGION 'event handlers'}
procedure TfrmRegularExpressions.edtRegExpressionChange(Sender: TObject);
begin
  ExecuteUpdateView;
end;

procedure TfrmRegularExpressions.edtReplaceChange(Sender: TObject);
begin
  ExecuteUpdateView;
end;

procedure TfrmRegularExpressions.mmoInputChange(Sender: TObject);
begin
  ExecuteUpdateView;
end;

{ Executed AFTER the thread terminates }

procedure TfrmRegularExpressions.ThreadTerminate(Sender: TObject);
begin
  FThreadRunning := False;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmRegularExpressions.ExecuteUpdateView;
begin
  if (edtRegExpression.Text <> '') and (mmoInput.Text <> '') then
  begin
    if chkThreaded.Checked then
      UpdateViewThreaded
    else
      UpdateView;
  end;
end;

procedure TfrmRegularExpressions.UpdateActions;
begin
  inherited;
  if FThreadRunning then
    lblWorking.Caption := 'Working...'
  else
    lblWorking.Caption := '';
end;

procedure TfrmRegularExpressions.UpdateView;
var
  M  : TMatch;
begin
  mmoOutput.Clear;
  try
    for M in TRegEx.Matches(
      mmoInput.Text,
      edtRegExpression.Text
    ) do
    begin
      lblError.Caption := '';
      edtRegExpression.Color := clWhite;
      mmoOutput.Lines.Add(M.Value);
      FThreadRunning := False;
    end;
  except
    on E: Exception do
    begin
      edtRegExpression.Color := $00C4C4FF;
      lblError.Caption := E.Message;
    end;
  end;
end;

procedure TfrmRegularExpressions.UpdateViewThreaded;
var
  SI : string;
  SO : string;
  S  : string;
  T  : TThread;
  ES : string;
begin
  if FThreadRunning = False then
  begin
    mmoOutput.Clear;
    lblError.Caption := '';
    edtRegExpression.Color := clWhite;
    SI := mmoInput.Text;
    S  := edtRegExpression.Text;

    T := TThread.CreateAnonymousThread(
      procedure
      var
        M  : TMatch;
      begin
        FThreadRunning := True;
        try
          for M in TRegEx.Matches(
            SI,
            S
          ) do
          begin
            SO := SO + #13#10 + M.Value;
          end;
        except
          on E: Exception do
          begin
            ES := E.Message;
            TThread.Synchronize(
              TThread.CurrentThread,
              procedure
              begin
                edtRegExpression.Color := $00C4C4FF;
                lblError.Caption := ES;
              end
            );
          end;
        end;
        TThread.Queue(TThread.CurrentThread,
          procedure
          begin
            mmoOutput.Text := SO;
          end
        );
      end
    );
    // executed AFTER thread terminates and BEFORE it is freed.
    T.OnTerminate := ThreadTerminate;
    T.Start;
  end;
end;
{$ENDREGION}

end.
