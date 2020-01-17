{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.Winapi.LockPaint.Form;

{ Form demonstrating the effect of the WM_SETREDRAW windows messages and how
  they can be used to lock paint messages on a control. }

interface

uses
  System.Actions, System.Classes,
  Vcl.ActnList, Vcl.Controls, Vcl.StdCtrls, Vcl.Forms;

type
  TfrmLockPaint = class(TForm)
    {$REGION 'designer controls'}
    btnLockPaint          : TButton;
    btnUnlockPaint        : TButton;
    btnLockWindowUpdate   : TButton;
    btnUnlockWindowUpdate : TButton;
    aclMain               : TActionList;
    actLockPaint          : TAction;
    actUnlockPaint        : TAction;
    actLockWindowUpdate   : TAction;
    actUnlockWindowUpdate : TAction;
    actDraw               : TAction;
    {$ENDREGION}

    procedure actLockPaintExecute(Sender: TObject);
    procedure actUnlockPaintExecute(Sender: TObject);
    procedure actLockWindowUpdateExecute(Sender: TObject);
    procedure actUnlockWindowUpdateExecute(Sender: TObject);
    procedure actDrawExecute(Sender: TObject);

  end;

implementation

{$R *.dfm}

uses
  Winapi.Windows,

  DDuce.Utils;

{$REGION 'action handlers'}
procedure TfrmLockPaint.actDrawExecute(Sender: TObject);
begin
  Canvas.Rectangle(10, 100, 500, 300);
end;

procedure TfrmLockPaint.actLockPaintExecute(Sender: TObject);
begin
  LockPaint(Self);
end;

procedure TfrmLockPaint.actLockWindowUpdateExecute(Sender: TObject);
begin
  LockWindowUpdate(Self.Handle);
end;

procedure TfrmLockPaint.actUnlockPaintExecute(Sender: TObject);
begin
  UnLockPaint(Self);
end;

procedure TfrmLockPaint.actUnlockWindowUpdateExecute(Sender: TObject);
begin
  LockWindowUpdate(0);
end;
{$ENDREGION}

end.

