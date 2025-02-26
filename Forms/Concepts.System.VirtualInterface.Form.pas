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

{$I Concepts.inc}

unit Concepts.System.VirtualInterface.Form;

{ Form demonstrating how to use the System.Rtti.TVirtualInterface class, which
  was introduced in Delphi XE2. }

{
  Remarks
    - TVirtualInterface works only on interfaces which have TypeInfo enabled
      for it ($M+ directive or descendant from IInvokable)
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Rtti, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls;

type
  IGoStop = interface(IInvokable)
  ['{DAA28BA6-2243-41BE-BC61-2A548999753A}']
    procedure Go(AInteger: Integer = 5);
    procedure Stop(const AString: string = 'TEST');
  end;

  TfrmVirtualInterfaceDemo = class(TForm)
    aclMain       : TActionList;
    actGo         : TAction;
    actStop       : TAction;
    lblDefinition : TLabel;
    btnGo         : TButton;
    btnStop       : TButton;

    {$REGION 'action handlers'}
    procedure actGoExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    {$ENDREGION}

  private
    FGoStop : IGoStop;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmVirtualInterfaceDemo.AfterConstruction;
var
 VIIE : TVirtualInterfaceInvokeEvent; // anonymous callback procedure
begin
  inherited AfterConstruction;
  VIIE := procedure(Method: TRttiMethod; const Args: TArray<TValue>;
    out Result: TValue)
    const
      MSG = 'Method declaration: %s' + sLineBreak +
            'Argument values:' + sLineBreak + '%s' +
            'Return value: %s';
    var
      S : string;
      V : TValue;
    begin
      for V in Args do
        S := S + V.ToString + sLineBreak;
      S := Format(MSG, [Method.ToString, S, Result.ToString]);
      ShowMessage(S);
    end;
  FGoStop := TVirtualInterface.Create(TypeInfo(IGoStop), VIIE) as IGoStop;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmVirtualInterfaceDemo.actGoExecute(Sender: TObject);
begin
  FGoStop.Go;
end;

procedure TfrmVirtualInterfaceDemo.actStopExecute(Sender: TObject);
begin
  FGoStop.Stop;
end;
{$ENDREGION}

end.
