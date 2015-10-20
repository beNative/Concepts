(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Bindings.VCLDesigntime;

interface

uses
  Classes,
  DesignEditors,
  DSharp.Bindings.Designtime;

type
  TBindingGroupSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  ComCtrls,
  CommCtrl,
  DesignIntf,
  DSharp.Bindings,
  ExtCtrls,
  Forms,
  Grids,
  StdCtrls;

procedure Register;
begin
  RegisterSelectionEditor(TBindingGroup, TBindingGroupSelectionEditor);
end;

{ TBindingGroupSelectionEditor }

procedure TBindingGroupSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  LClass: TClass;
begin
  inherited;
  for LClass in SupportedClasses.Keys do
  begin
    Proc(LClass.UnitName);
  end;
  Proc(TStringGrid.UnitName);
  Proc('DSharp.Bindings.VCLControls');
end;

initialization
  SupportedClasses.Add(TCheckBox, 'Checked');
  SupportedClasses.Add(TColorBox, 'Selected');
  SupportedClasses.Add(TComboBox, 'Text');
  SupportedClasses.Add(TDateTimePicker, 'Date'); // need some review to change binding property depending on state
  SupportedClasses.Add(TEdit, 'Text');
  SupportedClasses.Add(TGroupBox, 'BindingSource');
  SupportedClasses.Add(TLabel, 'Caption');
  SupportedClasses.Add(TLabeledEdit, 'Text');
  SupportedClasses.Add(TListBox, 'ItemsSource');
  SupportedClasses.Add(TMemo, 'Text');
  SupportedClasses.Add(TMonthCalendar, 'Date');
  SupportedClasses.Add(TPanel, 'BindingSource');
  SupportedClasses.Add(TRadioButton, 'Checked');
  SupportedClasses.Add(TRadioGroup, 'ItemIndex');
//  SupportedClasses.Add(TStatusPanel, 'Text'); // does not work (yet)
  SupportedClasses.Add(TTrackBar, 'Position');

finalization
  SupportedClasses.Remove(TCheckBox);
  SupportedClasses.Remove(TColorBox);
  SupportedClasses.Remove(TComboBox);
  SupportedClasses.Remove(TDateTimePicker);
  SupportedClasses.Remove(TEdit);
  SupportedClasses.Remove(TGroupBox);
  SupportedClasses.Remove(TLabel);
  SupportedClasses.Remove(TLabeledEdit);
  SupportedClasses.Remove(TListBox);
  SupportedClasses.Remove(TMemo);
  SupportedClasses.Remove(TMonthCalendar);
  SupportedClasses.Remove(TPanel);
  SupportedClasses.Remove(TRadioButton);
  SupportedClasses.Remove(TRadioGroup);
//  SupportedClasses.Remove(TStatusPanel, 'Text');
  SupportedClasses.Remove(TTrackBar);

end.