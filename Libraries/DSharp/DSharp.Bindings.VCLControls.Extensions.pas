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

unit DSharp.Bindings.VCLControls.Extensions;

interface

uses
  DSharp.Bindings,
  DSharp.Bindings.VCLControls,
  ExtCtrls,
  StdCtrls;

type
  TCheckBoxHelper = class helper for TCheckBox
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TColorBoxHelper = class helper for TColorBox
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TComboBoxHelper = class helper for TComboBox
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TDateTimePickerHelper = class helper for TDateTimePicker
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TEditHelper = class helper for TEdit
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TLabelHelper = class helper for TLabel
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TListBoxHelper = class helper for TListBox
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TMemoHelper = class helper for TMemo
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TMonthCalendarHelper = class helper for TMonthCalendar
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TPanelHelper = class helper for TPanel
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TRadioButtonHelper = class helper for TRadioButton
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TRadioGroupHelper = class helper for TRadioGroup
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

  TTrackBarHelper = class helper for TTrackBar
  private
    function GetBinding: TBinding;
  public
    property Binding: TBinding read GetBinding;
  end;

implementation

uses
  Classes;

{ TColorBoxHelper }

function TColorBoxHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TCheckBoxHelper }

function TCheckBoxHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TComboBoxHelper }

function TComboBoxHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TDateTimePickerHelper }

function TDateTimePickerHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TEditHelper }

function TEditHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TLabelHelper }

function TLabelHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TListBoxHelper }

function TListBoxHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TMemoHelper }

function TMemoHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TMonthCalendarHelper }

function TMonthCalendarHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TPanelHelper }

function TPanelHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TRadioButtonHelper }

function TRadioButtonHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TRadioGroupHelper }

function TRadioGroupHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

{ TTrackBarHelper }

function TTrackBarHelper.GetBinding: TBinding;
begin
  Result := GetBindingForComponent(Self);
end;

end.
