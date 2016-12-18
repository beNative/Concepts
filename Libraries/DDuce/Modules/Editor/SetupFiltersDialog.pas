{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SetupFiltersDialog.pas

The Initial Developer of the Original Code is: AD <adsoft@nm.ru>
Copyright (c) 2005 ADSoft          
All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SetupFiltersDialog.pas,v 1.1.1.1 2005/10/25 11:38:03 adsoft Exp $
-------------------------------------------------------------------------------}

unit SetupFiltersDialog;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages,

  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls{, ADFileTypes};

{
object FT: TADFileTypes
    Items = <
      item
        Name = 'Default'
        AutoIndent = True
        UseTab = True
        SmartTab = False
        TabWidth = 4
        Highlighter = 'General'
        Filter = 'Any Files (*.*)|*.*'
        Comment = 'rem '
      end>
  end
}

type
  TdlgSetupFilters = class(TForm)
    LB: TListBox;
    bnOK: TButton;
    bnCancel: TButton;
    //FT: TADFileTypes;
    procedure FormShow(Sender: TObject);
    procedure LBDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgSetupFilters: TdlgSetupFilters;

implementation

{$R *.lfm}

procedure TdlgSetupFilters.FormShow(Sender: TObject);
//var
//  i: integer;
begin
  //for i := 1 to FT.Items.Count - 1 do
  //  LB.AddItem(FT.Items[i].Name, nil);
end;

procedure TdlgSetupFilters.LBDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Item, i: integer;
begin
  Item := LB.ItemAtPos(Point(X, Y), True);
  i := 0;
  while i <= LB.Items.Count - 1 do
  begin
    if (LB.Selected[i]) and (i <> Item) then
      LB.Items.Move(i, Item)
    else
      inc(i);
  end;
end;

procedure TdlgSetupFilters.LBDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = Source;
end;

procedure TdlgSetupFilters.FormHide(Sender: TObject);
//var
//  i, j: integer;
begin
  //if ModalResult = mrOK then
  //  for i := 0 to LB.Items.Count - 1 do
  //  begin
  //    j := FT.IndexOf(LB.Items[i]);
  //    FT.Items[j].Index := i + 1;
  //  end;
end;

end.
