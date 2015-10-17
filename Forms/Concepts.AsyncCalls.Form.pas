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

unit Concepts.AsyncCalls.Form;

interface

uses
  System.Actions, System.Classes, System.ImageList,
  Vcl.Forms, Vcl.ImgList, Vcl.Controls, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.ComCtrls,

  AsyncCalls;

{ Remark: The AsyncCalls library is not supported on x64 and is not
  maintained anymore. }

type
  TfrmAsyncCalls = class(TForm)
    sbrMain     : TStatusBar;
    aclMain     : TActionList;
    imlMain     : TImageList;
    actGetFiles : TAction;
    mmoFiles    : TMemo;
    btnGetFiles : TButton;
    mmoFiles2   : TMemo;
    mmoFiles3   : TMemo;

    procedure actGetFilesExecute(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  WinApi.Windows,
  System.SysUtils;

{$REGION 'non-interfaced routines'}
{ The cdecl function GetFiles() has two arguments, a string and an object which
  are declared like normal arguments. }
procedure GetFiles(const Directory: string; Filenames: TStrings); cdecl;
var
  h: THandle;
  FindData: TWin32FindData;
begin
  h := FindFirstFile(PChar(Directory + '\*.*'), FindData);
  if h <> INVALID_HANDLE_VALUE then
  begin
    repeat
      if (StrComp(FindData.cFileName, '.') <> 0)
        and (StrComp(FindData.cFileName, '..') <> 0) then
      begin
        Filenames.Add(Directory + '\' + FindData.cFileName);
        if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
          GetFiles(Filenames[Filenames.Count - 1], Filenames);
      end;
    until not FindNextFile(h, FindData);
    WinApi.Windows.FindClose(h);
  end;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmAsyncCalls.actGetFilesExecute(Sender: TObject);
 var
  Dir1, Dir2, Dir3: IAsyncCall;
  Dir1Files, Dir2Files, Dir3Files: TStrings;
begin
  Dir1Files := TStringList.Create;
  Dir2Files := TStringList.Create;
  Dir3Files := TStringList.Create;
  try
    { Call the cdecl function GetFiles() with two arguments, a string and an object. }
    Dir1 := AsyncCall(@GetFiles, ['C:\Windows', Dir1Files]);
    { Call the cdecl function GetFiles() with two arguments, a string and an object. }
    Dir2 := AsyncCall(@GetFiles, ['c:\Tools', Dir2Files]);
    { Call the cdecl function GetFiles() with two arguments, a string and an object. }
    Dir3 := AsyncCall(@GetFiles, ['C:\Program Files', Dir3Files]);

    { Wait until all async functions have finished their work. While waiting make the UI reacting on user interaction. }
    while AsyncMultiSync([Dir1, Dir2, Dir3], True, 10) = WAIT_TIMEOUT do
    begin
      sbrMain.Panels[0].Text := IntToStr(Dir1Files.Count);
      sbrMain.Panels[1].Text := IntToStr(Dir2Files.Count);
      sbrMain.Panels[2].Text := IntToStr(Dir3Files.Count);
      Application.ProcessMessages;
    end;
    //Dir1.Sync; // Force the Dir3 function to finish here

    mmoFiles.Lines.Add(Dir1Files[0]);
    mmoFiles2.Lines.Add(Dir2Files[0]);
    mmoFiles3.Lines.Add(Dir3Files[0]);
    sbrMain.Panels[0].Text := IntToStr(Dir1Files.Count);
    sbrMain.Panels[1].Text := IntToStr(Dir2Files.Count);
    sbrMain.Panels[2].Text := IntToStr(Dir3Files.Count);
  finally
    Dir3Files.Free;
    Dir2Files.Free;
    Dir1Files.Free;
  end;
end;
{$ENDREGION}

end.
