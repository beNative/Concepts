{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.Types;

interface

uses
  System.Classes, System.SysUtils;

type
  TSortDirection = (
    sdAscending,
    sdDescending
  );

  TSortScope = (
    ssWords,
    ssLines,
    ssParagraphs
  );
  TAlignToToken = (
    atLeftMost,
    atRightMost
  );
  // event types
  TCaretPositionEvent = procedure(
    Sender : TObject;
    X, Y   : Integer
  ) of object;

  TActionExecuteEvent = procedure(
    Sender       : TObject;
    AAction      : TBasicAction;
    var AHandled : Boolean
  ) of object;

  TStatusMessageEvent = procedure(
    Sender : TObject;
    Text   : string
  ) of object;

  TStorageEvent = procedure(
    Sender    : TObject;
    var AName : string
  ) of object;

  TNewEvent = procedure(
    Sender      : TObject;
    var AName   : string;
    const AText : string
  ) of object;

  TOpenOtherInstanceEvent = procedure(
    Sender        : TObject;
    const AParams : array of string
  ) of object;

implementation

end.
