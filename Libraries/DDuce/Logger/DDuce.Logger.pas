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

{$I .\..\DDuce.inc}

unit DDuce.Logger;

interface

{ A global logger singleton for shared usage. }

uses
  DDuce.Logger.Interfaces;

function Logger(const ALogLevel: Byte = 0): ILogger;

implementation

uses
  DDuce.Logger.Base;

var
  FLogger: ILogger;

function Logger(const ALogLevel: Byte): ILogger;
begin
  if Assigned(FLogger) then
    FLogger.LogLevel := ALogLevel;
  Result := FLogger;
end;

initialization
  FLogger := TLogger.Create;

end.
