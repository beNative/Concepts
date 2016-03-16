{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Logger.Factories;

//{$I DDuce.inc}

interface

uses
  DDuce.Logger.Interfaces;

type
  TLoggerFactories = class sealed
  private
  end;

implementation

uses
  DDuce.Logger, DDuce.Logger.Channels.LogFile, DDuce.Logger.Channels.WinIPC,
  DDuce.Logger.Channels.ZeroMQ;

initialization
  //Logger := TLogger.Create;
//  Logger.Channels.Add(TWinIPCChannel.Create);
//  Logger.Channels.Add(TLogFileChannel.Create);

end.
