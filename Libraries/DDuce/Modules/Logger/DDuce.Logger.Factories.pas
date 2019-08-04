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

{$I DDuce.inc}

unit DDuce.Logger.Factories;

interface

uses
  DDuce.Logger.Interfaces;

type
  TLoggerFactories = class sealed
    class function CreateLogger: ILogger;
    class function CreateWinIPCChannel: IWinIPCChannel;
    class function CreateZeroMQChannel: IZeroMQChannel;
    class function CreateMQTTChannel: IMQTTChannel;
    class function CreateLogFileChannel(AFileName: string = ''): ILogFileChannel;

  end;

implementation

uses
  DDuce.Logger.Base, DDuce.Logger.Channels.WinIPC, DDuce.Logger.Channels.ZeroMQ,
  {DDuce.Logger.Channels.MQTT,} DDuce.Logger.Channels.LogFile;

class function TLoggerFactories.CreateLogger: ILogger;
begin
  Result := TLogger.Create;
end;

class function TLoggerFactories.CreateMQTTChannel: IMQTTChannel;
begin
//  Result := TMQTTChannel.Create;
end;

class function TLoggerFactories.CreateWinIPCChannel: IWinIPCChannel;
begin
  Result := TWinIPCChannel.Create;
end;

class function TLoggerFactories.CreateZeroMQChannel: IZeroMQChannel;
begin
  Result := TZeroMQChannel.Create;
end;

class function TLoggerFactories.CreateLogFileChannel(AFileName: string)
  : ILogFileChannel;
begin
  Result := TLogFileChannel.Create(AFileName);
end;

end.
