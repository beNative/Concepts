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

unit DDuce.Logger.Channels.ZeroMQ;

//{$I DDuce.inc}

interface

uses
  DDuce.Logger.Interfaces, DDuce.Logger.Channels.Base;

type
  TZeroMQChannel = class(TCustomLogChannel)
  strict private
    //FClient       : TWinIPCClient;
    //FBuffer       : TMemoryStream;
    //FClearMessage : TLogMessage;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Clear; override;
    procedure Write(const AMsg: TLogMessage); override;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TZeroMQChannel.AfterConstruction;
begin
  inherited AfterConstruction;
//
end;

procedure TZeroMQChannel.BeforeDestruction;
begin
  inherited BeforeDestruction;
//
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TZeroMQChannel.Clear;
begin
  inherited Clear;
//
end;

procedure TZeroMQChannel.Write(const AMsg: TLogMessage);
begin
  inherited Write(AMsg);
//
end;
{$ENDREGION}

end.
