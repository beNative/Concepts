{******************************************************************************}
{                                                                              }
{  NATS.Delphi: Delphi Client Library for NATS                                 }
{  Copyright (c) 2022 Paolo Rossi                                              }
{  https://github.com/paolo-rossi/nats.delphi                                  }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit Nats.Monitor;

interface

uses
  System.SysUtils, System.SyncObjs, system.Classes, System.Generics.Collections,

  Nats.Classes;

type
  /// <summary>
  ///   Target resources need to implement INatsResource interface to be
  ///   monitored by TNatsMonitor
  /// </summary>
  INatsResource = interface
  ['{C49BC6AF-7852-4129-8639-5FA106813E4E}']
		procedure SendPing(AHandler: TNatsPingHandler);
		function GetResourceId: string;
		function IsConnected: Boolean;
  end;


  /// <summary>
  ///   Monitors a registered "Resource" by sending a PING message. It removes
  ///   the resource from the list when PING fails.
  /// </summary>
  TNatsMonitor = class(TNatsThread)
  private
    FResources: TDictionary<string, INatsResource>;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddResource(AId: string; AResource: INatsResource);
    procedure RemoveResource(AId: string);
  end;

implementation

{ TNatsMonitor }

procedure TNatsMonitor.AddResource(AId: string; AResource: INatsResource);
begin
  FResources.Add(AId, AResource);
end;

constructor TNatsMonitor.Create;
begin
  FResources := TDictionary<string, INatsResource>.Create;
end;

destructor TNatsMonitor.Destroy;
begin
  FResources.Free;
  inherited;
end;

procedure TNatsMonitor.Execute;
var
  LPair: TPair<string, INatsResource>;
begin
  while not Terminated do
  try
    FStopEvent.WaitFor(1000); //Ping Interval
    for LPair in FResources do
      if LPair.Value.IsConnected then
        LPair.Value.sendPing(nil);
  except


    {
      LOG.log(Level.WARNING, ioe.getMessage() + ", " + "Failed pinging resoure(" +
          resource.getResourceId() + ")");
      Subscription.removeSubscribers(resource.getResourceId());
      this.removeResource(resource.getResourceId());
    }

  end;
end;

procedure TNatsMonitor.RemoveResource(AId: string);
begin
  FResources.Remove(AId);
end;

end.
