{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Resources;

interface

const
// DDuce.Winipc.Client, DDuce.Winipc.Server
// old name maintained for backwards compatibility
  MSG_WND_CLASSNAME : PChar = 'FPCMsgWindowCls';
  SERVER_WINDOWNAME : PChar = 'ipc_log_server';

resourcestring
// DDuce.Winipc.Client, DDuce.Winipc.Server
  SServerNotActive             = 'Server with ID %s is not active.';
  SFailedToRegisterWindowClass = 'Failed to register message window class';
  SFailedToCreateWindow        = 'Failed to create message window %s';

// DDuce.DynamicRecord
  SFieldNotFound        = 'Record does not contain a field with name %s';
  SValueCannotBeRead    = 'Value of %s could not be read';
  SValueConversionError =
    'Error while trying to convert value of (%s) with type (%s)';
  SParamIsNotRecordOrInstanceType =
    'AInstance is not a record or instance type!';
  SPropertyNotFound = 'Property %s not found! (%s)';
  SArgumentTypeNotSupported =
    'The argument type of AssignTo is not supported';

implementation

end.
