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

{$IFNDEF DDUCE_INC}
{$DEFINE DDUCE_INC}

{$I jedi.inc}                // Import JEDI Development environment directives

{$IFDEF DELPHIXE8_UP}
  {$DEFINE HAS_UNIT_SYSTEM_IMAGELIST}
{$ENDIF}

{$IFNDEF DELPHIXE2_UP}
  {$MESSAGE ERROR 'This library requires Delphi XE2 or higher.'}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$MESSAGE ERROR 'This library currently only supports Windows.'}
{$ENDIF}

{$WARN UNIT_PLATFORM OFF}    // Turn off UNIT_PLATFORM warning
{$WARN SYMBOL_PLATFORM OFF}  // Turn off SYMBOL_PLATFORM warning

{$ENDIF ~DDUCE_INC}
