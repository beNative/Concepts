
{$I jedi.inc}                // Import JEDI Development environment directives
{$IFNDEF DDUCE_INC}
{$DEFINE DDUCE_INC}


{$DEFINE SPRING}
{$DEFINE DSHARP}

{$IFNDEF DELPHIXE2_UP}
  {$MESSAGE ERROR 'This library requires Delphi XE2 or higher.'}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$MESSAGE ERROR 'This library currently only supports Windows.'}
{$ENDIF}

{$WARN UNIT_PLATFORM OFF}    // Turn off UNIT_PLATFORM warning
{$WARN SYMBOL_PLATFORM OFF}  // Turn off SYMBOL_PLATFORM warning

{$ENDIF ~DDUCE_INC}
