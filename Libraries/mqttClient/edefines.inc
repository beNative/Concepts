// ----------------------------------------------------------------------------
// file:         edefines.inc
// type:         include file
// compiler:     Free Pascal Compiler 2.2.5
// platforms:    Win32/Linux
// author:       al@mnppsaturn.ru
// description:  Глобальные определения
// ----------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$DEBUGINFO OFF}
{$IFDEF FPC}
 {$SMARTLINK ON}
 {$MODE OBJFPC}
{$ENDIF}

{$H+}
{$I-}

// поддержка работы через шлюз
{$DEFINE SupportUDP}
{$DEFINE UseH323}

// ----------------------------------------------------------------------------

{$IFNDEF LINUX}
 {$IFNDEF MSWINDOWS}
  'supported only WIN32 and LINUX !'
 {$ENDIF}
{$ELSE}
 {$IFNDEF FPC}
  'for LINUX supported only FPC compilator !'
 {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
 {$IFNDEF WIN32}
  'WIN16 not supported !'
 {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
 {$IFNDEF CPUARM}
  {$IFNDEF CPUI386}
   {$FATAL only intel386 and ARM supported}
  {$ENDIF}
 {$ENDIF}
 {$IFNDEF VER2}
  {$FATAL required FPC version >= 2.0.0 }
 {$ENDIF}
{$ELSE}
 {$IFNDEF CPU386}
  'non intel386 not supported !'
 {$ENDIF}
{$ENDIF}

// ----------------------------------------------------------------------------

{$IFDEF CPUARM}
 {$DEFINE DOUBLE_HILO_SWAPPED}
 {$DEFINE BKD_PC}
{$ENDIF}

{$IFDEF LINUX}
 {$DEFINE USE_IOCTL_IP}
{$ENDIF}

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
 {$DEFINE USE_UNALIGNED}                // {$IFDEF USE_UNALIGNED}unaligned{$ENDIF}()
{$ENDIF}

{$IFDEF FPC}
 {$IFNDEF VER2_0}
  {$DEFINE DBHANDLE_POINTER}
 {$ENDIF}
{$ENDIF}

