{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Persistence.Core.Exceptions;

interface

uses
  SysUtils,
  Spring;

type
  /// <summary>
  ///   Base class for all ORM related exceptions. Cannot be instantiated
  ///   directly.
  /// </summary>
  EORMException = class abstract(Exception)
{$IFDEF AUTOREFCOUNT}
  protected
    procedure RaisingException(P: PExceptionRecord); override;
{$ENDIF}
  end;

  EBaseORMException = class(EORMException)
  protected
    function EntityToString(const entity: TObject): string; virtual;
  public
    constructor Create(const entity: TObject); reintroduce; overload;
  end;

  EEntityAlreadyPersisted = class(EBaseORMException);

  ECannotPersististEntityWithId = class(EBaseORMException);

  ETableNotSpecified = class(EBaseORMException);

  EORMMethodNotImplemented = class(EORMException);

  EUnknownMember = class(EORMException);

  EORMEnumException = class(EORMException);

  EEntityManagerNotSet = class(EORMException);

  EUnknownJoinType = class(EORMException);

  EORMRecordNotFoundException = class(EORMException);

  EORMUpdateNotSuccessfulException = class(EBaseORMException);

  EORMColumnCannotBeNull = class(EBaseORMException);

  EORMColumnNotFound = class(EBaseORMException);
  EORMPrimaryKeyColumnNotFound = class(EBaseORMException);

  EORMInvalidConversion = class(EBaseORMException);

  EORMContainerDoesNotHaveAddMethod = class(EORMException);

  EORMContainerDoesNotHaveClearMethod = class(EORMException);

  EORMContainerDoesNotHaveCountMethod = class(EORMException);

  EORMContainerAddMustHaveOneParameter = class(EORMException);

  EORMContainerItemTypeNotSupported = class(EORMException);

  EORMUnsupportedType = class(EORMException);

  EORMUnsupportedOperation = class(EORMException);

  EORMConnectionAlreadyRegistered = class(EORMException);
  EORMRowMapperAlreadyRegistered = class(EORMException);
  EORMConnectionNotRegistered = class(EORMException);

  EORMManyToOneMappedByColumnNotFound = class(EORMException);

  EORMTransactionNotStarted = class(EORMException);

  EORMListInSession = class(EORMException);

  EORMCannotConvertValue = class(EORMException);

  EORMInvalidArguments = class(EORMException);

  EORMOptimisticLockException = class(EBaseORMException);

  EORMCannotGenerateQueryStatement = class(EBaseORMException);

  /// <summary>
  ///   Base class for all adapter related exceptions. Ie. exceptions that may
  ///   occur when calling directly into the DB driver (ie. outside ORM scope).
  ///   Unless generic failure exception is encountered, adapter functions may
  ///   not raise any other exception that doesn't descent from this base
  ///   class.
  /// </summary>
  /// <remarks>
  ///   Additionally optional <c>Code</c> property has been added that may be
  ///   used by any descendant which can provide such information.
  /// </remarks>
  EORMAdapterException = class abstract(EORMException)
  strict private
    fErrorCode: Nullable<Integer>;
  public
    constructor Create(const msg: string); overload;
    constructor Create(const msg: string; errorCode: Integer); overload;
    constructor CreateFmt(const msg: string; const args: array of const); overload;
    constructor CreateFmt(const msg: string; const args: array of const; errorCode: Integer); overload;

    /// <summary>
    ///   Error code of the operation (optional).
    /// </summary>
    /// <remarks>
    ///   Driver dependent
    /// </remarks>
    property ErrorCode: Nullable<Integer> read fErrorCode;
  end;

  /// <summary>
  ///   Constraint validation exception such as duplicate primary key or unique
  ///   column.
  /// </summary>
  /// <remarks>
  ///   May not be supported by all adapters!
  /// </remarks>
  EORMConstraintException = class(EORMAdapterException);

  TORMAdapterExceptionClass = class of EORMAdapterException;

  /// <summary>
  ///   Used by adapaters to obtain concrete exception from driver
  ///   (ADO, DBX, ...) specific information.
  /// </summary>
  IORMExceptionHandler = interface
    ['{2ACF3197-867F-49FB-9E37-0A35742D66B0}']
    /// <summary>
    ///   Returns specific exception instance for given (driver specific)
    ///   exception (mostly its message or code is used) raised as outer or
    ///   default exception that the caller must raise.
    /// </summary>
    function HandleException(const defaultMsg: string): Exception;
  end;

  /// <summary>
  ///   Exception helper class that handles some basic checks then delegates to
  ///   provider-specific implementation.
  /// </summary>
  TORMExceptionHandler = class abstract(TInterfacedObject, IORMExceptionHandler)
  protected
    /// <summary>
    ///   Returns specific exception instance for given (adapter/driver
    ///   specific) exception or <c>nil</c> if given exception is not adapter
    ///   specific and is considered a generic failure in which case the
    ///   original exception is propagated. The returned instance is raised as
    ///   outer exception.
    /// </summary>
    /// <remarks>
    ///   If the DB driver is implemented correctly, it should either raise
    ///   specific exception for specific failure or more generic exception
    ///   with a specific error code set . Unfortunately this is not true for
    ///   all drivers so only generic error may be raised and only the
    ///   exception message differentiates the errors (which are driver
    ///   dependent).
    /// </remarks>
    function GetAdapterException(const exc: Exception;
      const defaultMsg: string): Exception; virtual; abstract;
  public
    /// <summary>
    ///   Handles current exception and converts it to ORM specific one.
    /// </summary>
    /// <param name="inst">
    ///   Source instance of the exception, if adapter specific exception is
    ///   encountered, it may be used to raise driver specific exception that
    ///   cannot be handled generally.
    /// </param>
    /// <param name="defaultMsg">
    ///   Default message to use
    /// </param>
    function HandleException(const defaultMsg: string = ''): Exception;
  end;


implementation


{$REGION 'EORMException'}

{$IFDEF AUTOREFCOUNT}
procedure EORMException.RaisingException(P: PExceptionRecord);
begin
  inherited RaisingException(P);
  // fixes AcquireExceptionObject ARC issue (RSP-13652)
  if Assigned(InnerException) then
    InnerException.__ObjRelease;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'EBaseORMException'}

constructor EBaseORMException.Create(const entity: TObject);
begin
  inherited Create(EntityToString(entity));
end;

function EBaseORMException.EntityToString(const entity: TObject): string;
begin
  if not Assigned(entity) then
    Result := 'null'
  else
    Result := 'ClassName: ' + entity.ClassName;
end;

{$ENDREGION}


{$REGION 'EORMAdapterException'}

constructor EORMAdapterException.Create(const msg: string);
begin
  inherited Create(msg);
end;

constructor EORMAdapterException.Create(const msg: string; errorCode: Integer);
begin
  Create(msg);
  fErrorCode := errorCode;
end;

constructor EORMAdapterException.CreateFmt(const msg: string;
  const args: array of const);
begin
  inherited CreateFmt(msg, args);
end;

constructor EORMAdapterException.CreateFmt(const msg: string;
  const args: array of const; errorCode: Integer);
begin
  CreateFmt(msg, args);
  fErrorCode := errorCode;
end;

{$ENDREGION}


{$REGION 'TORMExceptionHandler'}

type
  ExceptionHelper = class helper for Exception
    procedure SetAcquireInnerException;
  end;

  ExceptionHack = class(TObject)
  protected
    FMessage: string;
    FHelpContext: Integer;
    FInnerException: Exception;
    FStackInfo: Pointer;
    FAcquireInnerException: Boolean;
  end;

procedure ExceptionHelper.SetAcquireInnerException;
begin
  ExceptionHack(Self).FAcquireInnerException := True;
end;

function TORMExceptionHandler.HandleException(const defaultMsg: string): Exception;

{$IFNDEF DELPHIXTOKYO_UP}
{$IFDEF AUTOREFCOUNT}
  function AcquireExceptionObject: Exception; // fixes RSP-13652
  begin
    Pointer(Result) := System.AcquireExceptionObject;
  end;
{$ENDIF}
{$ENDIF}

var
  exc: TObject;
begin
  exc := ExceptObject;
  // If ORM exception is encountered just propagate it
  if exc is EORMException then
    // Revive the exception object so it doesn't get destroyed when the except
    // block ends.
    Exit(Exception(AcquireExceptionObject));

  if exc is Exception then
  begin
    if defaultMsg = '' then
      Result := GetAdapterException(Exception(exc), Exception(exc).Message)
    else
      Result := GetAdapterException(Exception(exc), defaultMsg);
  end
  else
    // Any TObject may be risen, GetAdapterException doesn't deal with that
    Result := nil;

  if not Assigned(Result) then
    // Non-handled exceptions are just propageted and treated as system-failuers
    // that ORM-specific except blocks should not handle (this includes EAbort).
    // Revive the exception object so it doesn't get destroyed when the except
    // block ends.
    Exit(Exception(AcquireExceptionObject));

  // New exception will acquire inner exception and thus will be raised as outer
  // but with standard `raise` keyword later without the need of calling
  // `Exception.RaiseOuterException`, this will revive the exception object.
  Result.SetAcquireInnerException;
end;

{$ENDREGION}


end.
