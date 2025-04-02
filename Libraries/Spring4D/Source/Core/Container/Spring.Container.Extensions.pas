{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
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

unit Spring.Container.Extensions;

interface

uses
  Spring.Container.Core;

type
  /// <summary>
  ///   Base class for all <see cref="TContainer" /> extension objects.
  /// </summary>
  TContainerExtension = class(TInterfacedObject, IContainerExtension)
  private
    fKernel: TKernel;
  protected
    /// <summary>
    ///   Initial the container with this extension's functionality.
    /// </summary>
    /// <remarks>
    ///   When overridden in a derived class, this method will modify the given
    ///   Kernel to install its functions into the container.
    /// </remarks>
    procedure Initialize; virtual; abstract;

    /// <summary>
    ///   The container calls this method when the extension is added.
    /// </summary>
    /// <param name="kernel">
    ///   An <see cref="TKernel" /> instance that gives the extension access to
    ///   the internals of the container.
    /// </param>
    procedure InitializeExtension(const kernel: TKernel);

    /// <summary>
    ///   The TKernel instance used to manipulate the inner state of the
    ///   container.
    /// </summary>
    property Kernel: TKernel read fKernel;
  end;

implementation


{$REGION 'TContainerExtension'}

procedure TContainerExtension.InitializeExtension(const kernel: TKernel);
begin
  fKernel := kernel;
end;

{$ENDREGION}


end.
