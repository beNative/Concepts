(*
  Copyright (c) 2011-2012, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Core.Framework;

interface

uses
  Classes;

type
  TFramework = class;

  TFrameworkClass = class of TFramework;

  TFramework = class
  private
    class var FFrameworkType: TFrameworkClass;
  protected
    class function DoGetChildren(Element: TComponent): TArray<TComponent>; virtual;
    class function DoGetParent(Element: TComponent): TComponent; virtual;
    class procedure DoNotifyPropertyChanged(Element: TComponent;
      const PropertyName: string); virtual;
  public
    class constructor Create;

    class function GetChildren(Element: TComponent): TArray<TComponent>;
    class function GetParent(Element: TComponent): TComponent;
    class procedure NotifyPropertyChanged(Element: TComponent; const PropertyName: string);

    class procedure Initialize;
  end;

implementation

{ TFramework }

class constructor TFramework.Create;
begin
  Initialize();
end;

class function TFramework.DoGetChildren(Element: TComponent): TArray<TComponent>;
begin
  Result := nil;
end;

class function TFramework.DoGetParent(Element: TComponent): TComponent;
begin
  Result := nil;
end;

class procedure TFramework.DoNotifyPropertyChanged(Element: TComponent;
  const PropertyName: string);
begin

end;

class function TFramework.GetChildren(Element: TComponent): TArray<TComponent>;
begin
  Result := FFrameworkType.DoGetChildren(Element);
end;

class function TFramework.GetParent(Element: TComponent): TComponent;
begin
  Result := FFrameworkType.DoGetParent(Element);
end;

class procedure TFramework.Initialize;
begin
  FFrameworkType := Self;
end;

class procedure TFramework.NotifyPropertyChanged(Element: TComponent;
  const PropertyName: string);
begin
  FFrameworkType.DoNotifyPropertyChanged(Element, PropertyName);
end;

end.
