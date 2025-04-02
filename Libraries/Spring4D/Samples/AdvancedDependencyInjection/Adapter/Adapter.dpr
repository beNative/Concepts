program Adapter;

{$APPTYPE CONSOLE}

uses
  Spring,
  Spring.Container,
  System.SysUtils;

type
  ICommand = interface
    ['{570FE027-09A1-4BE9-BFDF-A0E6EC41460C}']
  end;

  TSaveCommand = class(TInterfacedObject, ICommand)
  end;

  TOpenCommand = class(TInterfacedObject, ICommand)
  end;

  TToolButton = class
  private
    fCommand: ICommand;
  public
    constructor Create(const command: ICommand);
    property Command: ICommand read fCommand;
  end;

{ TToolButton }

constructor TToolButton.Create(const command: ICommand);
begin
  fCommand := command;
end;

procedure Main;
var
  buttons: TArray<TToolButton>;
begin
  GlobalContainer.RegisterType<ICommand,TSaveCommand>('save');
  GlobalContainer.RegisterType<ICommand,TOpenCommand>('open');
  GlobalContainer.RegisterType<TToolButton>;
  GlobalContainer.RegisterType<TArray<TToolButton>>(
    function: TArray<TToolButton>
    var
      commands: TArray<ICommand>;
      i: Integer;
    begin
      commands := GlobalContainer.ResolveAll<ICommand>;
      SetLength(Result, Length(commands));
      for i := 0 to High(commands) do
        Result[i] := GlobalContainer.Resolve<TToolButton>([TValue.From(commands[i])]);
    end);
  GlobalContainer.Build;

  buttons := GlobalContainer.Resolve<TArray<TToolButton>>;
  Assert(Length(buttons) = 2);
  Assert(buttons[0].Command is TSaveCommand);
  Assert(buttons[1].Command is TOpenCommand);
end;

begin
  Main;
end.
