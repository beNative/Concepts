unit uInterceptors;

interface

uses
  Spring.Interception;

type
  TLoggingAspect = class(TInterfacedObject, IInterceptor)
  public
    procedure Intercept(const invocation: IInvocation);
  end;

  TTransactionAspect = class(TInterfacedObject, IInterceptor)
  public
    procedure Intercept(const invocation: IInvocation);
  end;

implementation


{$REGION 'TLoggingAspect'}

procedure TLoggingAspect.Intercept(const invocation: IInvocation);
begin
  Writeln('Before ', invocation.Method.Name, '....');
  invocation.Proceed;
  Writeln('After ', invocation.Method.Name, '....');
end;

{$ENDREGION}


{$REGION 'TTransactionAspect'}

procedure TTransactionAspect.Intercept(const invocation: IInvocation);
begin
  Writeln('Starting transaction....');
  try
    invocation.Proceed;
    Writeln('Committing transaction....');
  except
    Writeln('Rollbacking transaction....');
  end;
end;

{$ENDREGION}


end.
