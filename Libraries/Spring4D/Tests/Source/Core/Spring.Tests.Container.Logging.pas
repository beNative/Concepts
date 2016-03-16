{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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

unit Spring.Tests.Container.Logging;

{$I Spring.inc}
{$I Spring.Tests.inc}

interface

uses
  Classes,
  Rtti,
  StrUtils,
  SysUtils,
  TestFramework,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Container,
  Spring.Container.Common,
  Spring.Logging,
  Spring.Logging.Appenders,
  Spring.Logging.Configuration,
  Spring.Logging.Configuration.Builder,
  Spring.Logging.Container,
  Spring.Logging.Controller,
  Spring.Logging.Extensions,
  Spring.Logging.Loggers,
  Spring.Reflection,
  Spring.Tests.Container,
  Spring.Tests.Logging.Types;

type
  {$REGION 'TTestLogInsideContainer'}
  TTestLogInsideContainer = class(TContainerTestCase)
  published
    procedure TestLog;
    procedure TestChainedControllers;
  end;
  {$ENDREGION}


  {$REGION 'TTestLogSubResolverAndConfiguration'}
  TTestLogSubResolverAndConfiguration = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestNoInject;
    procedure TestClass;
    procedure TestInterface;
    //procedure TestRecord;
    procedure TestBaseClass;
    procedure TestConstructor;
    procedure TestMethod;
    procedure TestLazy;
  end;
  {$ENDREGION}


  {$REGION 'TTestLoggingConfiguration'}
  TTestLoggingConfiguration = class(TContainerTestCase)
  private
    fStrings: TStrings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInjectMultipleAppendersToSingleController;
    procedure TestLeak;
    procedure TestMultipleConfiguration;
    procedure TestDuplicateDefault;
    procedure TestUnknownClass;
    procedure TestUnknownProperty;
    procedure TestUnknownPropertyKind;
    procedure TestNonInstanceType;

    procedure TestReadProperties;
    procedure TestReadAppenders;
    procedure TestDefaultController;
    procedure TestReadController;
    procedure TestReadSingleControllerAsDefault;
    procedure TestDefaultLogger;
    procedure TestReadLogger;

    procedure TestAddAppendersToControllers;
    procedure TestAddChainedController;
    procedure TestAddSerializersToControllers;
    procedure TestAddLoggerAssignments;

    procedure TestSimpleConfiguration;
    procedure TestComplexConfiguration;
    procedure Test_LoadFromStrings_Ensures_Container_Resolve_CanBeFreedWithoutErrors;
  end;
  {$ENDREGION}


  {$REGION 'TTestLoggingConfigurationBuilder'}
  TTestLoggingConfigurationBuilder = class(TContainerTestCase)
  private const
    NL = sLineBreak;
  protected
    procedure SetUp; override;
  published
    procedure TestEmpty;

    procedure TestAppender;
    procedure TestController;
    procedure TestLogger;

    procedure TestComplexConfiguration;
  end;
  {$ENDREGION}


implementation

uses
  Spring.TestUtils;


{$REGION 'Internal test helpers'}

type
  TStringsHelper = class helper for TStrings
    function Add(const s: string): TStrings;
  end;

function TStringsHelper.Add(const s: string): TStrings;
begin
  TStringList(Self).Add(s);
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TTestLogInsideContainer'}

procedure TTestLogInsideContainer.TestChainedControllers;
begin
  fContainer.RegisterType<ILogger, TLogger>.AsSingleton.AsDefault;
  fContainer.RegisterType<TLogger>.AsSingleton
    .Implements<ILogger>('l2').InjectConstructor(['c2.ctl']);
  fContainer.RegisterType<ILoggerController, TLoggerController>.AsSingleton;
  // acts as appender and controller together
  fContainer.RegisterType<TLoggerController>.AsSingleton
    .Implements<ILogAppender>('c2').Implements<ILoggerController>('c2.ctl')
    .InjectConstructor;

  fContainer.Build;

  fContainer.Resolve<Ilogger>;
  fContainer.Resolve<Ilogger>('l2');

  Pass;
end;

procedure TTestLogInsideContainer.TestLog;
var
  stream: TStringStream;
begin
  fContainer.RegisterType<ILogger, TLogger>.AsSingleton;
  fContainer.RegisterType<ILoggerController, TLoggerController>.AsSingleton;

  fContainer.Build;
  stream := TStringStream.Create;
  fContainer.Resolve<ILoggerController>.AddAppender(
    TStreamLogAppender.Create(stream));
  fContainer.Resolve<ILogger>.Warn('Test');
  CheckTrue(EndsStr('[WARN ] Test' + sLineBreak, stream.DataString));
end;

{$ENDREGION}


{$REGION 'TTestLogSubResolverAndConfiguration'}

procedure TTestLogSubResolverAndConfiguration.SetUp;
begin
  inherited;
  fContainer.Kernel.Resolver.AddSubResolver(
    TLoggerResolver.Create(fContainer.Kernel));
  fContainer.RegisterType<ILoggerController, TLoggerController>.AsSingleton;
  fContainer.RegisterType<TLoggingConfiguration>
    .Implements<TLoggingConfiguration>.AsSingleton;

  //And register some loggers that we may use
  fContainer.RegisterType<ILogger, TLoggerDefault>.AsSingleton.AsDefault;
  fContainer.RegisterType<TLogger1>.AsSingleton.Implements<ILogger>('logging.logger1');
  fContainer.RegisterType<TLogger2>.AsSingleton.Implements<ILogger>('logging.logger2');
end;

procedure TTestLogSubResolverAndConfiguration.TestBaseClass;
var
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObject>('logging.logger1');

  obj := fContainer.Resolve<TImpl>;

  CheckIs(obj.Logger1, TLogger1);
  CheckIs(obj.Logger2, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestClass;
var
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TImpl>('logging.logger1');

  obj := fContainer.Resolve<TImpl>;

  CheckIs(obj.Logger1, TLogger1);
  CheckIs(obj.Logger2, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestConstructor;
var
  obj: TObjCtor;
begin
  fContainer.RegisterType<TObjCtor>.Implements<TObjCtor>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObjCtor>('logging.logger1');

  obj := fContainer.Resolve<TObjCtor>;

  CheckIs(obj.Logger, TLogger1);
end;

procedure TTestLogSubResolverAndConfiguration.TestInterface;
var
  intf: IService;
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TImpl>('logging.logger1');

  intf := fContainer.Resolve<TImpl>;
  obj := TObject(intf) as TImpl;

  CheckIs(obj.Logger1, TLogger1);
  CheckIs(obj.Logger2, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestLazy;
var
  obj: TObjLazy;
begin
  fContainer.RegisterType<TObjLazy>.Implements<TObjLazy>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObjLazy>('logging.logger1');

  obj := fContainer.Resolve<TObjLazy>;

  CheckIs(obj.Logger1.Value, TLogger1);
  CheckIs(obj.Logger2.Value, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestMethod;
var
  obj: TObjProc;
begin
  fContainer.RegisterType<TObjProc>.Implements<TObjProc>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObjProc>('logging.logger1');

  obj := fContainer.Resolve<TObjProc>;

  CheckIs(obj.Logger, TLogger1);
end;

procedure TTestLogSubResolverAndConfiguration.TestNoInject;
var
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;

  obj := fContainer.Resolve<TImpl>;

  CheckIs(obj.Logger1, TLoggerDefault);
  CheckIs(obj.Logger2, TLogger2);
end;

{$ENDREGION}


{$REGION 'TTestLoggingConfiguration'}

procedure TTestLoggingConfiguration.SetUp;
begin
  inherited;
  fStrings := TStringList.Create;
end;

procedure TTestLoggingConfiguration.TearDown;
begin
  fStrings.Free;
  inherited;
end;

procedure TTestLoggingConfiguration.TestLeak;
begin
  //If done incorrectly this test will create a leak
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  Pass;
end;

procedure TTestLoggingConfiguration.TestMultipleConfiguration;
begin
  fContainer.RegisterType<TLoggingConfiguration>
    .Implements<TLoggingConfiguration>;
  ExpectedException := ERegistrationException;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestNonInstanceType;
var
  rec: TSomeRecord; //Reference the type
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TSomeRecord');
  ExpectedException := EClassNotFound;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  rec := Default(TSomeRecord);
end;

procedure TTestLoggingConfiguration.TestReadAppenders;
var
  i: ILogAppender;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  fStrings
    .Add('[appenders\default]')
    .Add('class = TAppenderMock2'); //Tets non-fully qualified name
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  i := fContainer.Resolve<ILogAppender>('logging.appender1.appender');
  CheckIs(TObject(i), TAppenderMock);
  i := fContainer.Resolve<ILogAppender>;
  CheckIs(TObject(i), TAppenderMock2);
end;

procedure TTestLoggingConfiguration.TestReadController;
var
  i: ILoggerController;
  a: ILogAppender;
begin
  fStrings
    .Add('[controllers\ctl1]')
    .Add('class = Spring.Tests.Logging.Types.TLoggerControllerMock');
  fStrings
    .Add('[controllers\default]');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  i := fContainer.Resolve<ILoggerController>('logging.ctl1.controller');
  CheckIs(TObject(i), TLoggerControllerMock);
  a := fContainer.Resolve<ILogAppender>('logging.ctl1.appender');
  CheckSame(TObject(i), TObject(a));
  i := fContainer.Resolve<ILoggerController>;
  CheckIs(TObject(i), TLoggerController);
end;

procedure TTestLoggingConfiguration.TestReadLogger;
var
  i: ILogger;
begin
  fStrings
    .Add('[loggers\log1]')
    .Add('class = Spring.Tests.Logging.Types.TLogger1');
  fStrings
    .Add('[loggers\default]');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  i := fContainer.Resolve<ILogger>('logging.log1');
  CheckIs(TObject(i), TLogger1);
  i := fContainer.Resolve<ILogger>;
  CheckIs(TObject(i), TLogger);
end;

procedure TTestLoggingConfiguration.TestReadProperties;
var
  i: ILogAppender;
  appender: TAppenderMock;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock')
    .Add('enabled = False')
    .Add('someInt = 1')
    .Add('someString = test')
    .Add('someEnum = Warn')
    .Add('levels = Error, Info')
    .Add('entryTypes = Text, Entering');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;
  i := fContainer.Resolve<ILogAppender>;

  CheckIs(TObject(i), TAppenderMock);
  appender := TAppenderMock(i);
  CheckFalse(appender.Enabled);
  CheckEquals(1, appender.SomeInt);
  CheckEquals('test', appender.SomeString);
  CheckEquals(Ord(TLogLevel.Warn), Ord(appender.SomeEnum));
  Check([TLogLevel.Error, TLogLevel.Info] = appender.Levels);
  Check([TLogEntryType.Text, TLogEntryType.Entering] = appender.EntryTypes);
end;

procedure TTestLoggingConfiguration.TestReadSingleControllerAsDefault;
var
  i: ILoggerController;
begin
  fStrings
    .Add('[controllers\ctl1]')
    .Add('class = Spring.Tests.Logging.Types.TLoggerControllerMock');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  i := fContainer.Resolve<ILoggerController>;
  CheckIs(TObject(i), TLoggerControllerMock);
end;

procedure TTestLoggingConfiguration.TestSimpleConfiguration;
var
  appender: TAppenderMock;
  o: TObjCtor;
begin
  fStrings
    .Add('[appenders\1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.RegisterType<TObjCtor>.Implements<TObjCtor>.AsSingleton;

  fContainer.Build;

  o := fContainer.Resolve<TObjCtor>;
  o.Logger.Fatal('test');

  appender := TObject(fContainer.Resolve<ILogAppender>) as TAppenderMock;
  CheckTrue(appender.WriteCalled);
  CheckEquals('test', appender.Entry.Msg);
end;

procedure TTestLoggingConfiguration.TestUnknownClass;
begin
  fStrings
    .Add('[appenders\default]')
    .Add('class = NotExistent');
  ExpectedException := EClassNotFound;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestUnknownProperty;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock')
    .Add('notExistent = False');
  ExpectedException := EPropertyError;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestUnknownPropertyKind;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock')
    .Add('someFloat = 0');
  ExpectedException := EPropertyConvertError;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestAddAppendersToControllers;
var
  controller: ILoggerController;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  fStrings
    .Add('[appenders\appender2]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock2');
  fStrings
    .Add('[controllers\controller1]')
    .Add('appender = appender2');

   TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.Build;

  controller := fContainer.Resolve<ILoggerController>;

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller)).AsType<IList<ILogAppender>>;

  CheckEquals(1, appenders.Count);
  CheckSame(fContainer.Resolve<ILogAppender>('logging.appender2.appender'),
    appenders[0]);
end;

procedure TTestLoggingConfiguration.TestAddChainedController;
var
  controller: ILoggerController;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  fStrings
    .Add('[controllers\default]')
    .Add('appender = chained');
  fStrings
    .Add('[controllers\chained]');

   TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.Build;

  controller := fContainer.Resolve<ILoggerController>;

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller)).AsType<IList<ILogAppender>>;

  CheckEquals(1, appenders.Count);
  CheckSame(fContainer.Resolve<ILogAppender>('logging.chained.appender'),
    appenders[0]);
end;

procedure TTestLoggingConfiguration.TestAddLoggerAssignments;
var
  objCtor: TObjCtor;
  objProc: TObjProc;
  objImpl: TImpl;
begin
  fStrings
    .Add('[loggers\default]');
  fStrings
    .Add('[loggers\logger2]')
    .Add('assign = TObjCtor');
  fStrings
    .Add('[loggers\logger3]')
    .Add('assign = TObjProc');
  fStrings
    .Add('[loggers\logger4]')
    .Add('assign = TObject');

  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.RegisterType<TObjCtor>.Implements<TObjCtor>.AsSingleton;
  fContainer.RegisterType<TObjProc>.Implements<TObjProc>.AsSingleton;
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;

  objCtor := fContainer.Resolve<TObjCtor>;
  objProc := fContainer.Resolve<TObjProc>;
  objImpl := fContainer.Resolve<TImpl>;

  CheckSame(fContainer.Resolve<ILogger>('logging.logger2'), objCtor.Logger);
  CheckSame(fContainer.Resolve<ILogger>('logging.logger3'), objProc.Logger);
  CheckSame(fContainer.Resolve<ILogger>('logging.logger4'), objImpl.Logger1);
  CheckSame(fContainer.Resolve<ILogger>('logging.logger2'), objImpl.Logger2);
end;

procedure TTestLoggingConfiguration.TestAddSerializersToControllers;
var
  controller1,
  controller2: ILoggerController;
  serializer1,
  serializer2: ITypeSerializer;
  f: TRttiField;
  serializers: IList<ITypeSerializer>;
begin
  fStrings
    .Add('[controllers\controller1]')
    .Add('serializer = TTypeSerializerMock');
  fStrings
    .Add('[controllers\controller2]')
    .Add('serializer = TTypeSerializerMock2')
    .Add('serializer = TTypeSerializerMock');

  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.Build;

  controller1 := fContainer.Resolve<ILoggerController>('logging.controller1.controller');
  controller2 := fContainer.Resolve<ILoggerController>('logging.controller2.controller');

  serializer1 := fContainer.Resolve<ITypeSerializer>(
    GetQualifiedClassName(TTypeSerializerMock));
  serializer2 := fContainer.Resolve<ITypeSerializer>(
    GetQualifiedClassName(TTypeSerializerMock2));

  f := TType.GetType<TLoggerController>.GetField('fSerializers');

  serializers := f.GetValue(TObject(controller1)).AsType<IList<ITypeSerializer>>;
  CheckEquals(1, serializers.Count);
  CheckSame(serializer1, serializers[0]);

  serializers := f.GetValue(TObject(controller2)).AsType<IList<ITypeSerializer>>;
  CheckEquals(2, serializers.Count);
  CheckSame(serializer2, serializers[0]);
  CheckSame(serializer1, serializers[1]);
end;

procedure TTestLoggingConfiguration.TestComplexConfiguration;
var
  logger1,
  logger2: ILogger;
  controller1,
  controller2: ILoggerController;
  appender1,
  appender2,
  appenderCtl: ILogAppender;
  f: TRttiField;
  appenders: IList<ILogAppender>;
  impl: TImpl;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  fStrings
    .Add('[appenders\appender2]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock2')
    .Add('levels = Fatal, Error, Warn')
    .Add('entryTypes = Text, Value');

  fStrings
    .Add('[controllers\default]')
    .Add('appender = appender1')
    .Add('appender = controller2');
  fStrings
    .Add('[controllers\controller2]')
    .Add('class = Spring.Tests.Logging.Types.TLoggerController2')
    .Add('appender = appender2')
    .Add('levels = Fatal, Error')
    .Add('entryTypes = Text, SerializedData');

  fStrings
    .Add('[loggers\default]');
  fStrings
    .Add('[loggers\logger2]')
    .Add('class = Spring.Tests.Logging.Types.TLogger2')
    .Add('controller = controller2')
    .Add('levels = Fatal')
    .Add('entryTypes = Leaving')
    .Add('assign = Spring.Tests.Logging.Types.TImpl');

  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;

  logger1 := fContainer.Resolve<ILogger>;
  logger2 := fContainer.Resolve<ILogger>('logging.logger2');
  controller1 := fContainer.Resolve<ILoggerController>;
  controller2 := fContainer.Resolve<ILoggerController>('logging.controller2.controller');
  appender1 := fContainer.Resolve<ILogAppender>('logging.appender1.appender');
  appender2 := fContainer.Resolve<ILogAppender>('logging.appender2.appender');
  appenderCtl := fContainer.Resolve<ILogAppender>('logging.controller2.appender');
  impl := fContainer.Resolve<TImpl>;

  f := TType.GetType<TLogger>.GetField('fController');
  CheckSame(controller1, f.GetValue(TObject(logger1)).AsType<ILoggerController>);
  CheckSame(controller2, f.GetValue(TObject(logger2)).AsType<ILoggerController>);

  f := TType.GetType<TLoggerController2>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller1)).AsType<IList<ILogAppender>>;

  CheckEquals(2, appenders.Count);
  CheckSame(appender1, appenders[0]);
  CheckSame(appenderCtl, appenders[1]);

  appenders := f.GetValue(TObject(controller2)).AsType<IList<ILogAppender>>;
  CheckEquals(1, appenders.Count);
  CheckSame(appender2, appenders[0]);

  CheckSame(logger2, impl.Logger1);
  CheckSame(logger2, impl.Logger2);
end;

procedure TTestLoggingConfiguration.TestDefaultController;
var
  controller: ILoggerController;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  fStrings
    .Add('[appenders\default]')
    .Add('class = TAppenderMock2'); //Tets non-fully qualified name
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  controller := fContainer.Resolve<ILoggerController>;
  CheckIs(TObject(controller), TLoggerController);

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller)).AsType<IList<ILogAppender>>;

  CheckEquals(appenders.Count, 2);
  CheckSame(fContainer.Resolve<ILogAppender>('logging.appender1.appender'), appenders[0]);
  CheckSame(fContainer.Resolve<ILogAppender>, appenders[1]);
end;

procedure TTestLoggingConfiguration.TestDefaultLogger;
var
  logger: ILogger;
begin
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.Build;

  logger := fContainer.Resolve<ILogger>;
  CheckIs(TObject(logger), TLogger);
end;

procedure TTestLoggingConfiguration.TestDuplicateDefault;
begin
  fStrings
    .Add('[appenders\default]')
    .Add('class = TAppenderMock');
  fStrings
    .Add('[appenders\default]')
    .Add('class = TAppenderMock');
  ExpectedException := ERegistrationException;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestInjectMultipleAppendersToSingleController;
var
  controller: ILoggerController;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  // check that current configuration implementation will work with the container
  fContainer.RegisterType<TAppenderMock>.AsDefault.AsSingleton;
  fContainer.RegisterType<TAppenderMock>.Implements<ILogAppender>('appender1')
    .AsSingleton;
  fContainer.RegisterType<TAppenderMock>.Implements<ILogAppender>('appender2')
    .AsSingleton;
  fContainer.RegisterType<TLoggerController>.AsSingleton.Implements<ILoggerController>
    .InjectConstructor
    .InjectMethod('AddAppender', ['appender2'])
    .InjectMethod('AddAppender', ['appender1'])
    .InjectMethod('AddAppender'); //Creates circular dependency, not if it is registered with implements

  fContainer.Build;

  controller := fContainer.Resolve<ILoggerController>;

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller)).AsType<IList<ILogAppender>>;

  CheckEquals(3, appenders.Count);
  CheckSame(fContainer.Resolve<ILogAppender>('appender2'), appenders[0]);
  CheckSame(fContainer.Resolve<ILogAppender>('appender1'), appenders[1]);
  CheckSame(fContainer.Resolve<ILogAppender>, appenders[2]);
end;

procedure TTestLoggingConfiguration.Test_LoadFromStrings_Ensures_Container_Resolve_CanBeFreedWithoutErrors;
var
  config: TStrings;
  i: ILogAppender;
  container: TContainer;
begin
  container := TContainer.Create;
  config := TStringList.Create;
  config
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  TLoggingConfiguration.LoadFromStrings(container, config);
  config.Free();

  container.Build;
  i := container.Resolve<ILogAppender>;

  container.Free;
  Pass;
end;

{$ENDREGION}


{$REGION 'TTestLoggingConfigurationBuilder'}

procedure TTestLoggingConfigurationBuilder.SetUp;
begin
  inherited;
end;

procedure TTestLoggingConfigurationBuilder.TestAppender;
var
  builder: TLoggingConfigurationBuilder;
begin
  builder := TLoggingConfigurationBuilder.Create
    .BeginAppender('app1', TAppenderMock)
      .Enabled(False)
      .Levels([TLogLevel.Warn])
      .EntryTypes([TLogEntryType.Text])
      .Prop('someProp', True)
    .EndAppender

    .BeginAppender('app2', 'TSomeAppender')
    .EndAppender;

  CheckEquals(
    '[appenders\app1]' + NL +
    'class = Spring.Tests.Logging.Types.TAppenderMock' + NL +
    'enabled = False' + NL +
    'levels = [Warn]' + NL +
    'entryTypes = [Text]' + NL +
    'someProp = True' + NL +
    NL +
    '[appenders\app2]' + NL +
    'class = TSomeAppender' + NL +
    NL, builder.ToString);
end;

procedure TTestLoggingConfigurationBuilder.TestComplexConfiguration;
var
  config: string;
  logger1,
  logger2: ILogger;
  controller1,
  controller2: ILoggerController;
  appender1,
  appender2,
  appenderCtl: ILogAppender;
  f: TRttiField;
  appenders: IList<ILogAppender>;
  impl: TImpl;
begin
  //This is the same whatever TTestLoggingConfiguration.TestComplexConfiguration
  //does with few adjustments
  config := TLoggingConfigurationBuilder.Create
    .BeginAppender('appender1', TAppenderMock)
    .EndAppender

    .BeginAppender('appender2', 'TAppenderMock2')
      .Levels([TLogLevel.Warn, TLogLevel.Fatal, TLogLevel.Error])
    .EndAppender

    .BeginController
      .AddAppender('appender1')
      .AddAppender('controller2')
    .EndController

    .BeginController('controller2', TLoggerController2)
      .AddAppender('appender2')
      .Levels([TLogLevel.Fatal, TLogLevel.Error])
    .EndController

    .BeginLogger
    .EndLogger

    .BeginLogger('logger2', TLogger2)
      .Controller('controller2')
      .Levels([TLogLevel.Fatal])
      .Assign(TImpl)
    .EndLogger

    .ToString;

  TLoggingConfiguration.LoadFromString(fContainer, config);
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;

  logger1 := fContainer.Resolve<ILogger>;
  logger2 := fContainer.Resolve<ILogger>('logging.logger2');
  controller1 := fContainer.Resolve<ILoggerController>;
  controller2 := fContainer.Resolve<ILoggerController>('logging.controller2.controller');
  appender1 := fContainer.Resolve<ILogAppender>('logging.appender1.appender');
  appender2 := fContainer.Resolve<ILogAppender>('logging.appender2.appender');
  appenderCtl := fContainer.Resolve<ILogAppender>('logging.controller2.appender');
  impl := fContainer.Resolve<TImpl>;

  f := TType.GetType<TLogger>.GetField('fController');
  CheckSame(controller1, f.GetValue(TObject(logger1)).AsType<ILoggerController>);
  CheckSame(controller2, f.GetValue(TObject(logger2)).AsType<ILoggerController>);

  f := TType.GetType<TLoggerController2>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller1)).AsType<IList<ILogAppender>>;

  CheckEquals(2, appenders.Count);
  CheckSame(appender1, appenders[0]);
  CheckSame(appenderCtl, appenders[1]);

  appenders := f.GetValue(TObject(controller2)).AsType<IList<ILogAppender>>;
  CheckEquals(1, appenders.Count);
  CheckSame(appender2, appenders[0]);

  CheckSame(logger2, impl.Logger1);
  CheckSame(logger2, impl.Logger2);
end;

procedure TTestLoggingConfigurationBuilder.TestController;
var
  builder: TLoggingConfigurationBuilder;
begin
  builder := TLoggingConfigurationBuilder.Create
    .BeginController
      .Enabled(True)
      .Levels([TLogLevel.Fatal, TLogLevel.Error])
      .EntryTypes([TLogEntryType.Text, TLogEntryType.Value])
      .AddAppender('app1')
      .Prop('test', 0)
    .EndController

    .BeginController('ctl2', 'TSomeController')
      .AddAppender('ctl1')
      .AddSerializer('TSomeSerializer')
    .EndController

    .BeginController('ctl3', TLoggerControllerMock)
      .AddAppender('ctl1')
      .AddSerializer(TTypeSerializerMock)
    .EndController;

  CheckEquals(
    '[controllers\default]' + NL +
    'enabled = True' + NL +
    'levels = [Error,Fatal]' + NL +
    'entryTypes = [Text,Value]' + NL +
    'appender = app1' + NL +
    'test = 0' + NL +
    NL +
    '[controllers\ctl2]' + NL +
    'class = TSomeController' + NL +
    'appender = ctl1' + NL +
    'serializer = TSomeSerializer' + NL +
    NL +
    '[controllers\ctl3]' + NL +
    'class = Spring.Tests.Logging.Types.TLoggerControllerMock' + NL +
    'appender = ctl1' + NL +
    'serializer = Spring.Tests.Logging.Types.TTypeSerializerMock' + NL +
    NL, builder.ToString);

end;

procedure TTestLoggingConfigurationBuilder.TestEmpty;
begin
  CheckEquals('', TLoggingConfigurationBuilder.Create.ToString);
end;

procedure TTestLoggingConfigurationBuilder.TestLogger;
var
  builder: TLoggingConfigurationBuilder;
begin
  builder := TLoggingConfigurationBuilder.Create
    .BeginLogger
      .Enabled(True)
      .Levels([TLogLevel.Error, TLogLevel.Debug])
      .EntryTypes([TLogEntryType.Text, TLogEntryType.Value])
      .Controller('ctl1')
      .Prop('test', TLogLevel.Trace)
    .EndLogger

    .BeginLogger('log2', 'TSomeLogger')
      .Controller('ctl2')
    .EndLogger

    .BeginLogger('log3', TLogger1)
      .Controller('ctl3')
      .Assign(TImpl)
      .Assign('TSomeClass')
    .EndLogger;

  CheckEquals(
    '[loggers\default]' + NL +
    'enabled = True' + NL +
    'levels = [Debug,Error]' + NL +
    'entryTypes = [Text,Value]' + NL +
    'controller = ctl1' + NL +
    'test = Trace' + NL +
    NL +
    '[loggers\log2]' + NL +
    'class = TSomeLogger' + NL +
    'controller = ctl2' + NL +
    NL +
    '[loggers\log3]' + NL +
    'class = Spring.Tests.Logging.Types.TLogger1' + NL +
    'controller = ctl3' + NL +
    'assign = Spring.Tests.Logging.Types.TImpl' + NL +
    'assign = TSomeClass' + NL +
    NL, builder.ToString);
end;

{$ENDREGION}


end.
