{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2014 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit System.Threading;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.SysUtils, System.Classes, System.Generics.Collections,
  System.Generics.Defaults, System.Timespan, System.SyncObjs;

type
  TFunctionEvent<T> = function (Sender: TObject): T of object;

  TExceptionHandlerEvent = procedure (const AException: Exception; var Handled: Boolean) of object;
  TExceptionHandlerProc = reference to procedure (const AException: Exception; var Handled: Boolean);
  EAggregateException = class(Exception)
  public type
    TExceptionEnumerator = class
    private
      FIndex: Integer;
      FException: EAggregateException;
      function GetCurrent: Exception; inline;
      constructor Create(const AException: EAggregateException);
    public
      function MoveNext: Boolean; inline;
      property Current: Exception read GetCurrent;
    end;
  private
    FInnerExceptions: TArray<Exception>;
    function GetCount: Integer; inline;
    function GetInnerException(Index: Integer): Exception; inline;
    procedure ExtractExceptionsToList(const AList: TList<Exception>);
    constructor Create(const AMessage: string; const AList: TList<Exception>); overload;
  public
    constructor Create(const AExceptionArray: array of Exception); overload;
    constructor Create(const AMessage: string; const AExceptionArray: array of Exception); overload;
    destructor Destroy; override;

    function GetEnumerator: TExceptionEnumerator; inline;
    procedure Handle(AExceptionHandlerEvent: TExceptionHandlerEvent); overload;
    procedure Handle(const AExceptionHandlerProc: TExceptionHandlerProc); overload;
    function ToString: string; override;
    property Count: Integer read GetCount;
    property InnerExceptions[Index: Integer]: Exception read GetInnerException; default;
  end;

  TSparseArray<T: class> = class
  strict private
    FLock: TObject;
    FArray: TArray<T>;
  public
    constructor Create(InitialSize: Integer);
    destructor Destroy; override;
    function Add(const Item: T): Integer;
    procedure Remove(const Item: T);
    property Current: TArray<T> read FArray;
  end;

  TWorkStealingQueue<T> = class
  strict private const InitialSize = 32;
  strict private
    FComparer: IEqualityComparer<T>;
    FArray: TArray<T>;
    FMask: Integer;
    [Volatile] FHeadIndex: Integer;
    [Volatile] FTailIndex: Integer; { volatile }
    FForeignLock: TObject;
    function GetIsEmpty: Boolean;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function LocalFindAndRemove(const AItem: T): Boolean;
    procedure LocalPush(const AItem: T);
    function LocalPop(out AItem: T): Boolean;
    function TrySteal(out AItem: T; Timeout: Cardinal = 0): Boolean;
    function Remove(const AItem: T): Boolean;
    property IsEmpty: Boolean read GetIsEmpty;
    property Count: Integer read GetCount;
  end;

  TObjectCache = class
  public const
    CObjectCacheLimit = 50;
  private type
    PCacheEntry = ^TCacheEntry;
    TCacheEntry = record
      Next: PCacheEntry;
    end;
  private
    FRoot: PCacheEntry;
    FCount: Integer;
    FClassType: TClass;
    procedure Push(var Stack: PCacheEntry; EventItem: PCacheEntry);
    function Pop(var Stack: PCacheEntry): PCacheEntry;
  public
    constructor Create(AClass: TClass);
    destructor Destroy; override;

    function Insert(Instance: Pointer): Boolean;
    function Remove: Pointer;
  end;

  TObjectCaches = class(TObjectDictionary<TClass, TObjectCache>)
  public
    procedure AddObjectCache(AClass: TClass);
  end;

  TThreadPool = class sealed
  public type
    IControlFlag = interface(IInterface)
      function Increment: Integer;
    end;
  protected type
    TSafeSharedInteger = record
    private
      FSharedVar: PInteger;
      function GetInteger: Integer; inline;
      procedure SetInteger(const Value: Integer); inline;
    public
      constructor Create(var SharedVar: Integer);
      function Increment: Integer; inline;
      function Decrement: Integer; inline;
      function CompareExchange(Value: Integer; Comparand: Integer): Integer; inline;
      class operator Explicit(Value: TSafeSharedInteger): Integer; inline;

      property Value: Integer read GetInteger write SetInteger;
    end;
    TSafeSharedCardinal = record
    private
      FSharedVar: PCardinal;
      function GetCardinal: Cardinal; inline;
      procedure SetCardinal(const Value: Cardinal); inline;
    public
      constructor Create(var SharedVar: Cardinal);
      function Increment: Cardinal; inline;
      function Decrement: Cardinal; inline;
      class operator Explicit(Value: TSafeSharedCardinal): Cardinal; inline;

      property Value: Cardinal read GetCardinal write SetCardinal;
    end;
    IThreadPoolWorkItem = interface(IInterface)
      function ShouldExecute: Boolean;
      procedure ExecuteWork;
    end;
    TControlFlag = class(TInterfacedObject, IControlFlag)
    strict private
      FControlFlag: Integer;
    private
      function Increment: Integer;
    public
      constructor Create; overload;
    end;

    TAbstractWorkerData = class(TInterfacedObject)
    protected
      FControlFlag: IControlFlag;
      function ShouldExecute: Boolean;
    public
      [Result: Unsafe] class function NewInstance: TObject; override;
      procedure FreeInstance; override;
    end;

    TWorkerData = class(TAbstractWorkerData, IThreadPoolWorkItem)
    private
      class constructor Create;
    protected
      FSender: TObject;
      FWorkerEvent: TNotifyEvent;
      FProc: TProc;
      procedure ExecuteWork;
    end;

    TBaseWorkerThread = class(TThread)
    private
      FThreadPool: TThreadPool;
      FRunningEvent: TLightweightEvent;
      class var WorkerThreadID: Integer;
    protected
      procedure SafeTerminate;
      procedure Execute; override;
      property ThreadPool: TThreadPool read FThreadPool;
      property RunningEvent: TLightweightEvent read FRunningEvent;
    public
      constructor Create(AThreadPool: TThreadPool);
      destructor Destroy; override;
    end;

    TQueueWorkerThread = class(TBaseWorkerThread)
    private
      FWorkQueue: TWorkStealingQueue<IThreadPoolWorkItem>;
      FThreadSuspended: TSafeSharedInteger; // This is shared among all worker threads so that threads suspend along an interval
      FLastSuspendTick: TSafeSharedCardinal; // Shared among workers, the tick count for the last thread to come out of suspension
      FRetiredThreadWakeEvent: TLightweightEvent;
    protected
      function SuspendWork: Boolean;
      function TryToRetire: Boolean;
      procedure ExecuteWorkItem(var Item: IThreadPoolWorkItem);
      procedure Execute; override;
      procedure PushLocalWorkToGlobal;
      property WorkQueue: TWorkStealingQueue<IThreadPoolWorkItem> read FWorkQueue;
    public
      constructor Create(AThreadPool: TThreadPool);
      destructor Destroy; override;
    end;

    TThreadPoolMonitor = class(TThread)
    private
      FThreadPool: TThreadPool;
    protected
      function IsThrottledDelay(LastCreationTick: Cardinal; ThreadCount: Cardinal): Boolean;
      procedure Execute; override;
      procedure GrowThreadPoolIfStarved;
    public
      constructor Create(AThreadPool: TThreadPool);
    end;

  protected class threadvar
    QueueThread: TQueueWorkerThread;
  private const
    MaxThreadsPerCPU = 25;
    // Constants used for calculating CPU Usage
    CPUUsageHigh = 95; // Start retiring/removing threads when CPU usage gets this high
    CPUUsageLow = 80; // Add more threads if the CPU usage is below this
    CPUUsageLowest = 20; // Shrink the thread pool when CPU usage falls below this
    NumCPUUsageSamples = 10; // Keep a running list of CPU Usage samples over which the average is calculated
    MonitorThreadDelay = 500;
    SuspendInterval = 5000 + MonitorThreadDelay; // Interval to use for suspending work in worker threads
    SuspendTime = MonitorThreadDelay + 100; // Time to spend in SuspendWork;
    RetirementDelay = 5000; // Delay interval for retiring threads
  private type
    TMonitorThreadStat = (Created, NoWorkers, ForceSize = 31);
    TMonitorThreadStatus = set of TMonitorThreadStat;
  private var
    FQueue: TQueue<IThreadPoolWorkItem>;
    FQueues: TSparseArray<TWorkStealingQueue<IThreadPoolWorkItem>>;
    FThreads: TThreadList<TBaseWorkerThread>;

    FWorkerThreadCount: Integer;
    FMinLimitWorkerThreadCount: Integer;
    FMaxLimitWorkerThreadCount: Integer;
    FIdleWorkerThreadCount: Integer;
    [Volatile]
    FQueuedRequestCount: Integer;
    FRetiredWorkerThreadCount: Integer;
    FMonitorThreadStatus: TMonitorThreadStatus;
    FAverageCPUUsage: Integer;
    FCurrentCPUUsage: Integer;
    // Shared among this pool's worker threads
    FThreadSuspended: Integer;
    FLastSuspendTick: Cardinal;
    FRetiredThreadWakeEvent: TLightweightEvent;
    // Used to keep monitor thread from creating threads too quickly
    FLastThreadCreationTick: Cardinal;
    FLastQueuedRequestCount: Integer;

    FShutdown: Boolean;
    class var FDefaultPool: TThreadPool;
    class var FObjectCaches: TObjectCaches;
    function CreateWorkerThread: TBaseWorkerThread;
    procedure CreateMonitorThread;
    procedure WaitMonitorThread;
    procedure DecIdleThreadCount; inline;
    procedure DecWorkRequestCount; inline;
    procedure GrowWorkerPool;
    procedure IncIdleThreadCount; inline;
    procedure IncWorkRequestCount; inline;
    procedure ResurrectRetiredThread; inline;
    function ShouldGrowPool: Boolean; inline;
    procedure QueueWorkItem(const WorkerData: IThreadPoolWorkItem; UseLocalQueue: Boolean); overload;
    function TryRemoveWorkItem(const WorkerData: IThreadPoolWorkItem): Boolean;
    function GetMaxWorkerThreads: Integer;
    function GetMinWorkerThreads: Integer;
    class function GetObjectCaches: TObjectCaches; static;
    class constructor Create;
    class destructor Destroy;
    class property ObjectCaches: TObjectCaches read GetObjectCaches;
  public
    constructor Create;
    destructor Destroy; override;

    class function NewControlFlag: IControlFlag; static;

    procedure QueueWorkItem(Sender: TObject; WorkerEvent: TNotifyEvent; const AControlFlag: IControlFlag = nil); overload;
    procedure QueueWorkItem(const WorkerEvent: TProc; const AControlFlag: IControlFlag = nil); overload;

    // Returns false if attempting to set a value smaller than the number of processors. Setting this
    // value too high can lead problems with other libraries and processes. If too many threads execute
    // at the same time, task/thread switching overhead can become restrictive.
    function SetMaxWorkerThreads(Value: Integer): Boolean;
    // Returns false if attempting to set a value < 0 or > MaxWorkerThreads. The actual number of pool
    // threads could be less than this value depending on actual demand. Setting this to too few threads could
    // be less than optimal resource utilization.
    function SetMinWorkerThreads(Value: Integer): Boolean;

    property MaxWorkerThreads: Integer read GetMaxWorkerThreads;
    property MinWorkerThreads: Integer read GetMinWorkerThreads;

    class property Default: TThreadPool read FDefaultPool;
  end;

  TTaskStatus = (Created, WaitingToRun, Running, Completed, WaitingForChildren, Canceled, Exception);

  ITask = interface(TThreadPool.IThreadPoolWorkItem)
    function Wait(Timeout: LongWord = INFINITE): Boolean; overload;
    function Wait(const Timeout: TTimeSpan): Boolean; overload;
    procedure Cancel;
    procedure CheckCanceled;
    function Start: ITask;
    function GetStatus: TTaskStatus;

    property Status: TTaskStatus read GetStatus;
  end;

  IFuture<T> = interface(ITask)
    function Start: IFuture<T>;
    function GetValue: T;

    property Value: T read GetValue;
  end;

  TAbstractTask = class(TThreadPool.TAbstractWorkerData)
  protected type
    IInternalTask = interface(ITask)
      procedure AddCompleteEvent(const Proc: TProc<ITask>);
      procedure HandleChildCompletion(const Task: IInternalTask);
      procedure SetExceptionObject(const Exception: TObject);
      procedure RemoveCompleteEvent(const Proc: TProc<ITask>);
    end;
  end;

  EOperationCanceled = class(Exception);

  TTask = class(TAbstractTask, TThreadPool.IThreadPoolWorkItem, ITask, TAbstractTask.IInternalTask)
  protected type
    TOptionStateFlag = (Started, CallbackRun, ChildWait, Complete, Canceled, Faulted, Replicating, Replica, Raised, ForceSize = 31);
    TOptionStateFlags = set of TOptionStateFlag;
    TCreateFlag = (Replicating, Replica);
    TCreateFlags = set of TCreateFlag;
  protected const
    StateFlagMask = [TOptionStateFlag.Started, TOptionStateFlag.CallbackRun, TOptionStateFlag.ChildWait,
      TOptionStateFlag.Complete, TOptionStateFlag.Canceled, TOptionStateFlag.Faulted];
    OptionFlagMask = [TOptionStateFlag.Replicating, TOptionStateFlag.Replica];
  private type
    TCompleteEvents = TList<TProc<ITask>>;
    TUnsafeTask = record
    private
      [Unsafe] FTask: TTask;
    public
      class operator Implicit(const ATask: TTask): TUnsafeTask; inline;
      class operator Explicit(const ATask: TUnsafeTask): TTask; inline;
      property Value: TTask read FTask write FTask;
    end;
  private
    [Volatile]
    FState: TOptionStateFlags;
    FTaskCountdown: Integer;
    FParentTask: TAbstractTask.IInternalTask;
    [Volatile]
    FDoneEvent: TLightweightEvent;
    [Volatile]
    FFaultedChildren: TList<TAbstractTask.IInternalTask>;
    [Volatile]
    FCompleteEvents: TObject;
    class threadvar FCurrentTask: TTask;
    class var CompletedFlag: TObject;
    class constructor Create;
    class destructor Destroy;
    function GetDoneEvent: TLightweightEvent;
    class function TimespanToMilliseconds(const Timeout: TTimeSpan): Longword; static;
    function InternalAddCompleteEvent(const Proc: TProc<ITask>): Boolean;
  protected
    FPool: TThreadPool;
    FSender: TObject;
    FException: TObject;
    FEvent: TNotifyEvent;
    FProc: TProc;
    function UpdateStateAtomic(NewState: TOptionStateFlags; InvalidStates: TOptionStateFlags): Boolean; overload;
    function UpdateStateAtomic(NewState: TOptionStateFlags; InvalidStates: TOptionStateFlags; out OldState: TOptionStateFlags): Boolean; overload;
    procedure SetTaskStop;
    function ShouldCreateReplica: Boolean; virtual;
    function CreateReplicaTask(const AProc: TProc; AParent: TTask; CreateFlags: TCreateFlags): TTask; virtual;
    procedure CheckFaulted;
    procedure SetComplete;
    procedure AddChild;
    procedure ForgetChild;
    function InternalExecuteNow: Boolean;
    function GetExceptionObject: Exception;
    function GetIsComplete: Boolean; inline;
    function GetIsReplicating: Boolean; inline;
    function GetHasExceptions: Boolean; inline;
    function GetIsCanceled: Boolean; inline;
    function GetIsQueued: Boolean; inline;
    function GetWasExceptionRaised: Boolean; inline;
    procedure QueueEvents; virtual;
    procedure Complete(UserEventRan: Boolean);
    procedure IntermediateCompletion;
    procedure FinalCompletion;
    procedure ProcessCompleteEvents; virtual;
    procedure SetRaisedState;
    function InternalWork(CheckExecuting: Boolean): Boolean;
    procedure InternalExecute(var CurrentTaskVar: TTask);
    procedure Execute;
    procedure ExecuteReplicates(Root: TTask);
    procedure CallUserCode; inline;
    procedure HandleException(const ChildTask: ITask; const Exception: TObject);
    function MarkAsStarted: Boolean;
    function TryExecuteNow(WasQueued: Boolean): Boolean;
    { IThreadPoolWorkItem }
    procedure ExecuteWork;
    { ITask }
    function Wait(Timeout: LongWord = INFINITE): Boolean; overload;
    function Wait(const Timeout: TTimeSpan): Boolean; overload;
    procedure Cancel;
    procedure CheckCanceled;
    function Start: ITask;
    function GetStatus: TTaskStatus;
    { IInternalTask }
    procedure AddCompleteEvent(const Proc: TProc<ITask>);
    procedure HandleChildCompletion(const Task: TAbstractTask.IInternalTask);
    procedure SetExceptionObject(const Exception: TObject);
    procedure RemoveCompleteEvent(const Proc: TProc<ITask>);
    property IsComplete: Boolean read GetIsComplete;
    property IsReplicating: Boolean read GetIsReplicating;
    property HasExceptions: Boolean read GetHasExceptions;
    property IsCanceled: Boolean read GetIsCanceled;
    property IsQueued: Boolean read GetIsQueued;
    property WasExceptionRaised: Boolean read GetWasExceptionRaised;
    property DoneEvent: TLightweightEvent read GetDoneEvent;
    constructor Create(Sender: TObject; Event: TNotifyEvent; const AProc: TProc; const APool: TThreadPool; const AParent: TTask; CreateFlags: TCreateFlags = []); overload;
    class function DoWaitForAll(const Tasks: array of ITask; Timeout: LongWord): Boolean; static;
    class function DoWaitForAny(const Tasks: array of ITask; Timeout: LongWord): Integer; static;
  public
    class function CurrentTask: ITask; static; inline;
    constructor Create; overload; // do not call this constructor!!
    destructor Destroy; override;
    class function Create(Sender: TObject; Event: TNotifyEvent): ITask; overload; static; inline;
    class function Create(const Proc: TProc): ITask; overload; static; inline;
    class function Create(Sender: TObject; Event: TNotifyEvent; const APool: TThreadPool): ITask; overload; static; inline;
    class function Create(const Proc: TProc; APool: TThreadPool): ITask; overload; static; inline;
    class function Future<T>(Sender: TObject; Event: TFunctionEvent<T>): IFuture<T>; overload; static; inline;
    class function Future<T>(Sender: TObject; Event: TFunctionEvent<T>; APool: TThreadPool): IFuture<T>; overload; static; inline;
    class function Future<T>(const Func: TFunc<T>): IFuture<T>; overload; static; inline;
    class function Future<T>(const Func: TFunc<T>; APool: TThreadPool): IFuture<T>; overload; static; inline;
    class function Run(Sender: TObject; Event: TNotifyEvent): ITask; overload; static; inline;
    class function Run(Sender: TObject; Event: TNotifyEvent; APool: TThreadPool): ITask; overload; static; inline;
    class function Run(const Func: TProc): ITask; overload; static; inline;
    class function Run(const Func: TProc; APool: TThreadPool): ITask; overload; static; inline;
    class function WaitForAll(const Tasks: array of ITask): Boolean; overload; static;
    class function WaitForAll(const Tasks: array of ITask; Timeout: LongWord): Boolean; overload; static;
    class function WaitForAll(const Tasks: array of ITask; const Timeout: TTimeSpan): Boolean; overload; static;
    class function WaitForAny(const Tasks: array of ITask): Integer; overload; static;
    class function WaitForAny(const Tasks: array of ITask; Timeout: LongWord): Integer; overload; static;
    class function WaitForAny(const Tasks: array of ITask; const Timeout: TTimeSpan): Integer; overload; static;
  end;

  TFuture<T> = class sealed(TTask, IFuture<T>)
  private
    FEvent: TFunctionEvent<T>;
    FFunc: TFunc<T>;
    FResult: T;
    class constructor Create;
    procedure RunEvent(Sender: TObject);
  protected
    function Start: IFuture<T>;
    function GetValue: T;

    constructor Create(Sender: TObject; Event: TFunctionEvent<T>; const Func: TFunc<T>; APool: TThreadPool); overload;
  end;

  TParallel = class sealed
  public type
    TLoopState = class
    private type
      TLoopStateFlags = (Exception, Broken, Stopped, Cancelled, ForceSize = 31);
      TLoopStateFlagSet = set of TLoopStateFlags;
      TLoopStateFlag = class
      private
        FLoopStateFlags: TLoopStateFlagSet;
        function AtomicUpdate(NewState: TLoopStateFlagSet; InvalidStates: TLoopStateFlagSet): Boolean; overload;
        function AtomicUpdate(NewState: TLoopStateFlagSet; InvalidStates: TLoopStateFlagSet; var OldState: TLoopStateFlagSet): Boolean; overload;
        procedure SetFaulted;
        procedure Stop;
        //function Cancel: Boolean;
      strict protected
        function GetLoopStateFlags: TLoopStateFlagSet; inline;
      protected
        property LoopStateFlags: TLoopStateFlagSet read GetLoopStateFlags;
      end;
    private
      FLoopStateFlags: TLoopStateFlag;
    strict protected
      constructor Create(ALoopStateFlags: TLoopStateFlag);
      function GetStopped: Boolean; inline;
      function GetFaulted: Boolean; inline;
      function GetLowestBreakIteration: Variant; inline;
      procedure DoBreak; virtual;
      function DoShouldExit: Boolean; virtual;
      function DoGetLowestBreakIteration: Variant; virtual;
    public
      procedure Break;
      procedure Stop;
      function ShouldExit: Boolean;

      property Faulted: Boolean read GetFaulted;
      property Stopped: Boolean read GetStopped;
      property LowestBreakIteration: Variant read GetLowestBreakIteration;
    end;
    TLoopResult = record
    private
      FCompleted: Boolean;
      FLowestBreakIteration: Variant;
    public
      property Completed: Boolean read FCompleted;
      property LowestBreakIteration: Variant read FLowestBreakIteration;
    end;
    TIteratorEvent = procedure (Sender: TObject; AIndex: Integer) of object;
    TIteratorStateEvent = procedure (Sender: TObject; AIndex: Integer; const LoopState: TLoopState) of object;
    TIteratorEvent64 = procedure (Sender: TObject; AIndex: Int64) of object;
    TIteratorStateEvent64 = procedure (Sender: TObject; AIndex: Int64; const LoopState: TLoopState) of object;
  private type
    TLoopState32 = class sealed(TLoopState)
    private type
      TLoopStateFlag32 = class(TLoopState.TLoopStateFlag)
      private
        FLowestBreakIter: Integer;
        function GetLowestBreakIter: Integer; inline;
      protected
        constructor Create;
        function ShouldExit(ThisIter: Integer): Boolean; overload;
        function ShouldExit: Boolean; overload;

        property LowestBreakIter: Integer read GetLowestBreakIter;
      end;
    strict protected
      procedure DoBreak; override;
      function DoShouldExit: Boolean; override;
      function DoGetLowestBreakIteration: Variant; override;
    private
      FSharedLoopStateFlags: TLoopStateFlag32;
      FCurrentIteration: Integer;
    public
      constructor Create(ALoopStateFlags: TLoopStateFlag32);
      property CurrentIteration: Integer read FCurrentIteration write FCurrentIteration;
    end;
    TLoopState64 = class sealed(TLoopState)
    private type
      TLoopStateFlag64 = class(TLoopState.TLoopStateFlag)
      private
        FLowestBreakIter: Int64;
        function GetLowestBreakIter: Int64; inline;
      protected
        constructor Create;
        function ShouldExit(ThisIter: Int64): Boolean; overload;
        function ShouldExit: Boolean; overload;

        property LowestBreakIter: Int64 read GetLowestBreakIter;
      end;
    strict protected
      procedure DoBreak; override;
      function DoShouldExit: Boolean; override;
      function DoGetLowestBreakIteration: Variant; override;
    private
      FSharedLoopStateFlags: TLoopStateFlag64;
      FCurrentIteration: Int64;
    public
      constructor Create(ALoopStateFlags: TLoopStateFlag64);
      property CurrentIteration: Int64 read FCurrentIteration write FCurrentIteration;
    end;
    TReplicableTask = class(TTask)
    private
      FParallelism: Integer;
      class constructor Create;
    protected
      procedure SetParallelism;
      function ShouldCreateReplica: Boolean; override;
      function CreateReplicaTask(const AProc: TProc; AParent: TTask; CreateFlags: TTask.TCreateFlags): TTask; override;
      procedure QueueEvents; override;
      constructor Create(Sender: TObject; Event: TNotifyEvent; const AProc: TProc; const APool: TThreadPool; const AParent: TTask; AParallelism: Integer; CreateFlags: TTask.TCreateFlags = []); overload;
    end;
    TReplicaTask = class(TTask)
    private
      class constructor Create;
    protected
      constructor Create(const AProc: TProc; APool: TThreadPool; AParent: TTask; CreateFlags: TTask.TCreateFlags);
    end;
    TJoinTask = class(TReplicableTask)
    private
      FEvents: TArray<TNotifyEvent>;
      FProcs: TArray<TProc>;
      class constructor Create;
    protected
      constructor InternalCreate(Sender: TObject; AEvents: array of TNotifyEvent; const AProcs: array of TProc; APool: TThreadPool);
    public
      class function Create(Sender: TObject; AEvents: array of TNotifyEvent; APool: TThreadPool): ITask; overload; static;
      class function Create(const AProcs: array of TProc; APool: TThreadPool): ITask; overload; static;
    end;
    TStrideManager = record
    private
      FThreshold, FRequest: Integer;
      FCurrentStride: Integer;
      FMaxStride: Integer;
    public
      class function NewStrideManager(AStartStride, AThreshold: Integer): TStrideManager; static; inline;
      function NextStride: Integer;
    end;
    TStrideManager64 = record
    private
      FThreshold, FRequest: Integer;
      FCurrentStride: Int64;
      FMaxStride: Int64;
    public
      class function NewStrideManager(AStartStride, AThreshold: Int64): TStrideManager64; static; inline;
      function NextStride: Int64;
    end;
    class function ForWorker(Sender: TObject; AEvent: TIteratorEvent; AStateEvent: TIteratorStateEvent; const AProc: TProc<Integer>; const AProcWithState: TProc<Integer, TLoopState>; ALowInclusive, AHighExclusive, AStride: Integer; APool: TThreadPool): TLoopResult; static;
    class function ForWorker64(Sender: TObject; AEvent: TIteratorEvent64; AStateEvent: TIteratorStateEvent64; const AProc: TProc<Int64>; const AProcWithState: TProc<Int64, TLoopState>; ALowInclusive, AHighExclusive, AStride: Int64; APool: TThreadPool): TLoopResult; static;
  public

    class function &For(Sender: TObject; ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorEvent): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorEvent; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorStateEvent): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorStateEvent; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorEvent): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorEvent; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorStateEvent): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorStateEvent; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer>): TLoopResult; overload; static; inline;
    class function &For(ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer>; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer, TLoopState>): TLoopResult; overload; static; inline;
    class function &For(ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer, TLoopState>; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(AStride, ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer>): TLoopResult; overload; static; inline;
    class function &For(AStride, ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer>; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(AStride, ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer, TLoopState>): TLoopResult; overload; static; inline;
    class function &For(AStride, ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer, TLoopState>; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorEvent64): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorEvent64; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorStateEvent64): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorStateEvent64; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorEvent64): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorEvent64; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorStateEvent64): TLoopResult; overload; static; inline;
    class function &For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorStateEvent64; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64>): TLoopResult; overload; static; inline;
    class function &For(ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64>; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64, TLoopState>): TLoopResult; overload; static; inline;
    class function &For(ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64, TLoopState>; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(AStride, ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64>): TLoopResult; overload; static; inline;
    class function &For(AStride, ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64>; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(AStride, ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64, TLoopState>): TLoopResult; overload; static; inline;
    class function &For(AStride, ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64, TLoopState>; APool: TThreadPool): TLoopResult; overload; static; inline;
    class function Join(Sender: TObject; AEvents: array of TNotifyEvent): ITask; overload; static;
    class function Join(Sender: TObject; AEvents: array of TNotifyEvent; APool: TThreadPool): ITask; overload; static;
    class function Join(Sender: TObject; AEvent1, AEvent2: TNotifyEvent): ITask; overload; static; inline;
    class function Join(Sender: TObject; AEvent1, AEvent2: TNotifyEvent; APool: TThreadPool): ITask; overload; static;
    class function Join(const AProcs: array of TProc): ITask; overload; static;
    class function Join(const AProcs: array of TProc; APool: TThreadPool): ITask; overload; static;
    class function Join(const AProc1, AProc2: TProc): ITask; overload; static; inline;
    class function Join(const AProc1, AProc2: TProc; APool: TThreadPool): ITask; overload; static;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.RTLConsts,
  System.Variants,
  System.Diagnostics, System.Math;

{ EAggregateException }

constructor EAggregateException.Create(const AMessage: string; const AList: TList<Exception>);
var
  I: Integer;
begin
  inherited Create(AMessage);
  SetLength(FInnerExceptions, AList.Count);
  for I := 0 to AList.Count - 1 do
    FInnerExceptions[I] := AList[I];
end;

constructor EAggregateException.Create(const AExceptionArray: array of Exception);
begin
  Create('SDefaultAggregateExceptionMsg', AExceptionArray);
end;

constructor EAggregateException.Create(const AMessage: string; const AExceptionArray: array of Exception);
var
  I: Integer;
begin
  inherited Create(AMessage);
  SetLength(FInnerExceptions, Length(AExceptionArray));
  for I := 0 to Length(FInnerExceptions) - 1 do
    FInnerExceptions[I] := AExceptionArray[I];
end;

destructor EAggregateException.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FInnerExceptions) - 1 do
    FInnerExceptions[I].Free;
  inherited;
end;

procedure EAggregateException.ExtractExceptionsToList(const AList: TList<Exception>);
begin
  AList.AddRange(FInnerExceptions);
  SetLength(FInnerExceptions, 0);
  //FInnerExceptions := [];
end;

function EAggregateException.GetCount: Integer;
begin
  Result := Length(FInnerExceptions);
end;

function EAggregateException.GetEnumerator: TExceptionEnumerator;
begin
  Result := TExceptionEnumerator.Create(Self);
end;

function EAggregateException.GetInnerException(Index: Integer): Exception;
begin
  Result := FInnerExceptions[Index];
end;

procedure EAggregateException.Handle(const AExceptionHandlerProc: TExceptionHandlerProc);
var
  I: Integer;
  Handled: Boolean;
  List: TList<Exception>;
begin
  List := nil;
  try
    for I := 0 to Length(FInnerExceptions) - 1 do
    begin
      Handled := False;
      AExceptionHandlerProc(FInnerExceptions[I], Handled);
      if not Handled then
      begin
        if List = nil then
          List := TList<Exception>.Create;
        List.Add(FInnerExceptions[I]);
      end;
    end;
    if List <> nil then
    begin
      FInnerExceptions := nil;
      raise EAggregateException.Create(Message, List);
    end;
  finally
    List.Free;
  end;
end;

procedure EAggregateException.Handle(AExceptionHandlerEvent: TExceptionHandlerEvent);
begin
  Handle(
    procedure (const AException: Exception; var Handled: Boolean)
    begin
      AExceptionHandlerEvent(AException, Handled);
    end);
end;

function EAggregateException.ToString: string;
var
  S: TStringBuilder;
  E: Exception;
begin
  S := TStringBuilder.Create;
  try
    S.Append(inherited ToString);
    for E in FInnerExceptions do
      S.AppendLine.Append(E.ToString);
    Result := S.ToString;
  finally
    S.Free;
  end;
end;

{ EAggregateException.TExceptionEnumerator }

constructor EAggregateException.TExceptionEnumerator.Create(const AException: EAggregateException);
begin
  inherited Create;
  FException := AException;
  FIndex := -1;
end;

function EAggregateException.TExceptionEnumerator.GetCurrent: Exception;
begin
  Result := FException.InnerExceptions[FIndex];
end;

function EAggregateException.TExceptionEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FException.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FException.Count;
end;

{ TThreadPool.TSafeSharedInteger }

function TThreadPool.TSafeSharedInteger.CompareExchange(Value, Comparand: Integer): Integer;
begin
  Result := TInterlocked.CompareExchange(FSharedVar^, Value, Comparand);
end;

constructor TThreadPool.TSafeSharedInteger.Create(var SharedVar: Integer);
begin
  FSharedVar := @SharedVar;
end;

function TThreadPool.TSafeSharedInteger.Decrement: Integer;
begin
  Result := TInterlocked.Decrement(FSharedVar^);
end;

class operator TThreadPool.TSafeSharedInteger.Explicit(Value: TSafeSharedInteger): Integer;
begin
  Result := Value.FSharedVar^;
end;

function TThreadPool.TSafeSharedInteger.GetInteger: Integer;
begin
  Result := FSharedVar^;
end;

function TThreadPool.TSafeSharedInteger.Increment: Integer;
begin
  Result := TInterlocked.Increment(FSharedVar^);
end;

procedure TThreadPool.TSafeSharedInteger.SetInteger(const Value: Integer);
begin
  FSharedVar^ := Value;
end;

{ TThreadPool.TSafeSharedCardinal }

constructor TThreadPool.TSafeSharedCardinal.Create(var SharedVar: Cardinal);
begin
  FSharedVar := @SharedVar;
end;

function TThreadPool.TSafeSharedCardinal.Decrement: Cardinal;
begin
  Result := Cardinal(TInterlocked.Decrement(Integer(FSharedVar^)));
end;

class operator TThreadPool.TSafeSharedCardinal.Explicit(Value: TSafeSharedCardinal): Cardinal;
begin
  Result := Value.FSharedVar^;
end;

function TThreadPool.TSafeSharedCardinal.GetCardinal: Cardinal;
begin
  Result := FSharedVar^;
end;

function TThreadPool.TSafeSharedCardinal.Increment: Cardinal;
begin
  Result := Cardinal(TInterlocked.Increment(Integer(FSharedVar^)));
end;

procedure TThreadPool.TSafeSharedCardinal.SetCardinal(const Value: Cardinal);
begin
  FSharedVar^ := Value;
end;

{ TWorkStealingQueue<T> }

constructor TWorkStealingQueue<T>.Create;
begin
  inherited Create;
  SetLength(FArray, InitialSize);
  FMask := InitialSize - 1;
  FForeignLock := TObject.Create;
  FComparer := TEqualityComparer<T>.Default;
end;

destructor TWorkStealingQueue<T>.Destroy;
begin
  FForeignLock.Free;
  inherited;
end;

function TWorkStealingQueue<T>.GetCount: Integer;
begin
//  MemoryBarrier;
  Result := FTailIndex - FHeadIndex;
end;

function TWorkStealingQueue<T>.GetIsEmpty: Boolean;
begin
  Result := FHeadIndex >= FTailIndex;
end;

function TWorkStealingQueue<T>.LocalFindAndRemove(const AItem: T): Boolean;
var
  I: Integer;
  DidLock: Boolean;
  LItem: T;
begin
  if FComparer.Equals(FArray[(FTailIndex - 1) and FMask], AItem) then
    Result := LocalPop(LItem);
  I := FTailIndex - 2;
  while I >= FHeadIndex do
  begin
    if FComparer.Equals(FArray[I and FMask], AItem) then
    begin
      DidLock := TMonitor.TryEnter(FForeignLock);
      try
        if FComparer.Equals(FArray[I and FMask], Default(T)) then
          Exit(False);
        MemoryBarrier;
        FArray[I and FMask] := Default(T);
        if I = FTailIndex then
          Dec(FTailIndex)
        else if I = FHeadIndex then
          Inc(FHeadIndex);
      finally
        if DidLock then
          TMonitor.Exit(FForeignLock);
      end;
    end;
    Dec(I);
  end;
end;

function TWorkStealingQueue<T>.LocalPop(out AItem: T): Boolean;
var
  Tail: Integer;
  Index: Integer;
begin
  while True do
  begin
    Tail := FTailIndex;
    if FHeadIndex >= Tail then
      Exit(False);
    Dec(Tail);
    TInterlocked.Exchange(FTailIndex, Tail);
    if FHeadIndex <= Tail then
    begin
      Index := Tail and FMask;
      AItem := FArray[Index];
      if FComparer.Equals(AItem, Default(T)) then
        Continue;
      FArray[Index] := Default(T);
      Exit(True);
    end else
    begin
      TMonitor.Enter(FForeignLock);
      try
        if FHeadIndex <= Tail then
        begin
          Index := Tail and FMask;
          AItem := FArray[Index];
          if FComparer.Equals(AItem, Default(T)) then
            Continue;
          FArray[Index] := Default(T);
          Exit(True);
        end else
        begin
//          MemoryBarrier; // don't have any notion of "volatile"
          FTailIndex := Tail + 1;
          Exit(False);
        end;
      finally
        TMonitor.Exit(FForeignLock);
      end;
    end;
  end;
end;

procedure TWorkStealingQueue<T>.LocalPush(const AItem: T);
var
  I, Tail, Head, Count: Integer;
  NewArray: TArray<T>;
begin
  Tail := FTailIndex;
  if Tail < FHeadIndex + FMask then
  begin
    FArray[Tail and FMask] := AItem;
    FTailIndex := Tail + 1;
  end else
  begin
    TMonitor.Enter(FForeignLock);
    try
      Head := FHeadIndex;
      Count := FTailIndex - FHeadIndex;
      if Count >= FMask then
      begin
        SetLength(NewArray, Length(FArray) shl 1);
        for I := 0 to Length(FArray) - 1 do
          NewArray[I] := FArray[(I + Head) and FMask];
        FArray := NewArray;
        FHeadIndex := 0;
        FTailIndex := Count;
        Tail := Count;
        FMask := (FMask shl 1) or 1;
      end;
      FArray[Tail and FMask] := AItem;
      FTailIndex := Tail + 1;
    finally
      TMonitor.Exit(FForeignLock);
    end;
  end;
end;

function TWorkStealingQueue<T>.Remove(const AItem: T): Boolean;
var
  I: Integer;
begin
  for I := FTailIndex - 1 downto FHeadIndex + 1 do
  begin
    if FComparer.Equals(FArray[I and FMask], AItem) then
    begin
      TMonitor.Enter(FForeignLock);
      try
        if FComparer.Equals(FArray[I and FMask], AItem) then
        begin
          if I = FTailIndex - 1 then
            Dec(FTailIndex)
          else if I = FHeadIndex + 1 then
            Inc(FHeadIndex)
          else
            FArray[I and FMask] := Default(T);
          Result := True;
        end else
          Result := False;
      finally
        TMonitor.Exit(FForeignLock);
      end;
    end;
  end;
end;

function TWorkStealingQueue<T>.TrySteal(out AItem: T; Timeout: Cardinal): Boolean;
var
  Head: Integer;
  Index: Integer;
begin
  if TMonitor.Enter(FForeignLock, Timeout) then
  try
    while True do
    begin
      Head := FHeadIndex;
      TInterlocked.Exchange(FHeadIndex, Head + 1);
      if Head < FTailIndex then
      begin
        Index := Head and FMask;
        AItem := FArray[Index];
        if FComparer.Equals(AItem, Default(T)) then
          Continue;
        FArray[Index] := Default(T);
        Exit(True);
      end else
      begin
        FHeadIndex := Head;
//        MemoryBarrier;
        Exit(False);
      end;
    end;
  finally
    TMonitor.Exit(FForeignLock);
  end else
    Result := False;
end;

{ TParallel.TStrideManager }

function TParallel.TStrideManager.NextStride: Integer;
var
  NewStride: Integer;
begin
  Result := FCurrentStride;
  if (Result < FMaxStride) and (TInterlocked.Increment(FRequest) mod FThreshold = 0) then
  begin
    NewStride := Min(Result shl 1, FMaxStride);
    if NewStride <= FMaxStride then
      TInterlocked.CompareExchange(FCurrentStride, NewStride, Result);
  end;
end;

class function TParallel.TStrideManager.NewStrideManager(AStartStride, AThreshold: Integer): TStrideManager;
begin
  Result.FCurrentStride := AStartStride;
  Result.FThreshold := AThreshold;
  Result.FMaxStride := AThreshold * 16;
  Result.FRequest := AThreshold - 1;
end;

{ TParallel.TStrideManager64 }

function TParallel.TStrideManager64.NextStride: Int64;
var
  NewStride: Int64;
begin
  Result := FCurrentStride;
  if (Result < FMaxStride) and (TInterlocked.Increment(FRequest) mod FThreshold = 0) then
  begin
    NewStride := Min(Result shl 1, FMaxStride);
    if NewStride <= FMaxStride then
      TInterlocked.CompareExchange(FCurrentStride, NewStride, Result);
  end;
end;

class function TParallel.TStrideManager64.NewStrideManager(AStartStride, AThreshold: Int64): TStrideManager64;
begin
  Result.FCurrentStride := AStartStride;
  Result.FThreshold := AThreshold;
  Result.FMaxStride := AThreshold * 16;
  Result.FRequest := AThreshold - 1;
end;

{ TParallel }

class function TParallel.ForWorker(Sender: TObject; AEvent: TIteratorEvent; AStateEvent: TIteratorStateEvent; const AProc: TProc<Integer>;
  const AProcWithState: TProc<Integer, TLoopState>; ALowInclusive, AHighExclusive, AStride: Integer; APool: TThreadPool): TLoopResult;
var
  Index: Integer;
  SharedFlags: TLoopState32.TLoopStateFlag32;
  FinalFlags: TLoopState.TLoopStateFlagSet;
  RootTask: ITask;
  StrideMgr: TStrideManager;
begin
  if AHighExclusive <= ALowInclusive then
  begin
    Result.FCompleted := True;
    Result.FLowestBreakIteration := NULL;
    Exit;
  end;
  Index := ALowInclusive;
  if AStride <= 0 then AStride := 1;
  StrideMgr := TStrideManager.NewStrideManager(AStride, TThread.ProcessorCount);
  SharedFlags := TLoopState32.TLoopStateFlag32.Create;
  try
    RootTask := TReplicableTask.Create(nil, TNotifyEvent(nil),
      procedure
      var
        I, FromInclusive, ToExclusive, Stride: Integer;
        State: TLoopState32;
      begin
        if Assigned(AProcWithState) or Assigned(AStateEvent) then
          State := TLoopState32.Create(SharedFlags)
        else
          State := nil;
        try
          Stride := AStride;
          FromInclusive := TInterlocked.Add(Index, Stride) - Stride;
          try
            while FromInclusive < AHighExclusive do
            begin
              I := FromInclusive;
              ToExclusive := Min(FromInclusive + Stride, AHighExclusive);
              while (I < ToExclusive) and ((SharedFlags.LoopStateFlags = []) or not SharedFlags.ShouldExit) do
              begin

                //       This would make it so that only a single virtual call is made to process the iterations.
                if Assigned(AEvent) then
                  AEvent(Sender, I)
                else if Assigned(AProc) then
                  AProc(I)
                else if Assigned(AProcWithState) then
                begin
                  State.CurrentIteration := I;
                  AProcWithState(I, State);
                end
                else if Assigned(AStateEvent) then
                begin
                  State.CurrentIteration := I;
                  AStateEvent(Sender, I, State);
                end;
                Inc(I);
              end;
              Stride := StrideMgr.NextStride;
              FromInclusive := TInterlocked.Add(Index, Stride) - Stride;
              if (SharedFlags.LoopStateFlags <> []) and SharedFlags.ShouldExit(FromInclusive) then Break;
            end;
          except
            SharedFlags.SetFaulted;
            raise;
          end;
        finally
          State.Free;
        end;
      end, APool, nil, Min(TThread.ProcessorCount * 2, AHighExclusive - ALowInclusive), [TTask.TCreateFlag.Replicating]);
    RootTask.Start.Wait;
    FinalFlags := SharedFlags.LoopStateFlags;
    Result.FCompleted := FinalFlags = [];
    if FinalFlags <> [] then
      Result.FLowestBreakIteration := SharedFlags.FLowestBreakIter
    else
      Result.FLowestBreakIteration := NULL;
  finally
    SharedFlags.Free;
  end;
end;

class function TParallel.ForWorker64(Sender: TObject; AEvent: TIteratorEvent64; AStateEvent: TIteratorStateEvent64;
  const AProc: TProc<Int64>; const AProcWithState: TProc<Int64, TLoopState>; ALowInclusive, AHighExclusive,
  AStride: Int64; APool: TThreadPool): TLoopResult;
var
  Index: Int64;
  SharedFlags: TLoopState64.TLoopStateFlag64;
  FinalFlags: TLoopState.TLoopStateFlagSet;
  RootTask: ITask;
  StrideMgr: TStrideManager64;
begin
  if AHighExclusive <= ALowInclusive then
  begin
    Result.FCompleted := True;
    Result.FLowestBreakIteration := NULL;
    Exit;
  end;
  Index := ALowInclusive;
  if AStride <= 0 then AStride := 1;
  StrideMgr := TStrideManager64.NewStrideManager(AStride, TThread.ProcessorCount);
  SharedFlags := TLoopState64.TLoopStateFlag64.Create;
  try
    RootTask := TReplicableTask.Create(nil, TNotifyEvent(nil),
      procedure
      var
        I, FromInclusive, ToExclusive, Stride: Int64;
        State: TLoopState64;
      begin
        if Assigned(AProcWithState) or Assigned(AStateEvent) then
          State := TLoopState64.Create(SharedFlags)
        else
          State := nil;
        try
          Stride := AStride;
          FromInclusive := TInterlocked.Add(Index, Stride) - Stride;
          try
            while FromInclusive < AHighExclusive do
            begin
              I := FromInclusive;
              ToExclusive := Min(FromInclusive + Stride, AHighExclusive);
              while (I < ToExclusive) and ((SharedFlags.LoopStateFlags = []) or not SharedFlags.ShouldExit) do
              begin

                //       This would make it so that only a single virtual call is made to process the iterations.
                if Assigned(AEvent) then
                  AEvent(Sender, I)
                else if Assigned(AProc) then
                  AProc(I)
                else if Assigned(AProcWithState) then
                begin
                  State.CurrentIteration := I;
                  AProcWithState(I, State);
                end
                else if Assigned(AStateEvent) then
                begin
                  State.CurrentIteration := I;
                  AStateEvent(Sender, I, State);
                end;
                Inc(I);
              end;
              Stride := StrideMgr.NextStride;
              FromInclusive := TInterlocked.Add(Index, Stride) - Stride;
              if (SharedFlags.LoopStateFlags <> []) and SharedFlags.ShouldExit(FromInclusive) then Break;
            end;
          except
            SharedFlags.SetFaulted;
            raise;
          end;
        finally
          State.Free;
        end;
      end, APool, nil, [TTask.TCreateFlag.Replicating]);
    RootTask.Start.Wait;
    FinalFlags := SharedFlags.LoopStateFlags;
    Result.FCompleted := FinalFlags = [];
    if FinalFlags <> [] then
      Result.FLowestBreakIteration := SharedFlags.FLowestBreakIter
    else
      Result.FLowestBreakIteration := NULL;
  finally
    SharedFlags.Free;
  end;
end;

class function TParallel.&For(Sender: TObject; ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorEvent): TLoopResult;
begin
  Result := ForWorker(Sender, AIteratorEvent, nil, nil, nil, ALowInclusive, AHighInclusive + 1, 1, TThreadPool.Default);
end;

class function TParallel.&For(ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer>): TLoopResult;
begin
  Result := ForWorker(nil, nil, nil, AIteratorEvent, nil, ALowInclusive, AHighInclusive + 1, 1, TThreadPool.Default);
end;

class function TParallel.&For(ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer, TLoopState>): TLoopResult;
begin
  Result := ForWorker(nil, nil, nil, nil, AIteratorEvent, ALowInclusive, AHighInclusive + 1, 1, TThreadPool.Default);
end;

class function TParallel.&For(AStride, ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer, TLoopState>): TLoopResult;
begin
  Result := ForWorker(nil, nil, nil, nil, AIteratorEvent, ALowInclusive, AHighInclusive + 1, AStride, TThreadPool.Default);
end;

class function TParallel.&For(AStride, ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer, TLoopState>; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker(nil, nil, nil, nil, AIteratorEvent, ALowInclusive, AHighInclusive + 1, AStride, APool);
end;

class function TParallel.&For(Sender: TObject; ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorStateEvent; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker(Sender, nil, AIteratorEvent, nil, nil, ALowInclusive, AHighInclusive + 1, 1, APool);
end;

class function TParallel.&For(Sender: TObject; ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorStateEvent): TLoopResult;
begin
  Result := ForWorker(Sender, nil, AIteratorEvent, nil, nil, ALowInclusive, AHighInclusive + 1, 1, TThreadPool.Default);
end;

class function TParallel.&For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorStateEvent; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker(Sender, nil, AIteratorEvent, nil, nil, ALowInclusive, AHighInclusive + 1, AStride, APool);
end;

class function TParallel.&For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorStateEvent): TLoopResult;
begin
  Result := ForWorker(Sender, nil, AIteratorEvent, nil, nil, ALowInclusive, AHighInclusive + 1, AStride, TThreadPool.Default);
end;

class function TParallel.&For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorEvent64; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker64(Sender, AIteratorEvent, nil, nil, nil, ALowInclusive, AHighInclusive + 1, AStride, APool);
end;

class function TParallel.&For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorEvent64): TLoopResult;
begin
  Result := ForWorker64(Sender, AIteratorEvent, nil, nil, nil, ALowInclusive, AHighInclusive + 1, AStride, TThreadPool.Default);
end;

class function TParallel.&For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorStateEvent64; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker64(Sender, nil, AIteratorEvent, nil, nil, ALowInclusive, AHighInclusive + 1, AStride, APool);
end;

class function TParallel.&For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorStateEvent64): TLoopResult;
begin
  Result := ForWorker64(Sender, nil, AIteratorEvent, nil, nil, ALowInclusive, AHighInclusive + 1, AStride, TThreadPool.Default);
end;

class function TParallel.&For(Sender: TObject; ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorEvent64; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker64(Sender, AIteratorEvent, nil, nil, nil, ALowInclusive, AHighInclusive + 1, 1, APool);
end;

class function TParallel.&For(Sender: TObject; ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorEvent64): TLoopResult;
begin
  Result := ForWorker64(Sender, AIteratorEvent, nil, nil, nil, ALowInclusive, AHighInclusive + 1, 1, TThreadPool.Default);
end;

class function TParallel.&For(Sender: TObject; ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorStateEvent64; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker64(Sender, nil, AIteratorEvent, nil, nil, ALowInclusive, AHighInclusive + 1, 1, APool);
end;

class function TParallel.&For(Sender: TObject; ALowInclusive, AHighInclusive: Int64; AIteratorEvent: TIteratorStateEvent64): TLoopResult;
begin
  Result := ForWorker64(Sender, nil, AIteratorEvent, nil, nil, ALowInclusive, AHighInclusive + 1, 1, TThreadPool.Default);
end;

class function TParallel.&For(AStride, ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64>; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker64(nil, nil, nil, AIteratorEvent, nil, ALowInclusive, AHighInclusive + 1, AStride, APool);
end;

class function TParallel.&For(AStride, ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64>): TLoopResult;
begin
  Result := ForWorker64(nil, nil, nil, AIteratorEvent, nil, ALowInclusive, AHighInclusive + 1, AStride, TThreadPool.Default);
end;

class function TParallel.&For(AStride, ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64, TLoopState>; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker64(nil, nil, nil, nil, AIteratorEvent, ALowInclusive, AHighInclusive + 1, AStride, APool);
end;

class function TParallel.&For(AStride, ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64, TLoopState>): TLoopResult;
begin
  Result := ForWorker64(nil, nil, nil, nil, AIteratorEvent, ALowInclusive, AHighInclusive + 1, AStride, TThreadPool.Default);
end;

class function TParallel.&For(ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64>; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker64(nil, nil, nil, AIteratorEvent, nil, ALowInclusive, AHighInclusive + 1, 1, APool);
end;

class function TParallel.&For(ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64>): TLoopResult;
begin
  Result := ForWorker64(nil, nil, nil, AIteratorEvent, nil, ALowInclusive, AHighInclusive + 1, 1, TThreadPool.Default);
end;

class function TParallel.&For(ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64, TLoopState>; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker64(nil, nil, nil, nil, AIteratorEvent, ALowInclusive, AHighInclusive + 1, 1, APool);
end;

class function TParallel.&For(ALowInclusive, AHighInclusive: Int64; const AIteratorEvent: TProc<Int64, TLoopState>): TLoopResult;
begin
  Result := ForWorker64(nil, nil, nil, nil, AIteratorEvent, ALowInclusive, AHighInclusive + 1, 1, TThreadPool.Default);
end;

class function TParallel.&For(Sender: TObject; ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorEvent; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker(Sender, AIteratorEvent, nil, nil, nil, ALowInclusive, AHighInclusive + 1, 1, APool);
end;

class function TParallel.&For(ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer>; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker(nil, nil, nil, AIteratorEvent, nil, ALowInclusive, AHighInclusive + 1, 1, APool);
end;

class function TParallel.&For(ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer, TLoopState>; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker(nil, nil, nil, nil, AIteratorEvent, ALowInclusive, AHighInclusive + 1, 1, APool);
end;

class function TParallel.&For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorEvent): TLoopResult;
begin
  Result := ForWorker(Sender, AIteratorEvent, nil, nil, nil, ALowInclusive, AHighInclusive + 1, AStride, TThreadPool.Default);
end;

class function TParallel.&For(AStride, ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer>): TLoopResult;
begin
  Result := ForWorker(nil, nil, nil, AIteratorEvent, nil, ALowInclusive, AHighInclusive + 1, AStride, TThreadPool.Default);
end;

class function TParallel.&For(Sender: TObject; AStride, ALowInclusive, AHighInclusive: Integer; AIteratorEvent: TIteratorEvent; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker(Sender, AIteratorEvent, nil, nil, nil, ALowInclusive, AHighInclusive + 1, AStride, APool);
end;

class function TParallel.&For(AStride, ALowInclusive, AHighInclusive: Integer; const AIteratorEvent: TProc<Integer>; APool: TThreadPool): TLoopResult;
begin
  Result := ForWorker(nil, nil, nil, AIteratorEvent, nil, ALowInclusive, AHighInclusive + 1, AStride, APool);
end;

class function TParallel.Join(Sender: TObject; AEvent1, AEvent2: TNotifyEvent; APool: TThreadPool): ITask;
begin
  Result := TJoinTask.Create(Sender, [AEvent1, AEvent2], APool).Start;
end;

class function TParallel.Join(const AProc1, AProc2: TProc; APool: TThreadPool): ITask;
begin
  Result := TJoinTask.Create([AProc1, AProc2], APool).Start;
end;

class function TParallel.Join(Sender: TObject; AEvents: array of TNotifyEvent; APool: TThreadPool): ITask;
begin
  Result := TJoinTask.Create(Sender, AEvents, APool).Start;
end;

class function TParallel.Join(const AProcs: array of TProc; APool: TThreadPool): ITask;
begin
  Result := TJoinTask.Create(AProcs, APool).Start;
end;

class function TParallel.Join(const AProcs: array of TProc): ITask;
begin
  Result := TJoinTask.Create(AProcs, TThreadPool.Default).Start;
end;

class function TParallel.Join(const AProc1, AProc2: TProc): ITask;
begin
  Result := Join(AProc1, AProc2, TThreadPool.Default);
end;

class function TParallel.Join(Sender: TObject; AEvents: array of TNotifyEvent): ITask;
begin
  Result := TJoinTask.Create(Sender, AEvents, TThreadPool.Default).Start;
end;

class function TParallel.Join(Sender: TObject; AEvent1, AEvent2: TNotifyEvent): ITask;
begin
  Result := Join(Sender, AEvent1, AEvent2, TThreadPool.Default);
end;

{ TTask.TUnsafeTask }

class operator TTask.TUnsafeTask.Explicit(const ATask: TUnsafeTask): TTask;
begin
  Result := ATask.FTask;
end;

class operator TTask.TUnsafeTask.Implicit(const ATask: TTask): TUnsafeTask;
begin
  Result.FTask := ATask;
end;

{ TTask }

procedure TTask.AddChild;
begin
  TInterlocked.Increment(FTaskCountdown);
end;

procedure TTask.CallUserCode;
begin
  if Assigned(FEvent) then
    FEvent(FSender)
  else if Assigned(FProc) then
    FProc;
end;

class function TTask.CurrentTask: ITask;
begin
  Result := FCurrentTask;
end;

procedure TTask.Cancel;
begin
  if not IsComplete then
  begin
    SetTaskStop;
    UpdateStateAtomic([TOptionStateFlag.Canceled], [TOptionStateFlag.Faulted, TOptionStateFlag.Complete]);
  end;
end;

procedure TTask.CheckCanceled;
begin
  if TOptionStateFlag.Canceled in FState then
    raise EOperationCanceled.Create('Operation Canceled');
end;

procedure TTask.CheckFaulted;
var
  Exception: TObject;
begin
  Exception := GetExceptionObject;
  if Exception <> nil then
  begin
    SetRaisedState;
    raise Exception;
  end;
end;

procedure TTask.Complete(UserEventRan: Boolean);
var
  I: Integer;
begin
  if not UserEventRan then
    IntermediateCompletion
  else
  begin
    if ((FTaskCountdown = 1) and not IsReplicating) or (TInterlocked.Decrement(FTaskCountDown) = 0) then
      IntermediateCompletion
    else
      UpdateStateAtomic([TOptionStateFlag.ChildWait], [TOptionStateFlag.Faulted, TOptionStateFlag.Canceled, TOptionStateFlag.Complete]);
    if FFaultedChildren <> nil then
    begin
      TMonitor.Enter(FFaultedChildren);
      try
        for I := FFaultedChildren.Count - 1 downto 0 do
          if TTask(FFaultedChildren[I]).WasExceptionRaised then
            FFaultedChildren.Delete(I);
      finally
        TMonitor.Exit(FFaultedChildren);
      end;
    end;
  end;
end;

constructor TTask.Create(Sender: TObject; Event: TNotifyEvent; const AProc: TProc; const APool: TThreadPool; const AParent: TTask; CreateFlags: TCreateFlags);
begin
  inherited Create;
  FTaskCountdown := 1;
  if APool = nil then
    FPool := TThreadPool.Default
  else
    FPool := APool;
  FProc := AProc;
  FSender := Sender;
  FEvent := Event;
  FParentTask := AParent;
  FControlFlag := TThreadPool.NewControlFlag;
  if AParent <> nil then
    AParent.AddChild;
  try
    if TCreateFlag.Replicating in CreateFlags then
      Include(FState, TOptionStateFlag.Replicating);
    if TCreateFlag.Replica in CreateFlags then
      Include(FState, TOptionStateFlag.Replica);
  except
    if AParent <> nil then
      AParent.ForgetChild;
    raise;
  end;
end;

class constructor TTask.Create;
begin
  TThreadPool.ObjectCaches.AddObjectCache(TTask);
  CompletedFlag := TObject.Create;
end;

function TTask.CreateReplicaTask(const AProc: TProc; AParent: TTask; CreateFlags: TCreateFlags): TTask;
begin
  Result := TTask.Create(nil, TNotifyEvent(nil), AProc, FPool, AParent, CreateFlags);
end;

class function TTask.Create(const Proc: TProc): ITask;
begin
  Result := TTask.Create(nil, TNotifyEvent(nil), Proc, TThreadPool.Default, nil);
end;

class function TTask.Create(Sender: TObject; Event: TNotifyEvent): ITask;
begin
  Result := TTask.Create(Sender, Event, nil, TThreadPool.Default, nil);
end;

class function TTask.Create(const Proc: TProc; APool: TThreadPool): ITask;
begin
  Result := TTask.Create(nil, TNotifyEvent(nil), Proc, APool, nil);
end;

constructor TTask.Create;
begin
  //raise ENoConstructException.CreateRes(@sInvalidTaskConstruction);
  raise ENoConstructException.Create('@sInvalidTaskConstruction');
end;

class function TTask.Create(Sender: TObject; Event: TNotifyEvent; const APool: TThreadPool): ITask;
begin
  Result := TTask.Create(Sender, Event, nil, APool, nil);
end;

destructor TTask.Destroy;
begin
  try
    Cancel;
    if not IsComplete then
      Wait;
  except
    // if Wait isn't called from anyplace else any pending exceptions are simply tossed here;
  end;
  FDoneEvent.Free;
  FFaultedChildren.Free;
  if FCompleteEvents <> CompletedFlag then
    FCompleteEvents.Free;
  inherited;
end;

class destructor TTask.Destroy;
begin
  CompletedFlag.Free;
end;

class function TTask.DoWaitForAll(const Tasks: array of ITask; Timeout: LongWord): Boolean;
var
  I: Integer;
  Completed, Exceptions, Cancelled: Boolean;
  Task: TUnsafeTask;
  InternalTasks: TArray<TUnsafeTask>;
  Event: TCountdownEvent;
  CompleteProc: TProc<ITask>;
  ExceptionList: TList<System.SysUtils.Exception>;

  procedure AddExceptionsFromTask(var Exceptions: TList<System.SysUtils.Exception>; const ATask: TTask);
  var
    Exception: System.SysUtils.Exception;
  begin
    Exception := ATask.GetExceptionObject;
    if Exception is EAggregateException then
    begin
      if Exceptions = nil then
        Exceptions := TList<System.SysUtils.Exception>.Create;
      ATask.SetRaisedState;
      EAggregateException(Exception).ExtractExceptionsToList(Exceptions);
    end;
    Exception.Free;
  end;

begin
  Result := True;
  Exceptions := False;
  Cancelled := False;
  ExceptionList := nil;
  for I := High(Tasks) downto Low(Tasks) do
  begin
    Task.Value := TTask(Tasks[I]);
    if Task.Value = nil then
      //raise EArgumentNilException.CreateRes(@sWaitNilTask);
      raise EArgumentNilException.Create('@sWaitNilTask');
    Completed := Task.Value.IsComplete;
    if not Completed then
    begin
      if Timeout <> INFINITE then
      begin
        //InternalTasks := InternalTasks + [Task]
        SetLength(InternalTasks, Length(InternalTasks) + 1);
        InternalTasks[Length(InternalTasks) - 1] := Task;
      end
      else
      begin
        Completed := Task.Value.InternalExecuteNow and Task.Value.IsComplete;
        if not Completed then
        begin
          SetLength(InternalTasks, Length(InternalTasks) + 1);
          InternalTasks[Length(InternalTasks) - 1] := Task;
          //InternalTasks := InternalTasks + [Task];
        end;
      end;
    end;
    if Completed then
    begin
      if Task.Value.HasExceptions then
        Exceptions := True
      else if Task.Value.IsCanceled then
        Cancelled := True;
    end;
  end;
  if Length(InternalTasks) > 0 then
  begin
    Event := TCountdownEvent.Create(Length(InternalTasks));
    try
      CompleteProc :=
        procedure (ATask: ITask)
        begin
          Event.Signal;
        end;
      try
        for Task in InternalTasks do
          Task.Value.AddCompleteEvent(CompleteProc);
        Result := Event.WaitFor(Timeout) <> TWaitResult.wrTimeout;
      finally
        if not Result then
        begin
          for Task in InternalTasks do
            if not Task.Value.IsComplete then
              Task.Value.RemoveCompleteEvent(CompleteProc);
        end;
      end;
    finally
      Event.Free;
    end;
    if Result then
    begin
      for Task in InternalTasks do
      begin
        if Task.Value.HasExceptions then
          Exceptions := True
        else if Task.Value.IsCanceled then
          Cancelled := True;
      end;
    end;
  end;
  if not Result or (not Exceptions and not Cancelled) then
    Exit;
  if not Exceptions then
    //raise EOperationCanceled.CreateRes(@sOneOrMoreTasksCancelled);
    raise EOperationCanceled.Create('@sOneOrMoreTasksCancelled');
  try
    for I := Low(Tasks) to High(Tasks) do
      AddExceptionsFromTask(ExceptionList, TTask(Tasks[I]));
    if ExceptionList <> nil then
      raise EAggregateException.Create(ExceptionList.ToArray);
  finally
    ExceptionList.Free;
  end;
end;

class function TTask.DoWaitForAny(const Tasks: array of ITask; Timeout: LongWord): Integer;
var
  I: Integer;
  Task: TUnsafeTask;
  Event: TLightweightEvent;
  CompleteProc: TProc<ITask>;
  CompleteTask: ITask;
  WaitRes: TWaitResult;
begin
  Result := -1;
  for I := Low(Tasks) to High(Tasks) do
  begin
    Task.Value := TTask(Tasks[I]);
    if Task.Value = nil then
      //raise EArgumentNilException.CreateRes(@sWaitNilTask);
      raise EArgumentNilException.Create('@sWaitNilTask');
    if (Result = -1) and Task.Value.IsComplete then
      Result := I;
  end;
  if (Result = -1) and (Length(Tasks) > 0) then
  begin
    Event := TLightweightEvent.Create(False);
    try
      CompleteProc :=
        procedure (ATask: ITask)
        var
          LTask: ITask;
        begin
          LTask := ATask;
          if TInterlocked.CompareExchange(Pointer(CompleteTask), Pointer(LTask), Pointer(nil)) = nil then
            Pointer(LTask) := nil;
          Event.SetEvent;
          TTask(ATask).RemoveCompleteEvent(CompleteProc);
        end;
      for I := Low(Tasks) to High(Tasks) do
      begin
        Task.Value := TTask(Tasks[I]);
        if CompleteTask <> nil then
          Continue
        else if Task.Value.IsComplete then
        begin
          CompleteProc(Task.Value);
          Break;
        end else
          Task.Value.AddCompleteEvent(CompleteProc);
      end;
      WaitRes := Event.WaitFor(Timeout);
      for I := Low(Tasks) to High(Tasks) do
      begin
        Task.Value := TTask(Tasks[I]);
        if (Tasks[I] = CompleteTask) and (WaitRes <> TWaitResult.wrTimeout) then
          Result := I;
        if not Task.Value.IsComplete then
          Task.Value.RemoveCompleteEvent(CompleteProc);
      end;
      if (CompleteTask <> nil) and (WaitRes <> TWaitResult.wrTimeout) then
      begin
        CompleteTask.CheckCanceled;
        CompleteTask.Wait;
      end;
    finally
      Event.Free;
    end;
  end;
end;

procedure TTask.Execute;
begin
  if IsReplicating then
    ExecuteReplicates(Self)
  else
    try
      CallUserCode;
    except
      HandleException(Self, TObject(AcquireExceptionObject));
    end;
end;



procedure TTask.ExecuteReplicates(Root: TTask);
var

//  ReplicasQuitting: Boolean;
  ReplicaProc: TProc;
begin
//  ReplicasQuitting := False;
  ReplicaProc := procedure
    var
      CurrentTask, ChildTask: ITask;
    begin
      CurrentTask := CurrentTask;
      if not Root.ShouldCreateReplica {or ReplicasQuitting} then
        Exit;
      ChildTask := Root.CreateReplicaTask(ReplicaProc, Self, [TCreateFlag.Replicating, TCreateFlag.Replica]);
      ChildTask.Start;
      try
        Root.CallUserCode;
      except
        Root.HandleException(TTask(ChildTask), TObject(AcquireExceptionObject));
      end;
    end;
  ReplicaProc;
end;

procedure TTask.ExecuteWork;
begin
  try
    InternalWork(False);
  except
    HandleException(Self, TObject(AcquireExceptionObject));
    Complete(False);
  end;
end;

// Break out the final completion state here so it can be called from both normal completion logic and
// the cancellation completion logic, should that be supported
// This is also where we'd activate child continuations
procedure TTask.FinalCompletion;
begin
  if (FParentTask <> nil) and (TOptionStateFlag.Replica in FState) then
    FParentTask.HandleChildCompletion(Self);
  ProcessCompleteEvents;
end;

procedure TTask.ForgetChild;
begin
  TInterlocked.Decrement(FTaskCountdown);
end;

class function TTask.Future<T>(Sender: TObject; Event: TFunctionEvent<T>; APool: TThreadPool): IFuture<T>;
begin
  Result := TFuture<T>.Create(Sender, Event, nil, APool).Start;
end;

class function TTask.Future<T>(Sender: TObject; Event: TFunctionEvent<T>): IFuture<T>;
begin
  Result := TFuture<T>.Create(Sender, Event, nil, TThreadPool.Default).Start;
end;

class function TTask.Future<T>(const Func: TFunc<T>; APool: TThreadPool): IFuture<T>;
begin
  Result := TFuture<T>.Create(TObject(nil), TFunctionEvent<T>(nil), Func, APool).Start;
end;

class function TTask.Future<T>(const Func: TFunc<T>): IFuture<T>;
begin
  Result := TFuture<T>.Create(TObject(nil), TFunctionEvent<T>(nil), Func, TThreadPool.Default).Start;
end;

procedure TTask.SetComplete;
var
  Done: TLightweightEvent;
begin
  Done := FDoneEvent;
  if Done <> nil then
    Done.SetEvent;
end;

procedure TTask.AddCompleteEvent(const Proc: TProc<ITask>);
begin
  if not InternalAddCompleteEvent(Proc) then
    Proc(Self);
end;

procedure TTask.SetExceptionObject(const Exception: TObject);
begin
  FException := Exception;
end;

procedure TTask.SetRaisedState;
begin
  if (FParentTask <> nil) and (CurrentTask = FParentTask) then
    UpdateStateAtomic([TOptionStateFlag.Raised], []);
end;

procedure TTask.SetTaskStop;
begin
  FControlFlag.Increment;
end;

function TTask.ShouldCreateReplica: Boolean;
begin
  Result := True;
end;

function TTask.GetDoneEvent: TLightweightEvent;
var
  Event: TLightweightEvent;
  Completed: Boolean;
begin
  if FDoneEvent = nil then
  begin
    Completed := IsComplete;
    Event := TLightweightEvent.Create;
    if TInterlocked.CompareExchange<TLightweightEvent>(FDoneEvent, Event, nil) <> nil then
      Event.Free
    // did the completion state get set while we were allocating the event?
    else if not Completed and IsComplete then
      Event.SetEvent;
  end;
  Result := FDoneEvent;
end;

function TTask.GetExceptionObject: Exception;
var
  I, J: Integer;
  E: TArray<Exception>;
  Task: TTask;
begin
  if HasExceptions then
  begin
    TMonitor.Enter(FFaultedChildren);
    try
      SetLength(E, FFaultedChildren.Count);
      J := 0;
      for I := 0 to FFaultedChildren.Count - 1 do
      begin
        Task := TTask(FFaultedChildren[I]);
        if Task.FException is Exception then
          E[J] := Exception(Task.FException);
        Inc(J);
      end;
      if J < FFaultedChildren.Count then
        SetLength(E, J);
      FFaultedChildren.Clear;
    finally
      TMonitor.Exit(FFaultedChildren);
    end;
    Result := EAggregateException.Create(E);
  end else
    Result := nil;
end;

function TTask.GetHasExceptions: Boolean;
begin
  Result := (FFaultedChildren <> nil) and (FFaultedChildren.Count > 0);
end;

function TTask.GetIsCanceled: Boolean;
begin
  Result := (FState * [TOptionStateFlag.Canceled, TOptionStateFlag.Faulted]) = [TOptionStateFlag.Canceled];
end;

function TTask.GetIsComplete: Boolean;
begin
  Result := FState * [TOptionStateFlag.Complete, TOptionStateFlag.Canceled, TOptionStateFlag.Faulted] <> [];
end;

function TTask.GetIsQueued: Boolean;
begin
  Result := (FState * [TOptionStateFlag.Started, TOptionStateFlag.Canceled, TOptionStateFlag.Faulted, TOptionStateFlag.Complete]) = [TOptionStateFlag.Started];
end;

function TTask.GetIsReplicating: Boolean;
begin
  Result := FState * [TOptionStateFlag.Replicating, TOptionStateFlag.Replica] = [TOptionStateFlag.Replicating];
end;

function TTask.GetStatus: TTaskStatus;
var
  LFlags: TOptionStateFlags;
begin
  LFlags := FState;
  if TOptionStateFlag.Faulted in LFlags then
    Result := TTaskStatus.Exception
  else if TOptionStateFlag.Canceled in LFlags then
    Result := TTaskStatus.Canceled
  else if TOptionStateFlag.Complete in LFlags then
    Result := TTaskStatus.Completed
  else if TOptionStateFlag.ChildWait in LFlags then
    Result := TTaskStatus.WaitingForChildren
  else if TOptionStateFlag.CallbackRun in LFlags then
    Result := TTaskStatus.Running
  else if TOptionStateFlag.Started in LFlags then
    Result := TTaskStatus.WaitingToRun
  else
    Result := TTaskStatus.Created;
end;

function TTask.GetWasExceptionRaised: Boolean;
begin
  Result := TOptionStateFlag.Raised in FState;
end;

procedure TTask.HandleChildCompletion(const Task: TAbstractTask.IInternalTask);
var
  LTask: TTask;
begin
  LTask := TTask(Task);
  if (LTask <> nil) and LTask.HasExceptions and not LTask.WasExceptionRaised then
    HandleException(LTask, LTask.GetExceptionObject);
  if TInterlocked.Decrement(FTaskCountdown) = 0 then
    IntermediateCompletion;
end;

procedure TTask.HandleException(const ChildTask: ITask; const Exception: TObject);
var
  LList: TList<IInternalTask>;
  InternalTask: IInternalTask;
begin
  if FFaultedChildren = nil then
  begin
    LList := TInterlocked.CompareExchange<TList<IInternalTask>>(FFaultedChildren, TList<IInternalTask>.Create, nil);
    LList.Free;
  end;
  TMonitor.Enter(FFaultedChildren);
  try
    InternalTask := TTask(ChildTask);
    InternalTask.SetExceptionObject(Exception);
    FFaultedChildren.Add(InternalTask);
  finally
    TMonitor.Exit(FFaultedChildren);
  end;
end;

procedure TTask.IntermediateCompletion;
var
  CompletedState: TOptionStateFlags;
begin
  if HasExceptions then
    CompletedState := [TOptionStateFlag.Faulted]
//  else if CheckForCancellation then
//    CompletedState := [TOptionStateFlag.Canceled]
  else
    CompletedState := [TOptionStateFlag.Complete];
  TInterlocked.Exchange(Integer(FState), Integer(FState + CompletedState));

  SetComplete;

  FinalCompletion;
end;

function TTask.InternalAddCompleteEvent(const Proc: TProc<ITask>): Boolean;
var
  LList: TCompleteEvents;
begin
  if FCompleteEvents = nil then
  begin
    LList := TCompleteEvents.Create;
    LList := TCompleteEvents(TInterlocked.CompareExchange(FCompleteEvents, TObject(LList), TObject(nil)));
    LList.Free;
  end;
  if FCompleteEvents <> CompletedFlag then
  begin
    TMonitor.Enter(FCompleteEvents);
    try
      if FCompleteEvents is TCompleteEvents then
      begin
        TCompleteEvents(FCompleteEvents).Add(Proc);
        Exit(True);
      end;
    finally
      TMonitor.Exit(FCompleteEvents);
    end;
  end;
  Result := False;
end;

procedure TTask.InternalExecute(var CurrentTaskVar: TTask);
var
  PreviousTask: TTask;
begin
  PreviousTask := CurrentTaskVar;
  try
    CurrentTaskVar := Self;
    Execute;
    Complete(True);
  finally
    CurrentTaskVar := PreviousTask;
  end;
end;

function TTask.InternalExecuteNow: Boolean;
begin
  Result := IsQueued;
  if Result then
    Result := TryExecuteNow(Result);
end;

function TTask.InternalWork(CheckExecuting: Boolean): Boolean;
var
  PreviousState: TOptionStateFlags;
begin
  if CheckExecuting or (TOptionStateFlag.Replicating in FState) then
  begin
    PreviousState := [];
    if not UpdateStateAtomic([TOptionStateFlag.CallbackRun], [TOptionStateFlag.CallbackRun], PreviousState) and
      not (TOptionStateFlag.Canceled in FState) then
      Exit(False);
  end else
    Include(FState, TOptionStateFlag.CallbackRun);
  if not IsCanceled then
    InternalExecute(FCurrentTask);
  Result := True;
end;

function TTask.MarkAsStarted: Boolean;
begin
  Result := UpdateStateAtomic([TOptionStateFlag.Started], [TOptionStateFlag.Started, TOptionStateFlag.Canceled]);
end;

procedure TTask.ProcessCompleteEvents;
var
  List: TObject;
  Proc: TProc<ITask>;

  function ProcWrapper(const ATask: ITask; const AProc: TProc<ITask>): TProc;
  begin
    Result :=
      procedure
      begin
        AProc(ATask);
      end;
  end;

begin
  // assign to a local var so that the refcount is bumped under ARC
  List := CompletedFlag;
  // The atomic exchange here doesn't fiddle with the reference counts, but that's OK. See above comment.
  List := TInterlocked.Exchange(FCompleteEvents, List);
  if List is TCompleteEvents then
    for Proc in TCompleteEvents(List) do
      if Assigned(Proc) then
        if not (TOptionStateFlag.ChildWait in FState) then
          Proc(Self)
        else
          Run(ProcWrapper(Self, Proc), FPool);
end;

procedure TTask.QueueEvents;
begin
  FPool.QueueWorkItem(Self, True);
end;

class function TTask.Run(Sender: TObject; Event: TNotifyEvent; APool: TThreadPool): ITask;
begin
  Result := TTask.Create(Sender, Event, APool).Start;
end;

class function TTask.Run(Sender: TObject; Event: TNotifyEvent): ITask;
begin
  Result := TTask.Create(Sender, Event, TThreadPool.Default).Start;
end;

procedure TTask.RemoveCompleteEvent(const Proc: TProc<ITask>);
var
  I: Integer;
  Events: TCompleteEvents;
begin
  if FCompleteEvents <> nil then
  begin
    TMonitor.Enter(FCompleteEvents);
    try
      if FCompleteEvents is TCompleteEvents then
      begin
        Events := TCompleteEvents(FCompleteEvents);
        I := Events.IndexOf(Proc);
        if I > -1 then
        begin
          Events[I] := nil;
          if Events.Count > 128 then
            for I := Events.Count - 1 downto 0 do
              if not Assigned(Events[I]) then
                Events.Delete(I);
        end;
      end;
    finally
      TMonitor.Exit(FCompleteEvents);
    end;
  end;
end;

class function TTask.Run(const Func: TProc; APool: TThreadPool): ITask;
begin
  Result := TTask.Create(Func, APool).Start;
end;

class function TTask.Run(const Func: TProc): ITask;
begin
  Result := TTask.Create(Func, TThreadPool.Default).Start;
end;

function TTask.Start: ITask;
begin
  if IsComplete then
    //raise EInvalidOperation.CreateRes(@sCannotStartCompletedTask);
    raise EInvalidOperation.Create('@sCannotStartCompletedTask');
  if MarkAsStarted then
  try
    QueueEvents;
  except
    // Should we wrap the exception up and re-raise that?
    Complete(False);
    raise;
  end;
  Result := Self;
end;

class function TTask.TimespanToMilliseconds(const Timeout: TTimeSpan): Longword;
var
  Total: Int64;
begin
  Total := Trunc(Timeout.TotalMilliseconds);
  if (Total < 0) or (Total > $7FFFFFFF) then
    raise EArgumentOutOfRangeException.CreateResFmt(@sInvalidTimeoutValue, [string(Timeout)]);
  Result := Longword(Total);
end;

function TTask.TryExecuteNow(WasQueued: Boolean): Boolean;
begin
  Result := not WasQueued or FPool.TryRemoveWorkItem(Self);
  if Result then
    Result := InternalWork(False);
end;

function TTask.UpdateStateAtomic(NewState, InvalidStates: TOptionStateFlags): Boolean;
var
  OldState: TOptionStateFlags;
begin
  Result := UpdateStateAtomic(NewState, InvalidStates, OldState);
end;

function TTask.UpdateStateAtomic(NewState, InvalidStates: TOptionStateFlags; out OldState: TOptionStateFlags): Boolean;
var
  Spinner: TSpinWait;
begin
  Spinner.Reset;
  repeat
    OldState := FState;
    if (OldState * InvalidStates) <> [] then
      Exit(False);
    if TInterlocked.CompareExchange(Integer(FState), Integer(OldState + NewState), Integer(OldState)) = Integer(OldState) then
      Exit(True);
    Spinner.SpinCycle;
  until False;
end;

function TTask.Wait(Timeout: LongWord): Boolean;
begin
  Result := IsComplete;
  if not Result then
    Result := DoneEvent.WaitFor(Timeout) <> TWaitResult.wrTimeout;
  if Result then
  begin
    CheckCanceled;
    CheckFaulted;
  end;
end;

function TTask.Wait(const Timeout: TTimeSpan): Boolean;
begin
  Result := Wait(TimespanToMilliseconds(Timeout));
end;

class function TTask.WaitForAll(const Tasks: array of ITask; const Timeout: TTimeSpan): Boolean;
begin
  Result := DoWaitForAll(Tasks, TimespanToMilliseconds(Timeout));
end;

class function TTask.WaitForAll(const Tasks: array of ITask; Timeout: LongWord): Boolean;
begin
  Result := DoWaitForAll(Tasks, Timeout);
end;

class function TTask.WaitForAll(const Tasks: array of ITask): Boolean;
begin
  Result := DoWaitForAll(Tasks, INFINITE);
end;

class function TTask.WaitForAny(const Tasks: array of ITask): Integer;
begin
  Result := DoWaitForAny(Tasks, INFINITE);
end;

class function TTask.WaitForAny(const Tasks: array of ITask; const Timeout: TTimeSpan): Integer;
begin
  Result := DoWaitForAny(Tasks, TimespanToMilliseconds(Timeout));
end;

class function TTask.WaitForAny(const Tasks: array of ITask; Timeout: LongWord): Integer;
begin
  Result := DoWaitForAny(Tasks, Timeout);
end;

{ TTask<T> }

constructor TFuture<T>.Create(Sender: TObject; Event: TFunctionEvent<T>; const Func: TFunc<T>; APool: TThreadPool);
begin
  FEvent := Event;
  FFunc := Func;
  inherited Create(Sender, RunEvent, nil, APool, nil, []);
end;

class constructor TFuture<T>.Create;
begin
  TThreadPool.ObjectCaches.AddObjectCache(TFuture<T>);
end;

function TFuture<T>.GetValue: T;
begin
  Wait;
  Result := FResult;
end;

procedure TFuture<T>.RunEvent(Sender: TObject);
begin
  if Assigned(FEvent) then
    FResult := FEvent(Sender)
  else if Assigned(FFunc) then
    FResult := FFunc
  else
    FResult := Default(T);
end;

function TFuture<T>.Start: IFuture<T>;
begin
  inherited Start;
  Result := Self;
end;

{ TParallel.TReplicableTask }

class constructor TParallel.TReplicableTask.Create;
begin
  TThreadPool.ObjectCaches.AddObjectCache(TReplicableTask);
end;

constructor TParallel.TReplicableTask.Create(Sender: TObject; Event: TNotifyEvent; const AProc: TProc;
  const APool: TThreadPool; const AParent: TTask; AParallelism: Integer; CreateFlags: TTask.TCreateFlags);
begin
  inherited Create(Sender, Event, AProc, APool, AParent, CreateFlags);
  FParallelism := AParallelism;
end;

function TParallel.TReplicableTask.CreateReplicaTask(const AProc: TProc; AParent: TTask; CreateFlags: TTask.TCreateFlags): TTask;
begin
  Result := TParallel.TReplicaTask.Create(AProc, FPool, AParent, CreateFlags);
end;

procedure TParallel.TReplicableTask.SetParallelism;
begin
  if FParallelism <= 0 then
    FParallelism := TThread.ProcessorCount * 2;
end;

function TParallel.TReplicableTask.ShouldCreateReplica: Boolean;
begin
  if FParallelism = -1 then
    Result := True
  else if FParallelism > 0 then
  begin
    Dec(FParallelism);
    Result := True;
  end else
    Result := False;
end;

procedure TParallel.TReplicableTask.QueueEvents;
begin
  SetParallelism;
  inherited QueueEvents;
end;

{ TThreadPool }

constructor TThreadPool.Create;
begin
  inherited Create;
  FQueue := TQueue<IThreadPoolWorkItem>.Create;
  FQueues := TSparseArray<TWorkStealingQueue<IThreadPoolWorkItem>>.Create(16);
  FRetiredThreadWakeEvent := TLightweightEvent.Create;
  FMinLimitWorkerThreadCount := TThread.ProcessorCount;
  FMaxLimitWorkerThreadCount := TThread.ProcessorCount * MaxThreadsPerCPU;
  FThreads := TThreadList<TBaseWorkerThread>.Create;
  FThreads.Duplicates := dupIgnore;
end;

procedure TThreadPool.CreateMonitorThread;
var
  NewStatus, Status: TMonitorThreadStatus;
begin
  repeat
    Status := FMonitorThreadStatus;
    if Status = [] then
    begin
      repeat
        NewStatus := [TMonitorThreadStat.Created];
        Status := TMonitorThreadStatus(TInterlocked.CompareExchange(Integer(FMonitorThreadStatus), Integer(NewStatus), 0));
        if Status <> [] then
        begin
          if not (TMonitorThreadStat.NoWorkers in Status) then
            Exit
          else if TMonitorThreadStatus(TInterlocked.CompareExchange(Integer(FMonitorThreadStatus),
            Integer(Status - [TMonitorThreadStat.NoWorkers]), Integer(Status))) = Status then
            Exit;
        end else
          Break;
      until False;
      try
        TThreadPoolMonitor.Create(Self);
      except
        TInterlocked.Exchange(Integer(FMonitorThreadStatus), 0);
        raise;
      end;
    end else if TMonitorThreadStat.NoWorkers in Status then
    begin
      if TMonitorThreadStatus(TInterlocked.CompareExchange(Integer(FMonitorThreadStatus),
        Integer(Status - [TMonitorThreadStat.NoWorkers]), Integer(Status))) = Status then
        Exit;
    end else
      Exit;
  until False;
end;

procedure TThreadPool.DecIdleThreadCount;
begin
  TInterlocked.Decrement(FIdleWorkerThreadCount);
end;

procedure TThreadPool.DecWorkRequestCount;
begin
  TInterlocked.Decrement(FQueuedRequestCount);
end;

destructor TThreadPool.Destroy;
var
  I: Integer;
  Thread: TBaseWorkerThread;
  LocalFreeList: TList<TBaseWorkerThread>;
begin
  FShutdown := True;
  if FQueue <> nil then
  begin
    TMonitor.Enter(FQueue);
    try
      TMonitor.PulseAll(FQueue);
    finally
      TMonitor.Exit(FQueue);
    end;
  end;
  if FThreads <> nil then
  begin
    LocalFreeList := TList<TBaseWorkerThread>.Create;
    try
      with FThreads.LockList do
      try
        // Check each thread to see if it is already marked for termination and/or if it is hung.
        for I := 0 to Count - 1 do
        begin
          Thread := Items[I];
          LocalFreeList.Add(Thread);
        end;
      finally
        FThreads.UnlockList
      end;
      for I := 0 to LocalFreeList.Count - 1 do
        LocalFreeList[I].DisposeOf;
    finally
      LocalFreeList.Free;
    end;
  end;
  Assert(FWorkerThreadCount = 0);
  WaitMonitorThread;
  FThreads.Free;
  FQueue.Free;
  FQueues.Free;
  FRetiredThreadWakeEvent.Free;
  inherited;
end;

class constructor TThreadPool.Create;
begin
  FDefaultPool := TThreadPool.Create();
end;

class destructor TThreadPool.Destroy;
begin
  FDefaultPool.Free;
  FObjectCaches.Free;
end;

procedure TThreadPool.IncIdleThreadCount;
begin
  TInterlocked.Increment(FIdleWorkerThreadCount);
end;

function TThreadPool.SetMaxWorkerThreads(Value: Integer): Boolean;
begin
  Result := Value >= TThread.ProcessorCount;
  if Result then
    TInterlocked.Exchange(FMaxLimitWorkerThreadCount, Value);
end;

function TThreadPool.SetMinWorkerThreads(Value: Integer): Boolean;
begin
  Result := (Value > -1) and (Value < FMaxLimitWorkerThreadCount);
  if Result then
    TInterlocked.Exchange(FMinLimitWorkerThreadCount, Value);
end;

function TThreadPool.ShouldGrowPool: Boolean;
begin
  Result := (FWorkerThreadCount < FMinLimitWorkerThreadCount) and (FIdleWorkerThreadCount < FQueuedRequestCount) and
    (FWorkerThreadCount < Self.FMaxLimitWorkerThreadCount);
end;

function TThreadPool.TryRemoveWorkItem(const WorkerData: IThreadPoolWorkItem): Boolean;
begin
  Result := (QueueThread <> nil) and (QueueThread.WorkQueue <> nil);
  if Result then
    Result := QueueThread.WorkQueue.LocalFindAndRemove(WorkerData);
end;

function TThreadPool.CreateWorkerThread: TBaseWorkerThread;
begin
  TInterlocked.Increment(FWorkerThreadCount);
  Result := TQueueWorkerThread.Create(Self);
  FLastThreadCreationTick := TThread.GetTickCount;
end;

function TThreadPool.GetMaxWorkerThreads: Integer;
begin
  Result := FMaxLimitWorkerThreadCount;
end;

function TThreadPool.GetMinWorkerThreads: Integer;
begin
  Result := FMinLimitWorkerThreadCount;
end;

class function TThreadPool.GetObjectCaches: TObjectCaches;
begin
  if FObjectCaches = nil then
    FObjectCaches := TObjectCaches.Create([doOwnsValues], 5);
  Result := FObjectCaches;
end;

procedure TThreadPool.GrowWorkerPool;
begin
  if ShouldGrowPool then
  begin
    TMonitor.Enter(FQueue);
    try
      if ShouldGrowPool then
        if FRetiredWorkerThreadCount = 0 then
          CreateWorkerThread
        else
          ResurrectRetiredThread;
    finally
      TMonitor.Exit(FQueue);
    end;
  end else
    CreateMonitorThread;
end;

procedure TThreadPool.IncWorkRequestCount;
begin
  TInterlocked.Increment(FQueuedRequestCount);
end;

procedure TThreadPool.WaitMonitorThread;
var
  Status: TMonitorThreadStatus;
begin
  Status := FMonitorThreadStatus;
  if TMonitorThreadStat.Created in Status then
  begin
    repeat
      TThread.Sleep(MonitorThreadDelay div 2); // wait for half the monitor thread delay.. should loop at most once.
      Status := FMonitorThreadStatus;
    until Status = [];
  end;
end;

class function TThreadPool.NewControlFlag: IControlFlag;
begin
  Result := TControlFlag.Create;
end;

procedure TThreadPool.QueueWorkItem(Sender: TObject; WorkerEvent: TNotifyEvent; const AControlFlag: IControlFlag);
var
  WorkerData: TWorkerData;
begin
  WorkerData := TWorkerData.Create;
  try
    WorkerData.FWorkerEvent := WorkerEvent;
    WorkerData.FSender := Sender;
    if AControlFlag <> nil then
      WorkerData.FControlFlag := AControlFlag
    else
      WorkerData.FControlFlag := NewControlFlag;
  except
    WorkerData.Free;
    raise;
  end;
  QueueWorkItem(WorkerData, True);
end;

procedure TThreadPool.QueueWorkItem(const WorkerData: IThreadPoolWorkItem; UseLocalQueue: Boolean);
begin
  if (QueueThread <> nil) and UseLocalQueue then
  begin
    QueueThread.WorkQueue.LocalPush(WorkerData);
    IncWorkRequestCount;
    if FIdleWorkerThreadCount > 0 then
    begin
      TMonitor.Enter(FQueue);
      try
        TMonitor.Pulse(FQueue);
      finally
        TMonitor.Exit(FQueue);
      end;
    end else
      GrowWorkerPool;
  end else
  begin
    TMonitor.Enter(FQueue);
    try
      FQueue.Enqueue(WorkerData);
      IncWorkRequestCount;
      if FIdleWorkerThreadCount > 0 then
        TMonitor.Pulse(FQueue)
      else
        GrowWorkerPool;
    finally
      TMonitor.Exit(FQueue);
    end;
  end;
end;

procedure TThreadPool.QueueWorkItem(const WorkerEvent: TProc; const AControlFlag: IControlFlag);
var
  WorkerData: TWorkerData;
begin
  WorkerData := TWorkerData.Create;
  try
    WorkerData.FProc := WorkerEvent;
    if AControlFlag <> nil then
      WorkerData.FControlFlag := AControlFlag
    else
      WorkerData.FControlFlag := NewControlFlag;
  except
    WorkerData.Free;
    raise;
  end;
  QueueWorkItem(WorkerData, True);
end;

procedure TThreadPool.ResurrectRetiredThread;
begin
  FRetiredThreadWakeEvent.SetEvent;
end;

{ TThreadPool.TControlFlag }

constructor TThreadPool.TControlFlag.Create;
begin
  inherited Create;
  FControlFlag := -1;
end;

function TThreadPool.TControlFlag.Increment: Integer;
begin
  Result := TInterlocked.Increment(FControlFlag);
end;

{ TThreadPool.TBaseWorkerThread }

constructor TThreadPool.TBaseWorkerThread.Create(AThreadPool: TThreadPool);
begin
  inherited Create(False);
//  Priority := tpHigher;
  FRunningEvent := TLightweightEvent.Create(False);
  FThreadPool := AThreadPool;
  ThreadPool.FThreads.Add(Self);
end;

destructor TThreadPool.TBaseWorkerThread.Destroy;
begin
  if FRunningEvent <> nil then
    FRunningEvent.WaitFor(INFINITE); // This waits for the Execute to actually be called.
  if ThreadPool <> nil then
    ThreadPool.FThreads.Remove(Self);
  FRunningEvent.Free;
  inherited Destroy;
end;

procedure TThreadPool.TBaseWorkerThread.SafeTerminate;
begin
  if ThreadPool <> nil then
    ThreadPool.FThreads.Remove(Self);
  FreeOnTerminate := True;
  Terminate;
end;

procedure TThreadPool.TBaseWorkerThread.Execute;
begin
  NameThreadForDebugging(Format('Worker Thread - %s #%d', [ClassName, TInterlocked.Increment(WorkerThreadID)]));
  FRunningEvent.SetEvent;
end;

{ TThreadPool.TQueueWorkerThread }

constructor TThreadPool.TQueueWorkerThread.Create(AThreadPool: TThreadPool);
begin
  inherited Create(AThreadPool);
  FWorkQueue := TWorkStealingQueue<IThreadPoolWorkItem>.Create;
  FThreadSuspended := TSafeSharedInteger.Create(AThreadPool.FThreadSuspended);
  FLastSuspendTick := TSafeSharedCardinal.Create(AThreadPool.FLastSuspendTick);
  FRetiredThreadWakeEvent := AThreadPool.FRetiredThreadWakeEvent;
end;

destructor TThreadPool.TQueueWorkerThread.Destroy;
begin
  inherited Destroy;
  FWorkQueue.Free;
end;

procedure TThreadPool.TQueueWorkerThread.Execute;
const
  IdleTimeout = 40*1000; // Timeout waiting for work
  NoWorkTimeout = 10*1000; // Timeout for no work available
var
  I, WaitTime: Integer;
  Item: IThreadPoolWorkItem;
  LookedForSteals, ShouldTerminate, Signaled, Idle: Boolean;
  OldStatus: TThreadPool.TMonitorThreadStatus;
begin
  TThreadPool.QueueThread := Self;
  try
    inherited Execute;
    ThreadPool.FQueues.Add(WorkQueue);
    try
      WaitTime := IdleTimeout;
      ShouldTerminate := False;
      Idle := False;
      FLastSuspendTick.Value := TThread.GetTickCount;
      while True do
      begin
        Item := Default(IThreadPoolWorkItem);
        if not WorkQueue.LocalPop(Item) then
        begin
          LookedForSteals := False;
          while True do
          begin
            TMonitor.Enter(ThreadPool.FQueue);
            try
              if ThreadPool.FShutDown and (ThreadPool.FQueuedRequestCount = 0) then
                Signaled := False
              else if ThreadPool.FQueue.Count > 0 then
              begin
                Item := ThreadPool.FQueue.Dequeue;
                Signaled := True;
                Break;
              end else if LookedForSteals then
              begin
                if not Idle then
                  ThreadPool.IncIdleThreadCount;
                Idle := True;
                Signaled := TMonitor.Wait(ThreadPool.FQueue, WaitTime) and not ThreadPool.FShutdown;
                LookedForSteals := False;
                if Signaled then
                  Continue;
              end else
                Signaled := True;
            finally
              TMonitor.Exit(ThreadPool.FQueue);
            end;
            if Signaled then
            begin
              I := 0;
              while I < Length(ThreadPool.FQueues.Current) do
              begin
                if (ThreadPool.FQueues.Current[I] <> nil) and (ThreadPool.FQueues.Current[I] <> WorkQueue) and ThreadPool.FQueues.Current[I].TrySteal(Item) then
                  Break;
                Inc(I);
              end;
              if I <> Length(ThreadPool.FQueues.Current) then
                Break;
              LookedForSteals := True;
            end else if not ThreadPool.FShutdown then
            begin
              if ThreadPool.FWorkerThreadCount > 1 then
              begin
                if (ThreadPool.FQueuedRequestCount < 7 * (ThreadPool.FWorkerThreadCount - 1)) and (WaitTime > 2 * IdleTimeout) then
                  ShouldTerminate := True
                else
                begin
                  WaitTime := (WaitTime shl 1) and MaxInt;
                  Inc(WaitTime, 1000);
                end;
              end else
              begin
                if ThreadPool.FQueuedRequestCount = 0 then
                  if WaitTime < 4 * IdleTimeout then
                  begin
                    WaitTime := (WaitTime shl 1) and MaxInt;
                    Inc(WaitTime, 1000);
                  end else
                    ShouldTerminate := True;
              end;
              if ShouldTerminate then
                Break;
            end else
            begin
              ShouldTerminate := True;
              Break;
            end;
          end;
        end;
        if Item <> nil then
        begin
          if Idle then
            ThreadPool.DecIdleThreadCount;
          Idle := False;
          ThreadPool.DecWorkRequestCount;
          if Item.ShouldExecute then
          begin
            Item := nil;
            Continue;
          end;
          ExecuteWorkItem(Item);
          if FThreadSuspended.Value = 0 then
          begin
            if ((ThreadPool.FWorkerThreadCount - ThreadPool.FIdleWorkerThreadCount) > 2 * ThreadPool.FMinLimitWorkerThreadCount) and
              (ThreadPool.FAverageCPUUsage >= TThreadPool.CPUUsageHigh) and
              (TThread.GetTickCount > FLastSuspendTick.Value + TThreadPool.SuspendInterval) then
            begin
              // This will keep too many threads from suspending at the same time which could decrease
              // the pool's throughput
              if FThreadSuspended.CompareExchange(1, 0) = 0 then
              begin
                Assert(FThreadSuspended.Value = 1);
                ShouldTerminate := SuspendWork;
                FLastSuspendTick.Value := TThread.GetTickCount;
                FThreadSuspended.Value := 0;
              end;
            end;
          end;
        end;
        if ShouldTerminate then
        begin
          if not Idle then
            ThreadPool.IncIdleThreadCount;
          Idle := True;
          ShouldTerminate := ThreadPool.FShutdown or TryToRetire;
          if ShouldTerminate then
          begin
            TMonitor.Enter(ThreadPool.FQueue);
            try
              if ThreadPool.FWorkerThreadCount = 1 then
              begin
                if ThreadPool.FQueuedRequestCount = 0 then
                begin
                  while True do
                  begin
                    OldStatus := ThreadPool.FMonitorThreadStatus;
                    if (OldStatus = []) or (TMonitorThreadStatus(TInterlocked.CompareExchange(Integer(ThreadPool.FMonitorThreadStatus),
                      Integer(OldStatus + [TThreadPool.TMonitorThreadStat.NoWorkers]), Integer(OldStatus))) = OldStatus) then
                      Break;
                  end;
                end else
                  Continue;
              end;
              PushLocalWorkToGlobal;
              TInterlocked.Decrement(ThreadPool.FWorkerThreadCount);
              if Idle then
                ThreadPool.DecIdleThreadCount;
              Idle := False;
              Break;
            finally
              TMonitor.Exit(ThreadPool.FQueue);
            end;
          end;
        end;
      end;
    finally
      if ThreadPool <> nil then
        ThreadPool.FQueues.Remove(WorkQueue);
    end;
{$IFDEF MSWINDOWS}
    OutputDebugString(PChar(Format('Thread Exiting: %d', [ThreadId])));
{$ENDIF}
  finally
    TThreadPool.QueueThread := nil;
  end;
end;

procedure TThreadPool.TQueueWorkerThread.ExecuteWorkItem(var Item: IThreadPoolWorkItem);
begin
  try
    try
      Item.ExecuteWork;
    finally
      Item := nil;
    end;
  except
    // ???figure out error handling and exception passing system???
  end;
end;

procedure TThreadPool.TQueueWorkerThread.PushLocalWorkToGlobal;
var
  Item: TThreadPool.IThreadPoolWorkItem;
begin
  while FWorkQueue.LocalPop(Item) do
    ThreadPool.QueueWorkItem(Item, False);
end;

function TThreadPool.TQueueWorkerThread.SuspendWork: Boolean;
var
  I: Integer;
  ShouldTerminate: Integer;
  OldUsage: Integer;
begin
  ShouldTerminate := 1;
  I := 0;
  OldUsage := ThreadPool.FCurrentCPUUsage;
  while I < ShouldTerminate do
  begin
    TThread.Sleep(TThreadPool.SuspendTime);
    // if usage dips by 4% or more keep this thread in play
    if ThreadPool.FCurrentCPUUsage < OldUsage - 4 then
    begin
      ShouldTerminate := 0;
      Break;
    end;
    Inc(I);
  end;
  Result := ShouldTerminate <> 0;
end;

function TThreadPool.TQueueWorkerThread.TryToRetire: Boolean;
const
  MaxRetirementDelay = TThreadPool.RetirementDelay * 60;
var
  WaitInterval: Integer;
  WaitResult: TWaitResult;
begin
  TInterlocked.Increment(ThreadPool.FRetiredWorkerThreadCount);
  Result := True;
  WaitInterval := TThreadPool.RetirementDelay;
  while True do
  begin
    WaitResult := FRetiredThreadWakeEvent.WaitFor(WaitInterval);
    if (WaitResult = TWaitResult.wrTimeout) and (WaitInterval < MaxRetirementDelay) then
    begin
      if FWorkQueue.Count = 0 then
      begin
        Result := True;
        Break;
      end else
        WaitInterval := Min(2 * WaitInterval, MaxRetirementDelay);
    end else
    begin
      Result := False;
      Break;
    end;
  end;
  TInterlocked.Decrement(ThreadPool.FRetiredWorkerThreadCount);
end;

{ TThreadPool.TAbstractWorkerData }

procedure TThreadPool.TAbstractWorkerData.FreeInstance;
var
  FCache: TObjectCache;
begin
  CleanupInstance;
  if TThreadPool.FObjectCaches.TryGetValue(Self.ClassType, FCache) then
    if FCache.Insert(Pointer(Self)) then
      Exit;
  FreeMem(Pointer(Self));
end;

class function TThreadPool.TAbstractWorkerData.NewInstance: TObject;
var
  FCache: TObjectCache;
  Instance: Pointer;
begin
  if TThreadPool.FObjectCaches.TryGetValue(Self, FCache) then
  begin
    Instance := FCache.Remove;
    if Instance <> nil then
    begin
      Result := InitInstance(Instance);
      TAbstractWorkerData(Result).FRefCount := 1;
      Exit;
    end;
  end;
  Result := inherited NewInstance;
end;

function TThreadPool.TAbstractWorkerData.ShouldExecute: Boolean;
begin
  Result := FControlFlag.Increment > 0;
end;

{ TThreadPool.TWorkerData }

class constructor TThreadPool.TWorkerData.Create;
begin
  TThreadPool.ObjectCaches.AddObjectCache(TWorkerData);
end;

procedure TThreadPool.TWorkerData.ExecuteWork;
begin
  if Assigned(FWorkerEvent) then
    FWorkerEvent(FSender)
  else if Assigned(FProc) then
    FProc;
end;

{ TParallel.TJoinTask }

class function TParallel.TJoinTask.Create(Sender: TObject; AEvents: array of TNotifyEvent; APool: TThreadPool): ITask;
begin
  Result := TJoinTask.InternalCreate(Sender, AEvents, [], APool);
end;

class function TParallel.TJoinTask.Create(const AProcs: array of TProc; APool: TThreadPool): ITask;
begin
  Result := TJoinTask.InternalCreate(nil, [], AProcs, APool);
end;

class constructor TParallel.TJoinTask.Create;
begin
  TThreadPool.ObjectCaches.AddObjectCache(TJoinTask);
end;

constructor TParallel.TJoinTask.InternalCreate(Sender: TObject; AEvents: array of TNotifyEvent; const AProcs: array of TProc; APool: TThreadPool);
var
  I: Integer;
  RunProc: TProc;
begin
  SetLength(FEvents, Length(AEvents));
  for I := Low(AEvents) to High(AEvents) do
    FEvents[I] := AEvents[I];
  SetLength(FProcs, Length(AProcs));
  for I := Low(AProcs) to High(AProcs) do
    FProcs[I] := AProcs[I];
  if Length(FEvents) > 0 then
    RunProc :=
      procedure
      begin
        TParallel.For(Low(FEvents), High(FEvents),
          procedure (Index: Integer)
          begin
            FEvents[Index](FSender);
          end);
      end
  else if Length(FProcs) > 0 then
    RunProc :=
      procedure
      begin
        TParallel.For(Low(FProcs), High(FProcs),
          procedure (Index: Integer)
          begin
            FProcs[Index]();
          end);
      end
  else
    //raise EArgumentOutOfRangeException.CreateRes(@sEmptyJoinTaskList);
    raise EArgumentOutOfRangeException.Create('@sEmptyJoinTaskList');
  inherited Create(Sender, TNotifyEvent(nil), RunProc, APool, nil, 1, []);
end;

{ TSparseArray<T> }

function TSparseArray<T>.Add(const Item: T): Integer;
var
  I: Integer;
  LArray, NewArray: TArray<T>;
begin
  while True do
  begin
    LArray := FArray;
    TMonitor.Enter(FLock);
    try
      for I := 0 to Length(LArray) - 1 do
      begin
        if LArray[I] = nil then
        begin
          FArray[I] := Item;
          Exit(I);
        end else if I = Length(LArray) - 1 then
        begin
          if LArray <> FArray then
            Continue;
          SetLength(NewArray, Length(LArray) * 2);
          //TArray.Copy<T>(LArray, NewArray, I + 1);
          NewArray[I + 1] := Item;
          Exit(I + 1);
        end;
      end;
    finally
      TMonitor.Exit(FLock);
    end;
  end;
end;

constructor TSparseArray<T>.Create(InitialSize: Integer);
begin
  inherited Create;
  FLock := TObject.Create;
  SetLength(FArray, InitialSize);
end;

destructor TSparseArray<T>.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TSparseArray<T>.Remove(const Item: T);
var
  I: Integer;
begin
  TMonitor.Enter(FLock);
  try
    for I := 0 to Length(FArray) - 1 do
      if FArray[I] = Item then
      begin
        FArray[I] := nil;
        Exit;
      end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

{ TParallel.TLoopState.TLoopStateFlagRec }

function TParallel.TLoopState.TLoopStateFlag.AtomicUpdate(NewState, InvalidStates: TLoopStateFlagSet): Boolean;
var
  OldState: TLoopStateFlagSet;
begin
  Result := AtomicUpdate(NewState, InvalidStates, OldState);
end;

function TParallel.TLoopState.TLoopStateFlag.AtomicUpdate(NewState, InvalidStates: TLoopStateFlagSet; var OldState: TLoopStateFlagSet): Boolean;
var
  SW: TSpinWait;
begin
  SW.Reset;
  repeat
    OldState := FLoopStateFlags;
    if OldState * InvalidStates <> [] then
      Exit(False);
    if TInterlocked.CompareExchange(Integer(FLoopStateFlags), Integer(NewState + OldState), Integer(OldState)) = Integer(OldState) then
      Exit(True);
    SW.SpinCycle;
  until False;
end;

//function TParallel.TLoopState.TLoopStateFlag.Cancel: Boolean;
//begin
//  Result := AtomicUpdate([TLoopStateFlags.Cancelled], []);
//end;

function TParallel.TLoopState.TLoopStateFlag.GetLoopStateFlags: TLoopStateFlagSet;
begin
  Result := FLoopStateFlags;
end;

procedure TParallel.TLoopState.TLoopStateFlag.SetFaulted;
begin
  AtomicUpdate([TLoopStateFlags.Exception], []);
end;

procedure TParallel.TLoopState.TLoopStateFlag.Stop;
begin
  if not AtomicUpdate([TLoopStateFlags.Stopped], [TLoopStateFlags.Broken]) then
  raise EInvalidOperation.Create('@sStopAfterBreak');
    //raise EInvalidOperation.CreateRes(@sStopAfterBreak);
end;

{ TParallel.TLoopState }

procedure TParallel.TLoopState.Break;
begin
  DoBreak;
end;

constructor TParallel.TLoopState.Create(ALoopStateFlags: TLoopStateFlag);
begin
  inherited Create;
  FLoopStateFlags := ALoopStateFlags;
end;

procedure TParallel.TLoopState.DoBreak;
begin
  Assert(False);
end;

function TParallel.TLoopState.DoGetLowestBreakIteration: Variant;
begin
  Assert(False);
end;

function TParallel.TLoopState.DoShouldExit: Boolean;
begin
  Result := True;
  Assert(False);
end;

function TParallel.TLoopState.GetFaulted: Boolean;
begin
  Result := TLoopStateFlags.Exception in FLoopStateFlags.LoopStateFlags;
end;

function TParallel.TLoopState.GetStopped: Boolean;
begin
  Result := TLoopStateFlags.Stopped in FLoopStateFlags.LoopStateFlags;
end;

function TParallel.TLoopState.GetLowestBreakIteration: Variant;
begin
  Result := DoGetLowestBreakIteration;
end;

function TParallel.TLoopState.ShouldExit: Boolean;
begin
  Result := DoShouldExit;
end;

procedure TParallel.TLoopState.Stop;
begin
  FLoopStateFlags.Stop;
end;

{ TParallel.TLoopState32.TLoopStateFlag32 }

constructor TParallel.TLoopState32.TLoopStateFlag32.Create;
begin
  inherited Create;
  FLowestBreakIter := MaxInt;
end;

function TParallel.TLoopState32.TLoopStateFlag32.GetLowestBreakIter: Integer;
begin
  Result := FLowestBreakIter;
end;

function TParallel.TLoopState32.TLoopStateFlag32.ShouldExit(ThisIter: Integer): Boolean;
var
  Flags: TLoopStateFlagSet;
begin
  Flags := LoopStateFlags;
  Result := (Flags <> []) and ((Flags * [TLoopStateFlags.Exception, TLoopStateFlags.Stopped, TLoopStateFlags.Cancelled] <> []) or
    ((TLoopStateFlags.Broken in Flags) and (ThisIter > LowestBreakIter)));
end;

function TParallel.TLoopState32.TLoopStateFlag32.ShouldExit: Boolean;
var
  Flags: TLoopStateFlagSet;
begin
  Flags := LoopStateFlags;
  Result := (Flags <> []) and (Flags * [TLoopStateFlags.Exception, TLoopStateFlags.Cancelled] <> []);
end;

{ TParallel.TReplicaTask }

constructor TParallel.TReplicaTask.Create(const AProc: TProc; APool: TThreadPool; AParent: TTask; CreateFlags: TTask.TCreateFlags);
begin
  inherited Create(nil, TNotifyEvent(nil), AProc, APool, AParent, CreateFlags);
end;

class constructor TParallel.TReplicaTask.Create;
begin
  TThreadPool.ObjectCaches.AddObjectCache(TReplicaTask);
end;

{ TParallel.TLoopState32 }

constructor TParallel.TLoopState32.Create(ALoopStateFlags: TLoopStateFlag32);
begin
  inherited Create(ALoopStateFlags);
  FSharedLoopStateFlags := ALoopStateFlags;
end;

procedure TParallel.TLoopState32.DoBreak;
var
  OldValue: TLoopStateFlagSet;
  LowestBreak: Integer;
  Spinner: TSpinWait;
begin
  OldValue := [];
  if not FSharedLoopStateFlags.AtomicUpdate([TLoopStateFlags.Broken], [TLoopStateFlags.Exception, TLoopStateFlags.Stopped, TLoopStateFlags.Cancelled], OldValue) then
  begin
    if TLoopStateFlags.Stopped in OldValue then
      //raise EInvalidOperation.CreateRes(@sBreakAfterStop)
      raise EInvalidOperation.Create('@sBreakAfterStop')
    else
      Exit;
  end;
  LowestBreak := FSharedLoopStateFlags.FLowestBreakIter;
  if CurrentIteration < LowestBreak then
  begin
    Spinner.Reset;
    while TInterlocked.CompareExchange(FSharedLoopStateFlags.FLowestBreakIter, CurrentIteration, LowestBreak) <> LowestBreak do
    begin
      Spinner.SpinCycle;
      LowestBreak := FSharedLoopStateFlags.FLowestBreakIter;
      if CurrentIteration > LowestBreak then
        Break;
    end;
  end;
end;

function TParallel.TLoopState32.DoGetLowestBreakIteration: Variant;
begin
  if FSharedLoopStateFlags.LowestBreakIter = MaxInt then
    Result := System.Variants.Null
  else
    Result := FSharedLoopStateFlags.LowestBreakIter;
end;

function TParallel.TLoopState32.DoShouldExit: Boolean;
begin
  Result := FSharedLoopStateFlags.ShouldExit(CurrentIteration);
end;

{ TParallel.TLoopState64 }

constructor TParallel.TLoopState64.Create(ALoopStateFlags: TLoopStateFlag64);
begin
  inherited Create(ALoopStateFlags);
  FSharedLoopStateFlags := ALoopStateFlags;
end;

procedure TParallel.TLoopState64.DoBreak;
var
  OldValue: TLoopStateFlagSet;
  LowestBreak: Int64;
  Spinner: TSpinWait;
begin
  OldValue := [];
  if not FSharedLoopStateFlags.AtomicUpdate([TLoopStateFlags.Broken], [TLoopStateFlags.Exception, TLoopStateFlags.Stopped, TLoopStateFlags.Cancelled], OldValue) then
  begin
    if TLoopStateFlags.Stopped in OldValue then
      //raise EInvalidOperation.CreateRes(@sBreakAfterStop)
      raise EInvalidOperation.Create('@sBreakAfterStop')
    else
      Exit;
  end;
  LowestBreak := FSharedLoopStateFlags.FLowestBreakIter;
  if CurrentIteration < LowestBreak then
  begin
    Spinner.Reset;
    while TInterlocked.CompareExchange(FSharedLoopStateFlags.FLowestBreakIter, CurrentIteration, LowestBreak) <> LowestBreak do
    begin
      Spinner.SpinCycle;
      LowestBreak := FSharedLoopStateFlags.FLowestBreakIter;
      if CurrentIteration > LowestBreak then
        Break;
    end;
  end;
end;

function TParallel.TLoopState64.DoGetLowestBreakIteration: Variant;
begin
  if FSharedLoopStateFlags.LowestBreakIter = High(Int64) then
    Result := System.Variants.Null
  else
    Result := FSharedLoopStateFlags.LowestBreakIter;
end;

function TParallel.TLoopState64.DoShouldExit: Boolean;
begin
  Result := FSharedLoopStateFlags.ShouldExit(CurrentIteration);
end;

{ TParallel.TLoopState64.TLoopStateFlag64 }

constructor TParallel.TLoopState64.TLoopStateFlag64.Create;
begin
  inherited Create;
  FLowestBreakIter := High(Int64);
end;

function TParallel.TLoopState64.TLoopStateFlag64.GetLowestBreakIter: Int64;
begin
  Result := FLowestBreakIter;
end;

function TParallel.TLoopState64.TLoopStateFlag64.ShouldExit(ThisIter: Int64): Boolean;
var
  Flags: TLoopStateFlagSet;
begin
  Flags := LoopStateFlags;
  Result := (Flags <> []) and ((Flags * [TLoopStateFlags.Exception, TLoopStateFlags.Stopped, TLoopStateFlags.Cancelled] <> []) or
    ((TLoopStateFlags.Broken in Flags) and (ThisIter > LowestBreakIter)));
end;

function TParallel.TLoopState64.TLoopStateFlag64.ShouldExit: Boolean;
var
  Flags: TLoopStateFlagSet;
begin
  Flags := LoopStateFlags;
  Result := (Flags <> []) and (Flags * [TLoopStateFlags.Exception, TLoopStateFlags.Cancelled] <> []);
end;

{ TThreadPool.TThreadPoolMonitor }

constructor TThreadPool.TThreadPoolMonitor.Create(AThreadPool: TThreadPool);
begin
  inherited Create(False);
  FThreadPool := AThreadPool;
  FreeOnTerminate := True;
end;

procedure TThreadPool.TThreadPoolMonitor.Execute;
const
  MaxInactiveInterval = 30 * 1000;
  InactiveCountdown = MaxInactiveInterval div TThreadPool.MonitorThreadDelay;
var
  I: Integer;
  CPUInfo: TThread.TSystemTimes;
  CpuUsageArray: array[0..TThreadPool.NumCPUUsageSamples - 1] of Cardinal;
  CurUsageSlot: Integer;
  ExitCountdown: Integer;
  AvgCPU: Cardinal;
  CurMonitorStatus: TThreadPool.TMonitorThreadStatus;
begin
  NameThreadForDebugging(Format('Thread Pool Monitor Thread - %s', [ClassName]));
  TThread.Sleep(TThreadPool.MonitorThreadDelay);
  TThread.GetSystemTimes(CPUInfo);
  CurUsageSlot := 0;
  FillChar(CPUUsageArray, SizeOf(CPUUsageArray), 0);
  ExitCountdown := InactiveCountdown;
  while not Terminated do
  begin
    if not FThreadPool.FShutdown then
    begin
      TThread.Sleep(TThreadPool.MonitorThreadDelay);
      FThreadPool.FCurrentCPUUsage := TThread.GetCPUUsage(CPUInfo);
      CPUUsageArray[CurUsageSlot] := FThreadPool.FCurrentCPUUsage;
      if CurUsageSlot = TThreadPool.NumCPUUsageSamples - 1 then
        CurUsageSlot := 0
      else
        Inc(CurUsageSlot);
      AvgCPU := 0;
      for I := 0 to TThreadPool.NumCPUUsageSamples - 1 do
        Inc(AvgCPU, CPUUsageArray[I]);
      FThreadPool.FAverageCPUUsage := AvgCPU div TThreadPool.NumCPUUsageSamples;
      if FThreadPool.FCurrentCPUUsage < TThreadPool.CPUUsageLow then
        GrowThreadPoolIfStarved;
      CurMonitorStatus := FThreadPool.FMonitorThreadStatus;
      if FThreadPool.FShutdown then
        ExitCountdown := -1
      else if not (TThreadPool.TMonitorThreadStat.NoWorkers in CurMonitorStatus) then
        Dec(ExitCountdown)
      else
        ExitCountdown := InactiveCountdown;
    end else
      ExitCountdown := -1;
    if ExitCountdown <= 0 then
    begin
      if ExitCountdown < 0 then
      begin
        TInterlocked.Exchange(Integer(FThreadPool.FMonitorThreadStatus), 0);
        Exit;
      end else if TMonitorThreadStatus(TInterlocked.CompareExchange(Integer(FThreadPool.FMonitorThreadStatus),
        0, Integer(CurMonitorStatus))) = CurMonitorStatus then
        Exit
      else
        ExitCountdown := InactiveCountdown;
    end;
  end;
end;

procedure TThreadPool.TThreadPoolMonitor.GrowThreadPoolIfStarved;
var
  LastWorkCount: Integer;
  WakeRetiredThread: Boolean;
begin
  if (FThreadPool.FQueuedRequestCount > 0) and (FThreadPool.FWorkerThreadCount < FThreadPool.FMaxLimitWorkerThreadCount) then
  begin
    LastWorkCount := FThreadPool.FLastQueuedRequestCount;
    FThreadPool.FLastQueuedRequestCount := FThreadPool.FQueuedRequestCount;
    if (FThreadPool.FQueuedRequestCount >= LastWorkCount) and IsThrottledDelay(FThreadPool.FLastThreadCreationTick, FThreadPool.FWorkerThreadCount) then
    begin
      WakeRetiredThread := False;
      TMonitor.Enter(FThreadPool.FQueue);
      try
        if (FThreadPool.FQueuedRequestCount >= LastWorkCount) and (FThreadPool.FWorkerThreadCount < FThreadPool.FMaxLimitWorkerThreadCount) and
          (FThreadPool.FIdleWorkerThreadCount = FThreadPool.FRetiredWorkerThreadCount) then
        begin
          if FThreadPool.FRetiredWorkerThreadCount > 0 then
            WakeRetiredThread := True
          else
            FThreadPool.CreateWorkerThread;
        end;
      finally
        TMonitor.Exit(FThreadPool.FQueue);
      end;
      if WakeRetiredThread then
        FThreadPool.FRetiredThreadWakeEvent.SetEvent;
    end;
  end;
end;

function TThreadPool.TThreadPoolMonitor.IsThrottledDelay(LastCreationTick, ThreadCount: Cardinal): Boolean;
var
  CreationDelta: Cardinal;
begin
  CreationDelta := TThread.GetTickCount - LastCreationTick;
  if ThreadCount > Cardinal(TThread.ProcessorCount) then
    Dec(ThreadCount, TThread.ProcessorCount)
  else
    ThreadCount := 0;
  Result := CreationDelta > Trunc(Power(1.1, ThreadCount));
end;

{ TObjectCache }

constructor TObjectCache.Create(AClass: TClass);
begin
  inherited Create;
  FClassType := AClass;
end;

destructor TObjectCache.Destroy;
var
  P: PCacheEntry;
begin
  repeat
    P := Remove;
    FreeMem(P);
  until P = nil;
  inherited;
end;

procedure TObjectCache.Push(var Stack: PCacheEntry; EventItem: PCacheEntry);
var
  LStack: PCacheEntry;
begin
  repeat
    LStack := Stack;
    EventItem.Next := LStack;
  until AtomicCmpExchange(Pointer(Stack), EventItem, LStack) = LStack;
end;

function TObjectCache.Pop(var Stack: PCacheEntry): PCacheEntry;
begin
  repeat
    Result := Stack;
    if Result = nil then
      Exit;
  until AtomicCmpExchange(Pointer(Stack), Result.Next, Result) = Result;
end;

function TObjectCache.Insert(Instance: Pointer): Boolean;
begin
  if (FCount <= CObjectCacheLimit) and (TObject(Instance).ClassType = FClassType) then
  begin
    Push(FRoot, PCacheEntry(Instance));
    AtomicIncrement(FCount);
    Result := True;
  end else
    Result := False;
end;

function TObjectCache.Remove: Pointer;
begin
  Result := Pop(FRoot);
  if Result <> nil then
    AtomicDecrement(FCount);
end;

{ TObjectCaches }

procedure TObjectCaches.AddObjectCache(AClass: TClass);
begin
  if not ContainsKey(AClass) then
    Add(AClass, TObjectCache.Create(AClass));
end;

end.

