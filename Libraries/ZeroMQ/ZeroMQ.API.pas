(*
    Copyright (c) 2007-2013 Contributors as noted in the AUTHORS file

    This file is part of 0MQ.

    0MQ is free software; you can redistribute it and/or modify it under
    the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    0MQ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    *************************************************************************
    NOTE to contributors. This file comprises the principal public contract
    for ZeroMQ API users (along with zmq_utils.h). Any change to this file
    supplied in a stable release SHOULD not break existing applications.
    In practice this means that the value of constants must not change, and
    that old values may not be reused for new constants.
    *************************************************************************

    Unit owners :
       Henri Gourvest <hgourvest@gmail.com>
       Pierre Yager <pierre.y@gmail.com>
*)

unit ZeroMQ.API;

interface

uses
  winsock2;

const
  LIBZEROMQ = 'libzmq.dll';

{.$DEFINE EXPERIMENTAL}

{$WARN SYMBOL_PLATFORM OFF}
(******************************************************************************)
(*  0MQ versioning support.                                                   *)
(******************************************************************************)

const
(*  Version macros for compile-time API version detection                     *)
  ZMQ_VERSION_MAJOR = 4;
  ZMQ_VERSION_MINOR = 1;
  ZMQ_VERSION_PATCH = 6;

  ZMQ_VERSION_ =
    ZMQ_VERSION_MAJOR * 10000 +
    ZMQ_VERSION_MINOR * 100 +
    ZMQ_VERSION_PATCH;

(*  Run-time API version detection                                            *)
procedure zmq_version(major, minor, patch: PInteger); cdecl; external LIBZEROMQ delayed;

(******************************************************************************)
(*  0MQ errors.                                                               *)
(******************************************************************************)

(*  A number random enough not to collide with different errno ranges on      *)
(*  different OSes. The assumption is that error_t is at least 32-bit type.   *)
const
  ZMQ_HAUSNUMERO = 156384712;

(*  On Windows platform some of the standard POSIX errnos are not defined.    *)
  ENOTSUP         = (ZMQ_HAUSNUMERO + 1);
  EPROTONOSUPPORT = (ZMQ_HAUSNUMERO + 2);
  ENOBUFS         = (ZMQ_HAUSNUMERO + 3);
  ENETDOWN        = (ZMQ_HAUSNUMERO + 4);
  EADDRINUSE      = (ZMQ_HAUSNUMERO + 5);
  EADDRNOTAVAIL   = (ZMQ_HAUSNUMERO + 6);
  ECONNREFUSED    = (ZMQ_HAUSNUMERO + 7);
  EINPROGRESS     = (ZMQ_HAUSNUMERO + 8);
  ENOTSOCK        = (ZMQ_HAUSNUMERO + 9);
  EMSGSIZE        = (ZMQ_HAUSNUMERO + 10);
  EAFNOSUPPORT    = (ZMQ_HAUSNUMERO + 11);
  ENETUNREACH     = (ZMQ_HAUSNUMERO + 12);
  ECONNABORTED    = (ZMQ_HAUSNUMERO + 13);
  ECONNRESET      = (ZMQ_HAUSNUMERO + 14);
  ENOTCONN        = (ZMQ_HAUSNUMERO + 15);
  ETIMEDOUT       = (ZMQ_HAUSNUMERO + 16);
  EHOSTUNREACH    = (ZMQ_HAUSNUMERO + 17);
  ENETRESET       = (ZMQ_HAUSNUMERO + 18);

(*  Native 0MQ error codes.                                                   *)
  EFSM           = (ZMQ_HAUSNUMERO + 51);
  ENOCOMPATPROTO = (ZMQ_HAUSNUMERO + 52);
  ETERM          = (ZMQ_HAUSNUMERO + 53);
  EMTHREAD       = (ZMQ_HAUSNUMERO + 54);

(*  This function retrieves the errno as it is known to 0MQ library. The goal *)
(*  of this function is to make the code 100% portable, including where 0MQ   *)
(*  compiled with certain CRT library (on Windows) is linked to an            *)
(*  application that uses different CRT library.                              *)
  function zmq_errno(): Integer; cdecl; external LIBZEROMQ delayed;

(*  Resolves system errors and 0MQ errors to human-readable string.           *)
  function zmq_strerror(errnum: Integer): PAnsiChar; cdecl; external LIBZEROMQ delayed;

(******************************************************************************)
(*  0MQ infrastructure (a.k.a. context) initialisation & termination.         *)
(******************************************************************************)
const
(*  New API                                                                   *)
(*  Context options                                                           *)
  ZMQ_IO_THREADS  = 1;
  ZMQ_MAX_SOCKETS = 2;

(*  Default for new contexts                                                  *)
  ZMQ_IO_THREADS_DFLT  = 1;
  ZMQ_MAX_SOCKETS_DFLT = 1024;

function zmq_ctx_new(): Pointer; cdecl; external LIBZEROMQ delayed;
function zmq_ctx_term(context: Pointer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_ctx_shutdown(context: Pointer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_ctx_set(context: Pointer; option, optval: Integer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_ctx_get(context: Pointer; option: Integer): Integer; cdecl; external LIBZEROMQ delayed;

(*  Old (legacy) API                                                          *)
function zmq_init(io_threads: Integer): Pointer; cdecl; external LIBZEROMQ delayed; deprecated;
function zmq_term(context: Pointer): Integer; cdecl; external LIBZEROMQ delayed; deprecated;
function zmq_ctx_destroy(context: Pointer): Integer; cdecl; external LIBZEROMQ delayed; deprecated;

(******************************************************************************)
(*  0MQ message definition.                                                   *)
(******************************************************************************)

const
  //ZMQ_MAX_VSM_SIZE = 30;
  ZMQ_MAX_VSM_SIZE = 57;

type
  PZmqMsg = ^TZmqMsg;
  TZmqMsg = record
  case Boolean of
   True: (
     content: Pointer;
     flags: Byte;
     vsm_size: Byte;
     vsm_data: array[0..ZMQ_MAX_VSM_SIZE - 1] of Byte;
   );
   False: (
     _: array[0..(ZMQ_MAX_VSM_SIZE + 1 + SizeOf(Pointer) + SizeOf(Byte) + SizeOf(Byte))] of Byte;
   );
  end;

  TZmqFreeFunction = procedure(data, hint: Pointer); stdcall;

function zmq_msg_init(msg: PZmqMsg): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_init_size(msg: PZmqMsg; size: NativeUInt): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_init_data(msg: PZmqMsg; data: Pointer; size: NativeUInt; ffn: TZmqFreeFunction; hint: Pointer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_send(msg: PZmqMsg; s: Pointer; flags: Integer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_recv(msg: PZmqMsg; s: Pointer; flags: Integer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_close(msg: PZmqMsg): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_move(dest, src: PZmqMsg): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_copy(dest, src: PZmqMsg): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_data(msg: PZmqMsg): Pointer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_size(msg: PZmqMsg): NativeUInt; cdecl; external LIBZEROMQ delayed;
function zmq_msg_more(msg: PZmqMsg): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_get(msg: PZmqMsg; option: Integer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_msg_set(msg: PZmqMsg; option, optval: Integer): Integer; cdecl; external LIBZEROMQ delayed;

(******************************************************************************)
(*  0MQ socket definition.                                                    *)
(******************************************************************************)
const
(*  Socket types.                                                             *)
  ZMQ_PAIR   = 0;
  ZMQ_PUB    = 1;
  ZMQ_SUB    = 2;
  ZMQ_REQ    = 3;
  ZMQ_REP    = 4;
  ZMQ_DEALER = 5;
  ZMQ_ROUTER = 6;
  ZMQ_PULL   = 7;
  ZMQ_PUSH   = 8;
  ZMQ_XPUB   = 9;
  ZMQ_XSUB   = 10;
  ZMQ_STREAM = 11;

(*  Deprecated aliases                                                        *)
  ZMQ_XREQ = ZMQ_DEALER deprecated;
  ZMQ_XREP = ZMQ_ROUTER deprecated;

(*  Socket options.                                                           *)
  ZMQ_AFFINITY            = 4;
  ZMQ_IDENTITY            = 5;
  ZMQ_SUBSCRIBE           = 6;
  ZMQ_UNSUBSCRIBE         = 7;
  ZMQ_RATE                = 8;
  ZMQ_RECOVERY_IVL        = 9;
  ZMQ_SNDBUF              = 11;
  ZMQ_RCVBUF              = 12;
  ZMQ_RCVMORE             = 13;
  ZMQ_FD                  = 14;
  ZMQ_EVENTS              = 15;
  ZMQ_TYPE                = 16;
  ZMQ_LINGER              = 17;
  ZMQ_RECONNECT_IVL       = 18;
  ZMQ_BACKLOG             = 19;
  ZMQ_RECONNECT_IVL_MAX   = 21;
  ZMQ_MAXMSGSIZE          = 22;
  ZMQ_SNDHWM              = 23;
  ZMQ_RCVHWM              = 24;
  ZMQ_MULTICAST_HOPS      = 25;
  ZMQ_RCVTIMEO            = 27;
  ZMQ_SNDTIMEO            = 28;
  ZMQ_LAST_ENDPOINT       = 32;
  ZMQ_ROUTER_MANDATORY    = 33;
  ZMQ_TCP_KEEPALIVE       = 34;
  ZMQ_TCP_KEEPALIVE_CNT   = 35;
  ZMQ_TCP_KEEPALIVE_IDLE  = 36;
  ZMQ_TCP_KEEPALIVE_INTVL = 37;
  ZMQ_TCP_ACCEPT_FILTER   = 38;
  ZMQ_IMMEDIATE           = 39;
  ZMQ_XPUB_VERBOSE        = 40;
  ZMQ_ROUTER_RAW          = 41;
  ZMQ_IPV6                = 42;
  ZMQ_MECHANISM           = 43;
  ZMQ_PLAIN_SERVER        = 44;
  ZMQ_PLAIN_USERNAME      = 45;
  ZMQ_PLAIN_PASSWORD      = 46;
  ZMQ_CURVE_SERVER        = 47;
  ZMQ_CURVE_PUBLICKEY     = 48;
  ZMQ_CURVE_SECRETKEY     = 49;
  ZMQ_CURVE_SERVERKEY     = 50;
  ZMQ_PROBE_ROUTER        = 51;
  ZMQ_REQ_CORRELATE       = 52;
  ZMQ_REQ_RELAXED         = 53;
  ZMQ_CONFLATE            = 54;
  ZMQ_ZAP_DOMAIN          = 55;

(*  Message options                                                           *)
  ZMQ_MORE = 1;

(*  Send/recv options.                                                        *)
  ZMQ_DONTWAIT = 1;
  ZMQ_SNDMORE  = 2;

(*  Security mechanisms                                                       *)
  ZMQ_NULL  = 0;
  ZMQ_PLAIN = 1;
  ZMQ_CURVE = 2;

(*  Deprecated options and aliases                                            *)
  ZMQ_IPV4ONLY                = 31 deprecated;
  ZMQ_DELAY_ATTACH_ON_CONNECT = ZMQ_IMMEDIATE deprecated;
  ZMQ_NOBLOCK                 = ZMQ_DONTWAIT deprecated;
  ZMQ_FAIL_UNROUTABLE         = ZMQ_ROUTER_MANDATORY deprecated;
  ZMQ_ROUTER_BEHAVIOR         = ZMQ_ROUTER_MANDATORY deprecated;

(******************************************************************************)
(*  0MQ socket events and monitoring                                          *)
(******************************************************************************)

(*  Socket transport events (tcp and ipc only)                                *)
const
  ZMQ_EVENT_CONNECTED       = 1;
  ZMQ_EVENT_CONNECT_DELAYED = 2;
  ZMQ_EVENT_CONNECT_RETRIED = 4;

  ZMQ_EVENT_LISTENING       = 8;
  ZMQ_EVENT_BIND_FAILED     = 16;

  ZMQ_EVENT_ACCEPTED        = 32;
  ZMQ_EVENT_ACCEPT_FAILED   = 64;

  ZMQ_EVENT_CLOSED          = 128;
  ZMQ_EVENT_CLOSE_FAILED    = 256;
  ZMQ_EVENT_DISCONNECTED    = 512;
  ZMQ_EVENT_MONITOR_STOPPED = 1024;

  ZMQ_EVENT_ALL  = (
    ZMQ_EVENT_CONNECTED + ZMQ_EVENT_CONNECT_DELAYED +
    ZMQ_EVENT_CONNECT_RETRIED + ZMQ_EVENT_LISTENING +
    ZMQ_EVENT_BIND_FAILED + ZMQ_EVENT_ACCEPTED +
    ZMQ_EVENT_ACCEPT_FAILED + ZMQ_EVENT_CLOSED +
    ZMQ_EVENT_CLOSE_FAILED + ZMQ_EVENT_DISCONNECTED +
    ZMQ_EVENT_MONITOR_STOPPED
  );

(*  Socket event data (union member per event)                                *)
type
  PZmqEventData = ^TZmqEventData;
  TZmqEventData = packed record
    event: Word;     // id of the event as bitfield
    value: Integer;  // value is either error code, fd or reconnect interval
  end;

function zmq_socket(p: Pointer; kind: Integer): Pointer; cdecl; external LIBZEROMQ delayed;
function zmq_close(s: Pointer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_setsockopt(s: Pointer; option: Integer; const optval: Pointer; optvallen: NativeUInt): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_getsockopt (s: Pointer; option: Integer; optval: Pointer; optvallen: PCardinal): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_bind(s: Pointer; const addr: PAnsiChar): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_connect(s: Pointer; const addr: PAnsiChar): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_unbind(s: Pointer; const addr: PAnsiChar): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_disconnect(s: Pointer; const addr: PAnsiChar): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_send(s: Pointer; const buf: Pointer; len: NativeUInt; flags: Integer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_send_const (s: Pointer; const buf: Pointer; len: NativeUInt; flags: Integer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_recv(s, buf: Pointer; len: NativeUInt; flags: Integer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_socket_monitor(s: Pointer; const addr: PAnsiChar; events: Integer): Integer; cdecl; external LIBZEROMQ delayed;

function zmq_sendmsg(s: Pointer; msg: PZmqMsg; flags: Integer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_recvmsg(s: Pointer; msg: PZmqMsg; flags: Integer): Integer; cdecl; external LIBZEROMQ delayed;

(*  Experimental                                                              *)
type
  PZMQIOVec = type Pointer;

function zmq_sendiov(s: Pointer; iov: PZMQIOVec; count: NativeUInt; flags: Integer): Integer; cdecl; external LIBZEROMQ delayed;
function zmq_recviov(s: Pointer; iov: PZMQIOVec; count: PNativeUInt; flags: Integer): Integer; cdecl; external LIBZEROMQ delayed;

(******************************************************************************)
(*  I/O multiplexing.                                                         *)
(******************************************************************************)

const
  ZMQ_POLLIN  = 1;
  ZMQ_POLLOUT = 2;
  ZMQ_POLLERR = 4;

type
  PZmqPollItem = ^TZmqPollItem;
  TZmqPollItem = record
    socket: Pointer;
    fd: TSocket;
    events: SmallInt;
    revents: SmallInt;
  end;

const
  ZMQ_POLLITEMS_DFLT = 16;

function zmq_poll(items: PZmqPollItem; nitems: Integer; timeout: LongInt): Integer; cdecl; external LIBZEROMQ delayed;

(*  Built-in message proxy (3-way) *)

function zmq_proxy(frontend, backend, capture: Pointer): Integer; cdecl; external LIBZEROMQ delayed;

(* These functions are documented by man pages                                *)

(*  Encode a binary key as printable text using ZMQ RFC 32  *)
(* Encode data with Z85 encoding. Returns encoded data                        *)
function zmq_z85_encode(dest: PAnsiChar; data: PByte; size: NativeUInt): PAnsiChar; cdecl; external LIBZEROMQ delayed;

(*  Encode a binary key from printable text per ZMQ RFC 32  *)
(* Decode data with Z85 encoding. Returns decoded data                        *)
function zmq_z85_decode(dest: PByte; str: PAnsiChar): PByte; cdecl; external LIBZEROMQ delayed;

(* Generate z85-encoded public and private keypair with libsodium.            *)
(* Returns 0 on success.                                                      *)
function zmq_curve_keypair(z85_public_key, z85_secret_key: PAnsiChar): Integer; cdecl; external LIBZEROMQ delayed;

(*  Deprecated aliases *)
const
  ZMQ_STREAMER  = 1 deprecated;
  ZMQ_FORWARDER = 2 deprecated;
  ZMQ_QUEUE     = 3 deprecated;

(*  Deprecated method *)
function zmq_device(kind: Integer; frontend, backend: Pointer): Integer; cdecl; external LIBZEROMQ delayed; deprecated;

(*  These functions are not documented by man pages                           *)

(*  Helper functions are used by perf tests so that they don't have to care   *)
(*  about minutiae of time-related functions on different OS platforms.       *)

(*  Starts the stopwatch. Returns the handle to the watch.                    *)
function zmq_stopwatch_start: Pointer; cdecl; external LIBZEROMQ delayed;

(*  Stops the stopwatch. Returns the number of microseconds elapsed since     *)
(*  the stopwatch was started.                                                *)
function zmq_stopwatch_stop(watch: Pointer): Cardinal; cdecl; external LIBZEROMQ delayed;

(*  Sleeps for specified number of seconds.                                   *)
procedure zmq_sleep(seconds: Integer); cdecl; external LIBZEROMQ delayed;

type
  TZmqThreadFn = procedure(data: Pointer); stdcall;

(* Start a thread. Returns a handle to the thread.                            *)
function zmq_threadstart(func: TZmqThreadFn; arg: Pointer): Pointer; cdecl; external LIBZEROMQ delayed;

(* Wait for thread to complete then free up resources.                        *)
procedure zmq_threadclose(thread: Pointer); cdecl; external LIBZEROMQ delayed;

implementation

end.
