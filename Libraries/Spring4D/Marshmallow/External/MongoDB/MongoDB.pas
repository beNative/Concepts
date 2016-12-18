{
     Copyright 2009-2011 10gen Inc.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
}
{ This unit implements the TMongo connection class for connecting to a MongoDB server
  and performing database operations on that server. }
unit MongoDB;

interface
  uses
     MongoBson;

  const
    updateUpsert    = 1;
    updateMulti     = 2;
    updateBasic     = 4;

    indexUnique     = 1;
    indexDropDups   = 4;
    indexBackground = 8;
    indexSparse     = 16;

    { Create a tailable cursor. }
    cursorTailable  = 2;
    { Allow queries on a non-primary node. }
    cursorSlaveOk   = 4;
    { Disable cursor timeouts. }
    cursorNoTimeout = 16;
    { Momentarily block for more data. }
    cursorAwaitData = 32;
    { Stream in multiple 'more' packages. }
    cursorExhaust   = 64;
    { Allow reads even if a shard is down. }
    cursorPartial   = 128;

  type
    TMongoCursor = class;
    TStringArray = array of string;

    { TMongo objects establish a connection to a MongoDB server and are
      used for subsequent database operations on that server. }
    TMongo = class(TObject)
      { Pointer to externally managed data describing the connection.
        User code should not access this.  It is public only for
        access from the GridFS unit. }
      //var
      public

       handle : Pointer;
       host: string;
       port: Integer;
      { Create a TMongo connection object.  A connection is attempted on the
        MongoDB server running on the localhost '127.0.0.1:27017'.
        Check isConnected() to see if it was successful. }
      public
      constructor Create(); overload;
      { Create a TMongo connection object.  The host[:port] to connect to is given
        as the host string. port defaults to 27017 if not given.
        Check the result of isConnected() to see if it was successful. }
      constructor Create(host : string); overload;
      { Determine whether this TMongo is currently connected to a MongoDB server.
        Returns True if connected; False, if not. }

      function Connect() : Boolean; virtual;
      function isConnected() : Boolean;
      { Check the connection.  This returns True if isConnected() and the server
        responded to a 'ping'; otherwise, False. }
      function checkConnection() : Boolean;
      { Return True if the server reports that it is a master; otherwise, False. }
      function isMaster() : Boolean;
      { Temporarirly disconnect from the server.  The connection may be reestablished
        by calling reconnect.  This works on both normal connections and replsets. }
      procedure disconnect();
      { Reconnect to the MongoDB server after having called disconnect to suspend
        operations. }
      function reconnect() : Boolean;
      { Get an error code indicating the reason a connection or network communication
        failed. See mongo-c-driver/src/mongo.h and mongo_error_t. }
      function getErr() : Integer;
      { Set the timeout in milliseconds of a network operation.  The default of 0
        indicates that there is no timeout. }
      function setTimeout(millis : Integer) : Boolean;
      { Get the network operation timeout value in milliseconds.  The default of 0
        indicates that there is no timeout. }
      function getTimeout() : Integer;
      { Get the host:post of the primary server that this TMongo is connected to. }
      function getPrimary() : string;
      { Get the TCP/IP socket number being used for network communication }
      function getSocket() : Integer;
      { Get a list of databases from the server as an array of string }
      function getDatabases() : TStringArray;
      { Given a database name as a string, get the namespaces of the collections
        in that database as an array of string. }
      function getDatabaseCollections(db : string) : TStringArray;
      { Rename a collection.  from_ns is the current namespace of the collection
        to be renamed.  to_ns is the target namespace.
        The collection namespaces (from_ns, to_ns) are in the form 'database.collection'.
        Returns True if successful; otherwise, False.  Note that this function may
        be used to move a collection from one database to another. }
      function rename(from_ns : string; to_ns : string) : Boolean;
      { Drop a collection.  Removes the collection of the given name from the server.
        Exercise care when using this function.
        The collection namespace (ns) is in the form 'database.collection'. }
      function drop(ns : string) : Boolean;
      { Drop a database.  Removes the entire database of the given name from the server.
        Exercise care when using this function. }
      function dropDatabase(db : string) : Boolean;
      { Insert a document into the given namespace.
        The collection namespace (ns) is in the form 'database.collection'.
        See http://www.mongodb.org/display/DOCS/Inserting.
        Returns True if successful; otherwise, False. }
      function insert(ns : string; b : IBsonDocument) : Boolean; overload;
      { Insert a batch of documents into the given namespace (collection).
        The collection namespace (ns) is in the form 'database.collection'.
        See http://www.mongodb.org/display/DOCS/Inserting.
        Returns True if successful; otherwise, False. }
      function insert(ns : string; bs : array of IBsonDocument) : Boolean; overload;
      { Perform an update on the server.  The collection namespace (ns) is in the
        form 'database.collection'.  criteria indicates which records to update
        and objNew gives the replacement document.
        See http://www.mongodb.org/display/DOCS/Updating.
        Returns True if successful; otherwise, False. }
      function update(ns : string; criteria : IBsonDocument; objNew : IBsonDocument) : Boolean; overload;
      { Perform an update on the server.  The collection namespace (ns) is in the
        form 'database.collection'.  criteria indicates which records to update
        and objNew gives the replacement document. flags is a bit mask containing update
        options; updateUpsert, updateMulti, or updateBasic.
        See http://www.mongodb.org/display/DOCS/Updating.
        Returns True if successful; otherwise, False. }
      function update(ns : string; criteria : IBsonDocument; objNew : IBsonDocument; flags : Integer) : Boolean; overload;
      { Remove documents from the server.  The collection namespace (ns) is in the
        form 'database.collection'.  Documents that match the given criteria
        are removed from the collection.
        See http://www.mongodb.org/display/DOCS/Removing.
        Returns True if successful; otherwise, False. }
      function remove(ns : string; criteria : IBsonDocument) : Boolean;
      { Find the first document in the given namespace that matches a query.
        See http://www.mongodb.org/display/DOCS/Querying
        The collection namespace (ns) is in the form 'database.collection'.
        Returns the document as a TBson if found; otherwise, nil. }
      function findOne(ns : string; query : IBsonDocument) : IBsonDocument; overload;
      { Find the first document in the given namespace that matches a query.
        See http://www.mongodb.org/display/DOCS/Querying
        The collection namespace (ns) is in the form 'database.collection'.
        A subset of the documents fields to be returned is specified in fields.
        This can cut down on network traffic.
        Returns the document as a TBson if found; otherwise, nil. }
      function findOne(ns : string; query : IBsonDocument; fields : IBsonDocument) : IBsonDocument; overload;
      { Issue a query to the database.
        See http://www.mongodb.org/display/DOCS/Querying
        Requires a TMongoCursor that is used to specify optional parameters to
        the find and to step through the result set.
        The collection namespace (ns) is in the form 'database.collection'.
        Returns true if the query was successful and at least one document is
        in the result set; otherwise, false.
        Optionally, set other members of the TMongoCursor before calling
        find.  The TMongoCursor must be destroyed after finishing with a query.
        Instatiate a new cursor for another query.
        Example: @longcode(#
          var cursor : TMongoCursor;
          begin
          (* This finds all documents in the collection that have
             name equal to 'John' and steps through them. *)
            cursor := TMongoCursor.Create(BSON(['name', 'John']));
            if mongo.find(ns, cursor) then
              while cursor.next() do
                (* Do something with cursor.value() *)
          (* This finds all documents in the collection that have
             age equal to 32, but sorts them by name. *)
            cursor := TMongoCursor.Create(BSON(['age', 32]));
            cursor.sort := BSON(['name', True]);
            if mongo.find(ns, cursor) then
              while cursor.next() do
                (* Do something with cursor.value() *)
          end;
        #) }
      function find(ns : string; cursor : TMongoCursor) : Boolean;
      {
        The findAndModify command modifies and returns a single document.
        By default, the returned document does not include the modifications made on the update.
        To return the document with the modifications made on the update, use the new option.
      }
      function findAndModify(ns: string; query: IBsonDocument; fields: IBsonDocument
        ; update: IBsonDocument; upsert: Boolean = false): IBsonDocument;
      { Return the count of all documents in the given namespace.
        The collection namespace (ns) is in the form 'database.collection'. }
      function count(ns : string) : Double; overload;
      { Return the count of all documents in the given namespace that match
        the given query.
        The collection namespace (ns) is in the form 'database.collection'. }
      function count(ns : string; query : IBsonDocument) : Double; overload;
      { Create an index for the given collection so that accesses by the given
        key are faster.
        The collection namespace (ns) is in the form 'database.collection'.
        key is the name of the field on which to index.
        Returns nil if successful; otherwise, a TBson document that describes the error. }
      function distinct(ns : string; key : string) : IBsonDocument;
      { Returns a BSON document containing a field 'values' which
        is an array of the distinct values of the key in the given collection (ns).
        Example:
          var
             b : TBson;
             names : TStringArray;
          begin
             b := mongo.distinct('test.people', 'name');
             names := b.find('values').GetStringArray();
          end
      }
      function indexCreate(ns : string; key : string;indexName:string) : IBsonDocument; overload;
      { Create an index for the given collection so that accesses by the given
        key are faster.
        The collection namespace (ns) is in the form 'database.collection'.
        key is the name of the field on which to index.
        options specifies a bit mask of indexUnique, indexDropDups, indexBackground,
        and/or indexSparse.
        Returns nil if successful; otherwise, a TBson document that describes the error. }
      function indexCreate(ns : string; key : string;indexName:string; options : Integer) : IBsonDocument; overload;
      { Create an index for the given collection so that accesses by the given
        key are faster.
        The collection namespace (ns) is in the form 'database.collection'.
        key is a TBson document that (possibly) defines a compound key.
        For example, @longcode(#
          mongo.indexCreate(ns, BSON(['age', True, 'name', True]));
          (* speed up accesses of documents by age and then name *)
        #)
        Returns nil if successful; otherwise, a TBson document that describes the error. }
      function indexCreate(ns : string; key : IBsonDocument;indexName:string) : IBsonDocument; overload;
      { Create an index for the given collection so that accesses by the given
        key are faster.
        The collection namespace (ns) is in the form 'database.collection'.
        key is a TBson document that (possibly) defines a compound key.
        For example, @longcode(#
          mongo.indexCreate(ns, BSON(['age', True, 'name', True]));
          (* speed up accesses of documents by age and then name *)
        #)
        options specifies a bit mask of indexUnique, indexDropDups, indexBackground,
        and/or indexSparse.
        Returns nil if successful; otherwise, a TBson document that describes the error. }
      function indexCreate(ns : string; key : IBsonDocument;indexName:string; options : Integer) : IBsonDocument; overload;
      { Add a user name / password to the 'admin' database.  This may be authenticated
        with the authenticate function.
        See http://www.mongodb.org/display/DOCS/Security+and+Authentication }
      function addUser(name : string; password : string) : Boolean; overload;
      { Add a user name / password to the given database.  This may be authenticated
        with the authenticate function.
        See http://www.mongodb.org/display/DOCS/Security+and+Authentication }
      function addUser(name : string; password : string; db : string) : Boolean; overload;
      { Authenticate a user name / password with the 'admin' database.
        See http://www.mongodb.org/display/DOCS/Security+and+Authentication }
      function authenticate(name : string; password : string) : Boolean; overload;
      { Authenticate a user name / password with the given database.
        See http://www.mongodb.org/display/DOCS/Security+and+Authentication }
      function authenticate(name : string; password : string; db : string) : Boolean; overload;
      { Issue a command to the server.  This supports all commands by letting you
        specify the command object as a TBson document.
        If successful, the response from the server is returned as a TBson document;
        otherwise, nil is returned.
        See http://www.mongodb.org/display/DOCS/List+of+Database+Commands }
      function command(db : string; command : IBsonDocument) : IBsonDocument; overload;
      { Issue a command to the server.  This version of the command() function
        supports that subset of commands which may be described by a cmdstr and
        an argument.
        If successful, the response from the server is returned as a TBson document;
        otherwise, nil is returned.
        See http://www.mongodb.org/display/DOCS/List+of+Database+Commands }
      function command(db : string; cmdstr : string; arg : OleVariant) : IBsonDocument; overload;
      { Get the last error reported by the server.  Returns a TBson document describing
        the error if there was one; otherwise, nil. }
      function getLastErr(db : string) : IBsonDocument;
      { Get the previous error reported by the server.  Returns a TBson document describing
        the error if there was one; otherwise, nil. }
      function getPrevErr(db : string) : IBsonDocument;
      { Reset the error status of the server.  After calling this function, both
        getLastErr() and getPrevErr() will return nil. }
      procedure resetErr(db : string);
      { Get the server error code.  As a convenience, this is saved here after calling
        getLastErr() or getPrevErr(). }
      function getServerErr() : Integer;
      { Get the server error string.  As a convenience, this is saved here after calling
        getLastErr() or getPrevErr(). }
      function getServerErrString() : string;

      procedure parseNamespace(const ns: string; out ADatabase: string; out ACollection: string);
      { Destroy this TMongo object.  Severs the connection to the server and releases
        external resources. }
      destructor Destroy(); override;
    end;

    { TMongoReplset is a superclass of the TMongo connection class that implements
      a different constructor and several functions for connecting to a replset. }
    TMongoReplset = class(TMongo)
    private
      FSeedsAdded: Boolean;
      { Create a TMongoReplset object given the replset name.  Unlike the constructor
        for TMongo, this does not yet establish the connection.  Call addSeed() for each
        of the seed hosts and then call Connect to connect to the replset. }
      public
      constructor Create(name : string; seeds: array of string); overload;
      { Add a seed to the replset.  The host string should be in the form 'host[:port]'.
        port defaults to 27017 if not given/
        After constructing a TMongoReplset, call this for each seed and then call
        Connect(). }
      procedure addSeed(host : string);
      { Connect to the replset.  The seeds added with addSeed() are polled to determine
        if they belong to the replset name given to the constructor.  Their hosts
        are then polled to determine the master to connect to.
        Returns True if it successfully connected; otherwise, False. }
      function Connect() : Boolean; override;
      { Get the number of hosts reported by the seeds }
      function getHostCount() : Integer;
      { Get the Ith host as a 'host:port' string. }
      function getHost(i : Integer) : string;
    end;

    { Objects of class TMongoCursor are used with TMongo.find() to specify
      optional parameters of the find and also to step though the result set.
      A TMongoCursor object is also returned by GridFS.TGridfile.getChunks() which
      is used to step through the chunks of a gridfile. }
    TMongoCursor = class(TObject)
      //var
        public
        { Pointer to externally managed data.  User code should not modify this. }
        handle  : Pointer;
        { A TBson document describing the query.
          See http://www.mongodb.org/display/DOCS/Querying }
        query   : IBsonDocument;
        { A TBson document describing the sort to be applied to the result set.
          See the example for TMongo.find().  Defaults to nil (no sort). }
        sort    : IBsonDocument;
        { A TBson document listing those fields to be included in the result set.
          This can be used to cut down on network traffic. Defaults to nil \
          (returns all fields of matching documents). }
        fields  : IBsonDocument;
        { Specifies a limiting count on the number of documents returned. The
          default of 0 indicates no limit on the number of records returned.}
        limit   : Integer;
        { Specifies the number of matched documents to skip. Default is 0. }
        skip    : Integer;
        { Specifies cursor options.  A bit mask of cursorTailable, cursorSlaveOk,
          cursorNoTimeout, cursorAwaitData, cursorExhaust , and/or cursorPartial.
          Defaults to 0 - no special handling. }
        options : Integer;
        { hold ref to the TMongo object of the find.  Prevents release of the
          TMongo object until after this cursor is destroyed. }
        conn    : TMongo;
      { Create a cursor with a empty query (which matches everything) }
      public
      constructor Create(); overload;
      { Create a cursor with the given query. }
      constructor Create(query_ : IBsonDocument); overload;
      { Step to the first or next document in the result set.
        Returns True if there was a first or next document; otherwise,
        returns False when there are no more documents. }
      function next() : Boolean;
      { Return the current document of the result set }
      function value() : IBsonDocument;

      procedure DisposeCursor();

      { Destroy this cursor.  TMongoCursor objects should be released and destroyed
        after using them with TMongo.find().  This releases resources associated
        with the query on both the client and server ends.  Construct a new
        TMongoCursor for another query. }
      destructor Destroy(); override;
    end;

implementation
  Uses
    SysUtils, StrUtils;

 {$IF not declared(UTF8ToWideString)}
  function UTF8ToWideString(const s:UTF8string):widestring;
  begin
    result :=UTF8Decode(s);
  end;
  {$ifend}

  function mongo_env_sock_init() : Integer;  cdecl; external 'mongoc.dll';
  function mongo_create() : Pointer; cdecl; external 'mongoc.dll';
  procedure mongo_dispose(c : Pointer); cdecl; external 'mongoc.dll';
  function mongo_client(c : Pointer; host : PAnsiChar; port : Integer) : Integer;
    cdecl; external 'mongoc.dll';
  procedure mongo_destroy(c : Pointer); cdecl; external 'mongoc.dll';
  procedure mongo_replica_set_init(c : Pointer; name : PAnsiChar); cdecl; external 'mongoc.dll';
  procedure mongo_replica_set_add_seed(c : Pointer; host : PAnsiChar; port : Integer);
    cdecl; external 'mongoc.dll';
  function mongo_replica_set_client(c : Pointer) : Integer; cdecl; external 'mongoc.dll';
  function mongo_is_connected(c : Pointer) : Boolean;  cdecl; external 'mongoc.dll';
  function mongo_get_err(c : Pointer) : Integer; cdecl; external 'mongoc.dll';
  function mongo_set_op_timeout(c : Pointer; millis : Integer) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_get_op_timeout(c : Pointer) : Integer; cdecl; external 'mongoc.dll';
  function mongo_get_primary(c : Pointer) : PAnsiChar; cdecl; external 'mongoc.dll';
  function mongo_check_connection(c : Pointer) : Integer; cdecl; external 'mongoc.dll';
  procedure mongo_disconnect(c : Pointer); cdecl; external 'mongoc.dll';
  function mongo_reconnect(c : Pointer) : Integer; cdecl; external 'mongoc.dll';
  function mongo_cmd_ismaster(c : Pointer; b : Pointer) : Boolean;
    cdecl; external 'mongoc.dll';
  function mongo_get_socket(c : Pointer) : Integer; cdecl; external 'mongoc.dll';
  function mongo_get_host_count(c : Pointer) : Integer; cdecl; external 'mongoc.dll';
  function mongo_get_host(c : Pointer; i : Integer) : PAnsiChar; cdecl; external 'mongoc.dll';
  function mongo_insert(c : Pointer; ns : PAnsiChar; b : Pointer; wc : Pointer) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_insert_batch(c : Pointer; ns : PAnsiChar; bsons : Pointer; count : Integer; wc : Pointer; flags : Integer) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_update(c : Pointer; ns : PAnsiChar; cond : Pointer; op : Pointer; flags : Integer; wc : Pointer) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_remove(c : Pointer; ns : PAnsiChar; criteria : Pointer; wc : Pointer) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_find_one(c : Pointer; ns : PAnsiChar; query : Pointer; fields : Pointer; result : Pointer) : Integer;
    cdecl; external 'mongoc.dll';
  function bson_create() : Pointer;  external 'mongoc.dll';
  procedure bson_dispose(b : Pointer); cdecl; external 'mongoc.dll';
  procedure bson_copy(dest : Pointer; src : Pointer); cdecl; external 'mongoc.dll';
  function mongo_cursor_create() : Pointer;  cdecl; external 'mongoc.dll';
  procedure mongo_cursor_dispose(cursor : Pointer); cdecl; external 'mongoc.dll';
  procedure mongo_cursor_destroy(cursor : Pointer); cdecl; external 'mongoc.dll';
  function mongo_find(c : Pointer; ns : PAnsiChar; query : Pointer; fields : Pointer;
                      limit, skip, options : Integer) : Pointer; cdecl; external 'mongoc.dll';
  function mongo_cursor_next(cursor : Pointer) : Integer; cdecl; external 'mongoc.dll';
  function mongo_cursor_bson(cursor : Pointer) : Pointer; cdecl; external 'mongoc.dll';
  function mongo_cmd_drop_collection(c : Pointer; db : PAnsiChar; collection : PAnsiChar; result : Pointer) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_cmd_drop_db(c : Pointer; db : PAnsiChar) : Integer; cdecl; external 'mongoc.dll';
  function mongo_count(c : Pointer; db : PAnsiChar; collection : PAnsiChar; query : Pointer) : Double;
    cdecl; external 'mongoc.dll';
  function mongo_create_index(c : Pointer; ns : PAnsiChar; key : Pointer; name:Pointer ;options : Integer; res : Pointer) : Integer;
    cdecl; external 'mongoc.dll'; //in 0.7.1 this function is 4 paramter ,added name
  function mongo_cmd_add_user(c : Pointer; db : PAnsiChar; name : PAnsiChar; password : PAnsiChar) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_cmd_authenticate(c : Pointer; db : PAnsiChar; name : PAnsiChar; password : PAnsiChar) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_run_command(c : Pointer; db : PAnsiChar; command : Pointer; res: Pointer) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_cmd_get_last_error(c : Pointer; db : PAnsiChar; res: Pointer) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_cmd_get_prev_error(c : Pointer; db : PAnsiChar; res: Pointer) : Integer;
    cdecl; external 'mongoc.dll';
  function mongo_get_server_err(c : Pointer) : Integer; cdecl; external 'mongoc.dll';
  function mongo_get_server_err_string(c : Pointer) : PAnsiChar; cdecl; external 'mongoc.dll';

  procedure parseHost(host : string; var hosturl : string; var port : Integer);
  var i : Integer;
  begin
    i := Pos(':', host);
    if i = 0 then begin
      hosturl := host;
      port := 27017;
    end
    else begin
      hosturl := Copy(host, 1, i - 1);
      port := StrToInt(Copy(host, i + 1, Length(host) - i));
    end;
  end;

  constructor TMongo.Create();
  begin
    inherited;
    handle := mongo_create();
    host := '127.0.0.1';
    port := 27017;
    //mongo_client(handle, '127.0.0.1', 27017);
  end;


  constructor TMongo.Create(host : string);
  begin
    Create;
    parseHost(host, host, port);
  end;

  destructor TMongo.Destroy();
  begin
    mongo_destroy(handle);
    mongo_dispose(handle);
  end;

  constructor TMongoReplset.Create(name: string; seeds: array of string);
  var
    i: Integer;
  begin
    inherited Create();
    mongo_replica_set_init(handle, PAnsiChar(System.UTF8Encode(name)));
    for i := Low(seeds) to High(seeds) do
    begin
      addSeed(seeds[i]);
    end;
  end;

  procedure TMongoReplset.addSeed(host : string);
  var
    hosturl : string;
    port : Integer;
  begin
    parseHost(host, hosturl, port);
    mongo_replica_set_add_seed(handle, PAnsiChar(System.UTF8Encode(hosturl)), port);
    FSeedsAdded := True;
  end;

  function TMongoReplset.Connect() : Boolean;
  begin
    if FSeedsAdded then
      Result := (mongo_replica_set_client(handle) = 0)
    else
      Result := inherited Connect();
  end;

  function TMongo.isConnected() : Boolean;
  begin
    Result := mongo_is_connected(handle);
  end;

  function TMongo.checkConnection() : Boolean;
  begin
    Result := (mongo_check_connection(handle) = 0);
  end;

  function TMongo.isMaster() : Boolean;
  begin
    Result := mongo_cmd_ismaster(handle, nil);
  end;

  procedure TMongo.parseNamespace(const ns: string; out ADatabase, ACollection: string);
  var
    LDotPos: Integer;
  begin
    LDotPos := PosEx('.', ns);
    ADatabase := Copy(ns, 1, LDotPos - 1);
    ACollection := Copy(ns, LDotPos + 1, Length(ns));
  end;

procedure TMongo.disconnect();
  begin
    mongo_disconnect(handle);
  end;

  function TMongo.reconnect() : Boolean;
  begin
    Result := (mongo_reconnect(handle) = 0);
  end;

  function TMongo.getErr() : Integer;
  begin
    Result := mongo_get_err(handle);
  end;

  function TMongo.setTimeout(millis: Integer) : Boolean;
  begin
    Result := (mongo_set_op_timeout(handle, millis) = 0);
  end;

  function TMongo.getTimeout() : Integer;
  begin
    Result := mongo_get_op_timeout(handle);
  end;

  function TMongo.getPrimary() : string;
  begin
    Result := string(mongo_get_primary(handle));
  end;

  function TMongo.getSocket() : Integer;
  begin
    Result := mongo_get_socket(handle);
  end;

  function TMongoReplset.getHostCount() : Integer;
  begin
    Result := mongo_get_host_count(handle);
  end;

  function TMongoReplset.getHost(i : Integer) : string;
  begin
    Result := string(mongo_get_host(handle, i));
  end;
  // TODO: free iterators
  function TMongo.getDatabases() : TStringArray;
  var
    b : IBsonDocument;
    it, databases, database : TBsonIterator;
    name : string;
    count, i : Integer;
  begin
    b := command('admin', 'listDatabases', True);
    if b = nil then
      Result := nil
    else begin
      it := b.iterator;
      it.next();
      count := 0;
      databases := it.subiterator();
      while databases.next() do begin
        database := databases.subiterator();
        database.next();
        name := database.value();
        if (name <> 'admin') and (name <> 'local') then
          inc(count);
      end;
      SetLength(Result, count);
      i := 0;
      databases := it.subiterator();
      while databases.next() do begin
        database := databases.subiterator();
        database.next();
        name := database.value();
        if (name <> 'admin') and (name <> 'local') then begin
          Result[i] := name;
          inc(i);
        end;
      end;
    end;
  end;

  function TMongo.getDatabaseCollections(db : string) : TStringArray;
    var
      cursor : TMongoCursor;
      count, i : Integer;
      ns, name : string;
      b : IBsonDocument;
  begin
    count := 0;
    ns := db + '.system.namespaces';
    cursor := TMongoCursor.Create();
    if find(ns, cursor) then
      while cursor.next() do begin
        b := cursor.value();
        name := b.value('name');
        if (PosEx('.system.', name) = 0) and (PosEx('$', name) = 0) then
          inc(count);
      end;
    SetLength(Result, count);
    i := 0;
    cursor.Free;
    cursor := TMongoCursor.Create();
    if find(ns, cursor) then
      while cursor.next() do begin
        b := cursor.value();
        name := b.value('name');
        if (PosEx('.system.', name) = 0) and (PosEx('$', name) = 0) then begin
          Result[i] := name;
          inc(i);
        end;
      end;
    cursor.Free;
  end;

  function TMongo.rename(from_ns : string; to_ns : string) : Boolean;
  begin
    Result := (command('admin', BSON(['renameCollection', from_ns, 'to', to_ns])) <> nil);
  end;

  function TMongo.drop(ns : string) : Boolean;
    var
      db : string;
      collection : string;
      i : Integer;
  begin
    i := Pos('.', ns);
    if i = 0 then
      Raise Exception.Create('TMongo.drop: expected a ''.'' in the namespace.');
    db := Copy(ns, 1, i - 1);
    collection := Copy(ns, i+1, Length(ns) - i);
    Result := (mongo_cmd_drop_collection(handle, PAnsiChar(System.UTF8Encode(db)),
                                                 PAnsiChar(System.UTF8Encode(collection)), nil) = 0);
  end;

  function TMongo.dropDatabase(db : string) : Boolean;
  begin
    Result := (mongo_cmd_drop_db(handle, PAnsiChar(System.UTF8Encode(db))) = 0);
  end;

  function TMongo.insert(ns: string; b: IBsonDocument) : Boolean;
  begin
    Result := (mongo_insert(handle, PAnsiChar(System.UTF8Encode(ns)), b.getHandle, nil) = 0);
  end;

  function TMongo.insert(ns: string; bs: array of IBsonDocument) : Boolean;
  var
    ps : array of Pointer;
    i : Integer;
    len : Integer;
  begin
    len := Length(bs);
    SetLength(ps, Len);
    for i := 0 to Len-1 do
      ps[i] := bs[i].getHandle;
    //Result := (mongo_insert_batch(handle, PAnsiChar(System.UTF8Encode(ns)), @ps, len, nil, 0) = 0); //BUG @符号错误,必须在调用本单元的主程序
    Result := (mongo_insert_batch(handle, PAnsiChar(System.UTF8Encode(ns)), pointer(ps), len, nil, 0) = 0);
  end;

  function TMongo.update(ns : string; criteria : IBsonDocument; objNew : IBsonDocument; flags : Integer) : Boolean;
  begin
    Result := (mongo_update(handle, PAnsiChar(System.UTF8Encode(ns)), criteria.getHandle, objNew.getHandle, flags, nil) = 0);
  end;

  function TMongo.update(ns : string; criteria : IBsonDocument; objNew : IBsonDocument) : Boolean;
  begin
    Result := update(ns, criteria, objNew, 0);
  end;

  function TMongo.remove(ns : string; criteria : IBsonDocument) : Boolean;
  begin
    Result := (mongo_remove(handle, PAnsiChar(System.UTF8Encode(ns)), criteria.getHandle, nil) = 0);
  end;

  function TMongo.findOne(ns : string; query : IBsonDocument; fields : IBsonDocument) : IBsonDocument;
    var
      res : IBsonDocument;
  begin
    res := TBson.Create();
    if (mongo_find_one(handle, PAnsiChar(System.UTF8Encode(ns)), query.getHandle, fields.getHandle, res.getHandle) = 0) then
      Result := res
    else begin
      Result := nil;
    end;
  end;

  function TMongo.findOne(ns : string; query : IBsonDocument) : IBsonDocument;
  begin
    Result := findOne(ns, query, bsonEmpty);
  end;

  constructor TMongoCursor.Create();
  begin
    handle := nil;
    query := nil;
    sort := nil;
    fields := nil;
    limit := 0;
    skip := 0;
    options := 0;
    conn := nil;
  end;

  constructor TMongoCursor.Create(query_ : IBsonDocument);
  begin
    handle := nil;
    query := query_;
    sort := nil;
    fields := nil;
    limit := 0;
    skip := 0;
    options := 0;
    conn := nil;
  end;

  destructor TMongoCursor.Destroy();
  begin
    if Assigned(handle) then
    begin
      mongo_cursor_destroy(handle);
     // mongo_cursor_dispose(handle);
    end;
  end;

  procedure TMongoCursor.DisposeCursor;
  begin
    mongo_cursor_destroy(handle);
    //mongo_cursor_dispose(handle);
    handle := nil;
  end;

function TMongo.find(ns : string; cursor : TMongoCursor) : Boolean;
    var
       q  : IBsonDocument;
       bb : TBsonBuffer;
       ch : Pointer;
  begin
    if cursor.fields = nil then
       cursor.fields := bsonEmpty;
    q := cursor.query;
    if q = nil then
      q := bsonEmpty;
    if cursor.sort <> nil then begin
      bb := TBsonBuffer.Create();
      bb.append('$query', cursor.query);
      bb.append('$orderby', cursor.sort);
      q := bb.finish;
      bb.Free;
    end;
    cursor.conn := Self;
    ch := mongo_find(handle, PAnsiChar(System.UTF8Encode(ns)), q.getHandle, cursor.fields.getHandle,
                     cursor.limit, cursor.skip, cursor.options);
    if ch <> nil then begin
      cursor.handle := ch;
      Result := True;
    end
    else
      Result := False;
  end;


  function TMongo.findAndModify(ns: string; query, fields, update: IBsonDocument;
    upsert: Boolean): IBsonDocument;
  var
    LDatabase, LCollection: string;
    LCmdBson: IBsonDocument;
  begin
    parseNamespace(ns, LDatabase, LCollection);
    if upsert then
      LCmdBson := BSON([
      'findAndModify', LCollection,
      'upsert', upsert,
      'query', query,
      'update', update,
      'fields', fields,
      'new', true
      ])
    else
      LCmdBson := BSON([
      'findAndModify', LCollection,
      'query', query,
      'update', update,
      'fields', fields,
      'new', true
      ]);
    Result := command(LDatabase, LCmdBson);
  end;

function TMongoCursor.next() : Boolean;
  begin
    Result := Assigned(handle) and (mongo_cursor_next(handle) = 0);
  end;

  function TMongoCursor.value() : IBsonDocument;
  var
    b : IBsonDocument;
  begin
    b := TBson.Create();
    bson_copy(b.getHandle, mongo_cursor_bson(handle));
    Result := b;
  end;

  function TMongo.count(ns : string; query : IBsonDocument) : Double;
    var
      db : string;
      collection : string;
      i : Integer;
  begin
    i := PosEx('.', ns);
    if i = 0 then
      Raise Exception.Create('TMongo.drop: expected a ''.'' in the namespace.');
    db := Copy(ns, 1, i - 1);
    collection := Copy(ns, i+1, Length(ns) - i);
    Result := mongo_count(handle, PAnsiChar(System.UTF8Encode(db)),
                                  PAnsiChar(System.UTF8Encode(collection)), query.getHandle);
  end;

  function TMongo.count(ns : string) : Double;
  begin
    Result := count(ns, TBson.Create());
  end;

  //by steven 0.7.1 mongo-c-driver add indexName param
  function TMongo.indexCreate(ns : string; key : IBsonDocument; indexName:string; options : Integer) : IBsonDocument;
  var
    res : IBsonDocument;
    created : Boolean;
  begin
    res := TBson.Create();
    created := (mongo_create_index(handle, PAnsiChar(System.UTF8Encode(ns)), key.getHandle, pchar(indexName), options, res.getHandle) = 0);
    if not created then
      Result := res
    else
    begin
      Result := nil;
    end;
  end;

  function TMongo.indexCreate(ns : string; key : IBsonDocument; indexName:string) : IBsonDocument;
  begin
    Result := indexCreate(ns, key,indexName, 0);
  end;

  function TMongo.indexCreate(ns : string; key : string;indexName:string; options : Integer) : IBsonDocument;
  begin
    Result := indexCreate(ns, BSON([key, True]),indexName, options);
  end;

  function TMongo.indexCreate(ns : string; key : string;indexName:string) : IBsonDocument;
  begin
    Result := indexCreate(ns, key, indexName,0);
  end;

  function TMongo.addUser(name : string; password : string; db : string) : Boolean;
  begin
    Result := (mongo_cmd_add_user(handle, PAnsiChar(System.UTF8Encode(db)),
                                          PAnsiChar(System.UTF8Encode(name)),
                                          PAnsiChar(System.UTF8Encode(password))) = 0);
  end;

  function TMongo.addUser(name : string; password : string) : Boolean;
  begin
    Result := addUser(name, password, 'admin');
  end;

  function TMongo.authenticate(name : string; password : string; db : string) : Boolean;
  begin
    Result := (mongo_cmd_authenticate(handle, PAnsiChar(System.UTF8Encode(db)),
                                              PAnsiChar(System.UTF8Encode(name)),
                                              PAnsiChar(System.UTF8Encode(password))) = 0);
  end;

  function TMongo.authenticate(name : string; password : string) : Boolean;
  begin
    Result := authenticate(name, password, 'admin');
  end;

  function TMongo.command(db : string; command : IBsonDocument) : IBsonDocument;
  begin
    Result := TBson.Create();
    if mongo_run_command(handle, PAnsiChar(System.UTF8Encode(db)), command.getHandle, Result.getHandle) <> 0 then
      Result := nil;
  end;

  function TMongo.distinct(ns : string; key : string) : IBsonDocument;
    var b : IBsonDocument;
        buf : TBsonBuffer;
        p : Integer;
        db, collection : string;
  begin
    p := PosEx('.', ns);
    if p = 0 then
      Raise Exception.Create('Expected a ''.'' in the namespace');
    db := Copy(ns, 1, p-1);
    collection := Copy(ns, p+1, Length(ns) - p);
    buf := TBsonBuffer.Create();
    try
      buf.append('distinct', collection);
      buf.append('key', key);
      b := buf.finish;
      Result := command(db, b);
    finally
      buf.Free;
    end;
  end;

  function TMongo.command(db : string; cmdstr : string; arg : OleVariant) : IBsonDocument;
  begin
    Result := command(db, BSON([cmdstr, arg]));
  end;

function TMongo.Connect: Boolean;
begin
  Result := mongo_client(handle, PAnsiChar(System.UTF8Encode(host)), port) = 0;;
end;

function TMongo.getLastErr(db : string) : IBsonDocument;
  var
    res : IBsonDocument;
  begin
    res := TBson.Create;
    if mongo_cmd_get_last_error(handle, PAnsiChar(System.UTF8Encode(db)), res.getHandle) <> 0 then begin
      Result := res;
    end
    else
      Result := nil;
  end;

  function TMongo.getPrevErr(db : string) : IBsonDocument;
  var
    res : IBsonDocument;
  begin
    res := TBson.Create;
    if mongo_cmd_get_prev_error(handle, PAnsiChar(System.UTF8Encode(db)), res.getHandle) <> 0 then begin        
      Result := res;
    end
    else
      Result := nil;
  end;

  procedure TMongo.resetErr(db : string);
  begin
    command(db, 'reseterror', True);
  end;

  function TMongo.getServerErr() : Integer;
  begin
    Result := mongo_get_server_err(handle);
  end;

  function TMongo.getServerErrString() : string;
  begin
    Result := string(mongo_get_server_err_string(handle));
  end;

initialization
  mongo_env_sock_init();
end.

