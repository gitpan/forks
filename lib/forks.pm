package forks;   # make sure CPAN picks up on forks.pm
package threads; # but in fact we're masquerading as threads.pm

# Make sure we have version info for this module
# Set flag to indicate that we're really the original threads implementation
# Set flag to indicate that we're not really the original threads implementation
# Be strict from now on

$VERSION = '0.02';
$threads        = $threads        = 1; # twice to avoid warnings
$forks::threads = $forks::threads = 1; # twice to avoid warnings
use strict;

# Load only the stuff that we really need

use load;

# Load the XS stuff

require XSLoader;
XSLoader::load( 'forks',$threads::VERSION );

# Make sure we can die with lots of information
# Make sure we can do sockets and have the appropriate constants
# Make sure we can do select() on multiple sockets
# Make sure we have the necessary POSIX constants
# Make sure that we can freeze and thaw data structures

use Carp       ();
use IO::Socket qw(SOCK_STREAM);
use IO::Select ();
use POSIX      qw(BUFSIZ EWOULDBLOCK O_NONBLOCK F_GETFL F_SETFL);
use Storable   qw(freeze thaw);

# Thread local query server object
# The port on which the thread server is listening
# The process id in which the shared variables are stored
# Initialize thread local hash (key: pid) whether this process is a thread
# Thread local flag whether we're shut down

my $QUERY;
my $PORT;
my $SHARED;
my %ISATHREAD;
my $SHUTDOWN;

# Initialize the flag that indicates that we're still running
# Initialize the number of bytes to read at a time
# Initialize hash (key: client) with info to be written to client threads
# Initialize hash (key: client) with clients that we're done with
# Initialize the "thread local" thread id
# Initialize hash (key: module) with code references of CLONE subroutines

my $RUNNING = 1;
my $BUFSIZ  = BUFSIZ;
my %WRITE;
my %DONEWITH;
my $TID;
my %CLONE;

# Initialize the next thread ID to be issued
# Initialize hash (key: tid) with the thread id to client object translation
# Initialize hash (key: client) with the client object to thread id translation
# Initialize hash (key: tid) with the thread id to process id translation
# Initialize hash (key: pid) with the process id to thread id translation

my $NEXTTID = 0;
my %TID2CLIENT;
my %CLIENT2TID;
my %TID2PID;
my %PID2TID;

# Initialize hash (key: tid) with tid's that have been detached
# Initialize hash (key: tid) with results from threads
# Initialize hash (key: tid) with threads that have been joined

my %DETACHED;
my %RESULT;
my %JOINED;

# Initialize hash (key: pid) with clients blocking of pid->tid conversion
# Initialize hash (key: tid) with clients blocking for join() result
# Global debug flag

my %BLOCKING_PID2TID;
my %BLOCKING_JOIN;
my $DEBUG;

# Initialize hash (key: fq sub) with code references to tie subroutines
# List with objects of shared (tied) variables
# Ordinal number of next shared (tied) variable
# Initialize list (key: ordinal) of threads that have the lock for a variable
# Initialize list (key: ordinal) of threads that have a recursive lock
# Initialize list (key: ordinal) of threads that want to lock a variable
# Initialize list (key: ordinal) of threads are waiting in cond_wait
# Initialize list (key: ordinal) of variables that have been signalled

my %DISPATCH;
my @TIED;
my $NEXTTIED = 1;
my @LOCKED;
my @RECURSED;
my @LOCKING;
my @WAITING;
my @SIGNALLED;

# Create packed version of undef
# Create packed version of false
# Create packed version of true

my $undef = _pack( undef );
my $false = _pack( 0 );
my $true  = _pack( 1 );

# Make sure that equality works on thread objects

use overload
 '==' => \&equal,
 'fallback' => 1,
;

# Create new() -> create() equivalence

*create = \&new; create() if 0; # to avoid warning

# Satisfy -require-

1;

#---------------------------------------------------------------------------

# class methods

#---------------------------------------------------------------------------
#  IN: 1 class
#      2 subroutine reference of sub to start execution with
#      3..N any parameters to be passed
# OUT: 1 instantiated object

sub new {

# Obtain the class
# Obtain the subroutine reference
# If it is not a code ref yet (other refs will bomb later)
#  Make the subroutine fully qualified if it is not yet
#  Turn the name into a reference

    my $class = shift;
    my $sub = shift;
    unless (ref($sub)) {
        $sub = caller().'::'.$sub unless $sub =~ m#::#;
        $sub = \&{$sub};
    }

# Initialize the process id of the thread
# If it seems we're in the child process
#  Die now if the fork failed

    my $pid;
    unless ($pid = fork) {
        _croak( "Could not fork child from pid $$, tid $TID\n" )
         unless defined( $pid );

#  Set up the connection for handling queries
#  Execute the routine that we're supposed to execute
#  Save the result
#  And exit the process

        _init_thread();
        my @result = $sub->( @_ );
        _command( '_tojoin',@result );
        CORE::exit();
    }

# Obtain the thread id from the thread just started
# Create an object for it and return it

    my ($tid) = _command( '_waitpid2tid',$pid );
    $class->_object( $tid,$pid );
} #new

#---------------------------------------------------------------------------

sub isthread {

# Die now if this process is already marked as a thread
# Set up stuff so this process is now a detached thread
# Mark this thread as a detached thread

    _croak( "Process $$ already registered as a thread" )
     if exists( $ISATHREAD{$$} );
    _init_thread( 1 );
} #isthread

#---------------------------------------------------------------------------
#  IN: 1 class (ignored)
#      2 new value of debug flag (optional)
# OUT: 1 current value of debug flag

sub debug { $DEBUG = $_[1] if @_ > 1; $DEBUG } #debug

#---------------------------------------------------------------------------
#  IN: 1 class or instantiated object
# OUT: 1 thread id

sub tid {

# Obtain the object
# Return the thread local tid if called as a class method
# Return the field in the object, or fetch and set and return that

    my $self = shift;
    return $TID unless ref($self);
    $self->{'tid'} ||= _command( '_pid2tid',$self->{'pid'} );
} #tid

#---------------------------------------------------------------------------
#  IN: 1 class (ignored)
# OUT: 1 instantiated object

sub self { shift->_object( $TID,$$ ) } #self

#---------------------------------------------------------------------------
#  IN: 1 class (ignored)
#      2 thread id
# OUT: 1 instantiated object or undef if no thread by that tid or detached

sub object {

# Obtain the parameters
# If there is a defined thread id
#  Obtain the associated process id
#  Return blessed object if we actually got a process id
# Indicate we couldn't make an object

    my ($class,$tid) = @_;
    if (defined($tid)) {
        my $pid = _command( '_tid2pid',$tid );
        return $class->_object( $tid,$pid ) if defined( $pid );
    }
    undef;
} #object

#---------------------------------------------------------------------------
#  IN: 1 class
# OUT: 1..N instantiated objects

sub list {

# Obtain the class
# Obtain the hash with process ID's keyed to thread ID's
# Initialize list of objects
# For all of the threads, ordered by ID
#  Add instantiated object for this thread
# Return the list of instantiated objects

    my $class = shift;
    my %hash = _command( '_list_tid_pid' );
    my @object;
    foreach (sort {$a <=> $b} keys %hash) {
        push( @object,$class->_object( $_,$hash{$_} ) );
    }
    @object;
} #list

#---------------------------------------------------------------------------
# Sorry, we can't do yield() with forks

sub yield {} #yield

#---------------------------------------------------------------------------

# instance methods

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
# OUT: 1..N results of the indicated thread

sub detach { _command( '_detach',shift->tid ) } #detach

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
# OUT: 1..N results of the indicated thread

sub join { _command( '_join',shift->tid ) } #join

#---------------------------------------------------------------------------
#  IN: 1 instantiated threads object
#      2 other instantiated threads object
# OUT: 1 whether they refer to the same thread

sub equal { $_[0]->tid == $_[1]->tid } #equal

#---------------------------------------------------------------------------

# exportables

#---------------------------------------------------------------------------
#  IN: 1 subroutine reference of sub to start execution with
#      2..N any parameters to be passed
# OUT: 1 instantiated object

sub async (&;@) { threads->new( @_ ) } #async

#---------------------------------------------------------------------------

# standard Perl features

#---------------------------------------------------------------------------
#  IN: 1 class (ignored)
#      2..N subroutines to export (default: async only)

sub forks::import {

# Lose the class
# If there seems to be a threads.pm loaded
#  Die if it really was a 'use threads'
#  Perform the export needed 
#  And return

    shift;
    if (my $threads = $INC{'threads.pm'}) {
        _croak( "Can not mix 'use forks' with real 'use threads'\n" )
         unless $threads eq $INC{'forks.pm'};
        _export( scalar(caller()),@_ );
        return;
    }

# Fake that threads.pm was really loaded (this is the first time we're here)
# Do any exports that are needed

    $INC{'threads.pm'} = $INC{'forks.pm'};
    _export( scalar(caller()),@_ );
_log( " ! global startup" ) if $DEBUG;

# Create a server that can only take one connection at a time or die now
# Find out the port we're running on and save that for later usage
# Make sure that the server is non-blocking

    $QUERY = IO::Socket::INET->new(
     LocalAddr => '127.0.0.1',
     Listen    => 10,
    ) or _croak( "Couldn't start the listening server: $@\n" );
    $PORT = $QUERY->sockport;
    _nonblock( $QUERY );

# Make sure that children will be reaped automatically
# If we appear to be in the child
#  Die if the fork really failed
#  Start handling requests as the server
# Make this thread 0

    $SIG{CHLD} = 'IGNORE';
    unless ($SHARED = fork) {
        _croak( "Could not start initial fork\n" ) unless defined( $SHARED );
        goto &_server;
    }
    _init_thread();
} #forks::import

#---------------------------------------------------------------------------
#  IN: 1 class (ignored)
#      2..N subroutines to export (default: async only)

sub import {

# Lose the class
# Do whatever threads::import is supposed to do

    shift;
    _export( scalar(caller()),@_ );
} #import

#---------------------------------------------------------------------------

END {

# Return now if this process is not a thread
# Indicate that this process has been shut down to the server
# Mark this thread as shut down (so we won't send or receive anymore)

    return unless exists( $ISATHREAD{$$} );
    _send( $QUERY,'_shutdown',$TID );
    $SHUTDOWN = 1;
} #END

#---------------------------------------------------------------------------

# internal subroutines server-side

#---------------------------------------------------------------------------

sub _server {

# Make sure we take all the CPU that can be got if we're running as root
# Mark the parent thread id as detached

    POSIX::nice( -19 ) unless $<;
    $DETACHED{$NEXTTID} = undef;

# Create the select object in which all the connections are stored
# Initialize the hash with stringified object to real object mapping
# Initialize the length of message to be received hash
# Initialize the received message hash

    my $select = IO::Select->new( $QUERY );
    my %client;
    my %toread;
    my %read;

# Initialize the number of polls
# While we're running in the main dispatch loop
#  Wait until there is something to do
#  Increment number of polls

    my $polls = 0;
    while ($RUNNING) {
if ($DEBUG) {
 my $clients = keys %WRITE;
 _log( " ! $clients>>" ) if $clients;
}
        my $write = (each %WRITE) || '';
        my @reading = $select->can_read( ($write ? .001 : undef) );
_log( " ! <<".@reading ) if $DEBUG and @reading;
        $polls++;

#  For all of the clients that have stuff to read
#   If this is a new client
#    Accept the connection
#    Make sure the client is non-blocking

        foreach my $client (@reading) {
            if ($client == $QUERY) {
                $client = $QUERY->accept();
                _nonblock( $client );

#    Save refs to real client object keyed to thread id and stringified object
#    Make sure the reverse lookup will work
#    Add the client to the list of sockets that we can select on
#    Send the thread ID to the client and increment (now issued) thread ID
#    And reloop

_log( " ! adding thread $NEXTTID" ) if $DEBUG;
                $TID2CLIENT{$NEXTTID} = $client{$client} = $client;
                $CLIENT2TID{$client} = $NEXTTID;
                $select->add( $client );
                $WRITE{$client} = _pack( '_set_tid',$NEXTTID++ );
                next;
            }

#   Initialize the number of bytes to be read per block
#   If we haven't received the length of the message yet
#    Obtain the length, reloop if no length yet
#    Reduce first read to exactly match block size

            my $size = $BUFSIZ;
            unless ($toread{$client}) {
                next unless $toread{$client} = _length( $client );
#_log( " <$CLIENT2TID{$client} $toread{$client} length" ) if $DEBUG;
                $size -= 4;
            }

#   Initialize scalar to receive data in
#   If something went wrong with reading
#    Die (we can't have this going about now can we)
#   Add the data to the request read for this client

            my $data;
            unless (defined( recv($client,$data,$size,0) ) and length($data)) {
                _croak( "Error reading from $CLIENT2TID{$client}: $!\n" );
            }
_log( " <$CLIENT2TID{$client} ".length($data)." of $toread{$client}" ) if $DEBUG;
            $read{$client} .= $data;
        }

#  For all of the clients for which we have read stuff
#   If we have read something already
#    If we have all we're expecting

        while (my $client = each %read) {
            if (my $read = length( $read{$client} )) {
                if ($read == $toread{$client}) {
_log( " =$CLIENT2TID{$client} ".CORE::join(' ',_unpack( $read{$client} )) ) if $DEBUG;

#     Go handle that
#     Remove the number of characters to read
#    Elseif we got too much
#     Die now

                    _handle( $client,$read{$client} );
                    delete( $toread{$client} );
                    delete( $read{$client} );
                } elsif ($read > $toread{$client}) {
                    _croak( "Got $read bytes, expected only $toread{$client} from $CLIENT2TID{$client}: ".CORE::join( ' ',_unpack( $read{$client} ) )."\n" );
                }
            }
        }

#  While there is a client to which we can write
#   Try to write whatever there was to write
#   If write was successful
#    If number of bytes written exactly same as what was supposed to be written
#     Just remove everything that was supposed to be removed
#    Elsif we've written some but not all because of blocking
#     Remove what was written, still left for next time
#    Else (something seriously wrong)
#     Die now
#   Fetch the next client to write to

        while ($write) {
            my $written =
             send( $TID2CLIENT{$CLIENT2TID{$write}},$WRITE{$write},0 );
_log( " >$CLIENT2TID{$write} $written of ".length($WRITE{$write}) ) if $DEBUG;
            if (defined( $written )) {
                if ($written == length( $WRITE{$write} )) {
                    delete( $WRITE{$write} );
                } elsif ($! == EWOULDBLOCK) {
                    substr( $WRITE{$write},0,$written ) = '';
                } else {
                    _croak( "Could not write all data to $CLIENT2TID{$write}: $!\n" );
                }
            }
            $write = each %WRITE;
        }
my $error = [$select->has_exception( .1 )] if $DEBUG;
if ($DEBUG) { _log( " #$CLIENT2TID{$_} error" ) foreach @$error; }

#  For all of the clients that we're done with
#   Reloop if there is still stuff to send there
#   Make sure we won't check this client again

        while (my $client = each %DONEWITH) {
            next if exists( $WRITE{$client} );
_log( " !$CLIENT2TID{$client} shutting down" ) if $DEBUG;
            delete( $DONEWITH{$client} );

#   Obtain the thread id
#   Obtain the client object (rather than its stringification)
#   Remove the client from polling loop
#   Properly close the client from this end

            my $tid = $CLIENT2TID{$client};
            $client = $TID2CLIENT{$tid};
            $select->remove( $client );
            close( $client );

#   Do the clean up

            delete( $TID2CLIENT{$tid} );
            delete( $CLIENT2TID{$client} );
            delete( $PID2TID{$TID2PID{$tid}} );
            delete( $TID2PID{$tid} )
	     if exists( $DETACHED{$tid} ) or exists( $JOINED{$tid} );
            delete( $DETACHED{$tid} );
        }
    } 

# Exit now, we're in the shared process and we've been told to exit

_log( " ! global exit: did $polls polls" ) if $DEBUG;
    CORE::exit();
} #_server

#---------------------------------------------------------------------------
#  IN: 1 socket to put into nonblocking mode

sub _nonblock { # not sure whether needed, this is really cargo-culting

# Obtain the socket in question
# Obtain the current flags
# Set the non-blocking flag onto the current flags

    my $socket = shift;
    my $flags = fcntl( $socket, F_GETFL, 0 )
     or _croak( "Can't get flags for socket: $!\n" );
    fcntl( $socket, F_SETFL, $flags | O_NONBLOCK )
     or _croak( "Can't make socket nonblocking: $!\n" );
} #_nonblock

#---------------------------------------------------------------------------

# internal subroutines client-side

#---------------------------------------------------------------------------
#  IN: 1 namespace to export to
#      2..N subroutines to export

sub _export {

# Obtain the namespace
# If we're supposed to debug the server also
#  Set debug flag
#  Lose the parameter

    my $namespace = shift().'::';
    if ($_[0] eq 'debug') {
        $DEBUG = 1;
        shift;
    }

# Set the defaults if nothing specified
# Allow for evil stuff
# Export whatever needs to be exported

    @_ = qw(async) unless @_;
    no strict 'refs';
    *{$namespace.$_} = \&$_ foreach @_;
} #_export

#---------------------------------------------------------------------------
#  IN: 1 flag: whether to mark the thread as detached

sub _init_thread {

# Mark this process as a thread
# Reset thread local tid value (so the process doesn't have its parent's tid)

    $ISATHREAD{$$} = undef;
    undef( $TID );

# Attempt to create a connection to the server or die

    $QUERY = IO::Socket::INET->new(
     PeerAddr => '127.0.0.1',
     PeerPort => $PORT,
     Type     => SOCK_STREAM,
    ) or _croak( "Couldn't connect to query server: $@\n" );

# Obtain the initial message from the query server
# Die now if it is the wrong type of message
# Set the tid 
# Send the command to register the pid (in the meantime we're doing other stuff)
# Execute all of the CLONE subroutines if this is not the base thread

    my @param = _receive( $QUERY );
    _croak( "Received '$param[0]' unexpectedly\n" ) if $param[0] ne '_set_tid';
    $TID = $param[1];
    _send( $QUERY,'_register_pid',$TID,$$,shift );
    _run_CLONE() if $TID;

# Wait for result of registration, die if failed

    _croak( "Could not register pid $$ as tid $TID" ) unless _receive( $QUERY );
} #_init_thread

#---------------------------------------------------------------------------

# internal subroutines, both server-side as well as client-side

#---------------------------------------------------------------------------
#  IN: 1..N parameters to be put in message
# OUT: 1 formatted message (4 bytes packed length + Storable string)

sub _pack {

# Freeze the parameters that have been passed
# Calculate the length, pack it and return it with the frozen stuff

    my $frozen = freeze( \@_ );
    pack( 'N',length( $frozen ) ).$frozen;
} #_pack

#---------------------------------------------------------------------------
#  IN: 1 formatted message (without 4 byte length info)
# OUT: 1..N whatever was passed to "_pack"

sub _unpack { @{thaw( shift )} } #_unpack

#---------------------------------------------------------------------------
#  IN: 1 client object
#      2 flag: don't croak if there is no length yet
# OUT: 1 length of message to be received

sub _length {

# Obtain client
# Initialize length variable
# If we successfully read
#  If we got enough bytes for a length
#   Return the actual length
#  Elsif we didn't get anything
#   Return 0 if we don't need to croak yet
# Die, there was an error

    my $client = shift;
    my $length;
    my $result = recv( $client,$length,4,0 );
    if (defined( $result )) {
        if (length( $length ) == 4) {
            return unpack( 'N',$length );
        } elsif (length( $length ) == 0) {
            return 0 if shift;
	}
    }
    _croak( "Could not read length of message from $CLIENT2TID{$client}: $!\n");
} #_length

#---------------------------------------------------------------------------
#  IN: 1 client object
#      2 frozen message to send

sub _send {

# Obtain the client object
# Create frozen version of the data
# Calculate the length of data to be sent

    my $client = shift;
    my $frozen = _pack( @_ );
    my $length = length( $frozen );
_log( "> ".CORE::join(' ',eval {_unpack( substr($frozen,4) )}) ) if $DEBUG;

# Send the data, find out how many really got sent
# Die now if an error has occurred
# Die now if not all bytes sent

    my $sent = send( $client,$frozen,0 );
    _croak( "Error when sending message $sent to $CLIENT2TID{$client}: $!" )
     unless defined($sent);
    _croak( "Did not send all bytes: only $sent of $length to $CLIENT2TID{$client}\n" )
     unless $sent == $length;
} #_send

#---------------------------------------------------------------------------
#  IN: 1 client object
# OUT: 1..N parameters of message

sub _receive {

# Obtain the client object
# Obtain the length
# Initialize the data to be received

    my $client = shift;
    my $length = _length( $client );
    my $frozen;

# If we successfully get all data
#  Obtain any parameters if possible
#  Return the result

    if (defined( recv( $client,$frozen,$length,0 ) )
        and length( $frozen ) == $length) {
        my @result = @{thaw( $frozen )};
_log( "< @result" ) if $DEBUG;
        return wantarray ? @result : $result[0];
    }

# Die now (we didn't get the data)

    _croak( "Did not receive all bytes from $CLIENT2TID{$client}: $!\n" );
} #_receive

#---------------------------------------------------------------------------

# all client-side handler internal subroutines from here on

#---------------------------------------------------------------------------
#  IN: 1 command to execute
#      2..N parameters to send
# OUT: 1 values returned by server

sub _command {

# Return now if this thread has shut down already
# Send the command + parameters
# Return the result

    return if $SHUTDOWN;
    _send( $QUERY,@_ );
    _receive( $QUERY );
} #_command

#---------------------------------------------------------------------------
#  IN: 1 class
#      2 thread id
#      3 process id
# OUT: 1 instantiated thread object

sub _object { bless {tid => $_[1], pid => $_[2]},ref($_[0]) || $_[0] } #_object

#---------------------------------------------------------------------------

# all server-side handler internal subroutines from here on

#---------------------------------------------------------------------------
#  IN: 1 instantiated socket
#      2 frozen data to be handled

sub _handle {

# Obtain the socket
# Get the command name and its parameters
# Allow for variable references (sub name is not a ref)
# Execute the command, be sure to pass the socket

    my $client = shift;
    my ($sub,@param) = _unpack( shift );
    no strict 'refs';
    &{$sub}( $client,@param );
} #_handle

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 tid to register
#      3 pid to associate with the tid
#      4 flag: whether to mark thread as detached
# OUT: 1 whether successful (sent to client)

sub _register_pid {

# Obtain the parameters
# Initialize the status as error
# If we received a process id
#  If there is a client object for this thread
#   If this is the first time this thread is being registered
#    Register this thread
#    Make sure we can do a reverse lookup as well
#    Mark the thread as detached if so requested
#    Set status to indicate success

    my ($client,$tid,$pid,$detach) = @_;
    my $status = 0;
    if ($pid) {
        if ($TID2CLIENT{$tid}) {
            unless (exists $PID2TID{$pid}) {
                $TID2PID{$tid} = $pid;
                $PID2TID{$pid} = $tid;
                $DETACHED{$tid} = undef if $detach;
                $status = 1;
            }
        }

#   If there is a thread waiting for this pid/tid pair
#    Let that thread know
#    And forget that it was waiting for it

        if (my $blocking = $BLOCKING_PID2TID{$pid}) {
            _pid2tid( $blocking,$pid );
            delete( $BLOCKING_PID2TID{$pid} );
        }
    }

# Let the client now how it went

    $WRITE{$client} = _pack( $status );
} #_register_pid

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id to find associated process id of
# OUT: 1 associated process id

sub _tid2pid { $WRITE{$_[0]} = _pack( $TID2PID{$_[1]} ) } #_tid2pid

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 process id to find associated thread id of
# OUT: 1 associated thread id

sub _pid2tid { $WRITE{$_[0]} = _pack( $PID2TID{$_[1]} ) } #_pid2tid

#---------------------------------------------------------------------------
#  IN: 1 client socket
# OUT: 1..N tid/pid pairs of all threads

sub _list_tid_pid {

# Initialize the parameters to be sent
# For all of the registered threads
#  Obtain the thread id
#  Reloop if it is detached
#  Add this tid and pid to the list

    my @param;
    while (my($tid,$pid) = each %TID2PID) {
        next if exists( $DETACHED{$tid} ) or exists( $JOINED{$tid} );
        push( @param,$tid,$pid );
    }
    $WRITE{$_[0]} = _pack( @param );
} #_list_tid_pid

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2..N result of thread
# OUT: 1 whether saving successful

sub _tojoin {

# Obtain the client object
# If there is a thread id for this client, obtaining it on the fly
#  If there is a thread waiting for this result, obtaining client on the fly
#   Join the thread with this result
#  Elseif the thread was not detached
#   Save the result for later fetching
# Make sure the client knows the result

    my $client = shift;
    if (my $tid = $CLIENT2TID{$client}) {
        if (my $blocking = $BLOCKING_JOIN{$tid}) {
            _isjoined( $blocking,$tid,@_ );
        } elsif (!$DETACHED{$tid}) {
            $RESULT{$tid} = \@_;
        }
    }
    $WRITE{$client} = $true;
} #_tojoin

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id of thread to detach
# OUT: 1 whether first time detached

sub _detach {

# Obtain the parameters
# Set flag whether first time detached
# Detach this thread
# Let the client know the result

    my ($client,$tid) = @_;
    my $detached = !exists( $DETACHED{$tid} );
    $DETACHED{$tid} = undef;
    $WRITE{$client} = _pack( $detached );
} #_detach

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 process id to find associated thread id of

sub _waitpid2tid {

# If there is already a thread id for this process id, set that
# Start waiting for the tid to arrive

    goto &_pid2tid if exists $PID2TID{$_[1]};
    $BLOCKING_PID2TID{$_[1]} = $_[0];
} #_waitpid2tid

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id of thread to wait for result of

sub _join {

# If there is already a result for this thread
#  Mark the thread as joined and use the pre-saved result
# Elseif the results were fetched before
#  Propagate error to thread
# Else
#  Start waiting for the result to arrive

    my ($client,$tid) = @_;
    if ($RESULT{$tid}) {
        _isjoined( $client,$tid,@{$RESULT{$tid}} );
    } elsif (exists( $JOINED{$tid} )) {
        $WRITE{$client} = $undef; # must become error
    } else {
        $BLOCKING_JOIN{$tid} = $client;
    }
} #_join

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 reference to hash with parameters
#      3..N any extra values specified
# OUT: 1 tied ordinal number

sub _tie {

# Obtain client socket
# Obtain local copy of remote object
# Create the name of the routine to fake tying with here, in shared "thread"

    my $client = shift;
    my $remote = shift;
    my $tiewith = 'TIE'.uc($remote->{'type'});

# Obtain the module we should tie with
# If we could load that module successfully
#  Evaluate any code that needs to be evaluated
#  If there are module(s) to be used
#   If there is more than one
#    Use all of them
#   Else
#    Just use this one

    my $module = $remote->{'module'};
    if (eval "use $module; 1") {
        eval $remote->{'eval'};
        if (my $use = $remote->{'use'}) {
            if (ref($use)) {
                eval "use $_" foreach @$use;
            } else {
                eval "use $use";
            }
        
        }

#  Obtain the ordinal number to be used for this shared variable
#  If successful in tieing it and save the object for this shared variable
#   Return the ordinal (we need that remotely to link with right one here)
# Return indicating error

        my $ordinal = $NEXTTIED++;
        if ($TIED[$ordinal] = $module->$tiewith( @_ )) {
            $WRITE{$client} = _pack( $ordinal );
            return;
        }
    }
    $WRITE{$client} = $undef;
} #_tie

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable
#      3 fully qualified name of subroutine to execute
#      4..N parameters to be passed
# OUT: 1..N parameters to be returned

sub _tied {

# Obtain the client socket
# Obtain the object to work with
# Obtain subroutine name to execute

    my $client = shift;
    my $object = $TIED[shift];
    my $sub = shift;

# Initialize code reference
# If there is a code reference already (fetch it on the fly)
# Elseif this is the first time we try this subroutine
#  Create a non-fully qualified version of the subroutine
#  Attempt to get a code reference for that and save it
# Call the subroutine if there is one and return the result

    my $code;
    if ($code = $DISPATCH{$sub}) {
    } elsif( !exists( $DISPATCH{$sub} ) ) {
	$sub =~ m#^(?:.*)::(.*?)$#;
        $code = $DISPATCH{$sub} = $object->can( $1 );
    }
    $WRITE{$client} = $code ? _pack( $code->( $object,@_ ) ) : $undef;
} #_tied

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to remove
# OUT: 1 whether successful

sub _untie {

# Obtain the socket
# Obtain the ordinal number of the variable
# Obtain the object
# If we can destroy the object, obtaining code ref on the fly
#  Perform whatever needs to be done to destroy

    my $client = shift;
    my $ordinal = shift;
    my $object = $TIED[$ordinal];
    if (my $code = $object->can( 'DESTROY' )) {
        $code->( $object );
    }

# Kill all references to the variable
# Indicate that we're done to the client

    undef( $TIED[$ordinal] );
    $WRITE{$client} = $true;
} #_untie

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to lock

sub _lock {

# Obtain the client socket
# Obtain the thread id of the thread
# Obtain the ordinal number of the shared variable

    my $client = shift;
    my $tid = $CLIENT2TID{$client};
    my $ordinal = shift;

# If this shared variable is already locked, obtaining its tid on the fly
#  If it's the same thread id
#   Indicate a recursive lock for this variable
#   Let the client continue
#  Else
#   Add the thread to the list of ones that want to lock (and let it block)

    if (defined( my $tidlocked = $LOCKED[$ordinal] )) {
        if ($tid == $tidlocked) {
            $RECURSED[$ordinal]++;
            $WRITE{$client} = $true;
        } else {
            push( @{$LOCKING[$ordinal]},$tid );
        }

# Else (this variable was not locked yet)
#  Lock this variable
#  Let the client continue

    } else {
        $LOCKED[$ordinal] = $tid;
        $WRITE{$client} = $true;
    }
} #_lock

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to unlock

sub _unlock {

# Obtain the client socket
# Obtain ordinal while checking whether locked
# Do the actual unlock
# Make sure the client continues

    my $client = shift;
    my $ordinal = _islocked( $client,shift );
    _unlock_ordinal( $ordinal ) if $ordinal;
    $WRITE{$client} = $true;
} #_unlock

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to start waiting for

sub _wait {

# Check if the variable is locked and return ordinal number and thread id
# Unlock the variable
# Add this thread to the list of threads in cond_wait on this variable

    my ($ordinal,$tid) = _islocked( @_,'cond_wait' );
    _unlock_ordinal( $ordinal );
    push( @{$WAITING[$ordinal]},$tid );
} #_wait

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to signal one

sub _signal {

# Obtain local copy of the client
# Check if the variable is locked and return its ordinal number
# Set the signal flag (don't care whether set already)
# Make sure the client continues

    my $client = shift;
    my $ordinal = _islocked( $client,shift,'cond_signal' );
    $SIGNALLED[$ordinal] = 1;
    $WRITE{$client} = $true;
} #_signal

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to signal all

sub _broadcast {

# Obtain local copy of the client
# Check if the variable is locked and return its ordinal number
# If there are threads waiting
#  Add all threads to the list of threads that are about to lock()
#  Reset the list of threads that are waiting
# Make sure the client continues

    my $client = shift;
    my $ordinal = _islocked( $client,shift,'cond_broadcast' );
    if ($WAITING[$ordinal]) {
        unshift( @{$LOCKING[$ordinal]},reverse( @{$WAITING[$ordinal]} ) );
        $WAITING[$ordinal] = undef;
    }
    $WRITE{$client} = $true;
} #_broadcast

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id that was shutdown

sub _shutdown {

# Obtain the client socket
# If it is not the main thread shutting down
#  Mark this client for deletion
# Else (it's the main thread shutting down)
#  Reset running flag

    my $client = shift;
    if (my $tid = shift) {
        $DONEWITH{$client} = undef;
    } else {
        $RUNNING = 0;
    }
} #_shutdown

#---------------------------------------------------------------------------
#  IN: 1 ordinal number of shared variable to unlock

sub _unlock_ordinal {

# Obtain the ordinal number
# If this is a recursive lock
#  Remove one recursion
#  And return

    my $ordinal = shift;
    if ($RECURSED[$ordinal]) {
        $RECURSED[$ordinal]--;
        return;
    }

# Initialize the thread id
# If this variable was signalled
#  Take the next thread id from the list of waiting threads (if any)
#  Reset signal flag

    my $tid;
    if ($SIGNALLED[$ordinal]) {
        $tid = shift(@{$WAITING[$ordinal]});
        $SIGNALLED[$ordinal] = undef;
    }

# If there is a thread id for the lock, take from locking list if none yet
#  Make that the thread locking the variable
#  And have that thread continue
# Else (still no thread wanting to lock)
#  Just reset the lock for this variable

    if (defined( $tid ||= shift(@{$LOCKING[$ordinal]}) )) {
        $LOCKED[$ordinal] = $tid;
        $WRITE{$TID2CLIENT{$tid}} = $true;
    } else {
        $LOCKED[$ordinal] = undef;
    }
} #_unlock_ordinal

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to start waiting for
#      3 function to show when there is an error (undef: no error if wrong)
# OUT: 1 ordinal number of variable
#      2 thread id that keeps it locked

sub _islocked {

# Obtain the client socket
# Obtain the thread id of the thread
# Obtain the ordinal number of the shared variable
# If we're not the one locking
#  Return now with nothing if we don't want an error message
#  Die (we want an error message)
# Return the ordinal number and/or thread id

    my $client = shift;
    my $tid = $CLIENT2TID{$client};
    my $ordinal = shift;
    if ($tid != $LOCKED[$ordinal]) {
        return unless $_[0];
        _croak( "Must first lock variable #$ordinal ($tid != $LOCKED[$ordinal]) before doing a ".shift );
    }
    wantarray ? ($ordinal,$tid) : $ordinal;
} #_islocked

#---------------------------------------------------------------------------
#  IN: 1 client socket to which result will be sent
#      2 thread id of thread with result
#      3..N the result to be sent

sub _isjoined {

# Obtain the client
# Obtain the thread id

    my $client = shift;
    my $tid = shift;

# Unblock the client with the result
# Forget about that someone is waiting for this thread
# Forget about the result (if any)
# Forget about listing in ->list if this thread was shutdown already
# Mark that thread as joined

    $WRITE{$client} = _pack( @_ );
    delete( $BLOCKING_JOIN{$tid} );
    delete( $RESULT{$tid} );
    delete( $TID2PID{$tid} ) unless exists( $TID2CLIENT{$tid} );
    $JOINED{$tid} = undef;
} #_isjoined

#---------------------------------------------------------------------------

# debugging routines

#---------------------------------------------------------------------------
#  IN: 1 message to display

sub _croak { goto &Carp::confess } #_croak

#---------------------------------------------------------------------------
#  IN: 1 message to log

sub _log {

# Obtain the message
# If it is a thread message
#  Obtain the thread id
#  Prefix thread id value
# Log it

    my $message = shift;
    if (substr($message,0,1) ne ' ') {
        my $tid = defined($TID) ? $TID : '?';
        $message = "$tid $message";
    }
    print STDERR "$message\n";
}#_log

#---------------------------------------------------------------------------
#  IN: 1 client object
# OUT: 1 associated tid
#      2 associated pid

sub _client2tidpid {

# Obtain the thread id
# Return thread and process id

    my $tid = $CLIENT2TID{ (shift) };
    ($tid,$TID2PID{$tid});
} #_client2tidpid

#---------------------------------------------------------------------------

sub _run_CLONE {

# For every module loaded
#  Initialize code reference
#  If we tried to get the code reference before (may be undef if not found)
#   Use that

    while (my $logical = each %INC) {
        my $code;
        if (exists $CLONE{$logical}) {
            $code = $CLONE{$logical};

#  Else
#   Make copy of logical name
#   If it looks like a true module
#    Make sure directories are properly represented in the name
#    Attempt to obtain the code reference, don't care if failed
#   Else
#    Make sure we don't try this again
#  Execute the CLONE subroutine if found

        } else {
            my $module = $logical;
            if ($module =~ s#\.pm$##) {
                $module =~ s#/#::#g;
                $code = $CLONE{$logical} = eval { $module->can( 'CLONE' ) };
            } else {
                $CLONE{$logical} = undef;
            }
        }
        &{$code} if $code;
    }
} #_run_CLONE

#---------------------------------------------------------------------------

__END__

=head1 NAME

forks - drop-in replacement for Perl threads using fork()

=head1 SYNOPSIS

  use forks;

  my $thread = threads->new( sub {       # or ->create or async()
    print "Hello world from a thread\n";
  } );

  $thread->join;

  threads->detach;
  $thread->detach;

  my $tid    = $thread->tid;
  my $owntid = threads->tid;

  my $self    = threads->self;
  my $threadx = threads->object( $tidx );

  threads->yield();

  $_->join foreach threads->list;

  unless (fork) {
    threads->isthread; # intended to be used in a child-init Apache handler
  }

  use forks qw(debug);
  threads->debug( 1 );

  perl -Mforks -Mforks::shared threadapplication

=head1 DESCRIPTION

The "forks" pragma allows a developer to use threads without having to have
a threaded perl, or to even run 5.8.0 or higher.  There were a number of goals
that I am trying to reach with this implementation.

=head2 memory usage

The standard Perl 5.8.0 threads implementation is B<very> memory consuming,
which makes it basically impossible to use in a production environment,
particularly with mod_perl and Apache.  Because of the use of the standard
Unix fork() capabilities, most operating systems will be able to use the
Copy-On-Write (COW) memory sharing capabilities (whereas with the standard Perl
5.8.0 threads implementation, this is thwarted by the Perl interpreter
cloning process that is used to create threads).  The memory savings have
been confirmed.

=head2 mod_perl / Apache

This threads implementation allows you to use a standard, pre-forking Apache
server and have the children act as threads (with the class method
L<isthread>).  This is as yet untested within Apache, but should work.

=head2 same API as threads

You should be able to run threaded applications unchanged by simply making
sure that the "forks.pm" and "forks::shared.pm" modules are loaded, e.g. by
specifying them on the command line.  This doesn't work still because the
: shared attribute has not yet been implemented.

=head2 development / demonstration tool

Because you do not need a threaded Perl to use forks.pm, you can start
prototyping threaded applications with the Perl executable that you are used
to.  Just download the "forks.pm" package from CPAN and install that.  So
the threshold for trying out threads in Perl has become much lower.  Even
Perl 5.005 should in principle be able to support the forks.pm module: because
of some issues with regards to the availability of XS features between
different versions of Perl, it seems that 5.8.0 (unthreaded) is what you need
at least.

=head1 IMPLEMENTATION

This is the very first version that I'm making public.  There is still a lot
to do, but the basic functionalities seem to work.  The missing pieces are
just a matter of programming.  If you would like to participate in this,
please do!

This version is mostly written in Perl.  Inter-process communication
is done by using sockets, with the process that stores the shared variables
as the server and all the processes that function as threads, as clients.

=head2 why sockets?

The reason I chose sockets for inter-thread communication above using a shared
memory library, is that a blocking socket allows you to elegantly solve the
problem of a thread that is blocking for a certain event.  Any polling that
might occur, is not occurring at the Perl level, but at the level of the
socket, which should be much better and probably very optimized already.

=head1 EXTRA CLASS METHODS

Apart from the standard class methods, the following class methods are supplied
by the "forks" threads implementation.

=head2 isthread

 unless (fork) {
   threads->isthread; # this process is a detached thread now
   exit;              # can not return values, as thread is detached
 }

The "isthread" class method attempt to make a connection with the shared
variables process.  If it succeeds, then the process will function as a
detached thread and will allow all the threads methods to operate.

This method is mainly intended to be used from within a child-init handler
in a pre-forking Apache server.  All the children that handle requests become
threads as far as Perl is concerned, allowing you to use shared variables
between all of the Apache processes.

=head2 debug

 threads->debug( 1 );
 $debug = threads->debug;

The "debug" class method allows you to (re)set a flag which causes extensive
debugging output of the communication between threads to be output to STDERR.
The format is still subject to change and therefore still undocumented.

=head1 CAVEATS

Because of the use of sockets for inter-thread communication, there is an
inherent larger latency with the interaction between threads.  However, the
fact that sockets are used, may open up the possibility to share threads
over more than one physical machine.

=head1 KNOWN PROBLEMS

These problems are known and will be fixed in the future:

=over 2

=item test-suite exits in a weird way

Although there are no errors in the test-suite, the test harness thinks there
is something wrong because of an unexpected exit() value.  Not sure what to do
about this yet.

=back

=head1 CREDITS

Lars Fenneberg for helping me through the initial birthing pains.

Arthur Bergman for implementing the first working version of Perl threads
support and providing us with an API to build on.

=head1 AUTHOR

Elizabeth Mattijsen, <liz@dijkmat.nl>.

Please report bugs to <perlbugs@dijkmat.nl>.

=head1 COPYRIGHT

Copyright (c) 2002 Elizabeth Mattijsen <liz@dijkmat.nl>. All rights
reserved.  This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 SEE ALSO

L<threads>.

=cut
