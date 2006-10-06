package forks;   # make sure CPAN picks up on forks.pm
$VERSION = '0.20';

package threads; # but in fact we're masquerading as threads.pm

# Make sure we have version info for this module
# Set flag to indicate that we're really the original threads implementation
# Set flag to indicate that we're not really the original threads implementation
# Be strict from now on

$VERSION = '1.07';
$threads        = $threads        = 1; # twice to avoid warnings
$forks::threads = $forks::threads = 1; # twice to avoid warnings
use strict;
use Scalar::Util qw(reftype blessed);

#---------------------------------------------------------------------------
# If we're running in a perl before 5.8.0, we need a source filter to change
# all occurrences of
#
#  share( $x );
#
# to:
#
#  share( \$x );
#
# The same applies for lock(), cond_wait(), cond_timedwait(), cond_signal() and cond_broadcast().
#
# We do this by conditionally adding the source filter functionality if we're
# running in a versione before 5.8.0.

my $filtering; # are we filtering source code?
BEGIN {
    eval <<'EOD' if ($filtering = $] < 5.008); # need string eval ;-(

use Filter::Util::Call; # get the source filter stuff

#---------------------------------------------------------------------------
#  IN: 1 object (not used)
# OUT: 1 status

sub filter {

# Initialize status
# If there are still lines to read
#  Convert the line if there is any mention of our special subs
# Return the status

    my $status;
    if (($status = filter_read()) > 0) {
#warn $_ if         # activate if we want to see changed lines
        s#(\b(?:cond_broadcast|cond_wait|cond_timedwait|cond_signal|share|lock)\b\s*(?!{)\(?\s*)(?=[mo\$\@\%])#$1\\#sg;
    }
    $status;
} #filter
EOD
} #BEGIN

#---------------------------------------------------------------------------

# Global debug flag
# Global socket server Nice value
# Global CHLD force IGNORE flag
# Global UNIX socket flag
# Global INET socket IP mask regex value
# Do this at compile time
#  If there is a THREADS_DEBUG specification
#   Untaint value
#   Set its value
#   Make sure access is done with the DEBUG sub
#  Else (we never need any debug info)
#   Make DEBUG a false constant: debugging code should be optimised away
#  If there is a THREADS_SOCKET_UNIX specification
#   Set its value
#   Make sure socket is available; die if non-socket object exists
#   Remove existing socket file if defined
#   Make sure access is done with the THREADS_UNIX sub
#  Else 
#   Make THREADS_UNIX a false constant: default to INET sockets

my $DEBUG;
my $SERVER_NICE;
my $FORCE_SIGCHLD_IGNORE;
my $THREADS_UNIX;
my $INET_IP_MASK;

BEGIN {
    if (exists $ENV{'THREADS_DEBUG'}) {
        $ENV{'THREADS_DEBUG'} =~ m#^(.*)$#s;
        $DEBUG = $1;
    } else {
        $DEBUG = 0;
    }
    *DEBUG = sub () { $DEBUG };

    if (exists $ENV{'THREADS_NICE'}) {
        $ENV{'THREADS_NICE'} =~ m#^(.*)$#s;
        $SERVER_NICE = $1;
    } else {
        $SERVER_NICE = 0;
    }

    if (exists $ENV{'THREADS_SIGCHLD_IGNORE'}) {
        $ENV{'THREADS_SIGCHLD_IGNORE'} =~ m#^(.*)$#s;
        $FORCE_SIGCHLD_IGNORE = $1;
    } else {
        $FORCE_SIGCHLD_IGNORE = 0;
    }

    my $threads_socket_unix = '/var/tmp/perlforks.';
    if (defined $ENV{'THREADS_SOCKET_UNIX'} && $ENV{'THREADS_SOCKET_UNIX'} ne "") {
        #$ENV{'THREADS_SOCKET_UNIX'} =~ m#^(.*)$#s;
        $THREADS_UNIX = $threads_socket_unix;
    } else {
        $THREADS_UNIX = 0;
    }

    if (exists $ENV{'THREADS_IP_MASK'}) {
        $ENV{'THREADS_IP_MASK'} =~ m#^(.*)$#s;
        $INET_IP_MASK = $1;
    } else {
        $INET_IP_MASK = '^127\.0\.0\.1$';
    }
} #BEGIN

# Import Time::HiRes if it was loaded before forks, for increased timing precision

BEGIN {
    if (exists $INC{'Time/HiRes.pm'}) {
        require Time::HiRes;
        import Time::HiRes qw(time);
    }
} #BEGIN

# Load the XS stuff

require XSLoader;
XSLoader::load( 'forks',$forks::VERSION );

# Make sure we can die with lots of information
# Make sure we can do sockets and have the appropriate constants
# Make sure we can do select() on multiple sockets
# Make sure we have the necessary POSIX constants
# Make sure that we can freeze and thaw data structures

use Carp       ();
use Socket     qw(SOMAXCONN);
use IO::Socket ();
use IO::Select ();
use POSIX      qw(:signal_h :sys_wait_h BUFSIZ EWOULDBLOCK O_NONBLOCK F_GETFL F_SETFL);
use Storable   qw(freeze thaw);
use reaper     qw(reapPid);

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
# List of signals that we will delay if target platform requires custom CHLD handler
# Boolean indicating whether or not platform requires a custom CHLD handler
# Initialize hash (key: client) with info to be written to client threads
# Initialize hash (key: client) with clients that we're done with
# Initialize the "thread local" thread id
# Initialize hash (key: module) with code references of CLONE subroutines

my $RUNNING = 1;
my $BUFSIZ  = BUFSIZ;
my @DEFERRED_SIGNALS = (SIGCHLD);
my $CUSTOM_SIGCHLD = 0;
my %WRITE;
my %DONEWITH;
my $TID;
my %CLONE;

# Initialize the next thread ID to be issued
# Initialize hash (key: tid) with the thread id to client object translation
# Initialize hash (key: client) with the client object to thread id translation
# Initialize hash (key: tid) with the thread id to process id translation
# Initialize hash (key: pid) with the process id to thread id translation
# Initialize hash (key: ppid) with the parent pid to child tid queue (value: array ref)
# Initialize the thread parent pid

my $NEXTTID = 0;
my %TID2CLIENT;
my %CLIENT2TID;
my %TID2PID;
my %PID2TID;
my %PPID2CTID_QUEUE;
my $PARENT_PID;

# Initialize hash (key: tid) with tid's that have been detached
# Initialize hash (key: tid) with results from threads
# Initialize hash (key: tid) with threads that have been joined

my %DETACHED;
my %RESULT;
my %JOINED;

# Initialize hash (key: ppid) with clients blocking of ppid->ctid conversion
# Initialize hash (key: tid) with clients blocking for join() result

my %BLOCKING_PPID2CTID_QUEUE;
my %BLOCKING_JOIN;

# Initialize hash (key: fq sub) with code references to tie subroutines
# List with objects of shared (tied) variables
# Ordinal number of next shared (tied) variable

my %DISPATCH;
my @TIED;
my $NEXTTIED = 1;

# Initialize list (key: ordinal) of threads that have the lock for a variable
# Initialize list (key: ordinal) of threads that have a recursive lock
# Initialize list (key: ordinal) of threads that want to lock a variable
# Initialize list (key: ordinal) of threads are waiting in cond_wait
# Initialize hash (key: ordinal) of threads are waiting in cond_timedwait
# Initialize scalar representing unique ID of each timed event
# Initialize list (order: expiration time) representing a sorted version (pseudo-index) of %TIMEDWAITING
# Initialize scalar indicating when %TIMEDWAITING has changed and @TIMEDWAITING_IDX should be recalculated
# Initialize list (key: ordinal; subkey: tid) of TIMEDWAITING events that have timed out

my @LOCKED;
my @RECURSED;
my @LOCKING;
my @WAITING;
my %TIMEDWAITING;
my $TIMEDWAITING_ID = 0;
my @TIMEDWAITING_IDX;
my $TIMEDWAITING_IDX_EXPIRED = 0;
my @TIMEDWAITING_EXPIRED;

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
    my $ppid = $$;
    unless ($pid = fork) {
        _croak( "Could not fork child from pid $$, tid $TID\n" )
         unless defined( $pid );

#  Set up the connection for handling queries
#  Execute the routine that we're supposed to execute
#  Save the result
#  And exit the process

        $PARENT_PID = $ppid;
        _init_thread();
        my @result;
        eval { @result = $sub->( @_ ); };
        warn $@ if $@;
        _command( '_tojoin',@result );
        CORE::exit();
    }

# Obtain the thread id from the thread just started
# Create an object for it and return it

    reapPid($pid) if $CUSTOM_SIGCHLD;
    my ($tid) = _command( '_waitppid2ctid',$$ );
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

sub async (&;@) { new( 'threads',@_ ) } #async

#---------------------------------------------------------------------------

# standard Perl features

#---------------------------------------------------------------------------
# Default reaper, if using custom CHLD signal handler (prevents zombies)

sub REAPER {
# While we have zombie processes, loop and reap (don't care about exit status)

    while (my $pid = waitpid(-1, &WNOHANG) > 0) {}
} #REAPER

#---------------------------------------------------------------------------
#  IN: 1 class (ignored)
#      2..N subroutines to export (default: async only)

sub forks::import {

# Obtain the class
# Add filter if we're filtering

    my $self = shift;
    filter_add( bless {},$self ) if $filtering;

# If there seems to be a threads.pm loaded
#  Die if it really was a 'use threads'
#  Perform the export needed 
#  And return

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
_log( " ! global startup" ) if DEBUG;

# Create a server that can only take one connection at a time or die now
# Find out the port we're running on and save that for later usage
# Make sure that the server is non-blocking

    if ($THREADS_UNIX) {
        _croak( "UNIX socket file '$THREADS_UNIX$$' in use by non-socket file" ) if -e $THREADS_UNIX.$$ && !-S $THREADS_UNIX.$$;
        _croak( "Unable to delete UNIX socket file '$THREADS_UNIX$$'" ) if -S $THREADS_UNIX.$$ && !unlink($THREADS_UNIX.$$);
        $QUERY = IO::Socket::UNIX->new(
         Local  => $THREADS_UNIX.$$,
         Listen => SOMAXCONN,
        ) or _croak( "Couldn't start the listening server: $@\n" );
        chmod 0777, $THREADS_UNIX.$$;
        $PORT = $THREADS_UNIX.$$;
    } else {
        $QUERY = IO::Socket::INET->new(
         LocalAddr => '127.0.0.1',
         Listen    => SOMAXCONN,
        ) or _croak( "Couldn't start the listening server: $@\n" );
        $PORT = $QUERY->sockport;
    }
    _nonblock( $QUERY );

# Make sure that children will be reaped automatically
# Enable custom CHLD signal handler, if necessary
# If we appear to be in the child
#  Die if the fork really failed
#  Start handling requests as the server
# Mark PID for reaping, if using custom CHLD signal handler
# Make this thread 0

    $SIG{CHLD} = 'IGNORE';
    unless ($FORCE_SIGCHLD_IGNORE) {
        local $ENV{PATH} = "/bin:/usr/bin";
        if (system('/bin/test') == -1) {
            $SIG{CHLD} = sub { reaper::REAPER ( shift, \&REAPER ); };
            $CUSTOM_SIGCHLD = 1;
        } else {
            $CUSTOM_SIGCHLD = 0;
        }
    }
    unless ($SHARED = fork) {
        _croak( "Could not start initial fork\n" ) unless defined( $SHARED );
        return &_server;
    }
    $PARENT_PID = undef;    #this is main thread
    reapPid($SHARED) if $CUSTOM_SIGCHLD;
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

# Revert to simple CHLD handler to insure portable, reliable shutdown
# If this process is not a thread (e.g. is the shared server)
#  Shutdown the socket server
#  Delete UNIX socket file if the socket file exists
#  Return now
# Indicate that this process has been shut down to the server
# Mark this thread as shut down (so we won't send or receive anymore)

    $SIG{CHLD} = 'IGNORE';
    unless (exists( $ISATHREAD{$$} )) {
       $QUERY->shutdown(2) if defined $QUERY;
       unlink($PORT) if $THREADS_UNIX && -S $PORT;
       return;
    }
    _send( $QUERY,'_shutdown',$TID );
    $SHUTDOWN = 1;
} #END

#---------------------------------------------------------------------------

# internal subroutines server-side

#---------------------------------------------------------------------------

sub _server {

# Set nice value if environment variable set and if we're running as root
# Mark the parent thread id as detached

    POSIX::nice( $SERVER_NICE ) if $SERVER_NICE && !$<;
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
#  Update timedwaiting index
#  Load next event timedwaiting expiration time (if any)
#  Wait until there is something to do or a cond_timedwaiting event has expired
#  Increment number of polls
#  Handle any timedwaiting events that may have expired

    my $polls = 0;
    while ($RUNNING) {
if (DEBUG) {
 my $clients = keys %WRITE;
 _log( " ! $clients>>" ) if $clients;
}
        my $write = (each %WRITE) || '';
        _update_timedwaiting_idx();
        my $timedwaiting_next_event = @TIMEDWAITING_IDX ? $TIMEDWAITING_IDX[0]->[2] : undef;
        my $timedwaiting_delay = defined $timedwaiting_next_event ? $timedwaiting_next_event - time() : .001;
        my @reading = $select->can_read( ($write ? .001 : @TIMEDWAITING_IDX ? ( $timedwaiting_delay > 0 ? $timedwaiting_delay : .001 ) : undef) );
_log( " ! <<".@reading ) if DEBUG and @reading;
        $polls++;
        _handle_timedwaiting();
        
#  For all of the clients that have stuff to read
#   If this is a new client
#    Accept the connection
#    If using INET sockets
#     Check if client is in the allow list
#      Immediately close client socket if not in allow list
#      And reloop
#    Make sure the client is non-blocking

        foreach my $client (@reading) {
            if ($client == $QUERY) {
                $client = $QUERY->accept();
                unless ($THREADS_UNIX) {
                    if ($INET_IP_MASK ne '' && $client->peerhost() !~ m/$INET_IP_MASK/) {
                        warn 'Thread server rejected connection: '
                            .$client->peerhost().':'.$client->peerport().' does not match allowed IP mask'."\n";
                        close( $client );
                        next;
                    }
                }
                _nonblock( $client );

#    Save refs to real client object keyed to thread id and stringified object
#    Make sure the reverse lookup will work
#    Add the client to the list of sockets that we can select on
#    Send the thread ID to the client and increment (now issued) thread ID
#    And reloop

_log( " ! adding thread $NEXTTID" ) if DEBUG;
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
#_log( " <$CLIENT2TID{$client} $toread{$client} length" ) if DEBUG;
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
_log( " <$CLIENT2TID{$client} ".length($data)." of $toread{$client}" ) if DEBUG;
            $read{$client} .= $data;
        }

#  For all of the clients for which we have read stuff
#   If we have read something already
#    If we have all we're expecting

        while (my $client = each %read) {
            if (my $read = length( $read{$client} )) {
                if ($read == $toread{$client}) {
_log( " =$CLIENT2TID{$client} ".CORE::join(' ',_unpack( $read{$client} )) ) if DEBUG;

#     Create untainted version of what we got
#     Go handle that
#     Remove the number of characters to read
#    Elseif we got too much
#     Die now

                    $read{$client} =~ m#^(.*)$#s;
                    _handle( $client,$1 );
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
#   Else (something seriously wrong
#    Die now
#   Fetch the next client to write to

        while ($write) {
            my $written =
             send( $TID2CLIENT{$CLIENT2TID{$write}},$WRITE{$write},0 );
_log( " >$CLIENT2TID{$write} $written of ".length($WRITE{$write}) ) if DEBUG;
            if (defined( $written )) {
                if ($written == length( $WRITE{$write} )) {
                    delete( $WRITE{$write} );

#***************************************************************************
# For some reason, attempting to write too much does _not_ set $! to
# EWOULDBLOCK, instead it appears to always be 0.  For now we'll assume
# that if not everything was written but the write itself was successful
# (i.e, $written is defined) that we should attempt to send the rest again.
# The croak that was in the else clause, can now never happen and is
# therefore disabled for now.

#                } elsif ($! == EWOULDBLOCK) {
                } else {
                    substr( $WRITE{$write},0,$written ) = '';
#                } else {
#                    _croak( "Could not write all data to $CLIENT2TID{$write}: $!\n" );
                }
#***************************************************************************

            } else {
                _croak( "Could not write ".(length $WRITE{$write})." bytes to $CLIENT2TID{$write}: $!\n" );
            }
            $write = each %WRITE;
        }
my $error = [$select->has_exception( .1 )] if DEBUG;
if (DEBUG) { _log( " #$CLIENT2TID{$_} error" ) foreach @$error; }

#  For all of the clients that we're done with
#   Reloop if there is still stuff to send there
#   Make sure we won't check this client again

        while (my $client = each %DONEWITH) {
            next if exists( $WRITE{$client} );
_log( " !$CLIENT2TID{$client} shutting down" ) if DEBUG;
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

            my $pid = $TID2PID{$tid};
            delete( $TID2CLIENT{$tid} );
            delete( $CLIENT2TID{$client} );
            delete( $PID2TID{$pid} );
            delete( $TID2PID{$tid} )
               if exists( $DETACHED{$tid} ) or exists( $JOINED{$tid} );
            delete( $DETACHED{$tid} );
        }
    } 

# Exit now, we're in the shared process and we've been told to exit

_log( " ! global exit: did $polls polls" ) if DEBUG;
    CORE::exit();
} #_server

#---------------------------------------------------------------------------

sub _update_timedwaiting_idx {

#  If timedwaiting index expired flag set
#   Translate timedwaiting hash to sorted (index) array of all events
#   Reset index expired flag

    if ($TIMEDWAITING_IDX_EXPIRED) {
        @TIMEDWAITING_IDX = ();
        if (keys %TIMEDWAITING) {
            push @TIMEDWAITING_IDX, map($_, sort {$a->[2] <=> $b->[2]} map(@{$TIMEDWAITING{$_}}, keys %TIMEDWAITING));
        }
        $TIMEDWAITING_IDX_EXPIRED = 0;
    }
} #_update_timedwaiting_idx

#---------------------------------------------------------------------------

sub _handle_timedwaiting {
    
#  For all timed wait events
#   Obtain the tid, time, and ordinal event
#   If this timed event is expired and a timed event exists for this ordinal
#    Parse all timed events
#     If current event in list of timed events is the matching event to what has expired
#      Get the tid and target lock ordinal of the event
#      Delete event from list & expire timed event index
#      If ordinal is currently locked
#       Signal this variable for when the target locked variable is unlocked later
#      Else (ordinal not locked)
#       Assign lock to this tid
#       Immediately notify blocking thread that it should continue
#   Else last loop: minimize index parsing, as when current event isn't expired, remaining (ordered) events in array aren't either

    foreach (@TIMEDWAITING_IDX) {
        my (undef, $ordinal, $time, undef, $id) = @{$_};
        if ($time <= time() && defined $TIMEDWAITING{$ordinal} && ref($TIMEDWAITING{$ordinal}) eq 'ARRAY' && @{$TIMEDWAITING{$ordinal}}) {
            for (my $i = 0; $i < scalar @{$TIMEDWAITING{$ordinal}}; $i++) {
                if ($TIMEDWAITING{$ordinal}->[$i]->[4] == $id) {
                    my ($tid, $l_ordinal) = @{splice(@{$TIMEDWAITING{$ordinal}}, $i, 1)}[0,3];
                    delete $TIMEDWAITING{$ordinal} unless @{$TIMEDWAITING{$ordinal}};
                    $TIMEDWAITING_IDX_EXPIRED = 1;
                    if (defined $LOCKED[$l_ordinal]) {
                        push @{$TIMEDWAITING_EXPIRED[$ordinal]}, [$tid, $l_ordinal];
                    } else {
                        $LOCKED[$l_ordinal] = $tid;
                        $WRITE{$TID2CLIENT{$tid}} = $false;
                    }
                    last;
                }
            }
        } else { 
            last;
        }
    }
} #_handle_timedwaiting

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
    if (defined( $_[0] ) and $_[0] eq 'debug') {
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

    if ($THREADS_UNIX) {
        $QUERY = IO::Socket::UNIX->new(
         Peer => $PORT,
        ) or _croak( "Couldn't connect to query server: $@\n" );
    } else {
        $QUERY = IO::Socket::INET->new(
         PeerAddr => '127.0.0.1',
         PeerPort => $PORT,
        ) or _croak( "Couldn't connect to query server: $@\n" );
    }

# Obtain the initial message from the query server
# Die now if it is the wrong type of message
# Set the tid 
# Send the command to register the pid (in the meantime we're doing other stuff)
# Execute all of the CLONE subroutines if not in the base thread

    my @param = _receive( $QUERY );
    _croak( "Received '$param[0]' unexpectedly\n" ) if $param[0] ne '_set_tid';
    $TID = $param[1];
    _send( $QUERY,'_register_pid',$TID,$$,$PARENT_PID,shift );
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

# Exit now if this looks as if the connection was reset (probably dieing)
#**************************************************************************
# There is no easy way to handle if the main thread exits.  This shows up *
# as "Connection reset by peer" errors on the connection.  For now, we'll *
# just exit when this happens.  As we can't be sure on the exact wording  *
# of the error message, we're just looking for the word "reset".          *
#**************************************************************************

    CORE::exit() if !$! or $! =~ m#reset#i;

# Die, there was an error

    _croak( "Could not read length of message from $CLIENT2TID{$client}: $!\n");
} #_length

#---------------------------------------------------------------------------
#  IN: 1 POSIX signals to delay (until unblocked)

sub _block_sigset {
    my @signals = @_;
    my $sigset = POSIX::SigSet->new(@signals);
    my $old_sigset = POSIX::SigSet->new;
    unless (defined POSIX::sigprocmask(SIG_BLOCK, $sigset, $old_sigset)) {
        die "Could not block ".CORE::join(',', @signals)."\n";
    }
    return $old_sigset;
} #_block_sigset

#---------------------------------------------------------------------------
#  IN: 1 POSIX::SigSet object containing original sigset mask returned by _block_sigset

sub _unblock_sigset {
    my $old_sigset = shift;
    unless (defined POSIX::sigprocmask(SIG_UNBLOCK, $old_sigset)) {
        die "Could not unblock ".CORE::join(',', @DEFERRED_SIGNALS)."\n";
    }
    return 1;
} #_unblock_sigset

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
_log( "> ".CORE::join(' ',map {$_ || ''} eval {_unpack( substr($frozen,4) )}) )
 if DEBUG;

# Block signals, if using custom CHLD signal handler
# Send the data, find out how many really got sent
# Unblock signals, if using custom CHLD signal handler
# Die now if an error has occurred
# Die now if not all bytes sent

    $frozen =~ m#^(.*)$#s;
    my $old_sigset = _block_sigset(@DEFERRED_SIGNALS) if $CUSTOM_SIGCHLD;
    my $sent = send( $client,$1,0 );
    _unblock_sigset($old_sigset) if $CUSTOM_SIGCHLD;
    _croak( "Error when sending message to $CLIENT2TID{$client}: $!" )
     unless defined($sent);
    _croak( "Did not send all bytes: only $sent of $length to $CLIENT2TID{$client}\n" )
     unless $sent == $length;
} #_send

#---------------------------------------------------------------------------
#  IN: 1 client object
# OUT: 1..N parameters of message

sub _receive {

# Obtain the client object
# Block signals, if using custom CHLD signal handler
# Obtain the length
# Initialize the data to be received

    my $client = shift;
    my $old_sigset = _block_sigset(@DEFERRED_SIGNALS) if $CUSTOM_SIGCHLD;
    my $length = my $todo = _length( $client );
    my $frozen;

# While we successfully get all data
#  Add what we got this time
#  If we got it all
#   Untaint what we got
#   Obtain any parameters if possible
#   Return the result
#  Set up for next attempt to fetch
# Unblock signals, if using custom CHLD signal handler

    while (defined recv( $client,my $data,$todo,0 )) {
        $frozen .= $data;
        if (length( $frozen ) == $length) {
            $frozen =~ m#^(.*)$#s;
            my @result = @{thaw( $1 )};
_log( "< @{[map {$_ || ''} @result]}" ) if DEBUG;
            return wantarray ? @result : $result[0];
        }
        $todo -= length( $data );
    }
    _unblock_sigset($old_sigset) if $CUSTOM_SIGCHLD;

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
#    Push tid on ppid2tid queue, if thread has a parent (e.g. not main thread)
#    Mark the thread as detached if so requested
#    Set status to indicate success

    my ($client,$tid,$pid,$ppid,$detach) = @_;
    my $status = 0;
    if ($pid) {
        if ($TID2CLIENT{$tid}) {
            unless (exists $PID2TID{$pid}) {
                $TID2PID{$tid} = $pid;
                $PID2TID{$pid} = $tid;
                push @{$PPID2CTID_QUEUE{$ppid}}, $tid if $ppid;
                $DETACHED{$tid} = undef if $detach;
                $status = 1;
            }
        }

#   If thread has a parent and there is a thread waiting for this ppid/ctid pair
#    Let that thread know
#    And forget that it was waiting for it

        if ($ppid && (my $blocking = $BLOCKING_PPID2CTID_QUEUE{$ppid})) {
            _ppid2ctid_shift( $blocking,$ppid );
            delete( $BLOCKING_PPID2CTID_QUEUE{$ppid} );
        }
    }

# Let the client know how it went

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
#      2 process id of thread calling this method
# OUT: 1 associated thread id

sub _ppid2ctid_shift { $WRITE{$_[0]} = _pack( shift @{$PPID2CTID_QUEUE{$_[1]}} ); } #_ppid2ctid_shift

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

sub _waitppid2ctid {

# If there is already a thread id for this process id, set that
# Start waiting for the tid to arrive

    return &_ppid2ctid_shift if defined $PPID2CTID_QUEUE{$_[1]} && @{$PPID2CTID_QUEUE{$_[1]}};
    $BLOCKING_PPID2CTID_QUEUE{$_[1]} = $_[0];
} #_waitppid2ctid

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
        eval $remote->{'eval'} if defined( $remote->{'eval'} );
        if (my $use = $remote->{'use'} || '') {
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
    my @result;
    if ($code) {
        foreach ($code->( $object,@_ )) {
            if (my $ref = reftype($_)) {
                my $tied = $ref eq 'SCALAR' ? tied ${$_}
                    : $ref eq 'ARRAY' ? tied @{$_}
                    : $ref eq 'HASH' ? tied %{$_}
                    : $ref eq 'GLOB' ? tied *{$_}
                    : undef;
                if (defined $tied && blessed($tied) eq 'threads::shared') {
                    my $ref_obj = $TIED[$tied->{'ordinal'}];
                    bless($_, blessed(${$ref_obj})) if blessed(${$ref_obj});                    
                }
            }
            push @result, $_;
        }
    }
    $WRITE{$client} = $code ? _pack( @result ) : $undef;
} #_tied

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to bless
#      3 class type with which bless object
# OUT: 1 whether successful

sub _bless {

# Obtain the socket
# Obtain the ordinal number of the variable
# Set the tied object's blessed property

    my $client = shift;
    my $ordinal = shift;
    my $class = shift;
    bless(${$TIED[$ordinal]}, $class);

# Indicate that we're done to the client

    $WRITE{$client} = $true;
} #_bless

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
#      2 ordinal number of (signal) variable to start waiting for
#      3 (optional) ordinal number of lock variable

sub _wait {

# If this is second form of cond_wait
#  Store ordinal of signal variable
#  Check if the lock variable is locked and return ordinal number and thread id
# Else
#  Check if the variable is locked and return ordinal number and thread id
#  Lock ordinal and ordinal are the same in this case; assign ordinal value to lock ordinal
# Unlock the variable
# Add this thread to the list of threads in cond_wait on this variable

    my ($ordinal,$tid,$l_ordinal);
    if (scalar @_ > 2) {
        $ordinal = $_[1];
        ($l_ordinal,$tid) = _islocked( @_[0,2],'cond_wait' );
    } else {
        ($ordinal,$tid) = _islocked( @_,'cond_wait' );
        $l_ordinal = $ordinal;
    }
    _unlock_ordinal( $l_ordinal );
    push( @{$WAITING[$ordinal]},[$tid, $l_ordinal] );
} #_wait

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to start timed waiting for
#      3 absolute expiration time (epoch seconds) of timedwait event
#      4 (optional) ordinal number of lock variable

sub _timedwait {

# If this is second form of cond_wait
#  Store ordinal of signal variable
#  Check if the lock variable is locked and return ordinal number and thread id
# Else
#  Check if the variable is locked and return ordinal number and thread id
#  Lock ordinal and ordinal are the same in this case; assign ordinal value to lock ordinal
# Unlock the variable
# Add this thread to the list of threads in cond_timedwait on this variable

    my ($ordinal,$tid,$l_ordinal);
    my $time = splice(@_, 2, 1);
    if (scalar @_ > 2) {
        $ordinal = $_[1];
        ($l_ordinal,$tid) = _islocked( @_[0,2],'cond_timedwait' );
    } else {
        ($ordinal,$tid) = _islocked( @_,'cond_timedwait' );
        $l_ordinal = $ordinal;
    }
    _unlock_ordinal( $l_ordinal );
    push( @{$TIMEDWAITING{$ordinal}},[$tid, $ordinal, $time, $l_ordinal, ++$TIMEDWAITING_ID] );
    $TIMEDWAITING_IDX_EXPIRED = 1;
} #_timedwait

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to signal one

sub _signal {

# Obtain local copy of the client
# Check if the variable is locked and return its ordinal number
# If the signal ordinal is the same as the lock ordinal or the variable they are waiting to relock is currently locked
#  Add the next thread id from the list of waiting or timed waiting threads (if any) to the head of the locking list
# Else (lock var is not same as signal var and lock var is currently unlocked)
#  Assign lock to this tid
#  Immediately notify blocking thread that it should continue
# Make sure the client continues

    my $client = shift;
    my $ordinal = _islocked( $client,shift,'cond_signal' );
    my ($tid, $l_ordinal);
    if (defined $WAITING[$ordinal] && ref($WAITING[$ordinal]) eq 'ARRAY' && @{$WAITING[$ordinal]}) {
        ($tid, $l_ordinal) = @{shift(@{$WAITING[$ordinal]})};
        if ($ordinal == $l_ordinal || defined $LOCKED[$l_ordinal]) {
            unshift( @{$LOCKING[$l_ordinal]}, $tid );
        } else {
            $LOCKED[$l_ordinal] = $tid;
            $WRITE{$TID2CLIENT{$tid}} = $true;
        }
    }
    elsif (defined $TIMEDWAITING{$ordinal} && ref($TIMEDWAITING{$ordinal}) eq 'ARRAY' && @{$TIMEDWAITING{$ordinal}}) {
        ($tid, $l_ordinal) = @{shift(@{$TIMEDWAITING{$ordinal}})}[0,3];
        if ($ordinal == $l_ordinal || defined $LOCKED[$l_ordinal]) {
            unshift( @{$LOCKING[$l_ordinal]}, $tid );
        } else {
            $LOCKED[$l_ordinal] = $tid;
            $WRITE{$TID2CLIENT{$tid}} = $true;
        }
        delete $TIMEDWAITING{$ordinal} unless @{$TIMEDWAITING{$ordinal}};
        $TIMEDWAITING_IDX_EXPIRED = 1;
    }
    $WRITE{$client} = $true;
} #_signal

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to signal all

sub _broadcast {

# Obtain local copy of the client
# Check if the variable is locked and return its ordinal number
# If there are threads waiting or timed waiting
#  For all waiting or timed waiting threads
#   If the signal ordinal is the same as the lock ordinal or the variable they are waiting to relock is currently locked
#    Add it to the head of the locking list
#   Else (lock var is not same as signal var and lock var is currently unlocked)
#    Assign lock to this tid
#    Immediately notify blocking thread that it should continue
# Make sure the client continues

    my $client = shift;
    my $ordinal = _islocked( $client,shift,'cond_broadcast' );
    my ($tid, $l_ordinal);
    if (defined $WAITING[$ordinal] && ref($WAITING[$ordinal]) eq 'ARRAY' && @{$WAITING[$ordinal]}) {
        foreach (@{$WAITING[$ordinal]}) {
            ($tid, $l_ordinal) = @{$_};
            if ($ordinal == $l_ordinal || defined $LOCKED[$l_ordinal]) {
                unshift( @{$LOCKING[$l_ordinal]}, $tid );
            } else {
                $LOCKED[$l_ordinal] = $tid;
                $WRITE{$TID2CLIENT{$tid}} = $true;
            }
            delete $WAITING[$ordinal];
        }
    }
    if (defined $TIMEDWAITING{$ordinal} && ref($TIMEDWAITING{$ordinal}) eq 'ARRAY' && @{$TIMEDWAITING{$ordinal}}) {
        foreach (@{$TIMEDWAITING{$ordinal}}) {
            ($tid, $l_ordinal) = @{$_}[0,3];
            if ($ordinal == $l_ordinal || defined $LOCKED[$l_ordinal]) {
                unshift( @{$LOCKING[$l_ordinal]}, $tid );
            } else {
                $LOCKED[$l_ordinal] = $tid;
                $WRITE{$TID2CLIENT{$tid}} = $true;
            }
            delete $TIMEDWAITING{$ordinal};
            $TIMEDWAITING_IDX_EXPIRED = 1;
        }
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

# Initialize the thread id and target lock ordinal
# Initialize default response to true
# If there exist any timed waiting events that expired and are waiting to relock
#  Get the thread id and target lock ordinal
#  Set response to false (indicating this event timed out)

    my ($tid, $l_ordinal);
    my $response = $true;
    if (ref($TIMEDWAITING_EXPIRED[$ordinal]) eq 'ARRAY' && @{$TIMEDWAITING_EXPIRED[$ordinal]}) {
        ($tid, $l_ordinal) = @{shift @{$TIMEDWAITING_EXPIRED[$ordinal]}};
        $response = $false;
    } else {
        $l_ordinal = $ordinal;
    }

# Obtain thread id from locking list if there is no thread id yet
# If there is a thread id for the lock
#  Make that the thread locking the variable
#  And have that thread continue
# Else (still no thread wanting to lock)
#  Just reset the lock for this variable

    $tid = shift(@{$LOCKING[$l_ordinal]}) unless defined $tid;
    if (defined $tid){
        $LOCKED[$l_ordinal] = $tid;
        $WRITE{$TID2CLIENT{$tid}} = $response;
    } else {
        $LOCKED[$l_ordinal] = undef;
    }
} #_unlock_ordinal

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to start waiting for
#      3 function name to show when there is an error (undef: no error if wrong)
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
    if (!defined $LOCKED[$ordinal] || $tid != $LOCKED[$ordinal]) {
        return unless $_[0];
        _croak( "Must first lock variable #$ordinal ($tid != $LOCKED[$ordinal]) before doing a $_[0]" );
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

sub _croak { return &Carp::confess(threads->tid." ($$): ".shift) } #_croak

#---------------------------------------------------------------------------
#  IN: 1 message to log

sub _log {

# Obtain the message
# If it is a thread message
#  Obtain the thread id
#  Prefix thread id value
# Shorten message if _very_ long
# Log it

    my $message = shift;
    if (substr($message,0,1) ne ' ') {
        my $tid = defined($TID) ? $TID : '?';
        $message = "$tid $message";
    }
    $message = substr($message,0,256)."... (".(length $message)." bytes)"
     if length( $message ) > 256;
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
=pod

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

  # use forks as a drop-in replacement for an ithreads application
  perl -Mforks -Mforks::shared threadapplication
  
  # support fractional and use hi-res time for cond_timedwait events
  perl -MTime::HiRes -Mforks -Mforks::shared threadapplication

=head1 DESCRIPTION

The "forks" pragma allows a developer to use threads without having to have
a threaded perl, or to even run 5.8.0 or higher.  There were a number of goals
that I am trying to reach with this implementation.

=over 2

Using this module B<only> makes sense if you run on a system that has an
implementation of the C<fork> function by the Operating System.  Windows
is currently the only known system on which Perl runs which does B<not>
have an implementation of C<fork>.  Therefore, it B<doesn't> make any
sense to use this module on a Windows system.  And therefore, a check is
made during installation barring you from installing on a Windows system.

=back

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
L</"isthread">).  This is as yet untested within Apache, but should work.

=head2 same API as threads

You should be able to run threaded applications unchanged by simply making
sure that the "forks" and "forks::shared" modules are loaded, e.g. by
specifying them on the command line.

=head2 using as a development / demonstration tool

Because you do not need a threaded Perl to use forks.pm, you can start
prototyping threaded applications with the Perl executable that you are used
to.  Just download and install the "forks" package from CPAN.  So
the threshold for trying out threads in Perl has become much lower.  Even
Perl 5.005 should, in principle, be able to support the forks.pm module;
however, some issues with regards to the availability of XS features between
different versions of Perl, it seems that 5.6.0 (unthreaded) is what you need
at least.

=head2 using in production environments

This package has successfully been proven as stable and reliable in production 
environments.  I have personally used it in high-availability, database-driven, 
financial message processing server applications for more than two years now with 
great success.  Also, unlike pure ithreads, forks.pm is fully compatible with all 
perl modules, whether or not they have been updated to be ithread safe.  This 
means that you do not need to feel limited in what you can develop as a threaded 
perl application, a problem that continues to plague the acceptance of ithreads in
production enviroments today.  Just handle these modules as you would when using a 
standard fork: be sure to create new instances of, or connections to, resources where
a single instance can not be shared between multiple processes.

The only major concern is the potentially slow (relative to pure ithreads) performance
of shared data and locks.  If your application doesn't depend on extensive semaphore
use, and reads/writes from shared variables moderately (such as using them primarily
to deliver data to a child thread to process and the child thread uses a shared
structure to return the result), then this will likely not be an issue for your
application.  See the TODO section regarding plans to tackle this issue.

Also, you may wish to try <forks::BerkeleyDB>, which has shown signifigant performance
gains and consistent throughoutput in high-concurrency shared variable applications.

=head1 REQUIRED MODULES

 Devel::Required (any)
 IO::Socket (1.18)
 reaper (0.03)
 Scalar::Util (1.01)
 Storable (any)

=head1 IMPLEMENTATION

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

The C<isthread> class method attempt to make a connection with the shared
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

Debugging can B<only> be switched on by defining the environment variable
C<THREADS_DEBUG>.  If the environment variable does not exist when the forks.pm
module is compiled, then all debugging code will be optimised away to create
a better performance.  If the environment variable has a true value, then
debugging will also be enabled from the start.

=head1 EXTRA FEATURES

=head2 INET socket IP mask

For security, inter-thread communication INET sockets only will allow connections
from the default local machine IPv4 loopback address (e.g 127.0.0.1).  However,
this filter may be modified by defining the environment variable C<THREADS_IP_MASK>
with a standard perl regular expression (or with no value, which would disable the
filter).

=head2 UNIX socket support

For users who do not wish to (or can not) use TCP sockets, UNIX socket support
is available.  This can be B<only> switched on by defining the environment
variable C<THREADS_SOCKET_UNIX>.  If the environment variable has a true value, then
UNIX sockets will be used instead of the default TCP sockets.  Socket descriptors 
are currently written to /var/tmp and given a+rw access by default (for cleanest 
functional support on multi-user systems).

This feature is excellent for applications that require extra security, as it
does not expose forks.pm to any INET vunerabilities your system may be
subject to (i.e. systems not protected by a firewall).  It also may
provide an additional performance boost, as there is less system overhead
necessary to handle UNIX vs INET socket communication.

=head1 CAVEATS

Some caveats that you need to be aware of.

=head2 Greater latency

Because of the use of sockets for inter-thread communication, there is an
inherent larger latency with the interaction between threads.  However, the
fact that TCP sockets are used, may open up the possibility to share threads
over more than one physical machine.

You may decrease some latencyby using UNIX sockets (see L</"UNIX socket support">).

Also, you may wish to try <forks::BerkeleyDB>, which has shown signifigant performance
gains and consistent throughoutput in applications requiring high-concurrency shared
variable access.

=head2 Modules that modify $SIG{CHLD}

In order to be compatible with perl's core system() function on all platforms,
extra care has gone into implementing a smarter $SIG{CHLD} in forks.pm.  If any
modules you use modify $SIG{CHLD} (or if you attempt to modify it yourself), 
you may end up with undesired issues such as unreaped processes or a system()
function that returns -1 instead of the correct exit value.  See L<perlipc>
for more information regarding common issues with modifying $SIG{CHLD}.

If $SIG{CHLD} has to be modified in any way by your software, please take extra
care to implement a handler that follows the requirements of chained signal
handlers.  See L<reaper> for more information.

You may define the environment variable THREADS_SIGCHLD_IGNORE to to force 
forks to use 'IGNORE' on systems where a custom CHLD signal handler has been
automatically installed to support correct exit code of perl core system()
function.  There should be no need to use this unless you encounter specific
issues with L<reaper> signal chaining.

=head2 Source filter

To get forks.pm working on Perl 5.6.x, it was necessary to use a source
filter to ensure a smooth upgrade path from using forks under Perl 5.6.x to
Perl 5.8.x and higher.  The source filter used is pretty simple and may
prove to be too simple.  Please report any problems that you may find when
running under 5.6.x.

=head1 TODO

See the TODO file in the distribution.

=head1 KNOWN PROBLEMS

These problems are known and will hopefully be fixed in the future:

=over 2

=item signalling unlocked variables

In the standard Perl ithreads implementation, you can signal a variable without
having to lock() it.  This causes a (suppressable) warning.  Due to
implementation details, probably having to do with communication getting out
of sync between server and client thread, forks.pm needs to die when this
happens.  Patches are welcome.

=item test-suite exits in a weird way

Although there are no errors in the test-suite, the test harness sometimes
thinks there is something wrong because of an unexpected exit() value.  This
is an issue with Test::More's END block, which wasn't designed to co-exist
with a threads environment and forked processes.  Hopefully, that module will
be patched in the future, but for now, the warnings are harmless and may be
safely ignored.

=item share() doesn't lose value for arrays and hashes

In the standard Perl threads implementation, arrays and hashes are
re-initialized when they become shared (with the share()) function.  The
share() function of forks::shared does B<not> initialize arrays and hashes
when they become shared with the share() function.

This B<could> be considered a bug in the standard Perl implementation.  In any
case this is an inconsistency of the behaviour of threads.pm and forks.pm.
Maybe a special "totheletter" option should be added to forks.pm to make
forks.pm follow this behaviour of threads.pm to the letter.

And of course, there might be other, undiscovered issues.  Patches are welcome!

=back

=head1 ORIGINAL AUTHOR CREDITS

All the people reporting problems and fixes.  More specifically in
alphabetical order:

=over 2

=item Stephen Adkins

For finding that a child thread could not wake the very first parent thread
with cond_signal, and providing a patch to fix it.

=item Arthur Bergman

For implementing the first working version of Perl threads support and
providing us with an API to build on.

=item Lars Fenneberg

For helping me through the initial birthing pains.

=item Paul Golds

For spotting a problem with very large shared scalar values.

=item Bradley W. Langhorst

For making sure everything runs with warnings enabled.

=item Juerd Waalboer

For pointing me to the source filter solution for Perl 5.6.x.

=back

=head1 CURRENT MAINTAINER

Eric Rybski <rybskej@yahoo.com>.

=head1 ORIGINAL AUTHOR

Elizabeth Mattijsen, <liz@dijkmat.nl>.

=head1 COPYRIGHT

Copyright (c)
 2005-2006 Eric Rybski <rybskej@yahoo.com>,
 2002-2004 Elizabeth Mattijsen <liz@dijkmat.nl>.
All rights reserved.  This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

L<threads>, L<forks::BerkeleyDB>.

=cut
