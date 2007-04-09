package forks;   # make sure CPAN picks up on forks.pm
$VERSION = '0.23';

package threads; # but in fact we're masquerading as threads.pm

# Make sure we have version info for this module
# Set flag to indicate that we're really the original threads implementation
# Set flag to indicate that we're not really the original threads implementation
# Flag whether or not module is loaded in namespace override mode (e.g. threads.pm)
# Be strict from now on

$VERSION = '1.26';
$threads        = $threads        = 1; # twice to avoid warnings
$forks::threads = $forks::threads = 1; # twice to avoid warnings
$forks::threads_override = $forks::threads_override = 0; # twice to avoid warnings
use strict;
use warnings;

# Load default signal handler for most signals
# Import additional scalar methods for refs and objects
# Load library to set temp dir for forks data

use sigtrap ('handler', \&_sigtrap_handler,
    qw(untrapped normal-signals error-signals));
use Scalar::Util qw(reftype blessed refaddr);
use File::Spec;

# Set constant for IPC temp dir
# Set constant for IPC temp thread signal notifications

use constant ENV_ROOT   => File::Spec->tmpdir().'/perlforks';
use constant ENV_SIGNALS => ENV_ROOT.'/signals';

# Set constants for threads->list() operations: all, running, and joinable

use constant all      => ();
use constant running  => 1;
use constant joinable => 0;

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
# Modify Perl's Config.pm to simulate that it was built with ithreads

BEGIN {
    require Config;
    my $h = tied %Config::Config;
    $h->{useithreads} = 1;
}

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

# Load the XS stuff

require XSLoader;
XSLoader::load( 'forks',$forks::VERSION );

# Make sure we can die with lots of information
# Make sure we can do sockets and have the appropriate constants
# Make sure we can do select() on multiple sockets
# Make sure we have the necessary POSIX constants
# Make sure that we can freeze and thaw data structures
# Allow for chainable child reaping functions
# Enable hi-res time

use Carp       ();
use Socket     qw(SOMAXCONN);
use IO::Socket ();
use IO::Select ();
use POSIX      qw(WNOHANG
    BUFSIZ O_NONBLOCK F_GETFL F_SETFL
    SIG_BLOCK SIG_UNBLOCK SIGCHLD SIGKILL
    ECONNABORTED ECONNRESET EAGAIN EINTR EWOULDBLOCK);
use Storable   qw(freeze thaw);
use reaper     qw(reapPid);
use Time::HiRes qw(sleep time);
use List::MoreUtils;

# Flag whether or not forks has initialized the server process
# Thread local query server object
# The port on which the thread server is listening
# The process id in which the shared variables are stored
# Initialize thread local hash (key: pid) whether this process is a thread
# Thread local flag whether we're shutting down
# Thread local flag whether we're shut down

my $HANDLED_INIT = 0;
my $QUERY;
my $PORT;
my $SHARED;
my %ISATHREAD;
my $SHUTTING_DOWN;
my $SHUTDOWN;

# Initialize the flag that indicates that we're still running
# Initialize the number of bytes to read at a time
# SigSet of signals that we will delay if target platform requires custom CHLD handler
# Pseudo-signal mask indicating signals to handle when thread finished current server message handling
# Boolean indicating whether or not platform requires a custom CHLD handler
# Max sleep time of main server loop before looping once
# Initialize hash (key: client) with info to be written to client threads
# Initialize hash (key: client) with clients that we're done with
# Initialize the "thread local" thread id
# Initialize the pid of the thread
# Return context of thread (possible values are same as those of CORE::wantarray)
# Initialize hash (key: module) with code references of CLONE subroutines

my $RUNNING = 1;
my $BUFSIZ  = BUFSIZ;
my $DEFERRED_CHLD_SIGSET = POSIX::SigSet->new(SIGCHLD);
my %DEFERRED_SIGNAL;
my $CUSTOM_SIGCHLD = 0;
my $MAX_POLL_SLEEP = 60;    #seconds
my %WRITE;
my %DONEWITH;
my $TID;
my $PID;
my $THREAD_CONTEXT;
my %CLONE;

# Initialize the next thread ID to be issued
# Initialize hash (key: tid) with the thread id to client object translation
# Initialize hash (key: client) with the client object to thread id translation
# Initialize hash (key: tid) with the thread id to process id translation
# Initialize hash (key: pid) with the process id to thread id translation
# Initialize hash (key: ppid) with the parent pid to child tid queue (value: array ref)
# Initialize hash (key: tid) with the thread id to thread join context translation

my $NEXTTID = 0;
my %TID2CLIENT;
my %CLIENT2TID;
my %TID2PID;
my %PID2TID;
my %PPID2CTID_QUEUE;
my %TID2CONTEXT;

# Initialize hash (key: tid) with tid's that have been detached
# Initialize hash (key: tid) with detached threads are no longer running
# Initialize hash (key: tid) with results from threads
# Initialize hash (key: tid) with threads that have been joined

my %DETACHED;
my %DETACHED_DONE;
my %RESULT;
my %JOINED;

# Initialize hash (key: ppid) with clients blocking of ppid->ctid conversion
# Initialize hash (key: tid) with clients blocking for join() result
# Initialize period (seconds) of BLOCKING_JOIN check (abnormal thread death protection)
# Initialize time of next BLOCKING_JOIN check

my %BLOCKING_PPID2CTID_QUEUE;
my %BLOCKING_JOIN;
my $BLOCKING_JOIN_CHECK_PERIOD = 15;
my $BLOCKING_JOIN_CHECK_TS_NEXT = 0;

# Initialize hash (key: fq sub) with code references to tie subroutines
# List with objects of shared (tied) variables
# Ordinal number of next shared (tied) variable

my %DISPATCH;
my @TIED;
my $NEXTTIED = 1;

# Initialize list (key: ordinal) of threads that have the lock for a variable
# Initialize hash (key: ordinal) of TID caller information from the (non-recursive) lock()
# Initialize list (key: ordinal) of threads that have a recursive lock
# Initialize list (key: ordinal) of threads that want to lock a variable
# Initialize list (key: ordinal) of threads are waiting in cond_wait
# Initialize hash (key: ordinal) of threads are waiting in cond_timedwait
# Initialize scalar representing unique ID of each timed event
# Initialize list (order: expiration time) representing a sorted version (pseudo-index) of %TIMEDWAITING
# Initialize scalar indicating when %TIMEDWAITING has changed and @TIMEDWAITING_IDX should be recalculated
# Initialize list (key: ordinal; subkey: tid) of TIMEDWAITING events that have timed out

my @LOCKED;
my %TID2LOCKCALLER;
my @RECURSED;
my @LOCKING;
my @WAITING;
my %TIMEDWAITING;
my $TIMEDWAITING_ID = 0;
my @TIMEDWAITING_IDX;
my $TIMEDWAITING_IDX_EXPIRED = 0;
my @TIMEDWAITING_EXPIRED;

# Initialize hash (key: tid, value=signal) with clients to send sigals to

my %TOSIGNAL;

# Flag indicating whether deadlock detection enabled (default: disabled)
# Deadlock detection period (0 => sync detection; else async detect every N sec)
# Time of next deadlock detection event, if in asynchronous mode
# Initialize hash (key: tid; value: tid of blocker) with clients that are deadlocked
# Flag of whether server should terminate one thread of each deadlocked thread pair
# Signal to use to kill deadlocked processes

my $DEADLOCK_DETECT = 0;
my $DEADLOCK_DETECT_PERIOD = 0;
my $DEADLOCK_DETECT_TS_NEXT = 0;
my %DEADLOCKED;
my $DEADLOCK_RESOLVE = 0;
my $DEADLOCK_RESOLVE_SIG = SIGKILL;

# Create packed version of undef
# Create packed version of false
# Create packed version of true
# Create packed version of empty list

my $undef = _pack_response( [undef],  );
my $false = _pack_response( [0], '__boolean' );
my $true  = _pack_response( [1], '__boolean' );
my $empty = _pack_response( [],  );

# Miscellaneous command-related constants
# Command filters (closures) for optimized request/response handling

my %cmd_filter;
my @cmd_filtered;
my @cmd_num_to_filter;
my @cmd_num_to_type;
my %cmd_type_to_num;
BEGIN {
    use constant {
        CMD_FLTR_REQ    => 0,
        CMD_FLTR_RESP   => 1,
        CMD_FLTR_ENCODE => 0,
        CMD_FLTR_DECODE => 1,

        CMD_TYPE_DEFAULT    => 0,   #entire content is frozen
        CMD_TYPE_INTERNAL   => 1,   #msg has a custom filter

        MSG_LENGTH_LEN                  => 4,
        CMD_TYPE_IDX                    => 0,
        CMD_TYPE_LEN                    => 1,
        CMT_TYPE_FROZEN_CONTENT_IDX     => 1,
        CMD_TYPE_INTERNAL_SUBNAME_IDX   => 1,
        CMD_TYPE_INTERNAL_SUBNAME_LEN   => 2,
        CMD_TYPE_INTERNAL_CONTENT_IDX   => 3,
    };
    %cmd_filter = (  #pack: 1 arrayref input param; unpack: 1 scalar input param; pack/unpack: list output
        __boolean   => [    #client-to-server
            [   #request
                sub { $_[0]->[0] ? '1' : '0'; }, #pack
                sub { $_[0]; }  #unpack
            ],
            [   #response
                sub { $_[0]->[0] ? '1' : '0'; }, #pack
                sub { $_[0]; }  #unpack
            ],
        ],
    );
    %cmd_filter = (  #pack: 1 arrayref input param; unpack: 1 scalar input param; pack/unpack: list output
        %cmd_filter,
        _lock   => [    #client-to-server
            [   #request
                sub { pack('IIa*', @{$_[0]}[0..2]); }, #pack
                sub { unpack('IIa*', $_[0]); }  #unpack
            ],
            $cmd_filter{__boolean}->[CMD_FLTR_RESP]   #response
        ],
        _unlock   => [    #client-to-server
            [   #request
                sub { pack('I', $_[0]->[0]); }, #pack
                sub { unpack('I', $_[0]); }  #unpack
            ],
            $cmd_filter{__boolean}->[CMD_FLTR_RESP]   #response
        ],
    );
    @cmd_filtered = sort { lc($a) cmp lc($b) } keys %cmd_filter;
    for (my $i = 0; $i < scalar @cmd_filtered; $i++) {
        $cmd_num_to_filter[$i] = $cmd_filter{$cmd_filtered[$i]};
        $cmd_num_to_type[$i] = $cmd_filtered[$i];
        $cmd_type_to_num{$cmd_filtered[$i]} = $i;
    }
} #BEGIN

# Make sure that equality works on thread objects

use overload
 '==' => \&equal,
 '!=' => \&nequal,
 'fallback' => 1,
;

# Create new() -> create() equivalence

*create = \&new; create() if 0; # to avoid warning

# Overload global fork for best protection against external fork.

BEGIN {
    no warnings 'redefine';
    *CORE::GLOBAL::fork = sub {
    
# Perform the fork
# Clear critical state variables in child process
# Return the forked pid

        my $pid = CORE::fork();
        if (defined $pid && $pid == 0) { #in child
            delete $ISATHREAD{$$};
            undef( $TID );
            undef( $PID );
        }
        return $pid;
    };
} #BEGIN

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
# Initialize some local vars
# If sub is a hash ref
#  Assume thread-specific params were defined
#  Obtain the actual subroutine
#  Parse stack_size -- not supported (yet)
#  Parse thread context (presidence given to param over implicit context)
# Else
#  Store implicit thread context
    my $class = shift;
    my $sub = shift;
    my ($param, $stack_size, $thread_context);
    if (ref($sub) eq 'HASH') {
        $param = $sub;
        $sub = shift;
#        if (exists $param->{'stack_size'}) {}
        if ((exists $param->{'context'} && $param->{'context'} eq 'list')
            || (exists $param->{'list'} && $param->{'list'}))
        {
            $thread_context = 1;
        } elsif ((exists $param->{'context'} && $param->{'context'} eq 'scalar')
            || (exists $param->{'scalar'} && $param->{'scalar'}))
        {
            $thread_context = 0;
        } elsif ((exists $param->{'context'} && $param->{'context'} eq 'void')
            || (exists $param->{'void'} && $param->{'void'}))
        {
            $thread_context = undef;
        } else {
            $thread_context = CORE::wantarray;
        }
    } else {
        $thread_context = CORE::wantarray;
    }

# If it is not a code ref yet (other refs will bomb later)
#  Make the subroutine fully qualified if it is not yet
#  Turn the name into a reference

    unless (ref($sub)) {
        $sub = caller().'::'.$sub unless $sub =~ m#::#;
        $sub = \&{$sub};
    }

# Initialize the process id of the thread
# If it seems we're in the child process
#  If the fork failed
#   Print a detailed warning
#   Return undefined to indicate the failure

    my $pid;
    unless ($pid = fork) {
        unless (defined( $pid )) {
            warnings::warnif("Thread creation failed: Could not fork child from pid $$, tid $TID: ".($! ? $! : ''));
            return undef;
        }

#  Store the parent PID
#  Set up the connection for handling queries
#  If thread context is defined
#   If context is list
#    Execute the routine that we're supposed to execute (list context)
#   Else
#    Execute the routine that we're supposed to execute (scalar context)
#  Else
#    Execute the routine that we're supposed to execute (void context)
#  Print warning if thread terminated abnormally (if not main thread)
#  Mark this thread as shutting down
#  Save the result
#  And exit the process

        _init_thread($thread_context);
        my @result;
        if (defined $thread_context) {
            if ($thread_context) {
                eval { @result = $sub->( @_ ); };
            } else {
                eval { $result[0] = $sub->( @_ ); };
            }
        } else {
            eval { $sub->( @_ ); };
        }
#warn "$TID: context = ".(defined $thread_context ? $thread_context ? 'array' : 'scalar' : 'void').",result (".scalar(@result).")=".CORE::join(',',@result); #TODO: for debugging only
        warn "Thread $TID terminated abnormally: $@"
            if $@ && $TID && warnings::enabled();
        $SHUTTING_DOWN = 1;
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
    _init_thread( undef, 1 );
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
#  IN: 2 (optional) boolean value indicating type of list desired
# OUT: 1..N instantiated objects

sub list {

# Obtain the class
# Obtain the hash with process ID's keyed to thread ID's
# Initialize list of objects
# For all of the threads, ordered by ID
#  Add instantiated object for this thread
# Return the list of instantiated objects, or num of objects in scalar context

    my $class = shift;
    my %hash = _command( '_list_tid_pid', @_ );
    my @object;
    foreach (sort {$a <=> $b} keys %hash) {
        push( @object,$class->_object( $_,$hash{$_} ) );
    }
    wantarray ? @object : scalar @object;
} #list

#---------------------------------------------------------------------------

sub yield { sleep 0.001; } #yield

#---------------------------------------------------------------------------
#  IN: 1 class or instantiated object
# OUT: 1..N state of the indicated thread

sub is_detached { _command( '_is_detached',shift->tid ) } #is_detached

#---------------------------------------------------------------------------
#  IN: 1 class or instantiated object
# OUT: the memory location of the internal thread structure
# Note: this won't guarantee reusable address, as it's dynamically generated

sub _handle {

# Obtain the class or object
# If is an object, return address of object
# Otherwise, return address of class

    my $self = shift;
    return refaddr( $self->_object( $self->tid,$self->{'pid'} ) )
        if ref($self);
    return refaddr( $self->_object( $self->tid,$$ ) );

} #_handle

#---------------------------------------------------------------------------
#  IN: 1 class or instantiated object
# OUT: the thread (process) stack size
# Sorry, we can't do get_stack_size() with forks (yet)

sub get_stack_size { return 0; } #get_stack_size

#---------------------------------------------------------------------------
#  IN: 1 class
# OUT: the thread (process) stack size
# Sorry, we can't do set_stack_size() with forks (yet)

sub set_stack_size { return 0; } #set_stack_size

#---------------------------------------------------------------------------
#  IN: 1 class or instantiated object
# OUT: 1..N state of the indicated thread

# Obtain the class or object
# If is an object, return thread context of specified thread
# Otherwise, return thread context of current thread

sub wantarray {
    my $self = shift;
    return _command( '_wantarray',$self->tid ) if ref($self);
    return $THREAD_CONTEXT;
} #wantarray

#---------------------------------------------------------------------------

# instance methods

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
# OUT: 1..N results of the indicated thread

sub detach { _command( '_detach',shift->tid ) ? 1 : _croak('Thread already detached') } #detach

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
#  IN: 1 instantiated threads object
#      2 other instantiated threads object
# OUT: 1 whether they refer to the same thread

sub nequal { $_[0]->tid != $_[1]->tid } #nequal

#---------------------------------------------------------------------------
#  IN: 1 instantiated threads object
# OUT: 1 tid of the object

sub stringify { $_[0]->tid } #stringify

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
# OUT: 1 state of the indicated thread

sub is_running { _command( '_is_running',shift->tid ) } #is_running

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
# OUT: 1 state of the indicated thread

sub is_joinable { _command( '_is_joinable',shift->tid ) } #is_joinable

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
# OUT: 1 state of the indicated thread

sub is_deadlocked { _command( '_is_deadlocked',shift->tid ) } #is_deadlocked

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
#      2 signal number or name to send
# OUT: 1 thread object

sub kill {

# Get the object
# Get the signal
# Die if incorrect usage
# Return immediately if no signal defined
# Die unless signal is valid
# Send signal
# Return thread object

    my $self = shift;
    my $signal = shift;
    Carp::croak("Usage: \$thr->kill('SIG...')") unless blessed($self);
    return $self unless defined $signal;
    Carp::croak("Unrecognized signal name or number: $signal")
        unless grep(/^$signal$/,
            map('SIG'.$_, split(/\s+/, $Config::Config{sig_name})),
            split(/\s+/, $Config::Config{sig_name}),
            split(/\s+/, $Config::Config{sig_num}));
    _command( '_kill',$self->tid,$signal );
    $self;
} #kill

#---------------------------------------------------------------------------

# exportables

#---------------------------------------------------------------------------
#  IN: 1 subroutine reference of sub to start execution with
#      2..N any parameters to be passed
# OUT: 1 instantiated object

sub async (&;@) {
    if (defined CORE::wantarray) {
        if (CORE::wantarray) {
            my @result = new( 'threads',@_ );
        } else {
            my $result = new( 'threads',@_ );
        }
    } else {
        new( 'threads',@_ );
    }
} #async

#---------------------------------------------------------------------------

# standard Perl features

#---------------------------------------------------------------------------
# Default reaper, if using custom CHLD signal handler (prevents zombies)

sub REAPER {
# While we have zombie processes, loop and reap (don't care about exit status)

    while (my $pid = waitpid(-1, WNOHANG) > 0) {}
} #REAPER

#---------------------------------------------------------------------------
# Default sigtrap handler

sub _sigtrap_handler {

# Obtain the signal sent
# Print a general warning (if not main thread)
# Mark this thread as shutting down (for quiet exit)
# Exit

    my ($sig) = @_;
    print STDERR "Signal SIG$sig received, but no signal handler set"
        ." for thread $TID\n"
        if exists($ISATHREAD{$$}) && $TID && $$ == $PID
            && warnings::enabled('threads');
    $SHUTTING_DOWN = 1;
    CORE::exit();
} #_sigtrap_handler

#---------------------------------------------------------------------------
#  IN: 1 class (ignored)
#      2..N subroutines to export (default: async only)

sub import {

# Obtain the class
# Add filter if we're filtering

    my $self = shift;
    filter_add( bless {},$self ) if $filtering;

# Fake that threads.pm was really loaded (this is the first time we're here)

    if (defined $INC{'forks.pm'}) {
        $INC{'threads.pm'} ||= $INC{'forks.pm'};
    } elsif (defined $INC{'threads.pm'} && $forks::threads_override) {
        $INC{'forks.pm'} ||= $INC{'threads.pm'}
    } elsif (defined $INC{'threads.pm'} && !$forks::threads_override) {
        _croak( "Can not mix 'use forks' with real 'use threads'" )
    }

# Overload string context of thread object to return TID, if requested
# If there seems to be a threads.pm loaded
#  If threads.pm appears loaded
#   Die if it really was a 'use threads'
#  Perform the export needed
#  And return
# Else
#  Perform the export needed

    if ((my $idx = List::MoreUtils::firstidx(
        sub { $_ eq 'stringify' }, @_)) >= 0) {
        import overload '""' => \&stringify;
        splice(@_, $idx, 1);
    }
    if ($HANDLED_INIT) {
        _export( scalar(caller()),@_ );
        return;
    } else {
        _export( scalar(caller()),@_ );
    }

_log( " ! global startup" ) if DEBUG;

# Create a server that can only take one connection at a time or die now
# Find out the port we're running on and save that for later usage
# Make sure that the server is non-blocking

    if ($THREADS_UNIX) {
        _croak( "UNIX socket file '$THREADS_UNIX$$' in use by non-socket file" )
            if -e $THREADS_UNIX.$$ && !-S $THREADS_UNIX.$$;
        _croak( "Unable to delete UNIX socket file '$THREADS_UNIX$$'" )
            if -S $THREADS_UNIX.$$ && !unlink($THREADS_UNIX.$$);
        $QUERY = IO::Socket::UNIX->new(
         Local  => $THREADS_UNIX.$$,
         Listen => SOMAXCONN,
        ) or _croak( "Couldn't start the listening server: $@" );
        chmod 0777, $THREADS_UNIX.$$;
        $PORT = $THREADS_UNIX.$$;
    } else {
        $QUERY = IO::Socket::INET->new(
         LocalAddr => '127.0.0.1',
         Listen    => SOMAXCONN,
        ) or _croak( "Couldn't start the listening server: $@" );
        $PORT = $QUERY->sockport;
    }
    _nonblock( $QUERY );

# Make sure that children will be reaped automatically
# Enable custom CHLD signal handler, if necessary
# If we appear to be in the child
#  Die if the fork really failed
#  Store PID of shared in $SHARED for server exit later
#  Start handling requests as the server
# Mark PID for reaping, if using custom CHLD signal handler
# Make this thread 0
# Mark forks initialization as complete

    $SIG{CHLD} = 'IGNORE';
    unless ($FORCE_SIGCHLD_IGNORE) {
        my $system_test;
        {
            local %ENV;
            $ENV{PATH} = "/bin:/usr/bin";
            local $SIG{__WARN__} = sub {};
            $system_test = system('test');
        }
        if ($system_test == -1) {
            $SIG{CHLD} = sub { reaper::REAPER ( shift, \&REAPER ); };
            $CUSTOM_SIGCHLD = 1;
        } else {
            $CUSTOM_SIGCHLD = 0;
        }
    }
    _server_pre_startup();
    unless ($SHARED = fork) {
        _croak( "Could not start initial fork" ) unless defined( $SHARED );
        $SHARED = $$;
        return &_server;
    }
    reapPid($SHARED) if $CUSTOM_SIGCHLD;
    _init_thread();
    $HANDLED_INIT = 1;
} #forks::import

BEGIN {

# forks::shared and threads::shared share same import method

    *forks::import = *forks::import = \&import;
}

# Functions to allow external modules an API hook to specific runtime states

sub _server_pre_startup {}
sub _server_post_startup {}
sub _end_server_pre_shutdown {}
sub _end_server_post_shutdown {}

#---------------------------------------------------------------------------

END {

# Localize $? to prevent accidental override during shutdown
# Revert to simple CHLD handler to insure portable, reliable shutdown
# If this process is the shared server
#  Calculate and report stats on running and/or unjoined threads (excluding main thread)
#  Forcefully terminate any lingering thread processes (except main thread)
#  Shutdown the socket server
#  Delete UNIX socket file if the socket file exists
#  Allow external modules opportunity to clean up thread process group resources
#  Return now
# If this process is a thread (and wasn't a forked process from a thread)
#  Mark this thread as shutting down (in case exited via exit() or a signal)
#  Indicate that this process has been shut down to the server
#  Mark this thread as shut down (so we won't send or receive anymore)

    local $?;
    $SIG{CHLD} = 'IGNORE';
    if (!exists( $ISATHREAD{$$} ) && defined($SHARED) && $$ == $SHARED) {
        my $running_and_unjoined = 0;
        my $finished_and_unjoined = 0;
        my $running_and_detached = 0;
        foreach my $tid (grep(!/^0$/, keys %TID2PID)) {
            if (!exists $DETACHED{$tid} && !exists $DETACHED_DONE{$tid}) {
                $running_and_unjoined++
                    if !exists $RESULT{$tid} && !exists $JOINED{$tid};
                $finished_and_unjoined++ if exists $RESULT{$tid};
            }
        }
        foreach my $tid (grep(!/^0$/, keys %DETACHED)) {
            $running_and_detached++
                if exists $DETACHED{$tid} && !exists $DETACHED_DONE{$tid};
        }

        print STDERR "Perl exited with active threads:\n"
            ."\t$running_and_unjoined running and unjoined\n"
            ."\t$finished_and_unjoined finished and unjoined\n"
            ."\t$running_and_detached running and detached\n"
            if ($running_and_unjoined 
                || $finished_and_unjoined || $running_and_detached);

        my @pidtokill;
        while (my ($tid, $client) = each %TID2CLIENT) {
            eval {
                my $written = send( $client,'',0 );
                if (defined( $written )) {
                    push @pidtokill, $TID2PID{$tid}
                        if $tid && defined $TID2PID{$tid}
                            && CORE::kill(0, $TID2PID{$tid});
                };
            };
        }
        CORE::kill('SIGKILL', $_) foreach @pidtokill;

        $QUERY->shutdown(2) if defined $QUERY;
        unlink($PORT) if $THREADS_UNIX && -S $PORT;
        
        _end_server_post_shutdown();

        return;
    }
    if (exists( $ISATHREAD{$$} ) && $$ == $PID) {
        $SHUTTING_DOWN = 1;
        _command( '_shutdown',$TID );
        $SHUTDOWN = 1;
    }
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
# Initialize the var to hold current time (for time calculations each loop)

    my $select = IO::Select->new( $QUERY );
    my %client;
    my %toread;
    my %read;
    my $curtime;

# Initialize the number of polls
# While we're running in the main dispatch loop (or data pending for main thread)
#  Update timedwaiting index
#  Get current time
#  Load next event timedwaiting expiration time (if any)
#  Wait until there is something to do or a cond_timedwaiting event has expired
#  Get current time
#  Increment number of polls
#  Handle any timedwaiting events that may have expired

    my $polls = 0;
    _server_post_startup();
    while ($RUNNING || exists $WRITE{$TID2CLIENT{0}}) {
if (DEBUG) {
 my $clients = keys %WRITE;
 _log( " ! $clients>>" ) if $clients;
}
        my $write = (each %WRITE) || '';
        _update_timedwaiting_idx();
        $curtime = time();
        my ($sleep_min) = $write ? (.001) : List::MoreUtils::minmax(
            @TIMEDWAITING_IDX ? $TIMEDWAITING_IDX[0]->[2] - $curtime : $MAX_POLL_SLEEP,
            $DEADLOCK_DETECT_TS_NEXT ? $DEADLOCK_DETECT_TS_NEXT - $curtime : $MAX_POLL_SLEEP,
            $BLOCKING_JOIN_CHECK_TS_NEXT ? $BLOCKING_JOIN_CHECK_TS_NEXT - $curtime : $MAX_POLL_SLEEP        
        );
_log( " ! max sleep time = $sleep_min" ) if DEBUG;
        my @reading = $select->can_read( $sleep_min > 0 ? $sleep_min : 0.001 );
        $curtime = time();
_log( " ! <<".@reading ) if DEBUG and @reading;
        $polls++;
        _handle_timedwaiting();
        
#  For all of the clients that have stuff to read
#   If we're done with this client, ignore further input until socket closed
#   If this is a new client
#    Accept the connection
#    If using INET sockets
#     Check if client is in the allow list
#      Immediately close client socket if not in allow list
#      And reloop
#    Make sure the client is non-blocking

        foreach my $client (@reading) {
            next if exists( $DONEWITH{$client} );
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
                $WRITE{$client} = _pack_response( ['_set_tid',$NEXTTID++] );
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
                $size -= MSG_LENGTH_LEN;
            }

#   Initialize scalar to receive data in
#   If something went wrong with reading
#    Die (we can't have this going about now can we)
#     unless call would block or was interrupted by signal
#   Add the data to the request read for this client if anything was read

            my $data;
            unless (defined( recv($client,$data,$size,0) ) and length($data)) {
                _croak( "Error ".($! ? $! + 0 : '')." reading from $CLIENT2TID{$client}: ".($! ? $! : '') )
                    unless ($! == EWOULDBLOCK || $! == EAGAIN || $! == EINTR);
            }
_log( " <$CLIENT2TID{$client} ".length($data)." of $toread{$client}" ) if DEBUG;
            $read{$client} .= $data if defined($data);
        }

#  For all of the clients for which we have read stuff
#   If we have read something already
#    If we have all we're expecting

        while (my $client = each %read) {
            if (my $read = length( $read{$client} )) {
                if ($read == $toread{$client}) {
_log( " =$CLIENT2TID{$client} ".CORE::join(' ',(_unpack_request( $read{$client} ) || '')) ) if DEBUG;

#     Create untainted version of what we got
#     Go handle that
#     Remove the number of characters to read
#    Elseif we got too much
#     Die now

                    $read{$client} =~ m#^(.*)$#s;
                    _handle_request( $client,$1 );
                    delete( $toread{$client} );
                    delete( $read{$client} );
                } elsif ($read > $toread{$client}) {
                    _croak( "Got $read bytes, expected only $toread{$client} from $CLIENT2TID{$client}: ".CORE::join( ' ',_unpack_request( $read{$client} ) ) );
                }
            }
        }

#  While there is a client to which we can write
#   Verify that there still is data to be written (may have changed after read)
#   Try to write whatever there was to write
#   If write was successful
#    If number of bytes written exactly same as what was supposed to be written
#     Just remove everything that was supposed to be removed
#    Elsif we've written some but not all because of blocking
#     Remove what was written, still left for next time
#    Else (something seriously wrong)
#     Die now
#   Else (something seriously wrong)
#    Die now
#   Fetch the next client to write to

        while ($write) {
            unless (defined $WRITE{$write}) {
                delete( $WRITE{$write} );
                $write = each %WRITE;
                next;
            }
            my $written =
             send( $TID2CLIENT{$CLIENT2TID{$write}},$WRITE{$write},0 );
_log( " >$CLIENT2TID{$write} $written of ".length($WRITE{$write}) ) if DEBUG;
            if (defined( $written )) {
                if ($written == length( $WRITE{$write} )) {
                    delete( $WRITE{$write} );
                } else {
                    substr( $WRITE{$write},0,$written ) = '';
                }
            } elsif ($! == EWOULDBLOCK || $! == EAGAIN || $! == EINTR) {
                #defer writing this time around
            } else {
                _croak( "Error ".($! ? $! + 0 : '').": Could not write ".(length $WRITE{$write})
                    ." bytes to $CLIENT2TID{$write}: ".($! ? $! : '') );
            }
            $write = each %WRITE;
        }
my $error = [$select->has_exception( .1 )] if DEBUG;
if (DEBUG) { _log( " #$CLIENT2TID{$_} error" ) foreach @$error; }

#  If asynchronous deadlock detection enabled and next event time has expired

        if ($DEADLOCK_DETECT && $DEADLOCK_DETECT_PERIOD && $curtime >= $DEADLOCK_DETECT_TS_NEXT) {
            _detect_deadlock_all();
            $DEADLOCK_DETECT_TS_NEXT = $curtime + $DEADLOCK_DETECT_PERIOD;
        }

#  If deadlock resolution is enabled and there are deadlocked threads
#   Get only one thread from each pair of deadlocked threads
#   Schedule signal for each pid to terminate to resolve deadlock
#   Clear deadlocked thread list

        if ($DEADLOCK_RESOLVE && %DEADLOCKED) {
            my @tid_to_kill;
            while (my $tid = each %DEADLOCKED) {
                push @tid_to_kill, $tid
                    if defined $DEADLOCKED{$DEADLOCKED{$tid}}
                        && $tid == $DEADLOCKED{$DEADLOCKED{$tid}};
                delete $DEADLOCKED{$tid};
            }
            foreach my $tid (@tid_to_kill) {
                print STDERR "Deadlock resolution: Terminating thread"
                    ." $tid (PID $TID2PID{$tid}) with signal $DEADLOCK_RESOLVE_SIG\n"
                    if warnings::enabled();
                $TOSIGNAL{$tid} = $DEADLOCK_RESOLVE_SIG;
            }
            %DEADLOCKED = ();
        }

#  For all of the clients that we need to send signals to
#   Make sure we won't check this client again
#   Skip this client if it's already terminated
#   Send requested signal to appropriate thread
#   If signal was SIGKILL, manually handle clean up
#   (note: this assumes any other signal would result in process safe exit)

        while (my ($tid, $signal) = each %TOSIGNAL) {
            delete( $TOSIGNAL{$tid} );
            next unless defined $TID2CLIENT{$tid};
            _signal_thread($tid, $signal);
_log( "sent $TID2PID{$tid} signal ".abs($signal) ) if DEBUG;
            _cleanup_unsafe_thread_exit($tid)
                if $signal eq 'KILL' || $signal eq 'SIGKILL'
                    || ($signal =~ m/^\d+$/ && $signal == SIGKILL);
        }

# If next check time has expired
# For all of the clients that are currently blocking on threads
#  Check that process is still alive; otherwise, cleanup dead thread
#   If that did not clear the waiting thread
#    Output a warning (from server)
#    Notify the thread with undef (should really be an error)

        if ($curtime >= $BLOCKING_JOIN_CHECK_TS_NEXT) {
            while (my $tid = each %BLOCKING_JOIN) {
                unless (CORE::kill(0, $TID2PID{$tid})) {
                    _cleanup_unsafe_thread_exit($tid);
                    if (exists $BLOCKING_JOIN{$tid}) {
                        warn "BLOCKING_JOIN manually cleared for tid #$tid";
                        $WRITE{$TID2CLIENT{$tid}} = $undef;
                    }
                }
            }
            $BLOCKING_JOIN_CHECK_TS_NEXT = $curtime + $BLOCKING_JOIN_CHECK_PERIOD;
        }

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
        }
    }

# Allow external modules opportunity to clean up thread process group resources
# Exit now, we're in the shared process and we've been told to exit

_log( " ! global exit: did $polls polls" ) if DEBUG;
    _end_server_pre_shutdown();
    CORE::exit();
} #_server

#---------------------------------------------------------------------------
#  IN: 1 tid to cleanup
#      2 (optional) error text to report

sub _cleanup_unsafe_thread_exit {

# Get tid of thread to cleanup
# Get error text to display in stack trace

    my $tid = shift;
    my $errtxt = shift || '';
    Carp::cluck( "Performing cleanup for dead thread $tid: $errtxt" )
        if warnings::enabled() && $errtxt ne '';   #TODO: disable these conditions?

# If thread isn't already joined and shutdown
#  Mark this thread as shutdown
#  Delete any messages that might have been pending for this client

    if (defined $TID2CLIENT{$tid}) {
        _shutdown($TID2CLIENT{$tid}, $tid);
        delete( $WRITE{$TID2CLIENT{$tid}} );
    }
} #_cleanup_unsafe_thread_exit

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
     or _croak( "Error ".($! ? $! + 0 : '').": Can't get flags for socket: ".($! ? $! : '') );
    fcntl( $socket, F_SETFL, $flags | O_NONBLOCK )
     or _croak( "Error ".($! ? $! + 0 : '').": Can't make socket nonblocking: ".($! ? $! : '') );
} #_nonblock

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      3 flag whether to automatically detect deadlocks
#      2 detection period, in seconds
#      3 flag whether to resolve deadlock conflicts

sub _set_deadlock_option {

# Obtain client
# Set deadlock detection flag and period
# If period was changed
#  Set deadlock detection period (stored as a positive number)
#  Set next deadlock detection event,
# Set deadlock resolution flag
# If deadlock resolver is enabled, immediately do global deadlock detection
# Make sure the client knows the result

    my $client = shift;
    $DEADLOCK_DETECT = shift @_ ? 1 : $DEADLOCK_DETECT;
    my $period = shift @_;
    if (defined $period) {
        $DEADLOCK_DETECT_PERIOD = abs($period) + 0;
        $DEADLOCK_DETECT_TS_NEXT = time() + $DEADLOCK_DETECT_PERIOD;
    }
    $DEADLOCK_RESOLVE = shift @_ ? 1 : $DEADLOCK_RESOLVE;
    my $signal = shift @_;
    $DEADLOCK_RESOLVE_SIG = abs($signal) if $signal;
    _detect_deadlock_all() if $DEADLOCK_RESOLVE;
    $WRITE{$client} = $true;
} #_set_deadlock_option

#---------------------------------------------------------------------------
#  IN: 1 TID of thread waiting to lock
#      2 Ordinal of variable TID is waiting to lock
# OUT: 1 True or false, indicating whether or not deadlock was detected
#      2 TID of thread deadlocked with input TID
#      3 Ordinal of other variable involved in deadlock that is locked by output TID

sub _detect_deadlock {

# Obtain thread TID (1) and ordinal that it wants to lock
# Verify that that the ordinal is already locked (and not by the thread to analyze)
#  Get TID (2) of current ordinal locker
#  Get ordinal that TID (2) is currently blocking on
#  If TID (2) is blocking on TID (1) locked variable
#   Warn of the deadlock
#   Mark thread pair as deadlocked
#   Return true result
# Return false (no deadlock detected)

    my ($tid1, $tid1_locking_ordinal) = @_;
    if (defined $LOCKED[$tid1_locking_ordinal] && $LOCKED[$tid1_locking_ordinal] != $tid1) {
        my $tid2 = $LOCKED[$tid1_locking_ordinal];
        my $tid2_locking_ordinal = List::MoreUtils::firstidx(
            sub { ref($_) eq 'ARRAY' ? grep(/^$tid2$/, @{$_}) : 0 }, @LOCKING);
        if ($tid2_locking_ordinal != -1 && defined $LOCKED[$tid2_locking_ordinal]) {
            print STDERR "Deadlock detected:\n"
                .sprintf("% 7s% 12s% 13s   %s\n",'TID','SV LOCKED','SV LOCKING','Caller')
                .sprintf("% 7d% 12d% 13d   %s\n", $tid1, $tid2_locking_ordinal,
                    $tid1_locking_ordinal, CORE::join(' at line ', @{$TID2LOCKCALLER{$tid1}}[1..2]))
                .sprintf("% 7d% 12d% 13d   %s\n", $tid2, $tid1_locking_ordinal,
                    $tid2_locking_ordinal, CORE::join(' at line ', @{$TID2LOCKCALLER{$tid2}}[1..2]))
                if warnings::enabled();
            $DEADLOCKED{$tid1} = $tid2;
            $DEADLOCKED{$tid2} = $tid1;
            return CORE::wantarray ? (1, $tid2, $tid2_locking_ordinal) : 1;
        }
    }
    return 0;
} #_detect_deadlock

#---------------------------------------------------------------------------
# OUT: 1 Total number of deadlock (in terms of thread pairs) detected
#      2 Num of unique deadlock pairs detected

sub _detect_deadlock_all {

# Initialize counter
# For each ordinal in @LOCKING
#  If any threads are waiting to lock this ordinal
#   Increment deadlock counter foreach deadlock (unless thread is marked deadlocked)
# Return count of deadlocked pairs

    my $num_deadlocks = 0;
    for (my $ord = 0; $ord <= scalar @LOCKING; $ord++) {
        if (defined $LOCKING[$ord] && ref($LOCKING[$ord]) eq 'ARRAY') {
            foreach my $tid (@{$LOCKING[$ord]}) {
                $num_deadlocks += _detect_deadlock($tid, $ord)
                    unless exists $DEADLOCKED{$tid};
            }
         }
    }
    return $num_deadlocks;
} #_detect_deadlock_all

#---------------------------------------------------------------------------
#  IN: 1 TID of thread to signal
#      2 Signal to send (ID, name, or SIGname)

sub _signal_thread {

# Obtain the TID to signal
# Obtail the signal to send
# Determine the signal name or ID
# Send the signal

    my $tid = shift;
    my $signal = shift;
    my $mysig = uc($signal);

    $mysig = $1 if $mysig =~ m/^SIG(\w+)/;
    my $sigidx = List::MoreUtils::firstidx( sub { $_ eq $mysig },
        split(/\s+/, $Config::Config{sig_name}));
    my $signum = $sigidx == -1
        ? $signal : (split(/\s+/, $Config::Config{sig_name}))[$sigidx];
    
    CORE::kill($signal, $TID2PID{$tid});
} #_signal_thread

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

# Get return context of thread
# Get flag whether this thread should start detached or not
# Mark this process as a thread
# Reset thread local tid value (so the process doesn't have its parent's tid)
# Reset thread local pid value (so the process doesn't have its parent's pid)
# Store the return context of this thread

    my $thread_context = shift;
    my $is_detached = shift;
    $ISATHREAD{$$} = undef;
    undef( $TID );
    undef( $PID );
    $THREAD_CONTEXT = $thread_context;

# Attempt to create a connection to the server or die

    if ($THREADS_UNIX) {
        $QUERY = IO::Socket::UNIX->new(
         Peer => $PORT,
        ) or _croak( "Couldn't connect to query server: $@" );
    } else {
        $QUERY = IO::Socket::INET->new(
         PeerAddr => '127.0.0.1',
         PeerPort => $PORT,
        ) or _croak( "Couldn't connect to query server: $@" );
    }

# Obtain the initial message from the query server
# Die now if it is the wrong type of message
# Set the tid
# Set the pid
# Send the command to register the pid (unless starting detached or is main thread)
# Execute all of the CLONE subroutines if not in the base thread

    my @param = _receive( $QUERY );
    _croak( "Received '$param[0]' unexpectedly" ) if $param[0] ne '_set_tid';
    $TID = $param[1];
    $PID = $$;
    _send( $QUERY,'_register_pid',$TID,$$,$is_detached || !$TID ? undef : getppid(),$thread_context,$is_detached );
    _run_CLONE() if $TID;
    
# Wait for result of registration, die if failed

    _croak( "Could not register pid $$ as tid $TID" ) unless _receive( $QUERY );
} #_init_thread

#---------------------------------------------------------------------------

# internal subroutines, both server-side as well as client-side

#---------------------------------------------------------------------------
#  IN: 1 arrayref of parameters to be put in message
#  IN: 2 command filter type (request or response)
#  IN: 3 command name
# OUT: 1 formatted message (MSG_LENGTH_LEN bytes packed length + CMD_TYPE_INTERNAL + data)

sub _pack {
    my $data_aref = shift;
    my $cmd_fltr_type = shift;
    my $cmd_name = shift;
    my $cmd_num = $cmd_type_to_num{$cmd_name} if $cmd_name;
    my $is_default_pack_type = defined $cmd_fltr_type && defined $cmd_num ? 0 : 1;
    
# If using default pack type
#  Freeze the parameters that have been passed
# Else
#  Pack data using custom filter
    
    my $data;
    if ($is_default_pack_type) {
        $data = pack('C', CMD_TYPE_DEFAULT).freeze( $data_aref );
    } else {
        my $filter = $cmd_num_to_filter[$cmd_num]->[$cmd_fltr_type]->[CMD_FLTR_ENCODE];
        $data = pack('C', CMD_TYPE_INTERNAL).pack('S', $cmd_num).$filter->($data_aref);
    }

# Calculate the length, pack it and return it with the frozen stuff

    pack( 'N',length( $data ) ).$data;
} #_pack_internal

#---------------------------------------------------------------------------
#  IN: 1 arrayref of parameters to be put in message
#  IN: 2 command name
# OUT: 1 formatted message

sub _pack_request { _pack(shift, CMD_FLTR_REQ, @_); } #_pack_request

#---------------------------------------------------------------------------
#  IN: 1 arrayref of parameters to be put in message
#  IN: 2 command name
# OUT: 1 formatted message

sub _pack_response { _pack(shift, CMD_FLTR_RESP, @_); } #_pack_response

#---------------------------------------------------------------------------
#  IN: 1 formatted message (without MSG_LENGTH_LEN byte length info)
#  IN: 2 command filter type (request or response)
# OUT: 1..N [msg name (if known), whatever was passed to "_pack"]

sub _unpack {

# Handle either default or custom filtered messages

    my $msg = shift;
    my $cmd_fltr_type = shift;
    my $type = unpack('C', substr($msg, CMD_TYPE_IDX, CMD_TYPE_LEN));
    if ($type == CMD_TYPE_DEFAULT) {
        return (undef, @{thaw( substr($msg, CMT_TYPE_FROZEN_CONTENT_IDX) )});
    } elsif ($type == CMD_TYPE_INTERNAL) {
        my $cmd_num = unpack('S', substr($msg, CMD_TYPE_INTERNAL_SUBNAME_IDX, CMD_TYPE_INTERNAL_SUBNAME_LEN));
        my $filter = $cmd_num_to_filter[$cmd_num]->[$cmd_fltr_type]->[CMD_FLTR_DECODE];
        return ($cmd_num_to_type[$cmd_num], $filter->(substr($msg, CMD_TYPE_INTERNAL_CONTENT_IDX)));
    } else {
        _croak ( "Unknown command type: $type" );
    }
} #_unpack

#---------------------------------------------------------------------------
#  IN: 1 formatted message (without MSG_LENGTH_LEN byte length info)
# OUT: 1..N [msg name (if known), whatever was passed to "_pack"]

sub _unpack_request { _unpack(shift, CMD_FLTR_REQ); } #_unpack_request

#---------------------------------------------------------------------------
#  IN: 1 formatted message (without MSG_LENGTH_LEN byte length info)
# OUT: 1..N [msg name (if known), whatever was passed to "_pack"]

sub _unpack_response { _unpack(shift, CMD_FLTR_RESP); } #_unpack_response

#---------------------------------------------------------------------------
#  IN: 1 client object
#      2 flag: don't croak if there is no length yet
# OUT: 1 length of message to be received

sub _length {

# Obtain client
# Initialize length variable
# While true
#  If we successfully read
#   Add length read to total
#   If we read successfully
#    If we got enough bytes for a length
#     Return the actual length
#    Elsif we didn't get anything
#     Return 0 if we don't need to croak yet
#     Break out of loop (no data found, where data was expected)
#    Decrease how much left there is to read by how much we just read
#  Elsif action would block or was interrupted by a signal
#   Sleep for a short time (i.e. don't hog CPU)
#  Else
#   Break out of loop (as some other error occured)

    my $client = shift;
    my $total_length = 0;
    my $todo = MSG_LENGTH_LEN;
    while ($total_length < MSG_LENGTH_LEN) {
        my $result = recv( $client,my $length,$todo,0 );
        if (defined( $result )) {
            $total_length += length( $length );
            if ($total_length == MSG_LENGTH_LEN) {
                return unpack( 'N',$length );
            } elsif ($total_length == 0) {
                return 0 if shift;
                last;
            }
            $todo -= length( $length );
        } elsif ($! == EWOULDBLOCK || $! == EAGAIN || $! == EINTR) {
            sleep 0.001;
        } else {
            last;
        }
    }

# If was ECONNABORTED (server abort) or ECONNRESET (client abort)
#  If is a thread
#   Warn and exit immediately (server connection terminated, 
#    likely due to main thread shutdown)
#  Else (is shared server)
#   Warn about the error
#   Cleanup "dead" thread
#   Report no data (length 0)
# Unless we're shutting down and we're not running in debug mode
#  Die, there was an error

    my $tid = defined $TID ? 'server' : $CLIENT2TID{$client};
    my $errtxt = "Error ".($! ? $! + 0 : '').": Could not read length of message from $tid: ".($! ? $! : '');
    if (!$! || $! == ECONNABORTED || $! == ECONNRESET) {
        if (exists( $ISATHREAD{$$} )) {
            $SHUTTING_DOWN = 1;
_log( "Thread $TID terminated abnormally: $errtxt" ) if DEBUG;
#warn "***_length: Thread $TID terminated abnormally: $errtxt";  #TODO: for debugging only
            CORE::exit();
        } else {
            _cleanup_unsafe_thread_exit($CLIENT2TID{$client}, $errtxt);
            return 0;
        }
    }
    _croak( $errtxt ) unless ($SHUTTING_DOWN && !DEBUG);
} #_length

#---------------------------------------------------------------------------

sub _block_sigset {
    _croak( "Error ".($! ? $! + 0 : '').": Could not block SigSet" )
        unless (defined POSIX::sigprocmask(SIG_BLOCK, $DEFERRED_CHLD_SIGSET));
    return 1;
} #_block_sigset

#---------------------------------------------------------------------------

sub _unblock_sigset {
    _croak( "Error ".($! ? $! + 0 : '').": Could not unblock SigSet" )
        unless (defined POSIX::sigprocmask(SIG_UNBLOCK, $DEFERRED_CHLD_SIGSET));
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
    my $frozen = grep(/^$_[0]$/, @cmd_filtered) ? _pack_request( \@_, shift ) : _pack_request( \@_, $_[0] );
    my $length = length( $frozen );
_log( "> ".CORE::join(' ',map {$_ || ''} eval {_unpack_request( substr($frozen,MSG_LENGTH_LEN) )}) )
 if DEBUG;

# Block signals, if using custom CHLD signal handler
# Loop while there is data to send
#  Send the data, find out how many really got sent
#  If data was sent
#   Remove sent data from string buffer
#   Increment total bytes sent
#  Elsif action would block or was interrupted by a signal
#   Sleep for a short time (i.e. don't hog CPU)
#  Else (an error occured)
#   Unblock signals, if using custom CHLD signal handler
#   If was ECONNABORTED (server abort) or ECONNRESET (client abort)
#    Warn and exit immediately (server connection terminated, likely due to main thread shutdown)
#   Die now unless shuttind down and not in debug mode
#   Return immediately
# Unblock signals, if using custom CHLD signal handler

    $frozen =~ m#^(.*)$#s;
    my ($data, $total_sent) = ($1, 0);
    _block_sigset() if $CUSTOM_SIGCHLD;
    while ($total_sent < $length) {
        my $sent = send( $client,$data,0 );
        if (defined( $sent )) {
            substr($data, 0, $sent) = '';
            $total_sent += $sent;
        } elsif ($! == EWOULDBLOCK || $! == EAGAIN || $! == EINTR) {
            sleep 0.001;
        } else {
            _unblock_sigset() if $CUSTOM_SIGCHLD;
            my $errtxt = "Error ".($! ? $! + 0 : '')." when sending message to server: ".($! ? $! : '');
            if (!$! || $! == ECONNABORTED || $! == ECONNRESET) {
                warn "Thread $TID terminated abnormally: $errtxt"
                    if warnings::enabled() && $TID && !$SHUTTING_DOWN;
                $SHUTTING_DOWN = 1;
#warn "===Thread $TID terminated abnormally: $errtxt";   #TODO: for debugging only
                CORE::exit();
            }
            _croak( $errtxt ) unless ($SHUTTING_DOWN && !DEBUG);
            return;
        }
    }
    
    _unblock_sigset() if $CUSTOM_SIGCHLD;
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
    _block_sigset() if $CUSTOM_SIGCHLD;
    my $length = my $todo = _length( $client );
    my $frozen;

# While there is data to get
#  Get some data
#  If we got data
#   Add what we got this time
#   If we got it all
#    Untaint what we got
#    Obtain any parameters if possible
#    Remove method type from parameters
#    Return the result
#   Set up for next attempt to fetch
#  ElseIf call would block or was interrupted by signal
#   Sleep a bit (to not take all CPU time)
# Unblock signals, if using custom CHLD signal handler

    while ($todo > 0) {
        my $result = recv( $client,my $data,$todo,0 );
        if (defined $result) {
            $frozen .= $data;
            if (length( $frozen ) == $length) {
                $frozen =~ m#^(.*)$#s;
                my @result = _unpack_response( $1 );
                shift @result;
_log( "< @{[map {$_ || ''} @result]}" ) if DEBUG;
                return CORE::wantarray ? @result : $result[0];
            }
            $todo -= length( $data );
        } elsif ($! == EWOULDBLOCK || $! == EAGAIN || $! == EINTR) {
            sleep 0.001;
        } else {
            last;
        }
    }
    _unblock_sigset() if $CUSTOM_SIGCHLD;

# Unless we're shutting down and we're not running in debug mode
#  Die now (we didn't get the data)

    unless ($SHUTTING_DOWN && !DEBUG) {
        _croak( "Error ".($! ? $! + 0 : '').": Did not receive all bytes from $CLIENT2TID{$client}: ".($! ? $! : '') );
    }
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

sub _handle_request {

# Obtain the socket
# Get the command name and its parameters
# If this is CMD_TYPE_DEFAULT command, get sub from parameters
# Allow for variable references (sub name is not a ref)
# Execute the command, be sure to pass the socket

    my $client = shift;
    my ($sub,@param) = _unpack_request( shift );
    $sub = shift @param unless defined $sub;
    no strict 'refs';
    &{$sub}( $client,@param );
} #_handle_request

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
#    Store return context of thread
#    Push tid on ppid2tid queue, if thread has a parent (e.g. not main thread)
#    Mark the thread as detached if so requested
#    Set status to indicate success

    my ($client,$tid,$pid,$ppid,$thread_context,$detach) = @_;
    my $status = 0;
    if ($pid) {
        if (defined $TID2CLIENT{$tid}) {
            unless (exists $PID2TID{$pid}) {
                $TID2PID{$tid} = $pid;
                $PID2TID{$pid} = $tid;
                $TID2CONTEXT{$tid} = $thread_context;
                push @{$PPID2CTID_QUEUE{$ppid}}, $tid if $ppid;
                $DETACHED{$tid} = undef if $detach;
                $status = 1;
            }
        }

#   If thread has a parent and there is a thread waiting for this ppid/ctid pair
#    Let that thread know
#    And forget that it was waiting for it

        if (defined $ppid && exists $BLOCKING_PPID2CTID_QUEUE{$ppid}) {
            _ppid2ctid_shift( $BLOCKING_PPID2CTID_QUEUE{$ppid},$ppid );
            delete( $BLOCKING_PPID2CTID_QUEUE{$ppid} );
        }
    }

# Let the client know how it went

    $WRITE{$client} = _pack_response( [$status] );
} #_register_pid

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id to find associated process id of
# OUT: 1 associated process id

sub _tid2pid { $WRITE{$_[0]} = _pack_response( [$TID2PID{$_[1]}] ) } #_tid2pid

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 process id to find associated thread id of
# OUT: 1 associated thread id

sub _pid2tid { $WRITE{$_[0]} = _pack_response( [$PID2TID{$_[1]}] ) } #_pid2tid

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 process id of thread calling this method
# OUT: 1 associated thread id

sub _ppid2ctid_shift { $WRITE{$_[0]} = _pack_response( [shift @{$PPID2CTID_QUEUE{$_[1]}}] ); } #_ppid2ctid_shift

#---------------------------------------------------------------------------
#  IN: 1 client socket
#  IN: 2 (optional) boolean value indicating type of list desired
# OUT: 1..N tid/pid pairs of all threads

sub _list_tid_pid {

# Obtain the socket
# Initialize the parameters to be sent
# For all of the registered threads
#  Obtain the thread id
#  If user specified an argument to list()
#   If argument was a "true" value
#    Reloop if it is detached or joined or no longer running (non-detached)
#   Else
#    Reloop if it is detached or joined or still running (non-detached)
#  Else
#   Reloop if it is detached or joined
#  Add this tid and pid to the list
# Store the response

    my $client = shift;
    my @param;
    while (my($tid,$pid) = each %TID2PID) {
        if (@_) {
            if ($_[0]) {
                next if exists( $DETACHED{$tid} ) or exists( $JOINED{$tid} )
                    or exists( $RESULT{$tid} );
            } else {
                next if exists( $DETACHED{$tid} ) or exists( $JOINED{$tid} )
                    or !exists( $RESULT{$tid} );
            }
        } else {
            next if exists( $DETACHED{$tid} ) or exists( $JOINED{$tid} );
        }
        push( @param,$tid,$pid );
    }
    $WRITE{$client} = _pack_response( [@param] );
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
#  Elseif the thread was detached
#   Mark this detached thread as done
# Make sure the client knows the result

    my $client = shift;
    if (my $tid = $CLIENT2TID{$client}) {
        if (exists $BLOCKING_JOIN{$tid}) {
#warn "*** the result I got was ".scalar(@_).": ".CORE::join(',', @_);  #TODO: for debugging only
            _isjoined( $BLOCKING_JOIN{$tid},$tid,@_ );
        } elsif (!exists $DETACHED{$tid}) {
            $RESULT{$tid} = \@_;
        } elsif (exists $DETACHED{$tid}) {
            $DETACHED_DONE{$tid} = undef;
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
    $WRITE{$client} = _pack_response( [$detached] );
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

# If the thread is detached
#  Propagate error to thread
# ElseIf there is already a result for this thread
#  Mark the thread as joined and use the pre-saved result
# Elseif the results were fetched before
#  Propagate error to thread
# Elseif the thread terminated without join (i.e. terminated abnormally)
#  Return undef to thread
# Elseif thread process not running (i.e. thread death w/ no _shutdown)
#  Return undef to thread
# Elseif someone is already waiting to join this thread
#  Propagate error to thread
# Else
#  Start waiting for the result to arrive

    my ($client,$tid) = @_;
    if (exists $DETACHED{$tid}) {
        warn "Thread $CLIENT2TID{$client} attempted to join a detached thread: $tid";
        $WRITE{$client} = $undef; #TODO: must become error: die "Cannot join a detached thread"
    } elsif (exists $RESULT{$tid}) {
#warn "case 2: $CLIENT2TID{$client} joining $tid immediately";  #TODO: for debugging only
        _isjoined( $client,$tid,@{$RESULT{$tid}} );
    } elsif (exists( $JOINED{$tid} )) {
        warn "Thread $CLIENT2TID{$client} attempted to an already joined thread: $tid";
        $WRITE{$client} = $undef; #TODO: must become error: die "Thread already joined"
    } elsif (!exists $TID2CLIENT{$tid}) {
#warn "case 4: $CLIENT2TID{$client} cannot join $tid";  #TODO: for debugging only
        $WRITE{$client} = defined $TID2CONTEXT{$tid} ? $empty : $undef;
    } elsif (!CORE::kill(0, $TID2PID{$tid})) {
#warn "case 5: $CLIENT2TID{$client} cannot join $tid";  #TODO: for debugging only
        $WRITE{$client} = defined $TID2CONTEXT{$tid} ? $empty : $undef;
    } elsif (defined $BLOCKING_JOIN{$tid}) {
        warn "Thread $CLIENT2TID{$client} attempted to join a thread already pending join: $tid";
        $WRITE{$client} = $undef; # must become error: die "Thread already pending join"
    } else {
#warn "case 6: $CLIENT2TID{$client} blocking on $tid";  #TODO: for debugging only
        $BLOCKING_JOIN{$tid} = $client;
    }
} #_join

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id of thread to check state

sub _is_detached {

# Obtain client socket and TID
# Return boolean value to thread whether deatched or not

    my ($client,$tid) = @_;
    $WRITE{$client} = exists $DETACHED{$tid} ? $true : $false;
} #_is_detached

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id of thread to check state

sub _is_running {

# Obtain client socket and TID
# Return boolean value to thread whether running or not

    my ($client,$tid) = @_;
    $WRITE{$client} = (exists $DETACHED{$tid} && !exists $DETACHED_DONE{$tid})
        || (defined $TID2PID{$tid} && !exists $RESULT{$tid} && !exists $JOINED{$tid})
        ? $true : $false;
} #_is_running

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id of thread to check state

sub _is_joinable {

# Obtain client socket and TID
# Return boolean value to thread whether joinable or not

    my ($client,$tid) = @_;
    $WRITE{$client} = exists $RESULT{$tid} ? $true : $false;
} #_is_joinable

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id of thread to check state

sub _is_deadlocked {

# Obtain client socket and TID
# Obtain ordinal of shared that TID is currently trying to lock (if any)
# If TID is not trying to lock anything
#  Return false to client
# Else
#  Check if thread is deadlocked and write appropriate value to client
# Return boolean value to thread whether deadlocked or not

    my ($client,$tid) = @_;
    my $ordinal = List::MoreUtils::firstidx(
        sub { ref($_) eq 'ARRAY' ? grep(/^$tid$/, @{$_}) : 0 }, @LOCKING);
    if ($ordinal == -1) {
        $WRITE{$client} = $false;
    } else {
        $WRITE{$client} = _detect_deadlock($tid, $ordinal) ? $true : $false;
    }
} #_is_deadlocked

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id of thread to signal
#      3 (optional) signal to send

sub _kill {

# Obtain client socket, TID, and signal
# Mark the thread to be signaled with the specified signal
# Make sure the client continues

    my ($client,$tid,$signal) = @_;
    $TOSIGNAL{$tid} = $signal;
    $WRITE{$client} = $true;
} #_kill

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id of thread to check state

sub _wantarray {

# Obtain client socket and TID
# Return thread context (boolean or undef)

    my ($client,$tid) = @_;
    $WRITE{$client} = defined $TID2CONTEXT{$tid} ? $TID2CONTEXT{$tid}
        ? $true : $false : $undef;
} #_wantarray

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
            $WRITE{$client} = _pack_response( [$ordinal] );
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
    if (exists $DISPATCH{$sub} && ($code = $DISPATCH{$sub})) {
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
    $WRITE{$client} = $code ? _pack_response( \@result ) : $undef;
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
# Obtain the client caller filename and line

    my $client = shift;
    my $tid = $CLIENT2TID{$client};
    my $ordinal = shift;
    my $line = shift;
    my $filename = shift;

# If this shared variable is already locked, obtaining its tid on the fly
#  If it's the same thread id
#   Indicate a recursive lock for this variable
#   Let the client continue
#  Else
#   Add the thread to the list of ones that want to lock (and let it block)
#   Perform deadlock deadlock detection immediately, if appropriate

    if (defined $LOCKED[$ordinal]) {
        if ($tid == $LOCKED[$ordinal]) {
            $RECURSED[$ordinal]++;
            $WRITE{$client} = $undef;
        } else {
            push( @{$LOCKING[$ordinal]},$tid );
            $TID2LOCKCALLER{$tid} = [$ordinal, $filename, $line];
            _detect_deadlock($tid, $ordinal)
                if $DEADLOCK_DETECT && !$DEADLOCK_DETECT_PERIOD;
        }

# Else (this variable was not locked yet)
#  Lock this variable
#  Let the client continue

    } else {
        $LOCKED[$ordinal] = $tid;
        $TID2LOCKCALLER{$tid} = [$ordinal, $filename, $line];
        $WRITE{$client} = $undef;
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
# Obtain ordinal
# If the signal ordinal is the same as the lock ordinal or the variable they are waiting to relock is currently locked
#  Add the next thread id from the list of waiting or timed waiting threads (if any) to the head of the locking list
# Else (lock var is not same as signal var and lock var is currently unlocked)
#  Assign lock to this tid
#  Immediately notify blocking thread that it should continue
# Make sure the client continues

    my $client = shift;
    my $ordinal = shift;
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
    $WRITE{$client} = $undef;
} #_signal

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 ordinal number of variable to signal all

sub _broadcast {

# Obtain local copy of the client
# Obtain ordinal
# If there are threads waiting or timed waiting
#  For all waiting or timed waiting threads
#   If the signal ordinal is the same as the lock ordinal or the variable they are waiting to relock is currently locked
#    Add it to the head of the locking list
#   Else (lock var is not same as signal var and lock var is currently unlocked)
#    Assign lock to this tid
#    Immediately notify blocking thread that it should continue
# Make sure the client continues

    my $client = shift;
    my $ordinal = shift;
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
    
    $WRITE{$client} = $undef;
} #_broadcast

#---------------------------------------------------------------------------
#  IN: 1 client socket
#      2 thread id that was shutdown

sub _shutdown {

# Obtain the client socket
# Obtain the thread id
# If thread did not appear to exit cleanly
#  Simulate join of the thread with no result
# If it is not the main thread shutting down
#  Unlock all locked variables
#  Reset one of the following events (as thread can only be in one blocking state)
#   Try removing TID from @LOCKING
#   Try removing TID from @WAITING
#   Try removing TID from %TIMEDWAITING
#  Delete any messages that might have been pending for this client
# Else (it's the main thread shutting down)
#  Reset running flag
# Mark this client for deletion
# Send result to thread to allow it to shut down

    my $client = shift;
    my $tid = shift;
    if (!exists $RESULT{$tid} && !exists $JOINED{$tid}
        && !exists $DETACHED_DONE{$tid}) {
        _tojoin($client);
    }
    if ($tid) {
        while ((my $ordinal = List::MoreUtils::firstidx(
            sub { defined $_ ? $_ eq $tid : 0 }, @LOCKED)) >= 0) {
            $RECURSED[$ordinal] = 0;
            _unlock_ordinal($ordinal);
        }
        BLOCKING_EVENT: {
            if ((my $ordinal = List::MoreUtils::firstidx(
                sub { ref($_) eq 'ARRAY' ? grep(/^$tid$/, @{$_}) : 0 }, @LOCKING)) >= 0) {
                $LOCKING[$ordinal] = [grep(!/^$tid$/, @{$LOCKING[$ordinal]})];
                last BLOCKING_EVENT;
            }
            if ((my $ordinal = List::MoreUtils::firstidx(
                sub { ref($_) eq 'ARRAY' ? grep(/^$tid$/, @{$_}) : 0 }, @WAITING)) >= 0) {
                $WAITING[$ordinal] = [grep(!/^$tid$/, @{$WAITING[$ordinal]})];
                last BLOCKING_EVENT;
            }
            if ((my $ordinal = List::MoreUtils::firstidx(
                sub { $_->[0] == $tid }, @TIMEDWAITING_IDX)) >= 0) {
                if ((my $idx = List::MoreUtils::firstidx(sub { $_->[0] == $tid }, @{$TIMEDWAITING{$ordinal}})) >= 0) {
                    splice(@{$TIMEDWAITING{$ordinal}}, $idx, 1);
                    $TIMEDWAITING_IDX_EXPIRED = 1;
                    last BLOCKING_EVENT;
                }
            }
        }
        $DONEWITH{$client} = undef;
    } else {
        $RUNNING = 0;
    }
    $WRITE{$client} = $true;    #TODO: make sure socket is still alive, otherwise could cause server to croak on dead socket (need to protect server with correct error state--EPIPE?)
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
        _croak( "You need a lock before you can $_[0]: variable #$ordinal ($tid != $LOCKED[$ordinal])" );
    }
    CORE::wantarray ? ($ordinal,$tid) : $ordinal;
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

    $WRITE{$client} = _pack_response( \@_ );
#warn "case 7: tid $tid had this to say (".scalar(@_)."): ".CORE::join(',', @_);    #TODO: for debugging only
    delete( $BLOCKING_JOIN{$tid} );
    delete( $RESULT{$tid} );
    delete( $TID2PID{$tid} ) unless exists( $TID2CLIENT{$tid} );
    $JOINED{$tid} = undef;
} #_isjoined

#---------------------------------------------------------------------------

# debugging routines

#---------------------------------------------------------------------------
#  IN: 1 message to display

sub _croak { return &Carp::confess((defined $TID ? $TID : '')." ($$): ".shift) } #_croak

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

=head1 VERSION

This documentation describes version 0.23.

=head1 SYNOPSIS

  use forks;
  use warnings;

  my $thread = threads->new( sub {       # or ->create or async()
    print "Hello world from a thread\n";
  } );

  $thread->join;
  
  $thread = threads->new( { 'context' => 'list' }, sub {
    print "Thread is expected to return a list\n";
    return (1, 'abc', 5);
  }
  my @result = $thread->join();

  threads->detach;
  $thread->detach;

  my $tid    = $thread->tid;
  my $owntid = threads->tid;

  my $self    = threads->self;
  my $threadx = threads->object( $tidx );

  my @running = threads->list(threads::running);
  $_->join() foreach (threads->list(threads::joinable));
  $_->join foreach threads->list; #block until all threads done

  unless (fork) {
    threads->isthread; # could be used a child-init Apache handler
  }

  # Enable debugging
  use forks qw(debug);
  threads->debug( 1 );
  
  # Stringify thread objects
  use forks qw(stringify);
  
  # Check state of a thread
  my $thr = threads->new( ... );
  if ($thr->is_running()) {
    print "Thread $thr running\n"; #prints "Thread 1 running"
  }
  
  # Send a signal to a thread
  $thr->kill('SIGUSR1');

  # Manual deadlock detection
  if ($thr->is_deadlocked()) {
    print "Thread $thr is currently deadlocked!\n";
  }
  
  # Use forks as a drop-in replacement for an ithreads application
  perl -Mforks -Mforks::shared threadapplication
  
See L<threads/"SYNOPSYS"> for more examples.
  
=head1 DESCRIPTION

The "forks" pragma allows a developer to use threads without having to have
a threaded perl, or to even run 5.8.0 or higher.

Refer to the L<threads> module for ithreads API documentation.  Also, use

    perl -Mforks -e 'print $threads::VERSION'
    
to see what version of L<threads> you should refer to regarding supported API
features.

There were a number of goals that I am trying to reach with this implementation.

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
L</"isthread">).

=head2 same API as threads

You should be able to run threaded applications unchanged by simply making
sure that the "forks" and "forks::shared" modules are loaded, e.g. by
specifying them on the command line.  Forks is currently API compatible with
CPAN L<threads> version C<1.53>.

Additionally, you do not need to worry about upgrading to the latest Perl
maintenance release to insure that the (CPAN) release of threads you wish to
use is fully compatibly and stable.  Forks code is completely independent of
the perl core, and thus will guarantee reliable behavior on any release of
Perl 5.8 or later.  (Note that there may be behavior variances if running
under Perl 5.6.x, as that version does not support safe signals and requires
a source filter to load forks).

=head2 using as a development tool

Because you do not need a threaded Perl to use forks.pm, you can start
prototyping threaded applications with the Perl executable that you are used
to.  Just download and install the "forks" package from CPAN.  So
the threshold for trying out threads in Perl has become much lower.  Even
Perl 5.005 should, in principle, be able to support the forks.pm module;
however, some issues with regards to the availability of XS features between
different versions of Perl, it seems that 5.6.0 (unthreaded) is what you need
at least.

Additionally, forks offers a full thread deadlock detection engine, to help
discover and optionally resolve locking issues in threaded applications.  See
L<forks::shared/"Deadlock detection and resolution"> for more information.

=head2 using in production environments

This package has successfully been proven as stable and reliable in production 
environments.  I have personally used it in high-availability, database-driven, 
message processing server applications since 2004 with great success.

Also, unlike pure ithreads, forks.pm is fully compatible with all perl modules,
whether or not they have been updated to be ithread safe.  This means that you
do not need to feel limited in what you can develop as a threaded perl
application, a problem that continues to plague the acceptance of ithreads in
production enviroments today.  Just handle these modules as you would when
using a standard fork: be sure to create new instances of, or connections to,
resources where a single instance can not be shared between multiple processes.

The only major concern is the potentially slow (relative to pure ithreads)
performance of shared data and locks.  If your application doesn't depend on
extensive semaphore use, and reads/writes from shared variables moderately
(such as using them primarily to deliver data to a child thread to process
and the child thread uses a shared structure to return the result), then this
will likely not be an issue for your application.  See the TODO section
regarding plans to tackle this issue.

Also, you may wish to try L<forks::BerkeleyDB>, which has shown signifigant
performance gains and consistent throughoutput in high-concurrency shared
variable applications.

=head2 Perl built without native ithreads

If your Perl release was not built with ithreads or does not support ithreads,
you will have a compile-time option of installing forks into the threads and
threads::shared namespaces.  This is done as a convenience to give users a
reasonably seamless ithreads API experience without having to rebuild their
distribution with native threading (and its slight performance overhead on all
perl runtime, even if not using threads).

B<Note:> When using forks in this manner (e.g. "use threads;") for the first
time in your code, forks will attempt to behave identically to threads relative
to the current version of L<threads> it supports (refer to $threads::VERSION),
even if the behavior is (or was) considered a bug.  At this time, this means
that shared variables will lose their pre-existing value at the time they are
shared and that splice will die if attempted on a shared scalar.

If you use forks for the first time as "use forks" and other loaded code uses
"use threads", then this threads behavior emulation does not apply. 

=head1 REQUIRED MODULES

 Devel::Required (0.07)
 File::Spec (any)
 IO::Socket (1.18)
 List::MoreUtils (0.15)
 reaper (0.03)
 Scalar::Util (1.01)
 Storable (any)
 Time::HiRes (any)

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

=head2 Deadlock detection

Forks also offers a full thread deadlock detection engine, to help discover
and optionally resolve locking issues in threaded applications.  See
L<forks::shared/"Deadlock detection and resolution"> for more information.

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

=head1 NOTES

Some imporant items you should be aware of.

=head2 Signal behavior

Unlike ithreads, signals being sent are standard OS signals, so you should
program defensively if you plan to use inter-thread signals.

Also, be aware that certain signals may untrappable depending on the target
platform, such as SIGKILL and SIGSTOP.  Thus, it is recommended you only use
normal signals (such as TERM, INT, HUP, USR1, USR2) for inter-thread signal
handling.

=head2 Modifying signals

Since the threads API provides a method to send signals between threads
(processes), untrapped normal and error signals are defined by forks with
a basic CORE::exit() shutdown function to provide safe termination.

Thus, if you (or any modules you use) modify signal handlers, it is important
that the signal handlers at least remain defined and are not undefined (for
whatever reason).  The system signal handler default, usually abnormal
process termination which skips END blocks, may cause undesired behavior if
a thread exits due to an unhandled signal.

=head2 

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

=head1 CAVEATS

Some caveats that you need to be aware of.

=head2 Greater latency

Because of the use of sockets for inter-thread communication, there is an
inherent larger latency with the interaction between threads.  However, the
fact that TCP sockets are used, may open up the possibility to share threads
over more than one physical machine.

You may decrease some latency by using UNIX sockets (see L</"UNIX socket support">).

Also, you may wish to try L<forks::BerkeleyDB>, which has shown signifigant performance
gains and consistent throughoutput in applications requiring high-concurrency shared
variable access.


=head2 Module CLONE functions and threads

In rare cases, module CLONE functions may have issues when being auto-executed
by a new thread (forked process).  This only affects modules that use XS data
(objects or struts) created by to external C libraries.  If a module attempts
to CLONE non-fork safe XS data, at worst it may core dump only the newly
created thread (process).

If you treat such sensitive resources (such as L<DBI> driver instances) as 
non-thread-safe by default and close these resources prior to creating a new
thread, you should never encounter any issues.

=head2 Signals and safe-signal enabled Perl

In order to use signals, you must be using perl 5.8 compiled with safe signal
support.  Otherwise, you'll get a terminal error like "Cannot signal threads
without safe signals" if you try to use signal functions.

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

=item inter-thread signaling is experimental and potentially unstable

This feature is considered experimental and has rare synchronization issues
when sending a signal to a process in the middle of sending or receiving
socket data pertaining to a threads operation.  This will be addressed in a
future release.

=item test-suite exits in a weird way

Although there are no errors in the test-suite, the test harness sometimes
thinks there is something wrong because of an unexpected exit() value.  This
is an issue with Test::More's END block, which wasn't designed to co-exist
with a threads environment and forked processes.  Hopefully, that module will
be patched in the future, but for now, the warnings are harmless and may be
safely ignored.

And of course, there might be other, undiscovered issues.  Patches are welcome!

=back

=head1 CREDITS

Refer to the C<CREDITS> file included in the distribution.

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
