package forks::shared;    # make sure CPAN picks up on forks::shared.pm
$VERSION = '0.26';

use Config ();

#---------------------------------------------------------------------------
#  IN: 1 class
#      2..N Hash of parameters to set

sub set_deadlock_option {

# Get the class
# Get the options
# Initialize variables for final option values
# Set value for 'detect' option
# Set value for 'period' option
# Set value for 'resolve' option
# Send settings to server

    my $class = shift;
    my %opts = @_;
    my ($detect, $period, $resolve, $signal);
    $detect = $opts{detect} ? 1 : 0;
    $period = $opts{period} + 0 if defined $opts{period};
    $resolve = $opts{resolve} ? 1 : 0;
    threads::shared::_command( '_set_deadlock_option',
        $detect,$period,$resolve,$signal );
}

package
    threads::shared;  # but we're masquerading as threads::shared.pm

# Make sure we have version info for this module
# Compatibility with the standard threads::shared
# Do everything by the book from now on

$VERSION  = '1.14';
$threads_shared = $threads_shared = 1;
use strict;
use warnings;

# Make sure we can die with lots of information
# Make sure we can find out about blessed references correctly
# Load some additional list utility functions

use Carp ();
use Scalar::Util qw(reftype blessed refaddr);
use List::MoreUtils;

# If forks.pm is loaded
#  Make sure we have a local copy of the base command handler on the client side
# Else
#  Have share do nothing, just return the ref
#  Disable the cond_xxxx family

if ($forks::threads || $forks::threads) { # twice to avoid warnings
    *_command = \&threads::_command;
    *is_shared = \&_id;
} else {
    *share = \&share_disabled;
    *is_shared = *lock = *cond_signal = *cond_broadcast = sub (\[$@%]) {undef};
    *cond_wait = sub (\[$@%];\[$@%]) {undef};
    *cond_timedwait = sub (\[$@%]$;\[$@%]) {undef};
}

# Avoid warnings

*share =
*lock =
*cond_wait =
*cond_timedwait =
*cond_signal =
*cond_broadcast =
 sub {} if 0;

# Clone detection logic
# Ordinal numbers of shared variables being locked by this thread

our $CLONE = 0;
our %LOCKED;

# Do this at compile time
#  Allow for dirty stuff in here
#  For the three types for which we support : shared
#   Create the name of the subroutine in question
#   Get the reference of the current version

BEGIN {
    no strict 'refs';
    foreach my $type (qw(SCALAR ARRAY HASH)) {
        my $name = "UNIVERSAL::MODIFY_${type}_ATTRIBUTES";
        my $old = \&$name;

#   Put our own handler in there
#    Obtain the parameters in a way we can work with
#    Share whatever reference was specified (if shared attribute found)

        *$name = sub {
            my ($package,$ref,@attribute) = @_;
            _share( $ref ) if grep m#^shared$#, @attribute;

#     If there are other attributes to handle still
#      Call the original routine with the remaining attributes
#     Return the remaining attributes

            if (@attribute = grep !m#^shared$#,@attribute) {
                @attribute = $old->( $package,$ref,@attribute );
            }
            @attribute;
        } #$name
    }
} #BEGIN

# Satisfy require

1;

#---------------------------------------------------------------------------

# standard Perl features

#---------------------------------------------------------------------------
#  IN: 1 class
#      2..N subroutines to export (default: async only)

sub import {

# Lose the class
    my $class = shift;

# If we forks is running in shadow mode
#  Fake that forks::shared.pm was really loaded (if not set already)
# Elsif there seems to be a threads.pm loaded
#  Fake that threads::shared.pm was really loaded (if not set already)
# Elsif there are (real) threads loaded
#  Die now indicating we can't mix them
# Else (using forks::shared without either forks.pm or threads.pm)
#  Die (we'll handle this maybe later)

    if (defined $INC{'threads.pm'} && $forks::threads_override) {
        $INC{'forks/shared.pm'} ||= $INC{'threads/shared.pm'}
    } elsif (defined $INC{'forks.pm'}) {
        $INC{'threads/shared.pm'} ||= $INC{'forks/shared.pm'};
    } elsif (defined $INC{'threads.pm'} && !$forks::threads_override) {
        _croak( "Can not mix 'use forks::shared' with real 'use threads'\n" );
    } else {
        _croak( "Must first 'use forks'\n" ); #for now
    }

# Enable deadlock options, if requested
    
    if ((my $idx = List::MoreUtils::firstidx(
        sub { $_ eq 'deadlock' }, @_)) >= 0) {
        if (ref $_[$idx+1] eq 'HASH') {
            my (undef, $opts) = splice(@_, $idx, 2);
            $class->set_deadlock_option(%{$opts});
        } else {
            splice(@_, $idx, 1);
        }
    }
    
# Perform the export needed 

    _export( scalar(caller()),@_ );
} #import

BEGIN {

# forks::shared and threads::shared share same import method
# load set_deadlock_option into threads::shared namespace

    *forks::shared::import = *forks::shared::import = \&import;
    *set_deadlock_option = *set_deadlock_option
        = \&forks::shared::set_deadlock_option;
}

#---------------------------------------------------------------------------

# Increment the current clone value (mark this as a cloned version)

sub CLONE { $CLONE++ } #CLONE

#---------------------------------------------------------------------------
#  IN: 1 class for which to bless
#      2 reference to hash containing parameters
#      3 initial value of scalar
# OUT: 1 instantiated object

sub TIESCALAR { shift->_tie( 'scalar',@_ ) } #TIESCALAR

#---------------------------------------------------------------------------
#  IN: 1 class for which to bless
#      2 reference to hash containing parameters
# OUT: 1 instantiated object

sub TIEARRAY { shift->_tie( 'array',@_ ) } #TIEARRAY

#---------------------------------------------------------------------------
#  IN: 1 class for which to bless
#      2 reference to hash containing parameters
# OUT: 1 instantiated object

sub TIEHASH { shift->_tie( 'hash',@_ ) } #TIEHASH

#---------------------------------------------------------------------------
#  IN: 1 class for which to bless
#      2 reference to hash containing parameters
#      3..N any parameters passed to open()
# OUT: 1 instantiated object

sub TIEHANDLE { shift->_tie( 'handle',@_ ) } #TIEHANDLE

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
#      2..N input parameters
# OUT: 1..N output parameters

sub AUTOLOAD {

# Obtain the object
# Obtain the subroutine name
# Handle the command with the appropriate data and obtain the result
# Return whatever seems appropriate

    my $self = shift;
    (my $sub = $threads::shared::AUTOLOAD) =~ s#^.*::#$self->{'module'}::#;
    my @result = _command( '_tied',$self->{'ordinal'},$sub,@_ );
    wantarray ? @result : $result[0];
} #AUTOLOAD

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
#      2..N input parameters
# OUT: 1..N output parameters

sub PUSH {

# Obtain the object
# Obtain the subroutine name
# Handle the command with the appropriate data and obtain the result (using
#  evaluated array slice to insure shared scalar push value works, as push
#  doesn't evaluate values before pushing them on the stack)
# Return whatever seems appropriate

    my $self = shift;
    my $sub = $self->{'module'}.'::PUSH';
    my @result = _command( '_tied',$self->{'ordinal'},$sub,map($_, @_) );
    wantarray ? @result : $result[0];
} #SPLICE

#---------------------------------------------------------------------------
#  IN: 1 instantiated object
#      2..N input parameters
# OUT: 1..N output parameters

sub SPLICE {

# Die now if running in thread override mode
# Obtain the object
# Obtain the subroutine name
# Handle the command with the appropriate data and obtain the result
# Return whatever seems appropriate

    Carp::croak('Splice not implemented for shared arrays')
        if $forks::threads_override;
    my $self = shift;
    my $sub = $self->{'module'}.'::SPLICE';
    my @result = _command( '_tied',$self->{'ordinal'},$sub,@_ );
    wantarray ? @result : $result[0];
} #SPLICE

#---------------------------------------------------------------------------
#  IN: 1 instantiated object

sub UNTIE {

# Obtain the object
# Return if we're not in the originating thread
# Handle the command with the appropriate data

    my $self = shift;
    return if $self->{'CLONE'} != $CLONE;
    _command( '_untie',$self->{'ordinal'} );
} #UNTIE

#---------------------------------------------------------------------------
#  IN: 1 instantiated object

sub DESTROY {   #currently disabled, as DESTROY method is not used by threads

# Obtain the object
# Return if we're not in the originating thread
# Handle the command with the appropriate data

#    my $self = shift;
#    return if $self->{'CLONE'} != $CLONE;
#    _command( '_tied',$self->{'ordinal'},$self->{'module'}.'::DESTROY' );
} #DESTROY

#---------------------------------------------------------------------------

# internal subroutines

#---------------------------------------------------------------------------
#  IN: 1 namespace to export to
#      2..N subroutines to export

sub _export {

# Obtain the namespace
# Set the defaults if nothing specified
# Allow for evil stuff
# Export whatever needs to be exported

    my $namespace = shift().'::';
    my @export = qw(share is_shared lock cond_wait cond_timedwait cond_signal cond_broadcast);
    push @export, 'bless' if $threads::threads && $threads::threads;
    @export = @_ if @_;
    no strict 'refs';
    *{$namespace.$_} = \&$_ foreach @export;
} #_export

#---------------------------------------------------------------------------
#  IN: 1 base class with which to bless
#      2 string to be concatenated to class for tie-ing
#      3 reference to hash with parameters
#      4..N any other values to be passed to tieing routine
# OUT: 1 tied, blessed object

sub _tie {

# Obtain the class with which to bless with inside the "thread"
# Obtain the type of variable to be blessed
# Obtain hash with parameters or create an empty one

    my $class = shift;
    my $type = shift;
    my $self = shift || {};

# Make sure we can do clone detection logic
# Set the type of variable to be blessed
# Obtain the module name to be blessed inside the shared "thread"
# Obtain the ordinal number for this tied variable (don't pass ref if running in threads override mode)
# Create the blessed object and return it

    $self->{'CLONE'} = $CLONE;
    $self->{'type'} = $type;
    $self->{'module'} ||= $class.'::'.$type;
    $self->{'ordinal'} = _command( '_tie',$self,$forks::threads_override ? () : @_ );
    CORE::bless $self,$class;
} #_tie

#---------------------------------------------------------------------------
#  IN: 1 reference to variable to be shared

sub _share {

# Obtain the reference
# Create the reference type of that reference

    my $it = shift;
    my $ref = reftype $it;

# Tie the variable, or return already existing tied variable

    if ($ref eq 'SCALAR') {
        my $tied = tied ${$it};
        return $tied if blessed($tied) && $tied->isa('threads::shared');
        tie ${$it},'threads::shared',{},\${$it};
    } elsif ($ref eq 'ARRAY') {
        my $tied = tied @{$it};
        return $tied if blessed($tied) && $tied->isa('threads::shared');
        tie @{$it},'threads::shared',{},\@{$it};
    } elsif ($ref eq 'HASH') {
        my $tied = tied %{$it};
        return $tied if blessed($tied) && $tied->isa('threads::shared');
        tie %{$it},'threads::shared',{},\%{$it};
    } elsif ($ref eq 'GLOB') {
        my $tied = tied *{$it};
        return $tied if blessed($tied) && $tied->isa('threads::shared');
        tie *{$it},'threads::shared',{},\*{$it};
    } else {
        _croak( "Don't know how to share '$it'" );
    }
} #_share

#---------------------------------------------------------------------------
#  IN: 1 reference to variable to be shared

sub _id (\[$@%]) {

# Obtain the reference to the variable
# Create the reference type of that reference
# Initialize the object

    my $it  = shift;
    my $ref = reftype $it;
    my $object;

# Obtain the object

    if ($ref eq 'SCALAR') {
        $object = tied ${$it};
    } elsif ($ref eq 'ARRAY') {
        $object = tied @{$it};
    } elsif ($ref eq 'HASH') {
        $object = tied %{$it};
    } elsif ($ref eq 'GLOB') {
        $object = tied *{$it};
    }

# If the reference is a threads::shared tied object
#  Return the refaddr of the variable
# Else
#  Return undef

    if (defined $object && $object->isa('threads::shared')) {
        return refaddr($it);
    } else {
        return undef;
    }
}

#---------------------------------------------------------------------------
#  IN: 1..N ordinal numbers of variables to unlock

sub _unlock {

# For each ordinal number
#  Decrement the lock counter
#  Delete ordinal number from the local list, if counter is zero (lock released)
# Notify the remote process also

    foreach (@_) {
        $LOCKED{$_}--;
        delete $LOCKED{$_} if $LOCKED{$_} <= 0;
    }
    _command( '_unlock',@_ );
} #unlock

#---------------------------------------------------------------------------
#  IN: 1 reference to the shared variable
# OUT: 1 ordinal number of variable
#      2 return value scalar of _command

sub _bless {

# Obtain the reference to the variable
# Create the reference type of that reference
# Initialize the object

    my $it  = shift;
    my $ref = reftype $it;
    my $object;

# Obtain the object

    if ($ref eq 'SCALAR') {
        $object = tied ${$it};
    } elsif ($ref eq 'ARRAY') {
        $object = tied @{$it};
    } elsif ($ref eq 'HASH') {
        $object = tied %{$it};
    } elsif ($ref eq 'GLOB') {
        $object = tied *{$it};
    }

# If the reference is a threads::shared tied object
#  Execute the indicated subroutine for this shared variable
#  Return the variable's ordinal number (and _command return scalar value if wantarray)

    if (defined $object && $object->isa('threads::shared')) {
        my $ordinal = $object->{'ordinal'};
        my $retval = _command( '_bless',$ordinal,@_ );
        return wantarray ? ($ordinal,$retval) : $ordinal;
    }
}

#---------------------------------------------------------------------------
#  IN: 1 remote subroutine to call
#      2 parameter of which a reference needs to be locked
# OUT: 1 ordinal number of variable
#      2 return value scalar of _command

sub _remote {

# Obtain the subroutine
# Obtain the reference to the variable
# Create the reference type of that reference
# Initialize the object

    my $sub = shift;
    my $it  = shift;
    my $ref = reftype $it;
    my $object;

# Obtain the object

    if ($ref eq 'SCALAR') {
        $object = tied ${$it};
    } elsif ($ref eq 'ARRAY') {
        $object = tied @{$it};
    } elsif ($ref eq 'HASH') {
        $object = tied %{$it};
    } elsif ($ref eq 'GLOB') {
        $object = tied *{$it};
    }

# If there is an ordinal number (if no object, there's no number either)
#  If we're about to lock
#   Mark the variable as locked in this thread
#   Store some caller() info (for deadlock detection report use)
#  Else if this is second case of _wait or _timedwait (unique signal and lock vars)
#   Obtain the reference to the lock variable (pop it off stack)
#   Create the reference type of that reference
#   Initialize the lock object
#   Obtain the lock object
#   If there is an ordinal number (if no object, there's no number either)
#    Die now if the variable does not appear to be locked
#    Push lock ordinal back on stack
#  Else (doing something on a locked variable)
#   Die now if the variable does not appear to be locked

    if (my $ordinal = $object->{'ordinal'}) {
        if ($sub eq '_lock') {
            $LOCKED{$ordinal}++;
            push @_, (caller())[2,1];
        } elsif (($sub eq '_wait' && scalar @_ > 0) || ($sub eq '_timedwait' && scalar @_ > 1)) {
            my $it2 = pop @_;
            my $ref2 = reftype $it2;
            my $object2;
            if ($ref2 eq 'SCALAR') {
                $object2 = tied ${$it2};
            } elsif ($ref2 eq 'ARRAY') {
                $object2 = tied @{$it2};
            } elsif ($ref2 eq 'HASH') {
                $object2 = tied %{$it2};
            } elsif ($ref2 eq 'GLOB') {
                $object2 = tied *{$it2};
            }
            if (my $ordinal2 = $object2->{'ordinal'}) {
                Carp::croak( "You need a lock before you can cond$sub" )
                 if not exists $LOCKED{$ordinal2};
                push @_, $ordinal2;
            }
        } else {
            if (not exists $LOCKED{$ordinal}) {
                if ($sub eq '_signal' || $sub eq '_broadcast') {
                    warnings::warnif('threads', "cond$sub() called on unlocked variable");
                } else {
                    Carp::croak( "You need a lock before you can cond$sub" );
                }
            }
        }

#  Execute the indicated subroutine for this shared variable
#  Return the variable's ordinal number (and _command return scalar value if wantarray)
        my $retval = _command( $sub,$ordinal,@_ );
        return wantarray ? ($ordinal,$retval) : $ordinal;
    }

# Adapt sub name to what we know outside
# No ordinal found, not shared!  Die!

    $sub = $sub eq '_lock' ? 'lock' : "cond$sub";
    Carp::croak( "$sub can only be used on shared values" );
} #_remote

#---------------------------------------------------------------------------

# debugging routines

#---------------------------------------------------------------------------
#  IN: 1 message to display

sub _croak { return &Carp::confess(shift) } #_croak

#---------------------------------------------------------------------------

__END__

=head1 NAME

forks::shared - drop-in replacement for Perl threads::shared with forks()

=head1 SYNOPSIS

  use forks;
  use forks::shared;

  my $variable : shared;
  my @array    : shared;
  my %hash     : shared;

  share( $variable );
  share( @array );
  share( %hash );

  lock( $variable );
  cond_wait( $variable );
  cond_wait( $variable, $lock_variable );
  cond_timedwait( $variable, abs time );
  cond_timedwait( $variable, abs time, $lock_variable );
  cond_signal( $variable );
  cond_broadcast( $variable );
  
  bless( $variable, class name );
  
  # Enable deadlock detection and resolution
  use forks::shared deadlock => {
    detect => 1,
    resolve => 1
  );
  # or
  threads::shared->set_deadlock_option(
    detect  => 1,
    resolve => 1
  );

=head1 DESCRIPTION

The C<forks::shared> pragma allows a developer to use shared variables with
threads (implemented with the "forks" pragma) without having to have a
threaded perl, or to even run 5.8.0 or higher.

C<forks::shared> is currently API compatible with CPAN L<threads::shared>
version C<1.05>.

=head1 EXPORT

C<share>, C<cond_wait>, C<cond_timedwait>, C<cond_signal>, C<cond_broadcast>,
C<is_shared>, C<bless>

See L<threads::shared/"EXPORT"> for more information.

=head1 OBJECTS

L<forks::shared> exports a versio of L<bless()|perlfunc/"bless REF"> that
works on shared objects, such that blessings propagate across threads.  See
L<threads::shared> for usage information and the L<forks> test suite for
additional examples.

=head1 EXTRA FEATURES

=head2 Deadlock detection and resolution

In the interest of helping programmers debug one of the most common bugs in
threaded application software, forks::shared supports a full deadlock
detection and resolution engine.

=head3 Automated detection and resolution

There are two ways to enable these features: either at import time in a use
statement, such as:

    use forks::shared deadlock => { OPTIONS }
    
or during runtime as a class method call to C<set_deadlock_option>, like:

    forks::shared->set_deadlock_option( OPTIONS );
    #or
    threads::shared->set_deadlock_option( OPTIONS );
    
where C<OPTIONS> may be a combination of any of the following:

    detect         => 1 (enable) or 0 (disable)
    period         => number of seconds between asynchronous polls
    resolve        => 1 (enable) or 0 (disable)
    
The C<detect> option enables deadlock detection.  By itself, this option
enabled synchronous deadlock detection, which efficiently checks for
potential deadlocks at lock() time.  If any are detected and warnings are
enabled, it will print out details to C<STDERR> like the following example:

    Deadlock detected:
        TID   SV LOCKED   SV LOCKING   Caller
          1           3            4   t/forks06.t at line 41
          2           4            3   t/forks06.t at line 46

The C<period> option, if set to a value greater than zero, is the number of
seconds between asynchronous deadlock detection checks.  Asynchronous
detection is useful for debugging rare, time-critical race conditions leading
to deadlocks that may be masked by the slight time overhead introduced by
synchronous detection on each lock() call.  Overall, it is less CPU intensive
than synchronous deadlock detection.

The C<resolve> option enables auto-termination of one thread in each deadlocked
thread pair that has been detected.  As with the C<detect> option, C<resolve>
prints out the action it performs to STDERR, if warnings are enabled.
B<NOTE>: C<resolve> uses SIGKILL to break deadlocks, so this feature should not
be used in environments where stability of the rest of your application may be
adversely affected by process death in this manner.

For example:

    use forks;
    use forks::shared
        deadlock => {detect=> 1, resolve => 1};

=head3 Manual detection

If you wish to check for deadlocks without enabling automated deadlock
detection, forks provides an additonal thread object method,

    $thr->is_deadlocked()

that reports whether the thread in question is currently
deadlocked.  This method may be used in conjunction with the C<resolve>
deadlock option to auto-terminate offending threads.

=head2 Splice on shared array

As of at least L<threads::shared> 1.05, the splice function has not been
implememted for arrays; however, L<forks::shared> fully supports splice on
shared arrays.

=head2 share() doesn't lose value for arrays and hashes

In the standard Perl threads implementation, arrays and hashes are
re-initialized when they become shared (with the share()) function.  The
share() function of forks::shared does B<not> initialize arrays and hashes
when they become shared with the share() function.

This B<could> be considered a bug in the standard Perl implementation.  In any
case this is an inconsistency of the behaviour of threads.pm and forks.pm.
Maybe a special "totheletter" option should be added to forks.pm to make
forks.pm follow this behaviour of threads.pm to the letter.

NOTE: If you do not have a natively threaded perl and you have installed and
are using forks in "threads.pm" override mode (where "use threads" loads
forks.pm), then this module will explicitly emulate the behavior of standard
threads::shared and lose value for arrays and hashes with share().
Additionally, array splice function will become a no-op with a warning.

=head1 CAVIATS

These problems are known and will be fixed in the future:

=over 2

=item test-suite exits in a weird way

Although there are no errors in the test-suite, the test harness sometimes
thinks there is something wrong because of an unexpected exit() value.  This
is an issue with Test::More's END block, which wasn't designed to co-exist
with a threads environment and forked processes.  Hopefully, that module will
be patched in the future, but for now, the warnings are harmless and may be
safely ignored.

=back

=head1 CREDITS

=over 2

=item threads::shared

For some of the XS code used for forks::shared exported bless function.

=back

=head1 CURRENT AUTHOR AND MAINTAINER

Eric Rybski <rybskej@yahoo.com>.  Please send all module inquries to me.

=head1 ORIGINAL AUTHOR

Elizabeth Mattijsen, <liz@dijkmat.nl>.

=head1 COPYRIGHT

Copyright (c)
 2005-2007 Eric Rybski <rybskej@yahoo.com>,
 2002-2004 Elizabeth Mattijsen <liz@dijkmat.nl>.
All rights reserved.  This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

L<threads::shared>, L<forks>, L<forks::BerkeleyDB::shared>.

=cut
