package threads::shared;  # yes, we're masquerading as threads::shared.pm

# Make sure we have version info for this module
# Compatibility with the standard threads::shared
# Do everything by the book from now on

$VERSION  = '0.01';
$threads_shared = $threads_shared = 1;
use strict;

# Load only the stuff that we really need
# Make sure we can die with lots of information

use load;
use Carp ();

# If forks.pm is loaded
#  Make sure we have a local copy of the base command handler on the client side
# Else
#  Have share do nothing, just return the ref
#  Disable the cond_xxxx family

if ($forks::threads || $forks::threads) { # twice to avoid warnings
    *_command = \&threads::_command;
} else {
    *share = \&share_disabled;
    *lock = *cond_wait = *cond_signal = *cond_broadcast = sub (\[$@%]) {undef};
}

# Avoid warnings

*share =
*lock =
*cond_wait =
*cond_signal =
*cond_broadcast =
 sub {} if 0;

# Clone detection logic

our $CLONE = 0;

# Satisfy require

1;

#---------------------------------------------------------------------------

# standard Perl features

#---------------------------------------------------------------------------
#  IN: 1 class (ignored)
#      2..N subroutines to export (default: async only)

sub import {

# Lose the class
# Do whatever threads::shared::import is supposed to do

    shift;
    _export( scalar(caller()),@_ );
} #import

#---------------------------------------------------------------------------
#  IN: 1 class (ignored)
#      2..N subroutines to export (default: async only)

sub forks::shared::import {

# Lose the class
# If there seems to be a threads.pm loaded
#  Fake that threads::shared.pm was really loaded (if not set already)
#  Perform the export needed 

    shift;
    if ($INC{'forks.pm'}) {
        $INC{'threads/shared.pm'} ||= $INC{'forks::shared.pm'};
        _export( scalar(caller()),@_ );

# Elsif there are (real) threads loaded
#  Die now indicating we can't mix them
# Else (using forks::shared without either forks.pm or threads.pm)
#  Die (we'll handle this maybe later)

    } elsif ($INC{'threads.pm'}) {
        _croak( "Can not mix 'use forks::shared' with real 'use threads'\n" );
    } else {
        _croak( "Must first 'use forks'\n" ); #for now
    }
} #forks::shared::import

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

sub DESTROY {

# Obtain the object
# Return if we're not in the originating thread
# Handle the command with the appropriate data

    my $self = shift;
    return if $self->{'CLONE'} != $CLONE;
    _command( '_tied',$self->{'ordinal'},$self->{'module'}.'::DESTROY' );
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
    @_ = qw(share lock cond_wait cond_signal cond_broadcast) unless @_;
    no strict 'refs';
    *{$namespace.$_} = \&$_ foreach @_;
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
# Obtain the ordinal number for this tied variable
# Create the blessed object and return it

    $self->{'CLONE'} = $CLONE;
    $self->{'type'} = $type;
    $self->{'module'} ||= $class.'::'.$type;
    $self->{'ordinal'} = _command( '_tie',$self,@_ );
    bless $self,$class;
} #_tie

#---------------------------------------------------------------------------
#  IN: 1 reference to variable to be shared

sub _share {

# Obtain the reference
# Create the reference type of that reference

    my $it = shift;
    my $ref = ref($it);

# Tie the variable

    if ($ref eq 'SCALAR') {
        tie ${$it},'threads::shared',{},${$it};
    } elsif ($ref eq 'ARRAY') {
        tie @{$it},'threads::shared',{},@{$it};
    } elsif ($ref eq 'HASH') {
        tie %{$it},'threads::shared',{},%{$it};
    } elsif ($ref eq 'GLOB') {
        tie *{$it},'threads::shared',{},*{$it};
    } else {
        _croak( "Don't know how to share '$it'" );
    }
} #_share

#---------------------------------------------------------------------------
#  IN: 1..N ordinal numbers of variables to unlock

sub _unlock { _command( '_unlock',@_ ) } #unlock

#---------------------------------------------------------------------------
#  IN: 1 remote subroutine to call
#      2 parameter of which a reference needs to be locked
# OUT: 1 ordinal number of variable

sub _remote {

# Obtain the subroutine
# Obtain the reference to the variable
# Create the reference type of that reference
# Initialize the object

    my $sub = shift;
    my $it  = shift;
    my $ref = ref($it);
    my $object;

# Obtain the object

    if ($ref eq 'SCALAR') {
        $object = tied( ${$it} );
    } elsif ($ref eq 'ARRAY') {
        $object = tied( @{$it} );
    } elsif ($ref eq 'HASH') {
        $object = tied( %{$it} );
    } elsif ($ref eq 'GLOB') {
        $object = tied( *{$it} );
    }

# Obtain the ordinal number
# Execute the indicated subroutine for this shared variable
# Return the variable's ordinal number

    my $ordinal = $object->{'ordinal'};
    _command( $sub,$ordinal );
    $ordinal;
} #_remote

#---------------------------------------------------------------------------

# debugging routines

#---------------------------------------------------------------------------
#  IN: 1 message to display

sub _croak { goto &Carp::confess } #_croak

#---------------------------------------------------------------------------

__END__

=head1 NAME

forks::shared - drop-in replacement for Perl threads::shared with forks()

=head1 SYNOPSIS

  use forks;
  use forks::shared;

  my $variable : shared; # attributes do not work yet

  share( $variable );
  share( @array );
  share( %hash );

  lock( $variable );
  cond_wait( $variable );
  cond_signal( $variable );
  cond_broadcast( $variable );

=head1 DESCRIPTION

The "forks::shared" pragma allows a developer to use shared variables with
threads (implemented with the "forks" pragma) without having to have a
threaded perl, or to even run 5.8.0 or higher.

=head1 KNOWN PROBLEMS

These problems are known and will be fixed in the future:

=over 2

=item :shared attribute

The :shared attribute on variables does not work yet.  Use the
L<threads::share> function instead.

=item test-suite exits in a weird way

Although there are no errors in the test-suite, the test harness thinks there
is something wrong because of an unexpected exit() value.  Not sure what to do
about this yet.

=item shared variable in push() on shared array bombs

For some reason, using a bare shared variable as a parameter in a push() on a
shared array, bombs.  This can be fixed by adding B<.''> to the shared
variable.

  push( @shared,$shared );    # bombs
  push( @shared,$shared.'' ); # works

This could be a generic problem with tie() in Perl, judging from some very
recent discussion on p5p.

=back

=head1 CREDITS

Arthur Bergman for Hook::Scope (from which I swiped the code to have locked
variables automatically unlock upon leaving the scope they were locked in) and
threads::shared (from which I swiped the code to create references from the
parameter list passed to a subroutine).

=head1 AUTHOR

Elizabeth Mattijsen, <liz@dijkmat.nl>.

Please report bugs to <perlbugs@dijkmat.nl>.

=head1 COPYRIGHT

Copyright (c) 2002 Elizabeth Mattijsen <liz@dijkmat.nl>. All rights
reserved.  This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 SEE ALSO

L<forks>, L<threads>.

=cut
