package
    forks::signals; #hide from PAUSE
$VERSION = '0.28';

use strict;
use warnings;
use Carp ();
use vars qw($sig %usersig);
use List::MoreUtils;
#use Sys::SigAction qw(set_sig_handler);

# Had to duplicate Sys::SigAction 0.10 just to fix safe handling...

#
#   Sys::SigAction
#   Copyright (c) 2004 Lincoln A. Baxter
#
#   You may distribute under the terms of either the GNU General Public
#   License or the Artistic License, as specified in the Perl README file,
#   with the exception that it cannot be placed on a CD-ROM or similar media
#   for commercial distribution without the prior approval of the author.
{
package
    forks::signals::Sys::SigAction; #hide from PAUSE
require 5.005;
use strict;
#use warnings;
use POSIX ':signal_h' ;
require Exporter;
use vars qw( $VERSION @ISA @EXPORT_OK %EXPORT_TAGS );

#use Data::Dumper;

@ISA = qw( Exporter );
@EXPORT_OK = qw( set_sig_handler timeout_call sig_name sig_number );
$VERSION = '0.10';

use Config;
my %signame = ();
my %signo = ();
{
   defined $Config{sig_name} or die "This OS does not support signals?";
   my $i = 0;     # Config prepends fake 0 signal called "ZERO".
   my @numbers = split( ' ' ,$Config{sig_num} );
   foreach my $name (split(' ', $Config{sig_name})) 
   {
      $signo{$name} = $numbers[$i];
      $signame{$signo{$name}} = $name;
      #print "name=$name num=" .$numbers[$i] ."\n" ;
      $i++;
   }
}

sub sig_name {
   my ($sig) = @_;
   return $sig if $sig !~ m/^\d+$/ ;
   return $signame{$sig} ;
}
sub sig_number {
   my ($sig) = @_;
   return $sig if $sig =~ m/^\d+$/;
   return $signo{$sig} ;
}
#if ( $] < 5008 ) {
#   #over write definitions of sig_name and sig_number
#   sub sig_name { warn "sig_name() not supported on perl versions < 5.8.0"; }
#   sub sig_number { warn "sig_number() not supported on perl versions < 5.8.0"; }
#}

my $use_sigaction = ( $] >= 5.008 and $Config{d_sigaction} );

sub _attrs_warning($)
{
   my ( $attrs ) =  @_ ;
   #my $act =  POSIX::SigAction->new( $handler ,$mask ,$attrs->{flags} ,$attrs->{safe} );
   #steve ( SPURKIS@cpan.org submitted  http://rt.cpan.org/Ticket/Display.html?id=19916 
   #  puts out the above liin is a mis-interpretation of the API for POSIX::SigAcation
   #  so here is the fix (per his suggestion)... lab:
   #
   #http://rt.cpan.org/Public/Bug/Display.html?id=21777
   #2006-09-29: in perl 5.8.0 (RH) $act->safe() is broken 
   #            safe is not available until 5.8.2
   #            DAMN... it was in my docs too... 
   if ( exists( $attrs->{safe} ) )
   {
      if ( ( $] < 5.008002 ) && defined($attrs->{safe}) && $attrs->{safe} ) 
      {
         warn "safe mode is not supported in perl versions less than 5.8.2";
         delete $attrs->{safe};
      }
   }

}
sub set_sig_handler( $$;$$ )
{
   my ( $sig ,$handler ,$attrs ) = @_;      
   $attrs = {} if not defined $attrs;
   _attrs_warning($attrs);
   if ( not $use_sigaction )
   {
      #warn '$flags not supported in perl versions < 5.8' if $] < 5.008 and defined $flags;
      $sig = sig_name( $sig );
      my $ohandler = $SIG{$sig};
      $SIG{$sig} = $handler;
      return if not defined wantarray;
      return forks::signals::Sys::SigAction->new( $sig ,$ohandler );
   }
   my $act = mk_sig_action( $handler ,$attrs );
   return set_sigaction( sig_number($sig) ,$act );
}
sub mk_sig_action($$)
{
   my ( $handler ,$attrs ) = @_;      
   die 'mk_sig_action requires perl 5.8.0 or later' if $] < 5.008;
   $attrs->{flags} = 0 if not defined $attrs->{flags};
   $attrs->{mask} = [] if not defined $attrs->{mask};
   #die '$sig is not defined' if not defined $sig;
   #$sig = sig_number( $sig );
   my @siglist = ();
   foreach (@{$attrs->{mask}}) { push( @siglist ,sig_number($_)); };
   my $mask = POSIX::SigSet->new( @siglist );

   my $act =  POSIX::SigAction->new( $handler ,$mask ,$attrs->{flags} ); 
   $act->safe($attrs->{safe}) if defined $attrs->{safe};
   return $act;
}


sub set_sigaction($$)
{ 
   my ( $sig ,$action  ) = @_;
   die 'set_sigaction() requires perl 5.8.0 or later' if $] < 5.008;
   die '$sig is not defined' if not defined $sig;
   die '$action is not a POSIX::SigAction' if not UNIVERSAL::isa( $action ,'POSIX::SigAction' );
   $sig = sig_number( $sig );
   if ( defined wantarray )
   {
      my $oact = POSIX::SigAction->new();
      sigaction( $sig ,$action ,$oact );
      return forks::signals::Sys::SigAction->new( $sig ,$oact );
   }
   else
   {
      sigaction( $sig ,$action );
   }
}

use constant TIMEDOUT => {};
sub timeout_call( $$;$ )
{
   my ( $timeout ,$code ) = @_;
   my $timed_out = 0;
   my $ex;
   eval {
      #lab-20060625 unecessary: my $h = sub { $timed_out = 1; die TIMEDOUT; };
      my $sa = set_sig_handler( SIGALRM ,sub { $timed_out = 1; die TIMEDOUT; } );
      alarm( $timeout );
      &$code; 
      alarm(0);
   };
   alarm(0);
   if ($@)
   {
      #print "$@\n" ;
      die $@ if not ref $@;
      die $@ if $@ != TIMEDOUT;
   }
   return $timed_out;
}
sub new {
   my ($class,$sig,$act) = @_;
   bless { SIG=>$sig ,ACT => $act } ,$class ;
}
sub DESTROY 
{
   if ( $use_sigaction )
   {
      set_sigaction( $_[0]->{'SIG'} ,$_[0]->{'ACT'} );
   }
   else
   {
      #set it to default if not defined (suppress undefined warning)
      $SIG{$_[0]->{'SIG'}} = defined $_[0]->{'ACT'} ? $_[0]->{'ACT'} : 'DEFAULT' ;
   }
   return;
}
}

package
    forks::signals; #hide from PAUSE

# Declare private package variables

my $tied;
my %sig_undefined_map;
my %sig_defined_map;
my %is_sig_user_defined;

sub import {
    shift;

# Overload and tie %SIG

    unless ($sig) {
        %usersig = %SIG;
        $sig = \%SIG;
        *SIG = {};
        $tied = tie %SIG, __PACKAGE__;
    }
    
# Load wrapper subroutines and prepare %SIG for signals that were already defined.

    if ((my $idx = List::MoreUtils::firstidx(
        sub { $_ eq 'ifdef' }, @_)) >= 0) {
        if (ref $_[$idx+1] eq 'HASH') {
            my (undef, $opts) = splice(@_, $idx, 2);
            %sig_defined_map = map { $_ => $opts->{$_} } 
                map(defined $opts->{$_} && $opts->{$_} ne ''
                    ? $_ : (), keys %{$opts});

            _STORE($_, $usersig{$_})
                 foreach map(defined $usersig{$_} && $usersig{$_} ne ''
                    ? $_ : (), keys %sig_defined_map);
        } else {
            splice(@_, $idx, 1);
            %sig_defined_map = ();
        }
    }
    
# Load wrapper subroutines and prepare %SIG for signals that were not already defined.

    if ((my $idx = List::MoreUtils::firstidx(
        sub { $_ eq 'ifndef' }, @_)) >= 0) {
        if (ref $_[$idx+1] eq 'HASH') {
            my (undef, $opts) = splice(@_, $idx, 2);
            %sig_undefined_map = map { $_ => $opts->{$_} } 
                map(defined $opts->{$_} && $opts->{$_} ne ''
                    ? $_ : (), keys %{$opts});

            _STORE($_, (defined $usersig{$_} ? $usersig{$_} : undef))
                 foreach map(!defined $usersig{$_} || $usersig{$_} eq ''
                    ? $_ : (), keys %sig_undefined_map);
        } else {
            splice(@_, $idx, 1);
            %sig_undefined_map = ();
        }
    }

    return $tied;
}

sub _STORE    {
    my $k = shift;
    my $s = shift;
    my $flags;
    
# Install or remove signal handler (including wrapper subroutine, when apporpriate)

    if (!defined($s) || $s eq '' || $s eq 'DEFAULT') {
        if (grep(/^$k$/, keys %sig_undefined_map)) {
            if (ref $sig_undefined_map{$k} eq 'ARRAY') {
                $sig->{$k} = $sig_undefined_map{$k}[0];
                $flags = $sig_undefined_map{$k}[1];
            } else {
                $sig->{$k} = $sig_undefined_map{$k};
            }
        } else {
            delete( $sig->{$k} );
        }
        delete( $is_sig_user_defined{$k} );
    } elsif ($s eq 'IGNORE') {
        $sig->{$k} = 'IGNORE';
        delete( $is_sig_user_defined{$k} );
    } else {
        $sig->{$k} = ref($s) eq 'CODE'
            ? grep(/^$k$/, keys %sig_defined_map)
                ? sub { $sig_defined_map{$k}->(@_); $s->(@_) }
                : $s
            : grep(/^$k$/, keys %sig_defined_map)
                ? sub { $sig_defined_map{$k}->(@_); $s; }
                : $s;
        $is_sig_user_defined{$k} = 1;
    }
    
# If subroutine signal handler has custom flags, apply them to the handler if possible.
# Example: CHLD handler may have SA_RESTART flag, to minimize side effects with programs
# that don't install a custom CHLD handler (very common) but use slow system signals;
# programs that do install a custom CHLD handler.
# Note: custom handler flags only currently applied to ifndef, as use with ifdef might
# unexpectedly overwrite user flags, if user is using POSIX::sigaction to set signals.

    if (defined $flags && ref($sig->{$k}) eq 'CODE') {
        untie %SIG;
        forks::signals::Sys::SigAction::set_sig_handler($k, $sig->{$k}, {
            flags => $flags,
            safe  => $] < 5.008002 ? 0 : 1
        });
        tie %SIG, __PACKAGE__;
    }
    
    return $s;
}

# Package method returns wheter a user-defined handler is set for a given signal.
# Input argument must be a signal name string, i.e. INT, TERM, CHLD, etc.

sub is_sig_user_defined {
    return exists $is_sig_user_defined{$_[0]} ? $is_sig_user_defined{$_[0]} : 0;
}

sub CLONE {}

sub TIEHASH  { bless({}, shift) }
sub UNTIE    {
    my ($obj,$count) = @_;

# Note: refcount of 1 unavoidable, likely due to how %SIG is internally referenced
# in this module; however, anything larger indicates a potential issue.

    Carp::carp "untie attempted while $count inner references still exist" if $count > 1;
}
sub STORE    {
    $usersig{$_[1]} = $_[2];
    _STORE($_[1], $_[2]);
}
sub FETCH    { $sig->{$_[1]} }
sub FIRSTKEY { my $a = scalar keys %{$sig}; each %{$sig} }
sub NEXTKEY  { each %{$sig} }
sub EXISTS   { exists $sig->{$_[1]} }
sub DELETE   { _STORE($_[1], undef) }
sub CLEAR    {
    $_[0]->DELETE($_) while ($_) = each %{$sig};
    return;
}
sub SCALAR   { scalar %{$sig} }

1;

__END__

=head1 NAME

forks::signals - signal management for forks

=head1 DESCRIPTION

This module is only intended for internal use by L<forks>.

=head1 CREDITS

Implementation inspired by Cory Johns' L<libalarm/Alarm::_TieSIG>.

=head1 AUTHOR

Eric Rybski <rybskej@yahoo.com>.  Please send all module inquries to me.

=head1 COPYRIGHT

Copyright (c)
 2005-2008 Eric Rybski <rybskej@yahoo.com>.
All rights reserved.  This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

L<forks>

=cut
