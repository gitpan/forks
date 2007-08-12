package
	forks::signals; #hide from PAUSE
$VERSION = 0.25;

use strict;
use warnings;
use Carp ();
use vars qw($sig %usersig);
use List::MoreUtils;

my $tied;
my %sig_undefined_map;
my %sig_defined_map;

sub import {
	shift;

	unless ($sig) {
		%usersig = %SIG;
		$sig = \%SIG;
		*SIG = {};
		$tied = tie %SIG, __PACKAGE__;
	}

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
	if (!defined($s) || $s eq '' || $s eq 'DEFAULT') {
		if (grep(/^$k$/, keys %sig_undefined_map)) {
			$sig->{$k} = $sig_undefined_map{$k};
		} else {
			delete( $sig->{$k} );
		}
	} elsif ($s eq 'IGNORE') {
		$sig->{$k} = 'IGNORE';
	} else {
		$sig->{$k} = ref($s) eq 'CODE'
			? grep(/^$k$/, keys %sig_defined_map)
				? sub { $sig_defined_map{$k}->(@_); $s->(@_) }
				: $s
			: grep(/^$k$/, keys %sig_defined_map)
				? sub { $sig_defined_map{$k}->(@_); $s; }
				: $s;
	}
}

sub CLONE {}

sub TIEHASH { bless({}, shift) }
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
 2005-2007 Eric Rybski <rybskej@yahoo.com>.
All rights reserved.  This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

L<forks>

=cut
