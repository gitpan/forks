package
	forks::signals; #hide from PAUSE
$VERSION = 0.24;

use strict;
use warnings;
use Carp ();
use vars qw($sig);
use List::MoreUtils;

my %sig_default_map;
my %sig_defined_map;

sub import {
	shift;
	return if $sig;

	$sig = \%SIG;
	*SIG = {};

	%sig_default_map = map { $_ => $sig->{$_} }
		map(defined $sig->{$_} && $sig->{$_} ne '' ? $_ : (), keys %{$sig});
	$SIG{$_} = $sig_default_map{$_} foreach keys %sig_default_map;

	if ((my $idx = List::MoreUtils::firstidx(
		sub { $_ eq 'ifdef' }, @_)) >= 0) {
		if (ref $_[$idx+1] eq 'HASH') {
			my (undef, $opts) = splice(@_, $idx, 2);
			%sig_defined_map = map { $_ => $opts->{$_} } 
				map(defined $opts->{$_} && $opts->{$_} ne ''
					? $_ : (), keys %{$opts});
		} else {
			splice(@_, $idx, 1);
			%sig_defined_map = %sig_default_map;
		}
	}

	return tie %SIG, __PACKAGE__;
}

sub CLONE {}

sub TIEHASH { bless({}, shift) }
sub STORE    {
	shift;
	my $k = shift;
	my $s = shift;
	if (!defined($s) || $s eq '') {
		if (grep(/^$k$/, keys %sig_default_map)) {
			$sig->{$k} = $sig_default_map{$k};
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
sub FETCH    { $sig->{$_[1]} }
sub FIRSTKEY { my $a = scalar keys %{$sig}; each %{$sig} }
sub NEXTKEY  { each %{$sig} }
sub EXISTS   { exists $sig->{$_[1]} }
sub DELETE   { delete $sig->{$_[1]} }
sub CLEAR    { %{$sig} = () }
sub SCALAR   { scalar %{$sig} }

1;

__END__

=head1 NAME

forks::signals - signal management for forks

=head1 DESCRIPTION

This module is only intended for internal use by L<forks>.

=head1 CREDITS

Implementation inspired by Cory Johns' L<libalarm>.

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
