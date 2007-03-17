#!/usr/local/bin/perl -w
my @custom_inc;
BEGIN {
    if ($ENV{PERL_CORE}) {
        chdir 't' if -d 't';
        @custom_inc = @INC = '../lib';
    } elsif (!grep /blib/, @INC) {
        chdir 't' if -d 't';
        unshift @INC, (@custom_inc = ('../blib/lib', '../blib/arch'));
    }
}

BEGIN {delete $ENV{THREADS_DEBUG}} # no debugging during testing!

no warnings 'threads';
use forks; # must be done _before_ Test::More which loads real threads.pm
use forks::shared;

diag( <<EOD );

These tests check inter-thread signaling.

EOD

use Test::More tests => 3;
use strict;
use warnings;

my $thr = threads->new(sub { while (1) { sleep 1; } });
$thr->kill('TERM');
sleep 3;
ok(!$thr->is_running(), 'Check that thread is no longer running');

my $gotsig : shared = 0;
$thr = threads->new(sub {
	$SIG{TERM} = sub { $gotsig = 1; CORE::exit(); };
	while (1) { sleep 1; }
});
sleep 3;
$thr->kill('TERM');
sleep 3;
ok(!$thr->is_running(), 'Check that thread is no longer running');
ok($gotsig, 'Check that custom signal handler was used');

$_->join() foreach threads->list();

1;
