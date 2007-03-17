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

use Test::More tests => 1;
use strict;
use warnings;

my $libs;
if (@custom_inc) {
    $libs = '-Mlib='.join(',', @custom_inc);
} else {
    $libs = '-Mlib='.join(',', ('blib/lib', 'blib/arch'));
}
my $desired_exit_val = 42;

my $exit_val = system(qq{perl $libs -e '}
    .q|BEGIN {delete $ENV{THREADS_DEBUG}}|
    .qq{use forks; exit($desired_exit_val);'}) >> 8;
cmp_ok($exit_val, '==', $desired_exit_val, 'Check that perl exit value is correct with forks');

1;
