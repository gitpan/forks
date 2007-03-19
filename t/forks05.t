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
use Config;
use strict;
use warnings;

my $libs;
if (@custom_inc) {
    $libs = '"-Mlib='.join(',', @custom_inc).'"';
} else {
    $libs = '"-Mlib='.join(',', ('blib/lib', 'blib/arch')).'"';
}
my $desired_exit_val = 42;

my $secure_perl_path = $Config{perlpath};
if ($^O ne 'VMS') {
    $secure_perl_path .= $Config{_exe}
        unless $secure_perl_path =~ m/$Config{_exe}$/i;
}

my $cmd = qq{$secure_perl_path $libs -e '}
    .q|BEGIN {delete $ENV{THREADS_DEBUG}}|
    .qq{ use forks; exit($desired_exit_val);'};

my $exit_val = system($cmd) >> 8;
cmp_ok($exit_val, '==', $desired_exit_val, 'Check that perl exit value is correct with forks');

1;
