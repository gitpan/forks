#!/usr/local/bin/perl -T -w
BEGIN {				# Magic Perl CORE pragma
    if ($ENV{PERL_CORE}) {
        chdir 't' if -d 't';
        @INC = '../lib';
    }
}

use forks; # must be done _before_ Test::More which loads real threads.pm
use forks::shared;

my $warn = <<EOD;

Please note that there are some problems with testing the forks.pm module.
Some texts with 'WHOA!' may appear on the screen, and the final result of
the test may be inconclusive.  If all seperate tests have been successful,
then it should be safe to install the forks.pm modules.

EOD
warn $warn if $warn;

use Test::More tests => 53;
use strict;
use warnings;

can_ok( 'threads',qw(
 async
 create
 detach
 equal
 import
 isthread
 join
 list
 new
 self
 tid
) );

can_ok( 'threads::shared',qw(
 cond_broadcast
 cond_signal
 cond_wait
 lock
 share
 TIEARRAY
 TIEHANDLE
 TIEHASH
 TIESCALAR
) );

unless (my $pid = fork) {
  threads->isthread if defined($pid);
  exit;
}
sleep 3; # make sure fork above has started to ensure tid's are in sync

my $t1 = threads->new( sub { threads->tid } );
ok( $t1,'check whether we can start a thread with new()' );

my $t2 = threads->create( sub { threads->tid } );
ok( $t2,'check whether we can start a thread with create()' );

my $t3 = async( sub { threads->object( threads->tid )->tid } );
ok( $t3,'check whether we can start a thread with async()' );

my %tid;
$tid{$_->tid} = undef foreach threads->list;
is( join('',sort keys %tid),'234','check tids of all threads' );

is( $t3->join,'4','check return value thread 3' );
is( $t2->join,'3','check return value thread 2' );
is( $t1->join,'2','check return value thread 1' );

#== SCALAR =========================================================

my $scalar = 10;
share( $scalar );
my $tied = tied( $scalar );
isa_ok( $tied,'threads::shared',	'check tied object type' );

cmp_ok( $scalar,'==',10,		'check scalar numerical fetch' );
$scalar++;
cmp_ok( $scalar,'==',11,		'check scalar increment' );
$scalar = 'Apenootjes';
is( $scalar,'Apenootjes',		'check scalar fetch' );

threads->new( sub {$scalar = 'from thread'} )->join;
is( $scalar,'from thread',		'check scalar fetch' );

#== ARRAY ==========================================================

my @array = qw(a b c);
share( @array );
$tied = tied( @array );
isa_ok( $tied,'threads::shared',	'check tied object type' );
is( join('',@array),'abc',		'check array fetch' );

push( @array,qw(d e f) );
is( join('',@array),'abcdef',		'check array fetch' );

threads->new( sub {push( @array,qw(g h i) )} )->join;
is( join('',@array),'abcdefghi',	'check array fetch' );

shift( @array );
is( join('',@array),'bcdefghi',		'check array fetch' );

unshift( @array,'a' );
is( join('',@array),'abcdefghi',	'check array fetch' );

pop( @array );
is( join('',@array),'abcdefgh',		'check array fetch' );

push( @array,'i' );
is( join('',@array),'abcdefghi',	'check array fetch' );

splice( @array,3,3 );
is( join('',@array),'abcghi',		'check array fetch' );

splice( @array,3,0,qw(d e f) );
is( join('',@array),'abcdefghi',	'check array fetch' );

splice( @array,0,3,qw(d e f) );
is( join('',@array),'defdefghi',	'check array fetch' );

delete( $array[0] );
is( join('',map {$_ || ''} @array),'efdefghi',		'check array fetch' );

@array = qw(a b c d e f g h i);
is( join('',@array),'abcdefghi',	'check array fetch' );

cmp_ok( $#array,'==',8,			'check size' );
ok( exists( $array[8] ),		'check whether array element exists' );
ok( !exists( $array[9] ),		'check whether array element exists' );

$#array = 10;
cmp_ok( scalar(@array),'==',11,		'check number of elements' );
is( join('',map {$_ || ''} @array),'abcdefghi',	'check array fetch' );

ok( !exists( $array[10] ),		'check whether array element exists' );
$array[10] = undef;
ok( exists( $array[10] ),		'check whether array element exists' );

ok( !exists( $array[11] ),		'check whether array element exists' );
ok( !defined( $array[10] ),		'check whether array element defined' );
ok( !defined( $array[11] ),		'check whether array element defined' );
cmp_ok( scalar(@array),'==',11,		'check number of elements' );

@array = ();
cmp_ok( scalar(@array),'==',0,		'check number of elements' );
is( join('',@array),'',			'check array fetch' );

#== HASH ===========================================================

my %hash = (a => 'A');
share( %hash );
$tied = tied( %hash );
isa_ok( $tied,'threads::shared',	'check tied object type' );
is( $hash{'a'},'A',			'check hash fetch' );

$hash{'b'} = 'B';
is( $hash{'b'},'B',			'check hash fetch' );

is( join('',sort keys %hash),'ab',	'check hash keys' );

ok( !exists( $hash{'c'} ),		'check existence of key' );
threads->new( sub { $hash{'c'} = 'C' } )->join;
ok( exists( $hash{'c'} ),		'check existence of key' );
is( $hash{'c'},'C',			'check hash fetch' );

is( join('',sort keys %hash),'abc',	'check hash keys' );

my %otherhash = %hash;
is( join('',sort keys %otherhash),'abc','check hash keys' );

my @list;
while (my ($key,$value) = each %hash) { push( @list,$key,$value ) }
is( join('',sort @list),'ABCabc',	'check all eaches' );

delete( $hash{'b'} );
is( join('',sort keys %hash),'ac',	'check hash keys' );

%hash = ();
cmp_ok( scalar(keys %hash),'==',0,	'check number of elements' );
is( join('',keys %hash),'',		'check hash fetch' );

#===================================================================

warn $warn if $warn;
