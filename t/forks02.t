BEGIN {				# Magic Perl CORE pragma
    if ($ENV{PERL_CORE}) {
        chdir 't' if -d 't';
        @INC = '../lib';
    }
}

use forks; # must be done _before_ Test::More which loads real threads.pm
use forks::shared;

my $warn = ''; <<EOD;

Please note that there are some problems with testing the forks.pm module.
Some texts with 'WHOA!' may appear on the screen, and the final result of
the test may be inconclusive.  If all seperate tests have been successful,
then it should be safe to install the forks.pm modules.

EOD
warn $warn if $warn;

use Test::More tests => 6;

my $times = 100;

#= ARRAY ==============================================================

{
share( my @array );
my $tied = tied( @array );
isa_ok( $tied,'threads::shared',	'check object type' );

my @thread;
share( my $count );
$count  = 0;
push( @thread,threads->new( sub {
    while (1) {
        {lock( $count );
         return if $count == $times;
         $count++;
         push( @array,0+$count );
        }
    }
} ) ) foreach 1..10;
$_->join foreach @thread;

my $check;
$check .= $_ foreach 1..$times;
is( join('',@array),$check,		'check array contents' );

pop( @array ) foreach 1..$times;
is( join('',@array),'',			'check array contents' );
}

#= HASH ===============================================================

{
share( my %hash );
my $tied = tied( %hash );
isa_ok( $tied,'threads::shared',	'check object type' );

my @thread;
share( my $count );
$count = 0;
push( @thread,threads->new( sub {
    while (1) {
        {lock( $count );
         return if $count == $times;
         $count++;
         $hash{$count} = $count;
        }
    }
} ) ) foreach 1..10;
$_->join foreach @thread;

my $check;
$check .= ($_.$_) foreach 1..$times;
my $hash;
$hash .= ($_.$hash{$_}) foreach (sort {$a <=> $b} keys %hash);
is( $hash,$check,			'check hash contents' );

delete( $hash{$_} ) foreach 1..$times;
is( join('',%hash),'',			'check hash contents' );
}

#======================================================================

warn $warn if $warn;
