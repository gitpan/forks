require 5.006;
use strict;
use ExtUtils::MakeMaker;

my $old = 'forks.xs';
my $new = "$old.new";;
my $from = qq{#    PROTOTYPE: \\[\$\@\%]\n};
my $to   = qq{    PROTOTYPE: \\[\$\@\%]\n};

my @extra_prereq;
if ($] < 5.008) { # no prototypes before 5.8.0, we need to filter also
    ($from,$to) = ($to,$from);
    push @extra_prereq,qw(Filter::Util::Call 0);
}

warn "Fixing prototypes in $old\n";
open my $in,'<',$old   or die "Could not open $old: $!\n";;
open my $out,'>',$new  or die "Could not create $new: $!\n";;

while (<$in>) {
    print $out $_ eq $from ? $to : $_;
}

close $out        or die "Could not close $new: $!\n";
close $in         or die "Could not close $old: $!\n";
chmod 0644, $old  or die "Could not change permissions on $old\n";
unlink $old       or die "Could not remove $old\n";
rename $new,$old  or die "Could not rename $new to $old\n";
chmod 0444, $old  or die "Could not change permissions on $old\n";

WriteMakefile (
 NAME		=> "forks",
 AUTHOR		=> 'Elizabeth Mattijsen (liz@dijkmat.nl)',
 ABSTRACT	=> 'forks - simulate threads with fork',
 VERSION_FROM	=> 'lib/forks.pm',

 PREREQ_PM	=> {@extra_prereq,qw(
		    Test::More		0
		    Scalar::Util	1.01
		    Storable		0
		    load		0.11
                   )},
);
