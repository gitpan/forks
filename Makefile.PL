require 5.006;

BEGIN {
    die "Doesn't make sense to install 'forks' on this system.\n"
     if ($^O =~ m#Win32# or $ENV{THIS_IS_WIN32});
} #BEGIN

use strict;
use ExtUtils::MakeMaker;
eval "use Devel::Required";

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
 NAME       => "forks",
 AUTHOR     => 'Eric Rybski (rybskej@yahoo.com)',
 ABSTRACT   => 'forks - emulate threads with fork',
 VERSION_FROM   => 'lib/forks.pm',

 PREREQ_PM  => {@extra_prereq,qw(
            Devel::Required     0
            Scalar::Util        1.01
            Storable            0
            IO::Socket          1.18
            reaper              0.03
                )},
(MM->can('signature_target') ? (SIGN => 1) : ()),
);
