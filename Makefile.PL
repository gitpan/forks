require 5.000;
use ExtUtils::MakeMaker;

WriteMakefile (
 NAME		=> "forks",
 ($[ >= 5.005) ?
 (AUTHOR	=> 'Elizabeth Mattijsen (liz@dijkmat.nl)',
  ABSTRACT	=> 'forks - simulate threads with fork') : (),
 VERSION_FROM	=> 'lib/forks.pm',
 PREREQ_PM	=> {
		    Test::More => 0,
		    Storable => 0,
		    load => '0.03',
                   },
);
