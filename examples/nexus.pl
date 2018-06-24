#!/usr/bin/env perl

# this is a hand written program designed as an example of what the
# coordinating program should look like.

=head1 NAME

B<Manifold Nexus> - this is the manager for a Morloc workflow

=head1 INPUT

foo

=cut


use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;

if(scalar(@ARGV) == 0){
    &usage();
}

my $cmd = shift;

my %cmds = (
    sample_index => \&call_sample_index,
    rand         => \&call_rand,
);

my %type = (
    sample_index => \&type_sample_index,
    rand         => \&type_rand,
);

if($cmd eq "-h" || $cmd eq "-?" || $cmd eq "--help" || $cmd eq "?"){
    &usage();
}

if(exists($cmds{$cmd})){
    my $x = $cmds{$cmd}();
    print $x;
}

sub usage {
    print STDERR "this is a help message\n";
    exit 0;
}

sub call_sample_index {
    if(scalar(@ARGV) != 3){
        print STDERR "Expected 1 arguments to 'sample_index', given " . 
        scalar(@ARGV) . "\n";
        exit 1;
    }
    return `Rscript pool.R m1 $ARGV[0] $ARGV[1] $ARGV[2]`;
}

sub call_rand {
    if(scalar(@ARGV) != 1){
        print STDERR "Expected 1 arguments to 'sample_index', given " . 
        scalar(@ARGV) . "\n";
        exit 1;
    }
    return `Rscript pool.R m7 $ARGV[0]`;
}
