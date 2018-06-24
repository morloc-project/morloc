#!/usr/bin/env perl

use strict;
use warnings;

&printResult(&dispatch(@ARGV));

sub printResult {
    my $result = shift;
    print "$result";
}

sub dispatch {
    if(scalar(@_) == 0){
        &usage();
    }

    my %cmds = (
        sample_index => \&call_sample_index,
        rand         => \&call_rand,
    );

    my $cmd = shift;
    my $result = undef;

    if($cmd eq "-h" || $cmd eq "-?" || $cmd eq "--help" || $cmd eq "?"){
        &usage();
    }
    if(exists($cmds{$cmd})){
        $result = $cmds{$cmd}(@_);
    } else {
        print STDERR "Command '$cmd' not found";
        &usage();
    }

    return $result;
}

sub usage {
    print STDERR "this is a help message\n";
    exit 0;
}

sub call_sample_index {
    if(scalar(@_) != 3){
        print STDERR "Expected 1 arguments to 'sample_index', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `Rscript pool.R m1 $_[0] $_[1] $_[2]`;
}

sub call_rand {
    if(scalar(@_) != 1){
        print STDERR "Expected 1 arguments to 'sample_index', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `Rscript pool.R m7 $_[0]`;
}
