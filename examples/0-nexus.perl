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

    my $cmd = shift;
    my $result = undef;

    my %cmds = (
        transpose => \&call_transpose,
        rand_uniform => \&call_rand_uniform, 
        sum => \&call_sum,
        ceiling => \&call_ceiling,
        nrow => \&call_nrow
    );

    if($cmd eq '-h' || $cmd eq '-?' || $cmd eq '--help' || $cmd eq '?'){
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

sub usage{
    print STDERR "The following commands are exported:\n";
    print STDERR "  rand_uniform :: Int, Num, Num -> [Int]\n";
    print STDERR "  transpose :: Matrix -> Matrix\n";
    print STDERR "  sum :: [Num] -> Num\n";
    print STDERR "  ceiling :: Num -> Int\n";
    print STDERR "  nrow :: Table -> Int\n";
    exit 0;
}

sub call_transpose{
    if(scalar(@_) != 1){
        print STDERR "Expected 1 arguments to 'transpose', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `Rscript 0-pool.R m46 '$_[0]'`
}

sub call_rand_uniform{
    if(scalar(@_) != 3){
        print STDERR "Expected 3 arguments to 'rand_uniform', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `Rscript 0-pool.R m13 '$_[0]' '$_[1]' '$_[2]'`
}

sub call_sum{
    if(scalar(@_) != 1){
        print STDERR "Expected 1 arguments to 'sum', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `Rscript 0-pool.R m76 '$_[0]'`
}

sub call_ceiling{
    if(scalar(@_) != 1){
        print STDERR "Expected 1 arguments to 'ceiling', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `Rscript 0-pool.R m40 '$_[0]'`
}

sub call_nrow{
    if(scalar(@_) != 1){
        print STDERR "Expected 1 arguments to 'nrow', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `Rscript 0-pool.R m56 '$_[0]'`
}
