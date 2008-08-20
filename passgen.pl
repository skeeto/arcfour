#!/usr/bin/perl

use warnings;
use strict;
use POSIX;
use Math::Complex qw(logn);
use List::Util qw(reduce);

# Options
my $dict_file = "/usr/share/dict/words";
my $rand_dev  = "/dev/urandom";
my $num_words = 0;
my $key_len   = 64;
my $max_len   = 6;
my $quiet     = 0;

# Read in dictionary
open my($dict), $dict_file
    or die "Can't open word list ($dict_file).";
my @words = <$dict>;
chomp @words;
close $dict;

# Filter word list
@words = grep {$max_len >= length} @words;

# Calculate bits
my $bits_per_word = logn($#words, 2);
print "Bits per word: $bits_per_word\n" if (!$quiet);
if ($num_words == 0) {
    $num_words = ceil($key_len / $bits_per_word);
} else {
    $key_len = $num_words * $bits_per_word;
}

# Generate words
open my($rand), $rand_dev
    or die "Can't open $rand_dev.";
my @pass;
my $csize = ceil($bits_per_word / 8);
while ($#pass + 1 < $num_words) {
    read $rand, my($bytes), $csize;
    no warnings 'once';
    my $n = reduce {$a*256 + $b} unpack("C*", $bytes);
    push(@pass, $words[$n]) if ($n <= $#words);
}
close $rand;

# Print results
print "Key length: " . ($num_words * $bits_per_word) . "\n" if (!$quiet);
print "@pass\n";
