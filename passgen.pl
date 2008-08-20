#!/usr/bin/perl

use warnings;
use strict;
use POSIX;
use Getopt::Long qw(:config gnu_getopt);
use Math::Complex qw(logn);
use List::Util qw(reduce);

# Options
my $dict_file  = "/usr/share/dict/words";
my $rand_dev   = "/dev/urandom";
my $num_words  = 0;
my $key_len    = 64;
my $max_len    = 6;
my $quiet      = 0;
my $print_vers = 0;
my $print_help = 0;
my $version    = "1.0.0";

# Parse arguments
my $res = GetOptions(
    "k|key-length=i" => \$key_len,
    "w|words=i"      => \$num_words,
    "m|max-length=i" => \$max_len,
    "q|quiet!"       => \$quiet,
    "d|dictionary=s" => \$dict_file,
    "r|random-dev=s" => \$rand_dev,
    "v|version"      => \$print_vers,
    "h|help"         => \$print_help,
    );

if ($print_vers) {
    print <<EOF;
passgen, version $version
Copyright (C) 2008 Christopher Wellons
This is free software; see the source code for copying conditions.
There is ABSOLUTELY NO WARRANTY; not even for MERCHANTIBILITY or
FITNESS FOR A PARTICULAR PURPOSE.
EOF
    exit(0) if (!$print_help);
}

if ($print_help or !$res) {
    print <<EOF;
Usage $0 [options]

  -w  --words      num     Set number of words in passphrase
  -k, --key-length num     Set passphrase strength
  -m, --max-length num     Maximum word length (set to 0 for none)
  -d, --dictionary file    Specify word list to use
  -r, --random-dev file    Specify source of entropy (ie. /dev/random)
  -q, --quiet              Turn off verbosity
  -v, --version            Print version information
  -h, --help               Print this help information
EOF
    exit(0);
}

# Read in dictionary
open my($dict), $dict_file
    or die "Can't open word list ($dict_file).";
my @words = <$dict>;
chomp @words;
close $dict;

# Filter word list
@words = grep {$max_len >= length} @words if ($max_len > 0);

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
