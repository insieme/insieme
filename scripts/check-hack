#!/usr/bin/perl -w

use Data::Dumper;
use Getopt::Long;

# return the last component (file name) from a complete path
sub filename {
    my @c=split('/', $_[0]);
    return $c[-1];
}

my ($hackh, $gcc);
GetOptions("hackh=s" => \$hackh,   # path to hack.h within the Insieme tree
           "gcc=s"   => \$gcc,     # GCC source directory - root
    )
    and $hackh && -r $hackh
    and $gcc   && -d $gcc && -r $gcc
    or die("Error in cmd line args - read $0\n");

# initialize a map with GCC builtins missing in Clang
my %clangmap;
open($file, "curl -s http://clang.llvm.org/builtins.py |")
    or die "Could not read list of builtins from Clang web site";
while (<$file>) {
    $clangmap{$1}=$2 if /'(__builtin_.*)': '(.*)'/;
}
close($file);

# check which of the builtins are not included in our hack.h and print these
my %missing=();
foreach (keys %clangmap) {
    `grep -s $_ $hackh`;   # function found in hack.h?!
    if ($?) {
        my @gccfiles=map { /^(.+):/; $1 } grep(!/return/,
                     `grep -e $_'[^[:alnum:]_]' $gcc/gcc/config/i386/*.h`);
        # drop unknown builtins - clang sources are not correct sometimes
        $missing{$_}=$gccfiles[0] if $gccfiles[0];
    }
}

warn   "builtins missing from ".filename($hackh).":\n";
warn   "   (none)\n" unless keys %missing;
printf "   %-30s%s\n", $_, $missing{$_} foreach (sort keys %missing);
