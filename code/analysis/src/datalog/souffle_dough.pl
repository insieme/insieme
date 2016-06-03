#!/usr/bin/env perl

use strict;
use warnings;
use 5.10.0;
use Tie::File;

my $srcdir = $ARGV[0];
my $destdir = $ARGV[1];
my $incdir = "include";
my $basedir = system "pwd";

my $file_ext = "dl";
my $decltoken = "decl";
my $membertoken = "member";
my $usingtoken = "using";
my $mangle = "D474L06";

die "Dough takes exactly two arguments!" if @ARGV != 2;
die "Source dir ($srcdir) does not exist!" unless -d $srcdir;
die "Destination dir ($destdir) does not exist!" unless -d $destdir;
die "Invalid include dir ($incdir): Does not exist in '$srcdir'!" unless -d "$srcdir/$incdir";

# Step 1: Copy files
system "cp", <$srcdir/*.$file_ext>, "$destdir";
system "cp", "-r", "$srcdir/$incdir", "$destdir";

# Step 2: Name-mangle declarations in header files
chdir("$destdir/$incdir");
foreach my $filename (<*.$file_ext>) {

	say "Header file: $filename";

	# Skip ir.dl
	next if $filename eq "ir.$file_ext";

	tie my @lines, 'Tie::File', $filename;

	# Get all decls in this file
	my @decls = grep /^\s*\.$decltoken\s+/, @lines;
	$_ =~ s/^\s*\.$decltoken\s+(\w+).*/$1/ foreach @decls;

	# Create mangles
	foreach my $decl (@decls) {
		my $new_decl = create_mangle($filename, $decl);
		s/([^\w]?)$decl([^\w]+)/$1$new_decl$2/g foreach @lines;
	}

	# Replace relation names according to using-declarations
	expand_using_decls(\@lines);

	# Done: Commit changes to file
	untie $filename;
}
chdir("$basedir");

# Step 3: Name-mangle using-declarations in source files
chdir("$destdir");
foreach my $filename (<*.$file_ext>) {

	say "Source file: $filename";

	tie my @lines, 'Tie::File', $filename;

	expand_using_decls(\@lines);

	# Done: Commit changes to file
	untie $filename;
}
chdir("$basedir");

sub expand_using_decls {

	my ($lines_ref) = @_;

	# Get all using-declarations
	my %usings;
	for (@$lines_ref) {
		next unless s/(^\s*\.$usingtoken\s+(\w+)\s+from\s+(\w+)(?:\s+as\s+(\w+))?.*)/\/\/ $1/;
		my $use = $2;
		my $from = "$3.$file_ext";
		my $as = ($4?$4:$2);
		my $res = create_mangle($from, $use);
		$usings{$as} = $res;
		say "Found using: $use from $from";
	}

	# Replace relations with their mangled names
	while (my ($rel, $new_rel) = each %usings) {
		say "Replacing using of $rel";
		s/(^|\W)$rel(\s*\()/$1$new_rel$2/g foreach @$lines_ref;
	}
}

sub create_mangle {
	my ($filename, $decl) = @_;
	$filename =~ s/^(.)/\l$1/;
	$filename =~ s/([A-Z])/_\l$1/g;
	$filename =~ s/\./_/g;
	return "$mangle\_$filename\_$decl";
}
