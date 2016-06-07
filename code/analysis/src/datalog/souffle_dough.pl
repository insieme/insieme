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
my $comptoken = "comp";
my $inittoken = "init";
my $membertoken = "member";
my $usingtoken = "using";
my $mangle = "D474L06";

die "Dough takes exactly two arguments!" if @ARGV != 2;
die "Source dir ($srcdir) does not exist!" unless -d $srcdir;
die "Destination dir ($destdir) does not exist!" unless -d $destdir;
die "Invalid include dir ($incdir): Does not exist in '$srcdir'!" unless -d "$srcdir/$incdir";

# Step 1: Copy files
system "cp", <$srcdir/*.$file_ext>, "$destdir";
system "mkdir", "-p", "$destdir/$incdir";
system "cp", <$srcdir/$incdir/*.$file_ext>, "$destdir/$incdir";

# Step 2: Name-mangle declarations in header files
chdir("$destdir/$incdir");
foreach my $filename (<*.$file_ext>) {

	say "Header file: $filename";

	tie my @lines, 'Tie::File', $filename;

	# Skip mangling for ir.dl
	unless ($filename eq "ir.$file_ext") {

		# Get all decls, inits and comps in this file
		my @decls = grep /^\s*\.$decltoken\s+/, @lines;
		my @inits = grep /^\s*\.$inittoken\s+/, @lines;
		my @comps = grep /^\s*\.$comptoken\s+/, @lines;
		$_ =~ s/^\s*\.$decltoken\s+(\w+).*/$1/ foreach @decls;
		$_ =~ s/^\s*\.$inittoken\s+(\w+).*/$1/ foreach @inits;
		$_ =~ s/^\s*\.$comptoken\s+(\w+).*/$1/ foreach @comps;
		push @decls, @comps;

		# Create mangles
		foreach my $decl (@decls) {
			my $new_decl = create_mangle($filename, $decl);
			s/([^\w]?)$decl([^\w]+)/$1$new_decl$2/g foreach @lines;
		}
		foreach my $init (@inits) {
			my $new_init = create_mangle($filename, $init);
			s/(\.$inittoken\s+)$init(\s*=\s*\w+)/$1$new_init$2/g foreach @lines;
			s/([^\w]?)$init(\.\w+\s*\()/$1$new_init$2/g foreach @lines;
		}
	}

	# Replace relation names according to using-declarations
	expand_using_decls(\@lines);
	convert_usings_to_includes(\@lines);

	# Change 'member' declarations back to decl
	s/(\s*\.)$membertoken([^\w]+)/$1$decltoken$2/g foreach @lines;

	# Done: Commit changes to file
	unshift @lines, "#pragma once";
	untie $filename;
}
chdir("$basedir");

# Step 3: Name-mangle using-declarations in source files
chdir("$destdir");
foreach my $filename (<*.$file_ext>) {

	say "Source file: $filename";

	tie my @lines, 'Tie::File', $filename;

	expand_using_decls(\@lines);
	convert_usings_to_includes(\@lines);

	# Done: Commit changes to file
	untie $filename;
}
chdir("$basedir");

sub expand_using_decls {

	my ($lines_ref) = @_;

	# Get all using-declarations
	my %usings;
	for (@$lines_ref) {
		next unless m/(^\s*\.$usingtoken\s+(\w+)\s+from\s+(\w+)(?:\s+as\s+(\w+))?.*)/;
		my $use = $2;
		my $from = camelcase_to_underscore("$3.$file_ext");
		my $as = ($4?$4:$2);
		my $res = create_mangle($from, $use);
		$usings{$as} = $res;
	}

	# Replace relations with their mangled names
	while (my ($rel, $new_rel) = each %usings) {
		s/(^|\W)$rel(\s*\()/$1$new_rel$2/g foreach @$lines_ref;
		s/(^|\W)(\.$inittoken\s+\w+\s*=\s*)$rel(\s*)/$1$2$new_rel$3/g foreach @$lines_ref;
	}
}

sub convert_usings_to_includes {
	my ($lines_ref) = @_;
	unshift @$lines_ref, ".using _ from ir";
	my $searchstr = '^\s*\.'.$usingtoken.'\s+\w+\s+from\s+(\w+)(?:\s+as\s+\w+)?.*';
	s/$searchstr/"#include \"".camelcase_to_underscore($1).".dl\""/e foreach @$lines_ref;
}

sub create_mangle {
	my ($filename, $decl) = @_;
	$filename =~ s/\./_/g;
	return "$mangle\_$filename\_$decl";
}

sub camelcase_to_underscore {
	my ($in) = @_;
	$in =~ s/^(.)/\l$1/;
	$in =~ s/([A-Z])/_\l$1/g;
	return "$in";
}

