#! /usr/bin/env perl

use strict;
use warnings;

my $ROOT="tests/install/root";
my $USER=$ENV{USER};

my $ignore = qr!/lib\d*/modules|/X11|alsa-lib|aspell|dosemu|emacs|erlang|/perl|python|ruby|lisp|sbcl|/ghc-|ocaml|evolution|office|gimp|gtk|mysql|postgres|wordnet|xulrunner!;

sub linkdir ($$$) {
  my ($FROM, $TO, $DEPTH) = @_;
  mkdir $TO;
  for my $f (`ls $FROM`) {
    chomp $f;
    if (-d "$FROM/$f") {
      if (($DEPTH > 0) && ($FROM !~ $ignore)) {
        linkdir("$FROM/$f", "$TO/$f", $DEPTH-1);
      }
    } else {
      link "$FROM/$f", "$TO/$f";
    }
  }
}

mkdir "$ROOT";
mkdir "$ROOT/bin";
mkdir "$ROOT/sbin";
mkdir "$ROOT/dev";
mkdir "$ROOT/etc";
mkdir "$ROOT/etc/alternatives";
mkdir "$ROOT/lib";
mkdir "$ROOT/lib64";
mkdir "$ROOT/usr";
mkdir "$ROOT/usr/bin";
mkdir "$ROOT/usr/include";
mkdir "$ROOT/usr/lib";
mkdir "$ROOT/usr/lib/gcc";

linkdir "/bin", "$ROOT/bin", 1;
linkdir "/sbin", "$ROOT/sbin", 1;
link "/etc/passwd", "$ROOT/etc/passwd";
linkdir "/etc/alternatives", "$ROOT/etc/alternatives", 1;
linkdir "/lib", "$ROOT/lib", 3;
linkdir "/lib64", "$ROOT/lib64", 3;
linkdir "/usr/bin", "$ROOT/usr/bin", 3;
linkdir "/usr/include", "$ROOT/usr/include", 2;
linkdir "/usr/lib", "$ROOT/usr/lib", 3;
linkdir "/usr/lib/gcc", "$ROOT/usr/lib/gcc", 3;

`make dist`;
my $VERSION=`cat VERSION`;
chomp $VERSION;
`cp chibi-scheme-$VERSION.tgz $ROOT/`;
`sed -e 's/\@VERSION\@/$VERSION/g' <tests/install/run-install-test.sh >$ROOT/bin/run-install-test.sh`;
`chmod 755 $ROOT/bin/run-install-test.sh`;
exec "sudo chroot $ROOT run-install-test.sh";
