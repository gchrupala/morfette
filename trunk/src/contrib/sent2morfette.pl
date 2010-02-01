#!/usr/bin/perl

# script for converting a regular raw sentence by line format file to the one expected by morfetted (treetagger format, word by line)
# beware, morfette expects utf8 so don't forget to convert your input using iconv -f latin1 -t utf8
# usage :  cat mystuff.raw | sent2morfette.pl | morfette predict WHATEVERMODEL | ....


use strict;


while (<>){
      chomp;
      my $line=$_;
      my @ltokens=split(/ +/,$line);
      print  join("\n",@ltokens);
      print "\n\n";      
  }



