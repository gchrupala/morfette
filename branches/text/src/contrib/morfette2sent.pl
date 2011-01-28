#!/usr/bin/perl

# script to pipe the output of morfette (treetagger format, word by sentence, empty line marks the end of a sentence ) to the initial line by sentence
# usage :  ... | morfette predict WHATEVERMODEL | morfette2sent.pl > mystuff.tagged
# don't forget that morfette use utf8 for both input and output so you might need to convert your stuff using iconv -f utf8 -t latin and vice versa
# author, Djame Seddah (2009)
# last updated : January 20, 2010


use strict;



my $buf="";
while(<>){
    chomp;
    my $line=$_;
    if ($line=~/^$/){
        print "$buf\n";
        $buf="";
    }else{
      my ($word,$lemma,$pos)=split(/[ \t]+/,$line);
      $buf=$buf." "."$word^^$lemma^^$pos";
    }
}