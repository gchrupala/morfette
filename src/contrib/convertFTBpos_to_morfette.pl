#/usr/bin/perl

# this script takes a ftbX.pos files (as generated from the xml orginal data) and convert it to the morfette's input format
#  usage  : cat ftb_1.pos | convertFTBpos_to_morfette.pl | iconv -f latin1 -t utf8 > ftb_1.pos.morfetteready
# author : djame seddah 
# last modified  17/01/2010


use strict;

while(<>){
    chomp;
    my $lines="$_ ";
    $lines=~ s/(.+?)@(.+?)\/(.+?) /$2\t$1\t$3\n/g;
    print $lines;
    print "\n";
}  
    