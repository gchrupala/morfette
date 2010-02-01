#!/usr/bin/perl

# script which converts Morfette's ftbmax tagset output to ftb-cc output
# author Djame Seddah
# last modified January 31 2010

 
use strict;


my %dict=();
$dict{"_4/D_card-mp"}="_4/DET";
$dict{"_9/N_C-ms"}="_9/NC";
$dict{"A_card-fp"}="ADJ";
$dict{"A_card-fs"}="ADJ";
$dict{"A_card-mp"}="ADJ";
$dict{"A_card-ms"}="ADJ";
$dict{"ADV"}="ADV";
$dict{"ADV_excl"}="ADV";
$dict{"ADV_int"}="ADVWH";
$dict{"ADV_neg"}="ADV";
$dict{"A_ind-fp"}="ADJ";
$dict{"A_ind-fs"}="ADJ";
$dict{"A_ind-mp"}="ADJ";
$dict{"A_ind-ms"}="ADJ";
$dict{"A_int-fp"}="ADJWH";
$dict{"A_int-fs"}="ADJWH";
$dict{"A_int-mp"}="ADJWH";
$dict{"A_int-ms"}="ADJWH";
$dict{"_an/N_C-mp"}="_an/NC";
$dict{"A_ord-fp"}="ADJ";
$dict{"A_ord-fs"}="ADJ";
$dict{"A_ord-mp"}="ADJ";
$dict{"A_ord-ms"}="ADJ";
$dict{"A_poss-ms"}="ADJ";
$dict{"A_qual-3mp"}="ADJ";
$dict{"A_qual-fp"}="ADJ";
$dict{"A_qual-fs"}="ADJ";
$dict{"A_qual-mp"}="ADJ";
$dict{"A_qual-ms"}="ADJ";
$dict{"C_C"}="CC";
$dict{"CL_obj-1fp"}="CLO";
$dict{"CL_obj-1fs"}="CLO";
$dict{"CL_obj-1mp"}="CLO";
$dict{"CL_obj-1ms"}="CLO";
$dict{"CL_obj-2fp"}="CLO";
$dict{"CL_obj-2mp"}="CLO";
$dict{"CL_obj-2ms"}="CLO";
$dict{"CL_obj-3fp"}="CLO";
$dict{"CL_obj-3fs"}="CLO";
$dict{"CL_obj-3mp"}="CLO";
$dict{"CL_obj-3ms"}="CLO";
$dict{"CL_refl-1fs"}="CLR";
$dict{"CL_refl-1mp"}="CLR";
$dict{"CL_refl-1ms"}="CLR";
$dict{"CL_refl-2mp"}="CLR";
$dict{"CL_refl-3fp"}="CLR";
$dict{"CL_refl-3fs"}="CLR";
$dict{"CL_refl-3mp"}="CLR";
$dict{"CL_refl-3ms"}="CLR";
$dict{"CL_suj-1fp"}="CLS";
$dict{"CL_suj-1fs"}="CLS";
$dict{"CL_suj-1mp"}="CLS";
$dict{"CL_suj-1ms"}="CLS";
$dict{"CL_suj-2fs"}="CLS";
$dict{"CL_suj-2mp"}="CLS";
$dict{"CL_suj-2ms"}="CLS";
$dict{"CL_suj-3fp"}="CLS";
$dict{"CL_suj-3fs"}="CLS";
$dict{"CL_suj-3mp"}="CLS";
$dict{"CL_suj-3ms"}="CLS";
$dict{"C_S"}="CS";
$dict{"D_card-fp"}="DET";
$dict{"D_card-fs"}="DET";
$dict{"D_card-mp"}="DET";
$dict{"D_card-ms"}="DET";
$dict{"D_def-fp"}="DET";
$dict{"D_def-fs"}="DET";
$dict{"D_def-mp"}="DET";
$dict{"D_def-ms"}="DET";
$dict{"D_dem-fp"}="DET";
$dict{"D_dem-fs"}="DET";
$dict{"D_dem-mp"}="DET";
$dict{"D_dem-ms"}="DET";
$dict{"D_excl-fs"}="DET";
$dict{"D_excl-ms"}="DET";
$dict{"D_ind-fp"}="DET";
$dict{"D_ind-fs"}="DET";
$dict{"D_ind-mp"}="DET";
$dict{"D_ind-ms"}="DET";
$dict{"D_int-fp"}="DETWH";
$dict{"D_int-fs"}="DETWH";
$dict{"D_int-mp"}="DETWH";
$dict{"D_int-ms"}="DETWH";
$dict{"D_neg-fs"}="DET";
$dict{"D_neg-ms"}="DET";
$dict{"D_part-fs"}="DET";
$dict{"D_part-ms"}="DET";
$dict{"D_poss-1fp"}="DET";
$dict{"D_poss-1fs"}="DET";
$dict{"D_poss-1mp"}="DET";
$dict{"D_poss-1ms"}="DET";
$dict{"D_poss-2fp"}="DET";
$dict{"D_poss-2mp"}="DET";
$dict{"D_poss-2ms"}="DET";
$dict{"D_poss-3fp"}="DET";
$dict{"D_poss-3fs"}="DET";
$dict{"D_poss-3mp"}="DET";
$dict{"D_poss-3ms"}="DET";
$dict{"D_poss-mp"}="DET";
$dict{"D_poss-ms"}="DET";
$dict{"ET"}="ET";
$dict{"_h/N_C-mp"}="_h/NC";
$dict{"I"}="I";
$dict{"N_card-fp"}="NC";
$dict{"N_card-fs"}="NC";
$dict{"N_card-mp"}="NC";
$dict{"N_card-ms"}="NC";
$dict{"N_C-fp"}="NC";
$dict{"N_C-fs"}="NC";
$dict{"N_C-mp"}="NC";
$dict{"N_C-ms"}="NC";
$dict{"N_P-fp"}="NPP";
$dict{"N_P-fs"}="NPP";
$dict{"N_P-mp"}="NPP";
$dict{"N_P-ms"}="NPP";
$dict{"N_P-s"}="NPP";
$dict{"_OPE/N_C-fs"}="_OPE/NC";
$dict{"P+D_def"}="P+D";
$dict{"P+D"}="P+D";
$dict{"PONCT_S"}="PONCT";
$dict{"PONCT_W"}="PONCT";
$dict{"P"}="P";
$dict{"P+PRO_dem-3ms"}="P+PRO";
$dict{"P+PRO_int-3ms"}="P+PRO";
$dict{"P+PRO_rel-3fp"}="P+PRO";
$dict{"P+PRO_rel-3fs"}="P+PRO";
$dict{"P+PRO_rel-3mp"}="P+PRO";
$dict{"P+PRO_rel-3ms"}="P+PRO";
$dict{"PREF"}="PREF";
$dict{"PRO_card-1mp"}="PRO";
$dict{"PRO_card-3mp"}="PRO";
$dict{"PRO_card-3ms"}="PRO";
$dict{"PRO_card-fp"}="PRO";
$dict{"PRO_card-mp"}="PRO";
$dict{"PRO_card-ms"}="PRO";
$dict{"PRO_dem-3fp"}="PRO";
$dict{"PRO_dem-3fs"}="PRO";
$dict{"PRO_dem-3mp"}="PRO";
$dict{"PRO_dem-3ms"}="PRO";
$dict{"PRO_ind-1fp"}="PRO";
$dict{"PRO_ind-1fs"}="PRO";
$dict{"PRO_ind-1ms"}="PRO";
$dict{"PRO_ind-3fp"}="PRO";
$dict{"PRO_ind-3fs"}="PRO";
$dict{"PRO_ind-3mp"}="PRO";
$dict{"PRO_ind-3ms"}="PRO";
$dict{"PRO_ind-fp"}="PRO";
$dict{"PRO_ind-mp"}="PRO";
$dict{"PRO_ind-ms"}="PRO";
$dict{"PRO_int-1ms"}="PROWH";
$dict{"PRO_int-3fp"}="PROWH";
$dict{"PRO_int-3mp"}="PROWH";
$dict{"PRO_int-3ms"}="PROWH";
$dict{"PRO_int-ms"}="PROWH";
$dict{"PRO_neg-1ms"}="PRO";
$dict{"PRO_neg-3fs"}="PRO";
$dict{"PRO_neg-3ms"}="PRO";
$dict{"PRO_pers-1fs"}="PRO";
$dict{"PRO_pers-1mp"}="PRO";
$dict{"PRO_pers-1ms"}="PRO";
$dict{"PRO_pers-2mp"}="PRO";
$dict{"PRO_pers-3fp"}="PRO";
$dict{"PRO_pers-3fs"}="PRO";
$dict{"PRO_pers-3mp"}="PRO";
$dict{"PRO_pers-3ms"}="PRO";
$dict{"PRO_poss-3fs"}="PRO";
$dict{"PRO_poss-3mp"}="PRO";
$dict{"PRO_poss-3ms"}="PRO";
$dict{"PRO_refl-1mp"}="PRO";
$dict{"PRO_refl-1ms"}="PRO";
$dict{"PRO_refl-3fp"}="PRO";
$dict{"PRO_refl-3fs"}="PRO";
$dict{"PRO_refl-3mp"}="PRO";
$dict{"PRO_refl-3ms"}="PRO";
$dict{"PRO_refl-mp"}="PRO";
$dict{"PRO_refl-ms"}="PRO";
$dict{"PRO_rel-1ms"}="PROREL";
$dict{"PRO_rel-3fp"}="PROREL";
$dict{"PRO_rel-3fs"}="PROREL";
$dict{"PRO_rel-3mp"}="PROREL";
$dict{"PRO_rel-3ms"}="PROREL";
$dict{"PRO_rel-fs"}="PROREL";
$dict{"_S/N_P-s"}="_S/NPP";
$dict{"V-imperatifpresent1p"}="VIMP";
$dict{"V-imperatifpresent2p"}="VIMP";
$dict{"V-imperatifpresent2s"}="VIMP";
$dict{"V-indicatifconditionnel1p"}="V";
$dict{"V-indicatifconditionnel1s"}="V";
$dict{"V-indicatifconditionnel2p"}="V";
$dict{"V-indicatifconditionnel3p"}="V";
$dict{"V-indicatifconditionnel3s"}="V";
$dict{"V-indicatiffutur1p"}="V";
$dict{"V-indicatiffutur1s"}="V";
$dict{"V-indicatiffutur2p"}="V";
$dict{"V-indicatiffutur3p"}="V";
$dict{"V-indicatiffutur3s"}="V";
$dict{"V-indicatifimparfait1p"}="V";
$dict{"V-indicatifimparfait1s"}="V";
$dict{"V-indicatifimparfait2p"}="V";
$dict{"V-indicatifimparfait3p"}="V";
$dict{"V-indicatifimparfait3s"}="V";
$dict{"V-indicatifpasse-simple1s"}="V";
$dict{"V-indicatifpasse-simple3p"}="V";
$dict{"V-indicatifpasse-simple3s"}="V";
$dict{"V-indicatifpresent1p"}="V";
$dict{"V-indicatifpresent1s"}="V";
$dict{"V-indicatifpresent2p"}="V";
$dict{"V-indicatifpresent2s"}="V";
$dict{"V-indicatifpresent3p"}="V";
$dict{"V-indicatifpresent3s"}="V";
$dict{"V-infinitif"}="VINF";
$dict{"V-participepassefp"}="VPP";
$dict{"V-participepassefs"}="VPP";
$dict{"V-participepassemp"}="VPP";
$dict{"V-participepassems"}="VPP";
$dict{"V-participepresent"}="VPR";
$dict{"V-subjonctifimparfait3s"}="VS";
$dict{"V-subjonctifpresent1p"}="VS";
$dict{"V-subjonctifpresent1s"}="VS";
$dict{"V-subjonctifpresent2p"}="VS";
$dict{"V-subjonctifpresent3p"}="VS";
$dict{"V-subjonctifpresent3s"}="VS";




while(<>){
    chomp;
    my $line=$_;
    my ($word,$lemma,$pos)=split(/[\t ]/,$line);
    my $new_pos=$dict{$pos};
    print "$word\t$lemma\t$new_pos\n";
}
