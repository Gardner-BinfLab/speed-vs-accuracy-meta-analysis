#!/usr/bin/perl 

use warnings;
use strict;

#Read 
open(IN0, "< Does\ bioinformatic\ software\ trade\ speed\ for\ accuracy-\ -\ methodInfo.tsv"); 
while(my $in = <IN0>){
    next if $in =~ /^pubmedID/; 
    chomp($in);
    $in =~  s/\r//g;
    my @in = split(/\t/, $in); 


}
close(IN0);
######################################################################
#echo -ne "accuracyRank\tspeedRank\tnumMethods\n" > data && cut -f 7,8,9 Does\ bioinformatic\ software\ trade\ speed\ for\ accuracy-\ -\ Data.tsv | grep -v N | tr -d "=" | perl -lane 'if(/^acc|^N/ or $F[0] !~ /\d+/ or $F[1] !~ /\d+/){next}elsif(defined($F[2])){$max=$F[2]} printf "%0.2f\t%0.2f\t$max\n", ($F[0]-1)/($max-1), ($F[1]-1)/($max-1); ' >> data
    #  1	pubmedID
    #  2	Title
    #  3	accuracySource
    #  4	accuracyMetric
    #  5	speedSource
    #  6	Method
    #  7	accuracyRank
    #  8	speedRank
    #  9	numMethods
    # 10	Data set (if applicable)
    # 11	Bias


#ADD NEW METHODS TO "methodInfo" SHEET:
#cat Does\ bioinformatic\ software\ trade\ speed\ for\ accuracy-\ -\ methodInfo.tsv | cut -f 1 | sort -d | perl -lane 'print "$F[0] => 1, "' | tr -d "\n"
#cut -f 4 meanRankSpeedData.tsv | perl -lane '%meth = (abyss => 1, apg => 1, barry => 1, bismark => 1, biss => 1, bowtie => 1, bowtie2 => 1, bratbw => 1, bsmap => 1, bsseeker => 1, buckymrbayes => 1, buckymrbayesspa => 1, buckyraxml => 1, bwa => 1, bwasw => 1, caml => 1, camp => 1, ce => 1, celera => 1, clark => 1, clc => 1, clustalomega => 1, clustalw => 1, comus => 1, coprarna => 1, cosine => 1, cro => 1, cufflinks => 1, dali => 1, de => 1, dexseq => 1, dialign => 1, dialign22 => 1, dialignt => 1, dialigntx => 1, diffsplice => 1, diginormvelvet => 1, dima => 1, downhillsimplex => 1, dsgseq => 1, ebi => 1, edenanonstrict => 1, edenastrict => 1, edit => 1, erpin => 1, fa => 1, fasta => 1, fasttree => 1, genometa => 1, gojobori => 1, goldman => 1, gossamer => 1, gottcha => 1, greedyft => 1, gsnap => 1, heidge => 1, hmmer => 1, idbaud => 1, igtpduplossft => 1, inchworm => 1, infernal => 1, intarna => 1, kalign => 1, kbsps => 1, kraken => 1, kthse => 1, leidnl => 1, lmat => 1, lsqman => 1, mafft => 1, mafftfftns => 1, mafftfftns2 => 1, mafftlinsi => 1, maq => 1, mats => 1, megan => 1, metaphlan => 1, metaphyler => 1, method => 1, mgrast => 1, minia => 1, mira => 1, mosaik => 1, motu => 1, mrpml => 1, mrpmp => 1, mrsfast => 1, msinspect => 1, muscle => 1, musclemaxiters => 1, mzmine => 1, ncbiblast => 1, newbler => 1, novoalign => 1, oases => 1, onecodex => 1, openms => 1, pairfold => 1, paralign => 1, pass => 1, phylonetft => 1, piler => 1, poa => 1, pragcz => 1, probalign => 1, probcons => 1, pso => 1, pt => 1, qiime => 1, qsra => 1, ravenna => 1, raxml => 1, raxmllimited => 1, rdiffparam => 1, repeatfinder => 1, repeatgluer => 1, repeatscout => 1, rmap => 1, rnacofold => 1, rnaduplex => 1, rnahybrid => 1, rnaplex => 1, rnaup => 1, rsearch => 1, rsmatch => 1, sa => 1, sam => 1, scro => 1, segemehl => 1, seqgsea => 1, seqman => 1, seqmap => 1, sga => 1, sharcgs => 1, shrimp => 1, sl => 1, smalt => 1, snap => 1, soap => 1, soap2 => 1, soapdenovo => 1, spades => 1, sparse => 1, sparseassembler => 1, spcomp => 1, specarray => 1, spt => 1, srmapper => 1, ssaha => 1, ssake => 1, ssap => 1, ssearch => 1, ssm => 1, sst => 1, st => 1, strcutal => 1, taipan => 1, targetrna => 1, targetrna2 => 1, taxatortk => 1, tcoffee => 1, tmap => 1, transabyss => 1, trinity => 1, upmes => 1, vcake => 1, velvet => 1, wmrpmp => 1, woodhams => 1, wublast => 1, xalign => 1, xcmswithcorrection => 1, xcmswithoutretentiontime => 1, zema => 1); print if ( not defined($meth{$F[0]}) );' | sort -d



#ranks hash is keyed on each method, holds mean normalised speed rank and mean normalised accuracy rank
my ($max,$numBench)=(1,0);
my %ranks;
my %methodCounts;
my ($testId,$pmid,$accuracySource,$accuracyMetric,$speedSource)=("","","","","");

open(IN1, "< Does\ bioinformatic\ software\ trade\ speed\ for\ accuracy-\ -\ Data.tsv"); 
open(UT0, "> rawRankSpeedData.tsv");
print UT0 "testId\taccuracyRank\tspeedRank\tmethod\n";

while(my $in = <IN1>){
    next if $in =~ /^pubmedID/; 
    chomp($in);
    $in =~  s/\r//g;
    my @in = split(/\t/, $in); 
    if (isNumeric($in[8])){
        $max=$in[8];
        $numBench++;
    }
    
    #print "$in[5]/$in[6]:$in[7] max($in[8])\n";
    if(isNumeric($in[6]) && isNumeric($in[7])){
	
	if (defined($in[0]) && isNumeric($in[0]) && $in[0]>0){
	    $pmid = $in[0];
	    ($testId,$accuracySource,$accuracyMetric,$speedSource)=("","","","");
	}

	if (defined($in[2]) && length($in[2])>0){
	    $in[2] =~  s/[ -\/]//g;
	    $accuracySource = $in[2];
	}
	
	if (defined($in[3]) && length($in[3])>0){
	    $in[3] =~  s/[ -\/]//g;
	    $accuracyMetric = $in[3];
	}

	if (defined($in[4]) && length($in[4])>0){
	    $in[4] =~  s/[ -\/]//g;
	    $speedSource = $in[4];
	}
	
        if($pmid && length($accuracySource) && length($accuracyMetric) && length($speedSource) ){
	    $testId = "$pmid:$accuracySource:$accuracyMetric:$speedSource";
	}
	
	#fix method name:
        $in[5] =~  s/[ -=\/0-9]//g;
        $in[5]=lc($in[5]);
        
        if (not defined $ranks{$in[5]}){
            #sum(accuracy) sum(speed) numEntries whichBenchmark
            $ranks{$in[5]} = [0.0,0.0, 0, 0]; 
            $numBench++;
        }
        
        $ranks{$in[5]}[0] += ($in[6]-1)/($max-1); #normalised accuracy rank
        $ranks{$in[5]}[1] += ($in[7]-1)/($max-1); #normalised speed rank
        $ranks{$in[5]}[2]++;
        $ranks{$in[5]}[3]  = $numBench;
	
	#print UT0 "testId\tnormAccuracyRank\tnormSpeedRank\tmethod\n";
	printf UT0 "$testId\t%0.2f\t%0.2f\t$in[5]\n", ($in[6]-1)/($max-1), ($in[7]-1)/($max-1);
    }
    else {
        print "\tWTF:[$in]\n"
    }
}
close(IN1);
close(UT0);

#
open(UT, "> meanRankSpeedData.tsv");
print UT "sumRanks\taccuracyRank\tspeedRank\tmethod\tnumTests\n";

foreach my $meth (keys %ranks){    
    printf UT "%0.2f\t%0.2f\t%0.2f\t%s\t%d\n", $ranks{$meth}[0]/$ranks{$meth}[2] + $ranks{$meth}[1]/$ranks{$meth}[2],  $ranks{$meth}[0]/$ranks{$meth}[2], $ranks{$meth}[1]/$ranks{$meth}[2], $meth, $ranks{$meth}[2];     
}
close(UT);

system("R CMD BATCH --no-save ../bin/prettyPlot.R");

exit(0);

######################################################################
sub isNumeric {
    my $num = shift;
    if (defined($num) && $num=~/^-?\d+\.?\d*$/) { 
	return 1; 
    }
    else {
	return 0;
    }
}

######################################################################
