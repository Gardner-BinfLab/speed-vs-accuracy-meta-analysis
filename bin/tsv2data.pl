#!/usr/bin/perl 

use warnings;
use strict;
use Data::Dumper;

###################################
#Parse tool info (e.g. age, impact, etc.):
my @methodInfoKeys = qw(yearPublished H5 citations hindex mindex version commits contributors issues issuesFracOpen pullrequests forks); #IF 
my $methodInfoPtr = extractMethodInfo("speed-vs-accuracy-toolInfo2005-2020.tsv", "journalInfo2020.tsv"); #, \%methodInfo); 
my %methodInfo = %{$methodInfoPtr};
    
###################################
#Parse tool ranks 
#ranks hash: key: methodName, 0: sum of normalised accuracy rank, 1: sum of normalised speed rank, 2: number of times method benchmarked, 3: benchmark number
my (%ranks, %benchAccuracy, %benchSpeed, %benchMethod);
readRanks("speed-vs-accuracy-toolRanks2005-2020.tsv", "rawRankSpeedData2005-2020.tsv", \%ranks, \%benchAccuracy, \%benchSpeed, \%benchMethod); 

#my (%ranks2015, %benchAccuracy2015, %benchSpeed2015, %benchMethod2015);
#readRanks("speed-vs-accuracy-toolRanks2016-2020.tsv", "rawRankSpeedData2016-2020.tsv", \%ranks2015, \%benchAccuracy2015, \%benchSpeed2015, \%benchMethod2015); 



###################################
#Permutation test ranks! Compute relative ages & cites. 
my $numPermutations = 1000;
my (%ranksPerms,%datesRelRanks, %citesRelRanks);
permutationsRelativeRanks($numPermutations, \%benchMethod, \%ranksPerms,\%datesRelRanks, \%citesRelRanks, \%benchAccuracy, \%benchSpeed, \%methodInfo);
#my (%ranksPerms2015,%datesRelRanks2015, %citesRelRanks2015);
#permutationsRelativeRanks($numPermutations, \%benchMethod2015, \%ranksPerms2015,\%datesRelRanks2015, \%citesRelRanks2015, \%benchAccuracy2015, \%benchSpeed2015, \%methodInfo);

###################################
#Print mean speed/accuracy relative rank, and permutation results for each method:
printResults("meanRankSpeedData.tsv", "meanRankAccuracyPerms.tsv", "meanRankSpeedPerms.tsv", \%ranks, \%ranksPerms, \@methodInfoKeys, \%methodInfo, \%datesRelRanks, \%citesRelRanks);
#printResults("meanRankSpeedData2015.tsv", "meanRankAccuracyPerms2015.tsv", "meanRankSpeedPerms2015.tsv", \%ranks2015, \%ranksPerms2015, \@methodInfoKeys, \%methodInfo, \%datesRelRanks2015, \%citesRelRanks2015);



###################################
#Make all the figures, do some statistics: 
system("R CMD BATCH --no-save ../bin/prettyPlot.R");

exit(0);

######################################################################
#extractMethodInfo: read in publication, age etc. features for the tools:
sub extractMethodInfo {
    my ($infoFile, $journalInfoFile)= @_; #, $methodInfoPtr) = @_;

# head -n 1 journalInfo2020.tsv | tr "\t" "\n" | nl
#      1	numTools
#      2	h5-index (2020)
#      3	journal
    my %journalInfo;
    open(IN, "< $journalInfoFile") or die "FATAL: failed to open [$journalInfoFile].\n[$!]";  
    while(my $in = <IN>){
	next if $in =~ /^numTools/; 
	$in =~ s/[^[:ascii:]]//g; #strip off unicode chars
	chomp($in);
	$in =~  s/\r//g;
	my @in = split(/\t/, $in); 
	
	$in[2] =~ s/\s+//g;
	$in[2] =~ tr/[A-Z]/[a-z]/; 
	$journalInfo{$in[2]}=$in[1];
	#print "journalInfo[$in[2]]=[$in[1]];\n";
    }
    
    
# head -n 1 speed-vs-accuracy-toolInfo2005-2020.tsv | tr "\t" "\n" | nl
#      1	tool
#      2	yearPublished
#      3	journal
#      4	impactFactor(2017)
#      5	Journal H5-index(2017)
#      6	totalCitations(2017)
#      7	totalCitations(2020)
#      8	H-index: (Corresponding author)(2017)
#      9	M-index (Corresponding author)(2017)
#     10	H (2020)
#     11	M (2020)
#     12	Versions
#     13	Commits (Github)
#     14	Contributers (Github)
#     15	Github repo
#     16	fullCite


    #For each tool collect:
    #yearPublished jH5 citations hindex mindex version commits contributors
    my %methodInfo; #  = %{$methodInfoPtr}; 
    open(IN0, "< $infoFile") or die "FATAL: failed to open [$infoFile].\n[$!]";  #Does\ bioinformatic\ software\ trade\ speed\ for\ accuracy-\ -\ methodInfo.tsv
    while(my $in = <IN0>){
	next if $in =~ /^tool/; 
	$in =~ s/[^[:ascii:]]//g; #strip off unicode chars
	chomp($in);
	$in =~  s/\r//g;
	my @in = split(/\t/, $in); 
	
	if(defined($in[0]) && length($in[0]) > 1){

	    #Year of first publication
	    if(defined($in[1])){
		my @yrs = split(/;\s*/, $in[1]);
		$methodInfo{$in[0]}{'yearPublished'}='NA';
		$methodInfo{$in[0]}{'yearPublished'}=minA(@yrs) if ($in[1] !~ 'NA'); #using the first publication
		#printf "[$in[0]]\tyrsArray[@yrs]\tyearStr[$in[1]]\tminA[%d]\n", minA(@yrs);
	    }

	    #Look up H5 index for journals
	    if(defined($in[2])){
		#print "$in[2]\n";
		$in[2]=~s/\s+//g;
		$in[2]=~tr/[A-Z]/[a-z]/;
		my @journals = split(/;/, $in[2]);
		my @h5;
		foreach my $jnl (@journals){
		    if (defined( $journalInfo{$jnl} )){
			push( @h5, $journalInfo{$jnl});
		    }
		    else {
			push( @h5, 'NA');
			print "No journal H5 found for [$jnl]\n";
		    }
		}
		$methodInfo{$in[0]}{'H5'}='NA';
		$methodInfo{$in[0]}{'H5'}=maxA(@h5); #using the highest impact factor
	    }
	    
	    #Total citations
	    if(defined($in[6])){
		my @citations = split(/;\s*/, $in[6]);
		$methodInfo{$in[0]}{'citations'}='NA';
		$methodInfo{$in[0]}{'citations'}=sumA(@citations) if ($in[6] !~ 'NA'); #using the sum of cites (some cites counted twice due to multiple cites from 1 paper)
	    }

	    #Corresponding author's H-index:
	    if(defined($in[9])){
		$methodInfo{$in[0]}{'hindex'}='NA';
		
		if ($in[9] !~ 'NA'){
		    my @hs = split(/;\s*/, $in[9]);
		    for (my $i=0; $i<scalar(@hs); $i++){
			if($hs[$i] =~ /:(\d+)/){
			    $hs[$i] = $1;
			}
			else{
			    print "WARNING: MALFORMED H-index cell for [$in[0]]\n";
			    $hs[$i] = 0;
			}
		}
		    $methodInfo{$in[0]}{'hindex'}=maxA(@hs); #using the highest H-author
		}
	    }

	    #Corresponding author's M-index
	    if(defined($in[10])){
		$methodInfo{$in[0]}{'mindex'}='NA';
		if ($in[10] !~ 'NA'){
		    my @ms = split(/;\s*/, $in[10]);
		    for (my $i=0; $i<scalar(@ms); $i++){
			if($ms[$i] =~ /:(\d+\.\d+)/ or $ms[$i] =~ /:(\d+)/ or $ms[$i] =~ /(\d+\.\d+)/){
			    $ms[$i] = $1;
			}
			else{
			print "WARNING: MALFORMED M-index cell for [$in[0]]\n";
			$ms[$i] = 0;
			}
		    }
		    $methodInfo{$in[0]}{'mindex'}=maxA(@ms); #using the highest M-author
		}
	    }

	    #Software version 
	    if(defined($in[11])){
		$methodInfo{$in[0]}{'version'}=1.0; #minimum version set to 1.0. 
		$methodInfo{$in[0]}{'version'}= $in[11]      if (isNumeric($in[11])); 
	    }

	    #Commits to github
	    if(defined($in[12])){
		$methodInfo{$in[0]}{'commits'}='NA';
		$methodInfo{$in[0]}{'commits'}= $in[12]      if (isNumeric($in[12])); 
	    }

	    #Contributors to github
	    if(defined($in[13])){
		$methodInfo{$in[0]}{'contributors'}='NA';
		$methodInfo{$in[0]}{'contributors'}= $in[13] if (isNumeric($in[13])); 
	    }
	    
	}
	
    }
    close(IN0);

    #print "methodInfoHash1||"      . Dumper(\%methodInfo)      . "||\n";

    return \%methodInfo; 
}
######################################################################
sub readRanks {


    
    #echo -ne "accuracyRank\tspeedRank\tnumMethods\n" > data && cut -f 7,8,9 speed-vs-accuracy-toolRanks2005-2015.tsv | grep -v N | tr -d "=" | perl -lane 'if(/^acc|^N/ or $F[0] !~ /\d+/ or $F[1] !~ /\d+/){next}elsif(defined($F[2])){$max=$F[2]} printf "%0.2f\t%0.2f\t$max\n", ($F[0]-1)/($max-1), ($F[1]-1)/($max-1); ' >> data
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
    
    my ($rankFile, $rawRankSpeedDataFile, $ranks, $benchAccuracy, $benchSpeed, $benchMethod) = @_; 
    
    open(IN1, "< $rankFile") or die "FATAL: failed to open [$rankFile].\n[$!]"; 
    open(UT0, "> $rawRankSpeedDataFile");
    print UT0 "testId\taccuracyRank\tspeedRank\tmethod\n";
    my ($testId,$pmid,$accuracySource,$accuracyMetric,$speedSource)=("","","","","");
    my ($max,$numBench)=(1,0);
    
    while(my $in = <IN1>){
	next if $in =~ /^pubmed/i; 
	$in =~ s/[^[:ascii:]]//g; #strip those fucking unicode chars
	chomp($in);
	$in =~  s/\r//g; #Grrrrr
	my @in = split(/\t/, $in); 
	#fix method name:
	$in[5] =~  s/[ -=\/0-9]//g;
	$in[5]=lc($in[5]);
	
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

	    #Normalise ranks, values lie between 0.0 & 1.0, 0.0 = low acc/speed (highest rank)
	    my $accNorm = 1-($in[6]-1)/($max-1);
	    my $spdNorm = 1-($in[7]-1)/($max-1);
	    push(@{$benchMethod->{$testId}}, $in[5]);
	    push(@{$benchAccuracy->{$testId}}, $accNorm);
	    push(@{$benchSpeed->{   $testId}}, $spdNorm);
	    
	    if (not defined $ranks->{$in[5]}){
		#sum(accuracy) sum(speed) numEntries whichBenchmark
		$ranks->{$in[5]} = [0.0,0.0, 0, 0]; 
		$numBench++;
	    }
	    
	    #ranks: key: methodName, 0: sum of normalised accuracy rank, 1: sum of normalised speed rank, 2: number of times method benchmarked, 3: benchmark number
	    $ranks->{$in[5]}[0] += $accNorm; #normalised accuracy rank
	    $ranks->{$in[5]}[1] += $spdNorm; #normalised speed rank
	    $ranks->{$in[5]}[2]++;
	    $ranks->{$in[5]}[3]  = $numBench;
	    
	    
	    #            testId   normAccuracyRank   normSpeedRank   method
	    printf UT0 "$testId\t%0.3f\t%0.3f\t$in[5]\n", $accNorm, $spdNorm;
	}
	}
	else {
	    print "\tWTF:[$in]\n"
	}
	
	
	
    }
    close(IN1);
    close(UT0);
    
    return 0; #(\%ranks, \%benchAccuracy, \%benchSpeed, \%benchMethod);     
    
}

######################################################################
#permutationsRelativeRanks: 
sub permutationsRelativeRanks {

    my ($numPermutations, $benchMethod, $ranksPerms, $datesRelRanks, $citesRelRanks, $benchAccuracy, $benchSpeed, , $methodInfo) = @_;
    #my (%benchMethod, %ranksPerms, %datesRelRanks, %citesRelRanks, %benchAccuracy, %benchSpeed) = (%{$benchMethod}, %{$ranksPerms}, %{$datesRelRanks}, %{$citesRelRanks}, %{$benchAccuracy}, %{$benchSpeed});
    
    #my (%benchAccuracyPerms, %benchSpeedPerms);
    foreach my $bench (keys %{$benchMethod}){ 
	my %dates=(); 
	my %citations=(); 
	
	#GENERATE XXX PERMUTATIONS OF ACCURACY & SPEED RANKS
	for (my $p=0; $p<$numPermutations; $p++){
	    
	    my $pAccP = permuteA( @{$benchAccuracy->{$bench}} ); 
	    my $pSpdP = permuteA( @{$benchSpeed->{$bench}} ); 
	    ##THESE ARE UNUSED LATER, NOT NEEDED?
	    #my $testIdP = $bench . ":$p";
	    #push(@{$benchAccuracyPerms{$testIdP}}, @{$pAccP});  
	    #push(@{$benchSpeedPerms{   $testIdP}}, @{$pSpdP});
	    
	    my $cnt=0;
	    foreach my $meth (@{$benchMethod->{$bench}}){
		$ranksPerms->{"$meth:$p"}[0] += $pAccP->[$cnt]; #normalised accuracy rank
		$ranksPerms->{"$meth:$p"}[1] += $pSpdP->[$cnt]; #normalised speed rank
		$cnt++;
	    }
	    
	}
	
	
	#COMPUTE RELATIVE AGE & CITATION COUNT FOR EACH METHOD IN EACH BENCHMARK/TEST:
	#This block is awful. Problem is methods may have ages, but not cites, ... 
	#need to treat each stat seperately. Or learn how to deal with tonnes of missing values:
	foreach my $meth (@{$benchMethod->{$bench}}){
	    
	    
	    if(defined($methodInfo->{$meth}{'yearPublished'} )){
		$dates{$meth} = $methodInfo->{$meth}{'yearPublished'} if (isNumeric($methodInfo->{$meth}{'yearPublished'})); 
	    }
	    
	    if(defined($methodInfo->{$meth}{'citations'} )){
		$citations{$meth} = $methodInfo->{$meth}{'citations'} if (isNumeric($methodInfo->{$meth}{'citations'})); 
	    }
	    
	    #normaliseH returns a hash
	    my $relRankDates = normaliseH( \%dates ); 
	    foreach my $meth (keys %{$relRankDates}){ 
		push(@{ $datesRelRanks->{$meth} }, $relRankDates->{$meth} );
	    }
	    
	    my $relRankCites = normaliseH( \%citations ); 
	    foreach my $meth (keys %{$relRankCites}){ 
		push(@{ $citesRelRanks->{$meth} }, $relRankCites->{$meth} );
	    }
	}
    }
    
    return 0;
}

######################################################################
#printResults
#printResults("meanRankSpeedData.tsv", "meanRankAccuracyPerms.tsv", "meanRankSpeedPerms.tsv", \%ranks, \%ranksPerms, \@methodInfoKeys, \%methodInfo, \%datesRelRanks, \%citesRelRanks);
sub printResults {

    my ($meanRankSpeedDataFile, $meanRankAccuracyPermsFile, $meanRankSpeedPermsFile, $ranks, $ranksPerms, $methodInfoKeys, $methodInfo, $datesRelRanks, $citesRelRanks) = @_;
    
    #my (%ranks, %ranksPerms, @methodInfoKeys, %methodInfo, %datesRelRanks, %citesRelRanks) = (%{$ranksPtr}, %{$ranksPermsPtr}, @{$methodInfoKeysAPtr}, %{$methodInfoPtr}, %{$datesRelRanksPtr}, %{$citesRelRanksPtr});
    
    open( UT,   "> $meanRankSpeedDataFile"    );
    open( UTPA, "> $meanRankAccuracyPermsFile");
    open( UTPS, "> $meanRankSpeedPermsFile"   );
    
    my $methInfo = join("\t", @{$methodInfoKeys}); 
    #yearPublished H5 citations hindex mindex version commits contributors
    print UT   "sumRanks\taccuracyRank\tspeedRank\tmethod\tnumTests\t$methInfo\trelAge\trelCites\n";
    print UTPA "method\tpermutation\taccuracyRank\n";
    print UTPS "method\tpermutation\tspeedRank\n";


    #print "ranksHash||"      . Dumper($ranks)      . "||\n";
    #print "methodInfoHash||" . Dumper($methodInfo) . "||\n";

    
    foreach my $meth (sort keys %{$ranks}){ 
	#ranks: key: methodName, 0: sum of normalised accuracy rank, 
        #                        1: sum of normalised speed rank, 
        #                        2: number of times method benchmarked, 
        #                        3: benchmark number/ID

	printf UT "%0.3f\t%0.3f\t%0.3f\t%s\t%d", $ranks->{$meth}[0]/$ranks->{$meth}[2] + $ranks->{$meth}[1]/$ranks->{$meth}[2],  $ranks->{$meth}[0]/$ranks->{$meth}[2], $ranks->{$meth}[1]/$ranks->{$meth}[2], $meth, $ranks->{$meth}[2];
	
	#PRINT PERMUTATIONS OF ACCURACY & SPEED RANKS
	for (my $p=0; $p<$numPermutations; $p++){
	    printf UTPA "%s\t%d\t%0.3f\n", "$meth:$p", $p, $ranksPerms->{"$meth:$p"}[0]/$ranks->{$meth}[2];
	    printf UTPS "%s\t%d\t%0.3f\n", "$meth:$p", $p, $ranksPerms->{"$meth:$p"}[1]/$ranks->{$meth}[2];
	}
	
	foreach my $methInfo (@{$methodInfoKeys}){
	    if(not defined($methodInfo->{$meth}{$methInfo} )){
		print "[$meth] needs [$methInfo]!\n"; 
		$methodInfo->{$meth}{$methInfo} = 'NA';
	    }
	    printf UT "\t%s", $methodInfo->{$meth}{$methInfo};	
	}
	
	if( defined($datesRelRanks->{$meth}) && scalar(@{ $datesRelRanks->{$meth} }) ){
	    printf UT "\t%0.2f", meanA(@{ $datesRelRanks->{$meth} });
	}
	else{
	    printf UT "\tNA"
	}
	
	if( defined($citesRelRanks->{$meth}) && scalar(@{ $citesRelRanks->{$meth} }) ){
	    printf UT "\t%0.2f", meanA(@{ $citesRelRanks->{$meth} });
	}
	else{
	    printf UT "\tNA"
	}
	printf UT "\n";
    }
    close(UT);
    close(UTPA);
    close(UTPS);
    
    return 0;
    
}


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
#Max and Min
#max
sub max {
    return $_[0] if @_ == 1;
    return $_[0] if $_[1] eq 'NA';
    return $_[1] if $_[0] eq 'NA';
    $_[0] > $_[1] ? $_[0] : $_[1]
}

#min
sub min {
    return $_[0] if @_ == 1;
    return $_[0] if $_[1] eq 'NA';
    return $_[1] if $_[0] eq 'NA';
    $_[0] < $_[1] ? $_[0] : $_[1]
}
######################################################################
#Max and Min for arrays:
#max
sub maxA {
    my $max = $_[0];
    foreach my $a (@_){
	$max = max($max, $a) if isNumeric($a);
    }
    return $max;
}

#min
sub minA {
    my $min = $_[0];
    foreach my $a (@_){
	$min = min($min, $a) if isNumeric($a);
    }
    return $min;
}

######################################################################
#sum values in an array
sub sumA {
    my $sum = 0;
    foreach my $a (@_){
	$sum += $a if isNumeric($a);
    }
    return $sum;
}

######################################################################
#mean of values in an array
sub meanA {
    my ($sum,$count) = (0,0);
    foreach my $a (@_){
	if ( isNumeric($a) ){
	    $sum += $a;
	    $count++;
	}
    }
    return ($sum/$count);
}

######################################################################
#return normalised values in a hash: values to lie between 0 & 1. 
sub normaliseH {
    my $h = shift;
     my (@keys, @vals);
    foreach my $k (keys %{$h}){
	push(@keys, $k);
	push(@vals, $h->{$k});
    }
    my $vals = normaliseA(@vals);
    
    my %hash; 
    for(my $i=0; $i < scalar(@keys); $i++){
	$hash{ $keys[$i] } = $vals->[$i];
    }
    return \%hash;
}

######################################################################
#return normalise values in an array: normalised to lie between 0 & 1. 
sub normaliseA {
    my @a = @_; 
    my $minVal = minA(@a);
    #subtract min val.
    for(my $i = 0; $i<scalar(@a); $i++){
	$a[$i] = ($a[$i] - $minVal) if isNumeric($a[$i]);
    }
    
    my $maxVal = maxA(@a);
    #divide by max val.
    for(my $i = 0; $i<scalar(@a); $i++){
	$a[$i] = ($a[$i]/$maxVal) if (isNumeric($a[$i]) && $maxVal != 0);
    }
    
    return \@a;
}

######################################################################
#return permuted values in an array -- use Fisher - Yates: 
sub permuteA {
    my @a = @_; 
    my $length = scalar(@a); 
    
    #http://docstore.mik.ua/orelly/perl/cookbook/ch04_18.htm
    for (my $i = 0; $i < $length; $i++){
	my $r = int(rand($i + 1)); ## had been using $length. Apparently this is biased! 
	next if ($i == $r);
	($a[$i], $a[$r]) = ($a[$r], $a[$i]); 
    }
    
    return \@a; 
}


