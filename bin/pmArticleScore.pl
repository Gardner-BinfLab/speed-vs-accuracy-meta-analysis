#!/usr/bin/perl 

#pmArticleScore.pl: 
#      input: 1. list of "training" article from pubmed searches -- these include titles and abstracts
#      input: 2. list of "background" articles from pubmed searches -- these include titles and abstracts (OPTIONAL: plain, further background text -- possibly non-scientific)
#      input: 3. list of "candidate" articles from pubmed searches -- these may contain results similar to the "training" articles

#      output: a ranked list of candidate articles, based upon how well the terms in candidate articles match the training articles

#      Basic approach: 
#                      TRAINING
#                      1. build a dictionary for each training article (hash?)
#                      2. exclude high frequency words (possibly use the further background/non-scientific text for this e.g. Alice in Wonderland & The Hobbit)
#                                 --add a list of words to ignore e.g. methods, sub-disciplines of bioinformatics
#                                 --echo -e "assemblers\nmappers\nmapper\nassembler\ngabenchtob\nclustal\nenme\nsimprot\ncuresim\ncoffee\nseal\nsmidgen\nmsbi\nbrat\nidba\nfirefly\nsoapv\nreas\nhartigan\nhalle\ngsas\ncaap\nsegmod\nencad" | sort -d  > ignore.tsv
#                                 --cat meanRankSpeedData | cut -f 4 | grep -v method | perl -lane 's/[\s+\d+\.,;:\!\?\&\$\@\%\=\|\"\'()\[\]\-\/\_\*]//g; print;' >>ignore.tsv
#
#                      3. score each remaining word based upon how frequently it is used in the training data relative to the background set 
#                         (e.g. log2( [f(t.word)/t.size + d]/[f(b.word)/b.size + d] ) ...  )
#                      
#                      SCORING
#                      1. build a dictionary for each candidate article (hash?)
#                      2. exclude high frequency words
#                      3. score each article using sum( c.word, log( [f(t.word)/t.size + d]/[f(b.word)/b.size + d] ) )

#PUBMED SEARCHES:
#                      TRAINING
#                      less Does\ bioinformatic\ software\ trade\ speed\ for\ accuracy-\ -\ Data.tsv | perl -lane '@F=split(/\t/); print "$F[0]\[uid\] OR " if $F[0]>0' | grep -v 999999999 | tr -d "\n" && echo
#                      17151342[uid] OR 20047664[uid] OR 25198770[uid] OR 21483869[uid] OR 24526711[uid] OR 24839440[uid] OR 17062146[uid] OR 21423806[uid] OR 25511303[uid] OR 20617200[uid] OR 26778510[uid] OR 25521762[uid] OR 23593445[uid] OR 21525877[uid] OR 24708189[uid] OR 18287116[uid] OR 24602402[uid] OR 24086547[uid] OR 18793413[uid] OR 23393030[uid] OR 22132132[uid] OR 15701525[uid] OR 22152123[uid] OR 19046431[uid] OR 25760244[uid] OR 23758764[uid] OR 22172045[uid] OR 25574120[uid] OR 22506536[uid] OR 21856737[uid] OR 21113338[uid] OR 23842808[uid] OR 15840834[uid] OR 19179695[uid] OR 26862001[uid] OR 22492192[uid] OR 21615913[uid] OR 19126200[uid] OR 22574964[uid] OR 22287634[uid] OR 25777524[uid] OR 26220471[uid] OR 31159850[uid] OR 32183840[uid] OR 31136576[uid] OR 32138645[uid] OR 31874603[uid] OR 31159850[uid] OR 31324872[uid] OR 31080946[uid] OR 30936559[uid] OR 31639029[uid] OR 31984131[uid] OR 31465436[uid] OR 30717772[uid] OR 28569140[uid] OR 28808243[uid] OR 31948481[uid] OR 26628557[uid] OR 28052134[uid] OR 28739658[uid] OR 27256311[uid] OR 30658573[uid] OR 28934964[uid] OR 31015787[uid] OR 29568413[uid] 
#                      
#                      BACKGROUND
#                      cat checked-pmids.tsv | perl -lane 'print "$F[0]\[uid\] OR " if $F[0]>0' | tr -d "\n" && echo 
#                      bioinformatics [TIAB] 2013:2015 [dp] (sorted on first author)
#                      cat pubmed_result-checked.xml pubmed_result-background1.xml > pubmed_result-background.xml
#
#                      CANDIDATE
#                      ((bioinformatics OR (computational AND biology)) AND (algorithmic OR algorithms OR biotechnologies OR computational OR kernel OR methods OR procedure OR programs OR software OR technologies)) AND (accuracy OR analysis OR assessment OR benchmark OR benchmarking OR biases OR comparing OR comparison OR comparisons OR comparative OR comprehensive OR effectiveness OR estimation OR evaluation OR metrics OR efficiency OR performance OR perspective OR quality OR rated OR robust OR strengths OR suitable OR suitability OR superior OR survey OR weaknesses OR correctness OR correct OR evaluate OR competing OR competition) AND (complexity OR cputime OR runtime OR duration OR fast OR faster OR perform OR performance OR slow OR slower OR speed OR time OR (computational AND resources))
#
#                      cat the-hobbit.txt alice-in-wonderland.txt > background.txt
#../bin/pmArticleScore.pl  -t pubmed_result-training.xml -b pubmed_result-background.xml -c pubmed_result-2016-2020.xml -f background.txt -d checked-pmids.tsv -i ignore.tsv

use warnings;
use strict;
use Getopt::Long;

my( 
    $backgroundFile,
    $candidateFile,
    $doneFile,
    $fictionFile,
    $trainingFile,
    $ignoreFile,
    $verbose,
    $help
    );

&GetOptions( 
    "b|background=s"   => \$backgroundFile,
    "c|candidate=s"    => \$candidateFile,
    "d|done=s"         => \$doneFile,
    "f|fiction=s"      => \$fictionFile,
    "i|ignore=s"       => \$ignoreFile,
    "t|training=s"     => \$trainingFile,
    "v|verbose"        => \$verbose,
    "h|help"           => \$help
    );

if( $help ) {
    &help();
    exit(1);
}

my @commonWordDict=();
if(defined($ignoreFile)){
    open(IG, "< $ignoreFile"); 
    while(my $ig = <IG>){
	if($ig =~ /(\S+)/){
	    $ig = $1; 
	    next if ($ig =~ /\d/); 
	    next if ($ig !~ /\w/); 
	    $ig = lc($ig); 
	    push(@commonWordDict, $ig); 
	}
    }
}
my $commonWordDict = \@commonWordDict;

if(defined($fictionFile)){
#read some plain text to serve as background for excluding common english words:
    open(BG, "< $fictionFile"); 
    my @background = <BG>;
    close(BG);
    my ($backgroundWordCounts, $backgroundDiWordCounts, $backgroundDiWordCountsTot) = arrayToDictionary(\@background);
#filter words that are used less frequently than 1E-4
    $commonWordDict = findCommonWords($backgroundWordCounts, $commonWordDict, "1E-4");
    printf "Found [%d] common words.\n", scalar(keys %{$commonWordDict}) if (defined($verbose));
}


#read training data, exclude common words, compute word frequencies for the remainder: 
open(TD, "< $trainingFile") or die "[missing training file]: [$trainingFile]\n[$!]"; 
my @training = <TD>;
close(TD);
my %empty;
my $training = pubmedXML2array(\@training, 35000);
printf "Found [%d] training articles.\n", scalar(@{$training}) if (defined($verbose));
my ($trainingWordCounts, $trainingDiWordCounts, $trainingDiWordCountsTot) = arrayToDictionary($training);
my ($trainingWords) = filterCommonWords($trainingWordCounts, $commonWordDict, \%empty); 
$training    = pubmedXML2hash(\@training,   35000);
printf "Found [%d] training words. (common words filtered)\n", scalar(keys %{$trainingWords}) if (defined($verbose));

#read scientific background data, exclude common words, compute word frequencies for the remainder: 
open(SBG, "< $backgroundFile"); 
my @sbackground = <SBG>;
close(SBG);
my $sbackground = pubmedXML2array(\@sbackground, 35000); #limit to the first XXXX pubmed entries
   #ptrToHash            #wordCount
printf "Found [%d] sbackground articles.\n", scalar(@{$sbackground}) if (defined($verbose));
my ($sbackgroundWordCounts, $sbackgroundDiWordCounts, $sbackgroundDiWordCountsTot) = arrayToDictionary($sbackground);
my ($sbackgroundWords) = filterCommonWords($sbackgroundWordCounts, $commonWordDict, $training); 
printf "Found [%d] sbackground words. (common words filtered)\n", scalar(keys %{$sbackgroundWords}) if (defined($verbose));

#for the union of words in $trainingWords and $sbackgroundWords, compute a logodds score:
my $scores   = computeLODS(  $trainingWords, $sbackgroundWords, "1E-5");
my $diScores = computeDiLODS($trainingDiWordCounts, $trainingDiWordCountsTot, $sbackgroundDiWordCounts, $sbackgroundDiWordCountsTot, "1E-5");

#read candidate articles, exclude if they have been manually checked previously, 
#score and rank remaining articles
open(CD, "< $candidateFile"); 
my @candidates = <CD>;
close(CD);
my $candidates = pubmedXML2hash(\@candidates, 1000000); #limit to the first XXXX pubmed entries
printf "Found [%d] candidate articles.\n", scalar(keys %{$training}) if (defined($verbose));

#score the training articles
unlink("articleScores.tsv");
#scoreArticles($training,   $scores, $diScores, 'training', \%empty,    $doneFile); 
              
#score the candidate articles
scoreArticles($candidates, $scores, $diScores, 'candidate', $training, $doneFile); 

exit(0); 


######################################################################
#FUNCTION: parse pubmed xml:
#return: hash{pmid} = "$title\t$abstract";
sub pubmedXML2hash {
    my ($aPtr, $limit) = @_; 
    my @pubmedXML = @{ $aPtr };
    
    my ($title, $abstract) = ("","");
    my %words;
    my $count = 0; 
    foreach my $pmx (@pubmedXML){
	
	if($pmx =~ /<ArticleTitle>(.*)<\/ArticleTitle>/){
	    $title=$1;
	}
	elsif($pmx =~ /<AbstractText.*>(.*)<\/AbstractText>/){
	    $abstract .= "$1 "; 
	}
	elsif($pmx =~ /<ArticleId IdType="pubmed">(\d+)<\/ArticleId>/){
	    $words{$1}= "$title\t$abstract";
	    ($title, $abstract) = ("","");
	    $count++;
	}
	last if $count > $limit;
	
    }
    
    return \%words; 

#<ArticleTitle>Comparisons of computational methods for differential alternative splicing detection using RNA-seq in plant systems.</ArticleTitle>
            # <Abstract>
            #     <AbstractText Label="BACKGROUND" NlmCategory="BACKGROUND">....</AbstractText>
            #     <AbstractText Label="RESULTS" NlmCategory="RESULTS">....</AbstractText>
            #     <AbstractText Label="CONCLUSIONS" NlmCategory="CONCLUSIONS">....</AbstractText>
            # </Abstract>
#<ArticleId IdType="pubmed">25511303</ArticleId>        
}

######################################################################
#FUNCTION: parse pubmed xml, read the top $limit entries:
#return: array = ("[title1] [abstract1]", "[title2] [abstract2]", ...);
sub pubmedXML2array {
    my ($aPtr, $limit) = @_; 
    my @pubmedXML = @{ $aPtr };
    
    my ($title, $abstract) = ("","");
    my @words;
    my $count = 0; 
    foreach my $pmx (@pubmedXML){
	
	if($pmx =~ /<ArticleTitle>(.*)<\/ArticleTitle>/){
	    $title=$1;
	}
	elsif($pmx =~ /<AbstractText.*>(.*)<\/AbstractText>/){
	    $abstract .= "$1 "; 
	}
	elsif($pmx =~ /<ArticleId IdType="pubmed">(\d+)<\/ArticleId>/){
	    #print "pmid[$1]\t[$title]\t[$abstract]\n";
	    push(@words, "$title\t$abstract");
	    ($title, $abstract) = ("","");
	    $count++;
	}	
	last if $count > $limit;	
    }
    
    return \@words; 

}

######################################################################
#FUNCTION: convert an array of word strings to a hash keyed with each word & points to the count of times the word appears in the strings:
#return: (hash{word} = count, totalWordCount);
sub arrayToDictionary {

    my $strings = shift; 
    my @strings = @{ $strings };
    my (%wordCounts, %diWordCounts); 
    my ($wordCount,$diWordCount)=(0,0); 
    my $lastWord;
    foreach my $str (@strings){
	my @str = split(/[\s+\d+\.,;:\!\?\&\$\@\%\=\|\"\'()\[\]\-\/\_\*×]/, $str);
	foreach my $st (@str){
	    next if ($st =~ /\d/); 
	    next if ($st !~ /^\w+$/); 
	    #next if ($st !~ /\w/); 
	    $st = lc($st); 
	    $wordCounts{$st}=0 if (not defined($wordCounts{$st}));
	    $wordCounts{$st}++; 
	    $wordCount++;
	    #####
	    if (defined($lastWord)){
		my $diWord = $lastWord . $st; 
		$diWordCounts{$diWord}=0 if (not defined($diWordCounts{$diWord}));
		$diWordCounts{$diWord}++; 
		$diWordCount++;
	    }
	    $lastWord = $st; 
	}
    }
    
    return (\%wordCounts, \%diWordCounts, $diWordCount); 
}

######################################################################
#FUNCTION: input, pnter to a hash of words & counts, a total wordcount and a frequency threshold
#return: (hash{word} = freq);
sub findCommonWords {
    #$backgroundWordCounts, $totalBackgroundWordCount, "1E-4"
    my ($backgroundWordCounts, $commonWordDict, $freqThreshold) = @_;
    my %backgroundWordCounts = %{ $backgroundWordCounts };
    my %commonWords;
    my $tot=0;
    foreach my $word (keys %backgroundWordCounts){
	$tot += $backgroundWordCounts{$word};
    }
    
    open(CM, "> common-words.tsv");
    foreach my $word (keys %backgroundWordCounts){
	next if not defined($backgroundWordCounts{$word}); 
	my $freq = $backgroundWordCounts{$word}/$tot;
	if ($freq > $freqThreshold){
	    $commonWords{$word} = $freq;
	    printf CM "%0.4f\t$word\n", $freq;
	}
    }
    close(CM);
    
    foreach my $word (@{$commonWordDict}){
	$commonWords{$word}=1.0;
    }
    
    return \%commonWords;
}


######################################################################
#FUNCTION: input: pnter to a hash of words & counts, a total wordcount and a frequency threshold
#return: (hash{word} = freq);
sub filterCommonWords {
    #$trainingWordCounts, $commonWordDict
    my ($words, $commonWordDict, $skipWords) = @_;
    my %words          = %{ $words  };
    my %commonWordDict = %{ $commonWordDict };
    my %skipWords       = %{ $skipWords };
    my %filteredTrainingWords;
    my $sum = 0;
    foreach my $word (keys %words){
	next if (defined($commonWordDict{$word}));
	next if (length($word) < 4);#filter short words
	next if (defined($skipWords{$word}));
        #next if ($words{$word} < 1);#FILTER WORDS ONLY SEEN ONCE -- MAY REVISIT THIS!!!
	$filteredTrainingWords{$word} = $words{$word}; 
	$sum += $words{$word};
    }
    
    foreach my $word (keys %filteredTrainingWords){
	#printf "%0.8f\t%d\t$word\n", $filteredTrainingWords{$word}/$sum, $filteredTrainingWords{$word}; 
	$filteredTrainingWords{$word} = $filteredTrainingWords{$word}/$sum;
    }
    return \%filteredTrainingWords;
}



######################################################################
#FUNCTION: input: 2 hashes of words & frequencies, a "training"  hash and a "background" hash
#return: (hash{word} = log2( [f(t.word) + d]/[f(b.word) + d] ) );
sub computeLODS {
    my ($trainingWords, $backgroundWords, $pseudo) = @_;
    my %trainingWords  = %{ $trainingWords  };
    my %backgroundWords  = %{ $backgroundWords  };
    
    my %lods;
    my $log2 = log(2);

    printf "computing logOdds for [%d] training words & [%d] background words\n", scalar( keys %trainingWords ), scalar( keys %backgroundWords ) if (defined($verbose));

    foreach my $word (keys %trainingWords){
	
	$word =~ s/\s+//g; 
	if ( defined($backgroundWords{$word}) ){
	    $lods{$word} = log( ($trainingWords{$word} + $pseudo)/($backgroundWords{$word} + $pseudo)  )/$log2;
	}
	else {#background word missing:
	    $backgroundWords{$word}=0;
	    $lods{$word} = log( ($trainingWords{$word} + $pseudo)/($pseudo)  )/$log2;
	}
    }
    
    #ZERO COUNTS WERE PROBLEMATIC -- REVISIT, PLAY WITH PSEUDOCOUNTS?
    #Compute LODS for words only found in the background:
    foreach my $word (keys %backgroundWords){
	next if (defined( $trainingWords{$word} )); #already computed
	$word =~ s/\s+//g; 
	$trainingWords{$word}=0;
	$lods{$word} = log( ($pseudo)/($backgroundWords{$word} + $pseudo)  )/$log2;
    }

    open(WUT, "> wordScores.tsv");
    printf WUT "logOdds\ttraingFreq\tbackgroundFreq\n";
    printf "printing logOdds for [%d] words\n", scalar( keys %lods ) if (defined($verbose));
    foreach my $word (sort {$lods{$a} <=> $lods{$b}} keys %lods){
	
	next if (length($word) < 4);#filter short words
	next if ($word =~ /[\s+\d+\.,;:\!\?\&\$\@\%\=\|\"\'()\[\]\-\/\_\*×]/);
	next if ($word !~ /^\w+$/);

	if( defined($word) && defined($lods{$word}) && defined($trainingWords{$word}) && defined($backgroundWords{$word}) ){
	    printf WUT "%0.2f\t%0.10f\t%0.10f\t$word\n", $lods{$word}, $trainingWords{$word}, $backgroundWords{$word};
	}
    }
    close(WUT);
    
    return \%lods; 
}

######################################################################
#FUNCTION: input: 2 hashes of di-words & totals, a "training" hash and a "background" hash
#return: (hash{word} = log2( [f(t.diWord) + d]/[f(b.diWord) + d] ) );
sub computeDiLODS {
    my ($trainingDiWords, $trainingTot, $backgroundDiWords, $backgroundTot, $pseudo) = @_;
    my %trainingDiWords  = %{ $trainingDiWords  };
    my %backgroundDiWords  = %{ $backgroundDiWords  };
    
    my %lods;
    my $log2 = log(2);
    foreach my $word (keys %trainingDiWords){
	if ( defined($backgroundDiWords{$word}) ){
	    $lods{$word} = log( ($trainingDiWords{$word}/$trainingTot + $pseudo)/($backgroundDiWords{$word}/$backgroundTot + $pseudo)  )/$log2;
	}
	else {#background word missing:
	    $backgroundDiWords{$word}=0;
	    $lods{$word} = log( ($trainingDiWords{$word}/$trainingTot + $pseudo)/($pseudo)  )/$log2;
	}
    }
    
    #Compute LODS for words only found in the background:
    foreach my $word (keys %backgroundDiWords){
	next if (defined( $trainingDiWords{$word} )); #already computed
	$trainingDiWords{$word}=0;
	$lods{$word} = log( ($pseudo)/($backgroundDiWords{$word}/$backgroundTot + $pseudo)  )/$log2;
    }

    open(WUT, "> diWordScores.tsv");
    printf WUT "logOdds\ttraingFreq\tbackgroundFreq\n";
    foreach my $word (sort {$lods{$a} <=> $lods{$b}} keys %lods){
	if( defined($word) && defined($lods{$word}) && defined($trainingDiWords{$word}) && defined($backgroundDiWords{$word}) ){
	    printf WUT "%0.2f\t%0.4f\t%0.4f\t$word\n", $lods{$word}, $trainingDiWords{$word}/$trainingTot, $backgroundDiWords{$word}/$backgroundTot;
	}
    }
    close(WUT);
    
    return \%lods; 
}





######################################################################
#FUNCTION: input: a hash of candidate article titles & abstracts, key'ed on PMIDs, a hash of word log odd scores  
#return: prints a sum of LOD scores for each title abstract  
sub scoreArticles {
    
    my ($candidates, $scores, $diScores, $tag, $training, $doneFile) = @_;
    my %candidates = %{ $candidates };
    my %scores     = %{ $scores     };
    my %diScores     = %{ $diScores };
    my %training   = %{ $training   };
    
    my ($sumScores,$sumDiScores) = (0.00000,0.00000);
    
    my %checked;
    
    if (defined($doneFile) && -s $doneFile){
	open(CH, "< $doneFile");
	while ( my $ch = <CH> ){
	    if ($ch =~ /(\d+)/){
		$checked{$1}=1;
	    }
	}
	close(CH);
    }
    
    open(AUT, ">> articleScores.tsv");
    print AUT "articleScore(monoWord)\tarticleScore(diWord)\tPMID\tLabel\tTitle\tAbstract\n";
    foreach my $pmid (keys %candidates){
	next if (not defined($candidates{$pmid}));
 	my @str = split(/[\s+\d+\.,;:\!\?\&\$\@\%\=\|\"\'()\[\]\-\/\_\*×]/, $candidates{$pmid});
	my $lastWord;
	foreach my $word (@str){
	    next if ($word !~ /^\w+$/);
	    if( defined($scores{$word}) ){
		$sumScores += $scores{$word};
	    }
	    
	    if( defined($lastWord) ){
		my $diWord = $lastWord . $word; 
		if( defined($diScores{$diWord}) ){
		    $sumDiScores += $diScores{$diWord};
		}
	    }
	    $lastWord = $word; 
	}
	my $newTag = $tag;
	$newTag = 'checked'  if ( defined( $checked{$pmid}) );
	$newTag = 'training' if ( defined($training{$pmid}) );
	if ( $sumScores>0 && $sumDiScores>0 ){
	    printf AUT "%0.4f\t%0.4f\thttp://www.ncbi.nlm.nih.gov/pubmed/$pmid\t$newTag\t%s\n", $sumScores, $sumDiScores, $candidates{$pmid}; 
	}
	($sumScores,$sumDiScores) = (0.00000,0.00000);
    }
    close(AUT);
    return 1;
}

######################################################################

######################################################################
sub help {
    print STDERR <<EOF;
    
$0: ranks a list of candidate pubmed articles based upon they match a set of training (true) 
    background (similar but generally false) articles.

      input: 1. list of "training" article from pubmed searches -- these include titles and abstracts
      input: 2. list of "background" articles from pubmed searches -- these include titles and abstracts
      input: 3. list of "candidate" articles from pubmed searches -- these may contain results similar to the "training" articles
      input: 4. text from a work or works of fiction -- used to identify high-frequency words (which are excluded from the scoring) 
      
      output: a ranked list of candidate articles, based upon how well the terms in candidate articles match the training articles

Usage: $0 -t pubmed_result-training.xml -b pubmed_result-background.xml -c pubmed_result-candidate.xml -f background.txt

Options:       --h                  show this help

               -b|--background      <str> a pubmed XML file of "background" articles, from the field, but unlikely to be of interest for the current study.
               -c|--candidate       <str> a pubmed XML file of "candidate" articles, candidate articles may be of interest for the current study.
	       -d|--done            <str> a file containing a list of PMIDs for articles from the "candidates" that have been checked.  
               -f|--fiction         <str> a work of fiction or other text. Used to identify high-frequency words that are excluded from the scoring. [OPTIONAL] 
               -t|--training        <str> a pubmed XML file of "training" articles, training articles are of interest for the current study.
               -v|--verbose         Print lots of stuff

Examples:
../bin/pmArticleScore.pl  -t pubmed_result-training.xml -b pubmed_result-background.xml -c pubmed_result-candidate.xml -f background.txt -d checked-pmids.tsv

TODO:
generalise the "order" of the model e.g. single word freqs, double word freqs, ... 

EOF
}




