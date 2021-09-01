
* This repository contains scripts and datasets associated with a study of benchmarks of bioinformatic software.

- We aim to identify factors that influence the accuracy of software.

```
Directory descriptions:
 bin/        -- contains scripts associated with the data analysis and visualisation
 data/       -- raw data collected during the project
 figures/    -- images generated during the analysis
 manuscript/ -- manuscript files
```

* Work flow:

-- Step 1: literature mining for benchmarking studies with speed and accuracy ranks

--- Download XML files from pubmed for training a literature model and ranking candidate articles

--- Find training data with selected benchmark articles, pubmed query: "15701525[uid] OR 15840834[uid] OR 17062146[uid] OR 17151342[uid] OR 18287116[uid] OR 18793413[uid] OR 19046431[uid] OR 19126200[uid] OR 19179695[uid] OR 20047664[uid] OR 20617200[uid] OR 21113338[uid] OR 21423806[uid] OR 21483869[uid] OR 21525877[uid] OR 21615913[uid] OR 21856737[uid] OR 22132132[uid] OR 22152123[uid] OR 22172045[uid] OR 22287634[uid] OR 22492192[uid] OR 22506536[uid] OR 22574964[uid] OR 23393030[uid] OR 23758764[uid] OR 23842808[uid] OR 24086547[uid] OR 24526711[uid] OR 24602402[uid] OR 24708189[uid] OR 24839440[uid] OR 25198770[uid] OR 25511303[uid] OR 25521762[uid] OR 25574120[uid] OR 25760244[uid] OR 25777524[uid] OR 26220471[uid] OR 26628557[uid] OR 26778510[uid] OR 26862001[uid] OR 27256311[uid] OR 27941783[uid] OR 28052134[uid] OR 28569140[uid] OR 28739658[uid] OR 28808243[uid] OR 28934964[uid] OR 29568413[uid] OR 30329135[uid] OR 30658573[uid] OR 30717772[uid] OR 30936559[uid] OR 31015787[uid] OR 31077315[uid] OR 31080946[uid] OR 31136576[uid] OR 31159850[uid] OR 31324872[uid] OR 31465436[uid] OR 31639029[uid] OR 31874603[uid] OR 31907445[uid] OR 31948481[uid] OR 31984131[uid] OR 32138645[uid] OR 32183840[uid]"
--- Save as:
```data/pubmed_result-training.xml```

--- Find background data for calculating normal word frequencies, pubmed query: "bioinformatics [TIAB] 2010:2015 [dp] (sorted on first author)" or "bioinformatics [TIAB] 2016:2020 [dp] (sorted on first author)"

--- Save as:
```data/pubmed_result-background.xml```

--- Find candidate articles for scoring with benchmark literature model, pubmed query: either 2010:2015 [dp], or 2016:2020 [dp] AND ((bioinformatics OR (computational AND biology)) AND (algorithmic OR algorithms OR biotechnologies OR computational OR kernel OR methods OR procedure OR programs OR software OR technologies)) AND (accuracy OR analysis OR assessment OR benchmark OR benchmarking OR biases OR comparing OR comparison OR comparisons OR comparative OR comprehensive OR effectiveness OR estimation OR evaluation OR metrics OR efficiency OR performance OR perspective OR quality OR rated OR robust OR strengths OR suitable OR suitability OR superior OR survey OR weaknesses OR correctness OR correct OR evaluate OR competing OR competition) AND (complexity OR cputime OR runtime OR walltime OR duration OR elapsed OR fast OR faster OR perform OR performance OR slow OR slower OR speed OR time OR (computational AND resources))

--- Save as (warning, the file is large): 
```pubmed_result-candidate-2010-2020.xml```

--- Generate a ranked list of candidate articles, based upon word usage in titles and abstracts: 
```
cd $PROJECTHOME/speed-vs-accuracy-meta-analysis/data 
../bin/pmArticleScore.pl  -t pubmed_result-training.xml -b pubmed_result-background.xml -c pubmed_result-2010-2015.xml -f background.txt -d checked-pmids.tsv -i ignore.tsv
../bin/pmArticleScore.pl  -t pubmed_result-training.xml -b pubmed_result-background.xml -c pubmed_result-2016-2020.xml -f background.txt -d checked-pmids.tsv -i ignore.tsv
```

--- NB. The XML file sizes are large, and are therefore not included in the repository. They have been made available on FigShare ([https://doi.org/10.6084/m9.figshare.15121818.v1]). 

--- NNB. PubMed no longer supports the output format. 

--- Manually screen the ranked literature articles, add PMIDs for articles that do not meet the selection criteria to "checked-pmids.tsv"

--- Add articles that meet the selection criteria to the training data query and extract relative speed and accuracy ranks for each software tool (https://docs.google.com/spreadsheets/d/14xIY2PHNvxmV9MQLpbzSfFkuy1RlzDHbBOCZLJKcGu8/edit?usp=sharing)

--- Repeat until a sufficient sample size has been collected and/or no new benchmarks are recovered from the literature. 

-- Step 2: join the rank tables with citation, age etc tables, reformat for reading in to R, run R plot and analysis script:

```
cd $PROJECTHOME/speed-vs-accuracy-meta-analysis/data 
../bin/tsv2data.pl 
```

--- Dependencies:

----PERL v5.30.0

----Perl library - Data::Dumper

----R version 4.0.3

----R libraries: MASS,  RColorBrewer, fields, gplots, hash, vioplot

-- Step 3: compile latex documents:

```
cd $PROJECTHOME/speed-vs-accuracy-meta-analysis/manuscripts
pdflatex   manuscript-speed-accuracy.tex && bibtex ./manuscript-speed-accuracy && pdflatex   manuscript-speed-accuracy.tex && pdflatex   manuscript-speed-accuracy.tex && echo "DONE!" ; pdflatex  ./supplementary.tex && bibtex ./supplementary && pdflatex  ./supplementary.tex && pdflatex  ./supplementary.tex   && echo "DONE!"
```


* Files and descriptions:

--- [./README.md](./README.md) - this file 

--- [./LICENSE](./LICENSE) - license file 

-- "bin" directory, the software scripts used to process and visualise the results 

--- [./bin/pmArticleScore.pl](./bin/pmArticleScore.pl) - perl script for parsing PubMed XML files, and scoring the likelihood that a paper matches our selection criteria based upon word frequencies and previous selections for training. 

--- [./bin/prettyPlot.R](./bin/prettyPlot.R) - R script for parsing data files and generating the figures presented in the manuscript. 

--- [./bin/tsv2data.pl](./bin/tsv2data.pl) - perl script for parsing TSV files and converting/joining them to produce required data files for R. 

-- "data" directory

--- [./data/alice-in-wonderland.txt](./data/alice-in-wonderland.txt) - text to find high-frequency English words 

--- [./data/the-hobbit.txt](./data/the-hobbit.txt) - text to find high-frequency English words 

--- [./data/background.txt](./data/background.txt) - text to find high-frequency English words 

--- [./data/articleScores.tsv](./data/articleScores.tsv) - tab-seperated-values: scores for candidate articles, scores reflect the probability that the article is a benchmark that fulfils our selection criteria based upon word frequency analysis. 

--- [./data/checked-pmids.tsv](./data/checked-pmids.tsv) - tab-seperated-values: pubmed IDs for articles that have been checked, but do not meet our selection criteria. These are used as a negative dataset. 

--- [./data/common-words.tsv](./data/common-words.tsv) - tab-seperated-values: a list of common English words, with relative frequencies. 

--- [./data/ignore.tsv](./data/ignore.tsv) - tab-seperated-values: words that should be ignored for the purpose of scoring articles, largely the names of software tools. 

--- [./data/journalInfo2020.tsv](./data/journalInfo2020.tsv) - tab-seperated-values: for each Journal that has published at least one of the benchmarked software tools we provide a count of the number of tools, and the 2020 H5-index from GoogleScholar.   

--- [./data/meanRankAccuracyPerms.tsv](./data/meanRankAccuracyPerms.tsv) - tab-seperated-values: accuracy ranks for permuted rankings, 1,000 for each software tool.   

--- [./data/meanRankSpeedPerms.tsv](./data/meanRankSpeedPerms.tsv) - tab-seperated-values: speed ranks for permuted rankings, 1,000 for each software tool.   

--- [./data/pubmed_result-background.xml](./data/pubmed_result-background.xml) - XML file (place-holder), available from ([https://doi.org/10.6084/m9.figshare.15121818.v1]) -- contains pubmed entries for general bioinformatic (not-benchmarks) articles, used to compute background word frequencies.  

--- [./data/pubmed_result-checked.xml](./data/pubmed_result-checked.xml) - XML file (place-holder), available from ([https://doi.org/10.6084/m9.figshare.15121818.v1]) -- contains pubmed entries for articles that matched our search terms, but did meet our selection criteria

--- [./data/pubmed_result-training.xml](./data/pubmed_result-training.xml) - XML file (place-holder), available from ([https://doi.org/10.6084/m9.figshare.15121818.v1]) -- contains pubmed entries for articles that meet our selection criteria

--- [./data/rawRankSpeedData2005-2020.tsv](./data/rawRankSpeedData2005-2020.tsv) - tab-seperated-values: accuracy and speed ranks for each tool, seperated by benchmark. Tools may appear multiple times, from different benchmarks, summary statistics of these are used in ```meanRankSpeedData.tsv```. 

--- [./data/speed-vs-accuracy-journalIF2005-2015.tsv](./data/speed-vs-accuracy-journalIF2005-2015.tsv) - tab-seperated-values: Information about the different journals. 
```
     1	Journal
     2	Abbreviated Journal Title
     3	Number of methods
     4	Total Cites
     5	2014 Impact Factor
```

--- [./data/speed-vs-accuracy-toolInfo2005-2020.tsv](./data/speed-vs-accuracy-toolInfo2005-2020.tsv) - tab-seperated-values: data for each software tool, the columns correspond to:
```
     1	tool
     2	yearPublished
     3	journal
     4	impactFactor(2017)
     5	Journal H5-index(2017)
     6	totalCitations(2017)
     7	totalCitations(2020)
     8	H-index: (Corresponding author)(2017)
     9	M-index (Corresponding author)(2017)
    10	H (2020)
    11	M (2020)
    12	Versions
    13	Commits (Github)
    14	Contributers (Github)
    15	Github repo
    16	fullCite
```

--- [./data/speed-vs-accuracy-toolRanks2005-2020.tsv](./data/speed-vs-accuracy-toolRanks2005-2020.tsv) - tab-seperated-values: curated ranks extracted from selected benchmark papers. 
```
     1	PubMed ID
     2	Title
     3	accuracySource
     4	accuracyMetric
     5	speedSource
     6	Method
     7	accuracyRank
     8	speedRank
     9	numMethods
    10	Data set (if applicable)
    11	Bias
    12	Acc comment
    13	Speed comment
```

-- "figures" directory

--- [./figures/figure1.pdf](./figures/figure1.pdf) PDF format of manuscript Figure 1. 

--- [./figures/figure1.svg](./figures/figure1.svg) SVG format of manuscript Figure 1. 

--- [./figures/spearmanHeatmap.pdf](./figures/spearmanHeatmap.pdf) Figure 1A


--- [./figures/figure2.pdf](./figures/figure2.pdf) PDF format of manuscript Figure 2. 

--- [./figures/figure2.svg](./figures/figure2.svg) SVG format of manuscript Figure 2. 

Supplementary figures:

--- [./figures/litMiningFlowDiagram-edited.pdf](./figures/litMiningFlowDiagram-edited.pdf) Figure S1. 

--- [./figures/litMiningFlowDiagram.pdf](./figures/litMiningFlowDiagram.pdf) Figure S1. 

--- [./figures/litMiningFlowDiagram.tex](./figures/litMiningFlowDiagram.tex) Figure S1. 

--- [./figures/wordScores.pdf](./figures/wordScores.pdf) Figure S2. 

--- [./figures/supplementary-figures-small.pdf](./figures/supplementary-figures-small.pdf) Figure S3. 

--- [./figures/supplementary-distributions-permuted.pdf](./figures/supplementary-distributions-permuted.pdf) Figure S4. 

--- [./figures/supplementary-figures-pairs.pdf](./figures/supplementary-figures-pairs.pdf) Figure S5.

--- [./figures/spearmanBarplot.pdf](./figures/spearmanBarplot.pdf) Figure S6 (left). 

--- [./figures/spearmanBarplotSpeed.pdf](./figures/spearmanBarplotSpeed.pdf) Figure S6 (right). 

--- [./figures/relAge.pdf](./figures/relAge.pdf) Figure S7

--- [./figures/relAge-SpeedVsAccuracy-heatmap.pdf](./figures/relAge-SpeedVsAccuracy-heatmap.pdf) Figure S7 (left). 

--- [./figures/relAge-speedAcc-jitterPlot.pdf](./figures/relAge-speedAcc-jitterPlot.pdf) Figure S7 (right). 

--- [./figures/numberBenchmarksPerToolBarplot.pdf](./figures/numberBenchmarksPerToolBarplot.pdf) Figure S8 (top)

--- [./figures/numberRealValueFeaturesBarplot.pdf](./figures/numberRealValueFeaturesBarplot.pdf) Figure S8 (middle)

--- [./figures/powerCurves.pdf](./figures/powerCurves.pdf) Figure S8 (bottom)

-- "manuscript" directory, contains a copy of the draft manuscript, supplementary pdf and associated files

--- [./manuscript/manuscript-speed-accuracy.pdf](./manuscript/manuscript-speed-accuracy.pdf) PDF format of the main manuscript. 

--- [./manuscript/manuscript-speed-accuracy.tex](./manuscript/manuscript-speed-accuracy.tex) TEX format of the main manuscript. 

--- [./manuscript/references.bib](./manuscript/references.bib) References, in latex "bib" format. 

--- [./manuscript/supplementary.pdf](./manuscript/supplementary.pdf) PDF format of the supplementary figures and information. 

--- [./manuscript/supplementary.tex](./manuscript/supplementary.tex) TEX format of the supplementary figures and information. 

--- [./manuscript/supp-references.bib](./manuscript/supp-references.bib) Supplementary references, in latex "bib" format. 

--- [./manuscript/supplementary-tables-S1-S7.xlsx](./manuscript/supplementary-tables-S1-S7.xlsx) An excel spreadsheet, supplementary tables S1 to S7

----Table S1: rows for each benchmark, contain accuracy and speed ranks for each tool, as well as the source Figure/Table/Supplement the data was sourced from.

----Table S2: tool information. For each tool we collected the date(s) published, journal(s), impact factors, H5 index, citations, corresponding author(s) H & M index, most recent version, github commits, github contributers, github open issues, github closed issues, github pull requests, github forks, github URL, published reference

----Table S3: journal information: number tools, H5 index from Google Scholar Metrics (2020)

----Table S4: journal articles (2000-2015) ranked by the log-odds score of meeting our inclusion criteria. Classified by whether they are in our training set, have been checked and rejected, or are an unchecked candidate. Just the results for the top 700 articles are shown. 

----Table S5: further journal information: journal name, abbreviated title, number of tools, 2014 impact factor.

----Table S6:  journal articles (2016-2020). Same as Table S4, but restricted to articles published between 2016 and 2020.

----Table S7:  a small sample of manually collected recent candidate articles, along with notes for those that were excluded.




