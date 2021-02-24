
* This repository contains scripts and datasets associated with a study of benchmarks of bioinformatic software.

- We aim to identify factors that influence the accuracy of software.

```
Directory descriptions:
 bin/        -- contains scripts associated with the data analysis
 data/       -- raw data collected during the project
 figures/    -- images generated during the analysis
 manuscript/ -- manuscript files
```

* Work flow:

-- Step 1: literature mining for benchmarking studies with speed and accuracy ranks

--- Download XML files from pubmed for training a literature model and ranking candidate articles

--- Find training data with selected benchmark articles, pubmed query: "17151342[uid] OR 20047664[uid] OR 25198770[uid] OR 21483869[uid] OR 24526711[uid] OR 24839440[uid] OR 17062146[uid] OR 21423806[uid] OR 25511303[uid] OR 20617200[uid] OR 26778510[uid] OR 25521762[uid] OR 23593445[uid] OR 21525877[uid] OR 24708189[uid] OR 18287116[uid] OR 24602402[uid] OR 24086547[uid] OR 18793413[uid] OR 23393030[uid] OR 22132132[uid] OR 15701525[uid] OR 22152123[uid] OR 19046431[uid] OR 25760244[uid] OR 23758764[uid] OR 22172045[uid] OR 25574120[uid] OR 22506536[uid] OR 21856737[uid] OR 21113338[uid] OR 23842808[uid] OR 15840834[uid] OR 19179695[uid] OR 26862001[uid] OR 22492192[uid] OR 21615913[uid] OR 19126200[uid] OR 22574964[uid] OR 22287634[uid] OR 25777524[uid] OR 26220471[uid] OR 31159850[uid] OR 32183840[uid] OR 31136576[uid] OR 32138645[uid] OR 31874603[uid] OR 31159850[uid] OR 31324872[uid] OR 31080946[uid] OR 30936559[uid] OR 31639029[uid] OR 31984131[uid] OR 31465436[uid] OR 30717772[uid] OR 28569140[uid] OR 28808243[uid] OR 31948481[uid] OR 26628557[uid] OR 28052134[uid] OR 28739658[uid] OR 27256311[uid] OR 30658573[uid] OR 28934964[uid] OR 31015787[uid] OR 29568413[uid] "

data/pubmed_result-training.xml

--- Find background data for calculating normal word frequencies, pubmed query: "bioinformatics [TIAB] 2010:2015 [dp] (sorted on first author)" or "bioinformatics [TIAB] 2016:2020 [dp] (sorted on first author)"

data/pubmed_result-background.xml

--- Find candidate articles for scoring with benchmark literature model, pubmed query: either 2010:2015 [dp], or 2016:2020 [dp] AND  ((bioinformatics OR (computational AND biology)) AND (algorithmic OR algorithms OR biotechnologies OR computational OR kernel OR methods OR procedure OR programs OR software OR technologies)) AND (accuracy OR analysis OR assessment OR benchmark OR benchmarking OR biases OR comparing OR comparison OR comparisons OR comparative OR comprehensive OR effectiveness OR estimation OR evaluation OR metrics OR efficiency OR performance OR perspective OR quality OR rated OR robust OR strengths OR suitable OR suitability OR superior OR survey OR weaknesses OR correctness OR correct OR evaluate OR competing OR competition) AND (complexity OR cputime OR runtime OR walltime OR duration OR elapsed OR fast OR faster OR perform OR performance OR slow OR slower OR speed OR time OR (computational AND resources))

pubmed_result-2010-2015.xml

pubmed_result-2016-2020.xml

```
cd $PROJECTHOME/speed-vs-accuracy-meta-analysis/data 
../bin/pmArticleScore.pl  -t pubmed_result-training.xml -b pubmed_result-background.xml -c pubmed_result-2010-2015.xml -f background.txt -d checked-pmids.tsv -i ignore.tsv
../bin/pmArticleScore.pl  -t pubmed_result-training.xml -b pubmed_result-background.xml -c pubmed_result-2016-2020.xml -f background.txt -d checked-pmids.tsv -i ignore.tsv
```

--- Manually screen the ranked literature articles, add PMIDs for articles that do not meet the selection criteria to "checked-pmids.tsv"

--- Add articles that meet the selection criteria to the training data query and extract relative speed and accuracy ranks for each software tool (https://docs.google.com/spreadsheets/d/14xIY2PHNvxmV9MQLpbzSfFkuy1RlzDHbBOCZLJKcGu8/edit?usp=sharing)

--- Repeat until a sufficient sample size has been collected and/or no new benchmarks are recovered from the literature. 


-- Step 2: join the rank tables with citation, age etc tables, reformat for reading in to R, run R plot and analysis script:

```
cd $PROJECTHOME/speed-vs-accuracy-meta-analysis/data 
../bin/tsv2data.pl 
```


