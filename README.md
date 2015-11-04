
This repository contains scripts and datasets associated with a study of benchmarks of bioinformatic software. 

We aim to test:

a) is the speed and accuracy of software inversely correlated?

b) are any of journal impact, citation count, recency or PI-fame predictive of method speed+accuracy?







#GIT WRANGLING:
https://github.com/UCanCompBio/speed-vs-accuracy-meta-analysis.git
git add XXXX
git reset data/pubmed_result-*.xml #REMOVE LARGE FILES
git commit -a -v -m "Comment."
git push -u origin master


#LARGE (XML) FILES:
git lfs init #run once per repo
git lfs track "*.xml"
git add *.xml
git commit -m "Added large XML files."
git push origin master

