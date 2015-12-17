
This repository contains scripts and datasets associated with a study of benchmarks of bioinformatic software. 

We aim to test which factors influence the accuracy of software.








*GIT WRANGLING (don't judge me -- I can't remember this crap):
**https://github.com/UCanCompBio/speed-vs-accuracy-meta-analysis.git
**git add XXXX
**git reset data/pubmed_result-*.xml #REMOVE LARGE FILES
**git commit -a -v -m "Comment."
**git push -u origin master

*LARGE (XML) FILES:
**git lfs init #run once per repo
**git lfs track "*.xml"
**git add *.xml
**git commit -m "Added large XML files."
**git push origin master

