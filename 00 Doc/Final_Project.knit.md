---
title: "<center><b>Global Shipments Data</b></center>"
author: "<center><b>Yu-Chiao Fang, Elizabeth Kim, Seung Hoon Lee, Orlando Reategui</b></center>"
output:
  html_document:
    toc: yes
  html_notebook:
    toc: yes
---

#**Introduction**
This is our R Notebook, showing the steps we took to complete Project 6 for CS 329E. This notebook includes step-by-step instructions on how to reproduce our project. To obtain our data, we used data.world. 

#**R Configuration**
Below we display our sessionInfo().


```r
sessionInfo(package=NULL)
```

```
## R version 3.2.4 Revised (2016-03-16 r70336)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 14393)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
##  [1] backports_1.0.5 magrittr_1.5    rprojroot_1.2   tools_3.2.4    
##  [5] htmltools_0.3.5 yaml_2.1.14     Rcpp_0.12.10    stringi_1.1.2  
##  [9] rmarkdown_1.3   knitr_1.15.1    stringr_1.1.0   digest_0.6.12  
## [13] evaluate_0.10
```

#**Data Description**
Found in data.world, a customer based data set that looks at orders and shipments from different markets around the world.

#**Cleaning Data**
Here's our ETL file to clean our data set.





