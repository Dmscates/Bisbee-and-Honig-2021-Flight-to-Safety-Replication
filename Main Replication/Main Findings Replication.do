*** Replication Project ***
*** Table 1 ***

cd "C://Users//danie//Dropbox//Replication//dataverse_files//Replication//Daniel Reproduction"

ssc install did_multiplegt, replace

cap clear

use "finalDat.dta"


did_multiplegt scale_pcttw_sanders DMA_CODE date treatBin, controls(pcttw_sanders16) breps(10000) cluster(DMA_CODE) seed(123) 
