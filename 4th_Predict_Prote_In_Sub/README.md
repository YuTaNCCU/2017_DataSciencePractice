# hw4

## Predict protein subcellular localization

![PredictProtein](/images/img1.png)

* Chang, J.-M. M. et al. Efficient and interpretable prediction of protein functional classes by correspondence analysis and compact set relations. PLoS ONE 8, e75542 (2013).
* Chang J-M, Su EC-Y, Lo A, Chiu H-S, Sung T-Y, & Hsu W-L (2008) PSLDoc: Protein subcellular localization prediction based on gapped-dipeptides and probabilistic latent semantic analysis. Proteins: Structure, Function, and Bioinformatics 72(2):693-710.

## Input: Archaeal_tfpssm.csv

V2: labels of proteins

* CP: Cytoplasmic
* CW: Cell Wall
* EC: Extracellular
* IM: Inner membrane

V3 ~ V5602: the gapped-dipeptide features of each protein

### Code to read data

```R
d <- read.csv("Archaeal_tfpssm.csv", header = F)
levels(d[,2])
head(d[,5600:5603]) 
```

### cmd

```R
Rscript hw4_studentID.R -fold n –out performance.csv
```
* Perform n-fold cross-validation
* % of training, % of calibration, % of testing= n-2, 1, 1

![cross-validation](/images/img2.png)

## Model

* Any model you want
* Predict V2 value for each protein

## Output: performance.csv

set,accuracy

trainning,0.91

calibration,0.85

test,0.77

* accuracy = P/N, average of n-fold cross-validation
