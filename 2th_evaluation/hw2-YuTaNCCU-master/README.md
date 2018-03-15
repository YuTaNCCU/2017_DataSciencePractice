# hw2

## cmd

```R
Rscript hw2_yourID.R --target male/female --inputs meth1 meth2 ... methx --output result.csv

```

* Read in multiple files
* Positive case defined by “--target” option
* Find out which method contains the max
* yourID should be your student ID number, i.e., hw2_106769999.R

## Inputs

* examples/method1.csv
* where the last column, pred.score, is the predicted probability of "Male".


persons,prediction,reference,pred.score

person1,male,male,0.807018548483029

person2,male,male,0.740809247596189

person3,female,male,0.0944965328089893

person4,female,female,0.148418645840138

## Output
* examples/output1.csv

method,sensitivity,specificity,F1,AUC

method1,0.91,0.96,0.85,0.79

method2,0.99,0.98,0.86,0.70

highest,method2,method2,method2,method1

## Examples

```R
Rscript hw2_yourID.R --target male --inputs examples/method1.csv examples/method2.csv --output examples/output1.csv
Rscript hw2_yourID.R --target male --inputs examples/method1.csv examples/method3.csv examples/method5.csv --output examples/output2.csv
Rscript hw2_yourID.R --target female --inputs examples/method2.csv examples/method4.csv examples/method6.csv --output examples/output3.csv

```
