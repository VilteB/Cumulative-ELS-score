# ELS and PCM multimorbidity 

This repository contains a set of scripts for building cumulative early life stress (ELS) score for prenatal and postnatal periods using ALSPAC data.

The scripts are adapted from Cecil et al. (2014) cumulative environmental risk scores and harmonised with the equivalent ELS score in Generation R (see https://github.com/SereDef/cumulative-ELS-score#cumulative-els-score).

The construction of the score has 3 main steps:

1. Dichotomise items used in the construction of the score (sections 1.1 to 1.4).
2. Combine stressors together into a prenatal and postnatal score (2.1 and 2.2)
3. Include all outcomes, covariates and auxiliary variables and perform imputation of missing values. 
4. Run four regression models assesing associations between ELS and psycho-cardiometabolic (PCM) multimorbidity.

Have fun!


