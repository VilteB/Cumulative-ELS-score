# Early life stress and psycho-cardiometabolic multimorbidity in ALSPAC

This repository contains a set of scripts for building cumulative early life stress (ELS) score for prenatal and postnatal periods using ALSPAC data.

The scripts are adapted from Cecil et al. (2014) cumulative environmental risk scores and harmonised with the equivalent ELS score in Generation R (see https://github.com/SereDef/cumulative-ELS-score#cumulative-els-score).

The construction of the score has 3 main steps:

1. Run '0.functions.R' script to obtain the necessary functions.
2. Dichotomise items used in the construction of the score (sections. 1.1 to 1.4).
3. Create ELS scores for prenatal and postnattal periods (sections 2.1. to 2.2).

The rest of the scripts are used to assess associations between ELS and psycho-cardiometabolic (PCM) multimorbidity 

1. The '3.1.PCM_outcomes_covs_aux.R' script retrieves all necesasry outcome covariates and auxiliary variables.
2. The '3.2.Dataset_contruction.R' script builds the final dataset that will be used for the analysis of the association between ELS and PCM multimorbidity in ALSPAC children.
3. The '3.3.Imputation.R' performs the imputation of missing data, necessary for the analysis of the association between ELS and PCM multimorbidity.
4. The '3.4.Regressions.R' runs a set of analyses for the project "Early-life stress as a risk factor for poor mental and physical health in children: A population-based study" looking at the association between ELS and psycho-cardio-metabolic multi-morbidity at age 10."  
