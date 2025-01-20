# NeuralSignature

Contains code for the manuscript "A working memory neural signature as a powerful measure of individual differences in brain function", doi: xxx. 

Uses ABCD 5.1 - set wd or 'base_dir' to appropriate path in each file

In order:


## 1. Setup data for analyses

1. make_data_dictionary - create data dictionary for use when cleaning
2. prep_non_fMRI        - merge & clean behavioral data
3. combine_fmri         - pull & merge fMRI data, use data.dictionary.xlsx
4. balanced_splits      - identify balanced split-halves 
5. combat_fmri          - run fMRI data through LongCombat

## 2. Prelim analyses

1. CompareSplits        - compare split-halves
2. task_activation      - task activation and stability
3. task_activation_permutations - permutations of task activation correlations to compute significance
4. task_activation_permutations_maps - saves out permuted maps for downstream analyses

## 3. Create neural signature

1. final_classification - create neural signature w/ elastic-net
2. performance_plots    - plots of classification performance

## 4. Primary analyses

1. classification_ML_permutations - variable importance from classifiers trained on permuted fMRI data
2. comapre_ML_activation - compare patterns of activation and variable importance across splits
3. ML_reliability       - signature reliability (NB may need to change # of cores used)
4. ML_regressions       - associations of the signature with individual difference measures
5. region_reliability   - regional reliability
6. region_regressions   - regional individual difference associations
7. regression_plot      - plot regression results
8. reliability_plot     - plot reliability results
9. map_comparisons      - correlation of main-effect maps with regional association maps
10. rho_p                - significance of the correlations between main-effect and regional association maps
11. plot_all_correlationmaps - plot all correlations between main-effect and regional association maps

## 5. Other ML approaches

1. ML_classifier_allmodels - Generate neural signatures using different ML algorithms
2. ML_classifier_allmodels_compare - Compare signatures across different ML algos
3. regressions_compare_ML_toeachother - Compare generalizability of ML predictions across different outcomes
4. ML_indiff_regression_cluster - Train elastic net models to predict each outcome measure
5. regressions_compare_ML - ML individual difference associations
6. regressions_compare_ML_bootp - Significance comparing signature to ML associations
7. plot_compare_ML_final - Plot comparisons of ML to neural signature
8. plot_compare_ML_sig_otheroutcomes - Plot comparisons of ML generalizability

## 6. Simulations

1. bootstrap_samplesize - bootstrap sample size neural signature
2. bootstrap_samplesize_ML_try2 - bootstrap sample size for elastic net
3. plot_bootstraps      - plot bootstrap results

## 7. Synthetic Data

1. setup_sims - write out simulation parameters, for use with a HPC
2. run_sims_cluster2 - simple simulations of a neural signature with synthetic data
3. run_sims_cluster_complex - more realistic, and complex, simulations of a neural signature with synthetic data
4. sim_plots - Plot results of synthetic data simulations

## 8. Other

1. difference_scores_example - example of difference scores and low reliability
2. VariableNames - Expanded variable names for plotting 
3. Atlas_regions - Region names for plotting with {ggseg}

### 9. Making new predictions

1. Example_prediction - demonstrates how to use our published elastic-net weights to generate neural signature predictions in a new data set. 
