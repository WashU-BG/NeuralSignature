ABCD 5.1 - set wd or 'base_dir' to appropriate path in each file

In order:


1. Setup data for analyses

make_data_dictionary - create data dictionary for use when cleaning
prep_non_fMRI        - merge & clean behavioral data
combine_fmri         - pull & merge fMRI data, use data.dictionary.xlsx
balanced_splits      - identify balanced split-halves 
combat_fmri          - run fMRI data through LongCombat

2. Prelim analyses

CompareSplits        - compare split-halves
task_activation      - task activation and stability
task_activation_permutations - permutations of task activation correlations to compute significance
task_activation_permutations_maps - saves out permuted maps for downstream analyses

2. Create neural signature

final_classification - create neural signature w/ elastic-net
performance_plots    - plots of classification performance

3. Primary analyses

classification_ML_permutations - variable importance from classifiers trained on permuted fMRI data
comapre_ML_activation - compare patterns of activation and variable importance across splits
ML_reliability       - signature reliability (NB may need to change # of cores used)
ML_regressions       - associations of the signature with individual difference measures
region_reliability   - regional reliability
region_regressions   - regional individual difference associations
regression_plot      - plot regression results
reliability_plot     - plot reliability results
map_comparisons      - correlation of main-effect maps with regional association maps
rho_p                - significance of the correlations between main-effect and regional association maps
plot_all_correlationmaps - plot all correlations between main-effect and regional association maps

4. Other ML approaches

ML_classifier_allmodels - Generate neural signatures using different ML algorithms
ML_classifier_allmodels_compare - Compare signatures across different ML algos
regressions_compare_ML_toeachother - Compare generalizability of ML predictions across different outcomes
ML_indiff_regression_cluster - Train elastic net models to predict each outcome measure
regressions_compare_ML - ML individual difference associations
regressions_compare_ML_bootp - Significance comparing signature to ML associations
plot_compare_ML_final - Plot comparisons of ML to neural signature
plot_compare_ML_sig_otheroutcomes - Plot comparisons of ML generalizability

5. Simulations

bootstrap_samplesize - bootstrap sample size neural signature
bootstrap_samplesize_ML_try2 - bootstrap sample size elastic net
plot_bootstraps      - plot bootstrap results

6. Other

difference_scores_example - example of difference scores and low reliability
VariableNames - Expanded variable names for plotting 
Atlas_regions - Region names for plotting with {ggseg}

