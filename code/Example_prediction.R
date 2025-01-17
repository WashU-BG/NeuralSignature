##### Generating new predictions with the neural signature
library(dplyr)
####
# Read in elastic-net weights

weights = readxl::read_xlsx(path = "C:/Users/dbara/Documents/ABCD/NerualSig/Final/Supplement.xlsx",sheet = "Elastic-net Weights")
colnames(weights)[1] = "region"

#########
# read in your own data. note that regions MUST have the same name as the regions in the weights variable (except for 'intercept')

## Here I'll quickly make up some nonsense data to use for demonstration purposes
## Note that this data is not representative of results from real data, as the regions in this synthetic data are fully
### independent. This is not the case for real fMRI data, which has substantial correlations between regions. 

dat = MASS::mvrnorm(n = 1000,mu = rep(0,167),Sigma = diag(167)) %>% as.data.frame()
colnames(dat) = weights$region[-1]

############
# check that regions are in the same order, and have the same names. Should return 'TRUE'!
all.equal(weights$region[-1],colnames(dat))
  
################
# generate predictions with each split

z = apply(apply(dat,1,function(X){X * t(weights$Split1)[-1] }),2,sum)
z = z + weights$Split1[1] # add in intercept
pred1 = 1/(1 + exp(-z))

z = apply(apply(dat,1,function(X){X * t(weights$Split2)[-1] }),2,sum)
z = z + weights$Split2[1] # add in intercept
pred2 = 1/(1 + exp(-z))

final.pred = (pred1 + pred2)/2

write.csv(x = as.data.frame(final.pred),file = "neural_signature_predictions.csv")
