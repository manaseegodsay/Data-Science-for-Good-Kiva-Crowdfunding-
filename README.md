"# Data-Science-for-Good-Kiva-Crowdfunding-" 

Abstract:
Access to financial services can bring the poor the benefits of poverty reduction and limit some of the risks they can face, and crowdfunding is one way that can provide such benefits to the general population seeking investment and funds. Kiva.org, an online crowdfunding platform, provides financial services to poor and financially excluded people around the world. Kiva lenders have provided over $1 billion dollars in loans to over 2 million people. In this report, several models are used for assessing borrower welfare levels. We have developed approaches that will help Kiva understand its borrowers and their poverty levels in a better way so that it can better assess and maximize the impact of their work. We have tried to connect the features of each loan or product to one of several poverty mapping datasets, which indicate the average level of welfare in a region on as granular a level as possible.

Exploratory Ananlysis in R:
Kiva.org is an online crowdfunding platform to extend financial services to poor and financially excluded people around the world.
Goal: To predict if a loan would be accepted or not. 
Data balancing: We used stratified sampling with replacement. 
 
Methodology: 
Supervised Learning: Performed Logistic Regression, Xgboost, Random Forests. Xgboost generated smallest variance in AUC curve, thus showing this model was robust even when its parameters were stochastic. It had best AUC score across 40 folds. Almost in every fold, curve was within 1-sigma interval.  
Unsupervised Learning: Performed Cluster Analysis using partitioning around medoids (PAM) abd Gower distance. 

