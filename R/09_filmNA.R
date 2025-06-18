#
# This script illustrates the different between partykit trees and SEM trees
# for univariate predictions
#
library(semtree)
library(lavaan)
library(psychTools)
data(affect)

affect$Film <- factor(affect$Film, ordered = FALSE, 
                      labels=c("Frontline", "Halloween", "Nat. Geographic","Parenthood"))


tree.data <- affect[,c("Film","neur","ext","soc","traitanx","NA1","PA1","imp","lie")] 
#tree.data$DeltaNA <- affect$PA2-affect$PA1
tree.data$DeltaNA <- affect$NA2-affect$NA1

knitr::kable(head(tree.data))

lav_mod <- "DeltaNA ~~ DeltaNA
DeltaNA ~ 1"
fitted_mod <- lavaan(lav_mod, tree.data)

manifests<-c("DeltaNA")
latents<-c()
omx_model <- mxModel("Simple Model", 
                 type="RAM",
                 manifestVars = manifests,
                 latentVars = latents,
                 mxPath(from="one",to=manifests, free=c(TRUE), value=c(1.0) , arrows=1, label=c("mean") ),
                 mxPath(from=manifests,to=manifests, free=c(TRUE), value=c(1.0) , arrows=2, label=c("variance") ),
                 mxData(tree.data, type = "raw")
);

library(semtree)
ctrl = semtree.control(
  method="score", 
  bonferroni = TRUE, min.bucket = 30)
tree = semtree( model = fitted_mod, 
                data = tree.data, 
                control=ctrl)

plot(tree)

# PART 2 ---------------

treeF = semtree( model = omx_model, 
                data = tree.data, 
                control=ctrl, constraints=semtree.constraints(focus.parameters="mean"))


# PART 3 ---------------

library(partykit)

ptree <- partykit::ctree(DeltaNA ~ ., tree.data)

plot(ptree)

# PLOT

plot_tree_mix <- function(tree,xlim=c(-10, 10)) {

tndata <- semtree::getTerminalNodes(tree) 

cols <- viridis::plasma(nrow(tndata))

pl <- ggplot2::ggplot(data = data.frame(x = xlim), ggplot2::aes(x))+
  ggplot2::xlab("Change in Negative Affect")

for (i in 1:nrow(tndata)) {
  pl <- pl + ggplot2::stat_function(fun = dnorm, 
                                    n = 101, col=cols[i], args = list(mean = tndata[i,2], sd = sqrt(tndata[i,1])))
}

return(pl)

}

pl1 <- plot_tree_mix(tree)
pl2 <- plot_tree_mix(treeF)

save(pl1,pl2,tree, treeF, tree.data,ptree, file="filmtrees.rds")

# PART 4 ---------------

forest = semforest( model = omx_model, 
                 data = tree.data, 
                 control=semtree::semforest_score_control(num.trees=100), constraints=semtree.constraints(focus.parameters="mean"))

vim <- semtree::varimp(forest, method="permutationFocus")

save(forest, vim, file="filmforest.rds")
