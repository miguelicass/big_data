
install.packages("e1071")
install.packages("caret")
install.packages("MASS")
install.packages("dummies")
install.packages("naniar")
install.packages("nnet")
install.packages("NeuralNetTools")
install.packages("ggplot2")
install.packages("plotly")
install.packages("dplyr")
install.packages("data.table")
install.packages("reshape")
install.packages("pROC")
install.packages("reshape2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("randomForest")
install.packages("gbm")
install.packages("xgboost")
install.packages("caretEnsemble")
install.packages("smartdata")
install.packages("parallel")
install.packages("doParallel")
install.packages("visualpred")




# *******************************************************************************
# INSTALACION h2o (ultimo tema)
# *******************************************************************************
# The following two commands remove any previously installed H2O packages for R.

# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# # Next, we download packages that H2O depends on.
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
# if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# # Now we download, install and initialize the H2O package for R.
# install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/2/R")

#Para probar h2o, se hace
#h2o.init()
#Si no funciona (pide java) hay que instalar download e instalar la ?ltima versi?n java