
iris


# logistic function
logistic = function(x) {
  return(1 / (1 + exp(-x)))
}

# good value
set.seed(123)
# missing_vec = -0.1 * c(1, -2, 2, -2)
missing_vec = -0.1 * c(-2, -4, 2, 2)

set.seed(123)
# missing_vec = -0.1 * c(1, -2, 2, -2)
missing_vec = -0.1 * c(-2, -4, 2, 2)

dat = iris
for (i in 1:length(missing_vec)) {
  score = as.matrix(iris[,(1:4)[-i]]) %*% missing_vec[-i]
  propensity = logistic(score)
  missing = rbinom(n=nrow(dat), size = 1, prob = propensity)
  dat[missing == 1, i] = NA
}
dat

plot(iris[1:4], col = as.numeric(iris[,5]) + 1)

# install.packages("GGally")
# Parallel coordinates plot using GGally
# full iris data
GGally::ggpairs(data = iris, columns = 1:4, 
                ggplot2::aes(colour=Species, alpha = 0.7))
ggsave("plots/iris_data.pdf", height = 8, width = 14)


# missingness iris data
tab_dat = data.frame(variable = colnames(iris),
                     missing = colMeans(is.na(dat)))
ggplot(tab_dat[-5, ], aes(x = variable, y = missing)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")) +
  xlab("% missing values")
ggsave("plots/iris_data_missingness.pdf", height = 4, width = 7)

# set up bootstrap
set.seed(123)
res = data.frame(fold = 1:10, full_acc = 0, 
                 EM_acc = 0, na_rm_acc = 0, na_imp_acc = 0)
nfolds = 10

# impute means
means = colMeans(dat[,-5], na.rm = TRUE)
datImpMean = dat
datImpMean[is.na(datImpMean[,1]), 1] = means[1]
datImpMean[is.na(datImpMean[,2]), 2] = means[2]
datImpMean[is.na(datImpMean[,3]), 3] = means[3]


# cross validation for performance comparison
fold_id = sample(rep(1:10, length.out = nrow(dat)), 
                 nrow(dat), replace = FALSE)
for (fold in 1:nfolds) {
  test_ids = (1:nrow(dat))[fold_id == fold]
  train_ids = (1:nrow(dat))[fold_id != fold]
  
  # original data (no missing values)
  full <- rpart(Species ~ ., 
                data = iris[train_ids,])
  full_pred = predict(full, newdata = iris[test_ids,], type = "class")
  full_acc = mean(full_pred == dat[test_ids,"Species"])
  res[fold, "full_acc"] = full_acc
  # rpart.plot(full)
  
  # EM
  EM_fit = EM_rpart(Species ~ ., 
                    data = dat[train_ids,])
  EM_pred = predict(EM_fit, newdata = iris[test_ids,], type = "class")
  EM_acc = mean(EM_pred == dat[test_ids,"Species"])
  res[fold, "EM_acc"] = EM_acc
  # rpart.plot(EM_fit)
  
  # omit missing variables
  na_rm <- rpart(Species ~ ., 
                 data = dat[train_ids,])
  na_rm_pred = predict(na_rm, newdata = iris[test_ids,], type = "class")
  na_rm_acc = mean(na_rm_pred == dat[test_ids,"Species"])
  res[fold, "na_rm_acc"] = na_rm_acc
  # rpart.plot(na_rm)
  
  # impute means
  na_imp <- rpart(Species ~ ., 
                  data = datImpMean[train_ids,])
  na_imp_pred = predict(na_imp, newdata = iris[test_ids,], type = "class")
  na_imp_acc = mean(na_imp_pred == dat[test_ids,"Species"])
  res[fold, "na_imp_acc"] = na_imp_acc
  # rpart.plot(na_imp)
  
}
res

# average accuracy
colMeans(res)

# plot result of 10 fold CV
plot_res = reshape2::melt(res, id.vars = "fold")
ggplot(plot_res) +
  geom_boxplot(aes(x = variable, y = value)) +
  scale_x_discrete(labels = c("Full Data", "EM", "Remove NA", "Impute NA")) +
  xlab("Model") +
  ylab("Accuracy in 10 fold CV")
ggsave("plots/performance.pdf", height = 4, width = 7)



