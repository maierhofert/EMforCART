# simulation study

# create data for random forest

# underlying x and y
x = runif(min = -1, max = 1, n = 200)
y = runif(min = -1, max = 1, n = 200)
# true class label
label = rep(1, 200)
label[x < 0 & y < 0] = 2

# observed x and y
xobs = x + rnorm(200, sd = 1)
yobs = y + rnorm(200, sd = 1)

# plot original data
plot(xobs, yobs, col = label)

# data frame
dat = data.frame(xobs = x, yobs = yobs, label = factor(label))

# missing completely at random
MCAR = function(dat, prob_miss) {
  for (i in 1:(ncol(dat) - 1)) {
    samp = sample(c(TRUE, FALSE), 
                  prob = c(1 - prob_miss, prob_miss), 
                  size = nrow(dat), replace = TRUE)
    dat[!samp, i] = NA
  }
  return(dat)
}


set.seed(123)
# 20% missing completely at random
datMCAR20 = MCAR(dat, 0.1)
summary(datMCAR20)
plot(datMCAR20$xobs, datMCAR20$yobs, col = datMCAR20$label)


# logistic function
logistic = function(x) {
  return(1 / (1 + exp(x)))
}

# 20% missing at random
set.seed(123)
MAR = function(dat, prob_miss) {
  for (i in 1:(ncol(dat) - 1)) {
    # samp = rbinom(n = nrow(dat), size = 1,
    #               prob = logistic((dat[,i] + 1) * dat[,ncol(dat)]))
    samp = sample(1:nrow(dat),
                  prob = (dat[,i] - min(dat[,i]) + 1),
                  size = nrow(dat)*(prob_miss))
    dat[samp, i] = NA
  }
  return(dat)
}

# 20% missing at random
datMAR20 = MAR(dat, 0.2)
plot(datMAR20$xobs, datMAR20$yobs, col = datMAR20$label)


library("rpart.plot")

# set up bootstrap
set.seed(1234)
res = data.frame(fold = 1:10, full_acc = 0, 
                 EM_acc = 0, na_rm_acc = 0)
nfolds = 10

fold_id = sample(rep(1:10, length.out = nrow(dat)), 
                 nrow(dat), replace = FALSE)
for (fold in 1:nfolds) {
  test_ids = (1:nrow(dat))[fold_id == fold]
  train_ids = (1:nrow(dat))[fold_id != fold]
  
  # original data (no missing values)
  full <- rpart(label ~ xobs + yobs, 
               data = dat[train_ids,])
  full_pred = predict(full, newdata = dat[test_ids,], type = "class")
  full_acc = mean(full_pred == dat[test_ids,"label"])
  res[fold, "full_acc"] = full_acc
  # rpart.plot(fit)
  
  # EM
  EM_fit = EM_rpart(label ~ xobs + yobs, 
                    data = datMAR20[train_ids,])
  EM_pred = predict(EM_fit, newdata = dat[test_ids,], type = "class")
  EM_acc = mean(EM_pred == dat[test_ids,"label"])
  res[fold, "EM_acc"] = EM_acc
  # rpart.plot(EM_fit)
  
  # omit missing variables
  na_rm <- rpart(label ~ xobs + yobs, 
                  data = datMAR20[train_ids,])
  na_rm_pred = predict(na_rm, newdata = dat[test_ids,], type = "class")
  na_rm_acc = mean(na_rm_pred == dat[test_ids,"label"])
  res[fold, "na_rm_acc"] = na_rm_acc
  # rpart.plot(fit_rm)
}
res

#
colMeans(res)




