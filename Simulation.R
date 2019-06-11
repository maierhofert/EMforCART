# simulation study
source("EMforCART.R")
library("ggplot2")

# create data for CART

# #################################################################
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
plot(x, y, col = label)

# data frame
dat_under = data.frame(x = x, y = y, label = factor(label))
dat = data.frame(xobs = xobs, yobs = yobs, label = factor(label))

# plot data
ggplot(dat_under, aes(x = x, y = y, color = label)) +
  geom_point()

# ###############################################################
# square data 2 cuts
# alternative data
# underlying x and y
x = runif(min = -1, max = 1, n = 200)
y = runif(min = -1, max = 1, n = 200)
# true class label
label = rep(1, 200)
label[x < -0.5 | x > 0.5] = 2

# observed x and y
xobs = x + rnorm(200, sd = 0.5)
yobs = y + rnorm(200, sd = 0.5)

# plot original data
plot(xobs, yobs, col = label)
plot(x, y, col = label)

# data frame
dat_under = data.frame(x = x, y = y, label = factor(label))
dat = data.frame(xobs = xobs, yobs = yobs, label = factor(label))

# plot data
ggplot(dat_under, aes(x = x, y = y, color = label)) +
  geom_point()
ggsave("plots/dat_underlying.pdf", height = 4, width = 7)

ggplot(dat, aes(x = xobs, y = yobs, color = label)) +
  geom_point() +
  xlab("x") + ylab("y")
ggsave("plots/dat.pdf", height = 4, width = 7)


# ###############################################################
# linear data 2 cuts
# alternative data
# underlying x and y
x = rnorm(n = 200, mean = 0, sd = 3)
y = rnorm(n = 200, mean = x, sd = 1)
# true class label
label = rep(1, 200)
label[x < -1 | x > 1] = 2
mean(label)

# observed x and y
xobs = x + rnorm(200, sd = 1)
yobs = y + rnorm(200, sd = 1)

# plot original data
plot(xobs, yobs, col = label)
plot(x, y, col = label)

# data frame
dat_under = data.frame(x = x, y = y, label = factor(label))
dat = data.frame(xobs = xobs, yobs = yobs, label = factor(label))

# plot data
ggplot(dat_under, aes(x = x, y = y, color = label)) +
  geom_point()
ggsave("plots/dat_underlying.pdf", height = 4, width = 7)

ggplot(dat, aes(x = xobs, y = yobs, color = label)) +
  geom_point() +
  xlab("x") + ylab("y")
ggsave("plots/dat.pdf", height = 4, width = 7)


# ###############################################################

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



# 20% missing at random
set.seed(1234)
MAR = function(dat, prob_miss) {
  ret = dat
  for (i in 1:(ncol(dat) - 1)) {
    # use x to predict missingness in y
    # and y to predict missingness in x
    if (i == 1) {
      j = 2
    } else if (i == 2) {
      j = 1
    }
    # sample missing values
    samp = sample(x = 1:nrow(dat),
                  replace = FALSE,
                  prob = abs(dat[,j])^2,
                  # prob = (dat[,j] - min(dat[,j]) + 1)^2,
                  size = nrow(dat)*(prob_miss))
    ret[samp, i] = NA
  }
  return(ret)
}


# 20% missing at random
datMAR20 = MAR(dat, 0.2)
plot(datMAR20$xobs, datMAR20$yobs, col = datMAR20$label)

# plot MAR data missingness
plot_dat = dat
plot_dat$missing = "None"
plot_dat$missing[is.na(datMAR20$xobs)] = "X"
plot_dat$missing[is.na(datMAR20$yobs)] = "Y"
# plot data
ggplot(plot_dat, aes(x = xobs, y = yobs, color = missing)) +
  geom_point() +
  scale_color_manual(values = c("forestgreen", "firebrick2", "dodgerblue2")) +
  xlab("x") + ylab("y")
ggsave("plots/dat_MAR.pdf", height = 4, width = 7)

# impute means
means = colMeans(datMAR20[,1:2], na.rm = TRUE)
datImpMean = datMAR20
datImpMean[is.na(datImpMean[,1]), 1] = means[1]
datImpMean[is.na(datImpMean[,2]), 2] = means[2]
# plot imputed data
ggplot(datImpMean, aes(x = xobs, y = yobs, color = label)) +
  geom_point() +
  xlab("x") + ylab("y")

# plot complete data
ggplot(dat[complete.cases(datMAR20),], 
       aes(x = xobs, y = yobs, color = label)) +
  geom_point() +
  xlab("x") + ylab("y")


library("rpart.plot")

# set up bootstrap
set.seed(123)
res = data.frame(fold = 1:10, full_acc = 0, 
                 EM_acc = 0, na_rm_acc = 0, na_imp_acc = 0)
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
  # rpart.plot(full)
  
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
  # rpart.plot(na_rm)
  
  # impute means
  na_imp <- rpart(label ~ xobs + yobs, 
                 data = datImpMean[train_ids,])
  na_imp_pred = predict(na_imp, newdata = dat[test_ids,], type = "class")
  na_imp_acc = mean(na_imp_pred == dat[test_ids,"label"])
  res[fold, "na_imp_acc"] = na_imp_acc
  # rpart.plot(na_imp)
  
}
res

# average accuracy
colMeans(res)




