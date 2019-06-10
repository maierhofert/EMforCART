# Thomas Maierhofer
# EM algorithm for missing values in CART
# Sandbox


library(rpart)
library("mlr")
library("dplyr")


# generic example
str(kyphosis)
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit

fit$where
plot(fit)


# create missing values
dat = kyphosis
set.seed(1234)
for (i in 1:ncol(dat)) {
  samp = sample(c(TRUE, FALSE), prob = c(0.9, 0.1), 
                size = nrow(dat), replace = TRUE)
  dat[!samp, i] = NA
}
summary(dat)

# # add information whether missing values occured
# dat$missing = !complete.cases(dat)

# # information whether na or not
# is_na = is.na(dat)

# impute missing values
this.dat = impute(dat, 
             classes = list(integer = imputeMean(), 
                            numeric = imputeMean(), 
                            factor = imputeMode()))$data
this.dat

# imputation function
imp_dat = impute(dat,
                 classes = list(integer = imputeMean(), 
                                numeric = imputeMean(), 
                                factor = imputeMode()))
my_impute = function(dat) {
  impute(dat,
         classes = list(integer = imputeMean(), 
                        numeric = imputeMean(), 
                        factor = imputeMode()))$data
}

# initial model
this.fit = rpart(Kyphosis ~ Age + Number + Start, data = imp)
plot(this.fit)

# impute missing data
# does not use previously imputed information
this.dat2 = dat %>%
  mutate(leaf = this.fit$where) %>%
  group_by(leaf) %>%
  # does not use information of previously imputed data
  my_impute()

# create pseudo data that only contains imputed data
# combined with the original data, the previously imputed values will be 
# used in the next imputation
pseudo_dat = this.dat
for (i in 1:ncol(pseudo_dat)) {
  pseudo_dat[!is.na(dat[,i]),i] = NA
}
pseudo_dat$leaf = this.fit$where
pseudo_dat$pseudo = TRUE
pseudo_dat

# impute missing data
# uses previously imputed information
this.dat2 = dat %>%
  mutate(leaf = this.fit$where) %>%
  mutate(pseudo = FALSE) %>%
  rbind(pseudo_dat) %>%
  group_by(leaf) %>%
  # impute missing values
  my_impute() %>%
  # remove superfluous columns
  filter(!pseudo) %>%
  select(-pseudo)










# # find missing values in original data
# na_loc = is.na(dat)
# 
# my_mode = function(x) {
#   tab = table(x)
#   max = which.max(tab)
#   return(names(max))
# }
# 
# # impute mode and mean
# for (i in 1:ncol(dat)) {
#   if (class(dat[,i]) == "factor") {
#     dat[is.na(dat[,i]), i] = my_mode(dat[,i])
#   }
#   else if (class(dat[,i]) == "integer") {
#     dat[is.na(dat[,i]), i] = round(mean(dat[,i], na.rm = TRUE), 0)
#   }
#   else if (class(dat[,i]) == "numeric") {
#     dat[is.na(dat[,i]), i] = mean(dat[,i], na.rm = TRUE)
#   }
#   else {
#     print(paste("Case for class", class(dat[,i]), "not implemented"))
#   }
# }
# summary(dat)
# 
# my_aggregate = function(dat) {
#   ret = dat[1,]
#   for (i in 1:ncol(dat)) {
#     if ("factor" %in% class(as.data.frame(dat)[,i])) {
#       ret[,i] = my_mode(as.data.frame(dat)[,i])
#     } else if ("numeric" %in% class(as.data.frame(dat)[,i])) {
#       ret[,i] = mean(as.data.frame(dat)[,i], na.rm = TRUE)
#     } else {
#       # print(paste("Case for class", class(dat[,i]), "not implemented"))
#     }
#   }
#   ret
# }
# # library("dplyr")
# 
# this.fit = rpart(Kyphosis ~ Age + Number + Start, data = dat)
# plot(this.fit)
# 
# dat %>%
#   mutate(leaf = this.fit$where) %>%
#   group_by(leaf) %>%
#   summarise(Age = mean(Age))
# 
# dat2 = dat %>%
#   mutate(leaf = this.fit$where) %>%
#   group_by(leaf)



