# Thomas Maierhofer
# EM algorithm for missing values in CART

library(rpart)

?rpart
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

# find missing values in original data
na_loc = is.na(dat)

my_mode = function(x) {
  tab = table(x)
  max = which.max(tab)
  return(names(max))
}

# impute mode and mean
for (i in 1:ncol(dat)) {
  if (class(dat[,i]) == "factor") {
    dat[is.na(dat[,i]), i] = my_mode(dat[,i])
  }
  else if (class(dat[,i]) == "integer") {
    dat[is.na(dat[,i]), i] = round(mean(dat[,i], na.rm = TRUE), 0)
  }
  else if (class(dat[,i]) == "numeric") {
    dat[is.na(dat[,i]), i] = mean(dat[,i], na.rm = TRUE)
  }
  else {
    print(paste("Case for class", class(dat[,i]), "not implemented"))
  }
}
summary(dat)

my_aggregate = function(dat) {
  ret = dat[1,]
  for (i in 1:ncol(dat)) {
    if ("factor" %in% class(as.data.frame(dat)[,i])) {
      ret[,i] = my_mode(as.data.frame(dat)[,i])
    } else if ("numeric" %in% class(as.data.frame(dat)[,i])) {
      ret[,i] = mean(as.data.frame(dat)[,i], na.rm = TRUE)
    } else {
      # print(paste("Case for class", class(dat[,i]), "not implemented"))
    }
  }
  ret
}

library("dplyr")

this.fit = rpart(Kyphosis ~ Age + Number + Start, data = dat)
plot(this.fit)

dat %>%
  mutate(leaf = this.fit$where) %>%
  group_by(leaf) %>%
  summarise(Age = mean(Age))

dat2 = dat %>%
  mutate(leaf = this.fit$where) %>%
  group_by(leaf)



