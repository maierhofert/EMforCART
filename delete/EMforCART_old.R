# cleaned up
# EM for CART

# create data with missing values
dat = kyphosis
set.seed(1234)
for (i in 1:ncol(dat)) {
  samp = sample(c(TRUE, FALSE), prob = c(0.9, 0.1), 
                size = nrow(dat), replace = TRUE)
  dat[!samp, i] = NA
}
summary(dat)


# imputation function
my_impute = function(dat) {
  impute(dat,
         classes = list(integer = imputeMean(), 
                        numeric = imputeMean(), 
                        factor = imputeMode()))$data
}


EM_rpart = function (formula, data, nsteps = 10) {
  dat = data
  # initial imputation
  # impute missing values
  this.dat = my_impute(dat)
  
  # initial model
  this.fit = rpart(formula, data = this.dat)
  
  # EM algorithm
  for (it in 1:nsteps) {
    # M step
    # impute missing values
    # create pseudo data that only contains imputed data
    # combined with the original data, the previously imputed values will be 
    # used in the next imputation
    pseudo_dat = this.dat
    for (i in 1:ncol(dat)) {
      pseudo_dat[!is.na(dat[,i]),i] = NA
    }
    pseudo_dat$leaf = this.fit$where
    pseudo_dat$pseudo = TRUE
    
    # impute missing data
    # uses previously imputed information
    this.dat = dat %>%
      mutate(leaf = this.fit$where) %>%
      mutate(pseudo = FALSE) %>%
      rbind(pseudo_dat) %>%
      group_by(leaf) %>%
      # impute missing values
      my_impute() %>%
      # remove superfluous columns
      filter(!pseudo) %>%
      select(-pseudo)
    
    # E step
    # fit model
    this.fit = rpart(formula, data = this.dat)
  }
  return(this.fit)
}
# test
EM_fit = EM_rpart(Kyphosis ~ Age + Number + Start, data = dat)
plot(EM_fit)
# original fit
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
plot(fit)
