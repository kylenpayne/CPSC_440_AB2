lbf_test <- function(data, group_col, measure_var, alpha=0.05, type="levene"){
  ## -------------------------
  ## This function takes a melted data.frame as input
  ## -------------------------
  ## group_col is a string
  ## of the name of the column
  ## that corresponds to the grouping
  ## factor that defines the groupings.
  ## or if it is a number if is
  ## the number of the column
  ## --------------------------
  gc <- data[,group_col]

  N <- nrow(data)
  k <- length(levels(gc))
  ns <- plyr::ddply(data,group_col,nrow)
  Z <- numeric(N)


  means_ddply <- function(df, group_col, measure_var){
    means <- plyr::ddply(.data = df, group_col,.fun = function(xx){
      mean=mean(xx[,measure_var], na.rm=TRUE)
    })
    return(means)
  }

  medians_ddply <- function(df, group_col, measure_var){
    medians <- plyr::ddply(.data = df, group_col,.fun = function(xx){
      median=median(xx[,measure_var], na.rm=TRUE)
    })
    return(medians)
  }

  if(type == "levene"){
    means <- means_ddply(df=data, group_col=group_col, measure_var=measure_var)
    for(i in 1:k){
      rws <- gc %in% levels(gc)[i]
      Z[rws] <- abs(data[rws,measure_var] - means[i,2])
    }
  }

  if(type =="bf"){
    ## grab the medians
    medians<-medians_ddply(df=data,group_col=group_col,measure_var=measure_var)
    ## loop through and calculate the Z_ij's
    for(i in 1:k){
      rws <- gc %in% levels(gc)[i]
      Z[rws] <- abs(data[rws,measure_var] - medians[i,2])
    }

  }
  tmp_df <- data.frame(Z = Z, group_col = gc)
  means_Z<- plyr::ddply(tmp_df, .(group_col), summarize, mean(Z))
  z_gm <- mean(tmp_df$Z)

  numer <- sum(ns[,2]*(means_Z[,2] - z_gm)^2)

  denom<-0
  for(i in 1:k){
    rws <- gc %in% levels(gc)[i]
    denom <- denom + sum((tmp_df[rws,'Z']-means_Z[i,2])^2)
  }
  norm <- (N-k)/(k-1)

  W <- norm*(numer/denom)
  crit <- qf(1-alpha,k-1, N-k)
  pval <- 1-pf(W, k-1, N-k)
  res <- c(W, crit, pval)
  res <- matrix(res, ncol=3, byrow=T)
  colnames(res) <- c("Test Statistic", "Critical Value", "P-val")
  return(res)
}


confint_var_ratio <- function(var1, var2, alpha){
  if(var(var1)>var(var2)){
    ratio <- var(var1)/var(var2)
    F_l <- qf(alpha/2, df1=length(var1)-1, df2=length(var2)-1)
    F_u <- qf(1-alpha/2,df1=length(var1)-1, df2=length(var2)-1)
  }else{
    ratio <- var(var2)/var(var1)
    F_l <- qf(alpha/2, df1=length(var2)-1, df2=length(var1)-1)
    F_u <- qf(1-alpha/2,df1=length(var2)-1, df2=length(var1)-1)

  }

  confint <- matrix(c(ratio*F_l ,  ratio*F_u), ncol=2, nrow=1, byrow=T)
  colnames(confint) <- c("lower $F_l\\times \\frac{s^2_1}{s^2_2}$", "upper $F_U\\times\\frac{s^2_1}{s^2_2}$")

  return(confint)
}


f_max <- function(data, alpha=0.05){
  if(is.data.frame(data)){
    ## figure out the variances for each column
    vars <- apply(X = data, FUN = var, MARGIN = 2)
    ## this calculates the variance over the columns
    ## grab the largest variance and the smallest
    n_max <- nrow(data)
  }
  if(is.list(data)){
    ## if a list, figure out for each element in list.
    vars <- lapply(data, function(x){return(var(x))})
  }
  tmp_max <- which.max(vars)
  tmp_min <- which.min(vars)
  n_max <- length(data[[tmp_max]])
  n_min <- length(data[[tmp_min]])

  s2_max <- max(vars)
  s2_min <- min(vars)

  ## calculate f_max
  f_max <- s2_max/s2_min

  qf_max <- qf(p = 1-alpha,df1=n_max-1, df2=n_min-1)
  p_max <- pf(q = f_max, df1 = n_max-1, df2=n_min-1)

  res <- c(s2_min, s2_max, f_max, qf_max, p_max)
  res_names <- c("Min S^2", "Max S^2", "Fmax", "Fmax Critical", "Pval")
  res <- tablify(items = res,col_names = res_names,n_col = 4, n_row = 1)
  return(res)
}


