# Nest Survival Simulation
sims <- 1000 # number of nest simulations
nest_t <- 28 # length of nesting season

# initiate vegetation matrix 
veg_AHC <- matrix(nrow = sims, ncol = nest_t)

for(i in 1:sims){
  # a = asymptote height 
  # b = is the slope value when a is half its value
  # t = is the independent variable "day"
  a <- 122 + rnorm(1, 0, 15)
  b <- 6 + rnorm(1, 0, 0.5)
  
  for(t in 1:nest_t){
    
    veg_AHC[i, t] <- (a * t) / (b + t)
  }
}

matplot(t(veg_AHC), type = "l")

veg_AHC <- (veg_AHC - mean(veg_AHC)) / sd(veg_AHC)

# setting intercept and slope coefficients
b0 <- log(.963) - log(1 - .963)
b1 <- c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)

# initiate a survival matrix
nest_null <- matrix(nrow = sims, ncol = nest_t)
nest_p1 <- matrix(nrow = sims, ncol = nest_t)
nest_p2 <- matrix(nrow = sims, ncol = nest_t)
nest_p3 <- matrix(nrow = sims, ncol = nest_t)
nest_n1 <- matrix(nrow = sims, ncol = nest_t)
nest_n2 <- matrix(nrow = sims, ncol = nest_t)
nest_n3 <- matrix(nrow = sims, ncol = nest_t)

# Setting the first interval of the nesting season to 1
nest_null[, 1] <- nest_p1[, 1] <- nest_p2[, 1] <- nest_p3[, 1] <- nest_n1[, 1] <- nest_n2[, 1] <- nest_n3[, 1] <- 1

for(i in 1:sims){
  # looping through each nest

  for(t in 2:nest_t){
    # looping over each nesting interval
    
    # negative effect
    nest_n3[i, t] <- rbinom(n = 1, size = 1, 
                            prob = plogis(b0 + b1[1] * veg_AHC[i, t]) * nest_n3[i, t - 1])
    
    nest_n2[i, t] <- rbinom(n = 1, size = 1, 
                            prob = plogis(b0 + b1[2] * veg_AHC[i, t]) * nest_n2[i, t - 1])
    
    nest_n1[i, t] <- rbinom(n = 1, size = 1, 
                            prob = plogis(b0 + b1[3] * veg_AHC[i, t]) * nest_n1[i, t - 1])
    
    # No effect 
    nest_null[i, t] <- rbinom(n = 1, size = 1, 
                              prob = plogis(b0 + b1[4] * veg_AHC[i, t]) * nest_null[i, t - 1])
    # positive effect
    nest_p1[i, t] <- rbinom(n = 1, size = 1,
                            prob = plogis(b0 + b1[5] * veg_AHC[i, t]) * nest_p1[i, t - 1])
    
    nest_p2[i, t] <- rbinom(n = 1, size = 1, 
                            prob = plogis(b0 + b1[6] * veg_AHC[i, t]) * nest_p2[i, t - 1])
    
    nest_p3[i, t] <- rbinom(n = 1, size = 1, 
                            prob = plogis(b0 + b1[7] * veg_AHC[i, t]) * nest_p3[i, t - 1])
    
  }
  
}

# Create a single data.frame for each of the different effects with the different vegetation heights
# First column - nest fate (0/1)
# Second column - vegetation height at nest fate
# Third column - vegetation height at estimated nest hatch
# fourth column - vegetation as a continuous covariate through time

data <- data.frame(ID = rep(paste("ID", 1:1000, sep = ""), each = 28), 
                   Null = as.vector(t(nest_null)), 
                   p1 = as.vector(t(nest_p1)),
                   p2 = as.vector(t(nest_p2)), 
                   p3 = as.vector(t(nest_p3)), 
                   n1 = as.vector(t(nest_n1)), 
                   n2 = as.vector(t(nest_n2)), 
                   n3 = as.vector(t(nest_n3)), 
                   veg_fate_null = NA, 
                   veg_hatch_null = NA, 
                   veg_cont_null = NA,
                   veg_fate_p1 = NA, 
                   veg_hatch_p1 = NA,
                   veg_cont_p1 = NA,
                   veg_fate_p2 = NA, 
                   veg_hatch_p2 = NA, 
                   veg_cont_p2 = NA, 
                   veg_fate_p3 = NA, 
                   veg_hatch_p3 = NA,
                   veg_cont_p3 = NA,
                   veg_fate_n1= NA, 
                   veg_hatch_n1 = NA,
                   veg_cont_n1 = NA,
                   veg_fate_n2 = NA, 
                   veg_hatch_n2 = NA,
                   veg_cont_n2 = NA,
                   veg_fate_n3 = NA, 
                   veg_hatch_n3 = NA,
                   veg_cont_n3 = NA)

id <- unique(data$ID)

for(i in 1:length(id)){
  ind <- which(id[i] == data$ID)
  
  # No effect
  null_vec <- data$Null[ind]
  null_fail <- which(data$Null[ind] == 0)
  
  if(length(null_fail) > 0){
    # vegetation height at fate of nest (i.e date of failure)
    data$veg_fate_null[ind[1]:ind[null_fail][1]] <- veg_AHC[i, null_fail[1]]
    # vegetation height at estimated hatch date (day 28)
    data$veg_hatch_null[ind[1]:ind[null_fail][1]] <- veg_AHC[i, 28]
    # Vegetation height as continuous growth
    data$veg_cont_null[ind[1]:ind[null_fail][1]] <- veg_AHC[i, 1:null_fail[1]]
  } else {
    data$veg_fate_null[ind] <- veg_AHC[i, 28]
    data$veg_hatch_null[ind] <- veg_AHC[i, 28]
    data$veg_cont_null[ind] <- veg_AHC[i, ]
  }
  
  # Positive effect
  p1_vec <- data$p1[ind]
  p1_fail <- which(data$p1[ind] == 0)
  
  if(length(p1_fail) > 0){
    # vegetation height at fate of nest (i.e. date of failure)
    data$veg_fate_p1[ind[1]:ind[p1_fail][1]] <- veg_AHC[i, p1_fail[1]]
    # Vegetation height at estimated hatch date (day 28)
    data$veg_hatch_p1[ind[1]:ind[p1_fail][1]]  <- veg_AHC[i, 28]
    # vegetation height as continuous growth
    data$veg_cont_p1[ind[1]:ind[p1_fail][1]] <- veg_AHC[i, 1:p1_fail[1]]
  } else {
    data$veg_fate_p1[ind] <- veg_AHC[i, 28]
    data$veg_hatch_p1[ind] <- veg_AHC[i, 28]
    data$veg_cont_p1[ind] <- veg_AHC[i, ]
  }
  
  p2_vec <- data$p2[ind]
  p2_fail <- which(data$p2[ind] == 0)
  
  if(length(p2_fail) > 0){
    # vegetation height at fate of nest (i.e date of failure)
    data$veg_fate_p2[ind[1]:ind[p2_fail][1]] <- veg_AHC[i, p2_fail[1]]
    # vegetation height at estimated hatch date (day 28)
    data$veg_hatch_p2[ind[1]:ind[p2_fail][1]] <- veg_AHC[i, 28]
    # vegetation height continuous growth
    data$veg_cont_p2[ind[1]:ind[p2_fail][1]] <- veg_AHC[i, 1:p2_fail[1]]
  } else {
    data$veg_fate_p2[ind] <- veg_AHC[i, 28]
    data$veg_hatch_p2[ind] <- veg_AHC[i, 28]
    data$veg_cont_p2[ind] <- veg_AHC[i, ]
  }
  
  p3_vec <- data$p3[ind]
  p3_fail <- which(data$p3[ind] == 0)
  
  if(length(p3_fail) > 0){
    # vegetation height at fate of nest (i.e. date of failure)
    data$veg_fate_p3[ind[1]:ind[p3_fail][1]] <- veg_AHC[i, p3_fail[1]]
    # vegetation height at estimated hatch date (day 28)
    data$veg_hatch_p3[ind[1]:ind[p3_fail][1]] <- veg_AHC[i, 28]
    # vegetation height continuous growth
    data$veg_cont_p3[ind[1]:ind[p3_fail][1]] <- veg_AHC[i, 1:p3_fail[1]]
  } else {
    data$veg_fate_p3[ind] <- veg_AHC[i, 28]
    data$veg_hatch_p3[ind] <- veg_AHC[i, 28]
    data$veg_cont_p3[ind] <- veg_AHC[i, ]
  }
  
  # Negative effect
  n1_vec <- data$n1[ind]
  n1_fail <- which(data$n1[ind] == 0)
  
  if(length(n1_fail) > 0){
    # vegetation height at fate of nest (i.e. date of failure)
    data$veg_fate_n1[ind[1]:ind[n1_fail][1]] <- veg_AHC[i, n1_fail[1]]
    # Vegetation height at estimated hatch date (day 28)
    data$veg_fate_n1[ind[1]:ind[n1_fail][1]] <- veg_AHC[i, 28]
    # vegetation height continous growth
    data$veg_cont_n1[ind[1]:ind[n1_fail][1]] <- veg_AHC[i, 1:n1_fail[1]]
  } else {
    data$veg_fate_n1[ind] <- veg_AHC[i, 28]
    data$veg_hatch_n1[ind] <- veg_AHC[i, 28]
    data$veg_cont_n1[ind] <- veg_AHC[i, ]
  }
  
  n2_vec <- data$n2[ind]
  n2_fail <- which(data$n2[ind] == 0)
  
  if(length(n2_fail) > 0){
    # vegetation height at fate of nest (i.e. date of failure)
    data$veg_fate_n2[ind[1]:ind[n2_fail][1]] <- veg_AHC[i, n2_fail[1]]
    # Vegetation height at estimated hatch date (day 28)
    data$veg_hatch_n2[ind[1]:ind[n2_fail][1]] <- veg_AHC[i, 28]
    # vegetation height continuous growth
    data$veg_cont_n2[ind[1]:ind[n2_fail][1]] <- veg_AHC[i, 1:n2_fail[1]]
  } else {
    data$veg_fate_n2[ind] <- veg_AHC[i, 28]
    data$veg_hatch_n2[ind] <- veg_AHC[i, 28]
    data$veg_cont_n2[ind] <- veg_AHC[i, ]
  }
  
  n3_vec <- data$n3[ind]
  n3_fail <- which(data$n3[ind] == 0)
  
  if(length(n3_fail) > 0){
    # vegetation height at fate of nest (i.e. date of failure)
    data$veg_fate_n3[ind[1]:ind[n3_fail][1]] <- veg_AHC[i, n3_fail[1]]
    # vegetation height at hatch date (day 28)
    data$veg_hatch_n3[ind[1]:ind[n3_fail][1]] <- veg_AHC[i, 28]
    # vegetation height continuous growth
    data$veg_cont_n3[ind[1]:ind[n3_fail][1]] <- veg_AHC[i, 1:n3_fail[1]]
  } else {
    data$veg_fate_n3[ind] <- veg_AHC[i, 28]
    data$veg_hatch_n3[ind] <- veg_AHC[i, 28]
    data$veg_cont_n3[ind] <- veg_AHC[i, ]
  }
  print(i)
}

View(data)
