dat <- bdgraph.sim(p = p, n = n, prob = density, b = df)
cov_mat <- dat$sigma 
cor_mat <- cov2cor(cov_mat)
d <- MASS::mvrnorm(n=n, mu = rep(0,p), Sigma = (mat_list$cor_mat))