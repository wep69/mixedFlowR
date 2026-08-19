# mixedFlowR frozen validation battery
# This script is intentionally not run during package installation or CRAN checks.
# Seeds and targets are recorded in inst/metadata/simulation_scenarios.csv.

if (!requireNamespace("mixedFlowR", quietly = TRUE)) stop("Install mixedFlowR before running the validation battery.")
if (!requireNamespace("lme4", quietly = TRUE)) stop("lme4 is required for the core validation battery.")

root <- normalizePath(file.path(getwd()), mustWork = TRUE)
out_dir <- file.path(root, "validation-results")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

summarise_mc <- function(x, truth) {
  x <- x[is.finite(x)]
  data.frame(n = length(x), truth = truth, mean = mean(x), bias = mean(x)-truth,
             rmse = sqrt(mean((x-truth)^2)), sd = stats::sd(x))
}

sim_lmm <- function(seed = 26081802, n_rep = 1000L) {
  set.seed(seed)
  beta0 <- 20; beta1 <- 1.4; sd_b <- 2.0; sd_e <- 1.5
  est <- rep(NA_real_, n_rep); se <- rep(NA_real_, n_rep); cover <- rep(NA, n_rep)
  for (r in seq_len(n_rep)) {
    id <- factor(rep(seq_len(30), each = 6)); x <- rep(0:5, 30)
    b <- stats::rnorm(30, 0, sd_b); y <- beta0 + beta1*x + b[id] + stats::rnorm(length(x), 0, sd_e)
    d <- data.frame(id=id, x=x, y=y)
    m <- try(lme4::lmer(y~x+(1|id), d, REML=TRUE), silent=TRUE)
    if (!inherits(m,"try-error")) {
      cf <- summary(m)$coefficients; est[r] <- cf["x","Estimate"]; se[r] <- cf["x","Std. Error"]
      cover[r] <- (est[r]-1.96*se[r] <= beta1) && (est[r]+1.96*se[r] >= beta1)
    }
  }
  cbind(summarise_mc(est,beta1), coverage=mean(cover,na.rm=TRUE), convergence=mean(is.finite(est)))
}

sim_robust <- function(seed = 26081804, n_rep = 1000L, contamination = 0.05) {
  if (!requireNamespace("robustlmm",quietly=TRUE)) return(data.frame(status="robustlmm_not_installed"))
  set.seed(seed); truth <- 1.2
  classic <- robust <- rep(NA_real_,n_rep)
  for(r in seq_len(n_rep)) {
    id <- factor(rep(seq_len(40),each=5)); x <- rep(0:4,40); b <- stats::rnorm(40,0,1.5)
    y <- 10 + truth*x + b[id] + stats::rnorm(length(x),0,1)
    contaminated <- sample(levels(id),ceiling(40*contamination)); y[id %in% contaminated] <- y[id %in% contaminated] + 10
    d <- data.frame(id=id,x=x,y=y)
    a <- try(lme4::lmer(y~x+(1|id),d),silent=TRUE); q <- try(robustlmm::rlmer(y~x+(1|id),d),silent=TRUE)
    if(!inherits(a,"try-error")) classic[r] <- lme4::fixef(a)["x"]
    if(!inherits(q,"try-error")) robust[r] <- lme4::fixef(q)["x"]
  }
  rbind(cbind(method="classical",summarise_mc(classic,truth)),cbind(method="robust",summarise_mc(robust,truth)))
}

sim_car1 <- function(seed = 26081808, n_rep = 1000L) {
  if (!requireNamespace("nlme",quietly=TRUE)) return(data.frame(status="nlme_not_installed"))
  set.seed(seed); beta <- 0.8; phi <- 0.65; est <- rep(NA_real_,n_rep)
  times <- c(0,1,2.5,4,7)
  for(r in seq_len(n_rep)) {
    id <- factor(rep(seq_len(25),each=length(times))); time <- rep(times,25)
    y <- numeric(length(time))
    for(i in seq_len(25)) {
      ii <- which(id==levels(id)[i]); C <- outer(times,times,function(a,b) phi^abs(a-b))
      e <- as.numeric(t(chol(C)) %*% stats::rnorm(length(times)))
      y[ii] <- 5 + beta*times + e
    }
    d <- data.frame(id=id,time=time,y=y)
    m <- try(nlme::lme(y~time,random=~1|id,correlation=nlme::corCAR1(form=~time|id),data=d),silent=TRUE)
    if(!inherits(m,"try-error")) est[r] <- nlme::fixef(m)["time"]
  }
  cbind(summarise_mc(est,beta),convergence=mean(is.finite(est)))
}

sim_qual_quant <- function(seed = 26081812, n_rep = 1000L) {
  set.seed(seed); opt_true <- c(A=90,B=110); est <- matrix(NA_real_,n_rep,2,dimnames=list(NULL,c("A","B")))
  for(r in seq_len(n_rep)) {
    block <- factor(rep(seq_len(8),each=8)); trt <- factor(rep(rep(c("A","B"),each=4),8)); dose <- rep(c(0,50,100,150),16)
    opt <- opt_true[as.character(trt)]; y <- 35 - 0.0015*(dose-opt)^2 + stats::rnorm(8)[block] + stats::rnorm(length(dose),0,1.5)
    d <- data.frame(block,trt,dose,y)
    m <- try(lme4::lmer(y~trt*poly(dose,2,raw=TRUE)+(1|block),d),silent=TRUE)
    if(!inherits(m,"try-error")) {
      grid <- expand.grid(dose=seq(0,150,length.out=301),trt=levels(trt)); grid$block <- block[1]
      pr <- stats::predict(m,newdata=grid,re.form=NA)
      for(g in levels(trt)) {ii<-which(grid$trt==g);est[r,g]<-grid$dose[ii[which.max(pr[ii])]]}
    }
  }
  rbind(cbind(group="A",summarise_mc(est[,"A"],opt_true["A"])),cbind(group="B",summarise_mc(est[,"B"],opt_true["B"])))
}

run_all <- function(n_rep = 1000L) {
  results <- list(
    SIM02_lmm = sim_lmm(n_rep=n_rep),
    SIM04_robust = sim_robust(n_rep=n_rep),
    SIM08_car1 = sim_car1(n_rep=n_rep),
    SIM12_qual_quant = sim_qual_quant(n_rep=n_rep)
  )
  saveRDS(results,file.path(out_dir,"mixedFlowR_validation_results.rds"))
  for(nm in names(results)) utils::write.csv(results[[nm]],file.path(out_dir,paste0(nm,".csv")),row.names=FALSE)
  invisible(results)
}

# Explicit execution only:
# results <- run_all(n_rep = 1000L)
