# Bootstrap testing
if (F) {
  
  # Used: Trt
  
  dat <- read.csv("C:/Users/avike/OneDrive/Desktop/Avi/Research/Immune correlates pipeline/Figures + Tables/Moderna data/P3001ModernaCOVEimmunemarkerdata_correlates_processed_v1.1_lvmn_added_Jan14_2022.csv")
  nrow(dat)
  dat <- dat[dat$Trt==1,]
  nrow(dat)
  dat <- dat[dat$Perprotocol==1,]
  nrow(dat)
  dat <- dat[dat$ph1.D29==1,]
  nrow(dat)
  
  # Unused
  Bserostatus
  Perprotocol
  EventIndPrimaryD29
  SubcohortInd
  tps.stratum
  Wstratum
  TwophasesampIndD29
  wt.D29
  ph1.D29
  ph2.D29
  
  xtabs(~Wstratum, data=dat)
  xtabs(~tps.stratum, data=dat)
  xtabs(~Wstratum+tps.stratum, data=dat)
  xtabs(~Wstratum+tps.stratum+EventIndPrimaryD29, data=dat)
  
  
  tmp <- with(dat, table(Wstratum, ph2.D29))
  weights <- rowSums(tmp)/tmp[,2]
  
}

# Confirm equivalence of old and new transformations
if (F) {
  
  old_lims <- function(r, n, z, sigma) {
    expit(logit(r)+c(-1,1)*n^(-1/2)*z*deriv_logit(r)*sigma)
  }
  
  new_lims <- function(r, n, z, sigma) {
    (1 + (1/r-1)*exp((c(1,-1)*n^(-1/2)*z*sigma)/(r*(1-r))))^-1
  }
  
  both <- function(s, n, z, sigma) {
    print(old_lims(s, n, z, sigma))
    print(new_lims(s, n, z, sigma))
    invisible()
  }
  
  both(s=0.5, n=100, z=1.96, sigma=2)
  both(s=0.5, n=100, z=1.96, sigma=4)
  both(s=0.5, n=500, z=1.96, sigma=2)
  both(s=0.5, n=500, z=1.96, sigma=4)
  both(s=0.1, n=100, z=1.96, sigma=2)
  both(s=0.1, n=100, z=1.96, sigma=4)
  both(s=0.1, n=500, z=1.96, sigma=2)
  both(s=0.1, n=500, z=1.96, sigma=4)
  
}
