
library(TwoSampleMR)
library(dplyr)
library(metafor)
# from: https://github.com/mkanai/mr-forestplot

# specify path to where results are stored
output = "/path/to/output/"

# read in CM results from univariable MR
res_cm <- read.csv(paste0(output, 'reverse/SCZ-CM/results_MR_Shizophrenia-CM_withOR-revision.csv'), row.names = 1)
# read in loneliness results from univariable MR             
res_loneliness <- read.csv(paste0(output, 'reverse/SCZ-loneliness/results_MR_Schizophrenia-Loneliness_withOR-revision.csv'), row.names = 1)
# read in MVMR results
mvmr <- read.csv(paste0(output, 'reverse/reverse-MVMR/results_reverse_MVMR_schizophrenia-maltreatment-revision.csv'), row.names = 1)
mvmr <- mvmr %>% filter(exposure=='Schizophrenia')
mvmr_egger <- read.csv(paste0(output, 'reverse/reverse-MVMR/results_reverse_MVEgger_schizophrenia-maltreatment-revision.csv'), row.names = 1)
mvmr_egger <- mvmr_egger %>% filter(exposure=='exposure_2')

# add 'p2' and 'method' columns 
res_cm$p2 <- 'Maltreatment'
res_loneliness$p2 <- 'Loneliness'
mvmr$p2 <- paste0("MVMR:", mvmr$exposure)
mvmr_egger$p2 <- paste0("MR Egger:", mvmr$exposure)
mvmr$method <- "Inverse variance weighted"
mvmr_egger$method <- "MR Egger"


# create a vector with letters in the desired order and order cm and loneliness results
x <- c("Inverse variance weighted",  "MR Egger", "Weighted median", "Simple mode", "Weighted mode")
res_cm <- res_cm %>% slice(match(x, method))
res_loneliness <- res_loneliness %>% slice(match(x, method))

#select relevant columns and add to df
df <- rbind(res_cm[, c('method', 'b','se', 'p2', 'pval')], mvmr[, c('method', 'b','se', 'p2', 'pval')][1,], mvmr_egger[, c('method', 'b','se', 'p2', 'pval')][1,], res_loneliness[, c('method', 'b','se', 'p2', 'pval')])
df

# add ab extra column with univariable and mulrivariable MR specifications
for (i in 1:5) {
  df[i,4] <- 'Childhood Maltreatment
  
  Univariable MR'
}
for (i in 6:7) {
  df[i,4] <- '  Multivariable MR'
}
for (i in 8:12) {
  df[i,4] <- 'Loneliness / Isolation
  
  Univariable MR'
}


# plot output
# specify z
z <- qnorm((1 + 0.95) / 2, 0, 1)
pdf(paste0(output, "forest-uni-MR-reverse.pdf"), width = 10, height = 10)
forest(
  df$b,
  sei = df$se,
  slab = sprintf("    %s", df$method), 
  cex = 1,
  xlab = "Beta",
  annotate = FALSE,
  ilab = data.frame(
    sprintf("%.2f", df$b),
    sprintf("(%.2f, %.2f)", df$b - z * df$se, df$b + z * df$se),
    sprintf("%.2e", df$pval)),
  ilab.xpos = c(0.25, 0.33, 0.44), # position of the three vertical columns on the right side
  pch = 20, # was 16
  at = c(-0.10,-0.05, 0.0, 0.05, 0.10, 0.15),
  #at = log(c(0.605, 0.778, 1, 1.285, 1.650, 2.12)), # 2.72, 4.50
  # at = log(c(0.5, 1, 2, 4, 8, 16, 32)), # log axis scale
  # atransf = exp,
  #rows = rev(c(1:2, 4:8, 11:12, 14:18)), # which rows of dataframe to portray 
  rows = rev(c(4:8, 11:12, 14:18)), # which rows of dataframe to portray 
  xlim = c(-0.35, 0.5), # x axis range
  ylim = c(4, 22.5) # y axis range (starting value and end value)
)
# plot disease labels
par(font=2)
text(-0.35, c(9.2, 13, 19.2), pos=4, cex = 1, unique(rev(df$p2)))
# adjust the title spacing below
text(-0.35, c(21), pos=4, cex = 1, "Outcome                                                                                                                            Beta    (95% CI)           P-value")
dev.off()


