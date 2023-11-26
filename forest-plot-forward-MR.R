
library(TwoSampleMR)
library(dplyr)
library(metafor)
# from: https://github.com/mkanai/mr-forestplot

# specify path to where results are stored
output = "/path/to/output/"

# read in CM results from univariable MR
res_cm <- read.csv(paste0(output, 'forward/CM-SCZ/results_MR_CM-Shizophrenia_withOR-revision.csv'), row.names = 1)
# read in loneliness results from univariable MR             
res_loneliness <- read.csv(paste0(output, 'forward/Loneliness-SCZ/results_MR_Loneliness-Shizophrenia_withOR-revision.csv'), row.names = 1)
# read in MVMR results
mvmr <- read.csv(paste0(output, 'forward/MVMR/results_MVMR_loneliness-maltreatment-schizophrenia-revision.csv'), row.names = 1)
mvmr_egger <- read.csv(paste0(output, 'forward/MVMR/results_MVEgger_maltreatment-loneliness-schizophrenia-revision.csv'), row.names = 1)

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
df <- rbind(res_cm[, c('method', 'b','se', 'p2', 'pval')], mvmr[, c('method', 'b','se', 'p2', 'pval')][1,], mvmr_egger[, c('method', 'b','se', 'p2', 'pval')][1,], res_loneliness[, c('method', 'b','se', 'p2', 'pval')], mvmr[, c('method', 'b','se', 'p2', 'pval')][2,], mvmr_egger[, c('method', 'b','se', 'p2', 'pval')][2,])
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
for (i in 13:14) {
  df[i,4] <- '  Multivariable MR'
}

# plot output
# specify z
z <- qnorm((1 + 0.95) / 2, 0, 1)
pdf(paste0(output, "forest-uni-MR-revision.pdf"), width = 11.2, height = 11)
forest(
  df$b,
  sei = df$se,
  slab = sprintf("    %s", df$method), 
  cex = 1,
  xlab = "Odds ratio",
  annotate = FALSE,
  ilab = data.frame(
    sprintf("%.2f", exp(df$b)),
    sprintf("(%.2f, %.2f)", exp(df$b - z * df$se), exp(df$b + z * df$se)),
    sprintf("%.2e", df$pval)),
  ilab.xpos = c(4.3, 5.6, 7.1), # position of the three vertical columns on the right side
  pch = 16,
  at = log(c(0.5, 1, 2, 4, 8, 16, 32)), # log axis scale
  atransf = exp,
  rows = rev(c(1:2, 4:8, 11:12, 14:18)), # which rows of dataframe to portray 
  xlim = c(-5, 8), # x axis range
  ylim = c(0, 22.5) # y axis range
)

# plot disease labels
par(font=2)
text(-5, c(3, 9.2, 19.2, 13), pos=4, cex = 1, unique(rev(df$p2)))
# adjust the title spacing below
text(-5, c(21), pos=4, cex = 1, "Exposure                                                                                                                                               Odds ratio  (95% CI)      P-value")
dev.off()



