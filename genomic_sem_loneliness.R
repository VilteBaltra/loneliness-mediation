
library("devtools")
install_github("MichelNivard/GenomicSEM")
require(GenomicSEM)

#munge the files
munge(c("Retro_prospective_meta_childhoodmaltreatment.txt", "MTAG_results.txt.gz", "schizophrenia.txt"), "w_hm3.snplist",trait.names=c("Childhood_Maltreatment",  "Socialisolation_Loneliness","Schizophrenia"), c(185414,445024,105318), info.filter = 0.9, maf.filter = 0.01)

#compute the genetic covariance matrix S, and its parameter variance covariance matrix V
Data <- ldsc(traits =c("Childhood_Maltreatment.sumstats","socialisolation_loneliness.sumstats","Schizophrenia.sumstats"),sample.prev = c(NA,NA,.63),population.prev = c(NA,NA,.01),ld ="eur_w_ld_chr/",wld="eur_w_ld_chr/" )

#define the model
MediationModel<- '
 #regressions
 V3~ c*V1 + b*V2
 V2~ a*V1
 #direct effect
 dir := c
 #indirect effect
 ind := a*b
 #total effect
 tot := a*b + c'

#run the model using the user defined function
Mediationoutput <-usermodel(Data, estimation = "DWLS", model = MediationModel)

#print the output
Mediationoutput

# correlation plot
M <- round(cov2cor(Data$S), 2)
colnames(M) <- c("CM", "Isol/lon", "SCZ")
rownames(M) <- c("CM", "Isol/lon", "SCZ")
print(M)

#install.packages("corrplot")
library(corrplot)
corrplot(M, method="color",
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

