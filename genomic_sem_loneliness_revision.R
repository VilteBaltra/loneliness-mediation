
library("devtools")
#install_github("MichelNivard/GenomicSEM")
require(GenomicSEM)

#munge the files
munge(c("Retro_prospective_meta_childhoodmaltreatment.txt.gz", "MTAG_results.txt", "PGC3_SCZ_wave3.european.autosome.public.v3.vcf.tsv.gz"), "w_hm3.snplist",trait.names=c("Childhood_Maltreatment", "Socialisolation_Loneliness","Schizophrenia2"), c(185414,445024,NA), info.filter = 0.9, maf.filter = 0.01)
# sample size set as NA for Schizophrenia2 because SNP-specific sum of effective sample sizes are present (NEFF = 58749.13)
# prior to running munge(), I renamed FCON column (frequency of A1 in controls) to MAF in schizophrenia sumstats. 
# munge() will convert it to MAF, as it makes sure MAF is actually MAF by running file$MAF<-ifelse(file$MAF <= .5, file$MAF, (1-file$MAF)) (i.e., allows max value of .5 or less)

#compute the genetic covariance matrix S, and its parameter variance covariance matrix V
Data <- ldsc(traits =c("Childhood_Maltreatment.sumstats.gz","socialisolation_loneliness.sumstats.gz","Schizophrenia2.sumstats.gz"),sample.prev = c(NA,NA,.41),population.prev = c(NA,NA,.01),ld ="eur_w_ld_chr/",wld="eur_w_ld_chr/")
# schizophrenia2 sample prevalence: 53,386 individuals with schizophrenia and 77,258 control individuals, so 53386 / (53386 + 77258) = 0.408637

#define the hypothesised model
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

#define the reverse model (scz --> loneliness --> maltreatment)
ReverseMediationModel<- '
 #regressions
 V1~ c*V3 + b*V2
 V2~ a*V3
 #direct effect
 dir := c
 #indirect effect
 ind := a*b
 #total effect
 tot := a*b + c'

#run the model using the user defined function
Mediationoutput <-usermodel(Data, estimation = "DWLS", model = MediationModel)

#run the model using the user defined function
Reverse_Mediationoutput <-usermodel(Data, estimation = "DWLS", model = ReverseMediationModel)

#print the output
Mediationoutput # CM --> Isol/lon --> SCZ
Reverse_Mediationoutput # SCZ --> Isol/lon --> CM

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

