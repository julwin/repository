
if (!require("stringdist")) {
   install.packages("stringdist", repos="https://cran.uni-muenster.de/")
   library(stringdist)
   }

if (!require("plotly")) {
   install.packages("plotly", repos="https://cran.uni-muenster.de/")
   library(plotly)
   }

if (!require("RColorBrewer")) {
   install.packages("RColorBrewer", repos="https://cran.uni-muenster.de/")
   library(RColorBrewer)
   }

if (!require("gplots")) {
   install.packages("gplots", dependencies = TRUE,repos="https://cran.uni-muenster.de")
   library(gplots)
   }

if (!require("tm")) {
   install.packages("tm", dependencies = TRUE,repos="https://cran.uni-muenster.de")
   library(tm)
   }

if (!require("SnowballC")) {
   install.packages("SnowballC", dependencies = TRUE,repos="https://cran.uni-muenster.de")
   library(SnowballC)
   }

library(reshape2)
library(dplyr)

AnforderungenDB <- read.csv("skillsDB.csv", sep=";")

KenntnisseDB <- read.csv("skills.csv", sep=";")

dim(AnforderungenDB)

dim(KenntnisseDB)

head(AnforderungenDB)

head(KenntnisseDB)

Kandidat <-KenntnisseDB[sample(nrow(KenntnisseDB),20, replace=F),]
Kandidat

Anforderungen <- AnforderungenDB[sample(nrow(AnforderungenDB), 10, replace=F),]
Anforderungen

Anforderungen$Skill <- sapply(Anforderungen$Skill, tolower)
Anforderungen

Kandidat <- tolower(Kandidat)
Kandidat

Anforderungen$Skill <- gsub("-", " ", Anforderungen$Skill)

Kandidat <- gsub("-", " ", Kandidat)

corpus_anf <- VCorpus(VectorSource(Anforderungen$Skill))
corpus_kand <- VCorpus(VectorSource(Kandidat))

corpus_anf <- tm_map(corpus_anf, removeWords, stopwords("german"))
corpus_kand <- tm_map(corpus_kand, removeWords, stopwords("german"))

corpus_anf <- tm_map(corpus_anf, stemDocument, language="german")
corpus_kand <- tm_map(corpus_kand, stemDocument, language="german")

anf_data <- data.frame(lapply(corpus_anf, as.character))
kand_data <- data.frame(lapply(corpus_kand, as.character))

anf_data <- t(anf_data)
kand_data <- t(kand_data)

anf_data

kand_data

rownames(anf_data)<- rownames(Anforderungen)
colnames(anf_data) <- "Skill"
rownames(kand_data)<- rownames(Kandidat)
colnames(kand_data) <- "Skill"

kand_data

Anforderungen$Skill <- anf_data

Anforderungen

Kandidat <- kand_data

similarity_matrix <- data.frame(stringdistmatrix(Anforderungen$Skill,
                                                 Kandidat,
                                                 method = c("jw"),
                                                 p=0.25,
                                                 q=2,
                                                 useNames="strings",
                                                useBytes = TRUE))

similarity_matrix

result <- t(sapply(seq(nrow(similarity_matrix)), function(i) {
  most_similar <- which.min(similarity_matrix[i,])
  c(paste(rownames(similarity_matrix)[i], colnames(similarity_matrix)[most_similar], sep=' & '),
    similarity_matrix[i,most_similar])
}))

result

typeof(result["Distanz"])

colnames(result) <- c("Skillpaar","Distanz")

result <- as.data.frame(result)
result$Distanz <- as.numeric(as.character(result$Distanz))
rownames(result) <- rownames(Anforderungen)

result

filter(result, Distanz<=0.20)
cat("Anzahl der gematchten Skills:", nrow(filter(result, Distanz<=0.20)))

filter(result, Distanz > 0.20, Distanz < 0.30)
cat("Diese Skills sollten überprüft werden:", nrow(filter(result, Distanz > 0.20, Distanz < 0.30)))

heatmap(as.matrix(similarity_matrix), Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), margins =c(10,10))



