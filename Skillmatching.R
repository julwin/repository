library(data.tree)
library(stringdist)
library(plotly)
library(IRdisplay)
library(reshape2)

skillDB <- read.csv("skillsDB.csv", sep=";")
AnforderungenDB <- read.csv("skills.csv", sep=";")

Kandidat <-AnforderungenDB[sample(nrow(AnforderungenDB),20, replace=F),]

for (n in Kandidat) {
  print(tolower(n))
}

skillDB_lowercase <- data.frame(sapply(skillDB, tolower))
similarity_matrix <- data.frame(stringdistmatrix(tolower(Kandidat), tolower(skillDB_lowercase$Skill),method = c("jw"), useNames="strings"))
plot_ly(z=similarity_matrix, type="heatmap", name="test", showscale=F)

install.packages("mlbench", repos="https://cran.uni-muenster.de/")
library(plotly)
library(mlbench)

# Get Sonar data
data(Sonar)

# Use only numeric data
rock <- as.matrix(subset(Sonar, Class == "R")[,1:59])
mine <- as.matrix(subset(Sonar, Class == "M")[,1:59])

# For rocks
p1 <- plot_ly(z = rock, type = "heatmap", showscale = F)

# For mines
p2 <- plot_ly(z = mine, type = "heatmap", name = "test") %>%
  layout(title = "Mine vs Rock")

# Plot together
subplot(p1, p2)

