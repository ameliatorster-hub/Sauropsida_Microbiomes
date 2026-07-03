install.packages("DiagrammeR")
library(DiagrammeR)

install.packages("remotes") #installs github
library(remotes)

remotes::install_github("stopsack/khsmisc")

library(khsmisc)
library(stringr)

install.packages("dplyr")
library(dplyr)

install.packages(tibble)
library(tibble)


#information in place
design<-tibble::tribble(
  ~left,       ~n_left,~right,    ~n_right,
  "Study base",  72, "One Individual Sampled", 2,
  "Multiple Samplings", 70, "Sampling did not occur in the cloaca, intestines, colon, or fecal mater", 6,
  "Ending Sampling", 64, "", NA
)

plot(design, width=2)

grViz(paste0("diagraph flowchart{
    node[fontname=Helvectica, shape=rectangle, width=4]
             table1a[label='"))



