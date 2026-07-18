install.packages("DiagrammeR")
library(DiagrammeR)

install.packages("remotes") #installs github
library(remotes)

remotes::install_github("stopsack/khsmisc")

library(khsmisc)
library(stringr)

install.packages("dplyr")
library(dplyr)

install.packages("tibble")
library(tibble)

install.packages("DiagrammeRsvg")
library(DiagrammeRsvg)
install.packages("rsvg")
library(rsvg)

#information in place
design<-tibble::tribble(
  ~left,       ~n_left,~right,    ~n_right,
  "Study base",  72, "One Individual Sampled", 2,
  "Multiple Samplings", 70, "Sampling did not occur in the cloaca,\nintestines, colon, or fecal mater", 6,
  "Ending Sampling", 64, "", NA
)



flowchart <- grViz(paste0("digraph flowchart{
graph[
  rankdir=TB,
  label='Study Criteria',
  labelloc='t',
  fontsize=20,
  fontname=Helvetica
]

    node[fontname=Helvetica, shape=rectangle, width=4, height=1]
tab1a [group=1, label = '", design$left[1], "\\nn = ", design$n_left[1], "']
tab1b [group=2, label = '", design$right[1], "\\nn = ", design$n_right[1], "']

tab2a [group=1,label = '", design$left[2], "\\nn = ", design$n_left[2], "']
tab2b [group=2,label = '", design$right[2], "\\nn = ", design$n_right[2], "']

tab3a [group=3,label = '", design$left[3], "\\nn = ", design$n_left[3], "']

tab4a [width=1, label='Lizard\nn = 12']
tab4b [width=1,label='Snakes\nn = 9']
tab4c [width=1,label='Crocodilia\nn = 9']
tab4d [width=1,label='Turtles\nn = 11']
tab4e [width=1,label='Passerine Birds\nn = 11']
tab4f [width=1,label='Non Passerine Birds\nn = 5']

{rank=same; tab4a; tab4b; tab4c; tab4d; tab4e; tab4f;}

tab3a -> tab4a
tab3a -> tab4b
tab3a -> tab4c
tab3a -> tab4d
tab3a -> tab4e
tab3a -> tab4f

             {rank=same;tab1a;tab1b;}
             {rank=same;tab2a;tab2b;}


tab1a -> tab1b [label= 'Exclusion', fontsize=10]
tab1a -> tab2a
tab2a -> tab2b [label= 'Exclusion', fontsize=10]
tab2a -> tab3a

tab1b -> tab2b [style=invis]

}
"))


svg <- export_svg(flowchart)

rsvg_pdf(
  charToRaw(svg),
  file = "/Users/amelia/Documents/Reserach_Microbiome_Work/Sauropsida_Microbiomes/Study_Criteria.pdf"
)


