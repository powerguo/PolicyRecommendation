This repo includes all code neccessary to reproduce the manuscript. 

Code folder: 

DataCollection.R includes all group level results and figures

IndividualResult.R includes all individual level results and figures

Appendix.R includes all figures and results in the SI

Data folder:

Data folder includes all anonymized data used for stage three of the study. Individual results include survey results from participants who participated in all three stages of the study. Group results include all voting results, recommendations, and ranked order list of all alternatives for each group. 

NB: Data folder do not include all participants who filled out the survey, as not all participants who filled out the survey were able to participate in all three stages of the study. 

Figures folder:

Include the figures in the manuscript main body.

ShinyApps folder: Include the source code for the 7 shiny apps used for this project. P1Vote - P4Vote are the voting platforms for each participant in the group stage.  MeanVariance and WeightedSum are the recommendation models for the group task. Survey is the preference elicitation survey for stage 1 task. To run the two recommendation models and the Survey platform locally on your workstation, make sure you change login credentials from the default to a google account you have access to (line 163-172 for MeanVariance and WeightedSum apps, line 306-312 for Survey app). When you launch the apps for the first time, it will ask for your google credentials in order to link with the tidyverse packages. 