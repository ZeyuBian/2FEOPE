dgp=function(scenario){
  if (scenario=='Scenario 1') {
    source("~/Desktop/JASA_Sims/Mixture/Scenario 1/mixdata.R")
    source("~/Desktop/JASA_Sims/Mixture/Scenario 1/target.R")
  } else if (scenario=='Scenario 2') {
    source("~/Desktop/JASA_Sims/Mixture/Scenario 2/mixdata.R")
    source("~/Desktop/JASA_Sims/Mixture/Scenario 2/target.R")
  } else if (scenario=='Scenario 3') {
    source("~/Desktop/JASA_Sims/Mixture/Scenario 3/mixdata.R")
    source("~/Desktop/JASA_Sims/Mixture/Scenario 3/target.R")
  } else{
    source("~/Desktop/JASA_Sims/Mixture/Scenario 4/mixdata.R")
    source("~/Desktop/JASA_Sims/Mixture/Scenario 4/target.R")
  } 
}