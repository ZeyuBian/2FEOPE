dgp=function(scenario){
  if (scenario=='Scenario 1') {
    source("~/Desktop/JASA_Sims/Sensitivity/Setting 1/mixdata.R")
    source("~/Desktop/JASA_Sims/Sensitivity/Setting 1/target.R")
  } else if (scenario=='Scenario 2') {
    source("~/Desktop/JASA_Sims/Sensitivity/Setting 2/mixdata.R")
    source("~/Desktop/JASA_Sims/Sensitivity/Setting 2/target.R")
  } else if (scenario=='Scenario 3') {
    source("~/Desktop/JASA_Sims/Sensitivity/Setting 3/mixdata.R")
    source("~/Desktop/JASA_Sims/Sensitivity/Setting 3/target.R")
  } else if (scenario=='Scenario 4') {
    source("~/Desktop/JASA_Sims/Sensitivity/Setting 4/mixdata.R")
    source("~/Desktop/JASA_Sims/Sensitivity/Setting 4/target.R")
  } 
}