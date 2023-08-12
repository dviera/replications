library(tidyverse)
library(matrixStats)

#--------------------------
# get data
#--------------------------
data <- read.csv("results.csv")
data2 <- read.csv("results2.csv")

#--------------------------
# view data
#--------------------------
View(data)
View(data2)

#--------------------------
# table goals away vs home
#--------------------------
table(data$homegoal, data$awaygoal)

#--------------------------
#table 1 in the paper
#--------------------------
tapply(data2$goal, data2$team, mean) # average goals for
tapply(data2$goal, data2$opponent, mean) # average goals against

#--------------------------
# model
#--------------------------
m1 = glm(goal ~ home + team + opponent, family='poisson', data=data2, contrasts = list(
  team = "contr.sum", opponent = "contr.sum"))
summary(m1)

#--------------------------
#table 2 in the paper
#--------------------------
teams = unique(data$hometeam)
table_2 = data.frame(data.frame(teams[1:19]),data.frame(m1$coefficients[3:21], row.names = NULL), 
           data.frame(exp(m1$coefficients[3:21]), row.names = NULL),
           data.frame(m1$coefficients[22:40], row.names = NULL),
           data.frame(exp(m1$coefficients[22:40]), row.names = NULL))

colnames(table_2) <- c("Team", "Offensive_parameter", "Offensive_multiplier",
                       "Defensive_parameter", "Defensive_multiplier")

table_2 = rbind(table_2, data.frame(Team = "Wimbledon",
                          Offensive_parameter = -sum(table_2$Offensive_parameter),
                          Offensive_multiplier = exp(-sum(table_2$Offensive_parameter)),
                          Defensive_parameter = -sum(table_2$Defensive_parameter),
                          Defensive_multiplier = exp(-sum(table_2$Defensive_parameter))))

#--------------------------
# round the number to 2 as table 2
#--------------------------
table_2 = data.frame(table_2$Team, round(table_2[,2:5], 2))
colnames(table_2) <- c("Team", "Offensive_parameter", "Offensive_multiplier",
                       "Defensive_parameter", "Defensive_multiplier")

#==========================
# Plot not in the paper
#==========================
off_deff_plot <- ggplot(aes(Offensive_parameter, -Defensive_parameter), data = table_2) + 
  geom_point(size = 4, color = "#f96158") + 
  ggrepel::geom_text_repel(label=table_2$Team) + theme_light() +
  labs(title = "Premier League Season 95/96",
       x = "Offensive availability",
       y = "Deffensive availability",
       caption = expression(paste('Replicating the results from the paper ',
                                  italic('Lee, Alan J. "Modeling scores in the Premier League: is Manchester United really the best?." Chance 10, no. 1 (1997): 15-19.')))) +
  lluta_style()

finalize_lluta(plot_object = off_deff_plot, source_data = "", owner = "@lluta")

#--------------------------
# Prediction
#--------------------------
intercept = m1$coefficients[[1]]
home_adv = m1$coefficients[[2]]

# Man. U vs Liverpool - expected number of goals
# Man. U playing local
#a = exp(intercept)*exp(home_adv)*table_2[11, 3]
#b = exp(intercept)*table_2[9, 5]
a = exp(intercept + home_adv + table_2[11, 2] + table_2[9, 4])
b = exp(intercept  + table_2[9, 2] + table_2[11, 4])


mat_res = matrix(rep(NA, 8*8), nrow = 8, ncol = 8)
sum_wins = 0
sum_lost = 0
sum_tie = 0

for(i in 1:8) {
  for(j in 1:8) {
    mat_res[i,j] = a*b/(factorial(i-1)*factorial(j-1))
    
    if(i == j) { 
      sum_tie = sum_tie + mat_res[i,j] 
    } else if (i > j) { 
        sum_wins = sum_wins + mat_res[i,j] }
    else { sum_lost = sum_lost + mat_res[i,j] }
  }
}

#============================================
# simulate a match - check with Table 3
#============================================
sim_match <- function(home_team, away_team, n_sim = 1000){
  
  intercept = m1$coefficients[[1]]
  home_adv = m1$coefficients[[2]]
  
  ht <- table_2 %>% filter(Team == home_team)
  at <- table_2 %>% filter(Team == away_team)
  
  lambda_home = exp(intercept + home_adv + ht[[2]] + at[[4]])
  lambda_away = exp(intercept  + at[[2]] + ht[[4]])
  
  sim_home = rpois(n_sim, lambda_home)
  sim_away = rpois(n_sim, lambda_away)
  
  prob_win = mean((sim_home - sim_away) > 0)
  prob_draw = mean((sim_home - sim_away) == 0)
  prob_loss = mean((sim_home - sim_away) < 0)
  
  return(list("prob_win" = prob_win, "prob_draw" = prob_draw, "prob_loss" = prob_loss))
  
  
}

#=============================================
# simulate season
#=============================================

sim_season <- function(teams, n_sim = 1000){
  
  n_teams = length(teams)
  
  results = rep(0, n_teams)
  names(results) <- teams
  
  all_sim <- matrix(rep(NA, n_sim*n_teams), nrow = n_teams, ncol = n_sim)
  
  for(i in seq_along(1:n_sim)) {
    
    if(i %% 20 == 0) cat("iteration ", i, "\n")
  
    for(home in teams){
      
      for(away in teams){
        
        if(home != away){
          
          sim_result = sim_match(home, away, n_sim = 1)
          
          results[home] = results[home] + sum(unlist(sim_result) * c(3,1,0))
          results[away] = results[away] + sum(unlist(sim_result) * c(0,1,3))
            
        }
      }
      
    }
    
    for(r in seq_along(1:n_teams)){
      all_sim[r, i] = results[teams[r]]
    }
    
    results = rep(0, length(teams))
    names(results) <- teams
    
  }
  
  return(all_sim)
}

sim_all <- sim_season(unique(data$hometeam), n_sim = 1000)

tb4 <- data.frame(Team = unique(data$hometeam), Simulation_mean = rowMeans(sim_all),
           Simulation_std = rowSds(sim_all))

tb4[order(tb4$Simulation_mean, decreasing = TRUE), ]

