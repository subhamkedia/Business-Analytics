setwd("~/Desktop/Assignment-5")
email = read.csv("emails.csv")
head(email)

prob_1 = nrow(email[email$column.1 == 1,])/10000
prob_2 = nrow(email[email$column.2 == 1,])/10000
prob_3 = nrow(email[email$column.3 == 1,])/10000
prob_4 = nrow(email[email$column.4 == 1,])/10000
prob_5 = nrow(email[email$column.5 == 1,])/10000

## Pure Exploration ##
select_arm_PureExploration = function(t){
  if(t%%5==0){
    return(5)
  }
  else {
    return(t%%5)
  }
}

update.value = function(chosen_arm, reward, values, counts) {
  counts[chosen_arm] = counts[chosen_arm] + 1
  n = counts[chosen_arm]
  value = values[chosen_arm]
  new_value = ((n-1)/n*value + (1/n)*reward)
  values[chosen_arm] = new_value
  return(list(values, counts))
}

PureExploration = function(means, T = 10000) {
  n_arms = length(means)
  counts = rep(1, n_arms)
  values = rep(1, n_arms)
  chosen.vec = rep(0, T)
  value.vec = rep(0, T)
  
  for(t in 1:T) {
    chosen_arm = select_arm_PureExploration(t)
    reward = email[t, chosen_arm]
    updates = update.value(chosen_arm, reward, values, counts)
    values = updates[[1]]
    counts = updates[[2]]
    chosen.vec[t] = chosen_arm
    value.vec[t] = values[chosen_arm]
  }
  return(cbind(arm = chosen.vec, value = value.vec)) 
}
means1 = c(prob_1, prob_2, prob_3, prob_4, prob_5)
results1 = PureExploration(means1)
regret_PExplore = numeric(nrow(results1))
for (i in 1:nrow(results1)) {
  regret_PExplore[i] = i*max(means1) - sum(results1[1:i,2])
}
plot(regret_PExplore, type = "l", xlab = "iteration", ylab="Regret for Pure Exploration")

## Pure Exploitation ##
select_arm_PureExploitation1 = function(t){
  if(t%%5==0){
    return(5)
  }
  else {
    return(t%%5)
  }
}
select_arm_PureExploitation2 = function(values){
  return(which.max(values))
}

update.value = function(chosen_arm, reward, values, counts) {
  counts[chosen_arm] = counts[chosen_arm] + 1
  n = counts[chosen_arm]
  value = values[chosen_arm]
  new_value = ((n-1)/n*value + (1/n)*reward)
  values[chosen_arm] = new_value
  return(list(values, counts))
}

PureExploitation = function(means, T = 10000) {
  n_arms = length(means)
  counts = rep(0, n_arms)
  values = rep(0, n_arms)
  chosen.vec = rep(0, T)
  value.vec = rep(0, T)
  
  for(t in 1:T) {
    if(max(values)==0){
      chosen_arm = select_arm_PureExploitation1(t)
    }
    else{
      chosen_arm = select_arm_PureExploitation2(values)
    }
    reward = email[t, chosen_arm]
    updates = update.value(chosen_arm, reward, values, counts)
    values = updates[[1]]
    counts = updates[[2]]
    chosen.vec[t] = chosen_arm
    value.vec[t] = values[chosen_arm]
  }
  return(cbind(arm = chosen.vec, value = value.vec)) 
}

means2 = c(prob_1, prob_2, prob_3, prob_4, prob_5)
results2 = PureExploitation(means2)
regret_PExploit = numeric(nrow(results2))
for (i in 1:nrow(results2)) {
  regret_PExploit[i] = i*max(means2) - sum(results2[1:i,2])
}
plot(regret_PExploit, type = "l", xlab = "iteration", ylab="Regret for Pure Exploitation")

## Exploration and Exploitation ##
select_arm_ExploreExploit1 = function(t){
  if(t%%5==0){
    return(5)
  }
  else {
    return(t%%5)
  }
}

select_arm_ExploreExploit2 = function(values){
  return(which.max(values))
}

update.value = function(chosen_arm, reward, values, counts) {
  counts[chosen_arm] = counts[chosen_arm] + 1
  n = counts[chosen_arm]
  value = values[chosen_arm]
  new_value = ((n-1)/n*value + (1/n)*reward)
  values[chosen_arm] = new_value
  return(list(values, counts))
}

ExploreExploit = function(means, T = 10000) {
  n_arms = length(means)
  counts = rep(1, n_arms)
  values = rep(1, n_arms)
  chosen.vec = rep(0, T)
  value.vec = rep(0, T)
  
  for(t in 1:T) {
    if(t<=100){
      chosen_arm = select_arm_ExploreExploit1(t)
      }
    else{
      chosen_arm = select_arm_ExploreExploit2(values)
      }
    reward = email[t, chosen_arm]
    updates = update.value(chosen_arm, reward, values, counts)
    values = updates[[1]]
    counts = updates[[2]]
    chosen.vec[t] = chosen_arm
    value.vec[t] = values[chosen_arm]
  }
  return(cbind(arm = chosen.vec, value = value.vec)) 
}

means3 = c(prob_1, prob_2, prob_3, prob_4, prob_5)
results3 = ExploreExploit(means3)
regret_ExploreExploit = numeric(nrow(results3))
for (i in 1:nrow(results3)) {
  regret_ExploreExploit[i] = i*max(means3) - sum(results3[1:i,2])
}
plot(regret_ExploreExploit, type = "l", xlab = "iteration", ylab="Regret for Explore than Exploit")

## Epsilon Greedy ##
select_arm_EpsilonGreedy = function(epsilon, values) {
  if (runif(1) > epsilon) {  
    return(which.max(values)) 
  } else { return(sample(1:length(values), 1)) }
}

update.value = function(chosen_arm, reward, values, counts) {
  counts[chosen_arm] = counts[chosen_arm] + 1
  n = counts[chosen_arm]
  value = values[chosen_arm]
  new_value = ((n-1)/n*value + (1/n)*reward)
  values[chosen_arm] = new_value
  return(list(values, counts))
}

EpsilonGreedy = function(means, epsilon, T = 10000) {
  n_arms = length(means)
  counts = rep(1, n_arms)
  values = rep(1, n_arms)
  chosen.vec = rep(0, T)
  value.vec = rep(0, T)
  
  for(t in 1:T) {
    chosen_arm = select_arm_EpsilonGreedy(epsilon, values)
    reward = email[t, chosen_arm]
    updates = update.value(chosen_arm, reward, values, counts)
    values = updates[[1]]
    counts = updates[[2]]
    chosen.vec[t] = chosen_arm
    value.vec[t] = values[chosen_arm]
  }
  return(cbind(arm = chosen.vec, value = value.vec)) 
}
means3 = c(prob_1, prob_2, prob_3, prob_4, prob_5)
results4 = EpsilonGreedy(means3, 0.1)
regret_EG = numeric(nrow(results4))
for (i in 1:nrow(results4)) {
  regret_EG[i] = i*max(means3) - sum(results4[1:i,2])
}
plot(regret_EG, type = "l", xlab = "iteration", ylab="Regret for Epsilon Greedy")

## Upper Confidence Bounding (UCB) ##
select_arm_ucb = function(values, counts, alpha, t) {
  counts[counts==0] = 1e-12
  ucb_scores = values + alpha*sqrt(log(t)/counts)
  return(which.max(ucb_scores))
}

update.value = function(chosen_arm, reward, values, counts) {
  counts[chosen_arm] = counts[chosen_arm] + 1
  n = counts[chosen_arm]
  value = values[chosen_arm]
  new_value = ((n-1)/n*value + (1/n)*reward)
  values[chosen_arm] = new_value
  return(list(values, counts))
}

UCB = function(means, alpha = 0.75, T = 10000) {
  n_arms = length(means)
  counts = rep(1, n_arms)
  values = rep(1, n_arms)
  chosen.vec = rep(0, T)
  value.vec = rep(0, T)
  
  for(t in 1:T) {
    chosen_arm = select_arm_ucb(values, counts, alpha, t)
    reward = email[t, chosen_arm]
    updates = update.value(chosen_arm, reward, values, counts)
    values = updates[[1]]
    counts = updates[[2]]
    chosen.vec[t] = chosen_arm
    value.vec[t] = values[chosen_arm]
  }
  return(cbind(arm = chosen.vec, value = value.vec)) 
}

means5 = c(prob_1, prob_2, prob_3, prob_4, prob_5)
results5 = UCB(means5, alpha = 0.75, T = 10000)
regret_UCB = numeric(nrow(results5))
for (i in 1:nrow(results5)) {
  regret_UCB[i] = i*max(means5) - sum(results5[1:i,2])
}
plot(regret_UCB, type = "l", xlab = "iteration", ylab="Regret for UCB")

plot(regret_UCB, type = "l", xlab = "iteration", ylim = c(0,500), col='red', ann = FALSE)
par(new=TRUE)
plot(regret_EG, type = "l", xlab = "iteration",ylim = c(0,500), col='orange',ann = FALSE)
par(new=TRUE)
plot(regret_PExplore, type = "l", xlab = "iteration",ylim = c(0,500), col='blue',ann = FALSE)
par(new=TRUE)
plot(regret_PExploit, type = "l", xlab = "iteration", ylim=c(0,500), col='green',ann = FALSE)
par(new=TRUE)
plot(regret_ExploreExploit, type = "l", xlab = "iteration", ylim = c(0,500), col='purple',ann = FALSE)

legend(1,500, legend=c('UCB','Epsilon Greedy','pure Explore','Pure Exploit','Explore&Exploit'),
       col=c('red','orange','blue','green','purple'), lty = c(1,1,1,1,1), lwd=c(3,3,3,3,3))
title(xlab="iteration",ylab="Regret")

hist(results5[1:100,1], breaks = c(0,1,2,3,4,5), xlab ="Results")
hist(results5[9900:10000,1], breaks = c(0,1,2,3,4,5), xlab ="Results")

#Answer-2)a)
morning_demand = rnorm(10000, mean = 50, sd = 10)
evening_demand = runif(10000, min= 60, max = 80)
total_demand = morning_demand + evening_demand
hist(total_demand)
quantile(total_demand, c(0.1, 0.5,0.9))

#Answer-2)b)
sell_price = 4
cost_price = 1
exp_profit = c()
for (i in (1:10000)){
  if(total_demand[i]<120){
    exp_profit[i] = ((total_demand[i]*sell_price) - (120*cost_price))
  }
  else{
    exp_profit[i] = ((120*sell_price) - (120*cost_price))
  }
}
expected_profit = mean(exp_profit)
expected_profit
hist(exp_profit)

#Answer-2)c)
opt_profit = c()
range = c(70:160)
for (i in 1:length(range)){
  temp_profits = numeric(10000)
  for (j in 1:10000){
    if(total_demand[j]<range[i]){
      temp_profits[j] <- total_demand[j]*sell_price - range[i]*cost_price
      }
    else{
      temp_profits[j] <- range[i]*sell_price - range[i]*cost_price
    }
  }
  opt_profit[i]  = mean(temp_profits)
}

plot(opt_profit)

optimal_profit = opt_profit[which.max(opt_profit)]
optimal_profit
optimal_production = which.max(opt_profit) + 70
optimal_production