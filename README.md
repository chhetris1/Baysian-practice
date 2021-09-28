# Baysian-practice
echo "# Baysian-practice" >> README.md
git init
git add README.md
git commit -m "first commit"
git branch -M main
git remote add origin git@github.com:chhetris1/Baysian-practice.git
git push -u origin main


prev <- 0.00025
N <- 10^5
outcome <- sample(c("infected", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))
N_infected <- sum(outcome == "infected")
N_healthy <- sum(outcome == "Healthy")
#for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome =="infected"] <- sample(c("+", "-"), N_infected, replace = TRUE, prob = c(accuracy, 1-accuracy) )
test[outcome == "Healthy"] <- sample(c("-", "+"), N_healthy, replace = TRUE, prob = c(accuracy, 1-accuracy))
table(outcome, test)
1019/10^5
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+")| is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100- rawpoll_trump/100)
head(polls)
nrow(polls)
one_poll_per_pollster <- polls %>% 
  group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()
one_poll_per_pollster
results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread)))%>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
results

#computing the posterior mean, se, credible interval and prob
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))
posterior_mean
posterior_se
#95% credible interval 
posterior_mean + c(-1.96, 1.96)*posterior_se
# simulated data with X(j) = d + E(j)
J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
#now add i element
I <- 5
X <- sapply(1:I, function(i){
  d+rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
#now add h element (i.e. house effect)
h <- rnorm(I, 0, 0.025)
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
#calculating probability of d>0 with general bias
results_us_election_2016 %>% arrange(desc(electoral_votes))%>%
  top_n(5, electoral_votes)

results <- polls_us_election_2016 %>%
  filter(state != "U.S." & !grepl("CD", state)&
           enddate >= "2016-10-31" & 
           (grade %in% c("A+", "A", "A-", "B+")|is.na(grade)))%>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)%>%
  group_by(state) %>%
  summarise(avg = mean(spread), sd = sd(spread), n= n()) %>%
  mutate(state = as.character(state))
results %>% arrange(abs(avg))
results <- left_join(results, results_us_election_2016, by= "state")
results
results_us_election_2016 %>% filter(!state %in% results$state)
results <- results %>% mutate(sd =ifelse(is.na(sd), median(results$sd, na.rm = TRUE ), sd))
results
View(results)
mu <- 0
tau <- 0.02
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

#select all national polls by one pollster 
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
#observed standard error is higher than theory predicts 
se <- one_pollster %>% 
  summarise(empirical = sd(spread), 
            theoritcal = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))

se

#the distribution of the data is not normal 
one_pollster %>% ggplot(aes(spread))+
  geom_histogram(binwidth = 0.01, color = "black")

#trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>% 
  filter(n()>=10) %>%
  ungroup() %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)%>%
  ggplot(aes(enddate, spread))+
  geom_smooth(method = "loess", span = 0.1)+
  geom_point(aes(color = pollster), show.legend = F, alpha=0.6)

#plotting raw % across time
polls_us_election_2016 %>%
  filter(state =="U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump)%>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>% 
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>% 
  group_by(pollster) %>% 
  filter(n() >= 10) %>%
  ungroup() %>% 
  ggplot(aes(enddate, percentage, color = candidate))+
  geom_point(show.legend = F, alpha =.4)+
  geom_smooth(method = "loess", span = 0.15)+
  scale_y_continuous(limits = c(30,50))

#empirical Bayes election forecasting
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A-", "A", "B+")|is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>% 
  ungroup()
results <- one_poll_per_pollster %>% 
  summarise(avg = mean(spread), se = sd(spread)/sqrt(length(spread ))) %>% 
  mutate(start = avg -1.96*se, end = avg + 1.96*se)
results

#computing posterior mean, se, credible interval and prob
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 /(sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))
posterior_mean
posterior_se
#95% credible interval 
posterior_mean +c(-1.96, 1.96)*posterior_se
#probability of d>0
1 - pnorm(0, posterior_mean, posterior_se)

#simulated data with X(j) = d + E(j)
J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
p
X
#simulated data with X(i,j) = d + E(i,j)
I <- 5
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
X
#simulated data with X(i,j)=d+h(i)+E(i,j)
h <- rnorm(I, 0, 0.025) #standard error of posllter-to-pollster variability is 0.025, assumed
X <- sapply(1:I, function(i){
  d +rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
X  
#prob of d>0 with general bias 
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + 0.025^2)
Y <- results$avg
B <- sigma^2/(sigma^2 +tau^2)
posterior_mean <- B*mu+(1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))
1 - pnorm(0, posterior_mean, posterior_se)

#t distribution 
z <- qt(0.975, nrow(one_poll_per_pollster)-1)
z
one_poll_per_pollster %>% 
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - moe, end = avg + moe)
#quantile from t-distribution vs. normal distribution 
qt(0.975, 14) #14 = nrow(one_poll_per_pollster) -1
qnorm(0.975)
data("research_funding_rates")
research_funding_rates
#compute totals that were succesful or not
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarise_all(funs(sum))%>% 
  summarise(yes_men = awards_men, no_men = applications_men - awards_men, 
            yes_women = awards_women, no_women = applications_women - awards_women)
totals
#compare percentages of men/women with awards 
totals %>% summarize(percent_men = yes_men/(yes_men+no_men), percent_women = yes_women/(yes_women+no_women))

#lady tasting tea problem
tab <- matrix(c(3,1,1,3), 2,2)
tab
rownames(tab)<- c("poured before", "poured after")
colnames(tab)<- c("guessed before", "guessed after")
tab 
#p-value calculation with Fisher Exact test 
fisher.test(tab, alternative = "greater")

funding_rate <- totals %>% 
  summarise(percent_total = (yes_men + yes_women)/(yes_men+yes_women+no_men+no_women))%>%
  .$percent_total
funding_rate
two_by_two <- tibble(awarded = c('no', 'yes'), 
                     men = c(totals$no_men, totals$yes_men), 
                     women = c(totals$no_women, totals$yes_women))

two_by_two
#compute null hypothesis two by two table 
null <- tibble(awarded = c("no", "yes"), 
               men = (totals$no_men +totals$yes_men)*c(1-funding_rate, funding_rate ), 
               women = (totals$no_women+totals$yes_women)*c(1-funding_rate, funding_rate))
null
#chi-squared test 
chisq_test <- two_by_two %>% select(-awarded) %>% chisq.test()
chisq_test$p.value
#odds of getting funding for men 
odds_men <- (two_by_two$men[2]/sum(two_by_two$men))/(two_by_two$men[1]/sum(two_by_two$men))
odds_men
odds_women <- (two_by_two$women[2]/sum(two_by_two$women))/(two_by_two$women[1]/sum(two_by_two$women))
#odds ration- how many times larger odds are for men than women 
odds_men/odds_women
two_by_two %>% select(-awarded) %>% mutate(men = men*10, women = women*10) %>%
  chisq.test()


