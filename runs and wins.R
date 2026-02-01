library(tidyverse)
library(lahman)

my_teams <- Teams %>%
	filter(yearID > 2000) %>%
	select(teamID, yearID, lgID, G, W, L, R, RA)
my_teams %>%
	tail()

my_teams <- my_teams %>%
	mutate(RD = R - RA, Wpct = W/(W+L))

run_diff <- ggplot(my_teams, aes(x=RD, y = Wpct)) + 
geom_point() + 
scale_x_continuous("Run differential") + 
scale_y_continuous("Winning percentage")

linfit <- lm(Wpct ~ RD, data= my_teams)
linfit

library(broom)
my_teams_aug <- augment(linfit, data= my_teams)

base_plot <- ggplot(my_teams_aug, aes(x = RD, y = .resid)) +
geom_point(alpha=0.3)+
geom_hline(yintercept=0, linetype = 3)+
xlab("Run differential") + ylab("Residual")

highlight_teams <- my_teams_aug %>%
	arrange(desc(abs(.resid))) %>%
	head(4)

library(ggrepel)
base_plot + 
geom_point(data = highlight_teams, color = "blue") + 
geom_text_repel(data=highlight_teams, color= "blue", aes(label = paste(teamID, yearID)))

resid_summary <- my_teams_aug %>%
summarize( N=n(), avg = mean(.resid), RMSE = sqrt(mean(.resid^2)))
resid_summary

rmse <- resid_summary %>%
pull(RMSE)

my_teams_aug %>%
summarize (N=n(), 
	within_one = sum(abs(.resid)< rmse),
	within_two = sum(abs(.resid) < 2* rmse)) %>%
mutate(within_one_pct = within_one/N, within_two_pct = within_two/N)

my_teams <- my_teams %>% 
mutate(Wpct_pyt = R ^2/ (R^2 + RA^2))

my_teams <- my_teams %>%
mutate(residuals_pyt = Wpct - Wpct_pyt)

my_teams <- my_teams %>%
summarize(rmse=sqrt(mean(residuals_pyt^2)))