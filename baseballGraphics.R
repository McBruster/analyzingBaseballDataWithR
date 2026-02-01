library(remotes)
library(tidyverse)
library(Lahman)
library(abdwr3edata)
library(baseballr)
library(ggrepel)
remotes::install_github("beanumber/abdwr3edata")

hof <- hof_batting

hof <- hof %>%
 mutate(MidCareer = (From + To) / 2,
	Era = cut(MidCareer, 
		breaks= c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
		labels= c("19th Century","Dead Ball","Lively Ball","Integration",
				"Expansion","Free Agency","Long Ball")))

hof_eras <- summarize(group_by(hof, Era), N=n())

ggplot(hof, aes(x=Era)) + geom_bar()

ggplot(hof, aes(x=Era)) + 
	geom_bar() +
	xlab("Baseball Era") +
	ylab("Frequency") +
	ggtitle("Era of the Nonpitching Hall of Famers")

ggplot(hof_eras, aes(Era, N)) + 
	geom_point() +
	xlab("Baseball Era") +
	ylab("Frequency") +
	ggtitle("Era of the Nonpitching Hall of Famers") +
	coord_flip()

ggplot(hof, aes(x=Era)) + 
	geom_bar() +
	xlab("Baseball Era") +
	ylab("Frequency") +
	ggtitle("Era of the Nonpitching Hall of Famers")
ggsave("bargraph.png")

one-dimensional scatterplot no formating
ggplot(hof, aes(x=OPS, y=1)) +
	geom_jitter()

formated one-dimensional scatterplot
ggplot(hof, aes(x=OPS, y=1)) +
geom_jitter(height= 0.6) + ylim(-1,3) +
theme(axis.title.y= element_blank(),
	axis.text.y= element_blank(),
	axis.ticks.y= element_blank())+
 coord_fixed(ratio=0.03)

histogram
ggplot(hof, aes(x=OPS)) + 
	geom_histogram()
ggsave("ops_histogram.png")

histogram with specified bins
ggplot(hof, aes(x=OPS)) + 
	geom_histogram(breaks = seq(0.4, 1.2, by =0.1),
	color= "blue", fill="white")
ggsave("ops_histogram_specified_bins.png")

scatterplot
ggplot(hof, aes(MidCareer, OPS)) +
	geom_point() +
	geom_smooth()

scatterplot with ids
library(ggrepel)
ggplot(hof, aes(MidCareer, OPS)) +
	geom_point() +
	geom_smooth() +
	geom_text_repel(data= filter(hof, OPS > 1.05 | OPS < .5),
				aes(MidCareer, OPS, label = Player))

Scatterplot OPS and SLG
(p<- ggplot(hof, aes(OBP, SLG)) + geom_point())

scatterplot OPS and SLG with labels
(p <- p +
	xlim(0.35, 0.50) +
	xlab("On Base Percentage") +
	ylab("Slugging Percentage"))

could use the following to get the same result

p<- p+ 
	scale_x_continuous("On Base Percentage",
				  limits= c(0.25, 0.50))+
	scale_y_continuous("Slugging Percentage",limits= c(0.28,0.75))

scatter plot ops, slugging, overlayed with ops
(p <- p + geom_abline(slope = -1,
 intercept = seq(.07,1, by = 0.1)))

adding more labels
p + annotate("text",
	x = rep(.27,4) , y = c(.42, .52, .62, .72),
	label = paste("OPS = ", c(0.7, 0.8, 0.9, 1.0)))

could create a dataframe for labels

ops_labels <- tibble(
	OBP = rep(0.3,4),
	SLG = seq(0.4,0.7, by=0.1),
	label = paste("OPS =", OBP + SLG))
p + geom_text (data=ops_labels, hjust= "right",
	aes(label=label))



numberic variable and factor variable
hof <- mutate(hof, hr_rate = HR/AB)

parallel stripchart
ggplot(hof, aes(hr_rate, Era)) +
geom_jitter(height = 0.1)
ggsave("parallel_stripchart_hr_rate_era.png")

parallel boxplots
ggplot(hof, aes(Era, hr_rate)) +
geom_boxplot() + coord_flip()

verticle boxplot 
ggplot(hof, aes(Era, hr_rate)) +
geom_boxplot()

library(Lahman)

get_birthyear <- function(Name){
       Names <- unlist(strsplit(Name, " "))
       People %>%
              filter(nameFirst == Names[1],
                     nameLast == Names[2]) %>%
              mutate(birthyear = ifelse(birthMonth >=7,
                                        birthYear + 1, birthYear),
              Player = paste(nameFirst, nameLast))%>%
                     select(playerID, Player, birthyear)}

PlayerInfo <- bind_rows(get_birthyear("Babe Ruth"),
                        get_birthyear("Hank Aaron"),
                        get_birthyear("Barry Bonds"),
                        get_birthyear("Alex Rodriguez"))

Batting %>%
  inner_join(PlayerInfo, by="playerID")%>%
  mutate(Age = yearID - birthyear) %>%
  select(Player, Age, HR) %>%
  group_by(Player)%>%
  mutate(CHR = cumsum(HR)) -> HRdata

ggplot(HRdata, aes(x = Age, y = CHR, linetype = Player)) +
 geom_line()


retro1998 <- read_rds(here::here("data/retro1998.rds"))

data1998 <- read_csv("C:/Users/mcbru/Documents/Baseball analysis book/gl1998.txt")

C:\Users\mcbru\Documents\Baseball analysis book

abdwr3edata::parse_retrosheet_pbp


retrosheet_data(
  path_to_directory = NULL,
  years_to_acquire = 1998,
  sequence_years = FALSE
)

sosa_id <- People |>
+   filter(nameFirst == "Sammy", nameLast == "Sosa") |>
+   pull(retroID)
> sosa_id

mac_id <- People %>%
+ filter(nameFirst == "Mark", nameLast =="McGwire") %>%
+ pull(retroID)


mac_id <- People |>
filter(nameFirst == "Mark", nameLast == "McGwire") |>
pull(retroID)

hr_race<- data1998 %>% filter(batter %in% c(sosa_id, mac_id))

library(lubridate)
cum_hr <- function(d) {
  d %>%
	mutate(Date = ymd(str_sub(gid, 4, 11))) %>%
	arrange(Date) %>%
	mutate(HR =ifelse(hr == 1, 1, 0),
		 cumHR = cumsum(HR)) %>%
	select(Date, cumHR)}
hr

hr_ytd <- hr_race %>%
  split(pull(. , batter)) %>%
  map_df(cum_hr, .id="batter") %>%
  inner_join(People, by =c("batter" = "retroID"))

ggplot(hr_ytd, aes(Date, cumHR, linetype = nameLast)) +
  geom_line()+
  geom_hline(yintercept = 62, color= "blue")+
  annotate("text", ymd("1998-04-15"), 65,
           label = "62", color = "blue")+
  ylab("Home Runs in the Season")


> data(package = "abdwr3edata")
> hofpitching <- hof_pitching
> hofpitching <- hofpitching %>% 
+ mutate(BF.group = cut(BF, c(0, 10000, 15000, 20000, 30000),
+ labels = c("Less than 10000","(1000, 15000)", "(15000, 20000)","more than 20000")))
> head(hofpitching)

pitch <- summarize(group_by(hofpitching, BF.group), N=n())

ggplot(hofpitching , aes(x= BF.group)) + geom_bar()

ggplot(pitch , aes(BF.group, N))+
geom_point()

ggplot(hofpitching , aes(x=WAR))+
geom_histogram()+
	xlab("WAR") +
	ylab("Count") +
	ggtitle("Hall of fame pitchers WAR")


ggplot(hofpitching , aes(x=WAR))+
	geom_histogram()+
	geom_text_repel(data= filter(hofpitching , WAR > 100),
				aes(WAR, W, label = ...2))

hofpitching<- hofpitching %>% mutate(WAR.Season = WAR/Yrs)

ggplot(hofpitching, aes(WAR.Season, BF.group)) +
geom_jitter(height=0.1)+
coord_fixed(ratio = 2)


ggplot(hofpitching, aes(WAR.Season, BF.group)) +
geom_boxplot()+
coord_flip()
coord_fixed(ratio = 2)