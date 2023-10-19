library(rvest)
library(ggplot2)

Wave_Stats <- "https://fbref.com/en/squads/bf961da0/San-Diego-Wave-Stats"
NWSL1 <- read_html(Wave_Stats)
Team1_Stats <- html_text(html_elements(NWSL1,"#all_stats_standard .right , #all_stats_standard .center , #all_stats_standard .left"))
Team1_Stats[1:964]

Team1_Names <- html_text(html_elements(NWSL1,"tbody th"))
Team1_Names[1:279]
Team1_Ages <- html_text(html_elements(NWSL1,"tbody .center+ .center"))
Team1_Minutes <- html_text(html_elements(NWSL1,"tbody .right:nth-child(7)"))
Team1_Goals <- html_text(html_elements(NWSL1, "tbody .group_start:nth-child(9)"))

Thorns_Stats <- "https://fbref.com/en/squads/df9a10a1/Portland-Thorns-FC-Stats"
NWSL2 <- read_html(Thorns_Stats)
Team2_Stats <- html_text(html_elements(NWSL2, ".adblock+ .filter h4 , #all_stats_standard .left , #all_stats_standard .left a , #all_stats_standard .right , #all_stats_standard .center"))
Team2_Stats[1:900]

Team2_Names <- html_text(html_elements(NWSL2, "tbody th"))
Team2_Names[1:249]
Team2_Ages <- html_text(html_elements(NWSL2, "tbody .center+ .center"))
Team2_Minutes <- html_text(html_elements(NWSL2, "tbody .right:nth-child(7)"))
Team2_Goals <- html_text(html_elements(NWSL2, "tbody .group_start:nth-child(9)"))

Courage_Stats <- "https://fbref.com/en/squads/85c458aa/North-Carolina-Courage-Stats"
NWSL3 <- read_html(Courage_Stats)
Team3_Stats <- html_text(html_elements(NWSL3, "#all_stats_standard .center , #all_stats_standard .right , #all_stats_standard .left"))
Team3_Stats[1:896]

Team3_Names <- html_text(html_elements(NWSL3, "tbody th"))
Team3_Names[1:269]
Team3_Ages <- html_text(html_elements(NWSL3, "tbody .center+ .center"))
Team3_Ages
Team3_Minutes <- html_text(html_elements(NWSL3, "tbody .right:nth-child(7)"))
Team3_Minutes
Team3_Goals <- html_text(html_elements(NWSL3, "tbody .group_start:nth-child(9)"))
Team3_Goals
team1_data <- data.frame(
  Name = Team1_Names[1:279],
  Age = Team1_Ages[1:279],
  Minutes = Team1_Minutes[1:279],
  Goals = Team1_Goals[1:279]
)
team1_data <- na.omit(team1_data)

team2_data <- data.frame(
  Name = Team2_Names[1:249],
  Age = Team2_Ages[1:249],
  Minutes = Team2_Minutes[1:249],
  Goals = Team2_Goals[1:249]
)
team2_data <- na.omit(team2_data)

team3_data <- data.frame(
  Name = Team3_Names[1:269],
  Age = Team3_Ages[1:269],
  Minutes = Team3_Minutes[1:269],
  Goals = Team3_Goals[1:269]
)
team3_data <- na.omit(team3_data)


total_Data <- rbind(team1_data,team2_data,team3_data)
total_Data$Age <- as.numeric(total_Data$Age)

age_distribution <- ggplot(total_Data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Age Distribution of NWSL Players (San Diego Wave, Portland Thorns FC,
       North Carolina Courage)", x = "Age", y = "Frequency"
  )

age_distribution

total_Data$Minutes <- as.numeric(total_Data$Minutes)
total_Data$Goals <- as.numeric(total_Data$Goals)

goal_Distribution_by_MinutesPlayed <- ggplot(total_Data, aes(x=Minutes, y = Goals)) +
  geom_point(size = 2, alpha = 0.8) + labs( title = "Relationship Between
    Minutes Played and Goals Scored", x = "Minutes Played", y = "Goals Scored"
  )

specific_Player <- subset(total_Data, Name == "Sofia Jakobsson")

goal_Distribution_by_MinutesPlayed <- goal_Distribution_by_MinutesPlayed +
  geom_point(data = specific_Player, aes(x = Minutes, y = Goals), size = 4,
  color = "purple") + geom_text(data = specific_Player, aes(label = Name),
  hjust = 0.5, vjust = -0.5, size = 3)

goal_Distribution_by_MinutesPlayed

