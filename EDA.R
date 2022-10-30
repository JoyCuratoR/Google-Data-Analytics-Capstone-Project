library(tidyverse)


df <- read_csv("D:/R_Studio/Project_Vault/OP_GHGE/data/PartneredFacilities.csv")

spec(df) # checked the column specification and saw that 2020, 2019 is text
str(df)

 # checking for duplicates
duplicated(df)
sum(duplicated(df))

# 1 Converting the columns 2020, 2019 from chr to num
df$Emissions2019 <- as.numeric(as.character((df$Emissions2019)))
str(df)
sapply(df, class)

df$Emissions2020 <- as.numeric(as.character((df$Emissions2020)))
str(df)
sapply(df, class)

# 2 adding in a column of the results of the 10% decrease calculation 
df <- df |>
  mutate(Goal = round(((df$Emissions2020 - df$Emissions2019) / df$Emissions2019) * 100, digits = 2))

# 3 column based on other column:
df <- df |>
  mutate(YesNo = case_when(Goal <= - 10 ~ "1", TRUE ~ "0"))

df <- df |> 
  mutate(PosNegNum = case_when(Goal < 0 ~ "0", TRUE ~ "1"))


# visualizing Percentage of Emissions Decreased & Increased from 2019 - 2020
#by state

#df |> 
#select(State, Goal) |>
#ggplot(aes(x = State, y = Goal)) +
#geom_bar(stat = "identity", fill = "gold") + 
#scale_y_continuous() +
#geom_hline(yintercept = 0) +
#labs(title = "Percentage of Emissions Decreased and Increased by State",
#subtitle = "Between 2019 - 2020",
#y = "Percentage of Emissions") +
#annotate("text", x = 3.5, y = -500, 
# label = "CA, NY, TX decreased the most emissions", size = 5) +
#annotate("text", x = 3.5, y = 250, 
#label ="CA, TX, TN increased the most emissions", size = 5)


 # changing YesNo and PosNegNum column to numeric
colnames(df)
sapply(df, class)
df$YesNo <- as.numeric(as.character(df$YesNo))
df$PosNegNum <- as.numeric(as.character(df$PosNegNum))

# 5 how many facilities in each state met the Partners program goal?
df |> 
  select(State, YesNo) |>
  count(YesNo, State) |>
  ggplot(aes(x = State, y = n, fill = factor(YesNo))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs( y = "Number of Facilities",
        title = "Partnered Facilities that Met the Partner Program's Goal") +
  guides(fill = guide_legend(title = "Met the Goal")) +
  ylim(0, 100) +
  scale_fill_manual(values = c("gray80", "gold"),
                    labels = c('No', 'Yes'))
  
# 6 which facility had the highest percent of emissions decreased?
df |> 
  select(FacilityId, Goal) |>
  arrange(Goal)

df |> 
  select(FacilityId, IndustryTypeSectors, Goal) |>
  filter(FacilityId == "1010345")

# 7 which facility had the lowest percent of emissions decreased? 
df |> 
  select(FacilityId, Goal, PosNegNum) |>
  filter(PosNegNum == "0") |> 
  arrange(-Goal)

df |> 
  select(FacilityId, IndustryTypeSectors, Goal) |>
  filter(FacilityId == "1006873")

# 8 Of the facility that produced the highest emissions in '19...
df |>
  select(FacilityId, Emissions2019, Goal) |>
  arrange(-Emissions2019) |>
  head(5)

# 9 Average percent of emissions decreased
df |>
  summarize(Avg = mean(Goal[which(Goal < 0)], na.rm = TRUE))

 # by state
Avg_state <- df |> 
  group_by(State) |>
  summarize(Neg = round(mean(Goal[which(Goal < 0)], na.rm = TRUE), digits = 2),
            Pos = round(mean(Goal[which(Goal > 0)], na.rm = TRUE), digits = 2)) |>
              mutate(Highlight2 = ifelse(Pos == 40.83, "yes", "no")) |>
              mutate(Highlight = ifelse(Neg == -17.35, "yes", "no"))

Avg_state |>
  ggplot(aes(x = reorder(State, -Neg), y = Neg, fill = Highlight)) +
  geom_bar(stat = "identity") +
  ylim(-50, 50) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = Neg), vjust = 2, color = "black") +
  labs(x = "State", y = "Average Percent", 
       title = "Average Percent of Emissions Decreased by State",
       subtitle = "Between 2019-2020") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values =  c("yes" = "gold", "no" = "gray80"), guide = "none")

 # Avg Percentage of Emissions Increased
Avg_state|>
  ggplot(aes(x = reorder(State, -Pos), y = Pos, fill = Highlight2)) +
  geom_bar(stat = "identity") +
  ylim(-50, 50) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = Pos), vjust = -1, color = "black") +
  labs(x = "State", y = "Average Percent", 
       title = "Average Percent of Emissions Increased by State",
       subtitle = "Between 2019-2020") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values =  c("yes" = "gold", "no" = "gray80"), guide = "none")

# 10 What is the max, min percentage of emissions decreased?

 # min
df |>
  summarize(max = max(Goal[which(Goal < 0)], na.rm = TRUE))
# remember, it's for decreased emissions so we're looking for the neg num closest
#to zero

 # max 
df |>
  summarize(min = min(Goal[which(Goal < 0)], na.rm = TRUE))
# we're looking for the number furthers from zero


# 11 which states had the highest and lowest percentage decrease?
State <- df |> 
  group_by(State) |>
  summarize(Neg = sum(Goal[which(Goal < 0)], na.rm = TRUE),
            Pos = sum(Goal[which(Goal > 0)], na.rm = TRUE)) |>
  mutate(Highlight = ifelse(Pos == 358.79, "yes", "no")) |>
  mutate(Highlight = ifelse(Neg == -730.71, "yes", "no"))


ggplot(State, aes(x = reorder(State, -Neg), y = Neg, fill = Highlight)) +
  geom_bar(stat = "identity") +
  ylim(- 800, 400) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = Neg), vjust = 2, parse = T) +
  labs(x = "State", y = "Percent", 
       title = "Sum Percent of Emissions Decreased by State",
       subtitle = "Between 2019-2020") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values =  c("yes" = "gold", "no" = "gray80"), guide = "none")

 # Sum Percentage of Emissions highest and lowest emissions Increased? 

ggplot(State, aes(x = reorder(State, - Pos), y = Pos, fill = Highlight)) +
  geom_bar(stat = "identity") +
  ylim(- 800, 400) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = Pos), vjust = -1) +
  labs(x = "State", y = "Percent", 
       title = "Sum Percent of Emissions Increased by State",
       subtitle = "Between 2019-2020") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values =  c("yes" = "gold", "no" = "gray80"), guide = "none")

# 12 by sector
Sector <- df |> 
  group_by(IndustryTypeSectors) |>
  summarize(Neg = round(sum(Goal[which(Goal < 0)], na.rm = TRUE)),
            Pos = round(sum(Goal[which(Goal > 0)], na.rm = TRUE))) |>
  mutate(Highlight = ifelse(Neg == -398, "yes", "no")) |>
  mutate(Highlight2 = ifelse(Pos == 375, "yes", "no"))

# only neg by sectors
Sector |>
  filter(!is.na(Neg)) |>
  ggplot(aes(x = IndustryTypeSectors, y = Neg,
             fill = Highlight)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  ylim(-400, 400) +
  scale_x_discrete() +
  theme(legend.position = "none") +
  scale_fill_manual(values =  c("yes" = "gold", "no" = "gray80"), guide = "none") +
  labs(title = "Sectors with the Highest and Lowest Percent of Emissions Decreased",
       subtitle = "Between 2019-2020",
       x = "Sector", y = "Percent Decreased") +
  geom_text(aes(label = ifelse(Neg == "-398", "-398", "")), 
            vjust = 0.5, hjust = -1, color = "black")
  
  # only pos by Sectors
Sector |>
  filter(!is.na(Neg)) |>
  ggplot(aes(x = IndustryTypeSectors, y = Pos,
             fill = Highlight2)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  ylim(-400, 400) +
  scale_x_discrete() +
  theme(legend.position = "none") +
  scale_fill_manual(values =  c("yes" = "gold", "no" = "gray80"), guide = "none") +
  labs(x = "Sector", y = "Percent Increased") + 
  geom_text(aes(label = ifelse(Pos == "375", "375", "")), 
          vjust = 0.5, hjust = 1.2, color = "black")


# 13 Highest Increased Emissions Sum and Avg by County 
County <- df |> 
  group_by(County) |>
  summarize(Neg = round(sum(Goal[which(Goal < 0)], na.rm = TRUE)),
            Pos = round(sum(Goal[which(Goal > 0)], na.rm = TRUE))) |>
  arrange(County) |>
  mutate(Highlight = ifelse(Neg == -377, "yes", "no")) |>
  mutate(Highlight2 = ifelse(Pos == 197, "yes", "no"))
  
 # Pos 
County |>
  ggplot(aes(x = County, y = Pos,
             fill = Highlight2)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  ylim(-400, 250) +
  scale_x_discrete() +
  theme(legend.position = "none") +
  scale_fill_manual(values =  c("yes" = "gold", "no" = "gray80"), guide = "none") +
  labs(x = "County", y = "Percent Increased") + 
  geom_text(aes(label = ifelse(Pos == "197", "197", "")), 
            vjust = 0.5, hjust = -1, color = "black")

 # Dec
County |>
  ggplot(aes(x = County, y = Neg,
             fill = Highlight)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  ylim(-400, 250) +
  scale_x_discrete() +
  theme(legend.position = "none") +
  scale_fill_manual(values =  c("yes" = "gold", "no" = "gray80"), guide = "none") +
  labs(x = "County", y = "Percent Increased") + 
  geom_text(aes(label = ifelse(Neg == "-377", "-377", "")), 
            vjust = 0.5, hjust = -1, color = "black")
