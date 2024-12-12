#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("ggrepel")

library(ggplot2)
library(tidyverse)
library(ggrepel)

setwd("~/IdeaProjects/lvivjavaclub/rlang")

v1 <- c(1, 1, 1, 1)
v2 <- c(1, 2, 3, 4)
v3 <- c(1, -1)

x <- 5
typeof(x)
typeof(v1)

mx1 <- matrix(c(1, 2, 3, 4, 5), nrow = 3, ncol = 2)
mx1 + mx1

df <- data.frame(
  Language = c("Java", "Go", "JavaScript", "Rust", "Python"),
  Projects = c(150, 50, 130, 20, 200),
  Openings = c(100, 200, 50, 40, 90)
)

ggplot(df, aes(x = Projects, y = Openings)) + geom_point(aes(color=Language), size=5)

rm(x, v1, v2, v3, mx1, df)

events <- read.csv("events.csv")
backlog <- read.csv("backlog.csv")
videos <- read.csv("videos.csv")

tags_count <- events %>%
  mutate(tags = str_remove_all(tags, "\\[|\\]|'")) %>%
  separate_rows(tags, sep = ", \\s*") %>%
  count(tags, name = "count") %>%
  slice_max(count, n = 10)

ggplot(tags_count, aes(x=reorder(tags, -count), y=count)) +
  geom_bar(stat="identity", aes(fill=tags)) +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, size = 15))
ggsave("tags.png", width=10, height=8)

length(events_with_speaker$Number)
events_with_speaker <- events %>%
  mutate(Number=str_extract(title, "(?<=Event#)\\d+")) %>%
  filter(!is.na(Number)) %>%
  mutate(Number = as.numeric(Number)) %>%
  left_join(backlog %>% filter(!(User %in% c("Video", "TBD", "")) & Top != "Тривога") %>% select(Number, User), by = "Number") %>%
  filter(!is.na(User))

speaker_summary <- events_with_speaker %>%
  mutate(tags = str_remove_all(tags, "\\[|\\]|'") %>% str_split(", \\s*")) %>%
  unnest(tags) %>%
  group_by(User) %>%
  summarise(
    num_events = n_distinct(Number),
    num_tags = n_distinct(tags)
  )

ggplot(speaker_summary, aes(x = num_events, y=num_tags)) +
  geom_point(size=3, color="darkred") +
  geom_text_repel(aes(label=User)) +
  labs(
    title = "Активність спікерів",
    x = "Кількість івентів",
    y = "Унікальні теми"
  ) + theme_classic()

ggsave("speakers.png", width=10, height=8)


videos_summary <- videos %>%
  mutate(Video.publish.time = as.Date(Video.publish.time, format = "%b %d, %Y"))

ggplot(videos_summary, aes(x=Video.publish.time, y=Views)) + ylim(0, 410) + 
  geom_point(color = ifelse(videos$Views >= sort(videos$Views, decreasing = TRUE)[4], "green", "darkred")) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkgreen") +
  labs(
    title="Кількість переглядів відео",
    x="Опубліковано",
    y="Кількість переглядів"
  ) + theme_bw()

ggsave("video_views.png", width=10, height=8)
