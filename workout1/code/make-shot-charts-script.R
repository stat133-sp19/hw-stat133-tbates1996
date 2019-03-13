library(ggplot2)
library(dplyr)
library(jpeg)
library(grid)

klay_scatterplot <- ggplot(data = thompson) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag))

# court image (to be used as background of plot)
court_file <- "../images/nba-court.jpg"
# create raste object
court_image <- rasterGrob( readJPEG(court_file), width = unit(1, "npc"), height = unit(1, "npc"))

# Klay shot 
klay_shot_chart <- ggplot(data = thompson) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') + theme_minimal()

klay_shot_chart

ggsave("../images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5, units = "in")

# Steph shot 
steph_shot_chart <- ggplot(data = curry) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') + theme_minimal()

steph_shot_chart

ggsave("../images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5, units = "in")

# Draymond shot 
draymond_shot_chart <- ggplot(data = green) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') + theme_minimal()

draymond_shot_chart

ggsave("../images/draymond-green-shot-chart.pdf", width = 6.5, height = 5, units = "in")

# KD shot 
kd_shot_chart <- ggplot(data = durant) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') + theme_minimal()

kd_shot_chart

ggsave("../images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5, units = "in")

# Andre shot 
andre_shot_chart <- ggplot(data = iguodala) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') + theme_minimal()

andre_shot_chart

ggsave("../images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5, units = "in")

# GSW shot 
team_shot_chart <- ggplot(data = team) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: GSW (2016 Season)') + theme_minimal()

team_shot_chart + facet_wrap(~ name)

ggsave("../images/gsw-shot-chart.pdf", width = 8, height = 7, units = "in")
ggsave("../images/gsw-shot-chart.png", width = 8, height = 7, units = "in")