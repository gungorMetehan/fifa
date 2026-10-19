# required packages
library(ggplot2)
library(dplyr)
library(patchwork)

# read the CSV file directly from GitHub
data_url <- paste0(
  "https://raw.githubusercontent.com/gungorMetehan/",
  "fifa/refs/heads/main/datasets/referees.csv"
)

referees_raw <- read.csv(
  data_url,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE
)

# required columns
required_columns <- c("year", "country", "n", "stage")

stopifnot(all(required_columns %in% names(referees_raw)))

# clean text fields and convert numeric fields
referees <- referees_raw %>%
  mutate(
    year = as.integer(year),
    country = trimws(country),
    n = as.numeric(n),
    stage = trimws(stage)
  )

# validate the values used in the analysis
valid_stages <- c("quarterfinals", "semifinals", "finals")

stopifnot(
  !anyNA(referees[required_columns]),
  all(nzchar(referees$country)),
  all(referees$n > 0),
  all(referees$n == floor(referees$n)),
  all(referees$stage %in% valid_stages)
)

# (optional) add the two verified quarter-final replay appointments
# keep the country naming convention used in the source dataset
replay_appointments <- data.frame(
  year = c(1934L, 1938L),
  country = c("Switzerland", "France"),
  n = c(1, 1),
  stage = c("quarterfinals", "quarterfinals")
)

# avoid adding the replay rows again if the source is updated
missing_replays <- replay_appointments %>%
  anti_join(
    referees,
    by = c("year", "country", "stage")
  )

referees <- bind_rows(referees, missing_replays)

# count appointments rather than rows or unique referees
data4plot <- referees %>%
  group_by(stage, country) %>%
  summarise(
    freq = sum(n),
    .groups = "drop"
  )

# print the total number of appointments for each stage
stage_totals <- data4plot %>%
  group_by(stage) %>%
  summarise(
    appointments = sum(freq),
    .groups = "drop"
  )

print(stage_totals)

# use a common count scale across all three plots
max_freq <- max(data4plot$freq)

# create a reusable lollipop plotting function
make_lollipop <- function(stage_name, stage_title, point_color) {
  
  # sort countries by frequency and alphabetically within ties
  plot_data <- data4plot %>%
    filter(stage == stage_name) %>%
    arrange(desc(freq), country)
  
  # place the highest-frequency country at the top
  plot_data$country <- factor(
    plot_data$country,
    levels = rev(plot_data$country)
  )
  
  ggplot(
    plot_data,
    aes(x = country, y = freq)
  ) +
    geom_segment(
      aes(
        xend = country,
        y = 0,
        yend = freq
      ),
      color = "grey",
      linewidth = 0.75
    ) +
    geom_point(
      color = point_color,
      size = 3.5
    ) +
    geom_text(
      aes(label = freq),
      nudge_y = 0.35,
      hjust = 0,
      size = 3.5,
      color = "#404040"
    ) +
    scale_y_continuous(
      breaks = seq(0, max_freq, by = 1),
      limits = c(0, max_freq + 1.5),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_flip() +
    theme_light() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(
        family = "AvantGarde",
        size = 12,
        color = "#404040"
      ),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 15
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 10
      ),
      plot.margin = margin(10, 15, 10, 10)
    ) +
    labs(
      title = stage_title,
      subtitle = paste(
        sum(plot_data$freq),
        "referee appointments"
      ),
      x = NULL,
      y = NULL
    )
}

# assign a different point color to each stage
p_quarterfinals <- make_lollipop(
  "quarterfinals",
  "Quarter-finals",
  "#003f5c"
)

p_semifinals <- make_lollipop(
  "semifinals",
  "Semi-finals",
  "#bc4c96"
)

p_finals <- make_lollipop(
  "finals",
  "Finals",
  "#ffa600"
)

# combine the plots
combined_plot <- (
  p_quarterfinals | p_semifinals | p_finals
) +
  plot_annotation(
    title = "FIFA World Cup Referee Appointments by Country",
    subtitle = paste0(
      min(referees$year),
      "–",
      max(referees$year)
    ),
    caption = paste(
      "Includes the 1934 and 1938 quarter-final replays.",
      "Counts represent on-field referee appointments.",
      "Data Visualization by Metehan Güngör",
      sep = "\n"
    ),
    theme = theme(
      text = element_text(
        family = "AvantGarde",
        color = "#404040"
      ),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 18
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 12
      ),
      plot.caption = element_text(
        hjust = 0,
        size = 8
      )
    )
  )

# display all three plots together
print(combined_plot)