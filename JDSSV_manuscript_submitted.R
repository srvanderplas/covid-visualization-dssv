## ----get-nyt-data, include = F----------------------------------------------------------------------------------------------------------------------------

# Read in the data from NYT
library(tidyverse)
library(lubridate)
library(zoo)
library(ggrepel)
library(gridExtra)
cvstate <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

if (!dir.exists("Data")) dir.create("Data")
write_csv(cvstate, file = "Data/covid-data-nyt.csv") # Save data just in case

cvstate <- cvstate %>%
  group_by(state, fips) %>%
  mutate(date_100th_case = min(date[cases >= 100]),
         days_100th_case = date - date_100th_case,
         daily_cases = cases - lag(cases, 1, default = 0),
         week_avg_cases = rollmean(cases, 7, align = "right", fill = c(0, NA, NA), na.pad = T, na.rm = T),
         daily_avg_cases = rollmean(daily_cases, 7, align = "right", fill = c(0, NA, NA), na.pad = T, na.rm = T),
         daily_deaths = deaths - lag(deaths, 1, default = 0),
         week_avg_deaths = rollmean(deaths, 7, align = "right", fill = c(0, NA, NA), na.pad = T, na.rm = T),
         daily_avg_deaths = rollmean(daily_deaths, 7, align = "right", fill = c(0, NA, NA), na.pad = T, na.rm = T)) %>%
  ungroup() %>%
  mutate(state = factor(state))

download.file("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx", destfile = "Data/census_pop_state.xlsx", quiet = T, mode = "wb")
pop_data <- readxl::read_xlsx("Data/census_pop_state.xlsx", col_names = c("state", "pop2019"), col_types = c("text", rep("skip", 11), "numeric"), skip = 9) %>%
  mutate(state = str_remove(state, "[[:punct:]]"))

cvstate <- left_join(cvstate, pop_data, by = "state")

my_theme <- theme_bw() + 
  theme(plot.background = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent", colour = NA), 
        legend.background = element_rect(fill = "transparent", colour = NA),
        axis.text.y = element_text(angle = 90, hjust = 0.5)) 
theme_set(my_theme)


## ----colourpal, include = F-------------------------------------------------------------------------------------------------------------------------------
# Including this for temporary compatibility - we may want to use *shudder* viridis?
main_colours <- c("#d00000", "#f5f1e7", "#c7c8ca")

# use as accents
secondary_colours <- c("#001226", "#249ab5")

# These are only to be used for infographics/charts
tertiary_colours <- c("#bccb2a", "#f58a1f", "#005d84", "#ffd74f", "#a5228d")

colour_palette <- c("#d00000", "#249ab5",  "#1B8500", "#f58a1f", "#a5228d", "#001226", "#B1EB66", "#005d84", "#c25700")

heat_colour_palette <- c("#ffd74f", "#f58a1f", "#c25700", "#d00000", "#9D0000", "#6A0000")

blueseqpal <- c("001226", "06293E", "0C3F56", "12566E", "186D85", "1E839D", 
                "249AB5", 
                "39ABC1", "4EBCCE", "64CDDA", "79DDE6", "8EEEF3", "A3FFFF") %>% 
  paste0("#", .) %>% rev()

blueseqpal_lt <- rev(c("#B2FFFF", "#C2FFFF", "#D1FFFF", "#E0FFFF", "#F0FFFF"))

darkblueseqpal <- colorspace::darken(blueseqpal, .2)
lightblueseqpal <- colorspace::lighten(blueseqpal, .2)


## ----log-scale-initial, fig.width = 8, fig.height = 4, out.width = "\\linewidth", fig.cap = "In the early stages of the pandemic, log scales allowed the comparison of raw case counts in locations with vastly different population and case counts."----
# remotes::install_github("AllanCameron/geomtextpath")
library(geomtextpath)

library(facetscales)
scales_y <- list(
  Log = scale_y_log10(),
  Linear = scale_y_continuous()
)
tmp <- cvstate %>% 
  filter(date < ymd("2020-06-01")) %>%
  filter(state %in% c("Michigan", "Washington", "New York", "California"))

p1 <-  tmp %>%
  ggplot(aes(x = date, y = daily_avg_cases, group = state)) +
  geom_textline(aes(colour = state, label = state, vjust = -0.1, hjust =.93) , text_smoothing = 50) + 
  ylab("Daily average cases (7 day average)") + 
  xlab("Date") + 
  guides(colour = "none") + 
  scale_y_log10() + 
  facet_grid(~"Log") + 
  theme_bw()

p2 <-  tmp %>%
  ggplot(aes(x = date, y = daily_avg_cases, group = state)) +
  geom_textline(aes(colour = state, label = state, vjust = -0.25, hjust = .85) , text_smoothing = 60) + 
  ylab("Daily average cases (7 day average)") + 
  xlab("Date") + 
  guides(colour = "none") + 
  facet_grid(~"Linear") + 
  theme_bw() +
  theme(axis.title.y = element_blank()) 

grid.arrange(p1, p2, nrow = 1)


## ----log-scale-failures, echo = F, fig.width = 8, fig.height = 4, out.width = ".95\\linewidth", message = F, error = F, warning = F, fig.cap = "One problem with log scales is that if there is a background level of spread, it can be hard to notice the introduction of an additional source of exponential spread. Linear scales do not have this problem - the exponential source is noticeable very quickly in the total line, but on the log scale it is much harder to discern when the exponential source causes the total line to diverge from the background. In the top-right corner, it is difficult to identify that there is an exponential increase in cases amid the baseline, even though the exponential source makes up approximately 50\\% of the cases at the end of the time period shown."----
# Set up labeller to tweak things 
rlistmap <- function(x, myfun) {
  if ("list" %in% class(x)) return(purrr::map(x, ~rlistmap(., myfun)))
  
  return(myfun(x))
}
fix_labels <- function(x) {
  x  %>%
    str_replace('scale: (.*)', "\\1 scale") %>%
    str_replace("baseline: (\\d{1,})", "Baseline:\n\\1 cases")
}
label_custom <- function(labels, multi_line = T, sep = ": ") {
  out <- label_both(labels, multi_line = multi_line, sep = sep)
  
  out <- rlistmap(out, fix_labels)

  out
}
class(label_custom) <- "labeller"

# Set up data 
growth <- rpois(30, 15*exp((1:30 - 20)/5))

tmp <- tibble(baseline = 15,
       x = 1:30,
       y = rpois(30, 15),
       y1 = growth) %>%
  bind_rows(
    tibble(baseline = 50,
       x = 1:30,
       y = rpois(30, 50),
       y1 = growth) 
  ) %>%
  bind_rows(
    tibble(baseline = 200,
       x = 1:30,
       y = rpois(30, 200),
       y1 = growth) 
  ) %>%
  pivot_longer(matches("y")) %>%
  mutate(name = str_replace_all(name, c("y$" = "Background", "y1" = "Exponential")))  %>%
  group_by(baseline, x) %>%
  mutate(Total = sum(value)) %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  pivot_longer(matches("Background|Exponential|Total")) %>%
  mutate(name = factor(name, levels = c("Total", "Exponential", "Background")))

tmp <- bind_rows(mutate(tmp, scale = "Log"), mutate(tmp, scale = "Linear")) %>%
  mutate(Baseline = reorder(paste0("Baseline: ", baseline, " cases"), baseline)) %>%
  mutate(scale = factor(scale, levels = c("Log", "Linear")))

# Set up scales 
scales_y <- list(
  Log = scale_y_log10(),
  Linear = scale_y_continuous()
)

tmp %>%
ggplot(aes(x = x, y = value, colour = name, size = name)) + 
  geom_line(alpha = .9) +
  facet_grid_sc(rows = vars(scale), cols = vars(Baseline), 
                scales = list(x = "fixed", y = scales_y)) +
  scale_size_manual(values = c(1.75, 1, 1)) +
  scale_colour_manual(values = c("#249ab5", "#d00000", "#001226")) +
  scale_fill_discrete(name = "") + 
  theme_bw() + 
  theme(legend.position = "bottom", axis.title = element_blank(),
        legend.title = element_blank())


## ----linear-scales-ref-lines, fig.width = 8, fig.height = 4, out.width = ".8\\linewidth", fig.cap = "Reported COVID cases in New York State, 2020-2022. The linear scale makes it difficult to compare the trajectory of different waves to determine how severe the current status is relative to the past, because the primary contrast is the height of the relative peaks, rather than the growth \\emph{rate}. A similar graph on the log scale would have the peaks at much more similar heights (though there would still be a difference), allowing the reader to focus on other information, such as the slope of the relative lines."----
cvstate %>%
  filter(state == "New York") %>%
  ggplot(aes(x = date, y = daily_avg_cases)) + 
  geom_line() + 
  ggtitle("Reported COVID cases in New York State, 2020-2022") +
  ylab("Daily Cases reported (7-day average)") + 
  xlab("Date") + theme_bw()

