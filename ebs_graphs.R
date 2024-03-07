library(tidyverse)
library(here)

library(ggforce)

library(systemfonts)
clear_registry()

register_variant(
  name = "Myriad Pro SemiCondensed",
  family = "Myriad Pro",
  width = "semicondensed",
  weight = c("normal", "semibold"),
)

library(showtext)
showtext_opts(dpi = 300)
showtext_auto()

library(myriad)
import_myriad_semi()
import_myriad_condensed()

theme_set(theme_myriad_semi())


library(tabulizer)

# Bah
# df <- extract_tables(here("raw", "graphs_data.pdf"),
#                      output = "data.frame") |>
#   as_tibble()


df <- readxl::read_excel(here("data", "graph_data.xlsx")) |>
  janitor::clean_names() |>
  mutate(count_label = paste0("(N=", count, ")"),
         category = ifelse(category == "Total Participants", "  ", category),
         category_label = case_match(category,
                                     "Gender Identity" ~ "Gender\nIdentity",
                                     "Location of formative years" ~ "Formative\nYears",
                                     "Self-reported disability" ~ " ",
                                     "First-generation college students" ~ " ",
                                     "Institution type" ~ "College\nType",
                                     .default = category))

# df |>
#   ggplot(aes(x = percent, y = reorder(subcategory, percent))) +
#   geom_point() +
#   facet_wrap(~ reorder(category, -count), scales = "free_y", ncol = 1)

out <- df |>
  ggplot(aes(x = percent, y = reorder(subcategory, percent))) +
  geom_point(size = 2, aes(color = category_label)) +
  geom_text(aes(label = count_label),
             nudge_x = 6, nudge_y = 0.3, size = 2) +
  guides(color = "none") +
  facet_col(~ reorder(category_label, -count),
            scales = "free_y",
            space = "free",
            strip.position = "right") +
  labs(y = NULL,
       x = "Percent of Respondents",
       title = "Participant Demographics") +
  theme(strip.text = element_text(face = "bold",
                                  family = "Myriad Pro Condensed",
                                  size = rel(1.2),
                                  lineheight = 0.65),
        axis.text = element_text(family = "Myriad Pro Condensed"),
        panel.grid.major = element_line(color = "gray30"),
        panel.spacing = unit(1.5, "lines"))


ggsave(here("figures", "participant_demographics.pdf"),
       out,
       width = 7, height = 6)

ggsave(here("figures", "participant_demographics.png"),
       out,
       width = 7, height = 6)

