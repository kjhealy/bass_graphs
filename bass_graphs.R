library(tidyverse)
library(here)
library(ggforce)

### ---- Note on Fonts -----
## For these fonts to work you will need to have
## the full Myriad Pro font installed along with
## my {myriad} package. Myriad Pro is available
## from Adobe, and may already be installed on you
## computer. The {myriad} package is here:
## https://kjhealy.github.io/myriad/
## Install it with:
## remotes::install_github("kjhealy/myriad")

## If you don't have the fonts, you can comment
## out everything from here until the "Font setup ends"
## line below. Also change or remove the references
## to "Myriad Pro Condensed" in the main code below.
## With those removed, the plots will still work, but
## they'll be in Helvetica, and they won't look as good.

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

### ----- Font setup ends ---------------------


## Read in the data and clean it.
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


## Make the plot
out <- df |>
  ggplot(aes(x = percent, y = reorder(subcategory, percent))) +
  geom_point(size = 2, aes(color = category_label)) +
  geom_text(aes(label = count_label),
             nudge_x = 4.24, nudge_y = 0.3, size = 2,
            # Requires Myriad Pro!
            family = "Myriad Pro Condensed") +
  guides(color = "none") +
  facet_col(~ reorder(category_label, -count),
            scales = "free_y",
            space = "free",
            strip.position = "right") +
  labs(y = NULL,
       x = "Percent of Respondents",
       title = "Participant Demographics") +
  theme(strip.text = element_text(face = "bold",
                                  # Requires Myriad Pro!
                                  family = "Myriad Pro Condensed",
                                  size = rel(1.2),
                                  lineheight = 0.65),
        # Requires Myriad Pro!
        axis.text = element_text(family = "Myriad Pro Condensed"),
        panel.grid.major = element_line(color = "gray30"),
        panel.spacing = unit(1.5, "lines"))

## Save the plot
ggsave(here("figures", "participant_demographics.pdf"),
       out,
       width = 7, height = 6)

ggsave(here("figures", "participant_demographics.png"),
       out,
       width = 7, height = 6)

