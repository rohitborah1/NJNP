

# Introduction ------------------------------------------------------------

## This is the R source code for data visualizations in support of Rohit Borah's 
## 2020 No Justice, No Peace Project. All data can be found in either the GitHub
## repository (https://github.com/rohitborah1/NJNP-CivilUnrest-DomesticPolicy)
## under "NJNP Dataset.xlsx" or by emailing the author at (rohitborah1@gmail.com).
## Please feel free to reach out if you have any questions or if there is any 
## additional information that I can help to provide.

# Load Requisite Packages -------------------------------------------------

library(tidyverse)
library(ggrepel)
library(ggimage)
library(reshape2)
library(readxl)
library(RColorBrewer)
library(scales)
library(gridExtra)

# Comparable Countries by Police Violence ---------------------------------

## Insert Data
grpv <-
  as.data.frame(
    read_excel(
      "~/Documents/Personal/NJNP/Data/NJNP Dataset.xlsx",
      sheet = "Global Rates of Police Violence",
      skip = 4
    )
  )

## Manipulate Variables

grpv$Country <- as.factor(grpv$Country)
grpv$`Country Code` <- as.factor(grpv$`Country Code`)
grpv$Region <- as.factor(grpv$Region)
grpv$`Income Group` <- factor(
  grpv$`Income Group`,
  levels = c("High income",
             "Upper middle income",
             "Lower middle income")
)
grpv$`2018 Population` <- as.numeric(grpv$`2018 Population`)
grpv$`Police Killings Per 10,000,000` <-
  as.numeric(grpv$`Police Killings Per 10,000,000`)

grpv <- grpv %>% mutate_if(is.numeric, round, digits = 2)
grpv <- grpv[order(grpv$`Income Group`), ]
grpv <-
  grpv[complete.cases(grpv$`Police Killings Per 10,000,000`), ]

## Data Viz: Box Plot, Police Killings Per 10,000,000

grpv_bp <- ggplot(grpv,
                  aes(
                    x = reorder(Country,-`Police Killings Per 10,000,000`),
                    y = `Police Killings Per 10,000,000`,
                    fill = `Income Group`
                  )) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#00b159", "#003399", "#ff2b4f")) +
  geom_text(
    aes(label = `Police Killings Per 10,000,000`),
    position = position_dodge(width = 0.9),
    vjust = -.4,
    size = 3
  ) + labs(title = "Rate of Police Killings: G20 Countries Reporting Data",
           fill = "Income Group as defined\nby The World Bank",
           caption = "Source: The Mapping Police Violence Project, Government Reports, and The World Bank.\n\nCreated by Rohit Borah, June 2020") +
  ylab("Rate of Death In Custody Per 10,000,000") +
  theme(
    axis.text.x = element_text(
      angle = -45,
      hjust = 0,
      vjust = 1
    ),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.77, 0.795),
    legend.background = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

grpv_bp

# Rate of US Police Violence By Race -------------------------

## Insert Data

pvbr <-
  as.data.frame(
    read_excel(
      "~/Documents/Personal/NJNP/Data/NJNP Dataset.xlsx",
      sheet = "US Police Killings By Race",
      col_types = c(
        "text",
        "skip",
        "skip",
        "numeric",
        "skip",
        "skip",
        "numeric",
        "skip"
      ),
      na = "N/A",
      skip = 4
    )
  )

## Manipulate Variables

pvbr$Race <- as.factor(pvbr$Race)
pvbr <- pvbr %>% mutate_if(is.numeric, round, digits = 2)

pvbr <- pvbr[complete.cases(pvbr$`Race as % of Police Killings`), ]


# dplyr/melt to manage data

pvbr_melt <- melt(pvbr, id.vrs = 'Race')

## Data Viz:

pvbr_bp <-
  ggplot(pvbr_melt, aes(
    x = reorder(Race, -value),
    y = value,
    fill = variable
  )) +
  geom_bar(stat = 'identity', position = position_dodge2(padding = -0.1)) +
  geom_text(
    aes(label = paste0(value, "%")),
    position = position_dodge(width = 0.9),
    vjust = -.2,
    hjust = .4,
    size = 2.25
  ) +
  labs(title = "Police Killings in the United States by Race, 2013-2019",
       caption = "Sources:\nPopulation Data, 2018 American Community Survey, the United States Census Bureau\nPolice Killings Data, Mapping Police Violence Project\n\nCreated by Rohit Borah, June 2020") +
  scale_y_continuous(
    labels = function(x)
      paste0(x, "%")
  ) +
  theme(axis.text.x = element_text(
    angle = -45,
    hjust = 0,
    vjust = 1
  )) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.81),
    legend.background = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

pvbr_bp

# Violent Crime Rate by Number of Police / Police Expenditures ------------

## Insert Data

usp <-
  as.data.frame(
    read_excel(
      "~/Documents/Personal/NJNP/Data/NJNP Dataset.xlsx",
      sheet = "Municipal Police Data DB",
      col_types = c(
        "text",
        "numeric",
        "skip",
        "skip",
        "skip",
        "skip",
        "numeric",
        "numeric",
        "skip",
        "numeric"
      ),
      skip = 7
    )
  )

## Manipulate Variables

usp$City <- as.factor(usp$City)
usp <- usp %>% mutate_if(is.numeric, round, digits = 2)
lbl_emp <- paste("R^2 ==", 0.1931)
lbl_xpnd <- paste("R^2 ==", 0.1003)

## Data Viz:

usp_emp_sp <-
  ggplot(usp,
         aes(Police_Employment_PerCapita, Violent_Crimes_PerCapita)) +
  geom_point(aes(size = Population, fill = "#003399"),
             color = "#003399",
             stroke = 1) +
  geom_smooth(method = "loess", color = "white") +
  scale_size(range = c(1, 10)) +
  labs(title = "Standardized Rate of Police Employment against Violent Crime (2018)",
       subtitle = "Data reflects 66 Large American Cities Identified by the USDOJ",
       caption = "Sources:\n2016 Police Employment Data, US Bureau of Justice Statistics\nViolent Crime Data, Mapping Police Violence Project\n\nCreated by Rohit Borah, June 2020") +
  xlab("Police Employment Per 100,000 Residents") + ylab("Violent Crime Rate") +
  annotate(
    "text",
    x = 650,
    y = 2400,
    label = lbl_emp,
    parse = TRUE
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

usp_xpnd_sp <-
  ggplot(usp, aes(Police_XPND_PerCapita, Violent_Crimes_PerCapita)) +
  geom_point(aes(size = Population, fill = "#00b159"),
             color = "#00b159",
             stroke = 1) +
  geom_smooth(method = "loess", color = "white") +
  scale_size(range = c(1, 10)) +
  labs(title = "Standardized Rate of US Municipal Police Expenditure against Violent Crime (2016)",
       subtitle = "Data reflects 66 large American cities as identified by the USDOJ",
       caption = "Sources:\n2016 Police Expenditure Data, US Bureau of Justice Statistics\nViolent Crime Data, Mapping Police Violence Project\n\nCreated by Rohit Borah, June 2020") +
  xlab("Police Expenditure Per 100,000 Residents ($)") + ylab("Violent Crime Rate") +
  scale_x_continuous(labels = dollar, limits = c(20000000, 65000000)) +
  annotate(
    "text",
    x = 62500000,
    y = 1900,
    label = lbl_xpnd,
    parse = TRUE
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )


## Data Viz Arranged

usp_emp_sp_a <-
  ggplot(usp,
         aes(Police_Employment_PerCapita, Violent_Crimes_PerCapita)) +
  geom_point(aes(size = Population, fill = "#003399"),
             color = "#003399",
             stroke = 1) +
  geom_smooth(method = "loess", color = "white") +
  scale_size(range = c(1, 10)) +
  labs(title = "Standardized Rate of Police Employment against Violent Crime (2016)",
       subtitle = "Data reflects 66 Large American Cities Identified by the USDOJ") +
  xlab("Police Employment Per 100,000 Residents") + ylab("Violent Crime Rate") +
  annotate(
    "text",
    x = 650,
    y = 2400,
    label = lbl_emp,
    parse = TRUE
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

usp_xpnd_sp_a <-
  ggplot(usp, aes(Police_XPND_PerCapita, Violent_Crimes_PerCapita)) +
  geom_point(aes(size = Population, fill = "#00b159"),
             color = "#00b159",
             stroke = 1) +
  geom_smooth(method = "loess", color = "white") +
  scale_size(range = c(1, 10)) +
  labs(title = "Standardized Rate of US Municipal Police Expenditure against Violent Crime (2016)",
       caption = "Sources:\n2016 Justice Expenditure and Employment Extracts Series, US Bureau of Justice Statistics\nViolent Crime Data, Mapping Police Violence Project\n\nCreated by Rohit Borah, June 2020") +
  xlab("Police Expenditure Per 100,000 Residents ($)") + ylab("Violent Crime Rate") +
  scale_x_continuous(labels = dollar, limits = c(20000000, 65000000)) +
  annotate(
    "text",
    x = 62500000,
    y = 1900,
    label = lbl_xpnd,
    parse = TRUE
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

grid.arrange(usp_emp_sp_a, usp_xpnd_sp_a, nrow = 2)

# Military Expenditure for Top 9 Countries + US Police --------------------

## Insert Data

mxpnd <-
  as.data.frame(
    read_excel(
      "~/Documents/Personal/NJNP/Data/NJNP Dataset.xlsx",
      sheet = "Military Expenditure By Country",
      col_types = c(
        "text",
        "skip",
        "skip",
        "text",
        "numeric",
        "skip",
        "numeric",
        "skip"
      ),
      skip = 4
    )
  )



## Manipulate Variables

mxpnd$Entity <- as.factor(mxpnd$Entity)
mxpnd$`Income Group` <- as.ordered(mxpnd$`Income Group`)
mxpnd$`Income Group` <- factor(
  mxpnd$`Income Group`,
  levels = c("High income",
             "Upper middle income",
             "Lower middle income")
)

## Data Viz:

mxpnd_bp <- ggplot(mxpnd,
                   aes(
                     x = reorder(Entity, Expenditure),
                     y = Expenditure,
                     fill = `Income Group`
                   )) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#00b159", "#003399", "#ff2b4f")) +
  coord_flip() +
  labs(
    title = "Annual Military Expenditure ($)",
    subtitle = "Top Nine Countries in the World Plus U.S. Police",
    fill = "Income Group as defined\nby The World Bank",
    caption = "Sources:\n2018 Military Expenditure: The World Bank\n2016 US Police Expenditure: US Department of Justice\n\nCreated by Rohit Borah, June 2020"
  ) +
  ylab("2018 Military Expenditure (Unless Otherwise Specified)") +
  scale_y_continuous(labels = dollar) +
  theme(
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.77, 0.585),
    legend.background = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

# Charts: Quick Reference --------------------------------------------------------------------

## Global Rates of Police Violence
grpv_bp

## Police Violence By Race
pvbr_bp

## US Policing Effectiveness

# Employment vs. Violent Crime
usp_emp_sp

# Expenditure vs. Violent Crime
usp_xpnd_sp

# Combined Employment & Expenditure vs. Violent Crime
grid.arrange(usp_emp_sp_a, usp_xpnd_sp_a, nrow = 2)

## Global Military Expenditure Compared to US Police
mxpnd_bp

