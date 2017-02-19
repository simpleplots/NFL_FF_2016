#Load packages needed for analysis.

library(tidyverse)
library(XML)
library(rvest)
library(stringr)

url <- "http://www.pro-football-reference.com/years/2016/fantasy.htm"

#Web scrapping
pfr <- read_html(url) 
pfr_2016 <- pfr %>%
    html_node("table") %>% #pulling table into data table
    html_table(header = FALSE) 

#Header Clean Up
#combine row 1 and 2
row1 <- pfr_2016[1,]
row2 <- pfr_2016[2,]
headers <- headers <- paste(row1, row2, sep = " ")
headers[2] <- "Name"
headers<- str_trim(headers, side = "both") #trim headers 
headers <- str_replace_all(headers, "[ ]", "_")
pfr_2016 <- pfr_2016[-(1:2), ]
colnames(pfr_2016) <- headers
pfr_2016 <- as_tibble(pfr_2016)


numeric_col <- c(1, 5:27) #convert columns to numeric
pfr_2016[,numeric_col] <- lapply(pfr_2016[,numeric_col], as.numeric) #coverting stat columns into numeric
pfr_2016$FantPos <- as.factor(pfr_2016$FantPos) #coverting player position into factors

#remove NAs 
NA_val<- !is.na(pfr_2016$Games_G) 
pfr_2016 <- pfr_2016[NA_val,]
pfr_2016 <- arrange(pfr_2016, Rk)

pfr_2016$Name <- str_replace_all(pfr_2016$Name, "[*+]","") #removing special characters from player names
Positions <- levels(pfr_2016$FantPos)

#Creating tables for each position
QB_2016 <- pfr_2016 %>%
    filter(FantPos == "QB") %>%
    as_tibble()

RB_2016 <- pfr_2016 %>%
    filter(FantPos == "RB") %>%
    arrange(Rk)

WR_2016 <- pfr_2016 %>%
    filter(FantPos == "WR") %>%
    arrange(Rk)

TE_2016 <- pfr_2016 %>%
    filter(FantPos == "TE") %>%
    arrange(Rk)

#Creating plots

top_QB <- ggplot(data = head(QB_2016, n=20), aes(x=`Fantasy FantPt`, y = reorder(Name, desc(Rk)))) +
    geom_point(size = 3, color = "green4") +
    labs(x = "Fantasy Points", y = "Player") +
    ggtitle("Top QB 2016") +
    theme_bw() +
    ggsave("top_QB.png")

top_RB <- ggplot(data = head(RB_2016, n=20), 
                 aes(x=`Fantasy FantPt`, 
                     y = reorder(Name, desc(Rk)))) + 
    geom_point(size = 3, color = "dark blue") +
    labs(x = "Fantasy Points", y = "Player") +
    ggtitle("Top RB 2016") +
    theme_bw() +
    ggsave("top_RB.png")

top_WR <- ggplot(data = head(WR_2016, n=20), 
                 aes(x=`Fantasy FantPt`, 
                     y = reorder(Name, desc(Rk)))) + 
    geom_point(size = 3, color = "dark red") +
    labs(x = "Fantasy Points", y = "Player") +
    ggtitle("Top WR 2016") +
    theme_bw() +
    ggsave("top_WR.png")

top_TE <- ggplot(data = head(TE_2016, n=20), 
                 aes(x=`Fantasy FantPt`, 
                     y = reorder(Name, desc(Rk)))) + 
    geom_point(size = 3, color = "slategrey") +
    ggtitle("Top TE 2016") +
    labs(x = "Fantasy Points", y = "Player") +
    theme_bw() +
    ggsave("top_TE.png")
