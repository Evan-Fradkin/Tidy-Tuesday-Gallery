library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(here)

# Load the data --------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-03-23')

unvotes <- tuesdata$unvotes

roll_calls <- tuesdata$roll_calls

issues <- tuesdata$issues

# Important Issues --------------------------------------------------------
# of important votes, which issues are most frequent?

roll_issues <- roll_calls %>% left_join(issues) 

important_votes <- roll_issues %>% 
        filter(!is.na(issue) & importantvote == 1) %>% 
        count(issue, sort = TRUE) %>% 
        mutate(issue = fct_reorder(issue, n)) %>% 
        ggplot(aes(issue,n,fill = issue)) +
        geom_col(show.legend = FALSE) +
        coord_flip() + 
        labs(x = "Issue", y = "# of Occurrences",title = "Most Frequent Issues Amongst Important Votes (As Defined by US DoS)", caption = "Source: Harvard Dataverse" ) +
        theme_minimal()
        
ggsave("frequent_issues.png")


