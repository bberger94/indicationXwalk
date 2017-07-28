library(ggplot2)
library(ggthemes)


get.mode <- function(x){
  names(sort(table(x), decreasing = T))[1]
}




plotdata <- 
  merged %>%
  filter(ccs_label != 'Residual codes; unclassified',
         year <= 2016) %>% 
  mutate(year = factor(year),
         cancer = factor(cancer, levels = c(1,0)),
         ccs = factor(ccs))
  
ggplot(plotdata, aes(year)) +
  geom_bar(aes(fill = cancer)) +
  theme_fivethirtyeight() +
  scale_fill_discrete(name = '', 
                      breaks = c(1,0),
                      labels=c('Cancer indication', 'Noncancer indication')
                      ) +
  ggtitle('Number of first indication approvals')


ggplot(plotdata, aes(year)) +
  geom_bar(aes(fill = factor(`bbw`))) +
  theme_fivethirtyeight() +
  ggtitle('Number of first indication approvals') 




#ggplot(plotdata, aes(year, ncancerdrugs)) + geom_col()
ggplot(plotdata, aes(year, ndrugs, fill = pctcancer)) + geom_col()
#ggplot(plotdata, aes(year, pctcancer)) + geom_col()

