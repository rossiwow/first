########################### 4C - Graph 2 ############################
#### Load packages####
library("dplyr") 
library("ggplot2")
library("tidyr")
library("plotly")

#### Load and prepare data ####
df <- as_data_frame(read.csv(file = "http://www.stat.columbia.edu/~gelman/bda.course/electric.txt", sep = "" , skip = 1))
df$Grade <- as.ordered(df$Grade) # ordened factor
# Data transformation
df <- df %>% select(City, 
                    Grade,
                    'Treat Pretest' = Pretest,
                    'Treat Posttest' = Posttest, 
                    'Control Pretest' = Pretest.1, 
                    'Control Posttest' = Posttest.1) 
df <- df %>% 
  gather(key = "Variable", value = "Value", 
         `Treat Pretest`, 
         `Treat Posttest`, 
         `Control Pretest`, 
         `Control Posttest`)
df <- df %>% 
  separate(col = Variable, 
           into = c("Group", "Test"), 
           sep = " ") %>% 
  mutate(Test = substr(Test, start = 1, stop = nchar(Test)-4)) %>% 
  mutate(Group = as.factor(Group), Test = as.ordered(Test))

BasicStats <- df %>% 
  group_by(City, Grade, Group, Test) %>% 
  summarise_all(c("min", "mean", "median", "max", "sd"))

#### Plot data ####
ggplotly(ggplot(data = BasicStats, 
                aes(x = reorder(Test, mean), 
                    y = mean, 
                    color = Group)) + 
           geom_point() + 
           geom_line(aes(group = Group)) + 
           facet_grid(Grade~City, scales = "free") + 
           labs(x = "Test", 
                y = "Avg", 
                title = "Average pre and post test by group, grade and city"))
