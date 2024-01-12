# load packages
install.packages("pacman")
pacman::p_load(tidyverse, R2jags, parallel, polspline, utils)

##### load data #####
Dat <- read.csv('SarahHedvigDahlNielsen#8292/Exam/data/preprocessedDataNotRed.csv')

# extract every third line - data file has lines representing others responses and we don't need that
redDat <- Dat[seq(1,length(Dat$sessionid),3),]

### Plotting contributions
plots_folder <- "SarahHedvigDahlNielsen#8292/Exam/Final exam code/Contribution plots/"

# Create graphing function
plots <- function(df, na.rm = TRUE, ...){
  
  # create list of ID in data to loop over 
  group_list <- unique(df$groupid)
  
  df$subjectid <- as.factor(df$subjectid)
  
  # create for loop to subset group ID
  for (i in seq_along(group_list)) { 
    
    subset_group <- subset(df, df$groupid==group_list[i])
    
    # create plot for each trial in subset_ID 
    plot <- ggplot(subset_group, aes(x=period, y = senderscontribution, color = subjectid)) +
        
        geom_line() +
        
        labs(x = "Trial", y = "Contribution") + theme_bw()
    
    # save plots as .png
    ggsave(plot, file=paste(plots_folder,
                              "group",group_list[i], ".png", sep=''), scale=2)
    
    # print plots to screen
    print(plot)
    }
  }


plots(redDat)

Zip_Files <- list.files(path = plots_folder, pattern = ".png$")
setwd("/work/SarahHedvigDahlNielsen#8292/Exam/Final exam code/Contribution plots/")
zip(zipfile = 'ZipPlots', files = Zip_Files)

### Plotting correlation between prosocial and contribution difference
setwd("/work/")
plots_folder2 <- "SarahHedvigDahlNielsen#8292/Exam/Final exam code/prosocial_cont_diff plots/"

df <- Dat %>% filter(
  cont_diff >= 0 
) 

# Create graphing function
plots2 <- function(df, na.rm = TRUE, ...){
  
  # create list of ID in data to loop over 
  group_list <- unique(df$groupid)
  
  df$subjectid <- as.factor(df$subjectid)
  
  # create for loop to subset group ID
  for (i in seq_along(group_list)) { 
    
    subset_group <- subset(df, df$groupid==group_list[i])
    
    # create plot for each trial in subset_ID 
    plot <- ggplot(subset_group, aes(x=cont_diff, y = prosocial)) +
      
      geom_point() +
      
      geom_smooth(method = "lm") +
      
      labs(x = "Contribution difference", y = "Prosocial punishment") + theme_bw()
    
    # save plots as .png
    ggsave(plot, file=paste(plots_folder2,
                            "group",group_list[i], ".png", sep=''), scale=2)
    
    # print plots to screen
    print(plot)
  }
}

plots2(df)

Zip_Files2 <- list.files(path = plots_folder2, pattern = ".png$")
setwd("/work/SarahHedvigDahlNielsen#8292/Exam/Final exam code/prosocial_cont_diff plots/")
zip(zipfile = 'prosocial_cont_diff', files = Zip_Files2)

### Plotting the chosen group
thechosenone <- redDat %>% filter(
  groupid == 4802
)

thechosenone$subjectid <- as.factor(thechosenone$subjectid)

ggplot(thechosenone, aes(x=period, y = senderscontribution, color = subjectid)) +
  
  geom_line() +
  
  labs(x = "Trial", y = "Contribution") + theme_bw()

specialgroup <- df %>% 
  filter(
    groupid == 4802
  )

ggplot(specialgroup, aes(x=cont_diff, y = prosocial)) +
  
  geom_point() +
  
  geom_smooth(method = "lm") +
  
  labs(x = "Contribution difference", y = "Prosocial punishment") + theme_bw()

cor(df$cont_diff,df$prosocial)
