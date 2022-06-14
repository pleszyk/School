library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)

setwd("~/Documents")

list.files()
df <- read.csv("data.csv", stringsAsFactors = F, skip = 2)

# Cleaning the data
df <- df[colSums(!is.na(df)) > 0]
df <- df[-nrow(df),]

colnames(df)[1] <- 'Ward'
colnames(df) <- gsub("\\.", "_", colnames(df))

df <- df %>% separate(Ward, sep = "(?=P)" , remove = T, into = c("Ward", "Precient"), fill = "right")
df$Ward <- factor(df$Ward, levels=unique(df$Ward))
colnames(df)

# Ward wise total ballots cast
bp <- ggplot(df, aes(x = '', fill=Ward, y=Registered_Voters_Total))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie

colnames(df)
molten <- melt(df, id = c("Ward", "Precient", "Registered_Voters_Total", "Ballots_Cast_Total", "Ballots_Cast_Blank"))
colnames(molten)[6] <- "Name"
colnames(molten)[7] <- "Votes"
molten[c(3,4,5)] <- NULL

# Histogram for Votes per Ward
ggplot(molten, aes(x=reorder(Ward, -Votes), y=Votes, fill = 'red')) + 
  geom_bar(stat = "identity") + xlab('Wards') + ggtitle('Votes per ward') + 
  theme(legend.title = element_blank(), legend.position = "none")

# Heatmap for Ward, Precient distribution
ggplot(df, aes(Ward, Precient, fill= Ballots_Cast_Total)) + 
  geom_tile()+ ggtitle('Ward, Precient distribution heatmap') 

# Distribution of Votes among Candidates per ward
ggplot(molten, aes(Ward, Votes)) + geom_bar(aes(fill = Name), 
width = 0.4, position = position_dodge(width=0.5), stat="identity") +  theme(legend.position="top", 
legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y=element_blank())

# Boxplot for ballots per precient across wards
ggplot(df, aes(x=Precient, y=Ballots_Cast_Total, color=Precient)) +
  geom_boxplot()
