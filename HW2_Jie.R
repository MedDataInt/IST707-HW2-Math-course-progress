### Data preparation, cleaning, and exploration  
## read and View data
library(tidyverse)
data <-read.csv('data_storyteller.csv')
str(data)
data$Section <- factor(data$Section)

head(data)

## change/simplify column names for easy to read and manipulate 
data <- setNames(data, c('School','Section', 'VeryAhead','Middling', 'Behind', 'MoreBehind','VeryBehind','Completed'))
  
## summary data using summary_table function
library(qwraps2) 
options(qwraps2_markup = 'markdown')
summary_table(data[, c(3:8)])

# aggregate rows
classBySchool <-aggregate(cbind(VeryAhead = data$VeryAhead,
                                Middling = data$Middling,
                                Behind = data$Behind,
                                MoreBehind = data$MoreBehind,
                                VeryBehind = data$VeryBehind,
                                Completed = data$Completed), 
                          by = list(School = data$School), 
                          FUN = sum)
head(classBySchool)

# aggregate columns
student <- rowSums(data[,c(3:8)])
dataNew <- data.frame(data, student)
head(dataNew)


# Aggregate Rows and Columns 
studentBySchool <- aggregate(cbind(student = dataNew$student),
                             by = list(School = dataNew$School),
                             FUN = sum)
head(studentBySchool)

# sum the class by rows
student_1 <- colSums(data[3:8]) 

### visualization 
## student by school
# Pie of Student 
library(RColorBrewer)
cols2 <- brewer.pal(5,"Set2")
pie(studentBySchool$student, labels = Lab1, cex =1.8, main = "Distribution of Student",col = cols2)
legend("topright", c('A', 'B','C','D','E'),cex =0.8, fill = cols2)

Lab1 <- paste (studentBySchool$School, studentBySchool$student, sep = ':')


## Pie of Class Progress
cols <- brewer.pal(6,'Accent')
student_1 <- colSums(data[3:8]) 

# calculate the percentage for each of class progress, rounded to one decimal place
Piepercent <- round(100*student_1/sum(student_1),1)

# Concatenate a % sign after each value
Piepercent <- paste (Piepercent, '%', sep = '')

pie(student_1, label= Piepercent, cex =1.8, main = "Distribution of Class Progress",col = cols)
legend("topright", c('VeryAhead', 'Middling','Behind','MoreBehind','VeryBehind','Completed'),cex =0.8, fill = cols)


## status of class progress of each school
library(tidyverse)
library(tidyr)
library(forcats)
library(RColorBrewer)
library(ggplot2)
colors1 <- brewer.pal(6,'Set2')
head(classBySchool)
df <- gather(classBySchool, key = "status", value="students", 2:7)



theme <-  theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), 
                axis.title = element_text(size = 22, face = 'bold'),
                axis.text = element_text(size =20),
                plot.title = element_text(size = 24)
)

  
stacked_class<- ggplot(data=df1, aes(x=School, y=number, fill=fct_inorder(status))) +
  geom_bar(stat="identity",alpha = 0.7, color = 'grey50')+scale_fill_manual(name = "", values = colors1) + 
  ggtitle("Status of Students") +
  theme

# Add freq column
df1 <- df %>% group_by(School,status) %>% summarise(number = sum(students)) %>% mutate (freq = number/sum(number))


## Plot frequency
stacked_class_freq<- ggplot(data=df1, aes(x=School, y=freq, fill=fct_inorder(status))) +
  geom_bar(stat="identity", position = "fill", alpha = 0.7, color = 'grey50')+scale_fill_manual(name = "", values = colors1) + 
  ggtitle("Status of Students(%)") +
  theme +
  geom_text(aes(label=paste(round(freq*100,1),'%',sep ='')),position = position_stack(vjust=0.5), size = 5)
  
head(classBySchool)


# Boxplot
df3 <- gather(data, key="status", value="students", 3:8)
head(df3)
ClassBox <- ggplot(df3, aes(x= School, y= students)) + 
  geom_violin() + 
  geom_dotplot(binaxis = 'y',
               stackdir = 'center',
               dotsize = 1,
               fill = 'darkred')+
  facet_wrap(~ status) + 
  theme_bw() + 
  ggtitle("Boxplot of Class Progress in each of School") + 
  theme(axis.line = element_line(colour = "black"), 
        axis.title = element_text(size = 22, face = 'bold'),
        axis.text = element_text(size =20),
        plot.title = element_text(size = 24))

install.packages("dendextend")


## dendro graph
library(ggdendro)
library(dendextend)
# Concatenate first two columns 
data$SchoolSection <- paste(data$School, data$Section)
head(data)
data2 <- data.frame(data[,- c(1,2,9)], row.names = data$SchoolSection)
rownames(data2) <- str_replace_all(rownames(data2), pattern = " ", repl="") 
d <- dist(data2)
hc <- hclust(d)

dend1 <- color_branches(hc, k=3)
dend1 <- set(dend1, "labels_cex", 1.4) # larger fonts
plot(dend1, main = "Cluster Dendrogram")

## PCA plot 
install.packages("gmodels")
install.packages("ggthemes")
install.packages("ggpubr")

library(gmodels)
library(ggthemes)
library(ggpubr)

head(data2)
data3 <- as.data.frame(t(data2))

pca.info <- fast.prcomp(data3)
head(pca.info$rotation)

pca.data <- data.frame(sample = rownames(pca.info$rotation),
                       Type=c(rep("A",13),rep("B",12),rep("C",3),rep("D",1), rep("E",1)),
                       pca.info$rotation)
class(pca.data)

pca.data$Ratio <- (data2$VeryAhead+data2$Middling+data2$Completed)/(data2$Behind + data2$MoreBehind + data2$VeryBehind + data2$VeryBehind)

pca <- ggscatter(pca.data, x = "PC1", y= "PC2",
          color = "Type",
          ellipse = T,
          label = "sample",
          repel = F,
          palette = colors1,
          size = "Ratio",
          main="PCA Plot",font.label = c(14, "bold"))
