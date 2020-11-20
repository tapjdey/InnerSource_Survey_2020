library(ggplot2)
library(maps)
library(hrbrthemes)
library(viridis)
library(scales)
library(stringr)
library(ggthemr)



sudata = read.csv("_DONOTADD_State_of_the_InnerSource_Survey 2020_Responses.csv")
sudata$Timestamp = NULL
header.map = colnames(sudata)
names(header.map) = paste0("v",seq(79))
# clean data
colnames(sudata) = paste0("v",seq(79))
sudata$v79 = NULL
sudata$v78 = NULL
sudata = data.frame(lapply(sudata, trimws))
# Shorten some Responses
sudata$v4 = gsub('Top-Down: Someone from management initiated the adoption', 'Top-Down', sudata$v4)
sudata$v4 = gsub('A mix of both', 'Mixed', sudata$v4)
sudata$v4 = gsub('Bottom-Up: One/A small group of developers started the initiative', 'Bottom-Up', sudata$v4)
sudata$v6 = gsub("Recognition that the current approach won't scale with our ambitions / trajectory.", 'Scaling', sudata$v6)
sudata$v6 = gsub("Maintain and develop software developed by contractors that are no longer with the organization", 'Maintain Software w/o Support', sudata$v6)
sudata$v6 = gsub("Avoid wasted time/resources solving similar problems multiple times in isolation", 'Creating reusable software', sudata$v6)
sudata$v8 = gsub("We formed a cross organization group under our Open Source Program Office to foster the practice of InnerSource",'Establish InnerSource Foundation', sudata$v8)



# Specific Answer questions
toFact = c(2,4,11:15,18:40, 43:61, 70:72)
sudata[,toFact] = lapply(sudata[,toFact], as.factor)
# Deal with multi-answer questions
mad = c(6:8,16,17,41,42,64,69)
process.mad = function(x){
  x = unlist(x)
  x = paste(x,',')
  x = unlist(strsplit(x,','))
  x = trimws(x)
  x = x[x != ""]

  return(table(x))
}
t=process.mad(sudata$v70)

# Horizontal Bar Chart
df = as.data.frame(t)
df$x = as.character(levels(df$x))
df$newx = str_wrap(df$x, width = 20)
df = df[order(df$Freq, decreasing = T),]
ggthemr("grape") 
ggplot(data=df, aes(x = reorder(newx, Freq), y = Freq)) +
  geom_bar(position="dodge",stat="identity") + coord_flip() + 
  labs(title=str_wrap(gsub("\\.",' ', header.map['v70']), 50), x='', y = 'Frequency') 
ggthemr_reset()




#country data
t=process.mad(sudata$v69)
df0 = as.data.frame(t)
df0$x = as.character(levels(df0$x))

df2 = data.frame('region'=df0$x)
df2$lat = c(2,-15,15, 32, 50, 29, 46, -24)
df2$long = c(24, -55, 80, 118 ,7, 47, -93, 137)
df = merge(df2, df0, by.x = 'region', by.y = 'x')

worlddata = map_data('world')
ggplot() +
  geom_polygon(data=worlddata, aes(x = long, y = lat, group = group), fill="#e0e0e0", colour = "white", alpha=0.7)  +  geom_path(data = worlddata, aes(x = long, y = lat, group = group), alpha=0.7) + geom_point(data = df, aes(x = long, y = lat, size = Freq), fill='#bf24ee', alpha= 0.7, shape=21, color="black") + scale_size(range = c(15,40))+ geom_text(data=df, aes(x = long, y = lat, label=Freq),position = position_dodge(width=0.9),  size=10) + scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") + theme_void() + theme(legend.position="none") 


# Numeric data
numdata = c(1,3,5,10,63,67,68,73,75,76)
for (i in numdata){
  sudata[,i] = as.numeric(gsub('[^0-9.-]','',sudata[,i]))
}
