'''
####################################


Gephi - R Analysis   Part 1

Dynamic(Epidemic) Network

Hospital_Contact


Young Joon Oh
https://youngjoon5.github.io
2018.2.26
####################################
'''



library(EpiModel)

## loading data

setwd("C:/Users/YoungJoon/OneDrive/(Network_diffusion)practice/(Gephi_Py_R)Dynamic network/detailed_list_of_contacts_Hospital/R_netDynamic_Epimodel") 


node_data <- read.csv("nodes.csv", head=T, as.is=T) 

edge_data <- read.csv("edges.csv", head=T, as.is=T) 

names(node_data)
names(edge_data)
head(node_data)
head(edge_data)

table(node_data$status)

length(node_data[,1]) # 75    ->



'''
# DATA : from 1:pm, Monday to 2pm, Friday
'''
edge_data$time_start <- edge_data$time_start / 20 - 5   # 120 seconds(1:02pm, Mon) -> 1, unit : 20 seconds
edge_data$time_end <- edge_data$time_end / 20 - 5   # 120 seconds -> 1


# color, no do not red for infection
node_data$color <- "darkgray"      # PAT
node_data$color[node_data$status == "NUR"] <- "blue"
node_data$color[node_data$status == "MED"] <- "cyan"
node_data$color[node_data$status == "ADM"] <- "green"

names(node_data)
names(edge_data)
tail(edge_data)


# create spell columns for nodes 0 - Inf
node_data$onset <- 0
node_data$terminus <- Inf

head(node_data)




# create edge attributes
# N-N, N-P, M-M, A-N, M-N, M-P, A-M, A-P, A-A, P-P
# 

edge_data$attrs <- NA
nrow(edge_data)

for(i in 1 :  nrow(edge_data)){
n1 <- node_data$status[node_data$Id == edge_data$Source[i]]
n2 <- node_data$status[node_data$Id == edge_data$Target[i]]
n3 <- sort(c(n1,n2))
edge_data$attrs[i] <- paste(n3[1],"-",n3[2], sep = "")
}

head(edge_data)

names(node_data)  
names(edge_data)




########
###        combining dynamic edges
#####

edge_data[edge_data$attrs == "PAT-PAT",][1:10,] # a contact is divided by multiple time units -> increase # of edges



library(dplyr)


head(edge_data);names(edge_data)

test<-edge_data %>% 
group_by(Source,Target) %>%          #  grouping by columns
mutate(temp_id = row_number())      # row_number() : numbering by same group


test


test<-as.data.frame(test)


###
# combining
###

test$temp_ing <- 1


for (i in 1:nrow(test)){          
if ( i < nrow(test) & ((test$temp_id[i] - test$temp_id[i + 1]) == -1 & test$Source[i] == test$Source[i+1] ) ){ 
if (test$temp_ing[i] != 0 ){start <- i}    
test$time_end[i] <- NA   
test$temp_ing[i + 1] <- 0                
}
else{ if(test$temp_ing[i] == 0){test$time_end[start] <- test$time_end[i]   #   combine "time_end"s 
test$time_end[i] <- NA}
}

}

test

# remove rows with NA in time_end
edge_data <- test[complete.cases(test$time_end), ]

head(edge_data)





'''
#####################
convert to Network
#####################
'''


'''
Bug
'''
names(node_data) ; names(edge_data)
# Node Spell : onset, terminus, id.....  // Edge SPell : onset, terminus, tail, head,....
net_1 <-networkDynamic(vertex.spells=node_data[,c(6,7,1)],edge.spells=edge_data[,c(8,9,1,2)])
net_1  ## BUG
head(node_data)


# Fixing

node_data$newid <- 1:75 
edge_data$from <- match(edge_data$Source,node_data$Id)
edge_data$to <- match(edge_data$Target,node_data$Id)


node_data
edge_data


names(node_data);names(edge_data)



## After Fixing the BUG
net <-networkDynamic(vertex.spells=node_data[,c(6,7,8)],edge.spells=edge_data[,c(8,9,13,14)])
net
names(net)


set.vertex.attribute(net,"status", as.character(node_data$status))
set.vertex.attribute(net,"color", as.character(node_data$color))




# dynamic edge attribute  

head(edge_data)

for(i in 1:nrow(edge_data)){
eid <- get.edgeIDs(net,v=edge_data$from[i], alter=edge_data$to[i])
set.edge.attribute(net,"pairs", edge_data$attrs[i], e=eid)
activate.edge.attribute(net,"pairs",edge_data$attrs[i],onset=edge_data$time_start[i],
                        terminus=edge_data$time_end[i],e=eid)
}


list.edge.attributes(net)



#####
#    plot
#####




color<-get.vertex.attribute(net, "color")
color
plot(net, vertex.col = color, vertex.cex = 3 , edge.col = 1, displaylabel= T)

net1<-network.extract(net, onset = 1, terminus = 2000)
net2<-network.extract(net, onset = 2000, terminus = 4000)
plot(net1, vertex.col = color, vertex.cex = 3 , edge.col = 1, displaylabel= T)
plot(net2, vertex.col = color, vertex.cex = 3 , edge.col = 1, displaylabel= T)



library(sna) # different layout
gplot(net, mode = "hall" , vertex.cex = 3, vertex.col = color, 
      edge.col = 1, edge.lwd = 2, usecurve = T, displaylabel= T)

?gplot.layout




range(get.change.times(net))

# how many contacts in each time unit?
count_contacts <- sapply(0:17377,function(t){network.edgecount.active(net, 
                                                      at=t, active.default=F)}) 

### 180 units = 1 hour, 1980(Midnight) // 2160(12 hours) => 1980 + 2160 = 4140(Noon, Tue) 
#   // 4320(24 hours)
tt <- 4320

par(mfrow=c(1,1))
plot(count_contacts,type="o", xlab="time",ylab="contacts")
# Midnight line 
abline(v = c(1980, 1980 + tt, 1980 + 2*tt, 1980 + 3*tt ) , col = 'red') # h : horizontal, v : vertical   
# Noon line
abline(v = c(4140, 4140 + tt, 4140 + 2*tt, 4140 + 3*tt ) , col = 'blue') # h : horizontal, v : vertical   





## plots by pairs

pair1 <- unique(edge_data$attrs)
pair1 # 10

# Plots                       ===============  TIME // hours 
par(mfrow=c(2,5))

for(i in 1:10){
      pp <- paste("p",i,sep="")
      x <- sapply(0:17377,function(t){sum((get.edge.attribute.active(net,'pairs', 
                              at=t, require.active = T) == pair1[i]), na.rm=TRUE)})
      assign(pp,x)
      plot(x, main= pair1[i], type = "o", ylab="pairs",xlab='time' ) 
      abline(v = c(1980, 1980 + tt, 1980 + 2*tt, 1980 + 3*tt ) , col = 'red', lty =2)  
      abline(v = c(4140, 4140 + tt, 4140 + 2*tt, 4140 + 3*tt ) , col = 'blue', lty=2)   
    }

par(mfrow=c(1,1))





####
##    Duration
###


a1<-get.edge.activity(net,as.spellList=TRUE) ;a1
edge_duration<-select(a1, onset, duration)
range(edge_duration$duration)
plot(edge_duration, xlab="time")

# plots by pairs

# total edge : 1139
edge_ids <- unique(a1$edge.id) 
length(edge_ids)

a1$pairs <- NA
for (edge_id in edge_ids){a1$pairs[a1$edge.id == edge_id] <- 
  net$mel[[edge_id]]$atl$pairs.active[[1]][[1]]}

a1
duration_pairs<-select(a1, onset, duration, pairs)


# plot #
library(ggplot2)
ggplot(duration_pairs, aes(x = onset, y = duration, color = pairs)) + 
  labs(x = "Time") + geom_point()  +  facet_wrap( ~ pairs)  




'''
Part 2 : SI model simulation


'''

