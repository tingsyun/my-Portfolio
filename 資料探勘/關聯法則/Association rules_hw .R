library(readr)
library(arules)
grocerystore <- read.csv("/Users/teresa/Desktop/grocerystore.txt") 
head(grocerystore)
View(grocerystore)
grocerystore_new=grocerystore[,1:10]
grocerystore_new1=na.exclude(grocerystore_new)
grocerystore_new2=as.matrix(grocerystore_new1) 
rule=apriori(grocerystore_new2,parameter=list(supp=0.09, conf=0.8,maxlen=5))
#rule=apriori(grocerystore_new2,parameter=list(supp=0.02, conf=0.5,maxlen=5), appearance = list(rhs='ţ??', default ='lhs'))
#rule=apriori(grocerystore_new2,parameter=list(supp=0.02, conf=0.5,maxlen=5), appearance = list(lhs='ţ??', default ='rhs'))

length(rule)
inspect(head(rule))
quality(head(rule))

#rule=apriori(grocerystore,parameter=list(supp=0.09, conf=0.8,maxlen=5))
#rule=apriori(grocerystore,parameter=list(supp=0.02, conf=0.5,maxlen=5), appearance = list(rhs='?????', default ='lhs'))
#rule=apriori(grocerystore,parameter=list(supp=0.02, conf=0.5,maxlen=5), appearance = list(lhs='?????', default ='rhs'))

inspect(head(sort(rule,by="support"),10))
inspect(head(sort(rule,by="confidence"),10))


library(arulesViz)
#Heat map 
plot(rule)
#Balloon plot 
plot(rule,method="grouped") 

# Parallel coordinates plot ()
plot(rule, method = "paracoord", control = list(reorder = TRUE))



install.packages("arulesViz")
library(arulesViz)

plot(rule, engine = "html")
plot(rule, method = "graph", engine = "html")

install.packages('shiny')
install.packages('shinythemes')
library(shiny)
library(shinythemes)
ruleExplorer(rule)
