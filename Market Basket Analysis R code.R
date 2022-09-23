## ----include=FALSE----------------------------------------------------------
require(arules)
require(arulesViz)
ProjectData_ajman <- read.transactions("ajman.csv",rm.duplicates = TRUE,format = "basket",sep = ",")


## ----include=FALSE----------------------------------------------------------
IMG_ajman <- as(ProjectData_ajman,"matrix")
IMG_ajman


## ---------------------------------------------------------------------------
itemFrequencyPlot(ProjectData_ajman,support=0.15)
rules_ajman <- apriori(IMG_ajman,parameter=list(support=0.008,conf=.15))
rules_ajman
rules_ajman <- sort(rules_ajman,by="lift")
subset.matrix <- is.subset(rules_ajman,rules_ajman)
subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
redundant_ajman <- colSums(subset.matrix,na.rm=T) >= 1
rules_ajman <- rules_ajman[!redundant_ajman]
rules_ajman
x_ajman <- sort(rules_ajman,by="lift")
inspect(x_ajman[1:13])


## ---------------------------------------------------------------------------
is.redundant(x_ajman)


## ---------------------------------------------------------------------------
Conviction1 <- interestMeasure(x_ajman,"conviction",transactions = ProjectData_ajman)
cbind(quality(x_ajman),Conviction1)


## ----fig.width=10-----------------------------------------------------------
plot(x_ajman)
plot(x_ajman, method="grouped")
plot(x_ajman, method="graph")


## ----include=FALSE----------------------------------------------------------
ProjectData_woEmpora <- read.transactions("woEmpora.csv",rm.duplicates = TRUE,format = "basket",sep = ",")
ProjectData_woEmpora


## ----include=FALSE----------------------------------------------------------
IMG_woEmpora <- as(ProjectData_woEmpora,"matrix")
IMG_woEmpora


## ---------------------------------------------------------------------------
itemFrequencyPlot(ProjectData_woEmpora,support=0.1)
rules_woEmpora <- apriori(IMG_woEmpora,parameter=list(support=0.0008,conf=.15))
rules_woEmpora
rules_woEmpora <- sort(rules_woEmpora,by="lift")
subset.matrix <- is.subset(rules_woEmpora,rules_woEmpora)
subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
redundant_woEmpora <- colSums(subset.matrix,na.rm=T) >= 1
rules_woEmpora <- rules_woEmpora[!redundant_woEmpora]
rules_woEmpora
x_woEmpora <- sort(rules_woEmpora,by="lift")
inspect(x_woEmpora[1:20])


## ---------------------------------------------------------------------------
is.redundant(x_ajman)


## ---------------------------------------------------------------------------
Conviction2 <- interestMeasure(x_woEmpora,"conviction",transactions = ProjectData_woEmpora)
cbind(quality(x_woEmpora),Conviction2)


## ----fig.width=8------------------------------------------------------------
plot(x_woEmpora)
plot(x_woEmpora, method="grouped")
plot(x_woEmpora, method="graph")


## ----include=FALSE----------------------------------------------------------
ProjectData_wEmpora <- read.transactions("wEmpora.csv",rm.duplicates = TRUE,format = "basket",sep = ",")
ProjectData_wEmpora


## ----include=FALSE----------------------------------------------------------
IMG_wEmpora <- as(ProjectData_wEmpora,"matrix")
IMG_wEmpora


## ---------------------------------------------------------------------------
itemFrequencyPlot(ProjectData_wEmpora,support=0.06)
rules_wEmpora <- apriori(IMG_wEmpora,parameter=list(support=0.008,conf=.15))
rules_wEmpora
rules_wEmpora <- sort(rules_wEmpora,by="lift")
subset.matrix <- is.subset(rules_wEmpora,rules_wEmpora)
subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
redundant_wEmpora <- colSums(subset.matrix,na.rm=T) >= 1
rules_wEmpora1 <- rules_wEmpora[!redundant_wEmpora]
rules_wEmpora1
x_wEmpora <- sort(rules_wEmpora1,by="lift")
inspect(x_wEmpora[1:17])


## ---------------------------------------------------------------------------
is.redundant(x_wEmpora)


## ---------------------------------------------------------------------------
Conviction3 <- interestMeasure(x_wEmpora,"conviction",transactions = ProjectData_wEmpora)
cbind(quality(x_wEmpora),Conviction3)


## ---------------------------------------------------------------------------
plot(x_wEmpora)
plot(x_wEmpora, method="grouped")
plot(x_wEmpora, method="graph")

