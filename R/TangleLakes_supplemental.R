### NEED HEADER HERE

## loading some packages
library(tidyverse)
library(ggsankey)   # devtools::install_github("davidsjoberg/ggsankey")


## reading data and doing some initial manipulation
## - note: some things might look a little odd, it's to be able to paste in
##   some previously written code
mvt1 <- read_csv("flat_data/Tangle_movement.csv")[, c(4, 13:27)] %>%
  rename(Tagging=Lake) %>%
  mutate(Tagging = ifelse(Tagging == "Lower", "A_LT", Tagging)) %>%
  mutate(Tagging = ifelse(Tagging == "Round", "A_RT", Tagging)) %>%
  mutate(Tagging = ifelse(Tagging == "Shallow", "A_ST", Tagging)) %>%
  mutate(Tagging = ifelse(Tagging == "Upper", "A_UT", Tagging)) %>%
  as.data.frame
datelabels <- names(mvt1)



### Creating discrete time-series plot and Sankey plot
### - ALL POSSIBLE DESTINATIONS (non-Tangle nodes expanded)

# table(as.matrix(mvt1))
mvt2 <- mvt1
thelevels <- c("NFM","FM","AL","A_GL","A_UT","A_RT","A_ST","A_LT")
# thelevels <- c("A_UT","A_RT","A_ST","A_LT")

fulllevels <- c("Non-Fishing Mort", "Fishing Mort", "At Large", "Glacier Lake",
                "Upper Tangle", "Round Tangle", "Shallow Tangle", "Lower Tangle")
# fulllevels <- c("Upper Tangle", "Round Tangle", "Shallow Tangle", "Lower Tangle")

for(i in 1:ncol(mvt2)) {
  mvt2[,i] <- factor(mvt2[,i], levels=thelevels)
}
mvt2_numeric <- mvt2
for(i in 1:ncol(mvt2)) {
  mvt2_numeric[,i] <- as.numeric(mvt2[,i])
}



## ------ discrete time-series plot ------------

offset <- order(rowMeans(mvt2_numeric))/200 - .25
par(family="serif")
parmar <- par("mar")
par(mar=c(5,8,4,1)+.1)
plot(NA, xlim=c(1,ncol(mvt1)),
     ylim=c(0.5, length(thelevels)+.5),
     yaxt="n", xaxt="n",
     xlab="", ylab="",
     yaxs="i")
for(i in 1:nrow(mvt2_numeric)) {
  lines(as.numeric(mvt2_numeric[i,])+offset[i], col=adjustcolor(4, alpha.f=.5))
}
abline(h=seq(.5,9.5,by=1), lty=1)

axis(side=1, at=1:ncol(mvt1), labels=datelabels, las=2)
axis(side=2, at=1:length(thelevels), labels=fulllevels, las=2)

for(j in 1:ncol(mvt2)) text(x=rep(j,length(thelevels)), y=1:length(thelevels), labels=table(mvt2[,j]), cex=.7, font=2)



## -------- Sankey plot -----------------

mvt2_long <- mvt2 %>%
  make_long(names(mvt2))
mvt2_long$node <- factor(mvt2_long$node, levels=thelevels)
mvt2_long$next_node <- factor(mvt2_long$next_node, levels=thelevels)

# thetable <- table(mvt2[,1], useNA="ifany")
# # thetable
# delta <- 5 # 20/(length(thetable)-1)
# # delta <- 20/(length(thetable)-1)
# thestart <- c(-60, -60 + cumsum(thetable) + delta*(1:(length(thetable)-1)))
# top <- -60 + cumsum(thetable) + delta*(0:(length(thetable)-1))
# bottom <- c(-60, -60+cumsum(thetable[-length(thetable)]) + delta*(1:(length(thetable)-1)))
# middle <- (top+bottom)/2

mvt2_long %>%
  ggplot(aes(x = x,
             next_x = next_x,
             node = node,
             next_node = next_node,
             fill = node,
             label=node)) +
  geom_sankey(alpha=.7, type="sankey") +  # show.legend = F
  # geom_sankey_text(labels=mvt2_long$node) +
  # geom_sankey_label(labels=mvt2_long$node) +
  theme_bw() +
  theme(text=element_text(family="serif")) +
  scale_x_discrete(labels=datelabels) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_discrete(labels=fulllevels) +
  # scale_y_continuous(breaks=middle, labels=fulllevels) +
  # theme(panel.grid.major.y = element_blank(),
  #       panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="", fill="")


## ------------ tabular representation of the movements in Sankey plot ------

mvt2_fortable <- mvt2
for(j in 1:ncol(mvt2)) {
  mvt2_fortable[,j] <- factor(mvt2_fortable[,j], levels=rev(thelevels))
}



## table giving proportions TO each node

fromto <- expand.grid(rev(fulllevels), rev(fulllevels))
from <- as.character(fromto[,2])
to <- as.character(fromto[,1])
from[!(1:length(to) %in% seq(from=1, by=length(thelevels), to=length(to)))] <- ""
tableto_x <- matrix(nrow=length(thelevels)^2, ncol=ncol(mvt2)-1)  # x = multinomial counts
tableto_n <- matrix(nrow=length(thelevels)^2, ncol=ncol(mvt2)-1)  # n = multinomial sizes
for(j in 1:ncol(tableto_n)) {
  tableto_x[,j] <- table(mvt2_fortable[,c(j+1,j)])  # this transposes the table
  tableto_n[,j] <- rep(table(mvt2_fortable[,j]), each=length(thelevels))
}
tableto_p <- tableto_x/tableto_n
tableto_sep <- sqrt(tableto_p*(1-tableto_p)/(tableto_n-1))
data.frame(from,to,tableto_x)  # to display the table
data.frame(from,to,tableto_n)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw <- paste0(tableto_x, " ",
       round(tableto_p*100), "% (",
       round(tableto_sep*100), "%)")
out_tbl_raw[tableto_x==0] <- ""
out_tbl <- data.frame(from, to,
           matrix(out_tbl_raw, nrow=nrow(tableto_n), ncol=ncol(tableto_n)))
names(out_tbl)[3:17] <- datelabels[1:15]
# write.csv(out_tbl, "tables/tableto_full.csv")



## table giving proportions FROM each node

fromto <- expand.grid(rev(fulllevels), rev(fulllevels))
to <- as.character(fromto[,2])
from <- as.character(fromto[,1])
to[!(1:length(to) %in% seq(from=1, by=length(thelevels), to=length(to)))] <- ""

tablefrom_x <- matrix(nrow=length(thelevels)^2, ncol=ncol(mvt2)-1)  # x = multinomial counts
tablefrom_n <- matrix(nrow=length(thelevels)^2, ncol=ncol(mvt2)-1)  # n = multinomial sizes
for(j in 1:ncol(tablefrom_n)) {
  tablefrom_x[,j] <- table(mvt2_fortable[,c(j,j+1)])  # this transposes the table
  tablefrom_n[,j] <- rep(table(mvt2_fortable[,j+1]), each=length(thelevels))
}
tablefrom_p <- tablefrom_x/tablefrom_n
tablefrom_sep <- sqrt(tablefrom_p*(1-tablefrom_p)/(tablefrom_n-1))
data.frame(from,to,tablefrom_x)  # to display the table
data.frame(from,to,tablefrom_n)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw <- paste0(tablefrom_x, " ",
                      round(tablefrom_p*100), "% (",
                      round(tablefrom_sep*100), "%)")
out_tbl_raw[tablefrom_x==0] <- ""
out_tbl <- data.frame(from, to,
                      matrix(out_tbl_raw, nrow=nrow(tableto_n), ncol=ncol(tableto_n)))
names(out_tbl)[3:17] <- datelabels[2:16]
# write.csv(out_tbl, "tables/tablefrom_full.csv")



### Creating discrete time-series plot and Sankey plot
### - COLLAPSED DESTINATIONS (non-Tangle nodes collapsed)

# table(as.matrix(mvt1))
mvt2 <- mvt1
# thelevels <- c("NFM","FM","AL","A_GL","A_UT","A_RT","A_ST","A_LT")
thelevels <- c("A_UT","A_RT","A_ST","A_LT")

# fulllevels <- c("Non-Fishing Mort", "Fishing Mort", "At Large", "Glacier Lake",
#                 "Upper Tangle", "Round Tangle", "Shallow Tangle", "Lower Tangle")
fulllevels <- c("Upper Tangle", "Round Tangle", "Shallow Tangle", "Lower Tangle")

for(i in 1:ncol(mvt2)) {
  mvt2[,i] <- factor(mvt2[,i], levels=thelevels)
}
mvt2_numeric <- mvt2
for(i in 1:ncol(mvt2)) {
  mvt2_numeric[,i] <- as.numeric(mvt2[,i])
}



## ------ discrete time-series plot ------------

offset <- order(rowMeans(mvt2_numeric))/200 - .25
par(family="serif")
par(mar=c(5,8,4,1)+.1)
plot(NA, xlim=c(1,ncol(mvt1)),
     ylim=c(0.5, length(thelevels)+.5),
     yaxt="n", xaxt="n",
     xlab="", ylab="",
     yaxs="i")
for(i in 1:nrow(mvt2_numeric)) {
  lines(as.numeric(mvt2_numeric[i,])+offset[i], col=adjustcolor(4, alpha.f=.5))
}
abline(h=seq(.5,9.5,by=1), lty=1)

axis(side=1, at=1:ncol(mvt1), labels=datelabels, las=2)
axis(side=2, at=1:length(thelevels), labels=fulllevels, las=2)

for(j in 1:ncol(mvt2)) text(x=rep(j,length(thelevels)), y=1:length(thelevels), labels=table(mvt2[,j]), cex=.7, font=2)



## ------------ tabular representation of the movements in Sankey plot ------

mvt2_fortable <- mvt2
for(j in 1:ncol(mvt2)) {
  mvt2_fortable[,j] <- factor(mvt2_fortable[,j], levels=rev(thelevels))
}

## table giving proportions TO each node

fromto <- expand.grid(rev(fulllevels), rev(fulllevels))
from <- as.character(fromto[,2])
to <- as.character(fromto[,1])
from[!(1:length(to) %in% seq(from=1, by=length(thelevels), to=length(to)))] <- ""
tableto_x <- matrix(nrow=length(thelevels)^2, ncol=ncol(mvt2)-1)  # x = multinomial counts
tableto_n <- matrix(nrow=length(thelevels)^2, ncol=ncol(mvt2)-1)  # n = multinomial sizes
for(j in 1:ncol(tableto_n)) {
  tableto_x[,j] <- table(mvt2_fortable[,c(j+1,j)])  # this transposes the table
  # tableto_n[,j] <- rep(table(mvt2_fortable[,j]), each=length(thelevels))
  tableto_n[,j] <- rep(colSums(table(mvt2_fortable[,c(j+1,j)])), each=length(thelevels))
}
tableto_p <- tableto_x/tableto_n
tableto_sep <- sqrt(tableto_p*(1-tableto_p)/(tableto_n-1))
data.frame(from,to,tableto_x)  # to display the table
data.frame(from,to,tableto_n)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw <- paste0(tableto_x, " ",
                      round(tableto_p*100), "% (",
                      round(tableto_sep*100), "%)")
out_tbl_raw[tableto_x==0] <- ""
out_tbl <- data.frame(from, to,
                      matrix(out_tbl_raw, nrow=nrow(tableto_n), ncol=ncol(tableto_n)))
names(out_tbl)[3:17] <- datelabels[1:15]
# write.csv(out_tbl, "tables/tableto_collapse1.csv")



## table giving proportions FROM each node

fromto <- expand.grid(rev(fulllevels), rev(fulllevels))
to <- as.character(fromto[,2])
from <- as.character(fromto[,1])
to[!(1:length(to) %in% seq(from=1, by=length(thelevels), to=length(to)))] <- ""

tablefrom_x <- matrix(nrow=length(thelevels)^2, ncol=ncol(mvt2)-1)  # x = multinomial counts
tablefrom_n <- matrix(nrow=length(thelevels)^2, ncol=ncol(mvt2)-1)  # n = multinomial sizes
for(j in 1:ncol(tablefrom_n)) {
  tablefrom_x[,j] <- table(mvt2_fortable[,c(j,j+1)])  # this transposes the table
  tablefrom_n[,j] <- rep(table(mvt2_fortable[,j+1]), each=length(thelevels))
}
tablefrom_p <- tablefrom_x/tablefrom_n
tablefrom_sep <- sqrt(tablefrom_p*(1-tablefrom_p)/(tablefrom_n-1))
data.frame(from,to,tablefrom_x)  # to display the table
data.frame(from,to,tablefrom_n)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw <- paste0(tablefrom_x, " ",
                      round(tablefrom_p*100), "% (",
                      round(tablefrom_sep*100), "%)")
out_tbl_raw[tablefrom_x==0] <- ""
out_tbl <- data.frame(from, to,
                      matrix(out_tbl_raw, nrow=nrow(tableto_n), ncol=ncol(tableto_n)))
names(out_tbl)[3:17] <- datelabels[2:16]
# write.csv(out_tbl, "tables/tablefrom_collapse1.csv")


## -------- Sankey plot -----------------
thelevels1 <- c("Out","A_UT","A_RT","A_ST","A_LT")

fulllevels1 <- c("Out","Upper Tangle", "Round Tangle", "Shallow Tangle", "Lower Tangle")

# mvt2 <- mvt1
for(i in 1:ncol(mvt2)) {
  mvt2[,i] <- as.character(mvt2[,i])
}
mvt2[is.na(mvt2)] <- "Out"
for(i in 1:ncol(mvt2)) {
  mvt2[,i] <- factor(mvt2[,i], levels=thelevels1)
}

mvt2_long <- mvt2 %>%
  make_long("Tagging",    "6/23/2022",  "7/11/2022",  "7/21/2022",  "8/10/2022",  "8/23/2022",  "9/13/2022",  "9/27/2022",
            "10/26/2022", "4/27/2023",  "6/4/2023",   "6/23/2023",  "7/17/2023",  "8/15/2023",  "9/22/2023",  "10/11/2023")
mvt2_long$node <- factor(mvt2_long$node, levels=thelevels1)
mvt2_long$next_node <- factor(mvt2_long$next_node, levels=thelevels1)

# thetable <- table(mvt2[,1], useNA="ifany")
# # thetable
# delta <- 5 # 20/(length(thetable)-1)
# # delta <- 20/(length(thetable)-1)
# thestart <- c(-60, -60 + cumsum(thetable) + delta*(1:(length(thetable)-1)))
# top <- -60 + cumsum(thetable) + delta*(0:(length(thetable)-1))
# bottom <- c(-60, -60+cumsum(thetable[-length(thetable)]) + delta*(1:(length(thetable)-1)))
# middle <- (top+bottom)/2

mvt2_long %>%
  ggplot(aes(x = x,
             next_x = next_x,
             node = node,
             next_node = next_node,
             fill = node,
             label=node)) +
  geom_sankey(alpha=.7, type="sankey") +  # show.legend = F
  # geom_sankey_text(labels=mvt2_long$node) +
  # geom_sankey_label(labels=mvt2_long$node) +
  theme_bw() +
  theme(text=element_text(family="serif")) +
  scale_x_discrete(labels=datelabels) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_discrete(labels=fulllevels1) +
  # scale_y_continuous(breaks=middle, labels=fulllevels) +
  # theme(panel.grid.major.y = element_blank(),
  #       panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="", fill="")



## ------------ tabular representation of the movements in Sankey plot ------

mvt2_fortable <- mvt2
for(j in 1:ncol(mvt2)) {
  mvt2_fortable[,j] <- factor(mvt2_fortable[,j], levels=rev(thelevels1))
}

## table giving proportions TO each node

fromto <- expand.grid(rev(fulllevels1), rev(fulllevels1))
from <- as.character(fromto[,2])
to <- as.character(fromto[,1])
from[!(1:length(to) %in% seq(from=1, by=length(thelevels1), to=length(to)))] <- ""
tableto_x <- matrix(nrow=length(thelevels1)^2, ncol=ncol(mvt2)-1)  # x = multinomial counts
tableto_n <- matrix(nrow=length(thelevels1)^2, ncol=ncol(mvt2)-1)  # n = multinomial sizes
for(j in 1:ncol(tableto_n)) {
  tableto_x[,j] <- table(mvt2_fortable[,c(j+1,j)])  # this transposes the table
  # tableto_n[,j] <- rep(table(mvt2_fortable[,j]), each=length(thelevels))
  tableto_n[,j] <- rep(colSums(table(mvt2_fortable[,c(j+1,j)])), each=length(thelevels1))
}
tableto_p <- tableto_x/tableto_n
tableto_sep <- sqrt(tableto_p*(1-tableto_p)/(tableto_n-1))
data.frame(from,to,tableto_x)  # to display the table
data.frame(from,to,tableto_n)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw <- paste0(tableto_x, " ",
                      round(tableto_p*100), "% (",
                      round(tableto_sep*100), "%)")
out_tbl_raw[tableto_x==0] <- ""
out_tbl <- data.frame(from, to,
                      matrix(out_tbl_raw, nrow=nrow(tableto_n), ncol=ncol(tableto_n)))
names(out_tbl)[3:17] <- datelabels[1:15]
# write.csv(out_tbl, "tables/tableto_collapse2.csv")



## table giving proportions FROM each node

fromto <- expand.grid(rev(fulllevels1), rev(fulllevels1))
to <- as.character(fromto[,2])
from <- as.character(fromto[,1])
to[!(1:length(to) %in% seq(from=1, by=length(thelevels1), to=length(to)))] <- ""

tablefrom_x <- matrix(nrow=length(thelevels1)^2, ncol=ncol(mvt2)-1)  # x = multinomial counts
tablefrom_n <- matrix(nrow=length(thelevels1)^2, ncol=ncol(mvt2)-1)  # n = multinomial sizes
for(j in 1:ncol(tablefrom_n)) {
  tablefrom_x[,j] <- table(mvt2_fortable[,c(j,j+1)])  # this transposes the table
  tablefrom_n[,j] <- rep(table(mvt2_fortable[,j+1]), each=length(thelevels1))
}
tablefrom_p <- tablefrom_x/tablefrom_n
tablefrom_sep <- sqrt(tablefrom_p*(1-tablefrom_p)/(tablefrom_n-1))
data.frame(from,to,tablefrom_x)  # to display the table
data.frame(from,to,tablefrom_n)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw <- paste0(tablefrom_x, " ",
                      round(tablefrom_p*100), "% (",
                      round(tablefrom_sep*100), "%)")
out_tbl_raw[tablefrom_x==0] <- ""
out_tbl <- data.frame(from, to,
                      matrix(out_tbl_raw, nrow=nrow(tableto_n), ncol=ncol(tableto_n)))
names(out_tbl)[3:17] <- datelabels[2:16]
# write.csv(out_tbl, "tables/tablefrom_collapse2.csv")





## how many fish visit how many of the Tangle Lakes?

nlakes <- apply(mvt1, 1, function(x) length(unique(x[x %in% c("A_UT", "A_RT", "A_ST", "A_LT")])))
table(mvt1[,1], nlakes)


# how many fish visit each Tangle Lake?

visitsUpper <- apply(mvt1, 1, function(x) any(x=="A_UT"))
visitsRound <- apply(mvt1, 1, function(x) any(x=="A_RT"))
visitsShallow <- apply(mvt1, 1, function(x) any(x=="A_ST"))
visitsLower <- apply(mvt1, 1, function(x) any(x=="A_LT"))


# how many fish that visit each lake visit one/multiple lakes?

# nlakes <- sum of these
table(visitsUpper, nlakes)
table(visitsRound, nlakes)
table(visitsShallow, nlakes)
table(visitsLower, nlakes)


# how many fish visit each lake ONLY?

sum(visitsLower & !visitsShallow & !visitsRound & !visitsUpper) # 35
sum(!visitsLower & visitsShallow & !visitsRound & !visitsUpper) # 2
sum(!visitsLower & !visitsShallow & visitsRound & !visitsUpper) # 22
sum(!visitsLower & !visitsShallow & !visitsRound & visitsUpper) # 13


# how many fish visit each combination of lakes?

sum(visitsLower & visitsShallow & !visitsRound & !visitsUpper) # 5
sum(!visitsLower & visitsShallow & visitsRound & !visitsUpper) # 22
sum(!visitsLower & !visitsShallow & visitsRound & visitsUpper) # 0
sum(visitsLower & !visitsShallow & visitsRound & !visitsUpper) # 1
