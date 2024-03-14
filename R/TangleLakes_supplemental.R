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

# jpeg(filename="figures/DiscreteTS_full.jpg", width=9, height=6, units="in", res=300)

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

# dev.off()


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

sankey_full <- mvt2_long %>%
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
sankey_full
# ggsave(sankey_full, filename="figures/Sankey_full.jpg", width=9, height=6, units="in")


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
mvt2_collapsed <- mvt1
# thelevels <- c("NFM","FM","AL","A_GL","A_UT","A_RT","A_ST","A_LT")
thelevels_collapsed <- c("A_UT","A_RT","A_ST","A_LT")

# fulllevels <- c("Non-Fishing Mort", "Fishing Mort", "At Large", "Glacier Lake",
#                 "Upper Tangle", "Round Tangle", "Shallow Tangle", "Lower Tangle")
fulllevels_collapsed <- c("Upper Tangle", "Round Tangle", "Shallow Tangle", "Lower Tangle")

for(i in 1:ncol(mvt2_collapsed)) {
  mvt2_collapsed[,i] <- factor(mvt2_collapsed[,i], levels=thelevels_collapsed)
}
mvt2_numeric_collapsed <- mvt2_collapsed
for(i in 1:ncol(mvt2_collapsed)) {
  mvt2_numeric_collapsed[,i] <- as.numeric(mvt2_collapsed[,i])
}



## ------ discrete time-series plot ------------

# jpeg(filename="figures/DiscreteTS_tangle.jpg", width=9, height=5, units="in", res=300)

offset <- order(rowMeans(mvt2_numeric))/200 - .25
par(family="serif")
par(mar=c(5,8,4,1)+.1)
plot(NA, xlim=c(1,ncol(mvt1)),
     ylim=c(0.5, length(thelevels_collapsed)+.5),
     yaxt="n", xaxt="n",
     xlab="", ylab="",
     yaxs="i")
for(i in 1:nrow(mvt2_numeric_collapsed)) {
  lines(as.numeric(mvt2_numeric_collapsed[i,])+offset[i], col=adjustcolor(4, alpha.f=.5))
}
abline(h=seq(.5,9.5,by=1), lty=1)

axis(side=1, at=1:ncol(mvt1), labels=datelabels, las=2)
axis(side=2, at=1:length(thelevels_collapsed), labels=fulllevels_collapsed, las=2)

for(j in 1:ncol(mvt2_collapsed)) text(x=rep(j,length(thelevels_collapsed)), y=1:length(thelevels_collapsed), labels=table(mvt2_collapsed[,j]), cex=.7, font=2)

# dev.off()



## revisiting this plot, highlighting the number of times fish moved
nmoves <- apply(mvt2_numeric_collapsed, 1, function(x) sum(abs(diff(x)), na.rm=TRUE))
par(mfrow=c(2,2))
for(imoves in 1:max(nmoves)) {
  
  # jpeg(filename="figures/DiscreteTS_tangle.jpg", width=9, height=5, units="in", res=300)

  offset <- order(rowMeans(mvt2_numeric_collapsed))/200 - .25
  par(family="serif")
  par(mar=c(5,8,4,1)+.1)
  plot(NA, xlim=c(1,ncol(mvt1)),
       ylim=c(0.5, length(thelevels_collapsed)+.5),
       yaxt="n", xaxt="n",
       xlab="", ylab="",
       yaxs="i",
       main=paste(imoves, "moves"))
  for(i in 1:nrow(mvt2_numeric_collapsed)) {
    lines(as.numeric(mvt2_numeric_collapsed[i,])+offset[i], 
          col=adjustcolor(ifelse(nmoves[i]==imoves, 1, 4), alpha.f=
                            ifelse(nmoves[i]==imoves, .7, .2)),
          lwd=ifelse(nmoves[i]==imoves, 2, 1))
  }
  abline(h=seq(.5,9.5,by=1), lty=1)
  
  axis(side=1, at=1:ncol(mvt1), labels=datelabels, las=2)
  axis(side=2, at=1:length(thelevels_collapsed), labels=fulllevels_collapsed, las=2)
  
  for(j in 1:ncol(mvt2)) text(x=rep(j,length(thelevels_collapsed)), y=1:length(thelevels_collapsed), labels=table(mvt2_collapsed[,j]), cex=.7, font=2)

  # dev.off()

}

par(mfrow=c(1,1))



## ------------ tabular representation of the movements in Sankey plot ------

mvt2_fortable_collapsed <- mvt2_collapsed
for(j in 1:ncol(mvt2_collapsed)) {
  mvt2_fortable_collapsed[,j] <- factor(mvt2_fortable_collapsed[,j], levels=rev(thelevels_collapsed))
}

## table giving proportions TO each node

fromto_collapsed <- expand.grid(rev(fulllevels_collapsed), rev(fulllevels_collapsed))
from_collapsed <- as.character(fromto_collapsed[,2])
to_collapsed <- as.character(fromto_collapsed[,1])
from_collapsed[!(1:length(to_collapsed) %in% seq(from=1, by=length(thelevels_collapsed), to=length(to_collapsed)))] <- ""
tableto_x_collapsed <- matrix(nrow=length(thelevels_collapsed)^2, ncol=ncol(mvt2_collapsed)-1)  # x = multinomial counts
tableto_n_collapsed <- matrix(nrow=length(thelevels_collapsed)^2, ncol=ncol(mvt2_collapsed)-1)  # n = multinomial sizes
for(j in 1:ncol(tableto_n_collapsed)) {
  tableto_x_collapsed[,j] <- table(mvt2_fortable_collapsed[,c(j+1,j)])  # this transposes the table
  # tableto_n[,j] <- rep(table(mvt2_fortable[,j]), each=length(thelevels))
  tableto_n_collapsed[,j] <- rep(colSums(table(mvt2_fortable_collapsed[,c(j+1,j)])), each=length(thelevels_collapsed))
}
tableto_p_collapsed <- tableto_x_collapsed/tableto_n_collapsed
tableto_sep_collapsed <- sqrt(tableto_p_collapsed*(1-tableto_p_collapsed)/(tableto_n_collapsed-1))
data.frame(from_collapsed,to_collapsed,tableto_x_collapsed)  # to display the table
data.frame(from_collapsed,to_collapsed,tableto_n_collapsed)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw_collapsed <- paste0(tableto_x_collapsed, " ",
                      round(tableto_p_collapsed*100), "% (",
                      round(tableto_sep_collapsed*100), "%)")
out_tbl_raw_collapsed[tableto_x_collapsed==0] <- ""
out_tbl_collapsed <- data.frame(from_collapsed, to_collapsed,
                      matrix(out_tbl_raw_collapsed, nrow=nrow(tableto_n_collapsed), ncol=ncol(tableto_n_collapsed)))
names(out_tbl_collapsed)[3:17] <- datelabels[1:15]
# write.csv(out_tbl_collapsed, "tables/tableto_collapse1.csv")



## table giving proportions FROM each node

fromto_collapsed <- expand.grid(rev(fulllevels_collapsed), rev(fulllevels_collapsed))
to_collapsed <- as.character(fromto_collapsed[,2])
from_collapsed <- as.character(fromto_collapsed[,1])
to_collapsed[!(1:length(to_collapsed) %in% seq(from=1, by=length(thelevels_collapsed), to=length(to_collapsed)))] <- ""

tablefrom_x_collapsed <- matrix(nrow=length(thelevels_collapsed)^2, ncol=ncol(mvt2_collapsed)-1)  # x = multinomial counts
tablefrom_n_collapsed <- matrix(nrow=length(thelevels_collapsed)^2, ncol=ncol(mvt2_collapsed)-1)  # n = multinomial sizes
for(j in 1:ncol(tablefrom_n_collapsed)) {
  tablefrom_x_collapsed[,j] <- table(mvt2_fortable_collapsed[,c(j,j+1)])  # this transposes the table
  tablefrom_n_collapsed[,j] <- rep(table(mvt2_fortable_collapsed[,j+1]), each=length(thelevels_collapsed))
}
tablefrom_p_collapsed <- tablefrom_x_collapsed/tablefrom_n_collapsed
tablefrom_sep_collapsed <- sqrt(tablefrom_p_collapsed*(1-tablefrom_p_collapsed)/(tablefrom_n_collapsed-1))
data.frame(from_collapsed,to_collapsed,tablefrom_x_collapsed)  # to display the table
data.frame(from_collapsed,to_collapsed,tablefrom_n_collapsed)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw_collapsed <- paste0(tablefrom_x_collapsed, " ",
                      round(tablefrom_p_collapsed*100), "% (",
                      round(tablefrom_sep_collapsed*100), "%)")
out_tbl_raw_collapsed[tablefrom_x_collapsed==0] <- ""
out_tbl_collapsed <- data.frame(from_collapsed, to_collapsed,
                      matrix(out_tbl_raw_collapsed, nrow=nrow(tableto_n_collapsed), ncol=ncol(tableto_n_collapsed)))
names(out_tbl_collapsed)[3:17] <- datelabels[2:16]
# write.csv(out_tbl_collapsed, "tables/tablefrom_collapse1.csv")


## -------- Sankey plot -----------------
thelevels1_collapsed <- c("Out","A_UT","A_RT","A_ST","A_LT")

fulllevels1_collapsed <- c("Out","Upper Tangle", "Round Tangle", "Shallow Tangle", "Lower Tangle")

# mvt2 <- mvt1
for(i in 1:ncol(mvt2_collapsed)) {
  mvt2_collapsed[,i] <- as.character(mvt2_collapsed[,i])
}
mvt2_collapsed[is.na(mvt2_collapsed)] <- "Out"
for(i in 1:ncol(mvt2_collapsed)) {
  mvt2_collapsed[,i] <- factor(mvt2_collapsed[,i], levels=thelevels1_collapsed)
}

mvt2_long_collapsed <- mvt2_collapsed %>%
  make_long("Tagging",    "6/23/2022",  "7/11/2022",  "7/21/2022",  "8/10/2022",  "8/23/2022",  "9/13/2022",  "9/27/2022",
            "10/26/2022", "4/27/2023",  "6/4/2023",   "6/23/2023",  "7/17/2023",  "8/15/2023",  "9/22/2023",  "10/11/2023")
mvt2_long_collapsed$node <- factor(mvt2_long_collapsed$node, levels=thelevels1_collapsed)
mvt2_long_collapsed$next_node <- factor(mvt2_long_collapsed$next_node, levels=thelevels1_collapsed)

# thetable <- table(mvt2[,1], useNA="ifany")
# # thetable
# delta <- 5 # 20/(length(thetable)-1)
# # delta <- 20/(length(thetable)-1)
# thestart <- c(-60, -60 + cumsum(thetable) + delta*(1:(length(thetable)-1)))
# top <- -60 + cumsum(thetable) + delta*(0:(length(thetable)-1))
# bottom <- c(-60, -60+cumsum(thetable[-length(thetable)]) + delta*(1:(length(thetable)-1)))
# middle <- (top+bottom)/2

sankey_collapse <- mvt2_long_collapsed %>%
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
  scale_fill_discrete(labels=fulllevels1_collapsed) +
  # scale_y_continuous(breaks=middle, labels=fulllevels) +
  # theme(panel.grid.major.y = element_blank(),
  #       panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="", fill="")
sankey_collapse
# ggsave(sankey_collapse, filename="figures/Sankey_collapse.jpg", width=9, height=6, units="in")


## ------------ another tabular representation of the movements in Sankey plot ------

mvt2_fortable_collapsed1 <- mvt2_collapsed
for(j in 1:ncol(mvt2_fortable_collapsed1)) {
  mvt2_fortable_collapsed1[,j] <- factor(mvt2_fortable_collapsed1[,j], levels=rev(thelevels1_collapsed))
}

## table giving proportions TO each node

fromto_collapsed1 <- expand.grid(rev(fulllevels1_collapsed), rev(fulllevels1_collapsed))
from_collapsed1 <- as.character(fromto_collapsed1[,2])
to_collapsed1 <- as.character(fromto_collapsed1[,1])
from_collapsed1[!(1:length(to_collapsed1) %in% seq(from=1, by=length(thelevels1_collapsed), to=length(to_collapsed1)))] <- ""
tableto_x_collapsed1 <- matrix(nrow=length(thelevels1_collapsed)^2, ncol=ncol(mvt2_collapsed)-1)  # x = multinomial counts
tableto_n_collapsed1 <- matrix(nrow=length(thelevels1_collapsed)^2, ncol=ncol(mvt2_collapsed)-1)  # n = multinomial sizes
for(j in 1:ncol(tableto_n_collapsed1)) {
  tableto_x_collapsed1[,j] <- table(mvt2_fortable_collapsed1[,c(j+1,j)])  # this transposes the table
  # tableto_n[,j] <- rep(table(mvt2_fortable[,j]), each=length(thelevels))
  tableto_n_collapsed1[,j] <- rep(colSums(table(mvt2_fortable_collapsed1[,c(j+1,j)])), each=length(thelevels1_collapsed))
}
tableto_p_collapsed1 <- tableto_x_collapsed1/tableto_n_collapsed1
tableto_sep_collapsed1 <- sqrt(tableto_p_collapsed1*(1-tableto_p_collapsed1)/(tableto_n_collapsed1-1))
data.frame(from_collapsed1,to_collapsed1,tableto_x_collapsed1)  # to display the table
data.frame(from_collapsed1,to_collapsed1,tableto_n_collapsed1)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw_collapsed1 <- paste0(tableto_x_collapsed1, " ",
                      round(tableto_p_collapsed1*100), "% (",
                      round(tableto_sep_collapsed1*100), "%)")
out_tbl_raw_collapsed1[tableto_x_collapsed1==0] <- ""
out_tbl_collapsed1 <- data.frame(from_collapsed1, to_collapsed1,
                      matrix(out_tbl_raw_collapsed1, nrow=nrow(tableto_n_collapsed1), ncol=ncol(tableto_n_collapsed1)))
names(out_tbl_collapsed1)[3:17] <- datelabels[1:15]
# write.csv(out_tbl_collapsed1, "tables/tableto_collapse2.csv")



## table giving proportions FROM each node

fromto_collapsed1 <- expand.grid(rev(fulllevels1_collapsed), rev(fulllevels1_collapsed))
to_collapsed1 <- as.character(fromto_collapsed1[,2])
from_collapsed1 <- as.character(fromto_collapsed1[,1])
to_collapsed1[!(1:length(to_collapsed1) %in% seq(from=1, by=length(thelevels1_collapsed), to=length(to_collapsed1)))] <- ""

tablefrom_x_collapsed1 <- matrix(nrow=length(thelevels1_collapsed)^2, ncol=ncol(mvt2_collapsed)-1)  # x = multinomial counts
tablefrom_n_collapsed1 <- matrix(nrow=length(thelevels1_collapsed)^2, ncol=ncol(mvt2_collapsed)-1)  # n = multinomial sizes
for(j in 1:ncol(tablefrom_n_collapsed1)) {
  tablefrom_x_collapsed1[,j] <- table(mvt2_fortable_collapsed1[,c(j,j+1)])  # this transposes the table
  tablefrom_n_collapsed1[,j] <- rep(table(mvt2_fortable_collapsed1[,j+1]), each=length(thelevels1_collapsed))
}
tablefrom_p_collapsed1 <- tablefrom_x_collapsed1/tablefrom_n_collapsed1
tablefrom_sep_collapsed1 <- sqrt(tablefrom_p_collapsed1*(1-tablefrom_p_collapsed1)/(tablefrom_n_collapsed1-1))
data.frame(from_collapsed1,to_collapsed1,tablefrom_x_collapsed1)  # to display the table
data.frame(from_collapsed1,to_collapsed1,tablefrom_n_collapsed1)  # to display the table

## want to do an output table n p% (sep%)
out_tbl_raw_collapsed1 <- paste0(tablefrom_x_collapsed1, " ",
                      round(tablefrom_p_collapsed1*100), "% (",
                      round(tablefrom_sep_collapsed1*100), "%)")
out_tbl_raw_collapsed1[tablefrom_x_collapsed1==0] <- ""
out_tbl_collapsed1 <- data.frame(from_collapsed1, to_collapsed1,
                      matrix(out_tbl_raw_collapsed1, nrow=nrow(tableto_n_collapsed1), ncol=ncol(tableto_n_collapsed1)))
names(out_tbl_collapsed1)[3:17] <- datelabels[2:16]
# write.csv(out_tbl_collapsed1, "tables/tablefrom_collapse2.csv")





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

#### would really like to summarize this info in a single table
as_df <- data.frame(visitsLower, visitsShallow, visitsRound, visitsUpper)
column_scenarios <- list(1,2,3,4,1:2,2:3,3:4,c(1,3),c(2,4),c(1,4))
scenario_sums <- NA
for(i in 1:length(column_scenarios)) {
  scenario_sums[i] <- sum(as_df)   ##### incomplete!!
}


# revisiting tagging data
tagdata <- read_csv("flat_data/Tangle_movement.csv")[, 1:4] %>%
  mutate(Lake=factor(Lake, levels=c("Lower","Shallow","Round","Upper")))

with(tagdata, boxplot(Length ~ Lake))

tagdata %>%
  ggplot(aes(x=Length)) +
  geom_histogram(breaks=seq(400,750,by=50),) +
  facet_wrap(vars(Lake), nrow=4) +
  theme_bw() +
  theme(text=element_text(family="serif"))

tagdata %>%
  ggplot(aes(y=Length, x=Lake)) +
  geom_boxplot() +
  theme_bw() +
  theme(text=element_text(family="serif"))

## this didn't work like expected
# tagdata %>%
#   mutate(lakemoves=paste(Lake,nlakes)) -> tag2 #%>% 
#   ggplot(aes(y=Length, x=lakemoves)) +
#   geom_boxplot() +
#   theme_bw() +
#   theme(text=element_text(family="serif"))

# tagdata %>% 
#   mutate(nlakes=nlakes) %>%
#   filter(Lake %in% c("Round","Shallow")) %>%
#   ggplot(aes(y=Length, group=nlakes, x=nlakes)) +
#   geom_boxplot() +
#   theme_bw() +
#   theme(text=element_text(family="serif"))

cbind(tagdata, mvt2_numeric_collapsed) %>%
  pivot_longer(values_to="lake_num", cols=5:20, names_to="Survey") %>%  #print(n=20)
  ggplot(aes(y=Length, x=lake_num, col=Lake, group=Unique)) +
  geom_point() +
  geom_line()



## chi2 test truncated to round & shallow: all times to all times
mvt2_collapsed
k <- 1
thetables <- list()
for(i_first in 1:(ncol(mvt2_collapsed)-1)) {
  for(i_second in (i_first+1):ncol(mvt2_collapsed)) {
    thetables[[k]] <- table(mvt2_collapsed[mvt2_collapsed[,1] %in% c("A_RT","A_ST"), i_first], 
                            mvt2_collapsed[mvt2_collapsed[,1] %in% c("A_RT","A_ST"), i_second])[3:4,3:4]
    if(chisq.test(thetables[[k]], simulate.p.value=T)$p.value>.05 & i_first==1) print(thetables[[k]])
    k <- k+1
  }
}
thechisqtests <- lapply(thetables, chisq.test, simulate.p.value=T)
thepvals <- sapply(thechisqtests, function(x) x$p.value)
min_exp_n <- sapply(thechisqtests, function(x) min(x$expected))
par(mfrow=c(1,1))
plot(thepvals, pch=ifelse(min_exp_n >= 5, 16, 1))
abline(h=.05)
sum(thepvals > .05)

plot(min_exp_n, thepvals, pch=ifelse(seq_along(thepvals) < ncol(mvt2_collapsed), 16, 1))
abline(h=.05, v=5)



# how many movements between each pair of lakes
the_array <- array(dim=c(length(thelevels1_collapsed), length(thelevels1_collapsed) ,ncol(mvt2_collapsed)-1))
for(i_col in 1:(ncol(mvt2_collapsed)-1)) {
  the_array[,,i_col] <- table(mvt2_collapsed[,i_col], mvt2_collapsed[,i_col+1])
}
nmovements <- apply(the_array, 1:2, sum)
rownames(nmovements) <- colnames(nmovements) <- thelevels1_collapsed

sum(nmovements[upper.tri(nmovements)])
sum(nmovements[lower.tri(nmovements)])

justbetween <- nmovements + t(nmovements)
justbetween[!upper.tri(justbetween)] <- NA
