source("L:/Data Science/R Scripts/utils.R")
cItems <- dbGetQuery(vertica,"select * from sandbox_supply_chain.wmg_c_lifts")
targ <- seq(0.75,0.95,0.01)
cItems <- cItems[!is.infinite(cItems$total_ss),]
cItems <- cItems[!is.nan(cItems$total_ss),]
cItems <- cItems[!is.na(cItems$total_ss),]
cItems$total_ss <- ifelse(cItems$total_ss > 60,60,cItems$total_ss)
cItems$total_ss <- ifelse(cItems$total_ss < 6,6,cItems$total_ss)
cItems$slChar <- as.character(cItems$sl)
impact <- ldply(targ,function(x){
  sub <- cItems[cItems$slChar == x,]
  lessThan <- sub[sub$min_safetystk < sub$total_ss,]
  lessThan$increment <- (lessThan$total_ss - lessThan$min_safetystk) * lessThan$avedemqty
  lessThan$increment_dollars <- (lessThan$total_ss - lessThan$min_safetystk) * lessThan$avedemqty * lessThan$unitcost

  data.frame(sl = as.numeric(x),
             ss_increase = sum(lessThan$increment),
             dollar_increase = sum(lessThan$increment_dollars),
             increase_items = nrow(lessThan),
             totalItems = nrow(sub))
})
library(scales)
ggplot(impact,aes(x = sl,y = ss_increase)) +
  geom_line() +
 # coord_cartesian(ylim = c(400000,800000)) +
  scale_x_continuous(breaks = as.numeric(targ),minor_breaks = F) +
  scale_y_continuous(labels = comma,breaks = seq(400000,800000,50000))+
  labs(x = "Minimum SL",y = "Incremental SS Units",title = "Incremental SS Units by Bringing C Items to Minimum SL")



### now decrease ss to no more than...

ceilingImpact <- ldply(targ,function(x){
  sub <- cItems[cItems$slChar == x,]
  greaterThan <- sub[sub$min_safetystk > sub$total_ss,]
  greaterThan$increment <- (greaterThan$total_ss - greaterThan$min_safetystk) * greaterThan$avedemqty
  greaterThan$increment_dollars <- (greaterThan$total_ss - greaterThan$min_safetystk) * greaterThan$avedemqty * greaterThan$unitcost
  
  data.frame(sl = as.numeric(x),
             ss_decrease = sum(greaterThan$increment),
             dollar_decrease = sum(greaterThan$increment_dollars),
             decrease_items = nrow(greaterThan),
             totalItems = nrow(sub))
})


ggplot(ceilingImpact,aes(x = sl,y = ss_decrease)) +
  geom_line() +
  # coord_cartesian(ylim = c(400000,800000)) +
  scale_x_continuous(breaks = as.numeric(targ),minor_breaks = F) +
  #scale_y_continuous(labels = comma,breaks = seq(400000,800000,50000))+
  labs(x = "Minimum SL",y = "Incremental SS Units",title = "Incremental SS Units by Bringing C Items to Minimum SL")

tops <- unique(cItems[,c("item","location","avedemqty")])
tops <- tops[order(-tops$avedemqty),]
tops <- tops[1:floor(nrow(tops) / 2),]               
tops <- merge(tops[,c("item","location")],cItems,by = c("item","location"))
impactTop <- ldply(targ,function(x){
  sub <- tops[tops$slChar == x,]
  lessThan <- sub[sub$min_safetystk < sub$total_ss,]
  lessThan$increment <- (lessThan$total_ss - lessThan$min_safetystk) * lessThan$avedemqty
  lessThan$increment_dollars <- (lessThan$total_ss - lessThan$min_safetystk) * lessThan$avedemqty * lessThan$unitcost
  
  data.frame(sl = as.numeric(x),
             ss_increase = sum(lessThan$increment),
             dollar_increase = sum(lessThan$increment_dollars),
             increase_items = nrow(lessThan),
             totalItems = nrow(sub))
})
