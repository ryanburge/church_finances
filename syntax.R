library(ggplot2)

church <-read.csv("D:/church.csv", stringsAsFactors = FALSE)
church$Month <- as.character(church$Month)
church$Month <- factor(church$Month, levels=unique(church$Month))

church$net <- church$Income - church$Expenses

agg <- aggregate(church$net, list(church$Month), mean)
agg2 <- aggregate(church$Expenses, list(church$Month), mean)
agg3 <- aggregate(church$Income, list(church$Month), mean)
agg3$type <- c("Income")
agg2$type <- c("Expenses")
agg$type <- c("Net")


total <- rbind(agg, agg2, agg3)


colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))

ggplot(data = church, aes(x = Month, y = Expenses, color = Year, label = Year)) +
  geom_line(aes(group = Year), size=1.5) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + geom_text(check_overlap = TRUE) +scale_colour_continuous(guide = FALSE) + scale_colour_gradientn(colours = colfunc(5))  + theme(legend.position="none") + ylab("Total Expenses")

ggplot(data = church, aes(x = Month, y = net, color = Year, label = Year)) +
  geom_line(aes(group = Year), size=1.5) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + geom_text(check_overlap = TRUE) +scale_colour_continuous(guide = FALSE) + geom_hline(yintercept = 0, linetype = "longdash")+ scale_colour_gradientn(colours = colfunc(5))  + theme(legend.position="none") + ylab("Net Income")

ggplot(data = church, aes(x = Month, y = Income, color = Year, label = Year)) +
  geom_line(aes(group = Year), size=1.5) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + geom_text(check_overlap = TRUE) +scale_colour_continuous(guide = FALSE) + geom_hline(yintercept = mean(church$Income), linetype = "longdash")+ scale_colour_gradientn(colours = colfunc(5))  + theme(legend.position="none") + ylab("Total Income")

ggplot(data = church, aes(x = Month, y = net)) + geom_line(aes(group = Year)) +  geom_hline(yintercept = 0, linetype = "longdash") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~Year) + ylab("Net Income")

ggplot(data = church, aes(x = Month, y = Income)) + geom_line(aes(group = Year)) +  geom_hline(yintercept = mean(church$Income), linetype = "longdash") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~Year) + ylab("Total Income")

ggplot(data = church, aes(x = Month, y = Expenses)) + geom_line(aes(group = Year)) +  geom_hline(yintercept = mean(church$Expenses), linetype = "longdash") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~Year) + ylab("Total Expenses")

ggplot(data = total, aes(x = Group.1, y = x, color = type, label = type)) + geom_line(aes(group = type), size=1.5) + theme(axis.text.x = element_text(angle = 90)) + ylab("Dollars") + xlab("Month")
