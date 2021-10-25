with(df, plot(Weekday ~ Date))
# Sent daily through end of 2020, then 3x/week (MWF)


# Number of clicks
plot(df$Clicks ~ df$`Date/Time`)
ggplot(data = df, aes(x = Date, y = Clicks)) +
  geom_point() +
  xlim(c(mdy(06012021), mdy(06302021)))
# Number of Unsubscribes
plot(df$Unsubscribes ~ df$`Date/Time`)


plot(df$Opens ~ df$Date)

plot(df$Opens ~ df$Sends)
abline(a = 0, b = 1, col = 1)
glm(Opens ~ Sends, data = df)
abline(a= 184, b = 0.4455, col = 2)
text(x = 1800, y = 1800, labels = "Opens = Sends")



# Check Time-of-Day variation
plot(df$Clicks ~ as_datetime(df$Time))
plot(df$Unsubscribes ~ as_datetime(df$Time))
     

# Check unsubscribe correlation
with(df, plot(Unsubscribes ~ `Word Count`))
with(df, plot(Unsubscribes ~ Sends))
with(df, plot(Unsubscribes ~ Opens))     
with(df, plot(Unsubscribes ~ `Link Count`))
with(df, plot(Unsubscribes ~ Month))
with(df, plot(Unsubscribes ~ jitter(df$Weekday)))

# Check out day-of-week correlation
with(df2020, plot(Sends ~ Weekday))
with(df2021, plot(Sends ~ Weekday))
with(df2020, plot(Opens ~ Weekday))
with(df2021, plot(Opens ~ Weekday))


for (i in 1:nrow(maxclicks)) {
  index <- which((linkset$Clicks == maxclicks$max[i]) & (linkset$date == maxclicks$date[i]))
  linkset$max[index] <- TRUE
}
maxlinks <- linkset[linkset$max == TRUE,]

transition <- linkset[(linkset$date > mdy(06072021) & linkset$date < mdy(06142021)),]
breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
par(mfrow = c(3,3))

hist(linkset$Clicks[linkset$date == mdy(06022021)], main = "Jun 2", xlab = "# of Clicks", xlim = c(0,70), breaks = breaks)
hist(linkset$Clicks[linkset$date == mdy(06042021)], main = "Jun 4", xlab = "# of Clicks", xlim = c(0,70), breaks = breaks)
hist(linkset$Clicks[linkset$date == mdy(06072021)], main = "Jun 7", xlab = "# of Clicks", xlim = c(0,70), breaks = breaks)
hist(linkset$Clicks[linkset$date == mdy(06092021)], main = "Jun 9", xlab = "# of Clicks", xlim = c(0,70), breaks = breaks)
hist(linkset$Clicks[linkset$date == mdy(06112021)], main = "Jun 11", xlab = "# of Clicks", xlim = c(0,70), breaks = breaks)
hist(linkset$Clicks[linkset$date == mdy(06142021)], main = "Jun 14", xlab = "# of Clicks", xlim = c(0,70), breaks = breaks)
hist(linkset$Clicks[linkset$date == mdy(06162021)], main = "Jun 16", xlab = "# of Clicks", xlim = c(0,70), breaks = breaks)
hist(linkset$Clicks[linkset$date == mdy(06182021)], main = "Jun 18", xlab = "# of Clicks", xlim = c(0,70), breaks = breaks)
hist(linkset$Clicks[linkset$date == mdy(06212021)], main = "Jun 21", xlab = "# of Clicks", xlim = c(0,70), breaks = breaks)

################################
####  Indices for links
#################################
linkset2$Class <- 'Other'
#Wiki links
Wiki <- grep('wiki', linkset2$Link)
linkset2$Class[Wiki] <- 'Wikipedia'
#Self-links
Self <- grep('thecolumn.co', linkset2$Link)
linkset2$Class[Self] <- 'Self-link'
# CC License links
CC <- grep('license', linkset2$Link)
linkset2$Class[CC] <- 'Creative Commons'
# Donation link
Donate <- grep('givebutter', linkset2$Link)
linkset2$Class[Donate] <- 'Donation'
#Unsubscribe link
Unsub <- grep('Unsubscribe', linkset2$Link)
linkset2$Class[Unsub] <- 'Unsubscribe'
#Government link
Gov <- grep('.gov', linkset2$Link)
linkset2$Class[Gov] <- 'Govt'
# News link
pub <- 'reuters|news|cen.acs.org|chemweek|wsj.com|chemicalengineer.com|guardian.com|article|
  scienctdirect.com|ogj.com|nytimes.com|npr.org|forbes.com|cnbc.com|bloomberg.com'
News <- grep(pub, linkset2$Link)
linkset2$Class[News] <- 'News'
# # Corporate link
# comp <- 'dupont.com|exxonmobil.com|dow.com|evonik|unioncarbide.com|solvay.com|shell.com|
#   nouryon.com|neste.com|lanzatech.com|eastman.com'
# corp <- grep(comp, linkset2$Link)
# linkset2$Class[corp] <- 'Corporate'
# Image link
img <- grep('.jpg|photo', linkset2$Link)
linkset2$Class[img] <- 'Image'
#Video
vid <- grep('youtube|tiktok', linkset2$Link)
linkset2$Class[vid] <- 'Video'
#Sharing Links
LI <- grep('linkedin.com/share', linkset2$Link)
linkset2$Class[LI] <- 'LinkedIn Share'
FB <- grep('facebook.com/share', linkset2$Link)
linkset2$Class[FB] <- 'Facebook Share'
Mail <- grep('?body', linkset2$Link)
linkset2$Class[Mail] <- 'eMail Share'
Shares <- unique(LI %>% append(FB) %>% append(Mail))
####################
## Sharing
###################

par(mfrow = c(1,4))
with(linkset2[LI,], plot(Clicks ~ date, main = 'LinkedIn Shares', ylim = c(0,50)))
with(linkset2[FB,], plot(Clicks ~ date, main = "Facebook Shares", ylim = c(0,50)))
with(linkset2[Mail,], plot(Clicks ~ date, main = "E-mail Shares", ylim = c(0,50)))
with(linkset2[Shares,], plot(Clicks ~ date, main = "Total Shares", ylim = c(0,50)))
par(mfrow = c(1,1))

## Wiki Link performance
with(linkset2[Wiki,], plot(Clicks ~ date, main = 'Wiki links'))
## Self-Link performance
with(linkset2[Self,], plot(Clicks ~ date, main = 'Self-links'))
## CC link performance
with(linkset2[CC,], plot(Clicks ~ date, main = 'Creative Commons License Pages'))
## Donation link performance
with(linkset2[Donate,], plot(Clicks ~ date, main = 'Donation Clicks'))

# Compare Unsubscribe info
U1 <- linkset2[Unsub,c(3:4)] # Number of unsubscribes from link files
names(U1)[2] <- 'Date'
U2 <- df[,c(11,8)]
Unsubscribe <- merge(U1, U2)
ggplot(data = Unsubscribe) +
  geom_point(aes(x = Date, y = Clicks), shape = 0) +
  geom_point(aes(x = Date, y = Unsubscribes), shape = 1)
with(Unsubscribe, plot(Unsubscribes ~ Clicks, xlab = "Clicks from Link Files", ylab = "Unsubscribes from Summary File"))
