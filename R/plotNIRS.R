
plotNIRS <- function(Yvec,timevec,transfusionvec)
{
    #require(ggplot2)
    #require(mgcv)
    #require(gridExtra)
    datsub = data.frame(Gut = Yvec, tmsSec = timevec, Trans = transfusionvec)
    datsub = datsub[!is.na(datsub$Gut), ]
    if(sum(datsub$Gut == 15)>0)
    {
        datsub$Gut[datsub$Gut == 15] = 7.5
        datnew = datsub[datsub$Gut == 7.5, ]
        datnew$Gut = 15
    	
        d1 <- ggplot(datsub[datsub$Trans == 0, ], aes(x = datsub[datsub$Trans == 0, ]$tmsSec, 
            y = datsub[datsub$Trans == 0, ]$Gut)) + ylim(0, 100) + xlab("Time") + ylab("Outcome") + 
            labs(title = "Before-transfusion") + theme(plot.title = element_text(hjust = 0.5)) + 
            geom_point(data = datnew[datnew$Trans == 0, ], mapping = aes(x = datnew[datnew$Trans == 0, ]$tmsSec, 
                y = datnew[datnew$Trans == 0, ]$Gut), colour = "red") + geom_point(data = datsub[datsub$Trans == 
            0 & datsub$Gut != 7.5, ], mapping = aes(x =  datsub[datsub$Trans == 0 & datsub$Gut != 7.5, ]$tmsSec, y = datsub[datsub$Trans == 0 & datsub$Gut != 7.5, ]$Gut)) + 
            stat_smooth(fill = "blue", colour = "darkblue", size = 1, 
                alpha = 0.3, method = "auto")
    			
        d2 <- ggplot(datsub[datsub$Trans == 1, ], aes(x = datsub[datsub$Trans == 1, ]$tmsSec, 
            y = datsub[datsub$Trans == 1, ]$Gut)) + ylim(0, 100) + xlab("Time") + ylab("Outcome") + 
            labs(title = "After-transfusion") + theme(plot.title = element_text(hjust = 0.5)) + 
            geom_point(data = datnew[datnew$Trans == 1, ], mapping = aes(x = datnew[datnew$Trans == 1, ]$tmsSec, 
                y = datnew[datnew$Trans == 1, ]$Gut), colour = "red") + geom_point(data = datsub[datsub$Trans == 1 & datsub$Gut != 7.5, ], mapping = aes(x = datsub[datsub$Trans == 1 & datsub$Gut != 7.5, ]$tmsSec, y = datsub[datsub$Trans == 1 & datsub$Gut != 7.5, ]$Gut)) + 
            stat_smooth(fill = "blue", colour = "darkblue", size = 1, 
                alpha = 0.3, method = "auto")
        
    }else
    {
        d1 <- ggplot(datsub[datsub$Trans == 0, ], aes(x = datsub[datsub$Trans == 0, ]$tmsSec,y = datsub[datsub$Trans == 0, ]$Gut)) + 
            ylim(0, 100) + xlab("Time") + ylab("Outcome") + 
            labs(title = "Before-transfusion") + theme(plot.title = element_text(hjust = 0.5)) + 
            geom_point() + 
            stat_smooth(fill = "blue", colour = "darkblue", size = 1, alpha = 0.3, method = "auto")
        
        d2 <- ggplot(datsub[datsub$Trans == 1, ], aes(x = datsub[datsub$Trans == 1, ]$tmsSec,y = datsub[datsub$Trans == 1, ]$Gut)) + 
            ylim(0, 100) + xlab("Time") + ylab("Outcome") + 
            labs(title = "After-transfusion") + theme(plot.title = element_text(hjust = 0.5)) + 
            geom_point() + 
            stat_smooth(fill = "blue", colour = "darkblue", size = 1, alpha = 0.3, method = "auto")
    }
    grid.arrange(d1, d2, ncol = 2)
}