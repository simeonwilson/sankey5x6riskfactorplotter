

risk_factors_for_stroke_sankey_diagram_with_space <- function(data_to_be_plotted, plot_title_in_quotes){
  data_to_be_plotted<- as.data.frame(star)
  colnames(star)<- c("type", "1990", "1995", "2000", "2005", "2010")

  a <- data.frame("year" = 1990,"risk"=star$`1990`,"type" = star$type)
  a<- a[order(a$risk),]
  a1 <- data.frame("year" = 1995,"risk"=star$`1995`,"type" = star$type)
  a1<- a1[order(a1$risk),]
  a2 <- data.frame("year" = 2000,"risk"=star$`2000`,"type" = star$type)
  a2<- a2[order(a2$risk),]
  a3 <- data.frame("year" = 1995,"risk"=star$`2005`,"type" = star$type)
  a3<- a3[order(a3$risk),]
  a4 <- data.frame("year" = 1995,"risk"=star$`2010`,"type" = star$type)
  a4<- a4[order(a4$risk),]

  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }

  #not summed, used for upper and lowe limits

  smoke1<-c((a[(which(a$type == "Smoking")),2]),(a1[(which(a1$type == "Smoking")),2]) ,(a2[(which(a2$type == "Smoking")),2]),(a3[(which(a3$type == "Smoking")),2]) ,(a4[(which(a4$type == "Smoking")),2])     )

  Hypercholesterolemia1<-c((a[(which(a$type == "Hypercholesterolemia")),2]),(a1[(which(a1$type == "Hypercholesterolemia")),2]) ,(a2[(which(a2$type == "Hypercholesterolemia")),2]),(a3[(which(a3$type == "Hypercholesterolemia")),2]) ,(a4[(which(a4$type == "Hypercholesterolemia")),2]))

  Obesity1<-c((a[(which(a$type == "Obesity")),2]),(a1[(which(a1$type == "Obesity")),2]) ,(a2[(which(a2$type == "Obesity")),2]),(a3[(which(a3$type == "Obesity")),2]) ,(a4[(which(a4$type == "Obesity")),2])     )

  Diabetes1<-c((a[(which(a$type == "Diabetes")),2]),(a1[(which(a1$type == "Diabetes")),2]) ,(a2[(which(a2$type == "Diabetes")),2]),(a3[(which(a3$type == "Diabetes")),2]) ,(a4[(which(a4$type == "Diabetes")),2])     )

  Hypertension1<-c((a[(which(a$type == "Hypertension")),2]),(a1[(which(a1$type == "Hypertension")),2]) ,(a2[(which(a2$type == "Hypertension")),2]),(a3[(which(a3$type == "Hypertension")),2]) ,(a4[(which(a4$type == "Hypertension")),2])     )
  year<- c(1988.33,1990,1991.66,1993.33,1995,1996.66,1998.33,2000,2001.66,2003.33,2005,2006.66,2008.33,2010,2011.66)

  for_plot1 <- data.frame(Hypertension1, Obesity1, Hypercholesterolemia1, smoke1, Diabetes1)

  r3<-rep.row(for_plot1[1,],3)
  r6<-rep.row(for_plot1[2,],3)
  r9<-rep.row(for_plot1[3,],3)
  r12<-rep.row(for_plot1[4,],3)
  r15<-rep.row(for_plot1[5,],3)
  r3<- as.data.frame(r3)
  r6<- as.data.frame(r6)
  r9<- as.data.frame(r9)
  r12<- as.data.frame(r12)
  r15<- as.data.frame(r15)
  for_plot1<- rbind(r3,r6,r9,r12,r15)
  for_plot1<- cbind(for_plot1,year)
  colnames(for_plot1)<- c("Hypertension1", "Obesity1", "Hypercholesterolemia1", "Smoke1", "Diabetes1", "year1")

  for_plot1$Hypertension1<- (as.numeric(for_plot1$Hypertension1))
  for_plot1$Obesity1<- (as.numeric(for_plot1$Obesity1))
  for_plot1$Hypercholesterolemia1<- (as.numeric(for_plot1$Hypercholesterolemia1))
  for_plot1$Smoke1<- (as.numeric(for_plot1$Smoke1))
  for_plot1$Diabetes1<- (as.numeric(for_plot1$Diabetes1))
  for_plot1$year1<- (as.numeric(for_plot1$year1))
  colnames(for_plot1)<- c("Hypertension1", "Obesity1", "Hypercholesterolemia1", "Smoke1", "Diabetes1", "year1")

  #summed

  ## for white spaces


  smoke<-c(sum(a[1:(which(a$type == "Smoking")),2])+(which(a$type == "Smoking")*.03),sum(a1[1:(which(a1$type == "Smoking")),2])+(which(a1$type == "Smoking")*.03) ,sum(a2[1:(which(a2$type == "Smoking")),2])+(which(a2$type == "Smoking")*.03),sum(a3[1:(which(a3$type == "Smoking")),2]) +(which(a3$type == "Smoking")*.03),sum(a4[1:(which(a4$type == "Smoking")),2]) +(which(a4$type == "Smoking")*.03)    )

  Hypercholesterolemia<-c(sum(a[1:(which(a$type == "Hypercholesterolemia")),2])+(which(a$type == "Hypercholesterolemia")*.03),sum(a1[1:(which(a1$type == "Hypercholesterolemia")),2]) +(which(a1$type == "Hypercholesterolemia")*.03),sum(a2[1:(which(a2$type == "Hypercholesterolemia")),2])+(which(a2$type == "Hypercholesterolemia")*.03),sum(a3[1:(which(a3$type == "Hypercholesterolemia")),2]) +(which(a3$type == "Hypercholesterolemia")*.03),sum(a4[1:(which(a4$type == "Hypercholesterolemia")),2])+(which(a4$type == "Hypercholesterolemia")*.03))

  Obesity<-c(sum(a[1:(which(a$type == "Obesity")),2])+(which(a$type == "Obesity")*.03),sum(a1[1:(which(a1$type == "Obesity")),2])+(which(a1$type == "Obesity")*.03) ,sum(a2[1:(which(a2$type == "Obesity")),2])+(which(a2$type == "Obesity")*.03),sum(a3[1:(which(a3$type == "Obesity")),2]) +(which(a3$type == "Obesity")*.03),sum(a4[1:(which(a4$type == "Obesity")),2])+(which(a4$type == "Obesity")*.03)     )

  Diabetes<-c(sum(a[1:(which(a$type == "Diabetes")),2])+(which(a$type == "Diabetes")*.03),sum(a1[1:(which(a1$type == "Diabetes")),2])+(which(a1$type == "Diabetes")*.03) ,sum(a2[1:(which(a2$type == "Diabetes")),2])+(which(a2$type == "Diabetes")*.03),sum(a3[1:(which(a3$type == "Diabetes")),2]) +(which(a3$type == "Diabetes")*.03),sum(a4[1:(which(a4$type == "Diabetes")),2])+(which(a4$type == "Diabetes")*.03)     )

  Hypertension<-c(sum(a[1:(which(a$type == "Hypertension")),2])+(which(a$type == "Hypertension")*.03),sum(a1[1:(which(a1$type == "Hypertension")),2])+(which(a1$type == "Hypertension")*.03) ,sum(a2[1:(which(a2$type == "Hypertension")),2])+(which(a2$type == "Hypertension")*.03),sum(a3[1:(which(a3$type == "Hypertension")),2])+(which(a3$type == "Hypertension")*.03) ,sum(a4[1:(which(a4$type == "Hypertension")),2])+(which(a4$type == "Hypertension")*.03)     )

  year<- c(1988.33,1990,1991.66,1993.33,1995,1996.66,1998.33,2000,2001.66,2003.33,2005,2006.66,2008.33,2010,2011.66)

  for_plot <- data.frame(Hypertension, Obesity, Hypercholesterolemia, smoke, Diabetes)

  r3<-rep.row(for_plot[1,],3)
  r6<-rep.row(for_plot[2,],3)
  r9<-rep.row(for_plot[3,],3)
  r12<-rep.row(for_plot[4,],3)
  r15<-rep.row(for_plot[5,],3)
  r3<- as.data.frame(r3)
  r6<- as.data.frame(r6)
  r9<- as.data.frame(r9)
  r12<- as.data.frame(r12)
  r15<- as.data.frame(r15)
  for_plot<- rbind(r3,r6,r9,r12,r15)
  for_plot<- cbind(for_plot,year)
  colnames(for_plot)<- c("Hypertension", "Obesity", "Hypercholesterolemia", "Smoke", "Diabetes", "year")

  for_plot$Hypertension<- (as.numeric(for_plot$Hypertension))
  for_plot$Obesity<- (as.numeric(for_plot$Obesity))
  for_plot$Hypercholesterolemia<- (as.numeric(for_plot$Hypercholesterolemia))
  for_plot$Smoke<- (as.numeric(for_plot$Smoke))
  for_plot$Diabetes<- (as.numeric(for_plot$Diabetes))
  for_plot$year<- (as.numeric(for_plot$year))
  colnames(for_plot)<- c("Hypertension", "Obesity", "Hypercholesterolemia", "Smoke", "Diabetes", "year")

  for_plot[for_plot<.01]<-.0049
  for_plot1[for_plot1<.01]<-.0049
  #creating dataframe for labels

  for_plotl<- for_plot1
  for_plotl <- format(round(for_plot1,2),nsmall = 2 )

  for_plotl[1,]<- NA
  for_plotl[3,]<- NA
  for_plotl[4,]<- NA
  for_plotl[6,]<- NA
  for_plotl[7,]<- NA
  for_plotl[9,]<- NA
  for_plotl[10,]<- NA
  for_plotl[12,]<- NA
  for_plotl[13,]<- NA
  for_plotl[15,]<- NA

  suppressWarnings(ggplot(for_plot, aes(), ylim(0,2)) +
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Hypertension-(for_plot1$Hypertension1*1), ymax= for_plot$Hypertension+ (for_plot1$Hypertension1*0), fill = "Hypertension"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Obesity-(for_plot1$Obesity1*1), ymax= for_plot$Obesity+(for_plot1$Obesity1*0), fill = "Obesity" ))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Smoke-(for_plot1$Smoke1*1), ymax= for_plot$Smoke+(for_plot1$Smoke1*0), fill = "Smoke"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Hypercholesterolemia-(for_plot1$Hypercholesterolemia1*1), ymax= for_plot$Hypercholesterolemia+(for_plot1$Hypercholesterolemia1*0), fill = "Hypercholesterolemia"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Diabetes-(for_plot1$Diabetes1*1), ymax= for_plot$Diabetes+(for_plot1$Diabetes1*0), fill = "Diabetes"))+
                     #changing styling
                     theme(panel.grid.major = element_blank(),legend.position = "bottom",axis.ticks.x=element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.text.x = element_text(face="bold"),axis.title.y=element_blank(),
                           axis.text.y=element_blank(),
                           axis.ticks.y=element_blank(),axis.title.x = element_text(color="black", size=14, face="bold") )+
                     scale_x_continuous(position = 'top') + labs(x = plot_title_in_quotes)+
                     #adding inside labels
                     geom_label(label =  for_plotl$Hypertension1, y = (for_plot$Hypertension-(for_plot1$Hypertension1*.5)), x = for_plot1$year1, color = "white", fill = "darkseagreen", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Hypercholesterolemia1, y = (for_plot$Hypercholesterolemia-(for_plot1$Hypercholesterolemia1*.5)), x = for_plot1$year1, color = "white", fill = "tan2", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Smoke1, y = (for_plot$Smoke-(for_plot1$Smoke1*.5)), x = for_plot1$year1, color = "white", fill = "lightskyblue", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Diabetes1, y = (for_plot$Diabetes-(for_plot1$Diabetes1*.5)), x = for_plot1$year1, color = "white", fill = "tomato2", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Obesity1, y = (for_plot$Obesity-(for_plot1$Obesity1*.5)), x = for_plot1$year1, color = "white", fill = "steelblue4", colour = "white", label.size = 0, na.rm = TRUE)+
                     #changing fill colors
                     scale_fill_manual(values=c("tomato2", "tan2", "darkseagreen", "steelblue4","lightskyblue"), name=""))+
    #adding white lines
    geom_vline(xintercept = c(1988.33,1991.66,1993.33,1996.66,1998.33,2001.66,2003.33,2006.66,2008.33,2011.66), color = "white", size = 1)

}






risk_factors_for_stroke_sankey_diagram_with_vertical_space_only <- function(data_to_be_plotted, plot_title_in_quotes){
  data_to_be_plotted<- as.data.frame(star)
  colnames(star)<- c("type", "1990", "1995", "2000", "2005", "2010")
  a <- data.frame("year" = 1990,"risk"=star$`1990`,"type" = star$type)
  a<- a[order(a$risk),]
  a1 <- data.frame("year" = 1995,"risk"=star$`1995`,"type" = star$type)
  a1<- a1[order(a1$risk),]
  a2 <- data.frame("year" = 2000,"risk"=star$`2000`,"type" = star$type)
  a2<- a2[order(a2$risk),]
  a3 <- data.frame("year" = 1995,"risk"=star$`2005`,"type" = star$type)
  a3<- a3[order(a3$risk),]
  a4 <- data.frame("year" = 1995,"risk"=star$`2010`,"type" = star$type)
  a4<- a4[order(a4$risk),]

  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }

  #not summed, used for upper and lowe limits

  smoke1<-c((a[(which(a$type == "Smoking")),2]),(a1[(which(a1$type == "Smoking")),2]) ,(a2[(which(a2$type == "Smoking")),2]),(a3[(which(a3$type == "Smoking")),2]) ,(a4[(which(a4$type == "Smoking")),2])     )

  Hypercholesterolemia1<-c((a[(which(a$type == "Hypercholesterolemia")),2]),(a1[(which(a1$type == "Hypercholesterolemia")),2]) ,(a2[(which(a2$type == "Hypercholesterolemia")),2]),(a3[(which(a3$type == "Hypercholesterolemia")),2]) ,(a4[(which(a4$type == "Hypercholesterolemia")),2]))

  Obesity1<-c((a[(which(a$type == "Obesity")),2]),(a1[(which(a1$type == "Obesity")),2]) ,(a2[(which(a2$type == "Obesity")),2]),(a3[(which(a3$type == "Obesity")),2]) ,(a4[(which(a4$type == "Obesity")),2])     )

  Diabetes1<-c((a[(which(a$type == "Diabetes")),2]),(a1[(which(a1$type == "Diabetes")),2]) ,(a2[(which(a2$type == "Diabetes")),2]),(a3[(which(a3$type == "Diabetes")),2]) ,(a4[(which(a4$type == "Diabetes")),2])     )

  Hypertension1<-c((a[(which(a$type == "Hypertension")),2]),(a1[(which(a1$type == "Hypertension")),2]) ,(a2[(which(a2$type == "Hypertension")),2]),(a3[(which(a3$type == "Hypertension")),2]) ,(a4[(which(a4$type == "Hypertension")),2])     )
  year<- c(1988.33,1990,1991.66,1993.33,1995,1996.66,1998.33,2000,2001.66,2003.33,2005,2006.66,2008.33,2010,2011.66)

  for_plot1 <- data.frame(Hypertension1, Obesity1, Hypercholesterolemia1, smoke1, Diabetes1)

  r3<-rep.row(for_plot1[1,],3)
  r6<-rep.row(for_plot1[2,],3)
  r9<-rep.row(for_plot1[3,],3)
  r12<-rep.row(for_plot1[4,],3)
  r15<-rep.row(for_plot1[5,],3)
  r3<- as.data.frame(r3)
  r6<- as.data.frame(r6)
  r9<- as.data.frame(r9)
  r12<- as.data.frame(r12)
  r15<- as.data.frame(r15)
  for_plot1<- rbind(r3,r6,r9,r12,r15)
  for_plot1<- cbind(for_plot1,year)
  colnames(for_plot1)<- c("Hypertension1", "Obesity1", "Hypercholesterolemia1", "Smoke1", "Diabetes1", "year1")

  for_plot1$Hypertension1<- (as.numeric(for_plot1$Hypertension1))
  for_plot1$Obesity1<- (as.numeric(for_plot1$Obesity1))
  for_plot1$Hypercholesterolemia1<- (as.numeric(for_plot1$Hypercholesterolemia1))
  for_plot1$Smoke1<- (as.numeric(for_plot1$Smoke1))
  for_plot1$Diabetes1<- (as.numeric(for_plot1$Diabetes1))
  for_plot1$year1<- (as.numeric(for_plot1$year1))
  colnames(for_plot1)<- c("Hypertension1", "Obesity1", "Hypercholesterolemia1", "Smoke1", "Diabetes1", "year1")

  #summed

  ## for white spaces


  #for no spaces


  smoke<-c(sum(a[1:(which(a$type == "Smoking")),2]),sum(a1[1:(which(a1$type == "Smoking")),2]) ,sum(a2[1:(which(a2$type == "Smoking")),2]),sum(a3[1:(which(a3$type == "Smoking")),2]) ,sum(a4[1:(which(a4$type == "Smoking")),2])     )

  Hypercholesterolemia<-c(sum(a[1:(which(a$type == "Hypercholesterolemia")),2]),sum(a1[1:(which(a1$type == "Hypercholesterolemia")),2]) ,sum(a2[1:(which(a2$type == "Hypercholesterolemia")),2]),sum(a3[1:(which(a3$type == "Hypercholesterolemia")),2]) ,sum(a4[1:(which(a4$type == "Hypercholesterolemia")),2]))

  Obesity<-c(sum(a[1:(which(a$type == "Obesity")),2]),sum(a1[1:(which(a1$type == "Obesity")),2]) ,sum(a2[1:(which(a2$type == "Obesity")),2]),sum(a3[1:(which(a3$type == "Obesity")),2]) ,sum(a4[1:(which(a4$type == "Obesity")),2])     )

  Diabetes<-c(sum(a[1:(which(a$type == "Diabetes")),2]),sum(a1[1:(which(a1$type == "Diabetes")),2]) ,sum(a2[1:(which(a2$type == "Diabetes")),2]),sum(a3[1:(which(a3$type == "Diabetes")),2]) ,sum(a4[1:(which(a4$type == "Diabetes")),2])     )

  Hypertension<-c(sum(a[1:(which(a$type == "Hypertension")),2]),sum(a1[1:(which(a1$type == "Hypertension")),2]) ,sum(a2[1:(which(a2$type == "Hypertension")),2]),sum(a3[1:(which(a3$type == "Hypertension")),2]) ,sum(a4[1:(which(a4$type == "Hypertension")),2])     )

  year<- c(1988.33,1990,1991.66,1993.33,1995,1996.66,1998.33,2000,2001.66,2003.33,2005,2006.66,2008.33,2010,2011.66)

  for_plot <- data.frame(Hypertension, Obesity, Hypercholesterolemia, smoke, Diabetes)

  r3<-rep.row(for_plot[1,],3)
  r6<-rep.row(for_plot[2,],3)
  r9<-rep.row(for_plot[3,],3)
  r12<-rep.row(for_plot[4,],3)
  r15<-rep.row(for_plot[5,],3)
  r3<- as.data.frame(r3)
  r6<- as.data.frame(r6)
  r9<- as.data.frame(r9)
  r12<- as.data.frame(r12)
  r15<- as.data.frame(r15)
  for_plot<- rbind(r3,r6,r9,r12,r15)
  for_plot<- cbind(for_plot,year)
  colnames(for_plot)<- c("Hypertension", "Obesity", "Hypercholesterolemia", "Smoke", "Diabetes", "year")

  for_plot$Hypertension<- (as.numeric(for_plot$Hypertension))
  for_plot$Obesity<- (as.numeric(for_plot$Obesity))
  for_plot$Hypercholesterolemia<- (as.numeric(for_plot$Hypercholesterolemia))
  for_plot$Smoke<- (as.numeric(for_plot$Smoke))
  for_plot$Diabetes<- (as.numeric(for_plot$Diabetes))
  for_plot$year<- (as.numeric(for_plot$year))
  colnames(for_plot)<- c("Hypertension", "Obesity", "Hypercholesterolemia", "Smoke", "Diabetes", "year")

  for_plot[for_plot<.01]<-.0049
  for_plot1[for_plot1<.01]<-.0049
  #creating dataframe for labels

  for_plotl<- for_plot1
  for_plotl <- format(round(for_plot1,2),nsmall = 2 )

  for_plotl[1,]<- NA
  for_plotl[3,]<- NA
  for_plotl[4,]<- NA
  for_plotl[6,]<- NA
  for_plotl[7,]<- NA
  for_plotl[9,]<- NA
  for_plotl[10,]<- NA
  for_plotl[12,]<- NA
  for_plotl[13,]<- NA
  for_plotl[15,]<- NA

  suppressWarnings(ggplot(for_plot, aes(), ylim(0,2)) +
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Hypertension-(for_plot1$Hypertension1*1), ymax= for_plot$Hypertension+ (for_plot1$Hypertension1*0), fill = "Hypertension"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Obesity-(for_plot1$Obesity1*1), ymax= for_plot$Obesity+(for_plot1$Obesity1*0), fill = "Obesity" ))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Smoke-(for_plot1$Smoke1*1), ymax= for_plot$Smoke+(for_plot1$Smoke1*0), fill = "Smoke"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Hypercholesterolemia-(for_plot1$Hypercholesterolemia1*1), ymax= for_plot$Hypercholesterolemia+(for_plot1$Hypercholesterolemia1*0), fill = "Hypercholesterolemia"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Diabetes-(for_plot1$Diabetes1*1), ymax= for_plot$Diabetes+(for_plot1$Diabetes1*0), fill = "Diabetes"))+
                     #changing styling
                     theme(panel.grid.major = element_blank(),legend.position = "bottom",axis.ticks.x=element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.text.x = element_text(face="bold"),axis.title.y=element_blank(),
                           axis.text.y=element_blank(),
                           axis.ticks.y=element_blank(),axis.title.x = element_text(color="black", size=14, face="bold") )+
                     scale_x_continuous(position = 'top') + labs(x = plot_title_in_quotes)+
                     #adding inside labels
                     geom_label(label =  for_plotl$Hypertension1, y = (for_plot$Hypertension-(for_plot1$Hypertension1*.5)), x = for_plot1$year1, color = "white", fill = "darkseagreen", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Hypercholesterolemia1, y = (for_plot$Hypercholesterolemia-(for_plot1$Hypercholesterolemia1*.5)), x = for_plot1$year1, color = "white", fill = "tan2", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Smoke1, y = (for_plot$Smoke-(for_plot1$Smoke1*.5)), x = for_plot1$year1, color = "white", fill = "lightskyblue", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Diabetes1, y = (for_plot$Diabetes-(for_plot1$Diabetes1*.5)), x = for_plot1$year1, color = "white", fill = "tomato2", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Obesity1, y = (for_plot$Obesity-(for_plot1$Obesity1*.5)), x = for_plot1$year1, color = "white", fill = "steelblue4", colour = "white", label.size = 0, na.rm = TRUE)+
                     #changing fill colors
                     scale_fill_manual(values=c("tomato2", "tan2", "darkseagreen", "steelblue4","lightskyblue"), name=""))+
    #adding white lines
    geom_vline(xintercept = c(1988.33,1991.66,1993.33,1996.66,1998.33,2001.66,2003.33,2006.66,2008.33,2011.66), color = "white", size = 1)

}




risk_factors_for_stroke_sankey_diagram_with_horizontal_space_only<- function(data_to_be_plotted, plot_title_in_quotes){
  data_to_be_plotted<- as.data.frame(star)
  colnames(star)<- c("type", "1990", "1995", "2000", "2005", "2010")

  a <- data.frame("year" = 1990,"risk"=star$`1990`,"type" = star$type)
  a<- a[order(a$risk),]
  a1 <- data.frame("year" = 1995,"risk"=star$`1995`,"type" = star$type)
  a1<- a1[order(a1$risk),]
  a2 <- data.frame("year" = 2000,"risk"=star$`2000`,"type" = star$type)
  a2<- a2[order(a2$risk),]
  a3 <- data.frame("year" = 1995,"risk"=star$`2005`,"type" = star$type)
  a3<- a3[order(a3$risk),]
  a4 <- data.frame("year" = 1995,"risk"=star$`2010`,"type" = star$type)
  a4<- a4[order(a4$risk),]

  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }

  #not summed, used for upper and lowe limits

  smoke1<-c((a[(which(a$type == "Smoking")),2]),(a1[(which(a1$type == "Smoking")),2]) ,(a2[(which(a2$type == "Smoking")),2]),(a3[(which(a3$type == "Smoking")),2]) ,(a4[(which(a4$type == "Smoking")),2])     )

  Hypercholesterolemia1<-c((a[(which(a$type == "Hypercholesterolemia")),2]),(a1[(which(a1$type == "Hypercholesterolemia")),2]) ,(a2[(which(a2$type == "Hypercholesterolemia")),2]),(a3[(which(a3$type == "Hypercholesterolemia")),2]) ,(a4[(which(a4$type == "Hypercholesterolemia")),2]))

  Obesity1<-c((a[(which(a$type == "Obesity")),2]),(a1[(which(a1$type == "Obesity")),2]) ,(a2[(which(a2$type == "Obesity")),2]),(a3[(which(a3$type == "Obesity")),2]) ,(a4[(which(a4$type == "Obesity")),2])     )

  Diabetes1<-c((a[(which(a$type == "Diabetes")),2]),(a1[(which(a1$type == "Diabetes")),2]) ,(a2[(which(a2$type == "Diabetes")),2]),(a3[(which(a3$type == "Diabetes")),2]) ,(a4[(which(a4$type == "Diabetes")),2])     )

  Hypertension1<-c((a[(which(a$type == "Hypertension")),2]),(a1[(which(a1$type == "Hypertension")),2]) ,(a2[(which(a2$type == "Hypertension")),2]),(a3[(which(a3$type == "Hypertension")),2]) ,(a4[(which(a4$type == "Hypertension")),2])     )
  year<- c(1988.33,1990,1991.66,1993.33,1995,1996.66,1998.33,2000,2001.66,2003.33,2005,2006.66,2008.33,2010,2011.66)

  for_plot1 <- data.frame(Hypertension1, Obesity1, Hypercholesterolemia1, smoke1, Diabetes1)

  r3<-rep.row(for_plot1[1,],3)
  r6<-rep.row(for_plot1[2,],3)
  r9<-rep.row(for_plot1[3,],3)
  r12<-rep.row(for_plot1[4,],3)
  r15<-rep.row(for_plot1[5,],3)
  r3<- as.data.frame(r3)
  r6<- as.data.frame(r6)
  r9<- as.data.frame(r9)
  r12<- as.data.frame(r12)
  r15<- as.data.frame(r15)
  for_plot1<- rbind(r3,r6,r9,r12,r15)
  for_plot1<- cbind(for_plot1,year)
  colnames(for_plot1)<- c("Hypertension1", "Obesity1", "Hypercholesterolemia1", "Smoke1", "Diabetes1", "year1")

  for_plot1$Hypertension1<- (as.numeric(for_plot1$Hypertension1))
  for_plot1$Obesity1<- (as.numeric(for_plot1$Obesity1))
  for_plot1$Hypercholesterolemia1<- (as.numeric(for_plot1$Hypercholesterolemia1))
  for_plot1$Smoke1<- (as.numeric(for_plot1$Smoke1))
  for_plot1$Diabetes1<- (as.numeric(for_plot1$Diabetes1))
  for_plot1$year1<- (as.numeric(for_plot1$year1))
  colnames(for_plot1)<- c("Hypertension1", "Obesity1", "Hypercholesterolemia1", "Smoke1", "Diabetes1", "year1")

  #summed

  ## for white spaces


  smoke<-c(sum(a[1:(which(a$type == "Smoking")),2])+(which(a$type == "Smoking")*.03),sum(a1[1:(which(a1$type == "Smoking")),2])+(which(a1$type == "Smoking")*.03) ,sum(a2[1:(which(a2$type == "Smoking")),2])+(which(a2$type == "Smoking")*.03),sum(a3[1:(which(a3$type == "Smoking")),2]) +(which(a3$type == "Smoking")*.03),sum(a4[1:(which(a4$type == "Smoking")),2]) +(which(a4$type == "Smoking")*.03)    )

  Hypercholesterolemia<-c(sum(a[1:(which(a$type == "Hypercholesterolemia")),2])+(which(a$type == "Hypercholesterolemia")*.03),sum(a1[1:(which(a1$type == "Hypercholesterolemia")),2]) +(which(a1$type == "Hypercholesterolemia")*.03),sum(a2[1:(which(a2$type == "Hypercholesterolemia")),2])+(which(a2$type == "Hypercholesterolemia")*.03),sum(a3[1:(which(a3$type == "Hypercholesterolemia")),2]) +(which(a3$type == "Hypercholesterolemia")*.03),sum(a4[1:(which(a4$type == "Hypercholesterolemia")),2])+(which(a4$type == "Hypercholesterolemia")*.03))

  Obesity<-c(sum(a[1:(which(a$type == "Obesity")),2])+(which(a$type == "Obesity")*.03),sum(a1[1:(which(a1$type == "Obesity")),2])+(which(a1$type == "Obesity")*.03) ,sum(a2[1:(which(a2$type == "Obesity")),2])+(which(a2$type == "Obesity")*.03),sum(a3[1:(which(a3$type == "Obesity")),2]) +(which(a3$type == "Obesity")*.03),sum(a4[1:(which(a4$type == "Obesity")),2])+(which(a4$type == "Obesity")*.03)     )

  Diabetes<-c(sum(a[1:(which(a$type == "Diabetes")),2])+(which(a$type == "Diabetes")*.03),sum(a1[1:(which(a1$type == "Diabetes")),2])+(which(a1$type == "Diabetes")*.03) ,sum(a2[1:(which(a2$type == "Diabetes")),2])+(which(a2$type == "Diabetes")*.03),sum(a3[1:(which(a3$type == "Diabetes")),2]) +(which(a3$type == "Diabetes")*.03),sum(a4[1:(which(a4$type == "Diabetes")),2])+(which(a4$type == "Diabetes")*.03)     )

  Hypertension<-c(sum(a[1:(which(a$type == "Hypertension")),2])+(which(a$type == "Hypertension")*.03),sum(a1[1:(which(a1$type == "Hypertension")),2])+(which(a1$type == "Hypertension")*.03) ,sum(a2[1:(which(a2$type == "Hypertension")),2])+(which(a2$type == "Hypertension")*.03),sum(a3[1:(which(a3$type == "Hypertension")),2])+(which(a3$type == "Hypertension")*.03) ,sum(a4[1:(which(a4$type == "Hypertension")),2])+(which(a4$type == "Hypertension")*.03)     )

  year<- c(1988.33,1990,1991.66,1993.33,1995,1996.66,1998.33,2000,2001.66,2003.33,2005,2006.66,2008.33,2010,2011.66)

  for_plot <- data.frame(Hypertension, Obesity, Hypercholesterolemia, smoke, Diabetes)

  r3<-rep.row(for_plot[1,],3)
  r6<-rep.row(for_plot[2,],3)
  r9<-rep.row(for_plot[3,],3)
  r12<-rep.row(for_plot[4,],3)
  r15<-rep.row(for_plot[5,],3)
  r3<- as.data.frame(r3)
  r6<- as.data.frame(r6)
  r9<- as.data.frame(r9)
  r12<- as.data.frame(r12)
  r15<- as.data.frame(r15)
  for_plot<- rbind(r3,r6,r9,r12,r15)
  for_plot<- cbind(for_plot,year)
  colnames(for_plot)<- c("Hypertension", "Obesity", "Hypercholesterolemia", "Smoke", "Diabetes", "year")

  for_plot$Hypertension<- (as.numeric(for_plot$Hypertension))
  for_plot$Obesity<- (as.numeric(for_plot$Obesity))
  for_plot$Hypercholesterolemia<- (as.numeric(for_plot$Hypercholesterolemia))
  for_plot$Smoke<- (as.numeric(for_plot$Smoke))
  for_plot$Diabetes<- (as.numeric(for_plot$Diabetes))
  for_plot$year<- (as.numeric(for_plot$year))
  colnames(for_plot)<- c("Hypertension", "Obesity", "Hypercholesterolemia", "Smoke", "Diabetes", "year")

  for_plot[for_plot<.01]<-.0049
  for_plot1[for_plot1<.01]<-.0049
  #creating dataframe for labels

  for_plotl<- for_plot1
  for_plotl <- format(round(for_plot1,2),nsmall = 2 )

  for_plotl[1,]<- NA
  for_plotl[3,]<- NA
  for_plotl[4,]<- NA
  for_plotl[6,]<- NA
  for_plotl[7,]<- NA
  for_plotl[9,]<- NA
  for_plotl[10,]<- NA
  for_plotl[12,]<- NA
  for_plotl[13,]<- NA
  for_plotl[15,]<- NA

  suppressWarnings(ggplot(for_plot, aes(), ylim(0,2)) +
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Hypertension-(for_plot1$Hypertension1*1), ymax= for_plot$Hypertension+ (for_plot1$Hypertension1*0), fill = "Hypertension"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Obesity-(for_plot1$Obesity1*1), ymax= for_plot$Obesity+(for_plot1$Obesity1*0), fill = "Obesity" ))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Smoke-(for_plot1$Smoke1*1), ymax= for_plot$Smoke+(for_plot1$Smoke1*0), fill = "Smoke"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Hypercholesterolemia-(for_plot1$Hypercholesterolemia1*1), ymax= for_plot$Hypercholesterolemia+(for_plot1$Hypercholesterolemia1*0), fill = "Hypercholesterolemia"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Diabetes-(for_plot1$Diabetes1*1), ymax= for_plot$Diabetes+(for_plot1$Diabetes1*0), fill = "Diabetes"))+
                     #changing styling
                     theme(panel.grid.major = element_blank(),legend.position = "bottom",axis.ticks.x=element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.text.x = element_text(face="bold"),axis.title.y=element_blank(),
                           axis.text.y=element_blank(),
                           axis.ticks.y=element_blank(),axis.title.x = element_text(color="black", size=14, face="bold") )+
                     scale_x_continuous(position = 'top') + labs(x = plot_title_in_quotes)+
                     #adding inside labels
                     geom_label(label =  for_plotl$Hypertension1, y = (for_plot$Hypertension-(for_plot1$Hypertension1*.5)), x = for_plot1$year1, color = "white", fill = "darkseagreen", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Hypercholesterolemia1, y = (for_plot$Hypercholesterolemia-(for_plot1$Hypercholesterolemia1*.5)), x = for_plot1$year1, color = "white", fill = "tan2", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Smoke1, y = (for_plot$Smoke-(for_plot1$Smoke1*.5)), x = for_plot1$year1, color = "white", fill = "lightskyblue", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Diabetes1, y = (for_plot$Diabetes-(for_plot1$Diabetes1*.5)), x = for_plot1$year1, color = "white", fill = "tomato2", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Obesity1, y = (for_plot$Obesity-(for_plot1$Obesity1*.5)), x = for_plot1$year1, color = "white", fill = "steelblue4", colour = "white", label.size = 0, na.rm = TRUE)+
                     #changing fill colors
                     scale_fill_manual(values=c("tomato2", "tan2", "darkseagreen", "steelblue4","lightskyblue"), name=""))


}






risk_factors_for_stroke_sankey_diagram_with_no_spaces <- function(data_to_be_plotted, plot_title_in_quotes){
  data_to_be_plotted<- as.data.frame(star)
  colnames(star)<- c("type", "1990", "1995", "2000", "2005", "2010")
  a <- data.frame("year" = 1990,"risk"=star$`1990`,"type" = star$type)
  a<- a[order(a$risk),]
  a1 <- data.frame("year" = 1995,"risk"=star$`1995`,"type" = star$type)
  a1<- a1[order(a1$risk),]
  a2 <- data.frame("year" = 2000,"risk"=star$`2000`,"type" = star$type)
  a2<- a2[order(a2$risk),]
  a3 <- data.frame("year" = 1995,"risk"=star$`2005`,"type" = star$type)
  a3<- a3[order(a3$risk),]
  a4 <- data.frame("year" = 1995,"risk"=star$`2010`,"type" = star$type)
  a4<- a4[order(a4$risk),]

  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }

  #not summed, used for upper and lowe limits

  smoke1<-c((a[(which(a$type == "Smoking")),2]),(a1[(which(a1$type == "Smoking")),2]) ,(a2[(which(a2$type == "Smoking")),2]),(a3[(which(a3$type == "Smoking")),2]) ,(a4[(which(a4$type == "Smoking")),2])     )

  Hypercholesterolemia1<-c((a[(which(a$type == "Hypercholesterolemia")),2]),(a1[(which(a1$type == "Hypercholesterolemia")),2]) ,(a2[(which(a2$type == "Hypercholesterolemia")),2]),(a3[(which(a3$type == "Hypercholesterolemia")),2]) ,(a4[(which(a4$type == "Hypercholesterolemia")),2]))

  Obesity1<-c((a[(which(a$type == "Obesity")),2]),(a1[(which(a1$type == "Obesity")),2]) ,(a2[(which(a2$type == "Obesity")),2]),(a3[(which(a3$type == "Obesity")),2]) ,(a4[(which(a4$type == "Obesity")),2])     )

  Diabetes1<-c((a[(which(a$type == "Diabetes")),2]),(a1[(which(a1$type == "Diabetes")),2]) ,(a2[(which(a2$type == "Diabetes")),2]),(a3[(which(a3$type == "Diabetes")),2]) ,(a4[(which(a4$type == "Diabetes")),2])     )

  Hypertension1<-c((a[(which(a$type == "Hypertension")),2]),(a1[(which(a1$type == "Hypertension")),2]) ,(a2[(which(a2$type == "Hypertension")),2]),(a3[(which(a3$type == "Hypertension")),2]) ,(a4[(which(a4$type == "Hypertension")),2])     )
  year<- c(1988.33,1990,1991.66,1993.33,1995,1996.66,1998.33,2000,2001.66,2003.33,2005,2006.66,2008.33,2010,2011.66)

  for_plot1 <- data.frame(Hypertension1, Obesity1, Hypercholesterolemia1, smoke1, Diabetes1)

  r3<-rep.row(for_plot1[1,],3)
  r6<-rep.row(for_plot1[2,],3)
  r9<-rep.row(for_plot1[3,],3)
  r12<-rep.row(for_plot1[4,],3)
  r15<-rep.row(for_plot1[5,],3)
  r3<- as.data.frame(r3)
  r6<- as.data.frame(r6)
  r9<- as.data.frame(r9)
  r12<- as.data.frame(r12)
  r15<- as.data.frame(r15)
  for_plot1<- rbind(r3,r6,r9,r12,r15)
  for_plot1<- cbind(for_plot1,year)
  colnames(for_plot1)<- c("Hypertension1", "Obesity1", "Hypercholesterolemia1", "Smoke1", "Diabetes1", "year1")

  for_plot1$Hypertension1<- (as.numeric(for_plot1$Hypertension1))
  for_plot1$Obesity1<- (as.numeric(for_plot1$Obesity1))
  for_plot1$Hypercholesterolemia1<- (as.numeric(for_plot1$Hypercholesterolemia1))
  for_plot1$Smoke1<- (as.numeric(for_plot1$Smoke1))
  for_plot1$Diabetes1<- (as.numeric(for_plot1$Diabetes1))
  for_plot1$year1<- (as.numeric(for_plot1$year1))
  colnames(for_plot1)<- c("Hypertension1", "Obesity1", "Hypercholesterolemia1", "Smoke1", "Diabetes1", "year1")

  #summed

  ## for white spaces


  #for no spaces


  smoke<-c(sum(a[1:(which(a$type == "Smoking")),2]),sum(a1[1:(which(a1$type == "Smoking")),2]) ,sum(a2[1:(which(a2$type == "Smoking")),2]),sum(a3[1:(which(a3$type == "Smoking")),2]) ,sum(a4[1:(which(a4$type == "Smoking")),2])     )

  Hypercholesterolemia<-c(sum(a[1:(which(a$type == "Hypercholesterolemia")),2]),sum(a1[1:(which(a1$type == "Hypercholesterolemia")),2]) ,sum(a2[1:(which(a2$type == "Hypercholesterolemia")),2]),sum(a3[1:(which(a3$type == "Hypercholesterolemia")),2]) ,sum(a4[1:(which(a4$type == "Hypercholesterolemia")),2]))

  Obesity<-c(sum(a[1:(which(a$type == "Obesity")),2]),sum(a1[1:(which(a1$type == "Obesity")),2]) ,sum(a2[1:(which(a2$type == "Obesity")),2]),sum(a3[1:(which(a3$type == "Obesity")),2]) ,sum(a4[1:(which(a4$type == "Obesity")),2])     )

  Diabetes<-c(sum(a[1:(which(a$type == "Diabetes")),2]),sum(a1[1:(which(a1$type == "Diabetes")),2]) ,sum(a2[1:(which(a2$type == "Diabetes")),2]),sum(a3[1:(which(a3$type == "Diabetes")),2]) ,sum(a4[1:(which(a4$type == "Diabetes")),2])     )

  Hypertension<-c(sum(a[1:(which(a$type == "Hypertension")),2]),sum(a1[1:(which(a1$type == "Hypertension")),2]) ,sum(a2[1:(which(a2$type == "Hypertension")),2]),sum(a3[1:(which(a3$type == "Hypertension")),2]) ,sum(a4[1:(which(a4$type == "Hypertension")),2])     )

  year<- c(1988.33,1990,1991.66,1993.33,1995,1996.66,1998.33,2000,2001.66,2003.33,2005,2006.66,2008.33,2010,2011.66)

  for_plot <- data.frame(Hypertension, Obesity, Hypercholesterolemia, smoke, Diabetes)

  r3<-rep.row(for_plot[1,],3)
  r6<-rep.row(for_plot[2,],3)
  r9<-rep.row(for_plot[3,],3)
  r12<-rep.row(for_plot[4,],3)
  r15<-rep.row(for_plot[5,],3)
  r3<- as.data.frame(r3)
  r6<- as.data.frame(r6)
  r9<- as.data.frame(r9)
  r12<- as.data.frame(r12)
  r15<- as.data.frame(r15)
  for_plot<- rbind(r3,r6,r9,r12,r15)
  for_plot<- cbind(for_plot,year)
  colnames(for_plot)<- c("Hypertension", "Obesity", "Hypercholesterolemia", "Smoke", "Diabetes", "year")

  for_plot$Hypertension<- (as.numeric(for_plot$Hypertension))
  for_plot$Obesity<- (as.numeric(for_plot$Obesity))
  for_plot$Hypercholesterolemia<- (as.numeric(for_plot$Hypercholesterolemia))
  for_plot$Smoke<- (as.numeric(for_plot$Smoke))
  for_plot$Diabetes<- (as.numeric(for_plot$Diabetes))
  for_plot$year<- (as.numeric(for_plot$year))
  colnames(for_plot)<- c("Hypertension", "Obesity", "Hypercholesterolemia", "Smoke", "Diabetes", "year")

  for_plot[for_plot<.01]<-.0049
  for_plot1[for_plot1<.01]<-.0049
  #creating dataframe for labels

  for_plotl<- for_plot1
  for_plotl <- format(round(for_plot1,2),nsmall = 2 )

  for_plotl[1,]<- NA
  for_plotl[3,]<- NA
  for_plotl[4,]<- NA
  for_plotl[6,]<- NA
  for_plotl[7,]<- NA
  for_plotl[9,]<- NA
  for_plotl[10,]<- NA
  for_plotl[12,]<- NA
  for_plotl[13,]<- NA
  for_plotl[15,]<- NA

  suppressWarnings(ggplot(for_plot, aes(), ylim(0,2)) +
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Hypertension-(for_plot1$Hypertension1*1), ymax= for_plot$Hypertension+ (for_plot1$Hypertension1*0), fill = "Hypertension"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Obesity-(for_plot1$Obesity1*1), ymax= for_plot$Obesity+(for_plot1$Obesity1*0), fill = "Obesity" ))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Smoke-(for_plot1$Smoke1*1), ymax= for_plot$Smoke+(for_plot1$Smoke1*0), fill = "Smoke"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Hypercholesterolemia-(for_plot1$Hypercholesterolemia1*1), ymax= for_plot$Hypercholesterolemia+(for_plot1$Hypercholesterolemia1*0), fill = "Hypercholesterolemia"))+
                     geom_ribbon(aes(x = for_plot$year, ymin = for_plot$Diabetes-(for_plot1$Diabetes1*1), ymax= for_plot$Diabetes+(for_plot1$Diabetes1*0), fill = "Diabetes"))+
                     #changing styling
                     theme(panel.grid.major = element_blank(),legend.position = "bottom",axis.ticks.x=element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.text.x = element_text(face="bold"),axis.title.y=element_blank(),
                           axis.text.y=element_blank(),
                           axis.ticks.y=element_blank(),axis.title.x = element_text(color="black", size=14, face="bold") )+
                     scale_x_continuous(position = 'top') + labs(x = plot_title_in_quotes)+
                     #adding inside labels
                     geom_label(label =  for_plotl$Hypertension1, y = (for_plot$Hypertension-(for_plot1$Hypertension1*.5)), x = for_plot1$year1, color = "white", fill = "darkseagreen", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Hypercholesterolemia1, y = (for_plot$Hypercholesterolemia-(for_plot1$Hypercholesterolemia1*.5)), x = for_plot1$year1, color = "white", fill = "tan2", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Smoke1, y = (for_plot$Smoke-(for_plot1$Smoke1*.5)), x = for_plot1$year1, color = "white", fill = "lightskyblue", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Diabetes1, y = (for_plot$Diabetes-(for_plot1$Diabetes1*.5)), x = for_plot1$year1, color = "white", fill = "tomato2", colour = "white", label.size = 0, na.rm = TRUE)+

                     geom_label(label = for_plotl$Obesity1, y = (for_plot$Obesity-(for_plot1$Obesity1*.5)), x = for_plot1$year1, color = "white", fill = "steelblue4", colour = "white", label.size = 0, na.rm = TRUE)+
                     #changing fill colors
                     scale_fill_manual(values=c("tomato2", "tan2", "darkseagreen", "steelblue4","lightskyblue"), name=""))
}
