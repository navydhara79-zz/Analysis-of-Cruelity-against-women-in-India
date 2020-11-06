# Analysis-of-Cruelity-against-women-in-India

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/6487145b125b4672b25dbaed48dbd939)](https://app.codacy.com/gh/navydhara79/Analysis-of-Cruelity-against-women-in-India?utm_source=github.com&utm_medium=referral&utm_content=navydhara79/Analysis-of-Cruelity-against-women-in-India&utm_campaign=Badge_Grade)


## what to clean - > there are some empty values. some NA are represented as na. Some digits are written in words



dim(df)

summary(df)

str(df)

new_df = df

new_dfx2013 = NULL 	//delete the colum which has no values

new_df [new_df == "na"] <- NA

new_df[new_df =="twothousandtwo"] <- NA

sum(is.na(new_df))


test_df[is.na(test_df)] = 0

> ndf$X2008 = as.numeric(ndf$X2008)
> ndf$X2004[is.na(ndf$X2004)] = mean(ndf$X2004, na.rm =TRUE)
ck$X2004[is.na(ck$X2004)] <- round(mean(ck$X2004, na.rm = TRUE)) //replace with mean of coumn
apply(test_df,2,function(x) sum(is.na(test_df))) //count the na vals
> ck[sapply(ck, is.numeric)] <- lapply(ck[sapply(ck, is.numeric)], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

boxplot(t,horizontal = TRUE)$out //to see all the outliers



____________________________________________________________________________________________________________________________________________________________________________________________


## DIVIDING INTO SEPERATE DATA SETS

> ck_rape = subset(ck, CRIME.HEAD == "RAPE") //to divide into subsets
> ck_kidnap = subset(ck, CRIME.HEAD == "KIDNAPPING AND ABDUCTION")
> ck_dowry = subset(ck, CRIME.HEAD == "DOWRY DEATHS")
> ck_assault = subset(ck, CRIME.HEAD == "ASSAULT ON WOMEN WITH INTENT TO OUTRAGE HER MODESTY")
> ck_insult = subset(ck, CRIME.HEAD == "INSULT TO THE MODESTY OF WOMEN")
> ck_cruelty = subset(ck, CRIME.HEAD == "CRUELTY BY HUSBAND OR HIS RELATIVES (IPC SECTION 498A)")
> ck_importation = subset(ck, CRIME.HEAD == "IMPORTATION OF GIRLS FROM FOREIGN COUNTRY")
> ck_traffic = subset(ck, CRIME.HEAD == "IMMORAL TRAFFIC (P) ACT")
> ck_prohibition = subset(ck, CRIME.HEAD == "DOWRY PROHIBITION ACT")
> ck_indecent = subset(ck, CRIME.HEAD == "INDECENT REPRESENTATION OF WOMEN (P) ACT")
> ck_sati = subset(ck, CRIME.HEAD == "COMMISSION OF SATI (P) ACT")
> ck_total = subset(ck, CRIME.HEAD == "TOTAL CRIMES AGAINST WOMEN")


_________________________________________________________________________________________________________________________________________________________________________________________________

## PLOTS:

>boxplot(ck)
> plot.ts(ck_cruelty[3:12], main = "cruelty")
> plot.ts(ck_dowry[3:12], main = "DOWRY DEATHS")
> plot.ts(ck_importation[3:12], main = "IMPORTATION OF GIRLS FROM FOREIGN COUNTRY")
> plot.ts(ck_indecent[3:12], main = "INDECENT REPRESENTATION OF WOMEN (P) ACT")
> plot.ts(ck_insult[3:12], main = "INSULT TO THE MODESTY OF WOMEN")
> plot.ts(ck_kidnap[3:12], main = "KIDNAPPING AND ABDUCTION")
> plot.ts(ck_prohibition[3:12], main = "DOWRY PROHIBITION ACT")
> plot.ts(ck_rape[3:12], main = "RAPE CRIME")
> plot.ts(ck_sati[3:12], main = "COMMISSION OF SATI (P) ACT")
> plot.ts(ck_traffic[3:12], main = "IMMORAL TRAFFIC (P) ACT")
> plot.ts(ck_total[3:12], main = "Total Crimes Commited in Period of 2001-2010")


>plot(ck[,c(3,14)])
> barplot2(as.matrix(ck_assault[3:14]), main="ASSAULT ON WOMEN WITH INTENT TO OUTRAGE HER MODESTY" , xlab = "Years", ylab = "no of crimes commited")
> View(ck_cruelty)
> barplot2(as.matrix(ck_cruelty[3:14]), main="CRUELTY BY HUSBAND OR HIS RELATIVES (IPC SECTION 498A)	" , xlab = "Years", ylab = "no of crimes commited")
> barplot2(as.matrix(ck_[3:14]), main="CRUELTY BY HUSBAND OR HIS RELATIVES (IPC SECTION 498A)	" , xlab = "Years", ylab = "no of crimes commited")
> barplot2(as.matrix(ck_total[3:14]), main="Total Crimes" , xlab = "Years", ylab = "no of crimes commited")
> barplot2(as.matrix(ck_cruelty[3:14]), main="CRUELTY BY HUSBAND OR HIS RELATIVES (IPC SECTION 498A)	" , xlab = "Years", ylab = "no of crimes commited")
> qqnorm(ck_total$X2001)
> qqline(ck_total$X2001)
> q=ggplot(ck,aes(x= STATE.UT,y=ck$X2001))+geom_point()
> q + theme(axis.text.x = element_text(angle = 90, hjust = 1))
> q=ggplot(ck,aes(x= STATE.UT,y=ck$X2001),main = "Plot for crimes in each state in year 2001")+geom_bin2d()
> q + theme(axis.text.x = element_text(angle = 90, hjust = 1))
> barplot(as.matrix(ck_total[3:10]),as.matrix(ck_assault[3:10]),beside =  TRUE, main =  "Total Crimes V crimes under assault", xlab = "years",ylab = "No_of_crimes")
> barplot2(as.matrix(ck_dowry[3:10]),as.matrix(ck_indecent[3:10]),beside =  TRUE, main =  "Total Crimes under dowry crime V crimes under indecent behavior", xlab = "years",ylab = "No_of_crimes")
> barplot2(as.matrix(ck_rape[3:10]),as.matrix(ck_kidnap[3:10]),beside =  TRUE, main =  "Total Crimes under Rape crime V crimes under kidnap", xlab = "years",ylab = "No_of_crimes")


________________________________________________________________________________________________________________________________________________________________________________

## correlation

>ggqqplot(ck$X2010,main = "correlation of data")

________________________________________________________________________________________________________________________________________________________________________________________________________________

## HYPOTHESIS TESTING

> tt = data.frame(ck_total$X2001,ck_total$X2012)
> tt$diff = (tt$ck_total.X2012 - tt$ck_total.X2001)
> hist(tt$diff, right = FALSE, col = "skyblue", main ="total crime difference between 2012  and 2001", xlab = "After - Before Difference")
> t.test(tt$ck_total.X2012, tt$ck_total.X2001, paired = TRUE, alternative = "l") //p-val > 0.05, so increased crimes






