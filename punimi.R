install.packages(moments)

library(moments)


print(skewness(TeDhenat$Mosha))


TeDhenatNomalizuar<- sqrt(-TeDhenat$Mosha+max(TeDhenat$Mosha)+1)

moshahipotezes<- sqrt(-70+max(TeDhenat$Mosha)+1)


print(skewness(TeDhenatNomalizuar))

Xmean <- mean(TeDhenatNomalizuar)

Xsd <- sd(TeDhenatNomalizuar)

hist(TeDhenat$Mosha,
     xlim=c(30,100),
     breaks = 20,
     main = "Histogram i Moshave te te vdekurve ",
     ylab = "Numri i Vdekjeve",
     xlab = "Mosha",
     col="red")


hist(TeDhenatNomalizuar,
     xlim = c(0,9),
     breaks = 20,
     main = "Histogram i te dhenave pas transformimit ",
     ylab = "Numri i Vdekjeve",
     xlab = "Mosha pas transformimit",
     col="red")

z_test = function(HMean, Xmean , Xvar ) {
  
  
  z_score <-  (-1)*((Xmean-HMean)/(Xvar/sqrt(length(TeDhenatNomalizuar))))
  
  
   return(z_score)
}

z_score <- (z_test(moshahipotezes,mean(TeDhenatNomalizuar),var(TeDhenatNomalizuar)))



checkSignificance= function(score, alpha ){
  
      Z.critical <- qnorm(1-(alpha))
            if(score>Z.critical){paste0("Hipoteza eshte a sakte nivelin ",alpha)}
             else{print("Hipoteza ishte e gabuar")}
            
}


checkSignificance(z_score,alpha=0.01)



