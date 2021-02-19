Oldest_poneys <- function(mon_pari,ma_drogue){

  couleurs <- c("green","red","orange","blue","purple","brown")
  names(couleurs) <- (1:6)
  course <- "en course"
  vainqueur <- "empty"
  positions <- c(rep(-2,6))

  pas1 <- 5
  pas2 <- 5
  pas3 <- 5
  pas4 <- 5
  pas5 <- 5
  pas6 <- 5

  vitesses <- c(pas1,pas2,pas3,pas4,pas5,pas6)
  survie <- c(rep("vivant",6))
  names(positions) <- c(1:6)
  names(vitesses) <- c(1:6)
  names(survie) <- c(1:6)
  poneys <- data.frame(cbind(survie,positions,vitesses))

  drogues <- matrix(data=c(1,1.5,1.5,1,1.5,2,1,2,3), nrow=3,ncol=3,
                    byrow=F)



  ifelse(mon_pari == "green", prob1  <- c(drogues[,ma_drogue]), prob1 <- c(1,1,1))
  ifelse(mon_pari == "red", prob2  <- c(drogues[,ma_drogue]), prob2 <- c(1,1,1))
  ifelse(mon_pari == "orange", prob3  <- c(drogues[,ma_drogue]), prob3 <- c(1,1,1))
  ifelse(mon_pari == "blue", prob4  <- c(drogues[,ma_drogue]), prob4 <- c(1,1,1))
  ifelse(mon_pari == "purple", prob5  <- c(drogues[,ma_drogue]), prob5 <- c(1,1,1))
  ifelse(mon_pari == "brown", prob6  <- c(drogues[,ma_drogue]), prob6 <- c(1,1,1))


  #######################


  par(mar=c(9,1,9,1))
  plot(poneys$positions,1:6, main="Petit poney", sub="foule en delire",
       xlim=c(0, 100), ylim=c(0.5,6.5),ylab="",xlab="CRS",pch = 19,
       col=c("dark green","red","orange","blue","purple","brown"), xaxt='n', yaxt='n')

  text(x=5,y=5, "5 PURPLE", col="purple",cex=0.8,adj=0 )
  text(x=5,y=6, "6 BROWN", col="brown",cex=0.8,adj=0 )
  text(x=5,y=1, "1 GREEN", col="dark green",cex=0.8,adj=0 )
  text(x=5,y=4, "4 BLUE", col="blue",cex=0.8,adj=0 )
  text(x=5,y=2, "2 RED", col="red",cex=0.8,adj=0 )
  text(x=5,y=3, "3 ORANGE", col="orange",cex=0.8,adj=0 )
  text(x=97,y=3.5, "ARRIVEE",srt=270,cex=2,col="pink")

  abline(h=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),col="grey",lty="dashed")
  abline(v=c(0,100),lty=1,col="grey",pch="dashed",lwd=2)
  abline(v=c(25,50,75), col="grey", lty=2, pch="long dashed", lwd=2)

  Sys.sleep(0.9)

  ############### BOUCLE WHILE ################

  while(course == "en course"){

    ## BOUCLE MORT ##

    names(vitesses) <- c(1:6)
    if(any(vitesses > 10)){
      D <- names(vitesses[which(vitesses>10)])
      print(D)
      survie [D] <- "DCD"
      survie
      print(paste("le poney", D, "est mort"))
      poneys <- data.frame(cbind(survie,positions,vitesses))

    }

    ################ boucle VICTOIRE ##############

    if(any(positions > 100)){
      G <- names(positions[which(positions>100)])
      print(G)
      survie [G] <- "gagnant"
      positions [G] <- 100
      print(paste("le poney", G, "est arrive"))
      if(vainqueur == "empty"){
        vainqueur <- couleurs[G]
      }
      poneys <- data.frame(cbind(survie,positions,vitesses))
    }

    #############  course finie ##

    if(all(vitesses == 0)){
      course <- "finie"
    }

    ####################

    ifelse(poneys[1,1] == "vivant",
           ifelse(pas1 < 2,
                  pas1 <- pas1+2,
                  pas1 <- pas1 + sample(-1:1,1,prob=prob1)),
           pas1 <- 0)

    ifelse(poneys[2,1] == "vivant",
           ifelse(pas2 < 1.5,
                  pas2 <- pas2+1, pas2 <- pas2 + sample((-1:1),1,prob=prob2)),
           pas2 <- 0)

    ifelse(poneys[3,1] == "vivant",ifelse(
      pas3 < 1.5, pas3 <- pas3+1, pas3 <- pas3 + sample((-1:1),1,prob=prob3)),
      pas3 <- 0)
    ifelse(poneys[4,1] == "vivant",ifelse(
      pas4 < 1.5, pas4 <- pas4+1.5, pas4 <- pas4 + sample((-1:1),1,prob=prob4)),
      pas4 <- 0)
    ifelse(poneys[5,1] == "vivant",ifelse(
      pas5 < 1.5, pas5 <- pas5+1, pas5 <- pas5 + sample((-1:1),1,prob=prob5)),
      pas5 <- 0)
    ifelse(poneys[6,1] == "vivant",ifelse(
      pas6 < 1.5, pas6 <- pas6+1, pas6 <- pas6 + sample((-1:1),1,prob=prob6)),
      pas6 <- 0)

    print(prob1)

    vitesses <- c(pas1,pas2,pas3,pas4,pas5,pas6)
    positions <- positions + vitesses


    poneys <- data.frame(cbind(survie,positions,vitesses))

    plot(poneys$positions,1:6,main="Petit poney", sub="foule en delire",
         xlim=c(0, 100), ylim=c(0.5,6.5),ylab="",xlab="CRS",pch = 19,
         col=c("dark green","red","orange","blue","purple","brown"), xaxt='n', yaxt='n')


    text(x=5,y=5, "5 PURPLE", col="purple",cex=0.8,adj=0 )
    text(x=5,y=6, "6 BROWN", col="brown",cex=0.8,adj=0 )
    text(x=5,y=1, "1 GREEN", col="dark green",cex=0.8,adj=0 )
    text(x=5,y=4, "4 BLUE", col="blue",cex=0.8,adj=0 )
    text(x=5,y=2, "2 RED", col="red",cex=0.8,adj=0 )
    text(x=5,y=3, "3 ORANGE", col="orange",cex=0.8,adj=0 )
    text(x=97,yOldest_poneys <- function(mon_pari,ma_drogue){

      couleurs <- c("green","red","orange","blue","purple","brown")
      names(couleurs) <- (1:6)
      course <- "en course"
      vainqueur <- "empty"
      positions <- c(rep(-2,6))

      pas1 <- 5
      pas2 <- 5
      pas3 <- 5
      pas4 <- 5
      pas5 <- 5
      pas6 <- 5

      vitesses <- c(pas1,pas2,pas3,pas4,pas5,pas6)
      survie <- c(rep("vivant",6))
      names(positions) <- c(1:6)
      names(vitesses) <- c(1:6)
      names(survie) <- c(1:6)
      poneys <- data.frame(cbind(survie,positions,vitesses))

      drogues <- matrix(data=c(1,1.5,1.5,1,1.5,2,1,2,3), nrow=3,ncol=3,
                        byrow=F)



      ifelse(mon_pari == "green", prob1  <- c(drogues[,ma_drogue]), prob1 <- c(1,1,1))
      ifelse(mon_pari == "red", prob2  <- c(drogues[,ma_drogue]), prob2 <- c(1,1,1))
      ifelse(mon_pari == "orange", prob3  <- c(drogues[,ma_drogue]), prob3 <- c(1,1,1))
      ifelse(mon_pari == "blue", prob4  <- c(drogues[,ma_drogue]), prob4 <- c(1,1,1))
      ifelse(mon_pari == "purple", prob5  <- c(drogues[,ma_drogue]), prob5 <- c(1,1,1))
      ifelse(mon_pari == "brown", prob6  <- c(drogues[,ma_drogue]), prob6 <- c(1,1,1))


      #######################


      par(mar=c(9,1,9,1))
      plot(poneys$positions,1:6, main="Petit poney", sub="foule en delire",
           xlim=c(0, 100), ylim=c(0.5,6.5),ylab="",xlab="CRS",pch = 19,
           col=c("dark green","red","orange","blue","purple","brown"), xaxt='n', yaxt='n')

      text(x=5,y=5, "5 PURPLE", col="purple",cex=0.8,adj=0 )
      text(x=5,y=6, "6 BROWN", col="brown",cex=0.8,adj=0 )
      text(x=5,y=1, "1 GREEN", col="dark green",cex=0.8,adj=0 )
      text(x=5,y=4, "4 BLUE", col="blue",cex=0.8,adj=0 )
      text(x=5,y=2, "2 RED", col="red",cex=0.8,adj=0 )
      text(x=5,y=3, "3 ORANGE", col="orange",cex=0.8,adj=0 )
      text(x=97,y=3.5, "ARRIVEE",srt=270,cex=2,col="pink")

      abline(h=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),col="grey",lty="dashed")
      abline(v=c(0,100),lty=1,col="grey",pch="dashed",lwd=2)
      abline(v=c(25,50,75), col="grey", lty=2, pch="long dashed", lwd=2)

      Sys.sleep(0.9)

      ############### BOUCLE WHILE ################

      while(course == "en course"){

        ## BOUCLE MORT ##

        names(vitesses) <- c(1:6)
        if(any(vitesses > 10)){
          D <- names(vitesses[which(vitesses>10)])
          print(D)
          survie [D] <- "DCD"
          survie
          print(paste("le poney", D, "est mort"))
          poneys <- data.frame(cbind(survie,positions,vitesses))

        }

        ################ boucle VICTOIRE ##############

        if(any(positions > 100)){
          G <- names(positions[which(positions>100)])
          print(G)
          survie [G] <- "gagnant"
          positions [G] <- 100
          print(paste("le poney", G, "est arrive"))
          if(vainqueur == "empty"){
            vainqueur <- couleurs[G]
          }
          poneys <- data.frame(cbind(survie,positions,vitesses))
        }

        #############  course finie ##

        if(all(vitesses == 0)){
          course <- "finie"
        }

        ####################

        ifelse(poneys[1,1] == "vivant",
               ifelse(pas1 < 2,
                      pas1 <- pas1+2,
                      pas1 <- pas1 + sample(-1:1,1,prob=prob1)),
               pas1 <- 0)

        ifelse(poneys[2,1] == "vivant",
               ifelse(pas2 < 1.5,
                      pas2 <- pas2+1, pas2 <- pas2 + sample((-1:1),1,prob=prob2)),
               pas2 <- 0)

        ifelse(poneys[3,1] == "vivant",ifelse(
          pas3 < 1.5, pas3 <- pas3+1, pas3 <- pas3 + sample((-1:1),1,prob=prob3)),
          pas3 <- 0)
        ifelse(poneys[4,1] == "vivant",ifelse(
          pas4 < 1.5, pas4 <- pas4+1.5, pas4 <- pas4 + sample((-1:1),1,prob=prob4)),
          pas4 <- 0)
        ifelse(poneys[5,1] == "vivant",ifelse(
          pas5 < 1.5, pas5 <- pas5+1, pas5 <- pas5 + sample((-1:1),1,prob=prob5)),
          pas5 <- 0)
        ifelse(poneys[6,1] == "vivant",ifelse(
          pas6 < 1.5, pas6 <- pas6+1, pas6 <- pas6 + sample((-1:1),1,prob=prob6)),
          pas6 <- 0)

        print(prob1)

        vitesses <- c(pas1,pas2,pas3,pas4,pas5,pas6)
        positions <- positions + vitesses


        poneys <- data.frame(cbind(survie,positions,vitesses))

        plot(poneys$positions,1:6,main="Petit poney", sub="foule en delire",
             xlim=c(0, 100), ylim=c(0.5,6.5),ylab="",xlab="CRS",pch = 19,
             col=c("dark green","red","orange","blue","purple","brown"), xaxt='n', yaxt='n')


        text(x=5,y=5, "5 PURPLE", col="purple",cex=0.8,adj=0 )
        text(x=5,y=6, "6 BROWN", col="brown",cex=0.8,adj=0 )
        text(x=5,y=1, "1 GREEN", col="dark green",cex=0.8,adj=0 )
        text(x=5,y=4, "4 BLUE", col="blue",cex=0.8,adj=0 )
        text(x=5,y=2, "2 RED", col="red",cex=0.8,adj=0 )
        text(x=5,y=3, "3 ORANGE", col="orange",cex=0.8,adj=0 )
        text(x=97,y=3.5, "ARRIVEE",srt=270,cex=2,col="pink")

        abline(h=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),col="grey",lty="dashed")
        abline(v=c(0,100),lty=1,col="grey",pch="dashed",lwd=2)
        abline(v=c(25,50,75), col="grey", lty=2, pch="long dashed", lwd=2)


        Sys.sleep(0.6)


        ############### Fin boucle While ##################
      }
      print("la course est finie ♥")
      print(paste("le vainqueur est", vainqueur, "!!!"))
      if(vainqueur == mon_pari){
        print("vous gagnez")
      } else {
        print ("vous perdez")}
    }



    =3.5, "ARRIVEE",srt=270,cex=2,col="pink")

    abline(h=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),col="grey",lty="dashed")
    abline(v=c(0,100),lty=1,col="grey",pch="dashed",lwd=2)
    abline(v=c(25,50,75), col="grey", lty=2, pch="long dashed", lwd=2)


    Sys.sleep(0.6)


    ############### Fin boucle While ##################
  }
  print("la course est finie ♥")
  print(paste("le vainqueur est", vainqueur, "!!!"))
  if(vainqueur == mon_pari){
    print("vous gagnez")
  } else {
    print ("vous perdez")}
}



