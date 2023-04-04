#dev.off()
alpha=pi/3
A=1
B=1
minustrue=1
obstaclex=c(4.14)

radiusob=0.2
riztance=1.5
corridorw=2

A=A*radiusob
B=B*radiusob
cyclecount=0

N=20
tevacuation=numeric(N)
runcount=1
maxruncount=100
maxtimercounter=100
dataa=numeric(maxruncount)
vardataa=numeric(maxruncount)
while(runcount<=maxruncount){
  
  #GENERATE AND PLOT SPHERES
  n=4
  N=20 #Set the number of people we want
  #Set the starting y-position of particles on right side of corridor
  xs=runif(N,-2,0)                   #Combine to get the starting x-positions of all of them
  ys=runif(N,-1,1)    #Uniform randomly set at which point in corridor the particles will appear
  zs=data.frame(xs,ys) #pair the positions up to get vector positions
  zcurrent=zs
  xcurrent=xs
  ycurrent=ys
  theta = seq(0, 2 * pi, length = 200) #setup for plotting circles
  radi=0.2
  r=numeric(N)+0.2                    #radii of particles are uniform normal
  i=1
  while(i<=N/2){                         #Plots the circles
    xtemp=as.numeric(zs[i,][1])
    ytemp=as.numeric(zs[i,][2])
    plot(xtemp,ytemp,xlim = c(-5,5),ylim=c(-5,5),asp=1)
    par(new=TRUE)
    lines(x = r[i] * cos(theta) + xtemp, y = r[i] * sin(theta) + ytemp,asp=1,xlim = c(1,10),ylim=c(-10,40),col=2)
    
    par(new=TRUE)
    i=i+1
  }
  while(i<=N){                         #Plots the circles
    xtemp=as.numeric(zs[i,][1])
    ytemp=as.numeric(zs[i,][2])
    plot(xtemp,ytemp,xlim = c(-5,5),ylim=c(-5,5),asp=1)
    par(new=TRUE)
    lines(x = r[i] * cos(theta) + xtemp, y = r[i] * sin(theta) + ytemp,asp=1,xlim = c(1,10),ylim=c(-10,40),col=2)
    
    par(new=TRUE)
    i=i+1
  }
  
  #GENERATE DESIRED SPEEDS AND MASSES OF PARTICLES (AND CONSTANT REACTION TIME AS SUGGESTED BY TEXTBOOK)
  speeddesired=numeric(N)+1.34
  m=1
  timercount=1
  tau=0.1
  vstartxl=numeric(N/2)+1.34
  vstartxr=numeric(N/2)+1.34
  vstartx=c(vstartxl,vstartxr)
  vstarty=numeric(N)
  vcurrentx=vstartx
  vcurrenty=vstarty
  vcurrent=data.frame(vcurrentx,vcurrenty)
  #SOME CONSTANT VALUES FOR THE MODEL
  speedmax=1.3*speeddesired
  #the lower the tau the more agressive the pedestrians
  tdelta=2
  V0=2.1
  sigma=0.3
  timestep=0.05
  tcount=0
  corridorw=5
  exitw=0.55
  exitxpos=5
  corridorarg=seq(-11,11,length=200)
  lines(x=corridorarg,y=numeric(200)+corridorw/2)
  par(new=TRUE)
  lines(x=corridorarg,y=numeric(200)-corridorw/2)
  par(new=TRUE)
  
  #DESIRED DIRECTION - The desired direction for corridor movement will just be straight along x axis
  xdesiredl=numeric(N/2)+exitxpos
  xdesiredr=numeric(N/2)+exitxpos
  xdesiredplace=c(xdesiredl,xdesiredr)
  ydesiredplace=numeric(N)
  placedesired=data.frame(xdesiredplace,ydesiredplace)
  dirdesired=placedesired-zcurrent
  xdesired=dirdesired[,1]
  ydesired=dirdesired[,2]
  
  
  i=1
  while(i<=N){
    dirdesiredtemp=c(xdesired[i],ydesired[i])
    xdesired[i]=xdesired[i]/(norm(dirdesiredtemp,type="2"))
    ydesired[i]=ydesired[i]/(norm(dirdesiredtemp,type="2"))
    i=i+1
  }
  xdesired=c(numeric(N/2)+exitxpos+0.1,numeric(N/2)+exitxpos+0.1)
  dirdesired=data.frame(xdesired,ydesired)
  #DESIRED VELOCITY
  vdesiredx=speeddesired*xdesired
  vdesiredy=speeddesired*ydesired
  vdesired=data.frame(vdesiredx,vdesiredy)
  
  
  
  
  
  
  
  
  
  
  
  
  #START OF MAIN LOOP
  while(tcount<N){
    par(new=FALSE)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #ACCELERATION TERM TOWARDS DESIRED VELOCITY
    Fa=(1/tau)*(vdesired-vcurrent)
    
    #DIRECTION BETWEEN POSITIONS OF PARTICLES
    Dirx=matrix(0,N,N)
    i=1
    j=1
    while(i<=N){
      while(j<=N){
        Dirx[i,j]=xcurrent[i]-xcurrent[j]
        j=j+1
      }
      i=i+1
      j=1
    }
    
    Diry=matrix(0,N,N)
    i=1
    j=1
    while(i<=N){
      while(j<=N){
        Diry[i,j]=ycurrent[i]-ycurrent[j]
        j=j+1
      }
      i=i+1
      j=1
    }
    
    
    #SEMI-AXIS OF ELLIPSE CALCULATION
    #i=1
    #j=1
    #while(i<=N){
    #  while(j<=N){
    #    rtemp=c(Dirx[i,j],Diry[i,j])
    #    vtemp=c(vcurrent[j,1],vcurrent[j,2])
    #   dirtemp=c(dirdesired[j,1],dirdesired[j,2])
    #    b[i,j]=(1/2)*sqrt((norm(rtemp,type="2")+norm(rtemp-norm(vtemp,type="2")*tdelta*dirtemp,type="2"))^2-(norm(vtemp,type="2")*tdelta)^2)
    #    j=j+1
    #  }
    #  i=i+1
    # j=1
    #}
    
    
    #POTENTIALS BETWEEN PEDESTRIANS
    #Pp=matrix(0,N,N)
    #i=1
    #j=1
    #while(i<=N){
    #  while(j<=N){
    #    Pp[i,j]=V0*exp(-b[i,j]/sigma)
    #    j=j+1
    
    #  }
    #  i=i+1
    #  j=1
    #}
    
    
    
    
    #Ensuring opposite sign
    #  i=1
    #  j=1
    #  while(i<=N){
    #    while(j<=N){
    
    #      if(xcurrent[i]<xcurrent[j]){
    #       Fpedx[i,j]=-abs(Fpedx[i,j])
    #       Fpedx[j,i]=abs(Fpedx[j,i])
    #        j=j+1
    #      }
    #      else if(xcurrent[i]>xcurrent[j]){
    #        Fpedx[i,j]=abs(Fpedx[i,j])
    #       Fpedx[j,i]=-abs(Fpedx[j,i])
    #       j=j+1
    #     }
    #     else{j=j+1}
    
    
    
    #   }
    #   j=1
    #   i=i+1
    # }
    
    #  i=1
    #  j=1
    #  while(i<=N){
    #   while(j<=N){
    
    #     if(ycurrent[i]<ycurrent[j]){
    #       Fpedy[i,j]=-abs(Fpedy[i,j])
    #       Fpedy[j,i]=abs(Fpedy[j,i])
    #       j=j+1
    #      }
    #     else if(ycurrent[i]>ycurrent[j]){
    #       Fpedy[i,j]=abs(Fpedy[i,j])
    #       Fpedy[j,i]=-abs(Fpedy[j,i])
    #        j=j+1
    #     }
    #     else{j=j+1}
    
    
    #    }
    #    j=1
    #   i=i+1
    # }
    
    
    
    
    
    
    
    
    
    #OBTACLE REPULSION SETUP (CORRIDOR)- ASSUME CORRDOR IS 5m WIDE FOR NOW
    #corridor assumed between -3 and 3
    U0=10
    R=0.2
    
    #direction between pedestrian and closest wall position
    absmin <- function(x) { x[which.min( abs(x) )]}
    Dirwy=numeric(N)
    i=1
    while(i<=N){
      Dirwy[i]=absmin(c(ycurrent[i]-corridorw/2,ycurrent[i]+corridorw/2))
      i=i+1
    }
    
    Dirwx=numeric(N)
    
    
    
    
    #REPULSIVE OBSTACLES
    i=1
    Fpedwx=numeric(N)
    Fpedwy=numeric(N)
    
    while(i<=N){
      Pp<-function(Dirwx,Dirwy){
        B<-U0*exp(-norm(c(Dirwx,Dirwy),type="2")/R)
        return(B)
      }
      Partialxdir<-function(Dirwx,Dirwy){
        k=-1
        l=k-1
        m=k-2
        error=1
        while(error==1){
          B1<-(Pp(Dirwx+10^k,Dirwy)-Pp(Dirwx-10^k,Dirwy))/(2*10^k)
          B2<-(Pp(Dirwx+10^l,Dirwy)-Pp(Dirwx-10^l,Dirwy))/(2*10^l)
          B3<-(Pp(Dirwx+10^m,Dirwy)-Pp(Dirwx-10^m,Dirwy))/(2*10^m)
          e1=abs(B1-B2)
          e2=abs(B2-B3)
          k=k-1
          l=k-1
          m=k-2
          
          if(e1<=0.01 & e2<=0.01){
            error=0
          }
        }
        
        return(B3)
        
      }
      Partialydir<-function(xdir,ydir){
        k=-1
        l=k-1
        m=k-2
        error=1
        while(error==1){
          B1<-(Pp(xdir,ydir+10^k)-Pp(xdir,ydir-10^k))/(2*10^k)
          B2<-(Pp(xdir,ydir+10^l)-Pp(xdir,ydir-10^l))/(2*10^l)
          B3<-(Pp(xdir,ydir+10^m)-Pp(xdir,ydir-10^m))/(2*10^m)
          e1=abs(B1-B2)
          e2=abs(B2-B3)
          k=k-1
          l=k-1
          m=k-2
          
          if(e1<=0.01 & e2<=0.01){
            error=0
          }
        }
        
        return(B3)
        
        
        if(abs(ycurrent[i])<corridorw/2){
          Fpedwx[i]=-Partialxdir(Dirwx[i],Dirwy[i])
          Fpedwy[i]=-Partialydir(Dirwx[i],Dirwy[i])
          i=i+1
        }
        else{
          Fpedwx[i]=0
          Fpedwy[i]=-5*sign(ycurrent[i])
          i=i+1
        }
      }
      
      
      
      i=1
      while(i<=N){
        if(abs(ycurrent[i])<0.9){
          Fpedwx[i]=0
          Fpedwy[i]=0
          i=i+1
        }
        else{
          i=i+1
        }
      }
      
      
      
      
      #EXIT REPULSION
      
      
      #direction between pedestrian and closest wall position
      absmin <- function(x) { x[which.min( abs(x) )]}
      Dirwx=numeric(N)
      i=1
      while(i<=N){
        Dirwx[i]=xcurrent[i]-5
        i=i+1
      }
      
      Dirwy=numeric(N)
      
      
      
      #REPULSIVE OBSTACLES
      i=1
      Fpedwx2=numeric(N)
      Fpedwy2=numeric(N)
      
      while(i<=N){
        Pp<-function(Dirwx,Dirwy){
          B<-U0*exp(-norm(c(Dirwx,Dirwy),type="2")/R)
          return(B)
        }
        Partialxdir<-function(Dirwx,Dirwy){
          k=-1
          l=k-1
          m=k-2
          error=1
          while(error==1){
            B1<-(Pp(Dirwx+10^k,Dirwy)-Pp(Dirwx-10^k,Dirwy))/(2*10^k)
            B2<-(Pp(Dirwx+10^l,Dirwy)-Pp(Dirwx-10^l,Dirwy))/(2*10^l)
            B3<-(Pp(Dirwx+10^m,Dirwy)-Pp(Dirwx-10^m,Dirwy))/(2*10^m)
            e1=abs(B1-B2)
            e2=abs(B2-B3)
            k=k-1
            l=k-1
            m=k-2
            
            if(e1<=0.01 & e2<=0.01){
              error=0
            }
          }
          
          return(B3)
          
        }
        Partialydir<-function(xdir,ydir){
          k=-1
          l=k-1
          m=k-2
          error=1
          while(error==1){
            B1<-(Pp(xdir,ydir+10^k)-Pp(xdir,ydir-10^k))/(2*10^k)
            B2<-(Pp(xdir,ydir+10^l)-Pp(xdir,ydir-10^l))/(2*10^l)
            B3<-(Pp(xdir,ydir+10^m)-Pp(xdir,ydir-10^m))/(2*10^m)
            e1=abs(B1-B2)
            e2=abs(B2-B3)
            k=k-1
            l=k-1
            m=k-2
            
            if(e1<=0.01 & e2<=0.01){
              error=0
            }
          }
          
          return(B3)
          
        }
        if(abs(ycurrent[i])>exitw/2){
          Fpedwx2[i]=-Partialxdir(Dirwx[i],Dirwy[i])
          Fpedwy2[i]=-Partialydir(Dirwx[i],Dirwy[i])
          i=i+1
        }
        else{
          Fpedwx2[i]=0
          Fpedwy2[i]=0
          i=i+1
        }
      }
      
      
      
      
      
      
      
      
      #ADDITIONAL OBSTACLE REPULSION 
      
      #positions of obstacles
      nobstacles=length(obstaclex)
      obstacleznum=rep(1:2,length.out=nobstacles)
      obstacley=numeric(length(obstaclex))
      obstaclez=data.frame(obstaclex,obstacley)
      
      
      i=1
      j=1
      obstaclexdir=matrix(0,N,length(obstaclex))
      obstacleydir=matrix(0,N,length(obstaclex))
      while(i<=N){
        while(j<=length(obstaclex)){
          obstaclexdir[i,j]=as.numeric(xcurrent[i]-(radiusob*(zcurrent[i,]-obstaclez[j,])/norm(zcurrent[i,]-obstaclez[j,],"2")+obstaclez[j,])[1])
          obstacleydir[i,j]=as.numeric(ycurrent[i]-(radiusob*(zcurrent[i,]-obstaclez[j,])/norm(zcurrent[i,]-obstaclez[j,],"2")+obstaclez[j,])[2])
          j=j+1
        }
        j=1
        i=i+1
      }
      
      #Finding the forces
      
      i=1
      j=1
      Fpedox=matrix(0,N,length(obstaclex))
      Fpedoy=matrix(0,N,length(obstaclex))
      
      while(i<=N){
        while(j<=length(obstaclex)){
          Pp<-function(Dirwx,Dirwy){
            B<-U0*exp(-norm(c(Dirwx,Dirwy),type="2")/R)
            return(B)
          }
          Partialxdir<-function(Dirwx,Dirwy){
            k=-1
            l=k-1
            m=k-2
            error=1
            while(error==1){
              B1<-(Pp(Dirwx+10^k,Dirwy)-Pp(Dirwx-10^k,Dirwy))/(2*10^k)
              B2<-(Pp(Dirwx+10^l,Dirwy)-Pp(Dirwx-10^l,Dirwy))/(2*10^l)
              B3<-(Pp(Dirwx+10^m,Dirwy)-Pp(Dirwx-10^m,Dirwy))/(2*10^m)
              e1=abs(B1-B2)
              e2=abs(B2-B3)
              k=k-1
              l=k-1
              m=k-2
              
              if(e1<=0.01 & e2<=0.01){
                error=0
              }
            }
            
            return(B3)
            
          }
          Partialydir<-function(xdir,ydir){
            k=-1
            l=k-1
            m=k-2
            error=1
            while(error==1){
              B1<-(Pp(xdir,ydir+10^k)-Pp(xdir,ydir-10^k))/(2*10^k)
              B2<-(Pp(xdir,ydir+10^l)-Pp(xdir,ydir-10^l))/(2*10^l)
              B3<-(Pp(xdir,ydir+10^m)-Pp(xdir,ydir-10^m))/(2*10^m)
              e1=abs(B1-B2)
              e2=abs(B2-B3)
              k=k-1
              l=k-1
              m=k-2
              
              if(e1<=0.01 & e2<=0.01){
                error=0
              }
            }
            
            return(B3)
          }
          Fpedox[i,j]=-Partialxdir(obstaclexdir[i,j],obstacleydir[i,j])
          Fpedoy[i,j]=-Partialydir(obstaclexdir[i,j],obstacleydir[i,j])
          j=j+1
          
        }
        j=1
        i=i+1
      }
      
      
      
      
      
      
     
      
      
      
      #ENSURING NO PHASE THROUGH WALL
      
      #direction between pedestrian and closest wall position
      absmin <- function(x) { x[which.min( abs(x) )]}
      Dirwx=numeric(N)
      i=1
      while(i<=N){
        Dirwx[i]=xcurrent[i]-5.1
        i=i+1
      }
      
      Dirwy=numeric(N)
      
      
      
      #REPULSIVE OBSTACLES
      i=1
      Fpedwx3=numeric(N)
      Fpedwy3=numeric(N)
      
      while(i<=N){
        Pp<-function(Dirwx,Dirwy){
          B<-U0*exp(-norm(c(Dirwx,Dirwy),type="2")/R)
          return(B)
        }
        Partialxdir<-function(Dirwx,Dirwy){
          k=-1
          l=k-1
          m=k-2
          error=1
          while(error==1){
            B1<-(Pp(Dirwx+10^k,Dirwy)-Pp(Dirwx-10^k,Dirwy))/(2*10^k)
            B2<-(Pp(Dirwx+10^l,Dirwy)-Pp(Dirwx-10^l,Dirwy))/(2*10^l)
            B3<-(Pp(Dirwx+10^m,Dirwy)-Pp(Dirwx-10^m,Dirwy))/(2*10^m)
            e1=abs(B1-B2)
            e2=abs(B2-B3)
            k=k-1
            l=k-1
            m=k-2
            
            if(e1<=0.01 & e2<=0.01){
              error=0
            }
          }
          
          return(B3)
          
        }
        Partialydir<-function(xdir,ydir){
          k=-1
          l=k-1
          m=k-2
          error=1
          while(error==1){
            B1<-(Pp(xdir,ydir+10^k)-Pp(xdir,ydir-10^k))/(2*10^k)
            B2<-(Pp(xdir,ydir+10^l)-Pp(xdir,ydir-10^l))/(2*10^l)
            B3<-(Pp(xdir,ydir+10^m)-Pp(xdir,ydir-10^m))/(2*10^m)
            e1=abs(B1-B2)
            e2=abs(B2-B3)
            k=k-1
            l=k-1
            m=k-2
            
            if(e1<=0.01 & e2<=0.01){
              error=0
            }
          }
          
          return(B3)
          
        }
        if(abs(ycurrent[i])>exitw/2){
          Fpedwx3[i]=-Partialxdir(Dirwx[i],Dirwy[i])
          Fpedwy3[i]=-Partialydir(Dirwx[i],Dirwy[i])
          i=i+1
        }
        else{
          Fpedwx3[i]=0
          Fpedwy3[i]=0
          i=i+1
        }
      }
      
      
      
      
      
      #PEDESTRIAN ATTRACTION (LEAVE THIS FOR NOW)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #TOTAL FORCE
      i=1
      j=1
      Ftotx=numeric(N)
      Ftoty=numeric(N)
      while(i<=N){
        if(abs(ycurrent[i])<corridorw/2){
          Ftotx[i]=Fa[i,1]+Fpedwx[i]+Fpedwx2[i]+Fpedwx3[i]+sum(Fpedox[i,])
          Ftoty[i]=Fa[i,2]+Fpedwy[i]+Fpedwy2[i]+Fpedwy3[i]+sum(Fpedoy[i,])
          i=i+1}
        else{
          Ftotx[i]=Fa[i,1]+Fpedwx[i]+Fpedwx2[i]+Fpedwx3[i]+sum(Fpedox[i,])
          Ftoty[i]=Fa[i,2]+Fpedwy[i]+Fpedwy2[i]+Fpedwy3[i]+sum(Fpedoy[i,])
          i=i+1
        }
      }
      
      
      #ACCELERATION - We just take it to be force + some fluctuations
      
      acurrentx=Ftotx/m+rnorm(N,0,0.05)
      acurrenty=Ftoty/m+rnorm(N,0,0.05)
      
      
      #CURRENT DESIRED VELOCITY WITHOUT LIMITATION
      wcurrentx=acurrentx*timestep+vcurrentx
      wcurrenty=acurrenty*timestep+vcurrenty
      wcurrent=data.frame(wcurrentx,wcurrenty)
      
      #NORIMATRIX
      
      #  i=1
      #   j=1
      #   norimatrix=matrix(0,N,N)
      #   while(i<=N){
      #    while(j<=N){
      #      vecty=zcurrent[i,]-zcurrent[j,]
      #       idotprod=vcurrentx[i]*vecty[1]+vcurrenty[i]*vecty[2]
      #      jdotprod=-vcurrentx[j]*vecty[1]-vcurrenty[j]*vecty[2]
      #      idotprod=max(idotprod,0)
      #      jdotprod=max(jdotprod,0)
      
      #      noriratio=as.numeric(abs(idotprod/(jdotprod+idotprod)))
      #     norimatrix[i,j]=noriratio
      #  
      #       j=j+1
      
      #   }
      ##   j=1
      #     i=i+1
      #  }
      
      #NEW POSITION BEFORE CALCULATING NEW VELOCITIES
      xcurrent=vcurrentx*timestep+xcurrent
      ycurrent=vcurrenty*timestep+ycurrent
      zcurrent=data.frame(xcurrent,ycurrent)
      
      i=1
      j=1
      k=1
      noricount=40
      while(k<=noricount){
        while(i<=N){
          while(j<=N){
            vecty=zcurrent[i,]-zcurrent[j,]
            nori=norm(zcurrent[i,]-zcurrent[j,],"2")
            if(nori<2*radi & i != j){
              #  idotprod=vcurrentx[i]*vecty[1]+vcurrenty[i]*vecty[2]
              #  jdotprod=vcurrentx[j]*vecty[1]+vcurrenty[j]*vecty[2]
              # noriratio=as.numeric(abs(idotprod/(jdotprod+idotprod)))
              zcurrent[i,]=zcurrent[i,]+(2*radi-nori)/2*(zcurrent[i,]-zcurrent[j,])/nori
              zcurrent[j,]=zcurrent[j,]-(2*radi-nori)/2*(zcurrent[i,]-zcurrent[j,])/nori
              j=j+1
            }
            else{j=j+1}
          }
          j=1
          i=i+1
        }
        k=k+1
      }
      
      
      xcurrent=zcurrent[,1]
      ycurrent=zcurrent[,2]
      
      
      
      #NO OVERLAP WITH OBSTACLES
      
      
      
      i=1
      j=1
      k=1
      noricount=5
      while(k<=noricount){
        while(i<=N){
          while(j<=nobstacles){
            
            
            if(obstacleznum[j]%%2==0){
              
              xrot=(xcurrent[i]-obstaclez[j,][1])*cos(alpha)-(ycurrent[i]-obstaclez[j,][2])*sin(alpha)+obstaclez[j,][1]
              yrot=(xcurrent[i]-obstaclez[j,][1])*sin(alpha)+(ycurrent[i]-obstaclez[j,][2])*cos(alpha)+obstaclez[j,][2]
            }
            else{
              xrot=(xcurrent[i]-obstaclez[j,][1])*cos(minustrue*alpha)-(ycurrent[i]-obstaclez[j,][2])*sin(minustrue*alpha)+obstaclez[j,][1]
              yrot=(xcurrent[i]-obstaclez[j,][1])*sin(minustrue*alpha)+(ycurrent[i]-obstaclez[j,][2])*cos(minustrue*alpha)+obstaclez[j,][2]
              
            }
            
            
            
            
            zrot=c(xrot,yrot)
            
            
            vecty=zrot-obstaclez[j,]
            nori=norm(vecty,"2")
            testy=(((xrot-radi*((vecty/nori)[1])-obstaclez[j,][1])^2)/(A^2))+(((yrot-radi*((vecty/nori)[2])-obstaclez[j,][2])^2)/(B^2))
            doty=vecty[1]
            nordoty=doty/nori
            angly=acos(nordoty)
            elipradi=(A*B)/sqrt(A^2*(sin(angly))^2+B^2*(cos(angly))^2)
            nori=norm(zrot-obstaclez[j,],"2")
            if(testy<1){
              #  idotprod=vcurrentx[i]*vecty[1]+vcurrenty[i]*vecty[2]
              #  jdotprod=vcurrentx[j]*vecty[1]+vcurrenty[j]*vecty[2]
              # noriratio=as.numeric(abs(idotprod/(jdotprod+idotprod)))
              zrot=zrot+(radi+as.numeric(elipradi)-nori)*(zrot-obstaclez[j,])/nori
              xrot=zrot[1]
              yrot=zrot[2]
              
              if(obstacleznum[j]%%2==0){
                xcurrent[i]=as.numeric(cos(alpha)*(as.numeric(xrot)-obstaclez[j,][1])+sin(alpha)*(as.numeric(yrot)-obstaclez[j,][2])+obstaclez[j,][1])
                ycurrent[i]=as.numeric(-sin(alpha)*(as.numeric(xrot)-obstaclez[j,][1])+cos(alpha)*(as.numeric(yrot)-obstaclez[j,][2])+obstaclez[j,][2])
              }
              else{
                xcurrent[i]=as.numeric(cos(minustrue*alpha)*(as.numeric(xrot)-obstaclez[j,][1])+sin(minustrue*alpha)*(as.numeric(yrot)-obstaclez[j,][2])+obstaclez[j,][1])
                ycurrent[i]=as.numeric(-sin(minustrue*alpha)*(as.numeric(xrot)-obstaclez[j,][1])+cos(minustrue*alpha)*(as.numeric(yrot)-obstaclez[j,][2])+obstaclez[j,][2])
              }
              j=j+1
            }
            else{j=j+1}
          }
          j=1
          i=i+1
        }
        k=k+1
      }
      
      
      
      zcurrent=data.frame(xcurrent,ycurrent)
      
      
      
      xcurrent=zcurrent[,1]
      ycurrent=zcurrent[,2]
      
      
      i=1
      while(i<=N){
        if(abs(ycurrent[i])>corridorw/2){
          ycurrent[i]=sign(ycurrent[i])*corridorw/2 +rnorm(1,0,0.01)
          i=i+1}
        else{i=i+1}
      }  #BAD CODE HERE SINCE IT MAKES BOTTOM STUFF GO UP
      zcurrent=data.frame(xcurrent,ycurrent)
      
      #NO PHASE THROUGH WALL
      
      i=1
      while(i<=N){
        if(xcurrent[i]>exitxpos-radi & xcurrent[i]<exitxpos+0.3 & abs(ycurrent[i])>exitw/2){
          xcurrent[i]=exitxpos-radi
          i=i+1
        }
        else{i=i+1}
      }
      
      i=1
      while(i<=N){
        if(xcurrent[i]>exitxpos+0.1){
          xcurrent[i]=runif(1,10000,20000)
          i=i+1}
        else{i=i+1}
      }
      
      xcurrent=xcurrent+rnorm(N,0,0.01)
      ycurrent=ycurrent+rnorm(N,0,0.01) 
      zcurrent=data.frame(xcurrent,ycurrent)
      
      
      
      
      
      
      
      
      
      
      
      #NO OVERLAP WITH OBSTACLES
      
      
      
      
      
      i=1
      j=1
      k=1
      noricount=5
      while(k<=noricount){
        while(i<=N){
          while(j<=nobstacles){
            vecty=zcurrent[i,]-obstaclez[j,]
            nori=norm(zcurrent[i,]-obstaclez[j,],"2")
            if(nori<radi+radiusob){
              #  idotprod=vcurrentx[i]*vecty[1]+vcurrenty[i]*vecty[2]
              #  jdotprod=vcurrentx[j]*vecty[1]+vcurrenty[j]*vecty[2]
              # noriratio=as.numeric(abs(idotprod/(jdotprod+idotprod)))
              zcurrent[i,]=zcurrent[i,]+(radi+radiusob-nori)*(zcurrent[i,]-obstaclez[j,])/nori
              j=j+1
            }
            else{j=j+1}
          }
          j=1
          i=i+1
        }
        k=k+1
      }
      
      
      xcurrent=zcurrent[,1]
      ycurrent=zcurrent[,2]
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #PLOTTING NEW POSITIONS
      
      i=1
      while(i<=N/2){                         #Plots the circles
        xtemp=as.numeric(zcurrent[i,][1])
        ytemp=as.numeric(zcurrent[i,][2])
        plot(xtemp,ytemp,xlim = c(-5,5),ylim=c(-5,5),asp=1)
        
        lines(x = r[i] * cos(theta) + xtemp, y = r[i] * sin(theta) + ytemp,asp=1,xlim = c(1,10),ylim=c(-10,40),col=2)
        
        par(new=TRUE)
        i=i+1
      }
      lines(x=corridorarg,y=numeric(200)+corridorw/2)
      par(new=TRUE)
      lines(x=corridorarg,y=numeric(200)-corridorw/2)
      par(new=TRUE)
      segments(x0 = exitxpos, y0 = - corridorw/2, x1 =exitxpos, y1 = (corridorw-exitw)/2- corridorw/2, col = "black")
      par(new=TRUE)
      segments(x0 = exitxpos, y0 = corridorw/2, x1 =exitxpos, y1 = -(corridorw-exitw)/2+ corridorw/2, col = "black")
      par(new=TRUE)
      
      while(i<=N){                         #Plots the circles
        xtemp=as.numeric(zcurrent[i,][1])
        ytemp=as.numeric(zcurrent[i,][2])
        plot(xtemp,ytemp,xlim = c(-5,5),ylim=c(-5,5),asp=1)
        
        lines(x = r[i] * cos(theta) + xtemp, y = r[i] * sin(theta) + ytemp,asp=1,xlim = c(1,10),ylim=c(-10,40),col=2)
        
        par(new=TRUE)
        i=i+1
      }
      i=1
      while(i<=length(obstaclex)){
        xtemp=as.numeric(obstaclez[i,][1])
        ytemp=as.numeric(obstaclez[i,][2])
        plot(xtemp,ytemp,xlim = c(-5,5),ylim=c(-5,5),asp=1)
        if(obstacleznum[i]%%2==0){
          lines(x =  A*cos(theta)*cos(alpha)+B*sin(theta)*sin(alpha)+xtemp, y =   B*sin(theta)*cos(alpha)-A*sin(alpha)*cos(theta) + ytemp,asp=1,xlim = c(1,10),ylim=c(-10,40),col=1)
        }
        else{ lines(x =  A*cos(theta)*cos(minustrue*alpha)+B*sin(theta)*sin(minustrue*alpha)+xtemp, y =   B*sin(theta)*cos(minustrue*alpha)-A*sin(minustrue*alpha)*cos(theta) + ytemp,asp=1,xlim = c(1,10),ylim=c(-10,40),col=1)}
        par(new=TRUE)
        i=i+1
      }
      par(new=TRUE)
      lines(x=corridorarg,y=numeric(200)+corridorw/2)
      par(new=TRUE)
      lines(x=corridorarg,y=numeric(200)-corridorw/2)
      par(new=TRUE)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #CURRENT VELOCITY WITH MAX LIMITATION
      i=1
      j=1
      vcurrent=matrix(0,N,2)
      
      while(i<=2){
        while(j<=N){
          normtest=norm(c(wcurrent[j,1],wcurrent[j,2]),type="2")
          if(normtest<=speedmax[j]){
            vcurrent[j,i]=wcurrent[j,i]
          }
          else{
            vcurrent[j,i]=(wcurrent[j,i]*speedmax[j])/normtest
          }
          j=j+1
        }
        i=i+1
        j=1
      }
      
      vcurrentx=vcurrent[,1]
      vcurrenty=vcurrent[,2]
      
      #ADD THE NEW DESIRED DIRECTIONS AND VELOCITIES
      ydesiredplace=numeric(N)
      placedesired=data.frame(xdesiredplace,ydesiredplace)
      dirdesired=placedesired-zcurrent
      xdesired=dirdesired[,1]
      ydesired=dirdesired[,2]
      xdesiredpos=numeric(N)+exitxpos+0.1
      ydesiredpos=numeric(N)
      
      
      i=1
      while(i<=N){
        dirdesiredtemp=c(xdesired[i],ydesired[i])
        xdesired[i]=xdesired[i]/(norm(dirdesiredtemp,type="2"))
        ydesired[i]=ydesired[i]/(norm(dirdesiredtemp,type="2"))
        i=i+1
      }
      #xdesired=-(xcurrent-xdesiredpos)
      #ydesired=-(ycurrent-ydesiredpos)
      dirdesired=data.frame(xdesired,ydesired)
      
      #DESIRED VELOCITY
      vdesiredx=speeddesired*xdesired
      vdesiredy=speeddesired*ydesired
      vdesired=data.frame(vdesiredx,vdesiredy)
      
      
      #Keeping track of time to evacuate
      
      i=1
      while(i<=N){
        if(xcurrent[i]<100){
          tevacuation[i]=tevacuation[i]+1
        }
        else{}
        i=i+1
      }
      
      
      tcount=0
      i=1
      while(i<=N){
        if(xcurrent[i]>5){
          tcount=tcount+1
          i=i+1
        }
        else{i=i+1}
        
      }
      
      timercount=timercount+1
      
      
    }
    
    
    
    
    
    
    
  }
  dataa[runcount]=timercount
  w=1
  while(w<=N){
    tevacuation[w]=tevacuation[w]*0.05-(5-xs[w])/1.34
    w=w+1
  }
  vardataa[runcount]=var(tevacuation)
  runcount=runcount+1
}