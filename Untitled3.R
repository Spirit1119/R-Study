#Store the file as an object
credit=read.csv("fidi_carte.csv",header = TRUE)
CREDIT<-credit[,-(11:13)]

names(CREDIT)[4]<-"SCORE"
names(CREDIT)[10]<-"PLAFOND"
names(CREDIT)[7]<-"RATING_BEFORE"
names(CREDIT)[8]<-"PD_BEFORE"
names(CREDIT)[9]<-"BUREAU_SCORE"

Limit<-data.frame(CREDIT[,(10)])
names(Limit)[1]<-"PLAFOND"
Limit<-Limit[!duplicated(Limit$PLAFOND),]
Limit<-Limit[order(Limit)]
Limit<-data.frame(Limit)
names(Limit)[1]<-"PLAFOND"

Good<-vector(mode="numeric",length=86)
Bad<-vector(mode="numeric",length=86)

for (i in 1:nrow(CREDIT)) {
  for (j in 1:nrow(Limit)) {
    if(CREDIT[i,10]==Limit[j,1]){
      if(CREDIT[i,5]==1){
        Bad[j]=Bad[j]+1
      }
      else
        (Good[j]=Good[j]+1)
    }
  }
}

Limit<-cbind(Limit,Good,Bad)

LGroups<-c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,100000)
GOOD<-vector(mode="numeric",length=20)
BAD<-vector(mode="numeric",length=20)

for (i in 1:nrow(Limit)) {
  for (j in 1:19) {
    if(Limit[i,1]>=LGroups[j]&&Limit[i,1]<LGroups[j+1]){
      GOOD[j]=Limit[i,2]+GOOD[j]
      BAD[j]=Limit[i,3]+BAD[j]
    } 
  }
}
GOOD[20]=Limit[86,2]
BAD[20]=Limit[86,3]

LGroups<-cbind(LGroups,GOOD,BAD)
GOODR<-vector(mode="numeric",length=20)
BADR<-vector(mode="numeric",length=20)
GBODD<-vector(mode="numeric",length=20)
for (i in 1:20) {
  GOODR[i]=LGroups[i,2]/(LGroups[i,2]+LGroups[i,3])
  BADR[i]=LGroups[i,3]/(LGroups[i,2]+LGroups[i,3])
  GBODD[i]=GOODR[i]/BADR[i]
}

LGroups<-cbind(LGroups,GOODR,BADR,GBODD)

Group<-c("500~4000","4000~5000","5000~6000","6000~7500","7500~9000","9000~10000")
OG1<-vector(mode="numeric",length=6)
OB1<-vector(mode="numeric",length=6)
OG1[1]=GOOD[1]+GOOD[2]+GOOD[3]+GOOD[4]+GOOD[5]+GOOD[6]+GOOD[7]
OG1[2]=GOOD[8]+GOOD[9]
OG1[3]=GOOD[10]+GOOD[11]
OG1[4]=GOOD[12]+GOOD[13]+GOOD[14]
OG1[5]=GOOD[15]+GOOD[16]+GOOD[17]
OG1[6]=GOOD[18]+GOOD[19]+GOOD[20]
OB1[1]=BAD[1]+BAD[2]+BAD[3]+BAD[4]+BAD[5]+BAD[6]+BAD[7]
OB1[2]=BAD[8]+BAD[9]
OB1[3]=BAD[10]+BAD[11]
OB1[4]=BAD[12]+BAD[13]+BAD[14]
OB1[5]=BAD[15]+BAD[16]+BAD[17]
OB1[6]=BAD[18]+BAD[19]+BAD[20]
EG1<-vector(mode="numeric",length=6)
EB1<-vector(mode="numeric",length=6)
for (i in 1:6) {
  EG1[i]=(OG1[i]+OB1[i])*(sum(OG1[1:6]))/(sum(OG1[1:6])+sum(OB1[1:6]))
}
for (i in 1:6) {
  EB1[i]=(OG1[i]+OB1[i])*(sum(OB1[1:6]))/(sum(OG1[1:6])+sum(OB1[1:6]))
}
chi_square2<-0
for (i in 1:6) {
  chi_square2=chi_square2+(OG1[i]-EG1[i])^2/EG1[i]+(OB1[i]-EB1[i])^2/EB1[i]
}

GROUP<-c("500~4000","4000~5000","5000~6000","6000~7500","7500~9000","9000~10000")
GROUP1<-c("Limit1","Limit2","Limit3","Limit4","Limit5","Limit6")
GOOD<-OG1
BAD<-OB1
LIMIT_GROUP<-cbind(GROUP1,GOOD,BAD)

#----------------SCORE-------------------

Score<-data.frame(CREDIT[,(4)])
names(Score)[1]<-"SCORE"
Score<-Score[!duplicated(Score$SCORE),]
Score<-Score[order(Score)]
Score<-data.frame(Score)
names(Score)[1]<-"SCORE"

S<-c(-2.8,-2.6,-2.4,-2.2,-2.0,-1.8,-1.6,-1.4,-1.2,-1.0,-0.8,-0.6,-0.4,
     -0.2,-0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.8,3.0,3.2,3.4,3.6)
Score<-S
Score<-data.frame(Score)

GOOD1<-vector(mode="numeric",length=33)
BAD1<-vector(mode="numeric",length=33)

for (i in 1:nrow(CREDIT)) {
  for (j in 1:nrow(Score)) {
    if(CREDIT[i,4]>=S[j]&&CREDIT[i,4]<S[j+1]){
      if(CREDIT[i,5]==1){
        BAD1[j]=BAD1[j]+1
      }
      else
        (GOOD1[j]=GOOD1[j]+1)
    }
  }
}

Score<-cbind(Score,GOOD1,BAD1)

GOODR<-vector(mode="numeric",length=33)
BADR<-vector(mode="numeric",length=33)
GBODD<-vector(mode="numeric",length=33)
for (i in 1:33) {
  GOODR[i]=Score[i,2]/(Score[i,2]+Score[i,3])
  BADR[i]=Score[i,3]/(Score[i,2]+Score[i,3])
  GBODD[i]=GOODR[i]/BADR[i]
}

Score<-cbind(Score,GOODR,BADR,GBODD)

Group<-c("-2.8~-1.8","-1.8~-1.0","-1.0~-0.4","-0.4~0.6",
         "0.6~1.2","1.2~1.6","1.6~2.2","2.2~2.6","2.6~3.0","3.0~3.5")
OG2<-vector(mode="numeric",length=10)
OB2<-vector(mode="numeric",length=10)
OG2[1]=GOOD1[1]+GOOD1[2]+GOOD1[3]+GOOD1[4]+GOOD1[5]
OG2[2]=GOOD1[6]+GOOD1[7]+GOOD1[8]+GOOD1[9]
OG2[3]=GOOD1[10]+GOOD1[11]+GOOD1[12]
OG2[4]=GOOD1[13]+GOOD1[14]+GOOD1[15]+GOOD1[16]+GOOD1[17]
OG2[5]=GOOD1[18]+GOOD1[19]+GOOD1[20]
OG2[6]=GOOD1[21]+GOOD1[22]
OG2[7]=GOOD1[23]+GOOD1[24]+GOOD1[25]
OG2[8]=GOOD1[26]+GOOD1[27]
OG2[9]=GOOD1[28]+GOOD1[29]
OG2[10]=GOOD1[30]+GOOD1[31]+GOOD1[32]+GOOD1[33]
OB2[1]=BAD1[1]+BAD1[2]+BAD1[3]+BAD1[4]+BAD1[5]
OB2[2]=BAD1[6]+BAD1[7]+BAD1[8]+BAD1[9]
OB2[3]=BAD1[10]+BAD1[11]+BAD1[12]
OB2[4]=BAD1[13]+BAD1[14]+BAD1[15]+BAD1[16]+BAD1[17]
OB2[5]=BAD1[18]+BAD1[19]+BAD1[20]
OB2[6]=BAD1[21]+BAD1[22]
OB2[7]=BAD1[23]+BAD1[24]+BAD1[25]
OB2[8]=BAD1[26]+BAD1[27]
OB2[9]=BAD1[28]+BAD1[29]
OB2[10]=BAD1[30]+BAD1[31]+BAD1[32]+BAD1[33]
EG2<-vector(mode="numeric",length=10)
EB2<-vector(mode="numeric",length=10)
for (i in 1:10) {
  EG2[i]=(OG2[i]+OB2[i])*(sum(OG2[1:10]))/(sum(OG2[1:10])+sum(OB2[1:10]))
}
for (i in 1:10) {
  EB2[i]=(OG2[i]+OB2[i])*(sum(OB2[1:10]))/(sum(OG2[1:10])+sum(OB2[1:10]))
}
chi_square3<-0
for (i in 1:10) {
  chi_square3=chi_square3+(OG2[i]-EG2[i])^2/EG2[i]+(OB2[i]-EB2[i])^2/EB2[i]
}

GROUP<-c("-2.8~-1.8","-1.8~-1.0","-1.0~-0.4","-0.4~0.6",
         "0.6~1.2","1.2~1.6","1.6~2.2","2.2~2.6","2.6~3.0","3.0~3.5")
GROUP1<-c("Score1","Score2","Score3","Score4","Score5","Score6","Score7","Score8","Score9","Score10")
GOOD<-OG2
BAD<-OB2
SCORE_GROUP<-cbind(GROUP1,GOOD,BAD)



#----------------------------LOSS----------------------------------

L<-c(500,4000,5000,6000,7500,9000,10001)
S<-c(-2.8,-1.8,-1.0,-0.4,0.6,1.2,1.6,2.2,2.6,3.0,3.5)

Loss<-matrix(data=0,nrow=10,ncol=6)
Loss<-data.frame(Loss)
for (i in 1:nrow(CREDIT)) {
  if(CREDIT[i,5]==1){
    for (m in 1:10) {
      if(CREDIT[i,4]<S[m+1]&&CREDIT[i,4]>=S[m]){
        for (n in 1:6) {
          if(CREDIT[i,10]<L[n+1]&&CREDIT[i,10]>=L[n]){
            Loss[m,n]<-Loss[m,n]-CREDIT[i,10]
          }
        }
      }
    }
  }
}

names(Loss)<-c("Limit1","Limit2","Limit3","Limit4","Limit5","Limit6")
row.names(Loss)<-c("Score1","Score2","Score3","Score4","Score5","Score6","Score7","Score8","Score9","Score10")


#-------------------------transition matrix------------------------------
L1_Trans<-matrix(data=0,nrow=10,ncol=11)
S<-c(-2.8,-1.8,-1.0,-0.4,0.6,1.2,1.6,2.2,2.6,3.0,3.5)
L<-c(500,4000,5000,6000,7500,9000,10001)


for (i in 1:nrow(CREDIT)) {
  if(CREDIT[i,10]<4000&&CREDIT[i,10]>=500){
    for (m in 1:10) {
      if(CREDIT[i,4]<S[m+1]&&CREDIT[i,4]>=S[m]){
        if(CREDIT[i,5]==1){
          L1_Trans[m,11]=L1_Trans[m,11]+1
        }
        else{
          L1_Trans[m,m]=L1_Trans[m,m]+1
        }
      }
    }
  }
}

L1_Trans<-data.frame(L1_Trans)
names(L1_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                   "Score7","Score8","Score9","Score10","DEFAULT")
row.names(L1_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                       "Score7","Score8","Score9","Score10")

L2_Trans<-matrix(data=0,nrow=10,ncol=11)
for (i in 1:nrow(CREDIT)) {
  if(CREDIT[i,10]<5000&&CREDIT[i,10]>=4000){
    for (m in 1:10) {
      if(CREDIT[i,4]<S[m+1]&&CREDIT[i,4]>=S[m]){
        if(CREDIT[i,5]==1){
          L2_Trans[m,11]=L2_Trans[m,11]+1
        }
        else{
          L2_Trans[m,m]=L2_Trans[m,m]+1
        }
      }
    }
  }
}

L2_Trans<-data.frame(L2_Trans)
names(L2_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                   "Score7","Score8","Score9","Score10","DEFAULT")
row.names(L2_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                       "Score7","Score8","Score9","Score10")

L3_Trans<-matrix(data=0,nrow=10,ncol=11)
for (i in 1:nrow(CREDIT)) {
  if(CREDIT[i,10]<6000&&CREDIT[i,10]>=5000){
    for (m in 1:10) {
      if(CREDIT[i,4]<S[m+1]&&CREDIT[i,4]>=S[m]){
        if(CREDIT[i,5]==1){
          L3_Trans[m,11]=L3_Trans[m,11]+1
        }
        else{
          L3_Trans[m,m]=L3_Trans[m,m]+1
        }
      }
    }
  }
}

L3_Trans<-data.frame(L3_Trans)
names(L3_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                   "Score7","Score8","Score9","Score10","DEFAULT")
row.names(L3_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                       "Score7","Score8","Score9","Score10")

L4_Trans<-matrix(data=0,nrow=10,ncol=11)
for (i in 1:nrow(CREDIT)) {
  if(CREDIT[i,10]<7500&&CREDIT[i,10]>=6000){
    for (m in 1:10) {
      if(CREDIT[i,4]<S[m+1]&&CREDIT[i,4]>=S[m]){
        if(CREDIT[i,5]==1){
          L4_Trans[m,11]=L4_Trans[m,11]+1
        }
        else{
          L4_Trans[m,m]=L4_Trans[m,m]+1
        }
      }
    }
  }
}

L4_Trans<-data.frame(L4_Trans)
names(L4_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                   "Score7","Score8","Score9","Score10","DEFAULT")
row.names(L4_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                       "Score7","Score8","Score9","Score10")

L5_Trans<-matrix(data=0,nrow=10,ncol=11)
for (i in 1:nrow(CREDIT)) {
  if(CREDIT[i,10]<9000&&CREDIT[i,10]>=7500){
    for (m in 1:10) {
      if(CREDIT[i,4]<S[m+1]&&CREDIT[i,4]>=S[m]){
        if(CREDIT[i,5]==1){
          L5_Trans[m,11]=L5_Trans[m,11]+1
        }
        else{
          L5_Trans[m,m]=L5_Trans[m,m]+1
        }
      }
    }
  }
}

L5_Trans<-data.frame(L5_Trans)
names(L5_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                   "Score7","Score8","Score9","Score10","DEFAULT")
row.names(L5_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                       "Score7","Score8","Score9","Score10")

L6_Trans<-matrix(data=0,nrow=10,ncol=11)
for (i in 1:nrow(CREDIT)) {
  if(CREDIT[i,10]<10001&&CREDIT[i,10]>=9000){
    for (m in 1:10) {
      if(CREDIT[i,4]<S[m+1]&&CREDIT[i,4]>=S[m]){
        if(CREDIT[i,5]==1){
          L6_Trans[m,11]=L6_Trans[m,11]+1
        }
        else{
          L6_Trans[m,m]=L6_Trans[m,m]+1
        }
      }
    }
  }
}

L6_Trans<-data.frame(L6_Trans)
names(L6_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                   "Score7","Score8","Score9","Score10","DEFAULT")
row.names(L6_Trans)<-c("Score1","Score2","Score3","Score4","Score5","Score6",
                       "Score7","Score8","Score9","Score10")

sum1<-vector(mode="numeric",length=10)
for (i in 1:10) {
  sum1[i]=sum(L1_Trans[i,1:11])
  if(sum1[i]!=0){
    for (j in 1:11) {
      L1_Trans[i,j]=L1_Trans[i,j]/sum1[i]
    }
  }
}

sum2<-vector(mode="numeric",length=10)
for (i in 1:10) {
  sum2[i]=sum(L2_Trans[i,1:11])
  if(sum2[i]!=0){
    for (j in 1:11) {
      L2_Trans[i,j]=L2_Trans[i,j]/sum2[i]
    } 
  }
}

sum3<-vector(mode="numeric",length=10)
for (i in 1:10) {
  sum3[i]=sum(L3_Trans[i,1:11])
  if(sum3[i]!=0){
    for (j in 1:11) {
      L3_Trans[i,j]=L3_Trans[i,j]/sum3[i]
    } 
  }
}

sum4<-vector(mode="numeric",length=10)
for (i in 1:10) {
  sum4[i]=sum(L4_Trans[i,1:11])
  if(sum4[i]!=0){
    for (j in 1:11) {
      L4_Trans[i,j]=L4_Trans[i,j]/sum4[i]
    } 
  }
}

sum5<-vector(mode="numeric",length=10)
for (i in 1:10) {
  sum5[i]=sum(L5_Trans[i,1:11])
  if(sum5[i]!=0){
    for (j in 1:11) {
      L5_Trans[i,j]=L5_Trans[i,j]/sum5[i]
    } 
  }
}

sum6<-vector(mode="numeric",length=10)
for (i in 1:10) {
  sum6[i]=sum(L6_Trans[i,1:11])
  if(sum6[i]!=0){
    for (j in 1:11) {
      L6_Trans[i,j]=L6_Trans[i,j]/sum6[i]
    } 
  }
}



