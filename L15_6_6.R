library(dplyr)
library(mltools)
library(data.table)
library(corrplot)
library(RColorBrewer)
library(olsrr)
library(performanceEstimation)
library(caret)

rented_bike_count<-function(df){
  hist(df$Rented.Bike.Count,main="The frequency plot of rented bike count",xlab="Rented bike count") #biểu đồ tần số của lượng thuê xe 
  hist(log(df$Rented.Bike.Count),main="The frequency plot of rented bike count")#biểu đồ tần số của logarit lượng thuê xe 
}

hour<-function(df){
  boxplot(Rented.Bike.Count~Hour,data=df,main="Rented bike count by hour")#biểu đồ hộp của lượng thuê xe theo giờ 
}

correlation<-function(df){
  df1=df[,!names(df)%in%c("Seasons","Holiday","Functioning.Day","Date","Hour")]#loại biến 
  corrplot(cor(df1),type="upper",tl.pos="td",method="color",addCoef.col = "brown",number.cex=0.8,col=brewer.pal(n=8, name="RdYlBu"))#biểu đồ correlation 
}

weather<-function(df){
  plot(df$Temperature..C.,log(df$Rented.Bike.Count),main="Rented bike count in Seoul by temperature")#biểu đồ phân tán của log lượng thuê xe với nhiệt độ 
  plot(df$Solar.Radiation..MJ.m2.,log(df$Rented.Bike.Count),main="Rented bike count in Seoul by solar radiation")#biểu đồ phân tán của log lượng thuê xe với bức xạ mặt trời  
}

season<-function(df){
  avg_winter=mean(df[df$Seasons=="Winter",]$Rented.Bike.Count) #trung bình lượng thuê xe theo mùa đông 
  avg_spring=mean(df[df$Seasons=="Spring",]$Rented.Bike.Count) #...
  avg_summer=mean(df[df$Seasons=="Summer",]$Rented.Bike.Count) #...
  avg_autumn=mean(df[df$Seasons=="Autumn",]$Rented.Bike.Count) #...
  ss=c(avg_winter,avg_spring,avg_summer,avg_autumn)
  barplot(ss,names.arg=c("Winter","Spring","Summer","Autumn"),main="Average Rented Bike Count By Season",xlab="Season",ylab="Average Rented Bike Count")#biểu đố thanh trung bình lượng thuê xe theo mùa 
  boxplot(Rented.Bike.Count~Seasons,data=df,main="Rented bike count in Seoul from 2017 to 2018")#biểu đồ hộp lượng thuê xe theo mùa 
}

holiday<-function(df){
  Holiday=mean(df[df$Holiday=="Holiday",]$Rented.Bike.Count) #trung bình ngày lễ
  noHoliday=mean(df[df$Holiday=="No Holiday",]$Rented.Bike.Count) #trung bình ngày thường
  barplot(c(Holiday,noHoliday),beside=T,col=c("red","blue"),main="Average Rented Bike Count By Holiday",names.arg=c("Holiday","No Hoiday"),ylab="Average Rented Bike Count")#biểu đồ thanh về trung bình lượng xe theo ngày lễ 
}

model<-function(df){
  df=df[,!names(df)%in%c("Wind.speed..m.s.","Rainfall.mm.","Snowfall..cm.","Date","Functioning.Day","Dew.point.temperature..C.")]#loại biến 

  set.seed(1)
  split= sample(c(rep(0, 0.7 * nrow(df)), rep(1, 0.3 * nrow(df)))) #chia tập dữ liệu 
  train_data=df[split==0,]
  test_data=df[split==1,] 
  
  model1=lm(log(Rented.Bike.Count)~.,data=train_data)#mô hình 1
  summary(model1)
  
  

  train_data=train_data[,!names(train_data)%in%c("Visibility..10m.")]  #loại biến 
  test_data=test_data[,!names(test_data)%in%c("Visibility..10m.")]
  
  model2=lm(log(Rented.Bike.Count)~.,data=train_data)
  summary(model2)#mô hình 2
  
  anova(model1,model2)#so sánh 2 mô hình 
  
  train_error=regressionMetrics(trues=train_data$Rented.Bike.Count,preds=exp(predict(model2,newdata=train_data))) #sai số của mô hình theo tập train 
  test_error=regressionMetrics(trues=test_data$Rented.Bike.Count,preds=exp(predict(model2,newdata=test_data)))#sai số của mô hình theo tập test
  print(train_error)
  print(test_error)
  
  par(mfrow=c(2,2))
  plot(model2) #các biểu đồ kiểm tra giả định mô hình 
  
  cmp=data.frame(truth_value=test_data$Rented.Bike.Count,
                 predict_value=exp(predict(model2,newdata=test_data))) #so sánh giá trị thực tế so với giá trị dự đoán 
}

hd2<-function(){
  df=read.csv("SeoulBikeData.csv")
  head(df,10)
  apply(is.na(df),2,sum) #kiểm tra dữ liệu khuyết 
  summary(df) #thống kê mô tả 
  df=df[df$Functioning.Day=="Yes",] #loại hàng dữ liệu của những ngày không hoạt động 
  rented_bike_count(df)
  hour(df)
  correlation(df)
  weather(df)
  season(df)
  holiday(df)
  model(df)
}

hd1<-function(){
  diem_so <- read.csv("diem_so.csv") #Doc du lieu
  head(diem_so,10) #xem 10 dong dau tien
  new_DF <- diem_so[,c("G1","G2","G3","studytime","failures","absences","paid","sex")] # trich cac bien
  head(new_DF,10) #xem 10 dong dau tien
  
  apply(is.na(new_DF),2,which) #kiem tra va xuat vị tri du lieu khuyet (NA)
  apply(is.na(new_DF),2,sum) #kiem tra va dem gia tri NA
  apply(is.na(new_DF),2,mean) #ti le NA
  new_DF <- na.omit(new_DF)
  apply(is.na(new_DF),2,sum) #kiem tra lai NA
  head(new_DF,10) #xem 10 dong dau tien
  
  mean <- apply(new_DF[,c("G1","G2","G3")],2,mean) #tinh trung binh mau
  sd <- apply(new_DF[,c("G1","G2","G3")],2,sd) #tinh do lech chuan hieu chinh
  Q1 <- apply(new_DF[,c("G1","G2","G3")],2,quantile,probs=0.25) #tinh diem phan vi 1
  median <- apply(new_DF[,c("G1","G2","G3")],2,median) #tinh trung vi
  Q3 <- apply(new_DF[,c("G1","G2","G3")],2,quantile,probs=0.75) #tinh diem phan vi 3
  min <- apply(new_DF[,c("G1","G2","G3")],2,min) #tinh gia tri nho nhat
  max <- apply(new_DF[,c("G1","G2","G3")],2,max) #tinh gia tri lon nhat
  t(data.frame(mean, sd, Q1, median, Q3, min, max)) #tao bang voi cac bien
  
  table(new_DF$studytime) #lap bang thong ke so luong cho cac phan loai
  table(new_DF$failures) #lap bang thong ke so luong cho cac phan loai
  table(new_DF$paid) #lap bang thong ke so luong cho cac phan loai
  table(new_DF$sex) #lap bang thong ke so luong cho cac phan loai
  
  hist(new_DF$G3,main ="Đồ thị phân bố tần số điểm thi cuối khoá G3",xlab ="Điểm thi G3",ylab ="Tần số (Số học sinh)",label =T,ylim=c(0,90),col ="royalblue")
  boxplot(G3~studytime,data=new_DF,main="Biểu đồ hộp thể hiện phân phối điểm thi theo thời gian tự học",xlab="Thời gian tự học", ylab="Điểm thi G3", col=c(2,3,4,7))
  boxplot(G3~failures,data=new_DF,main="Biểu đồ hộp thể hiện phân phối điểm thi theo số lần không qua môn",xlab="Số lần không qua môn", ylab="Điểm thi G3",col=c(2,3,4,7))
  boxplot(G3~paid,data=new_DF,main="Biểu đồ hộp thể hiện phân phối điểm thi theo phân loại tham gia các lớp học thêm môn Toán ngoài trường",xlab="Tham gia các lớp học thêm môn Toán ngoài trường", ylab="Điểm thi G3",col=c(3,7))
  boxplot(G3~sex,data=new_DF,main="Biểu đồ hộp thể hiện phân phối điểm thi theo giới tính",xlab="Giới tính", ylab="Điểm thi G3",col=c(2,4))
  
  par(mfrow=c(1,3)) #Xep 3 bieu do vao 1 hang
  plot(G3~G1,data=new_DF,main ="G1 and G3",col =4,pch=16) #Ve do thi phan tan cua G3 theo G1
  plot(G3~G2,data=new_DF,main ="G2 and G3",col=2,pch=16) #Ve do thi phan tan cua G3 theo G2
  plot(G3~absences,data=new_DF,main ="absences and G3",col=1,pch=16) #Ve do thi phan tan cua G3 theo absences
  
  model_1 <- lm(G3~G1 + G2 + studytime + failures + absences +paid + sex,new_DF) #Xay dung mo hinh 1 va luu voi ten model_1
  summary(model_1) #Ket qua mo hinh 
  model_2 <- lm(G3~G1 + G2 + absences,new_DF) #Xay dung mo hinh 2 da loai bo bien studytime, failures, paid, sex tu mo hinh 1 va luu voi ten model_2
  summary(model_2) #Ket qua mo hinh 2
  
  anova(model_1,model_2) #So sanh mo hinh 1 va mo hinh 2
  
  par(mfrow = c(2, 2)) #xep 4 bieu do thanh 2 hang 2 cot
  plot(model_2) #ve do thi phan tich thang du
  
  X = data.frame("G1"= 15,"G2"= 12,"absences"= 2)
  predict(model_2,X,interval="confidence")  #thuc hien du bao cho bien X bang model_2
}

main(){
  hd1()
  hd2()
}