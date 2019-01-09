library(tuneR)
library(psych)
library(clusterSim)
library(nFactors)
library(caret)
library(e1071)
library(class)
library(pROC)

#extract statistical feaures for birds voices
bird1<-readWave("C:/Users/Desktop/sounds/birds/Bird chirps animals140.wav")
bird2<-readWave("C:/Users/Desktop/sounds/birds/Bird-chirp (Red Lories) animals119.wav")
bird3<-readWave("C:/Users/Desktop/sounds/birds/Crow animals010.wav")
bird4<-readWave("C:/Users/Desktop/sounds/birds/owl animals074.wav")
bird5<-readWave("C:/Users/Desktop/sounds/birds/Vulture animals008.wav")

bird1_data<-bird1@left
bird2_data<-bird2@left
bird3_data<-bird3@left
bird4_data<-bird4@left
bird5_data<-bird5@left

bird_features<-describe(bird1_data)
b1_fft <- fft(bird1_data)
amplitude <- Mod(b1_fft[1:round(length(b1_fft)/2,0)])
b1_fft_feature<-describe(amplitude)
featuredata<-cbind(bird_features[,3:13],b1_fft_feature[,3:13],"bird")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE )

bird_features<-describe(bird2_data)
b2_fft <- fft(bird2_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(bird_features[,3:13],b2_fft_feature[,3:13],"bird")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

bird_features<-describe(bird3_data)
b3_fft <- fft(bird3_data)
amplitude <- Mod(b3_fft[1:round(length(b3_fft)/2,0)])
b3_fft_feature<-describe(amplitude)
featuredata<-cbind(bird_features[,3:13],b3_fft_feature[,3:13],"bird")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

bird_features<-describe(bird4_data)
b4_fft <- fft(bird4_data)
amplitude <- Mod(b4_fft[1:round(length(b4_fft)/2,0)])
b4_fft_feature<-describe(amplitude)
featuredata<-cbind(bird_features[,3:13],b4_fft_feature[,3:13],"bird")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

bird_features<-describe(bird5_data)
b5_fft <- fft(bird5_data)
amplitude <- Mod(b2_fft[1:round(length(b5_fft)/2,0)])
b5_fft_feature<-describe(amplitude)
featuredata<-cbind(bird_features[,3:13],b5_fft_feature[,3:13],"bird")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

#extract statistical feaures for farm animal voices
farm1<-readWave("C:/Users/Desktop/sounds/farm/Cat meow animals020.wav")
farm2<-readWave("C:/Users/Desktop/sounds/farm/Cow animals055.wav")
farm3<-readWave("C:/Users/Desktop/sounds/farm/Dog animals080.wav")
farm4<-readWave("C:/Users/Desktop/sounds/farm/Goat animals115.wav")
farm5<-readWave("C:/Users/Desktop/sounds/farm/Sheep - ewe animals112.wav")

farm1_data<-farm1@left
farm2_data<-farm2@left
farm3_data<-farm3@left
farm4_data<-farm4@left
farm5_data<-farm5@left

farm_features<-describe(farm1_data)
b2_fft <- fft(farm1_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(farm_features[,3:13],b2_fft_feature[,3:13],"farm")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

farm_features<-describe(farm2_data)
b2_fft <- fft(farm2_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(farm_features[,3:13],b2_fft_feature[,3:13],"farm")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

farm_features<-describe(farm3_data)
b2_fft <- fft(farm3_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(farm_features[,3:13],b2_fft_feature[,3:13],"farm")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

farm_features<-describe(farm4_data)
b2_fft <- fft(farm4_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(farm_features[,3:13],b2_fft_feature[,3:13],"farm")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

farm_features<-describe(farm5_data)
b2_fft <- fft(farm5_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(farm_features[,3:13],b2_fft_feature[,3:13],"farm")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

wild1<-readWave("C:/Users/Desktop/sounds/wild/Elephant-angry animals035.wav")
wild2<-readWave("C:/Users/Desktop/sounds/wild/Leopard growl animals089.wav")
wild3<-readWave("C:/Users/Desktop/sounds/wild/Lion growl and snarl animals098.wav")
wild4<-readWave("C:/Users/Desktop/sounds/wild/Lion roar animals103.wav")
wild5<-readWave("C:/Users/Desktop/sounds/wild/Rhinoceros animals134.wav")
wild6<-readWave("C:/Users/Desktop/sounds/wild/Tiger growl animals026.wav")

wild1_data<-wild1@left
wild2_data<-wild2@left
wild3_data<-wild3@left
wild4_data<-wild4@left
wild5_data<-wild5@left
wild6_data<-wild6@left

#extract statistical feaures for wild animal voices
wild_features<-describe(wild1_data)
b2_fft <- fft(wild1_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(wild_features[,3:13],b2_fft_feature[,3:13],"wild")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

wild_features<-describe(wild2_data)
b2_fft <- fft(wild2_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(wild_features[,3:13],b2_fft_feature[,3:13],"wild")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

wild_features<-describe(wild3_data)
b2_fft <- fft(wild3_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(wild_features[,3:13],b2_fft_feature[,3:13],"wild")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

wild_features<-describe(wild4_data)
b2_fft <- fft(wild4_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(wild_features[,3:13],b2_fft_feature[,3:13],"wild")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

wild_features<-describe(wild5_data)
b2_fft <- fft(wild5_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(wild_features[,3:13],b2_fft_feature[,3:13],"wild")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

wild_features<-describe(wild6_data)
b2_fft <- fft(wild6_data)
amplitude <- Mod(b2_fft[1:round(length(b2_fft)/2,0)])
b2_fft_feature<-describe(amplitude)
featuredata<-cbind(wild_features[,3:13],b2_fft_feature[,3:13],"wild")
write.table(featuredata,"C:/Users/Desktop/sounds/featuredata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE )

#import feature data table
featureData<-read.table("C:/Users/Desktop/sounds/featuredata.csv", header = TRUE, sep = ",")
ft<-cbind(scale(featureData[,1:22],center=T),featureData$bird)
#-------------------------------------------------------------------------------------------------------------------
## 75% of the sample size
smp_size <- floor(0.75 * nrow(ft))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(ft)), size = smp_size)

train <- ft[train_ind,1:22 ]
test <- ft[-train_ind,1:22 ]

trainlabel<-ft[train_ind,23 ]
testlabel<-ft[-train_ind,23 ]

#Support Vector Machine for classification
model_svm <- svm(trainlabel ~ train )

#Use the predictions on the data
pred <- round(predict(model_svm, test),0)
confusionMatrix(pred[1:4],testlabel)

#ROC and AUC curves and their plots
roc.multi<-multiclass.roc(testlabel, pred[1:4],levels=c(1, 2, 3))
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
