#1 ĐỌc dữ liệu

BTLXS <- read.csv("C:/Users/USER/Downloads/archive/data.csv")
head(BTLXS,5)

#3 Xem data thuộc kiểu dữ liệu gì.
str(BTLXS)
#4 Trích ra một tệp tin con bao gồm dữ liệu sẽ tính toán 

new_BTLXS <- BTLXS[,c("layer_height","infill_density","nozzle_temperature", "print_speed",
                      "material","roughness","tension_strenght","elongation")]
head(new_BTLXS,5)

#5 Làm sạch dữ liệu
#5.1 Kiểm tra có dữ liệu bị khuyết hay không, đề xuất hướng xử lý
is.na(new_BTLXS) #Không sử dụng
#Trong đó na là dữ liệu khuyết is.na là có dữ liệu khuyết hay không TRUE là có FALSE là không 

library(ggplot2)
library(naniar)

apply(is.na(new_BTLXS),2,sum) 
miss_var_table(new_BTLXS)

#2 là hàng, 1 là cột
#5.2 Kiểm tra định dạng biến là số hay là chữ

str(new_BTLXS)

is.numeric(new_BTLXS$layer_height) #kiểm tra định dạng số 
is.character((new_BTLXS$infill_pattern)) #kiểm tra định dạng chữ
#Nếu chưa phải là định dạng chính xác thì
new_BTLXS$layer_height <- as.numeric(new_BTLXS$layer_height)
new_BTLXS$infill_pattern <- as.character(new_BTLXS$infill_pattern)
#6 Tính thống kê mô tả
summary(new_BTLXS)
#hoặc
so_new_BTLXS <- new_BTLXS[,c( "layer_height", "infill_density" ,"nozzle_temperature",
                               "print_speed","roughness","tension_strenght","elongation")]
head(so_new_BTLXS,5)

n = apply(so_new_BTLXS,2,length)
mean = apply(so_new_BTLXS,2,mean)
sd = apply(so_new_BTLXS,2,sd)
median = apply(so_new_BTLXS,2,median)
min = apply(so_new_BTLXS,2,min)
max = apply(so_new_BTLXS,2,max)
des=data.frame(n,mean,sd,median,min,max)
Q= apply(so_new_BTLXS,2,function(x){c(Q1= quantile(x,probs=0.25),Q3=quantile(x,probs=0.75))})
t(des)
Q
#6.1 Thống kê số lượng biến phân loại
  
table(new_BTLXS[,"material"])

#6.2 Vẽ đồ thị phân bố tần số     
hist(new_BTLXS[,"roughness"],xlab="Độ nhám",ylab="Tần số",
     main="Đồ thị phân phối tần số cho độ nhám của bản in",col=c("#009999", "#0000FF"),
     labels=T,ylim =c(0,12))
# Vẽ boxplot (biến chính theo biến định lượng) 
boxplot(tension_strenght~layer_height,xlab="layer height",ylab="tension strength",data=new_BTLXS,
        main="Đồ thị boxplot của sức căng theo vật liệu", col=c("#009999", "#0000FF"))
# Vẽ boxplot (biến chính theo biến phân loại) 
boxplot(tension_strenght~material,ylab="tension strength",data=new_BTLXS,
        main="Đồ thị boxplot của sức căng theo vật liệu", col=c("#009999", "#0000FF"))

# Đồ thị phân tán (biến chính theo biến định lượng)
plot(tension_strenght~layer_height,data=new_BTLXS,
     main="Sức căng theo chiều cao lớp",col=c("#009999","#0000FF"),pch=19)

#6.3 Vẽ đồ thị ma trận tương quan(Chỉ sử dụng biến định lượng)
M=cor(so_new_BTLXS)
corrplot(M)
# Hoặc 
cor(so_new_BTLXS)
#Nếu r = 1: tương quan tuyến tính tuyệt đối, khi biểu diễn trên đồ thị phân tán Scatter như hình vẽ ở trên, các điểm biểu diễn sẽ nhập lại thành 1 đường thẳng.
#Nếu r = 0: không có mối tương quan tuyến tính. Lúc này sẽ có 2 tình huống xảy ra. Một,không có một mối liên hệ nào giữa 2 biến. Hai,giữa chúng có mối liên hệ phi tuyến.
# Nếu hệ số tương quan=1 phải loại bỏ 1 trong 2 biến tùy ý
#6.4 So sánh anova 2 yếu tố layer height và infill density.
anova_2 <- aov(tension_strenght ~ layer_height * infill_density, data = new_BTLXS)
summary(anova_2)

#7 Hồi quy đa biến
#7.1 Xây dựng mô hình hồi quy

hq_1 <-lm(tension_strenght~layer_height+infill_density+nozzle_temperature+print_speed
             +material,new_BTLXS)
summary(hq_1)
#Phương trình hồi quy=B0 +layer_height.B1+ infill_density.B2 + nozzle_temperature.B3 + print_speed.B4+...+ μ_i
#Kiểm định các hệ số hồi quy
#Đánh giá từng biến,xem X nào ảnh hưởng đến Y
#H0: B1=0 (Hệ số không có ý nghĩa)
#H1: B1 khác 0 (Hệ số có ý nghĩa)
#pvalue(Pr(>|t|))<mức ý nghĩa(mặc định 5%) => bác bỏ H0 biến X1 có ảnh hưởng đến Y ,ngược lại không ảnh hưởng
#7.3 Xây dựng mô hình mới loại bỏ các biến không có ý nghĩa
hq_2 <-lm(tension_strenght~layer_height+infill_density+
            nozzle_temperature+material,new_BTLXS)
summary(hq_2)

hq_3 <-lm(tension_strenght~layer_height+infill_density+
            nozzle_temperature,new_BTLXS)
summary(hq_3)
#Nhận xét mô hình 2
#So sánh 2 mô hình
anova(hq_1,hq_2)
#H0: hai mô hình hq_1 và hq_2 giống nhau. 
#H1: Hai mô hình hq_1 và hq_2 khác nhau
anova(hq_2,hq_3)
#H0: Hai mô hình hq_2 và hq_3 giống nhau. 
#H1: Hai mô hình hq_2 và hq_3 khác nhau.
#7.4 Vẽ đồ thị  
plot(hq_1, which=1)
#7.6 Đồ thị Q-Q Residual
plot(hq_1,which=2)
#7.7 Đồ thị Scale location
plot(hq_1,which=3)
#7.8 Đồ thị Residual vs Leverage
plot(hq_1,which=5)
#7.9 Dự báo   
hq_interaction_model <- lm(tension_strenght ~ layer_height * infill_density * nozzle_temperature, data = new_BTLXS)
summary(hq_interaction_model)
#Đồ thị thể hiện độ tương tác giữa 2 biến layer_height và infill_ density.
interaction.plot(new_BTLXS$layer_height, new_BTLXS $infill_density, predict(hq_interaction_model, new_BTLXS), 
                   type="b", legend=F, xlab="Layer Height", ylab="Predicted Tension Strength", 
                   trace.label="Infill Density", main="Interaction Plot: Layer Height vs Infill Density")
#Đồ thị thể hiện độ tương tác giữa 2 biến layer_height và nozzle_temperature.
interaction.plot(new_BTLXS$layer_height,new_BTLXS$nozzle_temperature, predict(hq_interaction_model, new_BTLXS),
                 type="b", legend=F, xlab="Layer Height", ylab="Predicted Tension Strength",
                 trace.label="Nozzle Temperature", main="Interaction Plot: Layer Height vs Nozzle Temperature")
#Đồ thị thể hiện độ tương tác giữa 2 biến nozzle_temperature và infill_density
interaction.plot(new_BTLXS$infill_density,new_BTLXS$nozzle_temperature, predict(hq_interaction_model, BTL),
                 type="b",legend=F,xlab="Infill Density",ylab="Predicted Tension Strength",
                 trace.label="Nozzle Temperature", main="Interaction Plot:Infill Density vs Nozzle Temperature")
#Tạo biến actual
Actual=new_BTLXS$tension_strenght
#Dự đoán tension_strenght 
predicted_values <- predict(hq_interaction_model, new_BTLXS)
#Dự đoán hq_1
predicted_values_hq1 <- predict(hq_1, new_BTLXS)
#Tạo bảng so sánh 
comparison_table <- data.frame(Actual,predicted_values, predicted_values_hq1)
head(comparison_table,10)
#Đồ thị so sánh giữa mô hình actual và 2 mô hình predicted 
ggplot(comparison_table, aes(x = Actual)) +
  geom_point(aes(y = predicted_values_hq1, color = "hq_1"), alpha = 0.5) +
  geom_point(aes(y = predicted_values, color = "hq_interaction_model"), alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Comparison of Two Regression Models",
       x = "Actual",
       y = "Predicted",
       color = "Model") +
  scale_color_manual(values = c("hq_1" = "blue", "hq_interaction_model" = "green")) +
  theme_minimal()





