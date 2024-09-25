#(2)

# Load dataset
data(mtcars)

#View dataset
head(mtcars)
dim(mtcars)

# Remove mpg from the dataset
X <- mtcars[, -1]
Y <- mtcars[, 1]


# Perform PCA on the unscaled data
pca_unscaled <- prcomp(X, scale = FALSE)


# Perform PCA on the scaled data
pca_scaled <- prcomp(X, scale = TRUE)

#Get eigenvectors
R_unscaled = pca_unscaled$rotation

#Get eigenvectors
R_scaled = pca_scaled$rotation


#Print first eigenvectors for each case
R_unscaled[,1]
#cyl          disp           hp         drat           wt         qsec           vs           am         gear         carb 
#0.012042615  0.900235270  0.435074057 -0.002661394  0.006242550 -0.006676533 -0.002731293 -0.001963245 -0.002606103  0.005767541 


#names(sort(abs(R_unscaled[,1]), decreasing = TRUE)[1:8])


R_scaled[,1]
#cyl         disp         hp       drat         wt       qsec         vs         am       gear       carb 
#0.4029711  0.3959243  0.3543255 -0.3155948  0.3668004 -0.2198982 -0.3333571 -0.2474991 -0.2214375  0.2267080 

#names(sort(abs(R_scaled[,1]), decreasing = TRUE)[1:8])


#(3)
# View the results
summary(pca_scaled)
plot(pca_scaled)
#The first PC accounts for over 57.6% of the total variance explained.

R_scaled[,1]

#View the names of the five most important variables for the first  PC
rownames(R_scaled)[order(abs(R_scaled)[,1],decreasing =  TRUE)][1:5]
#"cyl"  "disp" "wt"   "hp"   "vs"


#The highest five loadings on PC1 are from cyl, disp, wt, hp and vs


#(4)

#Fit regression model
lm.o = lm(mpg ~ cyl+disp+wt+hp+vs, data = mtcars)  

summary(lm.o)
#wt is a significant predictor. The others are not.
