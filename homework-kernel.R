# Load necessary package
library(ks)
library(viridis)

# . Estimate the joint probability density using kernel density estimation.
# 1. Load the data
data <- read.csv("/Users/macc/Downloads/train.csv")

# Remove rows where Age or Fare is missing
data_clean <- data[complete.cases(data[c("Age", "Fare")]), ]

# Create a matrix from Age and Fare columns
kde_data <- cbind(data_clean$Age, data_clean$Fare)

# Compute an optimal bandwidth matrix using smoothed cross-validation
H <- Hscv(kde_data)

# Estimate the joint density using the ks package
kde_result <- kde(x = kde_data, H = H, gridsize = c(50, 50))


# b. Visualize using an Image (Heat) Plot with a Viridis Color Palette
image(x = kde_result$eval.points[[1]], 
      y = kde_result$eval.points[[2]], 
      z = kde_result$estimate,
      col = viridis(20), 
      xlab = "Age", 
      ylab = "Fare",
      main = "Image Plot of Joint Density (Age vs. Fare)")

# Overlay the original data points to add context
points(kde_result$x, pch = 20, col = "white")

#3d plot
persp(kde_result$eval.points[[1]], 
      kde_result$eval.points[[2]], 
      kde_result$estimate, 
      theta = 30,        # Rotation angle (degrees)
      phi = 30,          # Elevation angle (degrees)
      expand = 0.5,      # Scaling factor for the z-axis
      col = "lightblue", # Color for the surface
      xlab = "Age", 
      ylab = "Fare", 
      zlab = "Density",
      main = "3D  Plot of Joint Density (Age vs. Fare)")
#Slice plot
plot(kde_result, display="filled.contour2")

#Visualize using a Contour Plot
contour(x = kde_result$eval.points[[1]], 
        y = kde_result$eval.points[[2]], 
        z = kde_result$estimate,
        xlab = "Age", 
        ylab = "Fare",
        main = "Contour Plot of Joint Density (Age vs. Fare)")
