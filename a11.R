# 加载必要的库
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)
library(leaflet)
library(ggplot2)
library(gifski)

#1.
# 数据导入
hefei <- read.delim("D:/学习/NTU/应用经济学的大数据/data/geo/hefei.txt")

# 将数据转换为sf对象
coordinates(hefei) <- ~lng+lat

# 假设你有一个开发区的边界数据集
# 这里需要替换成真实的边界数据
development_zones <- st_read("D:/学习/NTU/应用经济学的大数据/data/geo/data/G341022合肥经济技术开发区.txt")

# 计算开发区内的企业数量
within_development_zone <- st_join(data, development_zones, join = st_within)

# 计算开发区周边不同半径内的企业数量
buffers_km <- st_buffer(development_zones, dist = c(1000, 3000, 5000))
count_in_buffers <- lapply(buffers_km, function(buffer) st_join(data, buffer, join = st_within))


#2
# 绘制地图
tm_shape(development_zones) + 
  tm_borders() +
  tm_shape(data) +
  tm_dots(col = "red", size = 0.5) +
  tm_layout(title = "企业分布")

# 或者使用leaflet创建交互式地图
leaflet(data) %>% 
  addTiles() %>% 
  addMarkers(~lng, ~lat, popup = ~pripid)


#3
srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
srtm

# Check the original resolution
res(srtm)

# Define the new resolution
new_res <- c(0.01, 0.01)

# Resampling using different methods
srtm_nearest <- resample(srtm, res=srtm, method="near")
srtm_bilinear <- resample(srtm, res=srtm, method="bilinear")
srtm_cubic <- resample(srtm, res=srtm, method="cubic")
srtm_average <- resample(srtm, res=srtm, method="average")
srtm_mode <- resample(srtm, res=srtm, method="mode")

# Create a plot layout
par(mfrow=c(3,2))

# Plot the original and resampled rasters
plot(srtm, main="Original")
plot(srtm_nearest, main="Nearest Neighbor")
plot(srtm_bilinear, main="Bilinear")
plot(srtm_cubic, main="Cubic")
plot(srtm_average, main="Average")
plot(srtm_mode, main="Mode")

# Reset plot layout
par(mfrow=c(1,1))

#4
