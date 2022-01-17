import elevation
print('Get elevation')
elevation.clip(bounds=[-124.409591, 32.534156, 114.131211, 42.009518], output='data/california-DEM.tif')
print('Elevation finished')
elevation.clean()
