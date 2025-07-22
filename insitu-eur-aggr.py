import cdstoolbox as ct
import xarray as xr
from . import util

@ct.application()
def calc_yearly_means():
  data = ct.catalogue.retrieve(
    'insitu-gridded-observations-europe',
    {
      'variable': [
            'maximum_temperature', 'mean_temperature', 'minimum_temperature',
            'precipitation_amount', 'relative_humidity', 'surface_shortwave_downwelling_radiation',
        ],
        'grid_resolution': '0.1deg',
        'period': '1950_1964',
        'version': '29.0e',
        'product_type': 'ensemble_mean',
    }
    )
  
  # Aggregate mean temperatures to yearly average
  yearly_mean_temperature = data['mean_temperature'].resample(time='1Y').mean()
  
  # Aggregate precipitation to yearly sum
  yearly_total_precipitation = data['precipitation_amount'].resample(time='1Y').sum()
  
  # Return the new grid
  return {
    xr.Dataset({
      'yearly_mean_temperature': yearly_mean_temperature,
      'yearly_total_precipitation': yearly_total_precipitation,
    })
  }
