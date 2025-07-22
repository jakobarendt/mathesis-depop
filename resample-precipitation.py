import cdstoolbox as ct


@ct.application(title='Resample and Download Weather Grid(s)')
@ct.output.download()
def download_resample_precipitation_grid(placeholder_var):

    precipitation = ct.catalogue.retrieve(
        'insitu-gridded-observations-europe',
        {
            # 'format': 'zip',
            'variable': 'precipitation_amount',
            'grid_resolution': '0.1deg',
            'period': 'full_period',
            'version': '29.0e',
            'product_type': 'ensemble_mean',
        }
    )

    # Aggregate precipitation to yearly sum
    yearly_total_precipitation = ct.cube.resample(
        precipitation,
        freq='year', how='sum')
    
    # Return the new grid
    return yearly_total_precipitation

# add again variables 'maximum_temperature', 'minimum_temperature', 'relative_humidity', 'surface_shortwave_downwelling_radiation'
