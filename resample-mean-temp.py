import cdstoolbox as ct


@ct.application(title='Resample and Download Weather Grid(s)')
@ct.output.download()
def download_resample_mean_temp_grid(placeholder_var):

    mean_temperature = ct.catalogue.retrieve(
        'insitu-gridded-observations-europe',
        {
            # 'format': 'zip',
            'variable': 'mean_temperature',
            'grid_resolution': '0.1deg',
            'period': 'full_period',
            'version': '29.0e',
            'product_type': 'ensemble_mean',
        }
    )

    # Aggregate mean temperatures to yearly average
    yearly_mean_temperature = ct.cube.resample(
        mean_temperature,
        freq='year', how='mean')

    # Return the new grid
    return yearly_mean_temperature

# add again variables 'maximum_temperature', 'minimum_temperature', 'relative_humidity', 'surface_shortwave_downwelling_radiation'
