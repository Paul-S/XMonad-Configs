#!/usr/bin/env python3.6
from bs4 import BeautifulSoup as bs
import datetime
import requests
import logging


# logging.basicConfig(level=logging.DEBUG, format='%(message)s %(asctime)s')
weather_page = 'https://www.metoffice.gov.uk/public/weather/forecast/gcjnp69fg'

html = requests.get(weather_page)
bsObj = bs(html.content, 'html5lib')


def main(bsObj):
    get_forecasts_and_print(bsObj)


def get_dates_and_format(bsObj):
    """
    Scrapes dates from MetOffice website
    for each day and formats them to days
    of the week

    """
    datetime_tag = bsObj.find(id='dayNav')
    date_tag = datetime_tag.find_all({'time': 'datetime'})
    raw_dates = [tag.get('datetime')[:10] for tag in date_tag]
    dates_to_convert = [datetime.datetime.strptime(
        raw_date, '%Y-%m-%d') for raw_date in raw_dates]
    final_dates = [date.strftime('%A') for date in dates_to_convert]

    return final_dates


def get_forecasts_and_print(bsObj):
    """
    Scrapes forecasts from the weather icons
    on the MetOffice website and re-formats
    them to more pleasing descriptions, then
    prints them all in one long-ass string

    """ 
    final_dates = get_dates_and_format(bsObj)
    forecast_tag = bsObj.find(id='dayNav')
    data = forecast_tag.find_all({'img': 'icon wx'})
    forecasts = [tag.get('title') for tag in data]
    forecasts = [string.replace('Sunny day', 'Sunny') for string in forecasts]
    forecasts = [string.replace('Light shower day', 'Light showers')
                 for string in forecasts]
    forecasts = [string.replace('Heavy shower day', 'Heavy showers')
                 for string in forecasts]
    zipped = [f'{day} - {forecast}' for day, forecast
              in zip(final_dates, forecasts)]
    long_as_fuck_string = '  '.join(zipped)

    print(long_as_fuck_string)


if __name__ == '__main__':
    main(bsObj)
