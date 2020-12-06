import json
import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from recipe_scrapers import scrape_me
import sys

import logging
	
class HelloFreshSpider(CrawlSpider):
    name = 'hellofresh-spider'

    start_urls = ['https://www.hellofresh.co.uk/recipes/most-popular-recipes?page=%s' % page for page in range(1, 2)]

    rules = (
        # Extract links matching 'category.php' (but not matching 'subsection.php')
        # and follow links from them (since no callback means follow=True by default).
        Rule(LinkExtractor(allow=('recipes', )), callback='parse_recipe', follow=False),
    )

    def parse_recipe(self, response):
        try:
            scraper = scrape_me(response.url)

            yield {
                "title": scraper.title(),
                "total_time": scraper.total_time(),
                "yields": scraper.yields(),
                "ingredients": scraper.ingredients(),
                "instructions": scraper.instructions(),
                "image": scraper.image(),
                "host": scraper.host(),
                "links": scraper.links()
            }
        except:
            logger.error("Error during scraping")

