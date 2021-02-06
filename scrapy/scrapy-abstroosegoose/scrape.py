import scrapy
import logging
from scrapy.shell import inspect_response

class AbstrooseSpider(scrapy.Spider):
    name = 'abstroose_gose'
    start_urls = ['https://abstrusegoose.com/1']

    def parse(self, response):
        for title in response.css('h1.storytitle'):
            yield {
                'title': title.css('::text').get(),
                'image': response.css('html body section img::attr(src)').get()
            }


        next_links = response.xpath("//a[contains(.//text(), 'Next')]")
        if len(next_links) == 0:
            # inspect_response(response, self)

        for next_page in next_links:
            yield response.follow(next_page, self.parse)
