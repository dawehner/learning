import scrapy

class HackagePackage(scrapy.Item):
    name = scrapy.Field()
    description = scrapy.Field()
    link = scrapy.Field()

    def __init__(self, name: str, description: str, link: str) -> None:
      self.item['name'] = name
      self.item['description'] = description
      self.item['link'] = link


class HackageSpider(scrapy.Spider):
    name = 'hackagespider'
    start_urls = ['https://hackage.haskell.org/packages/browse']

    def parse_package_page(self, response):
        name = response.css('h1 a::text').get()
        description = response.css('h1 small::text').get()
        link = response.css('h1 a').attrib['href']

        package = HackagePackage(
          name=name,
          description=description,
          link=link
          )

        yield package

    def parse(self, response):
        for link in response.xpath('//table/tbody/tr/td[1]/a'):
            yield response.follow(link, self.parse_package_page)
