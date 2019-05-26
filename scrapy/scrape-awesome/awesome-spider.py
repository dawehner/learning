import scrapy
import mistune
import re
from scrapy.linkextractors import LinkExtractor
from scrapy.http import TextResponse


class AwesomeSpider(scrapy.Spider):
    name = "awesome spider"

    start_urls = ['https://raw.githubusercontent.com/sindresorhus/awesome/master/readme.md']

    def parse_github_home(self, response):
        links = LinkExtractor(canonicalize=True, unique=True, allow_domains=['github.com'])\
            .extract_links(response)

        readme_links = [link for link in links if 'readme' in link.url or 'README' in link.url]
        for link in readme_links:
            m = re.search('github\.com\/([\w\.\-_]+)\/([\w\.\-_]+)\/blob\/master\/([\w\.\-_]+)', link.url)

            if m is not None:
                username = m.group(1)
                reponame = m.group(2)
                filename = m.group(3)

                url = f'https://raw.githubusercontent.com/{username}/{reponame}/master/{filename}'
                yield scrapy.Request(url, callback=self.parse)
            else:
                self.logger.info('dropped')
                self.logger.info(link.url)

        pass

    def parse(self, response):
        html = mistune.markdown(str(response.body))
        fake_response = TextResponse(response.url, body=html, encoding='utf-8')

        # Tries to find awesome list links
        awesome_list_links = LinkExtractor(canonicalize=True, unique=True, allow_domains=['github.com']).extract_links(fake_response)
        awesome_list_links = [link for link in awesome_list_links if 'awesome' in link.url]
        for link in awesome_list_links:
            yield scrapy.Request(link.url, callback=self.parse_github_home)

        proper_links = LinkExtractor(canonicalize=True, unique=True).extract_links(fake_response)
        proper_links = [link for link in proper_links if 'awesome' not in link.url]

        for link in proper_links:
            yield {
                "awesome-list": response.url,
                "url": link.url,
                "title": link.text,
            }

        pass


