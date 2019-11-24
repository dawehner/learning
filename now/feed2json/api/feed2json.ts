import { NowRequest, NowResponse } from '@now/node';
import RssParser from 'rss-parser';
import { type } from 'os';

export default async (_req: NowRequest, res: NowResponse) => {
  const url = _req.query.url;

  const rssParser = new RssParser();
  try {
    if (Array.isArray(url)) {
      const feeds = await Promise.all(url.map(url => rssParser.parseURL(url)));
      return res.status(200).json(feeds);
    }
    else {
      const feed = await rssParser.parseURL(url);
      return res.status(200).json(feed);
    }
  }
  catch (e) {
    console.error(e);
    return res.status(500).send('Error while parsing feed.');
  }
};
