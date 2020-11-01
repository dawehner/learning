export default async (req) => {
    const response = await fetch('https://xkcd.com/rss.xml');
    const content = await response.text();
    console.log(content);
    req.respond({ body: `Hello, from Deno v${Deno.version.deno}!` });
};
//# sourceMappingURL=file:///Users/dawehner/Documents/Projects/learning/comic-server/.deno/gen/file/Users/dawehner/Documents/Projects/learning/comic-server/api/comics.ts.js.map