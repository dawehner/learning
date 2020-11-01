import parse from "https://denopkg.com/nekobato/deno-xml-parser/index.ts";
export default async (req) => {
    const response = await fetch('https://xkcd.com/rss.xml');
    const content = await response.text();
    const node = parse(content);
    console.log(node);
    req.respond({ body: `Hello, from Deno v${Deno.version.deno}!` });
};
//# sourceMappingURL=file:///Users/dawehner/Documents/Projects/learning/comic-server/.deno/gen/file/Users/dawehner/Documents/Projects/learning/comic-server/api/comics.ts.js.map