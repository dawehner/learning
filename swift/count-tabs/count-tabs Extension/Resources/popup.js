console.log("Hello World2!", browser);


const tabs = await browser.tabs.query({});
const count = tabs.length;
console.log({count}, browser);


