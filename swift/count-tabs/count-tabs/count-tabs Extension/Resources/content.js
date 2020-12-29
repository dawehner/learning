browser.runtime.sendMessage({ greeting: "hello" }).then((response) => {
    console.log(123);
    const tabs = browser.tabs.query({});
    console.log(`Tabs count ${tabs}`);
    console.log("Received response: ", response);
});

console.log(124);


debugger;

browser.runtime.onMessage.addListener((request, sender, sendResponse) => {
    console.log("Received request: ", request);
});
