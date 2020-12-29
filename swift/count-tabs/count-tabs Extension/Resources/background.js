console.log(123);

browser.alarms.create('count-tabs', {
    when: 0,
    periodInMinutes: 1,
});

browser.alarms.onAlarm.addListener(async (alarm) => {
    const tabs = await browser.tabs.query({});
    const count = tabs.length;
    console.log({count});
});

browser.runtime.onMessage.addListener((request, sender, sendResponse) => {
    console.log("Received request: ", request);

    if (request.greeting === "hello")
        sendResponse({ farewell: "goodbye" });
});
