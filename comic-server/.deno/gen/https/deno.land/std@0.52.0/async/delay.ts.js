// Copyright 2018-2020 the Deno authors. All rights reserved. MIT license.
/* Resolves after the given number of milliseconds. */
export function delay(ms) {
    return new Promise((res) => setTimeout(() => {
        res();
    }, ms));
}
//# sourceMappingURL=file:///Users/dawehner/Documents/Projects/learning/comic-server/.deno/gen/https/deno.land/std@0.52.0/async/delay.ts.js.map