import { init } from "./base.ts";
const lookup = [];
const revLookup = [];
const code = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
for (let i = 0, l = code.length; i < l; ++i) {
    lookup[i] = code[i];
    revLookup[code.charCodeAt(i)] = i;
}
// Support decoding URL-safe base64 strings, as Node.js does.
// See: https://en.wikipedia.org/wiki/Base64#URL_applications
revLookup["-".charCodeAt(0)] = 62;
revLookup["_".charCodeAt(0)] = 63;
const mod = init(lookup, revLookup);
export const byteLength = mod.byteLength;
export const toUint8Array = mod.toUint8Array;
export const fromUint8Array = mod.fromUint8Array;
//# sourceMappingURL=file:///Users/dawehner/Documents/Projects/learning/comic-server/.deno/gen/https/deno.land/x/base64/mod.ts.js.map