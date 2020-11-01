/** A default TextEncoder instance */
export const encoder = new TextEncoder();
/** Shorthand for new TextEncoder().encode() */
export function encode(input) {
    return encoder.encode(input);
}
/** A default TextDecoder instance */
export const decoder = new TextDecoder();
/** Shorthand for new TextDecoder().decode() */
export function decode(input) {
    return decoder.decode(input);
}
//# sourceMappingURL=file:///Users/dawehner/Documents/Projects/learning/comic-server/.deno/gen/https/deno.land/std@0.52.0/encoding/utf8.ts.js.map