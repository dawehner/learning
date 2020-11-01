// Copyright 2018-2020 the Deno authors. All rights reserved. MIT license.
const isWindows = Deno.build.os == "windows";
export const SEP = isWindows ? "\\" : "/";
export const SEP_PATTERN = isWindows ? /[\\/]+/ : /\/+/;
//# sourceMappingURL=file:///Users/dawehner/Documents/Projects/learning/comic-server/.deno/gen/https/deno.land/std@0.52.0/path/separator.ts.js.map