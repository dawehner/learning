# Sleep TS

Usage:

```
import * as sleep from 'sleep';

sleep.start(sleep.milliseconds(10))
  .then(() => console.log(123));

sleep.start(sleep.tillSuccess(() => true))
  .then(() => console.log(123));
```