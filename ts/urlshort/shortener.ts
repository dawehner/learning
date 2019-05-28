import * as express from 'express';
import * as bodyParser from 'body-parser';

interface Shortcut {
  source: FullUrl; // The full original URL
  target: Target; // The shortcut used internally
}

type FullUrl = string;
type Target = string;

function hashString (value: string) : string {
  //console.log(value.split(''));
  return value.split('').reduce((hash, s, i) => {
    const chr = value.charCodeAt(i);
    const hash_ = (((hash << 5) - hash) + chr);
    return hash_;
    // return hash_ | 0;
  }, 0).toString(16);
}

function createShortcut(source : FullUrl) : Shortcut {
  return {
    source,
    target: hashString(source),
  };
}

const shortcuts : Map<Target, Shortcut> = (new Map()) ;

const addShortcut = (shortcut: Shortcut, shortcuts) => {
  shortcuts.set(shortcut.target, shortcut);
};

const app = express();

app.get('/shortcuts', (req, res) => {
  res.json(shortcuts);
})
app.post('/shortcuts', bodyParser.text(), (req, res) => {
  const shortcut = createShortcut(req.body);

  if (shortcuts.has(shortcut.target)) {
    return res.status(208).send(shortcut.target);
  }

  addShortcut(shortcut, shortcuts);
  res.send(shortcut.target);
})
app.get('/shortcut/:shortcut', (req, res) => {
  if (shortcuts.has(req.params['shortcut'])) {
    res.json(shortcuts.get(req.params['shortcut']));
  }

  res.status(404).send();
});

app.listen(3002);