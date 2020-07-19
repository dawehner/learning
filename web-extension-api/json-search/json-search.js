/*
Just draw a border round the document.body.
*/


console.log(document.body);

try {
  const content = JSON.parse(document.body.innerHTML)
  const app = Elm.Main.init({ node: document.body });
}
catch (e) {
  // Invalid json, nothing for us to do.
}
