var sum = process.argv.slice(2)
  .map((i) => { return parseInt(i)})
  .reduce((acc, b) => {return acc + b}, 0)

console.log(sum);
