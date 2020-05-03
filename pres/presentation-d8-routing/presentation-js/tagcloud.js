var fill = d3.scale.category20();

var data = {"delivery callback": 23, "access arguments": 790, "page callback": 1041, "theme callback": 34, "weight": 1093, "title": 3749, "title callback": 72, "menu item": 36, "page arguments": 679, "local_actions": 58, "local_tasks": 363, "contextual_links": 33, "file": 3676, "breadcrumb": 2500};
var i = 0;
var word_data = new Array();

for (var property in data) {
  word_data.push({text: property, size: Math.log(data[property]) * 6});
}

d3.layout.cloud().size([600, 600])
  .words(word_data)
  .padding(5)
  .font("Impact")
  .fontSize(function(d) { return d.size; })
  .on("end", draw)
  .start();

function draw(words) {
  d3.select("#tagcloud").append("svg")
    .attr("width", 600)
    .attr("height", 600)
  .append("g")
    .attr("transform", "translate(250,250)")
  .selectAll("text")
    .data(words)
  .enter().append("text")
    .style("font-size", function(d) { return d.size + "px"; })
    .style("font-family", "Inconsolata")
    .style("fill", function(d, i) { return fill(i); })
    .attr("text-anchor", "middle")
    .attr("transform", function(d) {
      return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
    })
    .text(function(d) { return d.text; });
}
