
var centerX = 300;
var centerY = 300;

var rs =
    d3.range(0, 10)
    .map(function () {
	return 20+Math.round(Math.random()*200);
    }).sort(d3.descending);

var color = d3.scale
    .category10()
    .domain([0, rs.length-1]);

var svg = d3.select("#app")
    .append("svg")
    .attr("width", 800)
    .attr("height", 600);

var circles = svg
    .selectAll("circle")
    .data(rs)
    .enter()
    .append("circle")
    .attr("cx", centerX)
    .attr("cy", centerY)
    .attr("r", function (d) { return d; })
    .style("fill", function (d, idx) { return color(idx); });


function translate(x, y) {
    return "translate(" + x + ", " + y + ")";
}
