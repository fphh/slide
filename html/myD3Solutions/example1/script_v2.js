
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

var backgroundRect = svg
    .append("rect")
    .attr("width", "100%")
    .attr("height", "100%")
    .attr("fill", "transparent");

var circles = svg.selectAll("circle")
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

var tooltip = svg
    .append("g")
    .attr("translate", translate(0, 0))
    .style("opacity", 0);

var rect = tooltip
    .append("rect")
    .style("fill", "#eeffaa")
    .style("stroke", "#000000")
    .style("stroke-width", 1)
    .style("shape-rendering", "crispEdges");

var text = tooltip
    .append("text")
    .style("fill", "#000000");

circles.on("mouseenter", function (d) {
    var evt = d3.mouse(this);
    var x = evt[0];
    var y = evt[1];
    var n = 4;
    var offset = 20;

    text.text(d);

    var bbox = text.node().getBBox();

    rect.attr("x", bbox.x-n)
	.attr("y", bbox.y-n)
	.attr("width", bbox.width+n*2)
	.attr("height", bbox.height+n*2);

    tooltip
	.attr("transform", translate(x+offset, y-offset))
	.style("opacity", 1);

});

circles.on("mouseleave", function (d) {
    tooltip
	.style("opacity", 0);
});

tooltip.on("mouseenter", function () {
    tooltip.style("opacity", 1);
});

backgroundRect.on("mouseenter", function () {
    tooltip.style("opacity", 0)
	.attr("transform", translate(-100, -100));
});
