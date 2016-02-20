

var cocktails = [ "Bloody Mary", "Mojito", "White Russian", "Pina Colada", "Tequila Sunrise", "Moscow Mule", "Margarita"];

var maxN = 10;

function updateData () {
    return cocktails.map(function (x) {
        var n = Math.floor(Math.random()*maxN);

        return {
            cocktail: x,
            number: n <= 3 ? 0 : n
        };
    }).filter(function (d) {
	return d.number !== 0;
    }).sort(function (a, b) {
	return b.number - a.number;
    });
}

function translate(x, y) {
    return "translate(" + x + ", " + y + ")";
}
function key(d) {
    return d.cocktail;
}

var data = updateData();

var height = 400;
var width = 800;

var margin = 80;


var svg = d3.select("#app")
    .append("svg")
    .attr("width", width+2*margin)
    .attr("height", height+2*margin);


var inner = svg
    .append("g")
    .attr("transform", translate(margin, margin));

var x = d3.scale.ordinal()
    .rangeRoundBands([0, width], 0.1)
    .domain(data.map(key));

var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom");

inner.append("g")
    .attr("class", "x axis")
    .attr("transform", translate(0, height))
    .call(xAxis);

var y = d3.scale.linear()
    .range([height, 0])
    .domain([0, maxN]);

var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left");

inner.append("g")
    .attr("class", "y axis")
    .call(yAxis);

inner
    .selectAll(".bar")
    .data(data, key)
    .enter()
    .append("rect")
    .attr("class", "bar")
    .attr("x", function(d) { return x(d.cocktail); })
    .attr("width", x.rangeBand())
    .attr("y", function(d) { return height-y(d.number); })
    .attr("height", function(d) { return y(d.number); });



var dur = 1000;

function update() {

    var newData = updateData();
    x.domain(newData.map(key));


    var bars = inner
	.selectAll(".bar")
	.data(newData, key);

    var t0 = d3.transition().duration(dur).each(function () {
	bars.exit()
	    .transition()
	    .attr("y", height)
	    .attr("height", 0)
	    .remove();
    });



    var t1 = t0.transition().duration(dur).each(function () {
	inner.select(".x.axis").call(xAxis);

	bars.transition()
	    .attr("x", function(d) { return x(d.cocktail); })
	    .attr("width", x.rangeBand());
    });

    var t2 = t1.transition().duration(dur).each(function () {
	bars.enter()
	    .append("rect")
	    .attr("class", "bar")
	    .attr("x", function(d) { return x(d.cocktail); })
	    .attr("width", x.rangeBand())
	    .attr("y", height)
	    .attr("height", 0);


	bars.transition()
	    .attr("y", function(d) { return y(d.number); })
	    .attr("height", function(d) { return height - y(d.number); });
    });
}
