$(function() {
  d3.json("/tutorials", function(error, events) {
    d3.select("svg").selectAll("image.tutorial").data(events).enter()
      .append("a")
      .attr("xlink:href", function(d) {return "/tutorials/" + d.id;})
      .append("image")
      .attr("class", "tutorial")
      .attr("xlink:href", function(d) {return "/img/images.jpeg";})
      .attr("width",60).attr("height",60)
      .attr("x", function(d) {return d.x * 100 + 20;})
      .attr("y", function(d) {return d.y * 50 + 20;});
  });

  d3.select("svg")
    .on("click", function() {
      var click = d3.mouse(this);
      var x = Math.floor(click[0]/100);
      var y = Math.floor((click[1] - 25)/50);

      $.post("/tutorials", {"new.x": x , "new.y": y, "new.title": "nope"}, function() {
	window.location.reload();
      });
    })
    .on("mousemove", function() {
      var click = d3.mouse(this);
      var x = Math.floor(click[0]/100);
      var y = Math.floor((click[1] - 25)/50);

      d3.select("svg").selectAll("image.feedback").remove();
      d3.select("svg")
	.append("image")
	.attr("class", "feedback")
	.attr("xlink:href", "/img/images.jpeg")
	.attr("width", 60).attr("height", 60)
	.attr("x", x * 100 + 20)
	.attr("y", y * 50 + 20)
	.attr("opacity", 0.5);
    });
});
