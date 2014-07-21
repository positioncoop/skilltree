$(function() {
  d3.json("/tutorials", function(error, events) {
    d3.select("svg").selectAll("image.tutorial").data(events).enter()
      .append("a")
      .attr("xlink:href", function(d) {return "/tutorials/" + d.id;})
      .append("image")
      .attr("class", "tutorial")
      .attr("xlink:href", function(d) {return "/img/images.jpeg";})
      .attr("width",60).attr("height",60)
      .attr("x", function(d) {return d.x;})
      .attr("y", function(d) {return d.y;});
  });

  d3.select("svg")
    .on("click", function() {
      var click = d3.mouse(this);
      $.post("/tutorials", {"new.x": click[0] - 30, "new.y": click[1] - 30, "new.title": "nope"}, function() {
	window.location.reload();
      });
  });
});
