$(function() {
  d3.json("/tutorials", function(error, events) {
    gs = d3.select("svg").selectAll("image.tutorial").data(events).enter().append("g")
      .attr("transform", function(d) {
	var dx = d.x * 100 + 20;
	var dy = d.y * 50 + 20;
	return "translate(" + dx + ", " + dy + ")";
      });

    as = gs.append("a")
      .attr("xlink:href", function(d) {return "/tutorials/" + d.id + "/edit";});

    imgs = as.append("image")
      .attr("class", "tutorial")
      .attr("xlink:href", function(d) {return d.iconPath || "/img/example.png";})
      .attr("width",60).attr("height",60);

    as.append("text")
      .attr("dx", 5)
      .attr("dy", 72)
      .text(function(d) { return d.title });
  });

  d3.select("svg")
    .on("click", function() {
      var click = d3.mouse(this);
      var x = Math.floor(click[0]/100);
      var y = Math.floor((click[1] - 25)/50);

      $.post("/tutorials/new", {"new.x": x , "new.y": y, "new.title": "nope"}, function() {
	window.location.reload();
      });
    })
    .on("mousemove", function() {
      var point = d3.mouse(this);
      var x = Math.floor(point[0]/100);
      var y = Math.floor((point[1] - 25)/50);

      feedback.attr("x", x * 100 + 20) .attr("y", y * 50 + 20);
    });

  var feedback = d3.select("svg")
      .append("image")
      .attr("class", "feedback")
      .attr("xlink:href", "/img/example.png")
      .attr("width", 60).attr("height", 60)
      .attr("x", -100)
      .attr("y", -100)
      .attr("opacity", 0.5);
});
