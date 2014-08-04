var to_display = function(p) {
  return {x: p.x * 100 + 20,
	  y: p.y * 50 + 20};
};

var from_mouse = function(mouse) {
  return {x: Math.floor(mouse[0]/100),
	  y: Math.floor((mouse[1] - 25)/50)};
}

$(function() {
  var moveTarget = null;

  d3.json("/tutorials", function(error, events) {
    gs = d3.select("svg").selectAll("image.tutorial").data(events.tutorials).enter()
      .append("g")
      .attr("transform", function(d) {
	var point = to_display(d);
	return "translate(" + point.x + ", " + point.y + ")";
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

    gs.append("text")
      .attr("dx", 58)
      .attr("dy", 40)
      .attr("data-json", function(d) {return JSON.stringify(d);})
      .attr("style","font-size: 25px; font-weight: regular")
      .text("")
      .attr("class", "fa move-icon")
      .on("click", function () {
	moveTarget = $(this).data("json");
	feedback.attr("xlink:href", moveTarget.iconPath || "/img/example.png");
	d3.event.stopPropagation();
      });

    lines = d3.select("svg").selectAll("line.dependency").data(events.dependencies).enter()
      .append("line")
      .attr("x1", function(d) {return to_display(d.source).x;})
      .attr("y1", function(d) {return to_display(d.source).y + 30;})
      .attr("x2", function(d) {return to_display(d.target).x + 60;})
      .attr("y2", function(d) {return to_display(d.target).y + 30;})
      .attr("style", "stroke:rgb(255,0,0);stroke-width:2");
  });

  d3.select("svg")
    .on("click", function() {
      var p = from_mouse(d3.mouse(this));

      if(moveTarget === null) {
	$.post("/tutorials/new", {"new.x": p.x , "new.y": p.y}, function() {
	  window.location.reload();
	});
      } else {
	$.post("/tutorials/" + moveTarget.id + "/move", {"move.x": p.x , "move.y": p.y}, function() {
	  moveTarget = null;
	  feedback.attr("xlink:href", "/img/example.png");
	  window.location.reload();
	});
      }
    })
    .on("mousemove", function() {
      var p = to_display(from_mouse(d3.mouse(this)));

      feedback.attr("x", p.x) .attr("y", p.y);
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
