var to_display = function(p) {
  return {x: p.x * 100 + 20,
	  y: p.y * 50 + 20};
};

var from_mouse = function(mouse) {
  return {x: Math.floor(mouse[0]/100),
	  y: Math.floor((mouse[1] - 25)/50)};
}

$(function() {
  d3.json("/tutorials", function(error, data) {
    var enter = d3.select("svg.grid").selectAll("g.tutorial").data(data.tutorials).enter();

    lines = d3.select("svg.grid").selectAll("line.dependency").data(data.dependencies).enter()
      .append("line")
      .attr("x1", function(d) {return to_display(d.source).x;})
      .attr("y1", function(d) {return to_display(d.source).y + 30;})
      .attr("x2", function(d) {return to_display(d.target).x + 60;})
      .attr("y2", function(d) {return to_display(d.target).y + 30;})
      .attr("style", "stroke:rgb(255,0,0);stroke-width:2");
    appendTutorial(enter);
  });

  var moveTarget = null;
  var dependencySource = null;
  var bullseyes = null;
  var appendTutorial = function(enter) {
    group = enter.append("g")
      .attr("class", "tutorial")
      .attr("transform", function(d) {
	var point = to_display(d);
	return "translate(" + point.x + ", " + point.y + ")";
      });

    group.append("image")
      .attr("xlink:href", function(d) {return d.iconPath || "/img/example.png";})
      .attr("width",60).attr("height",60);

    group.append("text")
      .attr("dx", 5)
      .attr("dy", 72)
      .text(function(d) { return d.title });

    group.append("text")
      .attr("dx", -22).attr("dy", 30)
      .attr("style","font-size: 18px; font-weight: regular")
      .text("")
      .attr("data-json", function(d) {return JSON.stringify(d);})
      .attr("class", "fa fa-star-o")
      .on("click", function () {
	dependencySource = $(this).data("json");
	feedback.attr("xlink:href", "/img/left-arrow.jpg");
	bullseyes.style("opacity", 1);
	d3.event.stopPropagation();
      });

    bullseyes = group.append("text")
      .attr("dx", 60).attr("dy", 50)
      .attr("style","font-size: 25px; font-weight: regular;")
      .style("opacity", 0)
      .text("")
      .attr("class", "fa fa-bullseye")
      .on("click", function (d) {
	if (dependencySource !== null) {
	  $.post("/dependencies/new", {"new.tutorialId": dependencySource.id, "new.dependencyId": d.id}, function() {
	    dependencySource = null;
	    feedback.attr("xlink:href", "/img/example.png");
	    window.location.reload();
	  });
	}
	d3.event.stopPropagation();
      });

    group.append("text")
      .attr("dx", 58).attr("dy", 0)
      .attr("data-json", function(d) {return JSON.stringify(d);})
      .attr("style","font-size: 25px; font-weight: regular")
      .text("")
      .attr("class", "fa move-icon")
      .on("click", function () {
	moveTarget = $(this).data("json");
	feedback.attr("xlink:href", moveTarget.iconPath || "/img/example.png");
	d3.event.stopPropagation();
      });

    group.append("text")
      .on("click", function(d) {
	d3.event.stopPropagation();
	window.location.href = "/tutorials/" + d.id + "/edit";
      })
      .attr("dx", 0).attr("dy", 0)
      .attr("style","font-size: 25px; font-weight: regular")
      .text("")
      .attr("class", "fa fa-pencil");
  }

  d3.select("svg.grid")
    .on("click", function() {
      var p = from_mouse(d3.mouse(this));

      if(moveTarget === null && dependencySource === null) {
	$.post("/tutorials/new", {"new.x": p.x , "new.y": p.y}, function() {
	  window.location.reload();
	});
      } else if (dependencySource !== null) {
	dependencySource = null;
	bullseyes.style("opacity", 0);
	feedback.attr("xlink:href", "/img/example.png");
      } else if (moveTarget !== null) {
	d3.event.stopPropagation();
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

  var feedback = d3.select("svg.grid")
      .append("image")
      .attr("class", "feedback")
      .attr("xlink:href", "/img/example.png")
      .attr("width", 60).attr("height", 60)
      .attr("x", -100)
      .attr("y", -100)
      .attr("opacity", 0.5);
});
