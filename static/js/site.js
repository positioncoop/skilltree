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
    tutorials = data.tutorials;
    var enter = d3.select("svg.grid").selectAll("g.tutorial").data(data.tutorials).enter();

    lines = d3.select("svg.grid").selectAll("line.dependency").data(data.dependencies).enter()
      .append("line")
      .attr("x1", function(d) {return to_display(d.source).x;})
      .attr("y1", function(d) {return to_display(d.source).y + 30;})
      .attr("x2", function(d) {return to_display(d.target).x + 60;})
      .attr("y2", function(d) {return to_display(d.target).y + 30;})
      .attr("style", "stroke:#A2A1A1;stroke-width:2;stroke-dasharray:3;");
    appendTutorial(enter);
  });


  var moveTarget = null;
  var dependencySource = null;
  var bullseyes = null;
  var toolboxes = null;
  var tutorials = null;
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

    toolboxes = group.append("g")
      .attr("transform", function(d) {
	return "translate(-15, -2)";
      });

    toolboxes.append("text")
      .attr("dx", 0)
      .attr("style","font-size: 25px; font-weight: regular")
      .text("")
      .attr("data-json", function(d) {return JSON.stringify(d);})
      .attr("class", "fa fa-long-arrow-left")
      .on("click", function () {
	dependencySource = $(this).data("json");
	feedback.attr("xlink:href", "");
	bullseyes.style("opacity", 1);
	toolboxes.style("opacity", 0);
	d3.event.stopPropagation();
      });

    toolboxes.append("text")
      .attr("dx", 30)
      .attr("data-json", function(d) {return JSON.stringify(d);})
      .attr("style","font-size: 25px; font-weight: regular")
      .text("")
      .attr("class", "fa move-icon")
      .on("click", function () {
	moveTarget = $(this).data("json");
	feedback.attr("xlink:href", moveTarget.iconPath || "/img/example.png");
	d3.event.stopPropagation();
      });

    toolboxes.append("text")
      .attr("dx", 60)
      .attr("style","font-size: 25px; font-weight: regular")
      .text("")
      .attr("class", "fa fa-pencil")
      .on("click", function(d) {
	d3.event.stopPropagation();
	window.location.href = "/tutorials/" + d.id + "/edit";
      });

    bullseyes = group.append("text")
      .attr("dx", 60).attr("dy", 38)
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
	toolboxes.style("opacity", 1);
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
      var gridPos = from_mouse(d3.mouse(this));
      var p = to_display(gridPos);

      var overlaps = tutorials.filter(function(t) {
	return (t.x == gridPos.x) &&
	  ((t.y == gridPos.y - 1) || (t.y == gridPos.y) || (t.y == gridPos.y + 1));
      });

      if (overlaps.length !== 0) {
	feedback.attr("opacity", 0);
      } else {
	feedback.attr("opacity", 0.5);
	feedback.attr("x", p.x) .attr("y", p.y);
      }
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
