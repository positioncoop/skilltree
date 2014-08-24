function to_display(p) {
  return {x: p.x * 100 + 20,
          y: p.y * 50 + 20};
}

function from_mouse(mouse) {
  return {x: Math.floor(mouse[0]/100),
          y: Math.floor((mouse[1] - 25)/50)};
}

function redirectTutorialEdit(tutorial) {
  window.location.href = "/tutorials/" + tutorial.id + "/edit";
}

$(function() {
  var grid = d3.select("svg.tree");
  var bullseyes = null;
  var toolboxes = null;
  var feedback = null;

  var moveTarget = null;
  var dependencySource = null;
  var tutorialData = null;
  var dependencyData = null;

  function draw() {
    if (tutorialData !== null && dependencyData !== null) {
      drawLines(dependencyData);
      var tutorials = drawTutorials(tutorialData);
      if (window.isLoggedIn) {
        drawToolboxes(tutorials);
        addEditHandlers();
      }
    }
  }

  d3.json("/tutorials?format=json", function(error, data) {
    tutorialData = data;
    draw();
  });

  d3.json("/dependencies?format=json", function(error, data) {
    dependencyData = data;
    draw();
  });

  function drawTutorials(tutorialData) {
    var enter = grid.selectAll("g.tutorial").data(tutorialData).enter();
    var tutorials = enter.append("g")
        .attr("data-tutorial-id", function(d) { return d.id; })
        .attr("data-json", function(d) {return JSON.stringify(d);})
        .attr("class", function(d) {
          return "tutorial " + d.publish;
        })
        .attr("transform", function(d) {
          var point = to_display(d);
          return "translate(" + point.x + ", " + point.y + ")";
        });

    tutorials.append("a")
      .attr("xlink:href", function(d) {return "/tutorials/" + d.id;})
      .append("image")
      .attr("xlink:href", function(d) {return d.iconPath || "/img/example.png";})
      .attr("width",60).attr("height",60);

    tutorials.append("text")
      .attr("dx", 5)
      .attr("dy", 72)
      .text(function(d) { return d.title });
    return tutorials;
  }

  function drawToolboxes(tutorials) {
    toolboxes = tutorials.append("g")
      .attr("transform", function(d) {
        return "translate(-10, -2)";
      });

    toolboxes.append("text")
      .attr("dx", 0)
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
      .attr("dx", 30)
      .attr("style","font-size: 25px; font-weight: regular")
      .text("")
      .attr("class", "fa fa-pencil")
      .on("click", function(d) {
        d3.event.stopPropagation();
	redirectTutorialEdit(d);
      });

    toolboxes.append("text")
      .attr("dx", 60)
      .attr("style","font-size: 25px; font-weight: regular")
      .text("")
      .attr("data-json", function(d) {return JSON.stringify(d);})
      .attr("class", "fa fa-long-arrow-right")
      .on("click", function () {
        dependencySource = $(this).data("json");
        feedback.attr("xlink:href", "");
        bullseyes.style("opacity", 1);
        toolboxes.style("opacity", 0);
        d3.event.stopPropagation();
      });



    bullseyes = tutorials.append("text")
      .attr("dx", -22).attr("dy", 38)
      .attr("style","font-size: 25px; font-weight: regular;")
      .style("opacity", 0)
      .text("")
      .attr("class", "fa fa-bullseye")
      .on("click", function (d) {
        if (dependencySource !== null) {
	  var existing = dependencyData.filter(function (dep) {
	    return (dep.source.id === d.id && dep.target.id === dependencySource.id)
                || (dep.target.id === d.id && dep.source.id === dependencySource.id);
	  });

	  function afterPost() {
	    var dep = dependencySource;
	    dependencySource = null;
            feedback.attr("xlink:href", "/img/example.png");
	    redirectTutorialEdit(dep);
          }

          if (existing.length !== 0) {
            var dep = existing[0];
            $.post("/dependencies/" + dep.id + "/delete", {}, afterPost);
          } else {
            $.post("/dependencies/new", {"new.tutorialId": dependencySource.id, "new.dependencyId": d.id}, afterPost);
          }
	}
        d3.event.stopPropagation();
      });
    return toolboxes;
  }

  function drawLines(dependencyData) {
    grid.selectAll("line.dependency").data(dependencyData).enter()
      .append("line")
      .attr("x1", function(d) {return to_display(d.source).x;})
      .attr("y1", function(d) {return to_display(d.source).y + 30;})
      .attr("x2", function(d) {return to_display(d.target).x + 60;})
      .attr("y2", function(d) {return to_display(d.target).y + 30;})
      .attr("style", "stroke:#A2A1A1;stroke-width:2;stroke-dasharray:3;");
  }

  function addEditHandlers() {
    grid
      .on("click", function() {
        var p = from_mouse(d3.mouse(this));

        if(moveTarget === null && dependencySource === null) {
          $.post("/tutorials/new", {"new.x": p.x , "new.y": p.y}, function(d) {
	    redirectTutorialEdit(d);
          });
        } else if (dependencySource !== null) {
          dependencySource = null;
          bullseyes.style("opacity", 0);
          toolboxes.style("opacity", 1);
          feedback.attr("xlink:href", "/img/example.png");
        } else if (moveTarget !== null) {
          d3.event.stopPropagation();
          $.post("/tutorials/" + moveTarget.id + "/move", {"move.x": p.x , "move.y": p.y}, function() {
	    var target = moveTarget;
            moveTarget = null;
            feedback.attr("xlink:href", "/img/example.png");
	    redirectTutorialEdit(target)
          });
        }
      })
      .on("mousemove", function() {
        var gridPos = from_mouse(d3.mouse(this));
        var p = to_display(gridPos);

        var overlaps = tutorialData.filter(function(t) {
          if (moveTarget !== null) {
            return (t.x == gridPos.x) && (t.y == gridPos.y);
          } else {
            return (t.x == gridPos.x) &&
              ((t.y == gridPos.y - 1) || (t.y == gridPos.y) || (t.y == gridPos.y + 1));
          }
        });

        if (overlaps.length !== 0) {
          feedback.attr("opacity", 0);
        } else {
          feedback.attr("opacity", 0.2);
          feedback.attr("x", p.x) .attr("y", p.y);
        }
      });

    feedback = grid
      .append("image")
      .attr("class", "feedback")
      .attr("xlink:href", "/img/example.png")
      .attr("width", 60).attr("height", 60)
      .attr("x", -100)
      .attr("y", -100)
      .attr("opacity", 0.2);
  }
});
