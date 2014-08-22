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

  d3.json("/tutorials", function(error, data) {
    tutorialData = data;
    draw();
  });

  d3.json("/dependencies", function(error, data) {
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

  $.ajax("/courses", {
    success: function (data, status, xhr) {
      var container = $(".courses");
      container.append("<h3>Courses</h3>");
      container.css({"position": "absolute",
                     "top": "0px",
                     "left": "10px"
                    });

      data.forEach(function(c) {
        var course = $("<div class='course'>");
        course.append(c.title);
        if (isLoggedIn) {
          course.append(" ");
          course.append($("<a onclick=\"return confirm('Are you sure you want to delete this?')\" href='/courses/" + c.id + "/delete'>del</a>"));
        }

        course.append(": Week ");
        c.weeks.forEach(function(w) {
          var weekLink = $("<a href='#'>").text(w.number);
          function turnOn() {
            $(".add-week-button").remove();
            $("g.tutorial").attr("stroke-width", 0).attr("stroke", "")
            if (isLoggedIn) {
              $("g.tutorial").each(function (_,_e) {
                var e = $(_e);
                var data = e.data("json");
                var add = $("<a class='add-week-button' data-add-week-id='" + data.id + "' href='/courses/" + c.id + "/weeks/" + w.id + "/toggle_tutorial?tutorial_id=" + data.id + "'>add</button>").on("click", function () {
                });
                add.css({"position": "absolute"
                         ,"display": "block"
                         ,"background-color": "white"
                         ,"border": "2px solid #ccc"
                         ,"padding": "5px"
                         ,"top": e.position().top
                         ,"left": e.position().left});
                $("body").append(add);
              });
            }

            $.ajax("/courses/" + c.id + "/weeks/" + w.id, {
              success: function (data, status, xhr) {
                data.forEach(function(t) {
                  $("a[data-add-week-id=" + t.id + "]").text("remove");
                  $("g[data-tutorial-id=" + t.id + "]").attr("stroke", "red").attr("stroke-width", "5");
                });
              }
            });

            weekLink.off("click.turn-on");

            weekLink.on("click.turn-off", function () {
              $(".add-week-button").remove();
              $("g.tutorial").attr("stroke-width", 0).attr("stroke", "");
              weekLink.on("click.turn-on", turnOn);
            });
          }

          weekLink.on("click.turn-on", turnOn);

          course.append(weekLink).append(" ");
        });

        if (isLoggedIn) {
          course.append($("<a href='/courses/" + c.id + "/weeks/new'>").text("+"));
          course.append(" ");
          course.append($("<a href='/courses/" + c.id + "/weeks/delete'>").text("-"));
        }

        container.append(course);
      });

      if (isLoggedIn) {
        var form = $("<form method='post' action='/courses/new'>");
        form.append($("<input type='text' name='new.title'>"));
        form.append($("<input type='submit' value='+'>"));
        container.append(form);

      }
      $("body").append(container);
    }
  });
});
