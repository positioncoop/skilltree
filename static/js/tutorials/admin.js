function redirectTutorialEdit(tutorial) {
  window.location.href = "/tutorials/" + tutorial.id + "/edit";
}

var grid = null;
var bullseyes = null;
var toolboxes = null;

var dependencySource = null;
var tutorialData = null;
var dependencyData = null;

function drawAdmin() {
  if (window.isLoggedIn) {
    grid = d3.select("svg.tree");
    drawToolboxes(grid.selectAll("g.tutorial"));
    addEditHandlers(grid);
    tutorialMover.drawMoveFeedback(grid);
  }
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
    .on("click", function(){tutorialMover.clickMove(this);});

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

var tutorialMover = {
  moveTarget: null,
  feedback: null,

  reset: function() {
    var target = this.moveTarget;
    this.moveTarget = null;
    this.feedback.attr("xlink:href", "/img/example.png");
    return target;
  },

  drawMoveFeedback: function(grid) {
    this.feedback = grid.append("image")
      .attr("class", "feedback")
      .attr("xlink:href", "/img/example.png")
      .attr("width", 60).attr("height", 60)
      .attr("x", -100)
      .attr("y", -100)
      .attr("opacity", 0);
  },

  clickMove: function (target) {
    this.moveTarget = $(target).data("json");
    this.feedback.attr("xlink:href", this.moveTarget.iconPath || "/img/example.png");
    d3.event.stopPropagation();
  },

  clickGrid: function(target) {
    if (dependencySource !== null) {
      dependencySource = null;
      bullseyes.style("opacity", 0);
      toolboxes.style("opacity", 1);
      this.feedback.attr("xlink:href", "/img/example.png");
    } else if (this.moveTarget !== null) {
      var p = from_mouse(d3.mouse(target));
      d3.event.stopPropagation();
      $.post("/tutorials/" + this.moveTarget.id + "/move", {"move.x": p.x , "move.y": p.y},
             function() {redirectTutorialEdit(tutorialMover.reset());});
    }
  },

  moveOnGrid: function(target) {
    if(this.moveTarget !== null) {
      var mousePos = from_mouse(d3.mouse(target));
      var overlaps = tutorialData.filter(function(t) {
        return (t.x == mousePos.x) && (t.y == mousePos.y);
      });

      if (overlaps.length !== 0) {
        this.feedback.attr("opacity", 0);
      } else {
        this.feedback.attr("opacity", 0.2);

        var p = to_display(mousePos);
        this.feedback.attr("x", p.x) .attr("y", p.y);
      }
    }
  },
};

function addEditHandlers(grid) {
  grid
    .on("click", function() {tutorialMover.clickGrid(this);})
    .on("mousemove", function() {tutorialMover.moveOnGrid(this);});
}
