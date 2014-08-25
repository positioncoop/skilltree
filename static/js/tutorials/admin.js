function redirectTutorialEdit(tutorial) {
  window.location.href = "/tutorials/" + tutorial.id + "/edit";
}

var grid = null;
var bullseyes = null;
var toolboxes = null;

var tutorialData = null;
var dependencyData = null;

function drawAdmin() {
  if (window.isLoggedIn) {
    grid = d3.select("svg.tree");
    drawToolboxes(grid.selectAll("g.tutorial"));
    addEditHandlers(grid);
    tutorialMover.init(grid);
  }
}

function drawTool(toolboxes, tool) {
  return toolboxes.append("text")
    .attr("dx", tool.dx)
    .attr("dy", tool.dy || 0)
    .attr("style","font-size: 25px; font-weight: regular")
    .attr("data-json", function(d) {return JSON.stringify(d);})
    .text(tool.text)
    .attr("class", tool.classes);
}

function drawToolboxes(tutorials) {
  toolboxes = tutorials.append("g").attr("transform", function() {return "translate(-10, -2)";});

  drawTool(toolboxes, {dx: 0, text: "", classes: "fa move-icon",})
    .on("click", function(d){
      d3.event.stopPropagation();
      tutorialMover.start(d);
    });

  drawTool(toolboxes, {dx: 30, text: "", classes: "fa fa-pencil",})
    .on("click", function(d) {
      d3.event.stopPropagation();
      redirectTutorialEdit(d);
    });

  bullseyes = drawTool(tutorials, {dx: -22, dy: 38, text:"", classes: "fa fa-bullseye",})
    .style("opacity", 0)
    .on("click", function(d) {
      d3.event.stopPropagation();
      tutorialDepender.finish(d);
    });

  drawTool(toolboxes, {dx: 60, text:"", classes: "fa fa-long-arrow-right",})
    .on("click", function (d) {
      d3.event.stopPropagation();
      tutorialDepender.start(d);
    });

  return toolboxes;
}

function addEditHandlers(grid) {
  grid
    .on("click", function() {
      d3.event.stopPropagation();
      tutorialMover.finish(d3.mouse(this));
      tutorialDepender.reset();
    })
    .on("mousemove", function() {
      tutorialMover.hover(d3.mouse(this));
    });
}

var tutorialDepender = {
  dependencySource: null,

  reset: function() {
    var dep = this.dependencySource;
    this.dependencySource = null;
    bullseyes.style("opacity", 0);
    toolboxes.style("opacity", 1);
    return dep;
  },

  start: function(d) {
    this.dependencySource = d;
    toolboxes.style("opacity", 0);
    bullseyes.style("opacity", 1);
  },

  finish: function (d) {
    if (this.dependencySource !== null) {
      var dependencySource = this.dependencySource;
      var existing = dependencyData.filter(function (dep) {
	return (dep.source.id === d.id && dep.target.id === dependencySource.id)
          || (dep.target.id === d.id && dep.source.id === dependencySource.id);
      });

      function afterPost() {
	redirectTutorialEdit(tutorialDepender.reset());
      }

      if (existing.length !== 0) {
        var dep = existing[0];
        $.post("/dependencies/" + dep.id + "/delete", {}, afterPost);
      } else {
        $.post("/dependencies/new", {"new.tutorialId": this.dependencySource.id, "new.dependencyId": d.id}, afterPost);
      }
    }
  },
};

var tutorialMover = {
  moveTarget: null,
  feedback: null,

  reset: function() {
    var target = this.moveTarget;
    this.moveTarget = null;
    this.feedback.attr("xlink:href", "/img/example.png");
    return target;
  },

  init: function(grid) {
    this.feedback = grid.append("image")
      .attr("class", "feedback")
      .attr("xlink:href", "/img/example.png")
      .attr("width", 60).attr("height", 60)
      .attr("x", -100)
      .attr("y", -100)
      .attr("opacity", 0);
  },

  start: function (d) {
    this.moveTarget = d
    this.feedback.attr("xlink:href", this.moveTarget.iconPath || "/img/example.png");
  },

  finish: function(mouse) {
    if (this.moveTarget !== null) {
      var p = from_mouse(mouse);
      $.post("/tutorials/" + this.moveTarget.id + "/move", {"move.x": p.x , "move.y": p.y},
             function() {
               redirectTutorialEdit(tutorialMover.reset());
             });
    }
  },

  hover: function(mouse) {
    if(this.moveTarget !== null) {
      var mousePos = from_mouse(mouse);
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
