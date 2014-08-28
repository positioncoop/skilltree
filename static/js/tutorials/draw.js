function redirectTutorialEdit(tutorial) {
  window.location.href = "/tutorials/" + tutorial.id + "/edit" + window.location.hash;
  window.location.reload();
}

function to_display(p) {
  return {x: p.x * 100 + 20,
          y: p.y * 50 + 20};
}

function from_mouse(mouse) {
  return {x: Math.floor(mouse[0]/100),
          y: Math.floor((mouse[1] - 25)/50)};
}

function drawTutorials(tutorialData) {
  var selection = d3.select("svg.tree").selectAll("g.tutorial").data(tutorialData);
  var tutorials = selection.enter().append("g")
      .attr("data-tutorial-id", function(d) { return d.id; })
      .attr("data-json", function(d) {return JSON.stringify(d);})
      .attr("class", function(d) {
        return "tutorial tutorial-" + d.id + " "  + d.publish;
      });

  tutorials.append("a")
    .attr("xlink:href", function(d) {return "/tutorials/" + d.id;})
    .append("image")
    .attr("xlink:href", function(d) { return d.iconPath || tutorialDefaultIconPath; })
    .attr("width",60).attr("height",60);

  tutorials.append("text")
    .attr("dx", 5)
    .attr("dy", 72)
    .text(function(d) { return d.title });

  selection.attr("transform", function(d) {
    var point = to_display(d);
    return "translate(" + point.x + ", " + point.y + ")";
  });

  tutorials.on("mousedown", function(d) {
    tutorialMover.start(d);
    $(".section-tree").addClass("dragging");
  });

  d3.select("svg.tree").on("mouseup", function() {
    tutorialMover.finish(d3.mouse(this));
    $(".section-tree").removeClass("dragging");
  });

  return tutorials;
}

function bezPath(d) {
  x1 = to_display(d.source).x;
  y1 = to_display(d.source).y + 30;
  x2 = to_display(d.target).x + 60;
  y2 = to_display(d.target).y + 30;
  return "M"
    + x1 + "," + y1
    + " C"
    + ((x2 + x1) / 2 - (Math.sqrt(y1 + y2) / 2)) + "," + y1
    + " "
    + ((x2 + x1) / 2 + (Math.sqrt(y1 + y2) / 2)) + "," + y2
    + " "
    + x2 + "," + y2;
}

function drawLines(dependencyData) {
  var selection = d3.select("svg.tree").selectAll("path.dependency").data(dependencyData, function(d) {return d.id;})

    selection.enter()
    .append("path")
    .attr("class", function(d) {
      var classes  = "dependency source-of-" + d.source.id + " target-of-" + d.target.id;
      if(d.source.publish === "Draft" || d.target.publish === "Draft") {
        classes += " draft";
      }
      return classes;
    });

    selection.attr("d", bezPath)
}
