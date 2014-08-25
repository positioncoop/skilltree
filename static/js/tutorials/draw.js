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
  var enter = d3.select("svg.tree").selectAll("g.tutorial").data(tutorialData).enter();
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

function drawLines(dependencyData) {
  d3.select("svg.tree").selectAll("line.dependency").data(dependencyData).enter()
    .append("line")
    .attr("x1", function(d) {return to_display(d.source).x;})
    .attr("y1", function(d) {return to_display(d.source).y + 30;})
    .attr("x2", function(d) {return to_display(d.target).x + 60;})
    .attr("y2", function(d) {return to_display(d.target).y + 30;})
    .attr("style", "stroke:#A2A1A1;stroke-width:2;stroke-dasharray:3;");
}
