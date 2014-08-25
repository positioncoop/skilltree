function drawCreate(tutorialData) {
  $(".modeTray").append($("<button class='createModeButton'>").text("create mode").on("click", function() {
    window.location.hash = "#create";
    window.location.reload();
  }));

  if (window.isLoggedIn && "#create" === window.location.hash) {
    $(".modeTray .createModeButton").addClass("active");
    var grid = d3.select("svg.tree");
    tutorialCreator.init(grid, tutorialData);

    grid
      .on("click", function() {
        tutorialCreator.create(d3.mouse(this));
      })
      .on("mousemove", function() {
        tutorialCreator.hover(d3.mouse(this));
      });
  }
}

tutorialCreator = {
  feedback: null,

  create: function(mouse) {
    var p = from_mouse(mouse);

    $.post("/tutorials/new", {"new.x": p.x , "new.y": p.y}, function(d) {
      console.log(window.location.href);
      redirectTutorialEdit(d);
    });
  },

  init: function(grid, tutorialData) {
    this.tutorialData = tutorialData;
    this.feedback = grid.append("image")
      .attr("class", "feedback")
      .attr("xlink:href", "/img/example.png")
      .attr("width", 60).attr("height", 60)
      .attr("x", -100)
      .attr("y", -100)
      .attr("opacity", 0.2);
  },

  hover: function(mouse) {
    var gridPos = from_mouse(mouse);

    var overlaps = this.tutorialData.filter(function(t) {
      return (t.x == gridPos.x) && ((t.y == gridPos.y - 1) || (t.y == gridPos.y) || (t.y == gridPos.y + 1));
    });

    if (overlaps.length !== 0) {
      this.feedback.attr("opacity", 0);
    } else {
      this.feedback.attr("opacity", 0.2);
      var p = to_display(gridPos);
      this.feedback.attr("x", p.x) .attr("y", p.y);
    }
  }
};
