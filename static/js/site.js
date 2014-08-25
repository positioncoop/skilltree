var tutorialData = null;
var dependencyData = null;
$(function() {
  d3.json("/tutorials?format=json", function(error, data) {
    tutorialData = data;
    drawSkilltree();
  });

  d3.json("/dependencies?format=json", function(error, data) {
    dependencyData = data;
    drawSkilltree();
  });

  function drawSkilltree() {
    if (tutorialData !== null && dependencyData !== null) {
      drawLines(dependencyData);
      drawTutorials(tutorialData);
      drawAdmin(tutorialData);
    }
  }
});
