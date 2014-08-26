$(function() {
  var tutorialData = null;
  var dependencyData = null;

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
      initVariables();
      drawLines(dependencyData);
      drawTutorials(tutorialData);
      drawCreate(tutorialData);
      drawTools(tutorialData, dependencyData);
    }
  }

  function initVariables() {
    //fetch variable from html, set as global variable
    window.tutorialDefaultIconPath = $("#javascript-helpers #tutorialDefaultIconPath").attr("src"); 
  }


$(".icon-edit-link .label").click(function() {
  $(".icon-edit-link").toggleClass("clicked");
});

//  $("textarea.tutorial-title").flexible();


});
