$(function() {
  var tutorialData = null;
  var dependencyData = null;

  d3.json("/tutorials?format=json", function(error, data) {
    data.sort(function(a,b) { return d3.ascending(parseInt(a.id), parseInt(b.id)); });
    tutorialData = data;
    drawSkilltree();
  });

  d3.json("/dependencies?format=json", function(error, data) {
    dependencyData = data;
    drawSkilltree();
  });

  function drawSkilltree() {
    if (tutorialData !== null && dependencyData !== null) {
      lookupDefaultIconPath();
      drawLines(dependencyData);
      drawTutorials(tutorialData);
      drawCreate(tutorialData);
      drawTools(tutorialData, dependencyData);
      setupSteps();
    }
  }

  $(".section-tree").mousemove(mousePointerScroll);

});

function lookupDefaultIconPath() {
  window.tutorialDefaultIconPath = $("#javascript-helpers #tutorialDefaultIconPath").attr("src");
}


function mousePointerScroll(e) {

  //what we weant to do: proportional to client, scroll page.
  //so: if cursor is 25% of clientX and 25% of clientY, scroll page to 25% of pageX and 25% of pageY.
  //but actually. we want some padding. so: if cursor is 25% of clientX and 25% of clientY, scroll page to 25% of pageX and 25% of pageY.

  var toScroll = $(".section-tree");
  var x = e.pageX - toScroll.offset().left;
  var y = e.pageY - toScroll.offset().top;
  var scrollToX = (x / toScroll.width()) * (toScroll[0].scrollWidth - toScroll.width());
  var scrollToY = (y / toScroll.height()) * (toScroll[0].scrollHeight - toScroll.height());
  console.log(e.pageY + " / " + toScroll.height() + " * " + toScroll[0].scrollHeight);


  toScroll.scrollTop(scrollToY);
}


