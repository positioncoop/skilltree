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
      setupTreeCanvas();
      lookupDefaultIconPath();
      drawLines(dependencyData);
      drawTutorials(tutorialData);
      drawCreate(tutorialData);
      drawTools(tutorialData, dependencyData);
      setupSteps();
    }
  }


});

function lookupDefaultIconPath() {
  window.tutorialDefaultIconPath = $("#javascript-helpers #tutorialDefaultIconPath").attr("src");
}

function setupTreeCanvas() {

    d3.select("svg.tree").append("g").attr("class", "tutorials");
    d3.select("svg.tree").append("g").attr("class", "paths");
/*  $(".section-tree").mousemove(function (e) {
    var toScroll = $(".section-tree");
    var x = e.pageX - toScroll.offset().left;
    var y = e.pageY - toScroll.offset().top;
    var scrollToX = (x / toScroll.width()) * (toScroll[0].scrollWidth - toScroll.width());
    var scrollToY = (y / toScroll.height()) * (toScroll[0].scrollHeight - toScroll.height());
    toScroll.scrollTop(scrollToY);
  } */

  if ( $.cookie("treeScroll") !== null ) {
    $(".section-tree").scrollTop( $.cookie("treeScroll") );
  }

  $(window).on('beforeunload', function(){
    $.cookie("treeScroll", $(".section-tree").scrollTop() );
  });
}



