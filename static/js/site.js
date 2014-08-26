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

/* eventually all of this shit should be moved somewhere */

$(".icon-edit-link .label").click(function() {
  $(".icon-edit-link").toggleClass("clicked");
});

$(".step-move-up").click(function() {
  var thisstep = $(this).parents(".tutorial-step");
  var prevstep = thisstep.prev(".tutorial-step");
  if(prevstep.length > 0) {
    thisstep.slideUp('normal', function() {
      prevstep.before(thisstep.detach())
      thisstep.slideDown('normal');
    });
  }
});

$(".step-move-down").click(function() {
  var thisstep = $(this).parents(".tutorial-step");
  var nextstep = thisstep.next(".tutorial-step");
  if(nextstep.length > 0) {
    thisstep.slideUp('normal', function() {
      nextstep.after(thisstep.detach())
      thisstep.slideDown('normal');
    });
  }
});

/*if($(".lazy-load img").attr("name").indexOf("youtube") == -1) { //vimeo
      $.getJSON("http://vimeo.com/api/oembed.json?url=http%3A//vimeo.com/76979871
    } */


$(".lazy-load").click(function() {
    var thisIframe = $(this).hide().parents(".step-video").children("iframe").show();
    thisIframe.attr("src", thisIframe.attr("name"));
});

});
