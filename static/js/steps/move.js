function setupSteps() {
  $(".icon-edit-link .label").click(function() {
    $(".icon-edit-link").toggleClass("clicked");
  });

  function swap(step1, step2) {
    return {ordinal: step1.data("ordinal"), id: step2.data("id")};
    //$.post("/steps/"+ id + "/edit?edit-step.ordinal=" + prevOrdinal, {}, function(){});
  }

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

/*  if($(".lazy-load-thumbnail").attr("name").indexOf("youtube") == -1) { //vimeo
    $.getJSON("http://vimeo.com/api/oembed.json?url=http%3A//vimeo.com/76979871", function(data) {
      console.log("got data.");
      console.log(data);
    });
    } */


  $(".lazy-load").click(function() {
    var thisIframe = $(this).hide().parents(".step-video").children("iframe").show();
    thisIframe.attr("src", thisIframe.attr("name"));
  });
}
