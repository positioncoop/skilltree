$(function() {
  var tutorialData = null;
  var dependencyData = null;
  var courseData = null;

  d3.json("/tutorials?format=json", function(error, data) {
    data.sort(function(a,b) { return d3.ascending(parseInt(a.id), parseInt(b.id)); });
    tutorialData = data;
    drawSkilltree();
  });

  d3.json("/dependencies?format=json", function(error, data) {
    dependencyData = data;
    drawSkilltree();
  });

  $.ajax("/courses", {
    success: function (data, status, xhr) {
      courseData = data;
      drawSkilltree();
    }
  });

  function drawSkilltree() {
    if (tutorialData !== null && dependencyData !== null && courseData !== null) {
      setupTreeCanvas();
      lookupDefaultIconPath();
      drawLines(dependencyData);
      drawTutorials(tutorialData);
      drawCreate(tutorialData);
      drawTools(tutorialData, dependencyData);
      setupSteps();
      drawCourses(courseData);
    }
  }
});

function lookupDefaultIconPath() {
  window.tutorialDefaultIconPath = $("#javascript-helpers #tutorialDefaultIconPath").attr("src");
}

function setupTreeCanvas() {
  d3.select("svg.tree").append("g").attr("class", "tutorials");
  d3.select("svg.tree").append("g").attr("class", "paths");

  if ( $.cookie("treeScroll") !== null ) {
    $(".section-tree").scrollTop( $.cookie("treeScroll") );
  }

  $(window).on('beforeunload', function(){
    $.cookie("treeScroll", $(".section-tree").scrollTop() );
  });
}

function drawCourses(courseData) {
  var container = $(".courses");
  container.append("<div class='title'>Courses</div>");

  courseData.forEach(function(c) {
    var course = $("<div class='course'>");
    course.append(c.title);
    if (isLoggedIn) {
      course.append(" ");
      course.append($("<a onclick=\"return confirm('Are you sure you want to delete this?')\" href='/courses/" + c.id + "/delete'>del</a>"));
    }

    course.append(": Week ");
    if (isLoggedIn) {
      course.append($("<a href='/courses/" + c.id + "/weeks/new'>").append($("<button>").text("+")));
      course.append($("<a href='/courses/" + c.id + "/weeks/delete'>").append($("<button>").text("-")));
    }


    c.weeks.forEach(function(w) {
      var weekLink = $("<a href='#'>").text(w.number);
      function turnOn() {
        $(".add-week-button").remove();
        $("g.tutorial").attr("stroke-width", 0).attr("stroke", "")
        if (isLoggedIn) {
          $("g.tutorial").each(function (_,_e) {
            var e = $(_e);
            var data = e.data("json");
            var add = $("<a class='add-week-button' data-add-week-id='" +
                        data.id + "' href='/courses/" + c.id + "/weeks/" +
                        w.id + "/toggle_tutorial?tutorial_id=" + data.id + "'>add</button>").on("click", function () {
            });
            add.css({"position": "absolute"
                     ,"display": "block"
                     ,"background-color": "white"
                     ,"border": "2px solid #ccc"
                     ,"padding": "5px"
                     ,"top": e.position().top
                     ,"left": e.position().left});
            $("body").append(add);
          });
        }

        $.ajax("/courses/" + c.id + "/weeks/" + w.id, {
          success: function (data, status, xhr) {
            data.forEach(function(t) {
              $("a[data-add-week-id=" + t.id + "]").text("remove");
              $("g[data-tutorial-id=" + t.id + "]").attr("stroke", "red").attr("stroke-width", "5");
            });
          }
        });

        weekLink.off("click.turn-on");

        weekLink.on("click.turn-off", function () {
          $(".add-week-button").remove();
          $("g.tutorial").attr("stroke-width", 0).attr("stroke", "");
          weekLink.on("click.turn-on", turnOn);
        });
      }

      weekLink.on("click.turn-on", turnOn);

      course.append(weekLink).append(" ");
    });

    container.append(course);
  });

  if (isLoggedIn) {
    var form = $("<form method='post' action='/courses/new'>");
    form.append($("<input type='text' name='new.title'>"));
    form.append($("<input type='submit' value='+'>"));
    container.append(form);
  }
}
