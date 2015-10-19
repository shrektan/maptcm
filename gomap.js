// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  var name = $el.data("name");
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: long,
    name: name,
    nonce: Math.random()
  });
});

// resize the window size of the map
$(window).resize(function(){
  $("div.outer").css("top", $("div.container").height());
});

// better loading panel
$(document).ready(function(){
  $("#loading-content").show();
});


$(window).load(function(){
  $("div.outer").css("top", $("div.container").height());
  $("#loading-content").fadeOut(1500);
});
