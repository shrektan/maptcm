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

var reset_size = function() {
  $("div.outer").css("top", $("nav.navbar-static-top").height());
   if ($(window).width() <= 940) {
    $("#github_ribbon").hide();
  } else {
    $("#github_ribbon").show();
  }
};
 
// resize the window size of the map
$(window).resize(function(){
  reset_size();
});

// better loading panel
$(document).ready(function(){
  $("#loading-content").show();
});

$(document).ready(function() {
 $('.navbar-collapse').on('shown.bs.collapse', function() {
   reset_size();
 });
 $('.navbar-collapse').on('hidden.bs.collapse', function() {
   reset_size();
 });
});

$(window).load(function(){
  $("#loading-content").fadeOut(1500);
  reset_size();
});
