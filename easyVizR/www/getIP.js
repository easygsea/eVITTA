$(document).ready(function(){
    // $.get('https://www.cloudflare.com/cdn-cgi/trace', function(response) {
    //   Shiny.onInputChange("getIP", response);
    // }, "json");
    $.getJSON('http://www.geoplugin.net/json.gp?jsoncallback=?', function(data){
    Shiny.onInputChange("getIP", data);},"json");
  });
  