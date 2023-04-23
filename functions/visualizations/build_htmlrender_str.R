build_htmlrender_str <- function(class_decoder, class_color){
  cx_position <- 600
  cy_position <- 10
  
  function_str <- '
  function(el, x, data) {
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Reference", "Prediction"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .text(labels[i]);
    })
    var svg = d3.select("svg")
    '
  for(i in 1:length(class_color)) {
    if (i != 1) {
      cy_position <- cy_position + 20
    }
      
    function_str <- paste0(
      function_str, "\n",
      '    svg.append("circle").attr("cx", ', cx_position, ').attr("cy", ', cy_position, ').attr("r", 6).style("fill", "', class_color[i], '")', "\n",
      '    svg.append("text").attr("x", ', cx_position+20, ').attr("y", ', cy_position, ').text("', class_decoder[i], '").style("font-size", "15px").attr("alignment-baseline","middle")'
    )
  }
  
  function_str <- paste0(function_str, '}')
  
  return(function_str)
  
}