const oxFract = 0.75;
const dsFract = 1.0 - oxFract;

const divs3 = [
  {id:"ox-plot", class:"svg-container"},
  {id:"ds-plot", class:"svg-container"},
  {id:"pc-paper", class:"footer"}];

// Provide the 3 DIVs formerly specified in my 'index.html' file:
div.selectAll("div").data(divs3).enter().append("div")
    .attr("id", d => d.id)
    .attr("class", d => d.class);

// TODO: Should I use 'classed()' above, instead?
    
// Provide the content for the 'pc-paper' DIV
var dl = div.select("#pc-paper").append('dl');
dl.append('dt').html('Reference');
dl.append('dd').html('Norris DC. Precautionary Coherence Unravels Dose Escalation Designs. <i>bioRxiv</i>. December 2017:240846. <a href="https://www.biorxiv.org/content/early/2017/12/29/240846">doi:10.1101/240846</a>');

div.select('#ox-plot')
    .style('width', oxFract * width + horiz(oxMargin) + 'px')
    .style('height', height + vert(oxMargin) + 'px')
    .style('position','absolute')
    .style('top', 0+'px')
    .style('left', 0+'px');

div.select('#ds-plot')
    .style('width', dsFract * width + horiz(dsMargin) + 'px')
    .style('height', height + vert(dsMargin) + 'px')
    .style('position','absolute')
    .style('top', 0+'px')
    .style('left', oxFract * width + horiz(oxMargin) + 'px');

// TODO: Consider passing the appropriate DIVs to these 'render*' functions.
//       This would insulate these functions from need for esoteric knowledge
//       of the -div- variable!
// OTOH: Maybe it's better not to pretend I'm achieving more modularity than
//       this programming environment truly supports. That is, perhaps it's
//       best to acknowledge the prevailing idioms, and treat r2d3-provided
//       variables as globals. I can expect to gain little from a superficial
//       simulacrum of modularity!
this.oxPlot = renderOXplot({width: oxFract*width,
                              margin: oxMargin,
                              });
this.dsPlot = renderDSplot({width: dsFract*width,
                              margin: dsMargin,
                              });

div.select('.footer')
    .style('top', height + vert(oxMargin) +'px');