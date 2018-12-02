const protoDSplot = { 
  width: 0.25 * width, 
  height: height, 
  margin: dsMargin,
  levels: 7,
};

function dsFactory(opts, proto = protoDSplot) {
  const dsPlot = Object.assign({}, proto, opts);

  const margin = dsPlot.margin;
  const horizMargins = margin.left + margin.right;
  const vertMargins = margin.top + margin.bottom;
  dsPlot.svg = div.select('#ds-plot')
      .append('svg')
      .attr('id', dsPlot.id || 'dsPlot')
      .attr('viewBox',`0 0 ${dsPlot.width+horizMargins} ${dsPlot.height+vertMargins}`)
      .attr('width', dsPlot.width + horizMargins) 
      .attr('height', dsPlot.height + vertMargins)
    .append('g')
      //.attr('id','container')
      .attr('transform', `translate(${margin.left}, ${margin.top})`);

  return dsPlot;
}

function renderDSplot(opts) {
  const dsPlot = dsFactory(opts);

  const x = d3.scaleLinear()
    .domain([1.0, 0.0])
    .range([0, dsPlot.width]);

  const y = d3.scaleLinear()
    .clamp(true)
    .range(d3.range(7).map(d => ((6.1-d)*dsPlot.height/6.25)))
    .domain([1, 2, 3, 4, 5, 6, 7]);
  
  const xAxis = d3.axisBottom().scale(x);
  xAxis.tickValues(d3.range(1,-0.01,-0.2));
  const yAxis = d3.axisRight().scale(y);
  yAxis.tickValues(d3.range(1,7+1));
  yAxis.tickFormat(d3.format('.0f'));

  dsPlot.svg.append('g')
    .attr('class','axis')
    .attr('transform', `translate(0, ${dsPlot.height})`)
    .call(xAxis);

  dsPlot.svg.append('g')
    .attr('class','axis')
    .attr('transform', `translate(${dsPlot.width}, 0)`)
    .call(yAxis);

  // 'Bypass rule' threshold
  dsPlot.svg.append('line')
      .attr('x1', x(0.8))
      .attr('x2', x(0.8))
      .attr('y1', y(1))
      .attr('y2', y(7))
      .attr('class','bypass-rule');

  // 'Stop rule' threshold
  dsPlot.svg.append('line')
      .attr('x1', x(1/3))
      .attr('x2', x(1/3))
      .attr('y1', y(1))
      .attr('y2', y(7))
      .attr('class','stop-rule');

/*
TODO:
1./Load & plot 'dose-survival.csv' using step interpolation
2./Plot the -upper- and -lower- series, too
3./Draw 80% and 33% thresholds
4. Colorize crossed segments of threshold lines
5. Seek out refinements & standard idioms
(a) Standard pattern for plotting the (surv, upper, lower) *series*?
*/

  // Colors from brewer.pal(4,"Dark2")
  const line = d3.line()
    .y(d => y(d.dose))
    .curve(d3.curveStepBefore);

//  d3.csv('data/dose-survival.csv', data => {

    data = d3.nest().key(d => d.period)
      .entries(data.ds)
      .map(e => e.values);
    //this.data_ds = data; // enable vetting at console
    
    dsPlot.svg.append('g').attr('class','ds-line')
      .selectAll('.surv-line')
        .data(data)
      .enter().append('path')
        .attr('period', (d,i) => i+1)
        .attr('class','surv-line')
        .attr('d', line.x(d => x(d.surv)))
        .attr('visibility',
              (d,i) => i+1 == data.length ? 'visible':'hidden');

    dsPlot.svg.append('g').attr('class','ds-line')
      .selectAll('.conf-line')
        .data(data)
      .enter().append('path')
        .attr('period', (d,i) => i+1)
        .attr('class','conf-line')
        .attr('d', line.x(d => x(d.upper)))
        .attr('visibility',
              (d,i) => i+1 == data.length ? 'visible':'hidden');

    dsPlot.svg.append('g').attr('class','ds-line')
      .selectAll('.conf-line')
        .data(data)
      .enter().append('path')
        .attr('period', (d,i) => i+1)
        .attr('class','conf-line')
        .attr('d', line.x(d => x(d.lower)))
        .attr('visibility',
              (d,i) => i+1 == data.length ? 'visible':'hidden');
//  }); 

  dsPlot.svg.append("text")
    .attr("class", "x label")
    .attr("text-anchor", "middle")
    .attr("x", x(0.5))
    .attr("y", height + 35)
    .text("Fraction tolerant");

  dsPlot.svg.append("text")
    .attr("class", "y label")
    .attr("text-anchor", "middle")
    .attr("x", -height*0.5)
    .attr("y", dsPlot.width + 25)
    .attr("dy", ".75em")
    .attr("transform", "rotate(-90)")
    .text("Dose level");



  return dsPlot;
}
