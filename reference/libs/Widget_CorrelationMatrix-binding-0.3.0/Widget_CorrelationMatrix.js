// The correlation matrix, made by the script every gsm.bio widget shares
// (inst/htmlwidgets/shared/gsm.bio.widget.js).
HTMLWidgets.widget({
    name: 'Widget_CorrelationMatrix',
    type: 'output',
    factory: GsmBioWidget.factory(function(chart, settings) {
        return BioViz.correlationMatrix(chart, settings);
    }, ['statistic'])
});
