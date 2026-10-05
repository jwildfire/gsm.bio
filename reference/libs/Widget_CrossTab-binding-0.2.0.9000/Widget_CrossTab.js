// The cross-tabulation, made by the script every gsm.bio widget shares
// (inst/htmlwidgets/shared/gsm.bio.widget.js).
HTMLWidgets.widget({
    name: 'Widget_CrossTab',
    type: 'output',
    factory: GsmBioWidget.factory(function(chart, settings) {
        return BioViz.crossTab(chart, settings);
    }, ['statistic'])
});
