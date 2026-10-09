// The group comparison chart, made by the script every gsm.bio widget shares
// (inst/htmlwidgets/shared/gsm.bio.widget.js).
HTMLWidgets.widget({
    name: 'Widget_GroupComparison',
    type: 'output',
    factory: GsmBioWidget.factory(function(chart, settings) {
        return BioViz.groupComparison(chart, settings);
    }, ['statistic'])
});
