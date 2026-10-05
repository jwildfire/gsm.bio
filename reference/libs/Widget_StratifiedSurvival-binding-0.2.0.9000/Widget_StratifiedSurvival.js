// The stratified survival chart, made by the script every gsm.bio widget
// shares (inst/htmlwidgets/shared/gsm.bio.widget.js).
HTMLWidgets.widget({
    name: 'Widget_StratifiedSurvival',
    type: 'output',
    factory: GsmBioWidget.factory(function(chart, settings) {
        return BioViz.stratifiedSurvival(chart, settings);
    }, ['statistic'])
});
