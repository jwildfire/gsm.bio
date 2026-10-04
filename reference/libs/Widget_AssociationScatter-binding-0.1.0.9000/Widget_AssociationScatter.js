// The association scatter, made by the script every gsm.bio widget shares
// (inst/htmlwidgets/shared/gsm.bio.widget.js).
HTMLWidgets.widget({
    name: 'Widget_AssociationScatter',
    type: 'output',
    factory: GsmBioWidget.factory(function(chart, settings) {
        return BioViz.associationScatter(chart, settings);
    }, ['statistic', 'fit_statistic'])
});
