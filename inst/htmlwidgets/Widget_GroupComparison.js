HTMLWidgets.widget({
    name: 'Widget_GroupComparison',
    type: 'output',
    factory: function(el, width, height) {
        let instance = null;

        // A table arrives by column and the chart takes an array of row
        // objects. A table of one row arrives with single values for columns.
        const toRows = function(table) {
            if (!table || Array.isArray(table))
                return table || null;
            const columns = {};
            Object.keys(table).forEach(function(name) {
                columns[name] = Array.isArray(table[name]) ? table[name] : [table[name]];
            });
            return HTMLWidgets.dataframeToD3(columns);
        };

        // Which R computed the stored results, said under the chart: a stored
        // result is the answer of the R that built the widget.
        const provenance = function(statistics) {
            const by = statistics.computed_by || {};
            const count = statistics.results.length;
            const when = String(by.computed_at || '').replace('T', ' ').replace(/:\d\dZ$/, ' UTC');
            return 'Statistics on this page were computed by R ' + by.r_version +
                ' with gsm.bio ' + by.gsm_bio_version + ' on ' + when + ' and stored with it: ' +
                count + ' stored result' + (count === 1 ? '' : 's') + '. No R runs in this page, ' +
                'so a view that was not computed prints that statistics are unavailable for it.';
        };

        return {
            renderValue: function(x) {
                if (x.bDebug)
                    console.log(x);

                // Empty R lists serialize as arrays; the module expects an object.
                const settings =
                    x.lSettings && !Array.isArray(x.lSettings) ? Object.assign({}, x.lSettings) : {};
                const statistics = x.lStatistics || {};
                statistics.results = Array.isArray(statistics.results) ? statistics.results : [];

                if (instance && typeof instance.destroy === 'function')
                    instance.destroy();
                instance = null;
                el.innerHTML = '';
                // As tall as the chart, unless a height was asked for.
                if (x.bAutoHeight)
                    el.style.height = 'auto';

                const chart = document.createElement('div');
                chart.className = 'gsm-bio-chart';
                el.appendChild(chart);

                try {
                    // The connection is made from the stored results and
                    // nothing else: no R is started and nothing is fetched.
                    settings.connection = BioViz.r.createConnection({ results: statistics.results });
                    instance = BioViz.groupComparison(chart, settings);
                    const participants = toRows(x.dfParticipants);
                    instance.init(participants
                        ? { results: toRows(x.dfResults), participants: participants }
                        : { results: toRows(x.dfResults) });
                } catch (error) {
                    // The chart says what it refused, in the page and not
                    // only in the console.
                    console.error(error);
                    if (!chart.textContent) {
                        const warning = document.createElement('p');
                        warning.className = 'gsm-bio-error';
                        warning.textContent = error && error.message ? error.message : String(error);
                        chart.appendChild(warning);
                    }
                    return;
                }

                if (settings.statistic !== null) {
                    const note = document.createElement('p');
                    note.className = 'gsm-bio-provenance';
                    note.style.cssText = 'margin:.75rem 0 0;font-size:.8rem;color:#52616f;';
                    note.textContent = provenance(statistics);
                    el.appendChild(note);
                }
            },
            resize: function(width, height) {
                if (instance && typeof instance.resize === 'function')
                    instance.resize();
            },
            // The chart itself, for a page that drives it:
            // HTMLWidgets.find(selector).chart().statistics() is what it asked
            // and what it was answered.
            chart: function() {
                return instance;
            }
        };
    }
});
