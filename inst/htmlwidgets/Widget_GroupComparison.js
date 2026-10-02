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
            const when = String(by.computed_at || '').replace('T', ' ').replace(/:\d\dZ$/, ' UTC');
            const who = 'R ' + by.r_version + ' with gsm.bio ' + by.gsm_bio_version + ' on ' + when;
            if (!statistics.results.length)
                return 'Statistics: no result was stored with this page (' + who +
                    '). No R runs here, so a test chosen in it says that statistics are unavailable.';
            return 'Statistics: computed by ' + who + ' and stored with this page. No R runs ' +
                'here, so a view that was not computed says that statistics are unavailable.';
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
                // As wide as its container and as tall as the chart, unless a
                // size was asked for: a page that gives every widget a fixed
                // size in pixels would cut the chart off on a narrow screen.
                if (x.bAutoWidth)
                    el.style.width = '100%';
                if (x.bAutoHeight)
                    el.style.height = 'auto';
                // A page that shows the widget as the output of code (a
                // reference page, a notebook) keeps lines unbroken around
                // it; the chart's sentences wrap.
                el.style.whiteSpace = 'normal';

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
                    note.style.cssText = 'margin:.75rem 0 0;font:.8rem/1.4 system-ui,-apple-system,' +
                        '"Segoe UI",sans-serif;color:#52616f;';
                    note.dataset.storedResults = String(statistics.results.length);
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
