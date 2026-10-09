// What every gsm.bio widget's binding is made with. A binding names its chart
// and nothing more:
//
//   HTMLWidgets.widget({
//       name: 'Widget_GroupComparison',
//       type: 'output',
//       factory: GsmBioWidget.factory(function(chart, settings) {
//           return BioViz.groupComparison(chart, settings);
//       }, ['statistic'])
//   });
//
// The rest is here, once: the tables are turned into rows, the chart's
// connection to R is made, and the page says which R computes its statistics.
// A saved page's connection is made from the results stored in it and nothing
// else. A widget drawn in a Shiny page by its render function stores none: its
// connection asks the R session behind the page (gsm.bio's Serve_Statistics()).
window.GsmBioWidget = (function() {
    // The Shiny input requests are sent as, and the message answers come back
    // as: the two names R/serve.R holds.
    const REQUEST = 'gsm_bio_request';
    const ANSWER = 'gsm-bio-answer';
    // How long a page waits for its Shiny session, and then for the session to
    // say it answers statistics, before saying it does not.
    const PATIENCE = 20000;

    // An error that says R was not reached, which the connection answers as
    // unavailable and never as an error of R's.
    const unreachable = function(message) {
        const error = new Error(message);
        error.unreachable = true;
        return error;
    };

    // An array of row objects to one array per column, by the rule of bio.viz's
    // own engine: every column has a value for every row, and a value that is
    // absent or not a number is null, which R reads as missing.
    const toColumns = function(records) {
        const columns = {};
        const rows = Array.isArray(records) ? records : [];
        rows.forEach(function(record, index) {
            Object.keys(record || {}).forEach(function(name) {
                if (!(name in columns))
                    columns[name] = new Array(rows.length).fill(null);
                const value = record[name];
                columns[name][index] = value === undefined || Number.isNaN(value) ? null : value;
            });
        });
        return columns;
    };

    // What reaches the R session behind a Shiny page: one for the page, shared
    // by every widget on it. `shiny` is the page's Shiny object and `on` hears
    // the page's events; both are given so a test can stand in for them.
    const shinyEngine = function(shiny, on) {
        const waiting = new Map();
        let last = 0;
        let queue = [];
        let ended = false;
        const connected = function() {
            return Boolean(shiny.shinyapp && typeof shiny.shinyapp.isConnected === 'function' && shiny.shinyapp.isConnected());
        };
        const lost = function() {
            return unreachable('the session with the server has ended; reload the page');
        };
        shiny.addCustomMessageHandler(ANSWER, function(answer) {
            const asked = answer && waiting.get(answer.id);
            if (!asked)
                return;
            waiting.delete(answer.id);
            if (answer.ok === true)
                asked.resolve(answer.value === undefined ? null : answer.value);
            else
                asked.reject(new Error(typeof answer.message === 'string' && answer.message ? answer.message : 'R stopped without a message'));
        });
        on('shiny:disconnected', function() {
            ended = true;
            waiting.forEach(function(asked) {
                asked.reject(lost());
            });
            waiting.clear();
            queue = [];
        });
        on('shiny:connected', function() {
            ended = false;
        });
        // Requests made in one turn of the page's loop go as one input, so none
        // is lost to another sent under the same name.
        const send = function() {
            const batch = queue;
            queue = [];
            if (batch.length)
                shiny.setInputValue(REQUEST, JSON.stringify(batch), { priority: 'event' });
        };
        const ask = function(request) {
            return new Promise(function(resolve, reject) {
                if (ended || !connected())
                    return reject(lost());
                last += 1;
                request.id = last;
                waiting.set(last, { resolve: resolve, reject: reject });
                queue.push(request);
                if (queue.length === 1)
                    setTimeout(send, 0);
            });
        };
        // The session, once the page has one.
        const session = function() {
            return new Promise(function(resolve, reject) {
                if (ended)
                    return reject(lost());
                if (connected())
                    return resolve();
                const timer = setTimeout(function() {
                    reject(unreachable('the page has no session with the server'));
                }, PATIENCE);
                on('shiny:connected', function() {
                    clearTimeout(timer);
                    resolve();
                });
            });
        };
        return {
            // A session answers statistics only when its server function calls
            // Serve_Statistics(): the page asks, and a session that does not say
            // so in time is one that answers none.
            start: function() {
                return session().then(function() {
                    return new Promise(function(resolve, reject) {
                        const timer = setTimeout(function() {
                            reject(unreachable('its session answers no statistics: Serve_Statistics() is not called in the server function'));
                        }, PATIENCE);
                        ask({ hello: true }).then(function() {
                            clearTimeout(timer);
                            resolve();
                        }, function(error) {
                            clearTimeout(timer);
                            reject(error);
                        });
                    });
                });
            },
            call: function(name, request) {
                return ask({
                    name: name,
                    columns: toColumns(request && request.data),
                    args: (request && request.args) || {}
                });
            }
        };
    };

    // The page's one engine, made the first time a widget needs it.
    let pageEngine = null;
    const engineOfPage = function() {
        if (!pageEngine) {
            pageEngine = shinyEngine(window.Shiny, function(event, heard) {
                window.jQuery(document).on(event, heard);
            });
        }
        return pageEngine;
    };

    // Whether this widget's statistics are the Shiny session's to answer: R
    // made it so, and the page is a Shiny page.
    const isServed = function(statistics) {
        return statistics.served === true && Boolean(window.Shiny) &&
            typeof window.Shiny.setInputValue === 'function' && Boolean(window.jQuery);
    };

    // Which R answers on the server: the R that made the widget is the R of
    // the session behind the page.
    const servedBy = function(statistics) {
        const by = statistics.computed_by || {};
        const record = {};
        if (typeof by.r_version === 'string' && by.r_version)
            record.r_version = by.r_version;
        if (record.r_version && typeof by.gsm_bio_version === 'string' && by.gsm_bio_version)
            record.gsm_bio_version = by.gsm_bio_version;
        return record.r_version ? record : undefined;
    };

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
    const provenance = function(statistics, served) {
        const by = statistics.computed_by || {};
        if (served)
            return 'Statistics: computed on request by R ' + by.r_version + ' with gsm.bio ' + by.gsm_bio_version +
                ' on this server. The rows a chart draws are sent to it with each request, and nothing is stored with this page.';
        const when = String(by.computed_at || '').replace('T', ' ').replace(/:\d\dZ$/, ' UTC');
        const who = 'R ' + by.r_version + ' with gsm.bio ' + by.gsm_bio_version + ' on ' + when;
        if (!statistics.results.length)
            return 'Statistics: no result was stored with this page (' + who +
                '). No R runs here, so a statistic asked for in it says that statistics are unavailable.';
        return 'Statistics: computed by ' + who + ' and stored with this page. No R runs ' +
            'here, so a view that was not computed says that statistics are unavailable.';
    };

    // `make(chart, settings)` makes the widget's chart in an element.
    // `functions` names the chart's settings that each name an R function it
    // asks: when every one of them is null the chart asks R for nothing, and
    // the page says nothing of statistics.
    const factory = function(make, functions) {
        return function(el, width, height) {
            let instance = null;
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
                        // In a saved page the connection is made from the stored
                        // results and nothing else: no R is started and nothing is
                        // fetched. The record of which R computed them goes with
                        // every stored answer, and the chart's own footnote names
                        // the versions. Drawn by a render function in a Shiny page,
                        // the widget stores none and its connection asks the session.
                        settings.connection = isServed(statistics)
                            ? BioViz.r.createConnection({ server: { engine: engineOfPage(), computedBy: servedBy(statistics) } })
                            : BioViz.r.createConnection({ results: statistics.results, computedBy: statistics.computed_by });
                        instance = make(chart, settings);
                        // The tables the chart is given: the results, the
                        // participants when there are any, and the outcomes
                        // table for a chart that reads one.
                        const tables = { results: toRows(x.dfResults) };
                        const participants = toRows(x.dfParticipants);
                        if (participants)
                            tables.participants = participants;
                        if (x.dfOutcomes)
                            tables.outcomes = toRows(x.dfOutcomes);
                        instance.init(tables);
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

                    const asks = functions.some(function(name) {
                        return settings[name] !== null;
                    });
                    if (asks) {
                        const note = document.createElement('p');
                        note.className = 'gsm-bio-provenance';
                        note.style.cssText = 'margin:.75rem 0 0;font:.8rem/1.4 system-ui,-apple-system,' +
                            '"Segoe UI",sans-serif;color:#52616f;';
                        note.dataset.storedResults = String(statistics.results.length);
                        note.dataset.served = String(isServed(statistics));
                        note.textContent = provenance(statistics, isServed(statistics));
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
        };
    };

    return { factory: factory, shinyEngine: shinyEngine, toColumns: toColumns };
})();
