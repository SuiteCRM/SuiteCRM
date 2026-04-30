(function ($) {
    'use strict';

    if (typeof $ === 'undefined') {
        return;
    }

    if (!window.SuiteSweetspot) {
        window.SuiteSweetspot = {};
    }

    var SS = window.SuiteSweetspot;

    SS.state = {
        isVisible: false,
        actions: [],
        recordCache: {},
        resultsFlat: [],
        activeIndex: 0,
        lastTerm: '',
        searchTimer: null
    };

    SS.init = function () {
        SS.createDom();
        SS.bindGlobalShortcuts();
        SS.bindOutsideClick();
        SS.loadActions();
    };

    SS.createDom = function () {
        if ($('#suite_sweetspot').length) {
            return;
        }

        var html = ''
            + '<div id="suite_sweetspot_backdrop" class="suite-sweetspot-backdrop" style="display:none;"></div>'
            + '<div id="suite_sweetspot" class="suite-sweetspot" style="display:none;">'
            + '  <div class="suite-sweetspot-searchbar">'
            + '    <i class="fa fa-search suite-sweetspot-search-icon"></i>'
            + '    <input type="text" class="suite-sweetspot-input" placeholder="Type to search modules, actions, or records..."/>'
            + '    <span class="suite-sweetspot-hint">Ctrl+Shift+Space</span>'
            + '  </div>'
            + '  <ul class="suite-sweetspot-results">'
            + '    <li data-section="actions" class="suite-ss-section"><p><i class="fa fa-bolt"></i> Actions</p><ul class="suite-sweetspot-actions"></ul></li>'
            + '    <li data-section="records" class="suite-ss-section"><p><i class="fa fa-file-text"></i> Records</p><ul class="suite-sweetspot-records"></ul></li>'
            + '  </ul>'
            + '  <div class="suite-sweetspot-empty" style="display:none;"><i class="fa fa-search"></i><p>No results found</p><span>Try a different search term</span></div>'
            + '  <div class="suite-sweetspot-footer">'
            + '    <span class="suite-sweetspot-footer-hint"><kbd>↑</kbd><kbd>↓</kbd> Navigate</span>'
            + '    <span class="suite-sweetspot-footer-hint"><kbd>Enter</kbd> Select</span>'
            + '    <span class="suite-sweetspot-footer-hint"><kbd>Esc</kbd> Close</span>'
            + '  </div>'
            + '</div>';

        $('body').append(html);

        $(document).on('keyup.suite_sweetspot_input', '#suite_sweetspot .suite-sweetspot-input', function (e) {
            var code = e.which || e.keyCode;
            if (code === 27 || code === 13 || code === 38 || code === 40) {
                return;
            }
            SS.queueSearch($.trim($(this).val()));
        });

        $(document).on('click.suite_sweetspot_result', '#suite_sweetspot li[data-sweetaction="true"]', function (e) {
            e.preventDefault();
            var $a = $(this).find('a').first();
            var url = $a.data('url');
            if (!url) {
                return;
            }
            SS.hide();
            window.location = url;
        });

        $(document).on('click.suite_sweetspot_backdrop', '#suite_sweetspot_backdrop', function () {
            SS.hide();
        });
    };

    SS.bindGlobalShortcuts = function () {
        $(document).on('keydown.suite_sweetspot_global', function (e) {
            var code = e.which || e.keyCode;
            if (e.ctrlKey && e.shiftKey && code === 32) {
                e.preventDefault();
                SS.toggle();
            }
        });
    };

    SS.bindOutsideClick = function () {
        $(document).on('mousedown.suite_sweetspot_click', function (e) {
            if (!SS.state.isVisible) {
                return;
            }
            if (!$(e.target).closest('#suite_sweetspot').length) {
                SS.hide();
            }
        });
    };

    SS.show = function () {
        if (SS.state.isVisible) {
            return;
        }
        SS.state.isVisible = true;
        $('#suite_sweetspot_backdrop').fadeIn(100);
        $('#suite_sweetspot').fadeIn(100).find('input.suite-sweetspot-input').val('').focus();
        SS.state.lastTerm = '';
        SS.state.resultsFlat = [];
        SS.state.activeIndex = 0;
        SS.bindInternalKeys();
        SS.recalcMaxHeight();
        SS.updateResultsForTerm('');
    };

    SS.hide = function () {
        if (!SS.state.isVisible) {
            return;
        }
        SS.state.isVisible = false;
        $('#suite_sweetspot').fadeOut(100);
        $('#suite_sweetspot_backdrop').fadeOut(100);
        SS.unbindInternalKeys();
        SS.state.activeIndex = 0;
        SS.state.lastTerm = '';
    };

    SS.toggle = function () {
        if (SS.state.isVisible) {
            SS.hide();
        } else {
            SS.show();
        }
    };

    SS.bindInternalKeys = function () {
        $(document).on('keydown.suite_sweetspot_internal', function (e) {
            if (!SS.state.isVisible) {
                return;
            }
            var code = e.which || e.keyCode;
            if (code === 27) {
                e.preventDefault();
                SS.hide();
            } else if (code === 38) {
                e.preventDefault();
                SS.moveBackward();
            } else if (code === 40) {
                e.preventDefault();
                SS.moveForward();
            } else if (code === 13) {
                e.preventDefault();
                SS.executeActive();
            }
        });
    };

    SS.unbindInternalKeys = function () {
        $(document).off('keydown.suite_sweetspot_internal');
    };

    SS.queueSearch = function (term) {
        if (SS.state.searchTimer) {
            window.clearTimeout(SS.state.searchTimer);
        }
        SS.state.searchTimer = window.setTimeout(function () {
            SS.state.searchTimer = null;
            SS.performSearch(term);
        }, 200);
    };

    SS.performSearch = function (term) {
        term = $.trim(term || '');
        SS.state.lastTerm = term;
        SS.updateResultsForTerm(term);

        if (term === '') {
            return;
        }

        $.ajax({
            url: 'index.php',
            type: 'GET',
            dataType: 'json',
            data: { entryPoint: 'suite_sweetspot_search', term: term, limit: 5 }
        }).done(function (data) {
            if (!data || !$.isArray(data.records)) {
                return;
            }
            var now = new Date().getTime();
            $.each(data.records, function (idx, rec) {
                if (!rec || !rec.id) {
                    return;
                }
                SS.state.recordCache[rec.id] = {
                    id: rec.id,
                    name: rec.name || '',
                    module: rec.module || '',
                    url: rec.url || '',
                    _ts: now
                };
            });

            if (SS.state.isVisible && SS.state.lastTerm === term) {
                SS.updateResultsForTerm(term);
            }
        });
    };

    SS.updateResultsForTerm = function (term) {
        term = $.trim(term || '');
        var termLower = term.toLowerCase();
        var actionsMatches = [];
        var recordsMatches = [];

        if (termLower !== '') {
            $.each(SS.state.actions, function (idx, action) {
                if (!action) {
                    return;
                }
                var score = 0;
                var label = (action.label || '').toLowerCase();
                var module = (action.module || '').toLowerCase();
                var keywords = action.keywords || [];
                var hay = label + ' ' + module;

                $.each(keywords, function (_, kw) {
                    var kwLower = (kw || '').toLowerCase();
                    if (kwLower === termLower) {
                        score = 100;
                        return false;
                    } else if (kwLower.indexOf(termLower) === 0) {
                        score = Math.max(score, 80);
                    } else if (kwLower.indexOf(termLower) !== -1) {
                        score = Math.max(score, 60);
                    }
                });

                if (label.indexOf(termLower) === 0) {
                    score = Math.max(score, 50);
                } else if (label.indexOf(termLower) !== -1) {
                    score = Math.max(score, 30);
                }

                if (module.indexOf(termLower) !== -1) {
                    score = Math.max(score, 20);
                }

                if (hay.indexOf(termLower) !== -1 && score === 0) {
                    score = 10;
                }

                if (score > 0) {
                    actionsMatches.push({
                        type: 'action',
                        id: action.id,
                        label: action.label || '',
                        module: action.module || '',
                        url: action.url || '',
                        score: score
                    });
                }
            });

            actionsMatches.sort(function (a, b) {
                if (a.score !== b.score) {
                    return (b.score || 0) - (a.score || 0);
                }
                return (a.label || '').localeCompare(b.label || '');
            });

            $.each(SS.state.recordCache, function (id, rec) {
                if (!rec) {
                    return;
                }
                var hay = ((rec.name || '') + ' ' + (rec.module || '')).toLowerCase();
                if (hay.indexOf(termLower) !== -1) {
                    recordsMatches.push({
                        type: 'record',
                        id: rec.id,
                        label: rec.name || '',
                        module: rec.module || '',
                        url: rec.url || ''
                    });
                }
            });
        }

        SS.renderResults(actionsMatches, recordsMatches);
    };

    SS.renderResults = function (actionsMatches, recordsMatches) {
        var $popup = $('#suite_sweetspot');
        var $actionsUl = $popup.find('.suite-sweetspot-actions');
        var $recordsUl = $popup.find('.suite-sweetspot-records');
        $actionsUl.empty();
        $recordsUl.empty();
        SS.state.resultsFlat = [];
        SS.state.activeIndex = 0;
        var idx = 0;

        $.each(actionsMatches, function (_, item) {
            var $li = $('<li>').attr('data-sweetaction', 'true');
            var $a = $('<a>').attr('href', item.url || 'javascript:void(0);').attr('data-url', item.url || '');
            $a.append($('<span>').addClass('suite-ss-label').text(item.label));
            $a.append($('<span>').addClass('suite-ss-meta').text(item.module));
            $li.append($a);
            $actionsUl.append($li);
            SS.state.resultsFlat.push({ index: idx++, url: item.url, $el: $li });
        });

        $.each(recordsMatches, function (_, item) {
            var $li = $('<li>').attr('data-sweetaction', 'true');
            var $a = $('<a>').attr('href', item.url || 'javascript:void(0);').attr('data-url', item.url || '');
            $a.append($('<span>').addClass('suite-ss-label').text(item.label));
            $a.append($('<span>').addClass('suite-ss-meta').text(item.module));
            $li.append($a);
            $recordsUl.append($li);
            SS.state.resultsFlat.push({ index: idx++, url: item.url, $el: $li });
        });

        var hasResults = actionsMatches.length > 0 || recordsMatches.length > 0;
        $popup.find('li[data-section="actions"]').toggle(actionsMatches.length > 0);
        $popup.find('li[data-section="records"]').toggle(recordsMatches.length > 0);
        $popup.find('.suite-sweetspot-empty').toggle(!hasResults && SS.state.lastTerm !== '');

        if (SS.state.resultsFlat.length > 0) {
            SS.state.activeIndex = 0;
            SS.updateActive();
        } else {
            SS.state.activeIndex = -1;
        }
    };

    SS.moveForward = function () {
        var max = SS.state.resultsFlat.length;
        if (max === 0) {
            return;
        }
        SS.state.activeIndex = (SS.state.activeIndex + 1) % max;
        SS.updateActive();
    };

    SS.moveBackward = function () {
        var max = SS.state.resultsFlat.length;
        if (max === 0) {
            return;
        }
        SS.state.activeIndex = (SS.state.activeIndex - 1 + max) % max;
        SS.updateActive();
    };

    SS.updateActive = function () {
        var list = SS.state.resultsFlat;
        var idx = SS.state.activeIndex;
        $('#suite_sweetspot li[data-sweetaction="true"]').removeClass('active');
        if (!list || list.length === 0 || idx < 0 || idx >= list.length) {
            return;
        }
        list[idx].$el.addClass('active');
    };

    SS.executeActive = function () {
        var list = SS.state.resultsFlat;
        var idx = SS.state.activeIndex;
        if (!list || list.length === 0 || idx < 0 || idx >= list.length) {
            return;
        }
        var entry = list[idx];
        if (!entry || !entry.url) {
            return;
        }
        SS.hide();
        window.location = entry.url;
    };

    SS.recalcMaxHeight = function () {
        var winH = $(window).height() || 600;
        var maxH = Math.max(200, Math.min(520, winH - 100));
        $('#suite_sweetspot').css('max-height', maxH + 'px');
        $('#suite_sweetspot .suite-sweetspot-results').css('max-height', (maxH - 60) + 'px');
    };

    SS.loadActions = function () {
        $.ajax({
            url: 'index.php',
            type: 'GET',
            dataType: 'json',
            data: { entryPoint: 'suite_sweetspot_actions' }
        }).done(function (data) {
            if (!data || !$.isArray(data.actions)) {
                return;
            }
            SS.state.actions = data.actions;
        });
    };

    $(window).on('resize.suite_sweetspot', function () {
        if (SS.state.isVisible) {
            SS.recalcMaxHeight();
        }
    });

    $(function () {
        SS.init();
    });
}(jQuery));
