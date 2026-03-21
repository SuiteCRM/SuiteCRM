if(mozaik || $.mozaik) {
    throw 'MOZAIK: conflict detected';
}

// mozaik helpers (private)
var mozaik = {
    getThumbnailListHTML: function(id, thumbs, base) {
        var html = '<ul class="mozaik-thumbs" id="' + id + '" title="'+SUGAR.language.translate('Campaigns', 'LBL_CLICK_TO_ADD')+'">';
        for(var name in thumbs) {
            var e = thumbs[name];
            html += '<li class="mozaik-thumbnail" data-name="' + name + '">' + (e.thumbnail ? '<img src="' + base + e.thumbnail + '" alt="' + (e.label ? e.label : '') + '" title="' + (e.label ? e.label : '') + '">' : '<span class="mozaik-thumb-label">' + (e.label ? e.label : '') + '</span>') + '</li>';
        }
        html += '</ul>';
        return html;
    },

    getEditorListHTML: function(width) {
        var left = false;
        if(width != 'initial' && parseInt(width) > 0) {
            left = true;
        }
        var html =
          '<ul class="mozaik-list">'+
          (left ? '   <li class="mozaik-width-set"><span class="handler" href="javascript:;" title="Resize" data-width="'+parseInt(width)+'"></span></li>' : '') +
          '</ul>';
        return html;
    },

    getEditorListElementHTML: function(name, innertext, ace, style, toolPlugins) {
        var html =
          '<li class="mozaik-elem" data-name="' + name + '">' +
          '	<ul class="mozaik-tools">' +
          '		<li class="mozaik-tool-btn mozaik-tool-handle"><a href="javascript:;" title="Move"></a></li>' +
          '		<li class="mozaik-tool-btn mozaik-tool-remove"><a href="javascript:;" title="Delete"></a></li>' +
          '		<li class="mozaik-tool-btn mozaik-tool-stick"><a href="javascript:;" title="Stick"></a></li>' +
          '		<li class="mozaik-tool-btn mozaik-tool-copy"><a href="javascript:;" title="Copy"></a></li>' +
          (ace ? '<li class="mozaik-tool-btn mozaik-tool-source"><a href="javascript:;" title="Source"></a></li>' : '') +
          (toolPlugins ? this.getToolPluginIconListHTML(toolPlugins, name) : '') +
          '	</ul>' + this.getMozaikInnerHTML(name, innertext, style) +
          '</li>';
        return html;
    },

    getToolPluginIconListHTML: function(toolPlugins, name) {
        var html = '';
        for(var i=0; i<toolPlugins.length; i++) {
            var plugin = toolPlugins[i];
            html += '<li class="mozaik-tool-btn mozaik-tool-'+plugin.name+'" style="background-image: url('+plugin.image+');"><a href="javascript:;" onclick="'+plugin.callback+'(this, \''+name+'\');" title="'+plugin.title+'">&nbsp;</a></li>';
        }
        return html;
    },

    getMozaikInnerHTML: function(name, innertext, style) {
        //style = ($(innertext).attr('style') ? $(innertext).attr('style') + ';' : '') + style;
        //style = style.replace(/;\s*;/, ';');
        var html = '<div class="mozaik-inner"' + (name ? ' data-name="' + name + '"' : '') + ' style="' + style + '">' + innertext + '</div>';
        return html;
    },

    getAceHTML: function() {
        var html =
          '<div id="mozaik-ace">' +
          '	<ul id="mozaik-ace-tools">' +
          '		<li id="mozaik-ace-tool-save"><a href="javascript:;" title="Save"></a></li>' +
          '		<li id="mozaik-ace-tool-cancel"><a href="javascript:;" title="Cancel"></a></li></li>' +
          '	</ul>' +
          '	<div id="mozaik-ace-editor"></div>' +
          '</div>';
        return html;
    },

    ace: null,
    aceElementId: null,

    getClearHTML: function() {
        var html = '<div class="mozaik-clear"></div>';
        return html;
    },

    refreshWidthSet: function() {
        $('.mozaik-width-set .handler').each(function(i,e){
            var width = parseInt($(e).attr('data-width'));
            var left = (($(e).closest('.mozaik-width-set').width()-width) / 2) - 25;
            $(e).css('left', left+'px');
        });
    },

    currentWidth: false,

    setWidth: function(width) {
        if(parseInt(width) <= 0) {
            width: 'initial';
        }
        if(width) {
            mozaik.currentWidth = width;
        }
        if(mozaik.currentWidth) {
            $('.mozaik-width-set .handler').each(function (i, e) {
                $(e).attr('data-width', parseInt(mozaik.currentWidth));
                $(e).closest('.mozaik-list').find('.mozaik-inner').css('max-width', mozaik.currentWidth);
                $(e).closest('.mozaik-list').find('.mozaik-inner').css('width', mozaik.currentWidth);
                $(e).closest('.mozaik-list').find('.mozaik-inner').css('margin', '0 auto');
            });
            mozaik.refreshWidthSet();
        }
    }

};


var plgBackground = {

    name: 'background',
    title: 'Background color',
    image: 'img/paint.png',
    callback: 'plgBackground.onClick',

    onClick: function(elem, name) {
        if(!$(elem).attr('data-initialized')) {
            // TODO : add colorpicker to packagist and composer
            var $mozaikInner = $(elem).closest('.mozaik-elem').find('.mozaik-inner');
            $(elem).ColorPicker({
                onShow: function (colpkr) {
                    //Function to convert hex format to a rgb color
                    function rgb2hex(orig, hash){
                        if(typeof hash == 'undefined' || !hash) {
                            hash = false;
                        }
                        var rgb = orig.replace(/\s/g,'').match(/^rgba?\((\d+),(\d+),(\d+)/i);
                        var ret = (rgb && rgb.length === 4) ? "#" +
                        ("0" + parseInt(rgb[1],10).toString(16)).slice(-2) +
                        ("0" + parseInt(rgb[2],10).toString(16)).slice(-2) +
                        ("0" + parseInt(rgb[3],10).toString(16)).slice(-2) : orig;
                        while(!hash && ret.match(/^\#/)) {
                            ret = ret.replace(/^\#/, '');
                        }
                        return ret;
                    }
                    $(this).ColorPickerSetColor(rgb2hex($mozaikInner.css('background-color')));
                },
                onChange: function(rbg, hex){
                    $mozaikInner.css('background-color', '#'+hex);
                    $mozaikInner.find('td').css('background-color', '#'+hex);
                },
            });
            $(elem).attr('data-initialized', '1');
            $(elem).click();
        }
    },

};

(function($){
    $.fn.mozaik = function(options) {

        /**
         * TinyMCE configuration for Mozaik drag-and-drop email template builder
         *
         * This configuration is intentionally different from standard editors because:
         * - undo/redo: Required for drag-drop template building operations
         * - emoticons: Allows template personalization with emojis
         * - media: Enables video/audio embedding in email templates
         * - print/preview: Essential for template preview before use
         * - Split toolbars: Better organization for template building context
         *
         * DO NOT standardize this config with other TinyMCE instances
         */
        var tinyMCESettings = {
            plugins: [
                'advlist autolink lists link image charmap print preview hr anchor pagebreak',
                'searchreplace wordcount visualblocks visualchars code fullscreen',
                'insertdatetime media nonbreaking save table contextmenu directionality',
                'emoticons template paste textcolor colorpicker textpattern imagetools'
            ],
            toolbar1: 'insertfile undo redo | styleselect | bold italic | alignleft aligncenter alignright alignjustify | bullist numlist outdent indent',
            toolbar2: 'print preview media | forecolor backcolor | image | emoticons | table | link | fontsizeselect',
            table_toolbar: "",
            image_advtab: true,
            textcolor_map: [
                "000000", "Black",
                "993300", "Burnt orange",
                "333300", "Dark olive",
                "003300", "Dark green",
                "003366", "Dark azure",
                "000080", "Navy Blue",
                "333399", "Indigo",
                "333333", "Very dark gray",
                "800000", "Maroon",
                "FF6600", "Orange",
                "808000", "Olive",
                "008000", "Green",
                "008080", "Teal",
                "0000FF", "Blue",
                "666699", "Grayish blue",
                "808080", "Gray",
                "FF0000", "Red",
                "FF9900", "Amber",
                "99CC00", "Yellow green",
                "339966", "Sea green",
                "33CCCC", "Turquoise",
                "3366FF", "Royal blue",
                "800080", "Purple",
                "999999", "Medium gray",
                "FF00FF", "Magenta",
                "FFCC00", "Gold",
                "FFFF00", "Yellow",
                "00FF00", "Lime",
                "00FFFF", "Aqua",
                "00CCFF", "Sky blue",
                "993366", "Red violet",
                "FFFFFF", "White",
                "FF99CC", "Pink",
                "FFCC99", "Peach",
                "FFFF99", "Light yellow",
                "CCFFCC", "Pale green",
                "CCFFFF", "Pale cyan",
                "99CCFF", "Light sky blue",
                "CC99FF", "Plum"
            ],

            file_browser_callback: function(field_name, url, type, win, e) {
                mozaik.uploadPathField = field_name;
                if(type=='image') {
                    $('form#upload_form input[type="file"]').each(function(i,e){
                        if($(e).css('display') != 'none') {
                            $(e).click();
                        }
                    });
                }
            }
        };

        var settings = $.extend({
            base: '',
            thumbs: {
                headline: {thumbnail: 'tpls/default/thumbs/headline.png', label: 'Headline', tpl: 'tpls/default/headline.html'},
                content1: {thumbnail: 'tpls/default/thumbs/content1.png', label: 'Content', tpl: 'tpls/default/content1.html'},
                content3: {thumbnail: 'tpls/default/thumbs/content3.jpg', label: 'Three column', tpl: 'tpls/default/content3.html'},
                content2: {thumbnail: 'tpls/default/thumbs/content2.jpg', label: 'Two column', tpl: 'tpls/default/content2.html'},
                image3: {thumbnail: 'tpls/default/thumbs/image3.jpg', label: 'Three column with image', tpl: 'tpls/default/image3.html'},
                image2: {thumbnail: 'tpls/default/thumbs/image2.jpg', label: 'Two column with image', tpl: 'tpls/default/image2.html'},
                image1left: {thumbnail: 'tpls/default/thumbs/image1left.jpg', label: 'Content with image (left)', tpl: 'tpls/default/image1left.html'},
                image1right: {thumbnail: 'tpls/default/thumbs/image1right.jpg', label: 'Content with image (right)', tpl: 'tpls/default/image1right.html'},
                footer: {thumbnail: 'tpls/default/thumbs/footer.png', label: 'Footer', tpl: 'tpls/default/footer.html'},
            },
            editables: 'editable',
            style: 'tpls/default/styles/default.css',
            namespace: false,
            ace: true,
            width: '600px',
            toolPlugins: [plgBackground],
            uploadPathField: null,
            widthSetField: '#mozaik_width_set', // you can override the valuse setter input field selector in PHP also
        }, options);

        // parse back saved width value
        var widths = [];
        var max = -1;
        var widthAtMax = -1;
        $(this).find('.mozaik-inner').each(function(i,e) {
            if(typeof widths[$(e).css('width')] === 'undefined') {
                widths[$(e).css('width')] = 0;
            }
            widths[$(e).css('width')]++;
            if(widths[$(e).css('width')]>max) {
                max = widths[$(e).css('width')];
                widthAtMax = parseInt($(e).css('width'));
            }
        });
        if(max > 0) {
            settings.width = widthAtMax;
        }

        // initialize valueset input field
        $(settings.widthSetField).keyup(function(){
            mozaik.setWidth(parseInt($(this))+'px');
        });
        $(settings.widthSetField).mouseup(function(){
            mozaik.setWidth(parseInt($(this))+'px');
        });
        $(settings.widthSetField).val(parseInt(settings.width));

        if(typeof settings.tinyMCE == 'undefined') {
            settings.tinyMCE = tinyMCESettings;
        }
        else {
            settings.tinyMCE = $.extend(settings.tinyMCE, tinyMCESettings);
        }

        // add ace editor
        if($('#mozaik-ace').length == 0) {
            $('body').append(mozaik.getAceHTML());
            if(typeof ace != 'undefined') {
                mozaik.ace = ace.edit('mozaik-ace-editor');
                mozaik.ace.setTheme("ace/theme/monokai");
                mozaik.ace.getSession().setMode("ace/mode/html");
                $('#mozaik-ace-tool-save').click(function () {
                    $('#' + mozaik.aceElementId).html(mozaik.ace.getValue());
                    $('#mozaik-ace').hide();
                });
                $('#mozaik-ace-tool-cancel').click(function () {
                    $('#mozaik-ace').hide();
                });
            }
            else {
                //console.error('ACE Editor javascript is not defined for Mozaik on this page');
            }
        }

        // template styles added?
        var style = false;

        return this.each(function(i, e){

            var insertTemplateSection = function(name){
                var regex = /^string:/i;
                if(settings.thumbs[name].tpl.match(regex)) {
                    addEditorListElement(name, settings.thumbs[name].tpl.replace(regex, ''), true, settings.toolPlugins);
                    onResize();
                }
                else {
                    var url = settings.base + settings.thumbs[name].tpl;
                    $.get(url, function (resp) {
                        addEditorListElement(name, resp, true, settings.toolPlugins);
                        onResize();
                    });
                }
                $(window).mouseup();
            }

            // namespace for styles
            var namespace = settings.namespace;

            // add unique id attribute
            if(!$(e).attr('id')) {
                $(e).attr('id', namespace ? namespace : 'mozaik-' + i);
            }
            namespace = $(e).attr('id');

            $(e).addClass('mozaik');

            // store and remove inner html and add preloader...
            var html = $(e).html();
            $(e).html('');

            // add 'canvas' area
            $(e).append(mozaik.getEditorListHTML(settings.width));
            mozaik.refreshWidthSet();

            var $mozaik = $(e).find('.mozaik-list').first();

            // add template styles
            if(!style && $('#mozaik-style-' + i).length==0) {
                $.get(settings.base + settings.style, function(css){
                    if(css) {
                        var splits = css.split('}');
                        for(var i=0; i<splits.length-1; i++) {
                            splits[i] = "\n" + '#' + namespace + ' ' + splits[i];
                        }
                        style = splits.join('}');
                        $mozaik.prepend('<style id="mozaik-style-' + i + '" type="text/css">' + style + '</style>');
                    }
                    if(!style) {
                        style = true;
                    }
                });
            }


            // create thumbnails
            var mozaikThumbsId = 'mozaik-thumbs-' + i;
            $(e).prepend(mozaik.getThumbnailListHTML(mozaikThumbsId, settings.thumbs, settings.base));
            $('.mozaik-thumbnail').draggable({
                helper: 'clone'
            });
            $('.mozaik-thumbnail').click(function(e){
                var name = $(this).attr('data-name');
                insertTemplateSection(name);
            });

            // set area layout
            //if(!$mozaik.css('min-width')) $mozaik.css('min-width', 600);
            //if(!$mozaik.css('min-height')) $mozaik.css('min-height', 300);
            //if(!$mozaik.css('background-color')) $mozaik.css('background-color', '#ddd');

            // add template particular
            var addEditorListElement = function(name, html, scrollDown, toolPlugins, style) {
                style = (style ? style + ';' : '') + 'max-width:' + settings.width;
                style = style.replace(/;\s*;/, ';');
                var listElemHTML = mozaik.getEditorListElementHTML(name, html, settings.ace, style, toolPlugins);
                $mozaik.append(listElemHTML);
                mozaik.setWidth();
                var editables = name && settings.thumbs[name].editables ? settings.thumbs[name].editables.split(',') : settings.editables.split(',');
                $.each(editables, function(i,e){
                    var sels = '.mozaik-inner'; //, .mozaik-inner ' + e;
                    var config = settings.tinyMCE;
                    config.selector = sels;
                    config.inline = true;
                    tinymce.init(config);
                });

                // initialize toolbar
                $mozaik.find('.mozaik-elem:last-child .mozaik-tool-remove').bind('click', function(){
                    $(this).closest('.mozaik-elem').remove();
                });
                $mozaik.find('.mozaik-elem:last-child .mozaik-tool-stick').bind('click', function(){
                    var next = $(this).closest('.mozaik-elem').next();
                    if(!next.hasClass('mozaik-elem')) {
                        alert('You can\'t stick the last element.');
                    }
                    else {
                        var html = next.find('.mozaik-inner').html();
                        $(this).closest('.mozaik-elem').find('.mozaik-inner').append(html);
                        next.remove();
                    }
                    return false;
                });
                $mozaik.find('.mozaik-elem:last-child .mozaik-tool-source').bind('click', function(){
                    var html = tinymce.activeEditor.getContent();
                    mozaik.ace.setValue(html);
                    $('#mozaik-ace').show();
                });
                $mozaik.find('.mozaik-elem:last-child .mozaik-tool-copy').bind('click', function(){
                    var html = $(this).closest('.mozaik-elem').find('.mozaik-inner').html();
                    addEditorListElement(false, html, scrollDown, toolPlugins);
                });

                if(scrollDown) {
                    var innerHeight = 0;
                    $mozaik.find('.mozaik-elem').each(function(i,e){
                        innerHeight+= $(e).outerHeight();
                    });
                    $mozaik.animate({
                        scrollTop: innerHeight
                    });
                }

                // clears

                $mozaik.find('.mozaik-clear').remove();
                $mozaik.find('.mozaik-inner').append(mozaik.getClearHTML());
            };

            // put elements back which was inside of the editor area originally
            if(html) {
                var length = 0;
                try {
                    length = $(html).find('.mozaik-inner').length;
                }
                catch(e){
                    length = -1;
                }
                if(length == 0) {
                    html = mozaik.getMozaikInnerHTML(false, html, 'max-width:' + settings.width);
                }
                else if(length == -1) {
                    html = mozaik.getEditorListElementHTML(false, html, settings.ace, 'max-width:' + settings.width, settings.toolPlugins);
                }
                if(!$(html).find('.mozaik-inner').length) {
                    html = '<div>' + html + '</div>'
                }
                $(html).find('.mozaik-inner').each(function(i,e){
                    //$mozaik.append(mozaik.getEditorListElementHTML($(e).attr('data-name'), $(e).html()));
                    addEditorListElement($(e).attr('data-name') ? $(e).attr('data-name') : false, $(e).html(), null, settings.toolPlugins, $(e).attr('style'));
                });

            }

            // reset layout positions and clears
            var onResize = function() {

                // layout width
                $mozaik.css({
                    width: $(e).outerWidth(true) - $('#' + mozaikThumbsId).outerWidth(true) - ($mozaik.outerWidth()-$mozaik.width()) - (parseInt($(e).css('border-left-width'))+parseInt($(e).css('border-right-width'))) //,
                    //height: $mozaik.parent().outerHeight(true) - ($mozaik.outerHeight(true)-$mozaik.height())
                });

                // srollbars on editor area
                if($mozaik.outerWidth(true)>$mozaik.parent().innerWidth()) {
                    $mozaik.css('overflow-x', 'scroll');
                }
                else {
                    $mozaik.css('overflow-x', 'auto');
                }

            };
            onResize();

            // make drag 'n' drop dropp area
            $mozaik.droppable({
                accept: '.mozaik-thumbnail',
                drop: function(event, ui) {
                    var name = ui.draggable.attr('data-name');
                    insertTemplateSection(name);
                }
            });

            $mozaik.sortable({
                // add handler to list, add tinymce to editable area (may default editable selector or *)
                handle: '.mozaik-tool-handle'
            });


            // styles
            $('body').imagesLoaded(function(){
                //$(e).css({
                //width: 'auto'
                //overflow: 'hidden'
                //});

                //$('#'+mozaikThumbsId).outerHeight($('#'+mozaikThumbsId).parent().outerHeight());
                //$mozaik.outerHeight($mozaik.parent().outerHeight());

                onResize();

                $(window).resize(function() {
                    onResize();
                });
                $(window).mousemove(function() {
                    onResize();
                });

                $(e).find('.mozaik-thumbs').show();
                //$(e).find('.mozaik-preloader').hide();

                mozaik.refreshWidthSet();
            });

        });

    };

}(jQuery));


/*
 * getStyleObject Plugin for jQuery JavaScript Library
 * From: http://upshots.org/?p=112
 */

(function($){
    $.fn.getStyleObject = function(){
        var dom = this.get(0);
        var style;
        var returns = {};
        if(window.getComputedStyle){
            var camelize = function(a,b){
                return b.toUpperCase();
            };
            style = window.getComputedStyle(dom, null);
            for(var i = 0, l = style.length; i < l; i++){
                var prop = style[i];
                var camel = prop.replace(/\-([a-z])/g, camelize);
                var val = style.getPropertyValue(prop);
                returns[camel] = val;
            };
            return returns;
        };
        if(style = dom.currentStyle){
            for(var prop in style){
                returns[prop] = style[prop];
            };
            return returns;
        };
        return this.css();
    }
})(jQuery);

(function($){
    $.fn.getMozaikValue = function(options) {

        var settings = $.extend({
            inlineStyles: false,
            applyStyles: true,
            width: '600px'
        }, options);

        var ret = [];
        this.each(function(i, e){
            // todo: test for mozaik already initialized on this element?!

            if(settings.applyStyles) {
                $(e).find('style').each(function (j, styleElem) {
                    var css = $(styleElem).html();
                    var splits = css.split('}');
                    for (var k = 0; k < splits.length - 1; k++) {
                        var parts = splits[k].split('{');
                        var sel = parts[0];
                        var defs = parts[1];
                        $(sel).each(function(l, el){
                            if(($(el).hasClass('mozaik-inner') || $(el).closest('.mozaik-inner').length) && !$(el).hasClass('mozaik-tools') && !$(el).closest('.mozaik-tools').length) {

                                // correct inline font-size and line height style
                                var fontFamily = $(el).css('font-family');
                                var fontSize = $(el).css('font-size');
                                var lineHeight = $(el).css('line-height');
                                var color = $(el).css('color');
                                $(el).css('font-family', fontFamily);
                                $(el).css('font-size', fontSize);
                                $(el).css('line-height', lineHeight);
                                $(el).css('color', color);

                                // correct template section margins and paddings..
                                var padding = $(el).css('padding-top') + ' ' + $(el).css('padding-right') + ' ' + $(el).css('padding-bottom') + ' ' + $(el).css('padding-left');
                                var margin = $(el).css('margin-top') + ' ' + $(el).css('margin-right') + ' ' + $(el).css('margin-bottom') + ' ' + $(el).css('margin-left');
                                $(el).css('padding', padding);
                                $(el).css('margin', margin);

                                if($(el).hasClass('mozaik-clear')) {
                                    $(el).css('height', '0');
                                }
                                if($(el).hasClass('mozaik-inner')) {
                                    if($(el).css('width', '100%')) {
                                        $(el).css('width', 'initial');
                                    }

                                    // generated style correction for save..
                                    $(el).css('max-width', $(el).css('max-width'));
                                    $(el).css('width', $(el).css('width'));
                                    $(el).css('background-color', $(el).css('background-color'));
                                    $(el).css('margin', parseInt($(el).css('margin-top')) + 'px auto '+parseInt($(el).css('margin-bottom')) + 'px auto');
                                    $(el).css('margin-top', parseInt($(el).css('margin-top')) + 'px');
                                    $(el).css('margin-bottom', parseInt($(el).css('margin-bottom')) + 'px');
                                    $(el).css('margin-left', 'auto');
                                    $(el).css('margin-right', 'auto');
                                }

                                // corrigate inline styles..
                                var style = defs + (typeof $(el).attr('style') != 'undefined' && $(el).attr('style') ? $(el).attr('style') + ';' : '');
                                style = style.replace(/;\s*;/, ';');
                                var cssProps = style.split(';');
                                for (var l=0; l < cssProps.length - 1; l++) {
                                    var cssDef = cssProps[l].split(':');
                                    var cssProp = cssDef[0];
                                    var cssValue = cssDef[1];
                                    $(el).css(cssProp, $(el).css(cssProp));
                                }


                            }
                        })
                    }
                });
            }


            var html = '';
            $(e).find('ul.mozaik-list div.mozaik-inner').each(function(i,e){
                if(settings.inlineStyles) {
                    var tmpHtml = $(e).html();
                    var tmpStyle = $(e).attr('style');

                    //$(e).ApplyCssInline();
                    var styles = $(e).getStyleObject();
                    $(e).css(styles);

                    $(e).find('*').each(function(i,e){
                        var styles = $(e).getStyleObject();
                        $(e).css(styles);
                    });

                    html += mozaik.getMozaikInnerHTML(false, $(e).html(), $(e).attr('style')); //'<div class="mozaik-inner" style="' + $(e).attr('style') + '">' + $(e).html() + '</div>';
                    $(e).html(tmpHtml);
                    $(e).attr('style', tmpStyle);
                }
                else {
                    html += mozaik.getMozaikInnerHTML(false, $(e).html(), $(e).attr('style'));
                }
            });
            ret.push(html);
        });


        if(ret.length == 0) {
            return null;
        }
        else if(ret.length == 1) {
            return ret[0];
        }

        return ret;

    };
}(jQuery));