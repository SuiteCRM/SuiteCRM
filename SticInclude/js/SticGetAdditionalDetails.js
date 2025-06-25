SUGAR.util.getAdditionalDetails = function(bean, id) {
  if (bean && id) {
    var url = 'index.php?to_pdf=1&module=Home&action=AdditionalDetailsRetrieve&bean=' + bean + '&id=' + id;
    //var title = '<div class="qtip-title-text">' + id + '</div>' + '<div class="qtip-title-buttons">' + '</div>';
    var body = '';

    // Check if we're in a popup window
    var isPopup = window.location.href.indexOf('action=Popup') !== -1;

    // Create an instance of qTip2 and configure it with the initial content.
    $(document).qtip({
      content: {
        title: {
          text: SUGAR.language.get("app_strings", "LBL_ADDITIONAL_DETAILS"),
          button: true,
        },
        text: body,
      },
      events: {
        render: function(event, api) {
          var divBody = '#qtip-' + api.id + '-content';
          $(divBody).html(SUGAR.language.translate("app_strings", "LBL_LOADING"));
          $.ajax(url)
            .done(function(data) {
              // Remove everything before the first "}"
              data = data.substring(data.indexOf("{"));
              // Remove everything after the last "}"
              data = data.substring(0, data.lastIndexOf("}") + 1);
              // Parse string as JSON
              var parsedData = JSON.parse(data);
              var contentDiv = $('<div/>').html(parsedData.body);
              
              // If we're in a popup, disable all links
              if (isPopup) {
                contentDiv.find('a').each(function() {
                  var $link = $(this);
                  var text = $link.text();
                  $link.replaceWith('<span class="disabled-link">' + text + '</span>');
                });
              }
        
              var titleDiv = contentDiv.find("h2").remove().html();
              var tryContent = contentDiv.clone().find("h2").remove().end().html();
              // Update qTip information
              var divTitle = '#qtip-' + api.id + '-title';
              $(divTitle).html(titleDiv);
              api.set('content.text', tryContent);
            })
            .fail(function() {
              var divBody = '#qtip-' + api.id + '-content';
              $(divBody).html(SUGAR.language.translate("app_strings", "LBL_ADDITIONAL_DETAILS_ERROR_GENERAL_TITLE"));
            });
        },
      },
      position: {
        my: 'top right',
        at: 'bottom left',
        target: $('#adspan_' + id + ' span'),
        viewport: $(window),
        adjust: {
          method: 'flip shift',
          resize: true
        }
      },
      show: {
        event: 'mouseenter',
        ready: true,
        solo: true,
      },
      hide: {
        event: 'mouseleave unfocus',
        fixed: true,
        delay: 200,
        target: $('#adspan_' + id + ' span'),
      },
      style: {
        width: 224,
        padding: 5,
        color: 'blue',
        textAlign: 'left',
        border: { width: 1, radius: 3 },
        tip: {
          corner: true,
          mimic: false,
          width: 8,
          height: 8,
          border: true,
          offset: 0
        },
        classes: {
          tooltip: 'ui-widget',
          tip: 'ui-widget',
          title: 'ui-widget-header',
          content: 'ui-widget-content',
        },
      },
    });
  }
};
