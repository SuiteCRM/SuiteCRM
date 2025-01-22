<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Jjwg_MapsViewMap_Display extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        // Limit URI query string parameters. Used to avoid URL length errors.
        $valid_names = array('action', 'module', 'entryPoint', 'submit', 'cron', 'geocoding_process', 'process_trigger', 'distance', 'unit_type', 'record', 'related_id', 'related_module', 'quick_address', 'display_module', 'list_id', 'uid', 'current_post');
        $url = $GLOBALS['sugar_config']['site_url'] . '/index.php?module=' . $GLOBALS['currentModule'] . '&action=map_markers';
        foreach (array_keys($_REQUEST) as $key) {
            if (in_array($key, $valid_names) && !in_array($key, array('action', 'module', 'entryPoint'))) {
                $url .= '&'.$key.'='.urlencode((string)$_REQUEST[$key]);
            }
        } ?>
<script type="text/javascript" src="modules/jjwg_Maps/javascript/jquery.iframe-auto-height.plugin.1.9.3.min.js"></script>
<script>

  function resizeDataTables() {
      $('#mapDisplayIframe').css('height: 200px;');
      setTimeout(function() {
          $('#resizeMapDisplayIframe').trigger("click");
      }, 250);
  }

  $(document).ready(function () {

    // fire iframe resize when window is resized
    var windowResizeFunction = function(resizeFunction, iframe) {
        $(window).resize(function () {
            resizeFunction(iframe);
        });
    };

    // fire iframe resize when a link is clicked
    var clickFunction = function (resizeFunction, iframe) {
        $('#resizeMapDisplayIframe').click(function () {
            resizeFunction(iframe);
            return false
        });
    };

    $('#mapDisplayIframe').iframeAutoHeight({
        debug: false,
        triggerFunctions: [
            windowResizeFunction,
            clickFunction
        ]
    });

  });

</script>

<iframe id="mapDisplayIframe" src="<?php echo $url; ?>"
	width="100%" height="800" frameborder="0" marginheight="0" marginwidth="0"
        scrolling="auto"><p>Sorry, your browser does not support iframes.</p></iframe>

<?php
    if (empty($_REQUEST['uid']) && empty($_REQUEST['current_post'])) {
        ?>
<p>IFrame:
    <a href="<?php echo htmlspecialchars($url); ?>"><?php echo $GLOBALS['mod_strings']['LBL_MAP']; ?> URL</a>
    <a href="#" id="resizeMapDisplayIframe" style="display: none;">.</a>
</p>
<?php
    } ?>


<?php
    }
}
