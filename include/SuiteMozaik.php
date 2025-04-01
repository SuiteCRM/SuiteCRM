<?php

#[\AllowDynamicProperties]
class SuiteMozaik
{
    private $mozaikPath = 'include/javascript/mozaik';
    private $vendorPath;

    private static $defaultThumbnails = array(
        'headline' => array(
            'label' => 'Headline',
            //'tpl' => 'tpls/default/headline.html',
            'tpl' => 'string:<p><h1>Add your headline here..</h1></p>',
            'thumbnail' => 'tpls/default/thumbs/headline.png',
        ),
        'content' => array(
            'label' => 'Content',
            'tpl' => 'string:<h2>Title</h2><p>{lipsum}</p>',
            'thumbnail' => 'tpls/default/thumbs/content1.png',
        ),
        'content2' => array(
            'label' => 'Content with two columns',
            'tpl' => 'string:<table style="width:100%;"><tbody><tr><td><h2>Title</h2></td><td><h2>Title</h2></td></tr><tr><td>{lipsum}</td><td>{lipsum}</td></tr></tbody></table>',
            'thumbnail' => 'tpls/default/thumbs/content2.png',
        ),
        'content3' => array(
            'label' => 'Content with three columns',
            'tpl' => 'string:<table style="width:100%;"><tbody><tr><td><h2>Title</h2></td><td><h2>Title</h2></td><td><h2>Title</h2></td></tr><tr><td>{lipsum}</td><td>{lipsum}</td><td>{lipsum}</td></tr></tbody></table>',
            'thumbnail' => 'tpls/default/thumbs/content3.png',
        ),
        'image1left' => array(
            'label' => 'Content with left image',
            'tpl' => 'string:<table style="width:100%;"><tbody><tr><td>{imageSmall}</td><td><h2>Title</h2>{lipsum}</td></tr></tbody></table>',
            'thumbnail' => 'tpls/default/thumbs/image1left.png',
        ),
        'image1right' => array(
            'label' => 'Content with right image',
            'tpl' => 'string:<table style="width:100%;"><tbody><tr><td><h2>Title</h2>{lipsum}</td><td>{imageSmall}</td></tr></tbody></table>',
            'thumbnail' => 'tpls/default/thumbs/image1right.png',
        ),
        'image2' => array(
            'label' => 'Content with two image',
            'tpl' => 'string:<table style="width:100%;"><tbody><tr><td>{imageSmall}</td><td><h2>Title</h2>{lipsum}</td><td>{imageSmall}</td><td><h2>Title</h2>{lipsum}</td></tr></tbody></table>',
            'thumbnail' => 'tpls/default/thumbs/image2.png',
        ),
        'image3' => array(
            'label' => 'Content with three image',
            'tpl' => 'string:<table style="width:100%;"><tbody><tr><td>{image}</td><td>{image}</td><td>{image}</td></tr><tr><td><h2>Title</h2>{lipsum}</td><td><h2>Title</h2>{lipsum}</td><td><h2>Title</h2>{lipsum}</td></tr></tbody></table>',
            'thumbnail' => 'tpls/default/thumbs/image3.png',
        ),
        'footer' => array(
            'label' => 'Footer',
            //'tpl' => 'tpls/default/footer.html',
            'tpl' => 'string:<p class="footer">Take your footer contents and information here..</p>',
            'thumbnail' => 'tpls/default/thumbs/footer.png',
        ),
    );

    private $thumbsCache = array();

    private $autoInsertThumbnails = true;

    private static $devMode = false;

    public function __construct()
    {
        $this->vendorPath = 'vendor/';
        if ($this->autoInsertThumbnails) {
            if ((is_countable($this->getThumbs()) ? count($this->getThumbs()) : 0)==0 || self::$devMode) {
                $ord = 0;
                foreach (self::$defaultThumbnails as $thumbName => $thumbData) {
                    $templateSectionLine = BeanFactory::newBean('TemplateSectionLine');
                    $templateSectionLine->name = $thumbData['label'];
                    $templateSectionLine->description = preg_replace('/^string:/', '', (string) $thumbData['tpl']);
                    $templateSectionLine->description = str_replace('{lipsum}', $this->getContentLipsum(), $templateSectionLine->description);
                    $templateSectionLine->description = str_replace('{imageSmall}', $this->getContentImageSample(130), $templateSectionLine->description);
                    $templateSectionLine->description = str_replace('{image}', $this->getContentImageSample(), $templateSectionLine->description);
                    $templateSectionLine->thumbnail = file_exists($this->mozaikPath . '/' . $thumbData['thumbnail']) ? $this->mozaikPath . '/' . $thumbData['thumbnail'] : null;
                    $templateSectionLine->ord = ++$ord;
                    $templateSectionLine->save();
                }
            }
            $this->thumbsCache = array();
        }
    }

    private function getContentLipsum()
    {
        return 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam tempus odio ante, in feugiat ex pretium eu. In pharetra tincidunt urna et malesuada. Etiam aliquet auctor justo eu placerat. In nec sollicitudin enim. Nulla facilisi. In viverra velit turpis, et lobortis nunc eleifend id. Curabitur semper tincidunt vulputate. Nullam fermentum pellentesque ullamcorper.';
    }

    private function getContentImageSample($width = null)
    {
        if (is_numeric($width)) {
            $width = ' width="' . $width . '"';
        } else {
            $width = '';
        }
        $splits = explode('index.php', $_SERVER['REQUEST_URI']);
        $url = $_SERVER['REQUEST_SCHEME'] . '://' . $_SERVER['SERVER_NAME'] . $splits[0];
        $image = '<img src="' . $url . $this->mozaikPath . '/tpls/default/images/sample.jpg" ' . $width . ' />';
        return $image;
    }

    public function getDependenciesHTML()
    {
        $html = <<<HTML
<script src='{$this->vendorPath}/tinymce/tinymce/tinymce.min.js'></script>
<script src="{$this->vendorPath}/gymadarasz/imagesloaded/imagesloaded.pkgd.min.js"></script>

<!-- for color picker plugin -->
<link rel="stylesheet" media="screen" type="text/css" href="{$this->mozaikPath}/colorpicker/css/colorpicker.css" />
<script type="text/javascript" src="{$this->mozaikPath}/colorpicker/js/colorpicker.js"></script>
HTML;
        return $html;
    }

    public function getIncludeHTML()
    {
        $html = <<<HTML
<link rel="stylesheet" href="{$this->mozaikPath}/jquery.mozaik.css">
<script src='{$this->mozaikPath}/jquery.mozaik.js'></script>
HTML;
        return $html;
    }

    private function tinyMCESetupArgumentFixer($tinyMCESetup = '{}')
    {
        if (!$tinyMCESetup) {
            $tinyMCESetup = '{}';
        }

        if (is_array($tinyMCESetup) || is_object($tinyMCESetup)) {
            $tinyMCESetup = json_encode($tinyMCESetup, JSON_THROW_ON_ERROR);
        }

        if (!preg_match('/^tinyMCE\s*:\s*/', (string) $tinyMCESetup)) {
            $tinyMCESetup = "tinyMCE: $tinyMCESetup";
        }
        return $tinyMCESetup;
    }

    public function getElementHTML($contents = '', $textareaId = null, $elementId = 'mozaik', $width = '600', $thumbs = array(), $tinyMCESetup = '{}')
    {
        if (is_numeric($width)) {
            $width .= 'px';
        }
        if (!$thumbs) {
            $thumbs = self::$defaultThumbnails;
        }
        $thumbsJSON = json_encode($thumbs, JSON_THROW_ON_ERROR);

        $tinyMCESetup = $this->tinyMCESetupArgumentFixer($tinyMCESetup);

        $refreshTextareaScript = '';
        if ($textareaId) {
            $refreshTextareaScript = $this->getRefreshTextareaScript($textareaId, $elementId, $width);
        }
        $html = <<<HTML
<style type="text/css">
#{$elementId} {position: relative; top: 0; left: 0;}
#{$elementId} ul.mozaik-thumbs li.mozaik-thumbnail {padding: 5px 0;}
#{$elementId} ul.mozaik-thumbs li.mozaik-thumbnail:hover {background-color: lightgray;}
#{$elementId} .mozaik-thumbnail.ui-draggable.ui-draggable-handle {cursor: -webkit-grab;}
#{$elementId} .mozaik-thumbnail.ui-draggable.ui-draggable-handle * {cursor: -webkit-grab;}
#{$elementId} .mozaik-thumbnail.ui-draggable.ui-draggable-handle.ui-draggable-dragging {cursor: -webkit-grabbing;}
#{$elementId} .mozaik-thumbnail.ui-draggable.ui-draggable-handle.ui-draggable-dragging * {cursor: -webkit-grabbing;}
#{$elementId} .mozaik-inner a {text-decoration: underline;}
</style>
<div id="{$elementId}">{$contents}</div>
<script type="text/javascript">
    $(function() {
        // initialize

        if(typeof window.mozaikSettings == 'undefined') {
            window.mozaikSettings = {};
        }

        window.mozaikSettings.{$elementId} = {
            base: '{$this->mozaikPath}/',
            thumbs: {$thumbsJSON},
            editables: 'editable',
            style: 'tpls/default/styles/default.css',
            namespace: false,
            ace: false,
            width: '{$width}', // default value
            {$tinyMCESetup}

        };

        window.plgBackground.image = '{$this->mozaikPath}/' + window.plgBackground.image;

        $('#{$elementId}').mozaik(window.mozaikSettings.{$elementId});

        $(window).mousemove(function(){
            var correction = -( ($('#{$elementId}').width()-100) / 2);
            $('#{$elementId} .mozaik-thumbnail.ui-draggable-dragging').css('margin-left', correction + 'px');
        });

    });
    // refresh textarea
    {$refreshTextareaScript}

</script>
HTML;
        return $html;
    }

    public function getAllHTML($contents = '', $textareaId = null, $elementId = 'mozaik', $width = '600', $group = '', $tinyMCESetup = '{}')
    {
        if (is_numeric($width)) {
            $width .= 'px';
        }
        $tinyMCESetup = $this->tinyMCESetupArgumentFixer($tinyMCESetup);
        $mozaikHTML = $this->getDependenciesHTML();
        $mozaikHTML .= $this->getIncludeHTML();
        $thumbs = $this->getThumbs($group);
        $mozaikHTML .= $this->getElementHTML($contents, $textareaId, $elementId, $width, $thumbs, $tinyMCESetup);
        return $mozaikHTML;
    }

    private function getRefreshTextareaScript($textareaId, $elementId, $width = 'initial')
    {
        if (is_numeric($width)) {
            $width .= 'px';
        }
        $js = <<<SCRIPT
$(window).mouseup(function(){
     $('#{$textareaId}').val($('#{$elementId}').getMozaikValue({width: '{$width}'}));

     // fix table editor panel
     var found = false;
     $('.mce-tinymce').each(function(i,e){
        if(!$(e).hasClass('mce-tinymce-inline-inside') && $(e).css('display') == 'block'){
            found = true;
        }
     });
     if(!found) {
        $('.mce-tinymce-inline-inside').css('display', 'none');
     }
});
SCRIPT;
        return $js;
    }

    private function getThumbs($group = '')
    {
        $cacheGroup = 'cached_' . $group;

        if (!isset($this->thumbsCache[$cacheGroup])) {
            $db = DBManagerFactory::getInstance();
            $_group = $db->quote($group);
            $templateSectionLineBean = BeanFactory::getBean('TemplateSectionLine');
            $thumbBeans = $templateSectionLineBean->get_full_list('ord', "(grp LIKE '$_group' OR grp IS NULL)");
            $thumbs = array();
            if ($thumbBeans) {
                foreach ($thumbBeans as $thumbBean) {
                    $thumbs[$thumbBean->name] = array(
                        'label' => $thumbBean->thumbnail ? $this->getThumbImageHTML($thumbBean->thumbnail, $thumbBean->name) : $thumbBean->name,
                        'tpl' => 'string:' . html_entity_decode((string) $thumbBean->description),
                    );
                }
            }
            $this->thumbsCache[$cacheGroup] = $thumbs;
        }

        $thumbs = $this->thumbsCache[$cacheGroup];

        return $thumbs;
    }

    private function getThumbImageHTML($src, $label)
    {
        if (file_exists($src)) {
            $html = '<img src="' . $src. '" alt="' . $label . '">';
        } else {
            $html = $label;
        }
        return $html;
    }
}
