<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

/**
 * PHP wrapper class for Javascript driven TinyMCE WYSIWYG HTML editor
 */
#[\AllowDynamicProperties]
class SugarTinyMCE
{
    public $customLanguageDir = 'custom/include/tinymce/langs/';
    public $customConfigFile = 'custom/include/tinyButtonConfig.php';
    public $customDefaultConfigFile = 'custom/include/tinyMCEDefaultConfig.php';

    public $buttonConfigs = [
        'extended' => [
            'toolbar1' => 'fontfamily fontsize | bold italic underline | forecolor backcolor | blocks | bullist numlist | link image | pagebreak',
        ],
        'standard' => [
            'toolbar1' => 'fontfamily fontsize | bold italic underline | forecolor backcolor | blocks | bullist numlist | link image',
        ],
        'minimal' => [
            'toolbar1' => 'code | bold italic underline strikethrough | bullist numlist | alignleft aligncenter alignright alignjustify | link unlink | forecolor backcolor | blocks fontfamily fontsize',
        ],
    ];

    public $pluginsConfig = [
        'extended' => 'code help searchreplace lists image anchor charmap insertdatetime table preview link directionality pagebreak',
        'standard' => 'code help searchreplace lists image anchor charmap insertdatetime table preview link directionality',
        'minimal' => 'lists link',
    ];

    public $menubarConfig = [
        'extended' => 'edit view insert format tools table',
        'standard' => 'edit view insert format tools table',
        'minimal' => false,
    ];

    public $defaultConfig = [
        'menubar' => false,
        'convert_urls' => false,
        'valid_children' => '+body[style]',
        'height' => 600,
        'width' => '100%',
        'language' => 'en',
        'extended_valid_elements' => 'style[dir|lang|media|title|type],hr[class|width|size|noshade],@[class|style]',
        'promotion' => false,
        'license_key' => 'gpl',
        'sandbox_iframes' => true,
        'convert_unsafe_embeds' => true,
        'entity_encoding' => 'named',
        'entities' => '160,nbsp,38,amp,60,lt,62,gt,34,quot,39,apos,162,cent,163,pound,165,yen,8364,euro,169,copy,174,reg,8482,trade',
        'forced_root_block' => false,
        'fix_list_elements' => false,
    ];

    /**
     * Maps legacy config names to semantic config names for backward compatibility
     */
    private $configAliases = [
        'email_compose_light' => 'standard',
        'default' => 'standard',
    ];

    /**
     * Sole constructor
     */
    public function __construct()
    {
        $this->overloadButtonConfigs();
        $this->overloadDefaultConfigs();
        $this->overloadLanguageConfigs();
    }

    /**
     * Resolves legacy config type aliases to semantic config names
     */
    private function resolveConfigType($type)
    {
        return isset($this->configAliases[$type]) ? $this->configAliases[$type] : $type;
    }

    /**
     * Validates configuration type exists
     */
    private function validateConfigType($type)
    {
        global $log;

        $validTypes = ['extended', 'standard', 'minimal'];

        if (!in_array($type, $validTypes)) {
            $log?->warn("[SugarTinyMCE][validateConfigType] Invalid config type: $type, falling back to 'standard'");
            return 'standard';
        }

        return $type;
    }

    /**
     * Builds TinyMCE configuration array for specified type
     */
    private function buildConfig($type)
    {
        $config = $this->defaultConfig;

        $config['menubar'] = $this->menubarConfig[$type] ?? false;

        $toolbars = array_filter([
            $this->buttonConfigs[$type]['toolbar1'] ?? '',
            $this->buttonConfigs[$type]['toolbar2'] ?? '',
            $this->buttonConfigs[$type]['toolbar3'] ?? '',
        ]);

        $config['toolbar'] = array_values($toolbars);
        $config['plugins'] = $this->pluginsConfig[$type] ?? $this->pluginsConfig['standard'] ?? '';

        return $config;
    }

    /**
     * Returns TinyMCE configuration array for specified type
     *
     * @param string $type Configuration type
     * @return array Configuration array
     */
    public function getConfigArray($type = 'standard')
    {
        global $log;

        $type = $this->resolveConfigType($type);
        $type = $this->validateConfigType($type);

        $log?->info("[SugarTinyMCE][getConfigArray] Generating config array for type: $type");

        return $this->buildConfig($type);
    }

    /**
     * Returns the Javascript necessary to initialize a TinyMCE instance for a given <textarea> or <div>
     * @param string $targets target Comma delimited list of DOM ID's, <textarea id='someTarget'>
     * @return string
     */
    public function getInstance($targets = "", $type = 'standard')
    {
        global $json, $log;

        if (empty($json)) {
            $json = getJSONobj();
        }

        $type = $this->resolveConfigType($type);
        $type = $this->validateConfigType($type);

        $log?->info("[SugarTinyMCE][getInstance] Generating TinyMCE instance for targets: $targets, type: $type");

        $config = $this->buildConfig($type);
        $config['directionality'] = SugarThemeRegistry::current()->directionality;

        $unique = 'default';
        if (!empty($targets)) {
            $allTargets = array_map(function ($element) {
                return "#$element";
            }, explode(',', $targets));

            $config['selector'] = implode(',', $allTargets);

            $exTargets = explode(",", $targets);
            $unique = $exTargets[0];
        }
        $jsConfig = $json->encode($config);
        $path = getJSPath('vendor/tinymce/tinymce/tinymce.min.js');
        $ret =<<<eoq
<script type="text/javascript"  src="$path"></script>
<script type="text/javascript">
<!--
$(document).ready(function(){
    if (!SUGAR.ajaxUI.hist_loaded){
      load_mce_$unique();
    }
    if (SUGAR.ajaxUI && SUGAR.ajaxUI.hist_loaded){
      setTimeout(function(){ load_mce_$unique();},40);
    }
  });
function load_mce_$unique(){
    if (!SUGAR.util.isTouchScreen()) {
        tinymce.init($jsConfig);
    } else {
eoq;
        $exTargets = explode(",", $targets);
        foreach ($exTargets as $instance) {
            $ret .=<<<eoq
    document.getElementById('$instance').style.width = '100%';
    document.getElementById('$instance').style.height = '100px';
eoq;
        }
        $ret .=<<<eoq
    }
}
-->
</script>

eoq;
        return $ret;
    }

    public function getConfig($type = 'standard')
    {
        global $json, $log;

        if (empty($json)) {
            $json = getJSONobj();
        }

        $type = $this->resolveConfigType($type);
        $type = $this->validateConfigType($type);

        $log?->info("[SugarTinyMCE][getConfig] Generating config for type: $type");

        $config = $this->buildConfig($type);
        $jsConfig = $json->encode($config);

        return "var tinyConfig = $jsConfig;";
    }

    /**
     * This function takes in html code that has been produced (and somewhat mauled) by TinyMCE
     * and returns a cleaned copy of it.
     *
     * @param $html
     * @return $html with all the tinyMCE specific html removed
     */
    public function cleanEncodedMCEHtml($html)
    {
        return str_replace(["mce:script", "mce_src=", "mce_href="], ["script", "src=", "href="], (string)$html);
    }

    /**
     * Reload the default button configs by allowing admins to specify
     * which tinyMCE buttons will be displayed in a separate config file.
     *
     */
    private function overloadButtonConfigs()
    {
        if (file_exists($this->customConfigFile)) {
            require_once($this->customConfigFile);

            if (!isset($buttonConfigs)) {
                return;
            }

            foreach ($buttonConfigs as $k => $v) {
                if (isset($this->buttonConfigs[$k])) {
                    $this->buttonConfigs[$k] = $v;
                }
            }
        }
    }

    /**
     * Reload the default tinyMCE config, preserving our default standard
     * allowable tag set.
     *
     */
    private function overloadDefaultConfigs()
    {
        if (file_exists($this->customDefaultConfigFile)) {
            require_once($this->customDefaultConfigFile);

            if (!isset($defaultConfig)) {
                return;
            }

            foreach ($defaultConfig as $k => $v) {
                if (isset($this->defaultConfig[$k])) {
                    if ($k == "extended_valid_elements") {
                        $this->defaultConfig[$k] .= "," . $v;
                    } else {
                        $this->defaultConfig[$k] = $v;
                    }
                }
            }
        }
    }

    /**
     * Reload the default tinyMCE config to set the custom language;
     * @return void
     */
    private function overloadLanguageConfigs(): void
    {
        global $current_language;
        $lang = substr((string)$current_language, 0, 2);
        if (file_exists($this->customLanguageDir . $lang.'.js')) {
            $this->defaultConfig['language_url'] = $this->customLanguageDir . $lang.'.js';
            $this->defaultConfig['language'] = $lang;
        }
    }
} // end class def
