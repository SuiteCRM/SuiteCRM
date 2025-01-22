<?php

namespace SuiteCRM;

/**
 * Class SugarCleaner
 * @package SuiteCRM
 * Html Sanitizer
 */
#[\AllowDynamicProperties]
class HtmlSanitizer
{
    /**
     * Singleton instance
     * @var HtmlSanitizer
     */
    private static $instance;

    /**
     * HTMLPurifier instance
     * @var \HTMLPurifier
     */
    protected $purifier;

    /**
     * SugarCleaner constructor.
     */
    public function __construct(array $extraConfigs = [])
    {
        $configurator = new \Configurator();
        $sugar_config = $configurator->config;


        $config = \HTMLPurifier_Config::createDefault();

        if (!is_dir(sugar_cached("htmlclean"))) {
            create_cache_directory("htmlclean/");
        }

        $baseConfigs = [];
        $baseConfigs['HTML.Doctype'] = 'XHTML 1.0 Transitional';
        $baseConfigs['Core.Encoding'] = 'UTF-8';
        $hidden_tags = array('script' => true, 'style' => true, 'title' => true, 'head' => true);
        $baseConfigs['Core.HiddenElements'] = $hidden_tags;
        $baseConfigs['URI.Base'] = $sugar_config['site_url'] ?? null;
        $baseConfigs['CSS.Proprietary'] = true;
        $baseConfigs['HTML.TidyLevel'] = 'light';
        $baseConfigs['HTML.ForbiddenElements'] = array('body' => true, 'html' => true);
        $baseConfigs['AutoFormat.RemoveEmpty'] = true;
        $baseConfigs['Cache.SerializerPermissions'] = 0775;
        $baseConfigs['Filter.ExtractStyleBlocks.TidyImpl'] = false;
        if (!empty($sugar_config['html_allow_objects'])) {
            $baseConfigs['HTML.SafeObject'] = true;
            $baseConfigs['HTML.SafeEmbed'] = true;
        }
        $baseConfigs['Output.FlashCompat'] = true;
        $baseConfigs['Filter.Custom'] = array(new HTMLPurifierFilterXmp());
        $baseConfigs['HTML.DefinitionID'] = 'Sugar HTML Def';
        $baseConfigs['HTML.DefinitionRev'] = 2;
        $baseConfigs['Cache.SerializerPath'] = sugar_cached('htmlclean/');
        $baseConfigs['Attr.EnableID'] = true;
        $baseConfigs['Attr.IDPrefix'] = 'sugar_text_';

        $this->applyConfigs($baseConfigs, $extraConfigs, $config);

        if ($def = $config->maybeGetRawHTMLDefinition()) {
            $iframe = $def->addElement(
                'iframe',
                'Flow',
                'Optional: #PCDATA | Flow | Block',
                'Core',
                array(
                    'src*' => 'URI',
                    'frameborder' => 'Enum#0,1',
                    'marginwidth' => 'Pixels',
                    'marginheight' => 'Pixels',
                    'scrolling' => 'Enum#|yes,no,auto',
                    'align' => 'Enum#top,middle,bottom,left,right,center',
                    'height' => 'Length',
                    'width' => 'Length',
                )
            );

            $iframe->excludes = array('iframe');
        }

        /** @var \HTMLPurifier_URIDefinition $uri */
        $uri = $config->getDefinition('URI');
        $uri->addFilter(new URIFilter(), $config);
        \HTMLPurifier_URISchemeRegistry::instance()->register('cid', new HTMLPurifierURISchemeCid());

        $this->purifier = new \HTMLPurifier($config);
    }

    /**
     * Get cleaner instance
     * @return HtmlSanitizer
     */
    public static function getInstance()
    {
        if (self::$instance === null) {
            self::$instance = new self();
        }

        return self::$instance;
    }

    /**
     * Clean string from potential XSS problems
     * @param string $dirtyHtml
     * @param bool $removeHtml - remove encoded html
     * @return string clean html
     */
    public static function cleanHtml($dirtyHtml, $removeHtml = false)
    {
        return self::getInstance()->clean($dirtyHtml, $removeHtml);
    }

    /**
     * @param $dirtyHtml
     * @param bool $isEncoded
     * @return string
     */
    public static function stripTags($dirtyHtml, $isEncoded = true)
    {
        if ($isEncoded) {
            $dirtyHtml = from_html($dirtyHtml);
        }

        if (preg_match('/([a-z]+)(?![^>]*\/>)[^>]*/', $dirtyHtml)) {
            $dirtyHtml = strip_tags($dirtyHtml);
        }

        return $isEncoded ? to_html($dirtyHtml) : $dirtyHtml;
    }

    /**
     * @param string $dirtyHtml
     * @param bool $removeHtml
     * @return string
     */
    public function clean(string $dirtyHtml, bool $removeHtml): string
    {
        // $encode_html previously effected the decoding process.
        // we should decode regardless, just in case, the calling method passing encoded html
        //Prevent that the email address in Outlook format are removed
        $pattern = '/(.*)(&lt;([a-zA-Z0-9.!#$%&\'*+\=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)&gt;)(.*)/';
        $replacement = '${1}<<a href="mailto:${3}">${3}</a>> ${4}';
        $dirtyHtml = preg_replace($pattern, $replacement, $dirtyHtml);
        $dirty_html_decoded = html_entity_decode($dirtyHtml);

        // Re-encode html
        if ($removeHtml === true) {
            // remove all HTML tags
            $purifier = $this->purifier;
            $clean_html = $purifier->purify($dirty_html_decoded);
        } else {
            // encode all HTML tags
            $clean_html = $dirty_html_decoded;
        }

        return $clean_html;
    }

    /**
     * @param array $baseConfigs
     * @param array $extraConfigs
     * @param \HTMLPurifier_Config $config
     */
    protected function applyConfigs(array $baseConfigs, array $extraConfigs, \HTMLPurifier_Config $config): void
    {
        $configKeys = array_keys($baseConfigs);
        if (!empty($extraConfigs)) {
            $configKeys = array_merge($configKeys, array_keys($extraConfigs));
        }

        foreach ($configKeys as $configKey) {
            // no base config, set the custom config
            if (!isset($baseConfigs[$configKey])) {
                $config->set($configKey, $extraConfigs[$configKey]);
                continue;
            }

            // no extra config, set the base config
            if (!isset($extraConfigs[$configKey])) {
                $config->set($configKey, $baseConfigs[$configKey]);
                continue;
            }

            // both values are arrays, merge and set
            if (is_array($baseConfigs[$configKey]) && is_array($extraConfigs[$configKey])) {
                $config->set($configKey, array_merge($baseConfigs[$configKey], $extraConfigs[$configKey]));
                continue;
            }

            // custom value does not match base value type, keep base value
            if (is_array($baseConfigs[$configKey]) && !is_array($extraConfigs[$configKey])) {
                $config->set($configKey, $baseConfigs[$configKey]);
                continue;
            }

            //Override base value with custom value
            $config->set($configKey, $extraConfigs[$configKey]);
        }
    }
}
