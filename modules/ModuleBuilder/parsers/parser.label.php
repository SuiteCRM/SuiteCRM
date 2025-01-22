<?php
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


require_once 'modules/ModuleBuilder/parsers/ModuleBuilderParser.php';

/**
 * Class ParserLabel
 */
#[\AllowDynamicProperties]
class ParserLabel
{
    /**
     * @var string $packageName
     */
    protected $packageName;

    /**
     * @var string $moduleName
     */
    protected $moduleName;

    /**
     * @var LoggerManager
     */
    protected static $logger;

    /**
     * ParserLabel constructor.
     * @param string $moduleName
     * @param string $packageName
     */
    public function __construct($moduleName, $packageName = '')
    {
        $this->moduleName = $moduleName;
        if (!empty($packageName)) {
            $this->packageName = $packageName;
        }

        static::setLogger();
    }

    /**
     * @return void
     */
    protected static function setLogger()
    {
        static::$logger = LoggerManager::getLogger();
    }

    /**
     * Takes in the request params from a save request and processes
     * them for the save.
     *
     * @param array  $params   Labels as "label_".System label => Display label pairs
     * @param string $language Language key, for example 'en_us'
     *
     * @return bool
     */
    public function handleSave($params, $language)
    {
        $labels = array();
        foreach ($params as $key => $value) {
            if (preg_match('/^label_/', $key) && strcmp($value, 'no_change') != 0) {
                $labels [ strtoupper(substr($key, 6)) ] = SugarCleaner::cleanHtml(from_html($value), false);
            }
        }
        if (!empty($this->packageName)) {
            //we are in Module builder

            return self::addLabels($language, $labels, $this->moduleName, "custom/modulebuilder/packages/{$this->packageName}/modules/{$this->moduleName}/language");
        } else {
            return self::addLabels($language, $labels, $this->moduleName);
        }
    }

    /**
     * Remove a label from the language pack for a module
     * @param string $language      Language key, for example 'en_us'
     * @param string $label         The label to remove
     * @param string $labelvalue    The value of the label to remove
     * @param string $moduleName    Name of the module to which to add these labels
     * @param string $basepath      base path of the language file
     * @param bool $forRelationshipLabel      whether this is a relationship label
     * @return bool
     */
    public static function removeLabel($language, $label, $labelvalue, $moduleName, $basepath = null, $forRelationshipLabel = false)
    {
        $deployedModule = false;
        static::setLogger();

        static::$logger->debug("ParserLabel::removeLabels($language, \$label, \$labelvalue, $moduleName, $basepath );");
        if (is_null($basepath)) {
            $deployedModule = true;
            $basepath = "custom/modules/$moduleName/language";
            if ($forRelationshipLabel) {
                $basepath = "custom/modules/$moduleName/Ext/Language";
            }
            if (!is_dir($basepath)) {
                static::$logger->debug("$basepath is not a directory.");

                return false;
            }
        }

        $filename = "$basepath/$language.lang.php";
        if ($forRelationshipLabel) {
            $filename = "$basepath/$language.lang.ext.php";
        }

        $dir_exists = is_dir($basepath);

        $mod_strings = array();

        if ($dir_exists) {
            if (file_exists($filename)) {
                // obtain $mod_strings
                include $filename;
            } else {
                static::$logger->debug("file $filename does not exist.");

                return false;
            }
        } else {
            static::$logger->debug("directory $basepath does not exist.");

            return false;
        }

        $changed = false;

        if (isset($mod_strings[$label]) && $mod_strings[$label] == $labelvalue) {
            unset($mod_strings[$label]);
            $changed = true;
        }

        if ($changed) {
            if (!write_array_to_file('mod_strings', $mod_strings, $filename)) {
                static::$logger->fatal("Could not write $filename");
            } else {
                // if we have a cache to worry about, then clear it now
                if ($deployedModule) {
                    static::$logger->debug('PaserLabel::addLabels: clearing language cache');
                    $cache_key = 'module_language.'.$language.$moduleName;
                    sugar_cache_clear($cache_key);
                    LanguageManager::clearLanguageCache($moduleName, $language);
                }
            }
        }

        return true;
    }

    /**
     * Add a set of labels to the language pack for a module, deployed or undeployed
     * @param string $language      Language key, for example 'en_us'
     * @param array $labels         The labels to add in the form of an array of System label => Display label pairs
     * @param string $moduleName    Name of the module to which to add these labels
     * @param string $basepath
     * @param bool $forRelationshipLabel
     * @return bool
     */
    public static function addLabels($language, $labels, $moduleName, $basepath = null, $forRelationshipLabel = false)
    {
        static::setLogger();

        static::$logger->debug("ParserLabel::addLabels($language, \$labels, $moduleName, $basepath );");
        static::$logger->debug('$labels:' . print_r($labels, true));

        $deployedModule = false;
        if (null === $basepath) {
            $deployedModule = true;
            $basepath = "custom/Extension/modules/$moduleName/Ext/Language";
            if (!is_dir($basepath)) {
                mkdir_recursive($basepath);
            }
        }

        if (!$deployedModule) {
            $filename = "$basepath/$language.lang.php";
        } else {
            $filename = "$basepath/_override_$language.lang.php";
        }
        $dir_exists = is_dir($basepath);

        $mod_strings = array();

        if ($dir_exists) {
            if (file_exists($filename)) {
                // obtain $mod_strings
                include $filename;
            } elseif ($forRelationshipLabel) {
                $fh = fopen($filename, 'ab');
                sugar_fclose($fh);
            }
        } else {
            return false;
        }

        $changed = false;

        //$charset = (isset($app_strings['LBL_CHARSET'])) ? $app_strings['LBL_CHARSET'] : $GLOBALS['sugar_config']['default_charset'] ;

        foreach ($labels as $key => $value) {
            if (!isset($mod_strings [ $key ]) || strcmp($value, $mod_strings [ $key ]) != 0) {
                $mod_strings [$key] = to_html(strip_tags(from_html($value))); // must match encoding used in view.labels.php
                $changed = true;
            }
        }

        if ($changed) {
            static::$logger->debug("ParserLabel::addLabels: writing new mod_strings to $filename");
            static::$logger->debug('ParserLabel::addLabels: mod_strings='.print_r($mod_strings, true));
            if (!write_override_label_to_file('mod_strings', $mod_strings, $filename)) {
                static::$logger->fatal("Could not write $filename");
            } else {
                // if we have a cache to worry about, then clear it now
                if ($deployedModule) {
                    SugarCache::cleanOpcodes();
                    static::$logger->debug('PaserLabel::addLabels: clearing language cache');
                    $cache_key = 'module_language.'.$language.$moduleName;
                    sugar_cache_clear($cache_key);
                    LanguageManager::clearLanguageCache($moduleName, $language);
                }
            }
        }

        // Fix for bug #51
        // when the label is recreated it defaults back to the original value (In this case its "User").

        // Solution:
        // 1. Changes to the label names should go to custom/Extension/modules/{ModuleName}/Ext/Language
        // This is done in case different users edit the same Relationship concurrently.
        // The changes from custom/Extension/modules/{ModuleName}/Ext/Language
        // will overwrite stuff in custom/modules/{ModuleName}/Ext/Language/en_us.lang.ext.php after
        //  Quick Repair and Rebuild is applied.
        if ($forRelationshipLabel) {
            if (!empty($_POST[view_module]) && !empty($_POST[relationship_name]) && !empty($_POST[rhs_label]) && !empty($_POST[lhs_module])) {
                // 1. Overwrite custom/Extension/modules/{ModuleName}/Ext/Language
                $extension_basepath = 'custom/Extension/modules/'.$_POST[view_module].'/Ext/Language';
                mkdir_recursive($extension_basepath);

                $headerString = "<?php\n//THIS FILE IS AUTO GENERATED, DO NOT MODIFY\n";
                $out = $headerString;

                $extension_filename = "$extension_basepath/$language.custom".$_POST[relationship_name].'.php';

                $mod_strings = array();
                if (file_exists($extension_filename)) {
                    // obtain $mod_strings
                    include $extension_filename;
                }

                foreach ($labels as $key => $value) {
                    foreach ($mod_strings as $key_mod_string => $value_mod_string) {
                        if (strpos($key_mod_string, strtoupper($_POST[relationship_name])) !== false) {
                            $mod_strings[$key_mod_string] = to_html(strip_tags(from_html($_POST[rhs_label]))); // must match encoding used in view.labels.php
                        }
                    }
                }

                // Fix for issue #551 - save new labels
                foreach ($labels as $key => $value) {
                    $mod_strings[$key] = $value;
                }

                foreach ($mod_strings as $key => $val) {
                    $out .= override_value_to_string_recursive2('mod_strings', $key, $val);
                }

                try {
                    $file_contents = fopen($extension_filename, 'wb');
                    fwrite($file_contents, $out, strlen($out));
                    sugar_fclose($file_contents);
                } catch (Exception $e) {
                    static::$logger->fatal("Could not write $filename");
                    static::$logger->fatal('Exception '.$e->getMessage());
                }

                //2. Overwrite custom/Extension/modules/relationships/language/{ModuleName}.php
                // Also need to overwrite custom/Extension/modules/relationships/language/{ModuleName}.php
                // because whenever new relationship is created this place is checked by the system to get
                // all the label names
                $relationships_basepath = 'custom/Extension/modules/relationships/language';
                mkdir_recursive($relationships_basepath);

                $headerString = "<?php\n//THIS FILE IS AUTO GENERATED, DO NOT MODIFY\n";
                $out = $headerString;

                $relationships_filename = "$relationships_basepath/".$_POST[lhs_module].'.php';

                $mod_strings = array();
                if (file_exists($relationships_filename)) {
                    // obtain $mod_strings
                    include $relationships_filename;
                }

                $changed_mod_strings = false;
                foreach ($labels as $key => $value) {
                    foreach ($mod_strings as $key_mod_string => $value_mod_string) {
                        if (strpos($key_mod_string, strtoupper($_POST[relationship_name])) !== false) {
                            $mod_strings[$key_mod_string] = to_html(strip_tags(from_html($_POST[rhs_label]))); // must match encoding used in view.labels.php
                            $changed_mod_strings = true;
                        }
                    }
                }

                // Fix for issue #551 - save new labels
                foreach ($labels as $key => $value) {
                    $mod_strings[$key] = $value;
                }

                foreach ($mod_strings as $key => $val) {
                    $out .= override_value_to_string_recursive2('mod_strings', $key, $val);
                }

                $failed_to_write = false;
                try {
                    $file_contents = fopen($relationships_filename, 'wb');
                    fwrite($file_contents, $out, strlen($out));
                    sugar_fclose($file_contents);
                } catch (Exception $e) {
                    static::$logger->fatal("Could not write $filename");
                    static::$logger->fatal('Exception '.$e->getMessage());
                    $failed_to_write = true;
                }

                if ($changed_mod_strings) {
                    if (!$failed_to_write) {
                        // if we have a cache to worry about, then clear it now
                        if ($deployedModule) {
                            SugarCache::cleanOpcodes();
                            static::$logger->debug('PaserLabel::addLabels: clearing language cache');
                            $cache_key = 'module_language.'.$language.$moduleName;
                            sugar_cache_clear($cache_key);
                            LanguageManager::clearLanguageCache($moduleName, $language);
                        }
                    }
                }
            }
        }

        return true;
    }

    /**
     * Takes in the request params from a save request and processes
     * them for the save.
     *
     * @param array $metadata
     * @param string $language Language key, for example 'en_us'
     */
    public function handleSaveRelationshipLabels($metadata, $language)
    {
        foreach ($metadata as $definition) {
            $labels = array();
            $labels[$definition [ 'system_label' ]] = $definition [ 'display_label' ];
            self::addLabels($language, $labels, $definition [ 'module' ], null, true);
        }
    }

    /**
     * @param array $labels
     */
    public function addLabelsToAllLanguages($labels)
    {
        $langs = get_languages();
        foreach ($langs as $lang_key => $lang_display) {
            self::addLabels($lang_key, $labels, $this->moduleName);
        }
    }
}
