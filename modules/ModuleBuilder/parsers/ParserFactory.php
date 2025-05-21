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


require_once 'modules/ModuleBuilder/parsers/constants.php' ;

#[\AllowDynamicProperties]
class ParserFactory
{

    /**
     * Create a new parser
     *
     * @param string $view          The view, for example EditView or ListView. For search views, use advanced_search or basic_search
     * @param string $moduleName    Module name
     * @param string $packageName   Package name. If present implies that we are being called from ModuleBuilder
     * @return AbstractMetaDataParser
     */

    public static function getParser($view, $moduleName, $packageName = null, $subpanelName = null)
    {
        $GLOBALS [ 'log' ]->info("ParserFactory->getParser($view,$moduleName,$packageName,$subpanelName )") ;
        $sm = null;
        $lView = strtolower($view);
        if (empty($packageName) || ($packageName == 'studio')) {
            $packageName = null ;
            //For studio modules, check for view parser overrides
            $parser = self::checkForStudioParserOverride($view, $moduleName, $packageName);
            if ($parser) {
                return $parser;
            }
            $sm = StudioModuleFactory::getStudioModule($moduleName);
            //If we didn't find a specofic parser, see if there is a view to type mapping
            foreach ($sm->sources as $file => $def) {
                if (!empty($def['view']) && $def['view'] == $view && !empty($def['type'])) {
                    $lView = strtolower($def['type']);
                    break;
                }
            }
        }

        switch ($lView) {
            case MB_EDITVIEW:
            case MB_DETAILVIEW:
            case MB_QUICKCREATE:
                require_once 'modules/ModuleBuilder/parsers/views/GridLayoutMetaDataParser.php' ;
                return new GridLayoutMetaDataParser($view, $moduleName, $packageName) ;
            case MB_BASICSEARCH:
            case MB_ADVANCEDSEARCH:
                require_once 'modules/ModuleBuilder/parsers/views/SearchViewMetaDataParser.php' ;
                return new SearchViewMetaDataParser($view, $moduleName, $packageName) ;
            case MB_LISTVIEW:
                if ($subpanelName == null) {
                    require_once 'modules/ModuleBuilder/parsers/views/ListLayoutMetaDataParser.php' ;
                    return new ListLayoutMetaDataParser(MB_LISTVIEW, $moduleName, $packageName) ;
                } else {
                    require_once 'modules/ModuleBuilder/parsers/views/SubpanelMetaDataParser.php' ;
                    return new SubpanelMetaDataParser($subpanelName, $moduleName, $packageName) ;
                }
                // no break
            case MB_DASHLET:
            case MB_DASHLETSEARCH:
                require_once 'modules/ModuleBuilder/parsers/views/DashletMetaDataParser.php' ;
                return new DashletMetaDataParser($view, $moduleName, $packageName);
            case MB_POPUPLIST:
            case MB_POPUPSEARCH:
                require_once 'modules/ModuleBuilder/parsers/views/PopupMetaDataParser.php' ;
                return new PopupMetaDataParser($view, $moduleName, $packageName);
            case MB_LABEL:
                require_once 'modules/ModuleBuilder/parsers/parser.label.php' ;
                return new ParserLabel($moduleName, $packageName) ;
            case MB_VISIBILITY:
                require_once 'modules/ModuleBuilder/parsers/parser.visibility.php' ;
                return new ParserVisibility($moduleName, $packageName) ;
            default:
                $parser = self::checkForParserClass($view, $moduleName, $packageName);
                if ($parser) {
                    return $parser;
                }

        }

        $GLOBALS [ 'log' ]->fatal("ParserFactory: cannot create ModuleBuilder Parser $view") ;
    }

    protected static function checkForParserClass($view, $moduleName, $packageName, $nameOverride = false)
    {
        $prefix = '';
        if (!is_null($packageName)) {
            $prefix = empty($packageName) ? 'build' :'modify';
        }
        $fileNames = array(
            "custom/modules/$moduleName/parsers/parser." . strtolower($prefix . $view) . ".php",
            "modules/$moduleName/parsers/parser." . strtolower($prefix . $view) . ".php",
            "custom/modules/ModuleBuilder/parsers/parser." . strtolower($prefix . $view) . ".php",
            "modules/ModuleBuilder/parsers/parser." . strtolower($prefix . $view) . ".php",
        );
        foreach ($fileNames as $fileName) {
            if (file_exists($fileName)) {
                require_once $fileName ;
                $class = 'Parser' . $prefix . ucfirst($view) ;
                if (class_exists($class)) {
                    $GLOBALS [ 'log' ]->debug('Using ModuleBuilder Parser ' . $fileName) ;
                    $parser = new $class() ;
                    return $parser ;
                }
            }
        }
        return false;
    }

    /**
     * @param string $view
     * @param string $moduleName
     * @param string $packageName
     * @return bool|SugarView
     */
    protected static function checkForStudioParserOverride($view, $moduleName, $packageName)
    {
        require_once('modules/ModuleBuilder/Module/StudioModuleFactory.php');
        $sm = StudioModuleFactory::getStudioModule($moduleName);
        foreach ($sm->sources as $file => $def) {
            if (!empty($def['view']) && $def['view'] == strtolower($view) && !empty($def['parser'])) {
                $pName = $def['parser'];
                $path = "modules/ModuleBuilder/parsers/views/{$pName}.php";
                if (file_exists("custom/$path")) {
                    require_once("custom/$path");
                } else {
                    if (file_exists($path)) {
                        require_once($path);
                    }
                }
                if (class_exists($pName)) {
                    return new $pName($view, $moduleName, $packageName);
                }
                //If it wasn't defined directly, check for a generic parser name for the view
                $parser = self::checkForParserClass($view, $moduleName, $packageName);
                if ($parser) {
                    return $parser;
                }
            }
        }
        return false;
    }
}
