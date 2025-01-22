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



require_once('modules/ModuleBuilder/parsers/views/ListLayoutMetaDataParser.php') ;
require_once 'modules/ModuleBuilder/parsers/constants.php' ;

class SearchViewMetaDataParser extends ListLayoutMetaDataParser
{
    public $_searchLayout;
    public $_saved;
    /**
     * @var array $variableMap
     */
    public static $variableMap = array(
                            MB_BASICSEARCH => 'basic_search' ,
                            MB_ADVANCEDSEARCH => 'advanced_search' ,
                            ) ;

    /**
     * Columns is used by the view to construct the listview - each column is built by calling the named function
     * @var array $columns
     */
    public $columns = array( 'LBL_DEFAULT' => 'getDefaultFields' , 'LBL_HIDDEN' => 'getAvailableFields' ) ;

    /**
     * @var bool $allowParent
     */
    protected $allowParent = true;

    /**
     * SearchViewMetaDataParser constructor.
     * Must set:
     * $this->columns   Array of 'Column LBL'=>function_to_retrieve_fields_for_this_column() - expected by the view
     * @param string $searchLayout	The type of search layout, e.g., MB_BASICSEARCH or MB_ADVANCEDSEARCH
     * @param string $moduleName     The name of the module to which this listview belongs
     * @param string $packageName    If not empty, the name of the package to which this listview belongs
     * @throws Exception
     */
    public function __construct($searchLayout, $moduleName, $packageName = '')
    {
        $GLOBALS [ 'log' ]->debug(get_class($this) . ": __construct( $searchLayout , $moduleName , $packageName )") ;

        // BEGIN ASSERTIONS
        if (! isset(self::$variableMap [ $searchLayout ])) {
            sugar_die(get_class($this) . ": View $searchLayout is not supported") ;
        }
        // END ASSERTIONS

        $this->_searchLayout = $searchLayout ;

        // unsophisticated error handling for now...
        try {
            if (empty($packageName)) {
                require_once 'modules/ModuleBuilder/parsers/views/DeployedMetaDataImplementation.php' ;
                $this->implementation = new DeployedMetaDataImplementation($searchLayout, $moduleName) ;
            } else {
                require_once 'modules/ModuleBuilder/parsers/views/UndeployedMetaDataImplementation.php' ;
                $this->implementation = new UndeployedMetaDataImplementation($searchLayout, $moduleName, $packageName) ;
            }
        } catch (Exception $e) {
            throw $e ;
        }

        $this->_saved = array_change_key_case($this->implementation->getViewdefs()) ; // force to lower case so don't have problems with case mismatches later
        if (isset($this->_saved['templatemeta'])) {
            $this->_saved['templateMeta'] = $this->_saved['templatemeta'];
            unset($this->_saved['templatemeta']);
        }

        if (! isset($this->_saved [ 'layout' ] [ self::$variableMap [ $this->_searchLayout ] ])) {
            // attempt to fallback on a basic_search layout...

            if (! isset($this->_saved [ 'layout' ] [ self::$variableMap [ MB_BASICSEARCH ] ])) {
                throw new Exception(get_class($this) . ": {$this->_searchLayout} does not exist for module $moduleName") ;
            }

            $this->_saved [ 'layout'] [ MB_ADVANCEDSEARCH ] = $this->_saved [ 'layout' ] [ MB_BASICSEARCH ] ;
        }

        $this->view = $searchLayout;
        // convert the search view layout (which has its own unique layout form) to the standard listview layout so that the parser methods and views can be reused
        $this->_viewdefs = $this->convertSearchViewToListView($this->_saved [ 'layout' ] [ self::$variableMap [ $this->_searchLayout ] ]) ;
        $this->_fielddefs = $this->implementation->getFielddefs() ;
        $this->_standardizeFieldLabels($this->_fielddefs);
    }

    /**
     * @param string $key
     * @param array $def
     * @return bool
     */
    public function isValidField($key, $def)
    {
        if (isset($def['type']) && $def['type'] == "assigned_user_name") {
            $origDefs = $this->getOriginalViewDefs();
            if (isset($def['group']) && isset($origDefs[$def['group']])) {
                return false;
            }
            if (!isset($def [ 'studio' ]) || (is_array($def [ 'studio' ]) && !isset($def [ 'studio' ]['searchview']))) {
                return true;
            }
        }
        
        if (isset($def [ 'studio' ]) && is_array($def [ 'studio' ]) && isset($def [ 'studio' ]['searchview'])) {
            return $def [ 'studio' ]['searchview'] !== false &&
                  ($def [ 'studio' ]['searchview'] === true || $def [ 'studio' ]['searchview'] != 'false');
        }
        
        if (!parent::isValidField($key, $def)) {
            return false;
        }
        
        //Special case to prevent multiple copies of assigned, modified, or created by user on the search view
        if (empty($def[ 'studio' ]) && $key == "assigned_user_name") {
            $origDefs = $this->getOriginalViewDefs();
            if ($key == "assigned_user_name" && isset($origDefs['assigned_user_id'])) {
                return false;
            }
        }
        if (substr($key, -8) == "_by_name" &&  isset($def['rname']) && $def['rname'] == "user_name") {
            return false;
        }

        //Remove image fields (unless studio was set)
        if (!empty($def [ 'studio' ]) && isset($def['type']) && $def['type'] == "image") {
            return false;
        }
        
        return true;
    }

    /**
     * Save the modified searchLayout
     * Have to preserve the original layout format, which is array('metadata'=>array,'layouts'=>array('basic'=>array,'advanced'=>array))
     * @param bool $populate
     */
    public function handleSave($populate = true)
    {
        if ($populate) {
            $this->_populateFromRequest() ;
        }
            
            
        $this->_saved [ 'layout' ] [ self::$variableMap [ $this->_searchLayout ] ] = $this->convertSearchViewToListView($this->_viewdefs);
        ;
        $this->implementation->deploy($this->_saved) ;
    }

    private function convertSearchViewToListView($viewdefs)
    {
        $temp = array( ) ;
        foreach ($viewdefs as $key => $value) {
            if (! is_array($value)) {
                $key = $value ;
                $def = array( ) ;
                $def[ 'name' ] = $key;
                $value = $def ;
            }

            if (!isset($value [ 'name' ])) {
                $value [ 'name' ] = $key;
            } else {
                $key = $value [ 'name' ] ; // override key with name, needed when the entry lacks a key
            }
            // now add in the standard listview default=>true
            $value [ 'default' ] = true ;
            $temp [ strtolower($key) ] = $value ;
        }
        return $temp ;
    }


    /**
     * @param $defs
     * @return array
     */
    public function normalizeDefs($defs)
    {
        $out = array();
        foreach ($defs as $def) {
            if (is_array($def) && isset($def['name'])) {
                $out[strtolower($def['name'])] = $def;
            }
        }
        return $out;
    }

    /**
     * @return array
     */
    public function getOriginalViewDefs()
    {
        $defs = $this->implementation->getOriginalViewdefs();
        $out = array();
        if (!empty($defs) && !empty($defs['layout']) && !empty($defs['layout'][$this->_searchLayout])) {
            if ($this->_searchLayout == "basic_search" &&  !empty($defs['layout']["advanced_search"])) {
                $out = $this->normalizeDefs($defs['layout']["advanced_search"]);
            }
            $out = array_merge($out, $this->normalizeDefs($defs['layout'][$this->_searchLayout]));
        }

        return $out;
    }
}
