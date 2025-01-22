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


require_once('include/ytree/Tree.php');
require_once('include/JSON.php');

/**
 * Class Tree
 *    usage:
 * initialize the tree, add nodes, generate header for required files inclusion.
 * generate function that has tree data and script to convert data into tree init.
 * generate call to tree init function.
 * subsequent tree data calls will be served by the node class.
 * tree view by default make ajax based calls for all requests.
 */
#[\AllowDynamicProperties]
class Tree
{
    public $tree_style = 'include/ytree/TreeView/css/folders/tree.css';
    public $_header_files = array(
        'include/javascript/yui/build/treeview/treeview.js',
        'include/ytree/treeutil.js',
    );

    public $_debug_window = false;
    public $_debug_div_name = 'debug_tree';
    public $_name;
    public $_nodes = array();
    public $json;
    //collection of parmeter properties;
    public $_params = array();

    public function __construct($name)
    {
        $this->_name = $name;
        $this->json = new JSON();
    }

    /**
     * optionally add json.js, required for making AJAX Calls.
     * @param null $reference
     */
    public function include_json_reference($reference = null)
    {
        // TODO: possibly deprecate
        // if (empty($reference)) {
        //  $this->_header_files[]='include/JSON.js';
        // } else {
        //  $this->_header_files[]=$reference;
        // }
    }

    /**
     * @param $node
     */
    public function add_node($node)
    {
        $this->_nodes[$node->uid] = $node;
    }

    /**
     * @return string - html for including necessary javascript files.
     */
    public function generate_header()
    {
        $ret = "<link rel='stylesheet' href='{$this->tree_style}'>\n";
        foreach ($this->_header_files as $filename) {
            $ret .= "<script language='JavaScript' src='" . getJSPath($filename) . "'></script>\n";
        }

        return $ret;
    }

    /**
     * properties set here will be accessible from the tree's name space..
     * @param $name
     * @param $value
     */
    public function set_param($name, $value)
    {
        if (!empty($name) && !empty($value)) {
            $this->_params[$name] = $value;
        }
    }

    /**
     * @param bool $scriptTags
     * @return string
     */
    public function generate_nodes_array($scriptTags = true)
    {
        global $sugar_config;
        $node = null;
        $ret = array();
        foreach ($this->_nodes as $node) {
            $ret['nodes'][] = $node->get_definition();
        }

        //todo removed site_url setting from here.
        //todo make these variables unique.
        $tree_data = "var TREE_DATA= " . $this->json->encode($ret) . ";\n";
        $tree_data .= "var param= " . $this->json->encode($this->_params) . ";\n";

        $tree_data .= "var mytree;\n";
        $tree_data .= "treeinit(mytree,TREE_DATA,'{$this->_name}',param);\n";
        if ($scriptTags) {
            return '<script>' . $tree_data . '</script>';
        } else {
            return $tree_data;
        }
    }

    /**
     * Generates the javascript node arrays without calling treeinit().  Also generates a callback function that can be
     * easily called to instatiate the treeview object onload().
     *
     * IE6/7 will throw an "Operation Aborted" error when calling certain types of scripts before the page is fully
     * loaded.  The workaround is to move the init() call to the onload handler.  See: http://www.viavirtualearth.
     * com/wiki/DeferScript.ashx
     *
     * @param bool insertScriptTags Flag to add <script> tags
     * @param string customInitFunction Defaults to "onloadTreeInit"
     * @return string
     */
    public function generateNodesNoInit($insertScriptTags = true, $customInitFunction = "")
    {
        $node = null;
        $ret = array();

        $initFunction = (empty($customInitFunction)) ? 'treeinit' : $customInitFunction;

        foreach ($this->_nodes as $node) {
            $ret['nodes'][] = $node->get_definition();
        }

        $treeData = "var TREE_DATA = " . $this->json->encode($ret) . ";\n";
        $treeData .= "var param = " . $this->json->encode($this->_params) . ";\n";
        $treeData .= "var mytree;\n";
        $treeData .= "function onloadTreeinit() { {$initFunction}(mytree,TREE_DATA,'{$this->_name}',param); }\n";

        if ($insertScriptTags) {
            $treeData = "<script type='text/javascript' language='javascript'>{$treeData}</script>";
        }

        return $treeData;
    }

    /**
     * @return array
     */
    public function generateNodesRaw()
    {
        $node = null;
        $ret = array();
        $return = array();

        foreach ($this->_nodes as $node) {
            $ret['nodes'][] = $node->get_definition();
        }

        $return['tree_data'] = $ret;
        $return['param'] = $this->_params;

        return $return;
    }
}

