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

require_once('include/Sugarpdf/sugarpdf_config.php');
require_once('include/MVC/View/SugarView.php');
require_once('include/Sugarpdf/FontManager.php');
#[\AllowDynamicProperties]
class ConfiguratorViewFontManager extends SugarView
{
   
    /**
     * Constructor
     */
    public function FontManager()
    {
        parent::__construct();
    }
    /**
     * display the form
     */
    public function display()
    {
        global $mod_strings, $app_list_strings, $app_strings, $current_user;
        $error="";
        if (!is_admin($current_user)) {
            sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']);
        }
        $fontManager = new FontManager();
        if (!$fontManager->listFontFiles()) {
            $error = implode("<br>", $fontManager->errors);
        }

        $this->ss->assign(
            "MODULE_TITLE",
            getClassicModuleTitle(
                $mod_strings['LBL_MODULE_ID'],
                array($mod_strings['LBL_FONTMANAGER_TITLE']),
                false
                )
            );
        if (!empty($_REQUEST['error'])) {
            $error .= "<br>".$_REQUEST['error'];
        }
        $this->ss->assign("error", $error);
        $this->ss->assign("MOD", $mod_strings);
        $this->ss->assign("APP", $app_strings);
        $this->ss->assign("JAVASCRIPT", $this->_getJS());
        if (isset($_REQUEST['return_action'])) {
            $this->ss->assign("RETURN_ACTION", $_REQUEST['return_action']);
        } else {
            $this->ss->assign("RETURN_ACTION", 'SugarpdfSettings');
        }
        $this->ss->assign("K_PATH_FONTS", K_PATH_FONTS);
        // YUI List
        $this->ss->assign("COLUMNDEFS", $this->getYuiColumnDefs($fontManager->fontList));
        $this->ss->assign("DATASOURCE", $this->getYuiDataSource($fontManager->fontList));
        $this->ss->assign("RESPONSESCHEMA", $this->getYuiResponseSchema());
        
        //display
        $this->ss->display('modules/Configurator/tpls/fontmanager.tpl');
    }
    
    /**
     * Returns JS used in this view
     */
    private function _getJS()
    {
        global $mod_strings;
        return <<<EOJAVASCRIPT

EOJAVASCRIPT;
    }
    
    /**
     * Return the columnDefs for the YUI datatable
     * @return String
     */
    private function getYuiColumnDefs($fontList)
    {
        global $mod_strings;
        // Do not show the column with the delete buttons if there is only core fonts
        $removeColumn = '{key:"button", label:"", formatter:removeFormatter}';
        if ($this->isAllOOBFont($fontList)) {
            $removeColumn = '';
        }
            
        $return = <<<BSOFR
[ 
    {key:"name", minWidth:140, label:"{$mod_strings['LBL_FONT_LIST_NAME']}", sortable:true},
    {key:"filename", minWidth:120, label:"{$mod_strings['LBL_FONT_LIST_FILENAME']}", sortable:true},
    {key:"type", minWidth:100, label:"{$mod_strings['LBL_FONT_LIST_TYPE']}", sortable:true},
    {key:"style", minWidth:90, label:"{$mod_strings['LBL_FONT_LIST_STYLE']}", sortable:true},
    {key:"filesize", minWidth:70, label:"{$mod_strings['LBL_FONT_LIST_FILESIZE']}", formatter:YAHOO.widget.DataTable.formatNumber, sortable:true},
    {key:"enc", minWidth:80, label:"{$mod_strings['LBL_FONT_LIST_ENC']}", sortable:true},
    {key:"embedded", minWidth:70, label:"{$mod_strings['LBL_FONT_LIST_EMBEDDED']}", sortable:true},
    $removeColumn
]
BSOFR;
        return $return;
    }
    
    /**
    * Return the dataSource for the YUI Data Table
    * @param $fontList
    * @return String
    */
    private function getYuiDataSource($fontList)
    {
        $return = "[";
        $first=true;
        foreach ($fontList as $k=>$v) {
            if ($first) {
                $first=false;
            } else {
                $return .= ',';
            }
            $return .= '{';
            if (!empty($v['displayname'])) {
                $return .= 'name:"'.$v['displayname'].'"';
            } else {
                if (!empty($v['name'])) {
                    $return .= 'name:"'.$v['name'].'"';
                }
            }
            $return .= ', filename:"'.$v['filename'].'"';
            $return .= ', fontpath:"'.$v['fontpath'].'"';
            $return .= ', style:"'.$this->formatStyle($v['style']).'"';
            $return .= ', type:"'.$this->formatType($v['type']).'"';
            $return .= ', filesize:'.$v['filesize'];
            if (!empty($v['enc'])) {
                $return .= ', enc:"'.$v['enc'].'"';
            }
            if ($v['embedded'] == true) {
                $return .= ', embedded:"<input type=\'checkbox\' checked disabled/>"}';
            } else {
                $return .= ', embedded:"<input type=\'checkbox\' disabled/>"}';
            }
        }
        $return .= "]";
        return $return;
    }
    
    /**
    * Return the Response Schema for the YUI data table
    * @return String
    */
    private function getYuiResponseSchema()
    {
        return <<<BSOFR
        { 
            fields: [{key:"name", parser:"string"},
                     {key:"filename", parser:"string"},
                     {key:"fontpath", parser:"string"},
                     {key:"type", parser:"string"},
                     {key:"style", parser:"string"},
                     {key:"filesize", parser:"number"},
                     {key:"enc", parser:"string"},
                     {key:"embedded", parser:"string"}] 
        }
BSOFR;
    }
    
    /**
    * Return the label of the passed style
    * @param $style
    * @return String
    */
    private function formatStyle($style)
    {
        global $mod_strings;
        $return = "";
        if ((is_countable($style) ? count($style) : 0) == 2) {
            $return .= "<b><i>".$mod_strings['LBL_FONT_BOLDITALIC']."</b></i>";
        } else {
            switch ($style[0]) {
                case "bold":
                    $return .= "<b>".$mod_strings['LBL_FONT_BOLD']."</b>";
                    break;
                case "italic":
                    $return .= "<i>".$mod_strings['LBL_FONT_ITALIC']."</i>";
                    break;
                default:
                    $return .= $mod_strings['LBL_FONT_REGULAR'];
            }
        }
        return $return;
    }
    
    private function formatType($type)
    {
        global $mod_strings;
        switch ($type) {
            case "cidfont0":
                $return = $mod_strings['LBL_FONT_TYPE_CID0'];break;
            case "core":
                $return = $mod_strings['LBL_FONT_TYPE_CORE'];break;
            case "TrueType":
                $return = $mod_strings['LBL_FONT_TYPE_TRUETYPE'];break;
            case "Type1":
                $return = $mod_strings['LBL_FONT_TYPE_TYPE1'];break;
            case "TrueTypeUnicode":
                $return = $mod_strings['LBL_FONT_TYPE_TRUETYPEU'];break;
            default:
                $return = "";
        }
        return $return;
    }
    
    /**
     * Determine if all the fonts are core fonts
     * @param $fontList
     * @return boolean return true if all the fonts are core type
     */
    private function isAllOOBFont($fontList)
    {
        foreach ($fontList as $v) {
            if ($v['type'] != "core" && $v['fontpath'] != K_PATH_FONTS) {
                return false;
            }
        }
        return true;
    }
}
