<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/stic_Import_Validation/views/ImportView.php');


#[\AllowDynamicProperties]
class stic_Import_ValidationViewStep2 extends stic_Import_ValidationView
{
    protected $pageTitleKey = 'LBL_STEP_2_TITLE';


    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings, $app_list_strings, $app_strings, $current_user, $import_bean_map, $import_mod_strings;

        $this->instruction = 'LBL_SELECT_UPLOAD_INSTRUCTION';
        $this->ss->assign('INSTRUCTION', $this->getInstruction());

        $this->ss->assign("MODULE_TITLE", $this->getModuleTitle(false));
        $this->ss->assign("IMP", $import_mod_strings);
        $this->ss->assign("CURRENT_STEP", $this->currentStep);
        $this->ss->assign("TYPE", (!empty($_REQUEST['type']) ? $_REQUEST['type'] : "import"));
        $this->ss->assign("CUSTOM_DELIMITER", (!empty($_REQUEST['custom_delimiter']) ? $_REQUEST['custom_delimiter'] : ","));
        $this->ss->assign("CUSTOM_ENCLOSURE", htmlentities(
            ((string) (!empty($_REQUEST['custom_enclosure']) && $_REQUEST['custom_enclosure'] != 'other'
                ? $_REQUEST['custom_enclosure'] :
                (!empty($_REQUEST['custom_enclosure_other'])
                    ? $_REQUEST['custom_enclosure_other'] : "")))
        ));

        $this->ss->assign("IMPORT_MODULE", $_REQUEST['import_module']);
        $this->ss->assign("HEADER", ($app_strings['LBL_STIC_IMPORT_VALIDATION'] ?? '') ." ". $mod_strings['LBL_MODULE_NAME']);
        $this->ss->assign("JAVASCRIPT", $this->_getJS());
        $this->ss->assign("SAMPLE_URL", "<a href=\"javascript: void(0);\" onclick=\"window.location.href='index.php?entryPoint=export&module=".urlencode($_REQUEST['import_module'])."&action=index&all=true&sample=true'\" >".$mod_strings['LBL_EXAMPLE_FILE']."</a>");

        $displayBackBttn = isset($_REQUEST['action']) && $_REQUEST['action'] == 'Step2' && isset($_REQUEST['current_step']) && $_REQUEST['current_step']!=='2'? true : false; //bug 51239
        $this->ss->assign("displayBackBttn", $displayBackBttn);

        // get user defined import maps
        $is_admin = is_admin($current_user);
        if ($is_admin) {
            $savedMappingHelpText = $mod_strings['LBL_MY_SAVED_ADMIN_HELP'];
        } else {
            $savedMappingHelpText = $mod_strings['LBL_MY_SAVED_HELP'];
        }

        $this->ss->assign('savedMappingHelpText', $savedMappingHelpText);
        $this->ss->assign('is_admin', $is_admin);

        $import_map_seed = BeanFactory::newBean('Import_1');
        $custom_imports_arr = $import_map_seed->retrieve_all_by_string_fields(array('assigned_user_id' => $current_user->id, 'is_published' => 'no','module' => $_REQUEST['import_module']));

        if (is_countable($custom_imports_arr) ? count($custom_imports_arr) : 0) {
            $custom = array();
            foreach ($custom_imports_arr as $import) {
                $custom[] = array( "IMPORT_NAME" => $import->name,"IMPORT_ID"   => $import->id);
            }
            $this->ss->assign('custom_imports', $custom);
        }

        // get globally defined import maps
        $published_imports_arr = $import_map_seed->retrieve_all_by_string_fields(array('is_published' => 'yes', 'module' => $_REQUEST['import_module'],));
        if (is_countable($published_imports_arr) ? count($published_imports_arr) : 0) {
            $published = array();
            foreach ($published_imports_arr as $import) {
                $published[] = array("IMPORT_NAME" => $import->name, "IMPORT_ID"   => $import->id);
            }
            $this->ss->assign('published_imports', $published);
        }
        //End custom mapping

        // add instructions for anything other than custom_delimited
        $instructions = array();
        $lang_key = "CUSTOM";

        for ($i = 1; isset($mod_strings["LBL_{$lang_key}_NUM_$i"]);$i++) {
            $instructions[] = array(
                "STEP_NUM"         => $mod_strings["LBL_NUM_$i"],
                "INSTRUCTION_STEP" => $mod_strings["LBL_{$lang_key}_NUM_$i"],
            );
        }

        $this->ss->assign("instructions", $instructions);

        $content = $this->ss->fetch('modules/stic_Import_Validation/tpls/step2.tpl');
        $this->ss->assign("CONTENT", $content);
        $this->ss->display('modules/stic_Import_Validation/tpls/wizardWrapper.tpl');
    }

    /**
     * Returns JS used in this view
     */
    private function _getJS()
    {
        global $mod_strings;

        return <<<EOJAVASCRIPT

document.getElementById('gonext').onclick = function(){
    // warning message that tells user that updates can not be undone
    if(document.getElementById('import_update').checked)
    {
        ret = confirm(SUGAR.language.get("stic_Import_Validation", 'LBL_CONFIRM_IMPORT'));
        if (!ret) {
            return false;
        }
    }
    clear_all_errors();
    var isError = false;
    // be sure we specify a file to upload
    if (document.getElementById('importstep2').userfile.value == "") {
        add_error_style(document.getElementById('importstep2').name,'userfile',"{$mod_strings['ERR_MISSING_REQUIRED_FIELDS']} {$mod_strings['ERR_SELECT_FILE']}");
        isError = true;
    }

    return !isError;

}

function publishMapping(elem, publish, mappingId, importModule)
{
    if( typeof(elem.publish) != 'undefined' )
        publish = elem.publish;

    var url = 'index.php?action=mapping&module=stic_Import_Validation&publish=' + publish + '&import_map_id=' + mappingId + '&import_module=' + importModule;
    var callback = {
                        success: function(o)
                        {
                            var r = YAHOO.lang.JSON.parse(o.responseText);
                            if( r.message != '')
                                alert(r.message);
                        },
                        failure: function(o) {}
                   };
    YAHOO.util.Connect.asyncRequest('GET', url, callback);
    //Toggle the button title
    if(publish == 'yes')
    {
        var newTitle = SUGAR.language.get('stic_Import_Validation','LBL_UNPUBLISH');
        var newPublish = 'no';
    }
    else
    {
        var newTitle = SUGAR.language.get('stic_Import_Validation','LBL_PUBLISH');
        var newPublish = 'yes';
    }

    elem.value = newTitle;
    elem.publish = newPublish;

}
function deleteMapping(elemId, mappingId, importModule )
{
    var elem = document.getElementById(elemId);
    var table = elem.parentNode;
    table.deleteRow(elem.rowIndex);

    var url = 'index.php?action=mapping&module=stic_Import_Validation&delete_map_id=' + mappingId + '&import_module=' + importModule;
    var callback = {
                        success: function(o)
                        {
                            var r = YAHOO.lang.JSON.parse(o.responseText);
                            if( r.message != '')
                                alert(r.message);
                        },
                        failure: function(o) {}
                   };
    YAHOO.util.Connect.asyncRequest('GET', url, callback);
}
var deselectEl = document.getElementById('deselect');
if(deselectEl)
{
    deselectEl.onclick = function() {
        var els = document.getElementsByName('source');
        for(i=0;i<els.length;i++)
        {
            els[i].checked = false;
        }
    }
}

EOJAVASCRIPT;
    }
}
