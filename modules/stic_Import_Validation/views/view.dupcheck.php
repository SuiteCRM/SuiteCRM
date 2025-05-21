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
require_once('modules/stic_Import_Validation/sources/ImportFile.php');
require_once('modules/stic_Import_Validation/ImportFileSplitter.php');
require_once('modules/stic_Import_Validation/ImportCacheFiles.php');
require_once('modules/stic_Import_Validation/ImportDuplicateCheck.php');

require_once('include/upload_file.php');

#[\AllowDynamicProperties]
class stic_Import_ValidationViewDupcheck extends stic_Import_ValidationView
{
    protected $pageTitleKey = 'LBL_STEP_DUP_TITLE';

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings, $app_strings, $current_user;
        global $sugar_config;

        $has_header = $_REQUEST['has_header'] == 'on' ? true : false;

        $this->instruction = 'LBL_SELECT_DUPLICATE_INSTRUCTION';
        $this->ss->assign('INSTRUCTION', $this->getInstruction());

        $this->ss->assign("MODULE_TITLE", $this->getModuleTitle(false));
        $this->ss->assign("DELETE_INLINE_PNG", SugarThemeRegistry::current()->getImage('delete_inline', 'align="absmiddle" alt="'.$app_strings['LNK_DELETE'].'" border="0"'));
        $this->ss->assign("PUBLISH_INLINE_PNG", SugarThemeRegistry::current()->getImage('publish_inline', 'align="absmiddle" alt="'.$mod_strings['LBL_PUBLISH'].'" border="0"'));
        $this->ss->assign("UNPUBLISH_INLINE_PNG", SugarThemeRegistry::current()->getImage('unpublish_inline', 'align="absmiddle" alt="'.$mod_strings['LBL_UNPUBLISH'].'" border="0"'));
        $this->ss->assign("IMPORT_MODULE", $_REQUEST['import_module']);
        $this->ss->assign("CURRENT_STEP", $this->currentStep);
        $this->ss->assign("JAVASCRIPT", $this->_getJS());

        $content = $this->ss->fetch('modules/stic_Import_Validation/tpls/dupcheck.tpl');
        $this->ss->assign("CONTENT", $content);
        $this->ss->display('modules/stic_Import_Validation/tpls/wizardWrapper.tpl');
    }

    private function getImportMap()
    {
        if (!empty($_REQUEST['source_id'])) {
            $import_map_seed = BeanFactory::newBean('Import_1');
            $import_map_seed->retrieve($_REQUEST['source_id'], false);

            return $import_map_seed->getMapping();
        } else {
            return array();
        }
    }

    /**
     * Returns JS used in this view
     */
    private function _getJS()
    {
        global $mod_strings, $sugar_config;

        $has_header = $_REQUEST['has_header'] == 'on' ? true : false;
        $uploadFileName = "upload://".basename((string) $_REQUEST['tmp_file']);
        $splitter = new ImportFileSplitter($uploadFileName, $sugar_config['import_max_records_per_file']);
        $splitter->splitSourceFile($_REQUEST['custom_delimiter'], html_entity_decode((string) $_REQUEST['custom_enclosure'], ENT_QUOTES), $has_header);
        $count = $splitter->getFileCount()-1;
        $recCount = $splitter->getRecordCount();

        //BEGIN DRAG DROP WIDGET
        $idc = new ImportDuplicateCheck($this->bean);
        $dupe_indexes = $idc->getDuplicateCheckIndexes();

        //grab all the import enabled fields and the field map
        $field_map = $this->getImportMap();
        $import_fields = $idc->getDuplicateCheckIndexedFiles();

        //check for saved entries from mapping
        $dupe_disabled =  array();
        $dupe_enabled =  array();
        $mapped_fields = array('full_name');

        //grab the list of user mapped fields
        foreach ($_REQUEST as $req_k => $req_v) {
            if (strpos($req_k, 'olnum')>0) {
                if (empty($req_v) || $req_v != '-1') {
                    $mapped_fields[] = $req_v;
                }
            }
        }

        foreach ($import_fields as $ik=>$iv) {

             //grab the field value from the key
            $ik_field = explode('::', $ik);

            //field is not a custom field and was not included in the key, or was not in mapped fields, so skip
            if (strpos($ik_field[0], 'ustomfield::')>0 || (empty($ik_field[1]) || !in_array($ik_field[1], $mapped_fields))) {
                //skip indexed fields that are not defined in user mapping or
                continue;
            }

            if (isset($field_map['dupe_'.$ik])) {
                //index is defined in mapping, so set this index as enabled if not already defined
                $dupe_enabled[] =  array("dupeVal" => $ik, "label" => $iv);
            } else {
                //index is not defined in mapping, so display as disabled if not already defined
                $dupe_disabled[] =  array("dupeVal" => $ik, "label" => $iv);
            }
        }

        $enabled_dupes = json_encode($dupe_enabled);
        $disabled_dupes = json_encode($dupe_disabled);

        $stepTitle4 = $mod_strings['LBL_STIC_IMPORT_VALIDATION_RECORDS'];

        $dateTimeFormat = $GLOBALS['timedate']->get_cal_date_time_format();
        $type = (isset($_REQUEST['type'])) ? $_REQUEST['type'] : '';
        $lblUsed = str_replace(":", "", (string) $mod_strings['LBL_INDEX_USED']);
        $lblNotUsed = str_replace(":", "", (string) $mod_strings['LBL_INDEX_NOT_USED']);
        return <<<EOJAVASCRIPT




/**
 * Singleton to handle processing the import
 */
ProcessImport = new function()
{
    /*
     * number of file to process processed
     */
    this.fileCount         = 0;

    /*
     * total files to processs
     */
    this.fileTotal         = {$count};

    /*
     * total records to process
     */
    this.recordCount       = {$recCount};

    /*
     * maximum number of records per file
     */
    this.recordThreshold   = {$sugar_config['import_max_records_per_file']};

    /*
     * submits the form
     */
    this.submit = function()
    {
        document.getElementById("importstepdup").tmp_file.value =
            document.getElementById("importstepdup").tmp_file_base.value + '-' + this.fileCount;
        YAHOO.util.Connect.setForm(document.getElementById("importstepdup"));
        YAHOO.util.Connect.asyncRequest('POST', 'index.php',
            {
                success: function(o) {
                    if (o.responseText.replace(/^\s+|\s+$/g, '') != '') {
                        this.failure(o);
                    }
                    else {
                        var locationStr = "index.php?module=stic_Import_Validation"
                            + "&action=Last"
                            + "&current_step=" + document.getElementById("importstepdup").current_step.value
                            + "&type={$type}"
                            + "&import_module={$_REQUEST['import_module']}"
                            + "&has_header=" +  document.getElementById("importstepdup").has_header.value ;
                        if ( ProcessImport.fileCount >= ProcessImport.fileTotal ) {
                        	YAHOO.SUGAR.MessageBox.updateProgress(1,'{$mod_strings['LBL_STIC_IMPORT_VALIDATION_COMPLETED']}');
                        	SUGAR.util.hrefURL(locationStr);
                        }
                        else {
                            document.getElementById("importstepdup").save_map_as.value = '';
                            ProcessImport.fileCount++;
                            ProcessImport.submit();
                        }
                    }
                },
                failure: function(o) {
                	YAHOO.SUGAR.MessageBox.minWidth = 500;
                	YAHOO.SUGAR.MessageBox.show({
                    	type:  "alert",
                    	title: '{$mod_strings['LBL_STIC_IMPORT_VALIDATION_ERROR']}',
                    	msg:   o.responseText,
                        fn: function() { window.location.reload(true); }
                    });
                }
            }
        );
        var move = 0;
        if ( this.fileTotal > 0 ) {
            move = this.fileCount/this.fileTotal;
        }
        YAHOO.SUGAR.MessageBox.updateProgress( move,
            "{$mod_strings['LBL_STIC_IMPORT_VALIDATION_RECORDS']} " + ((this.fileCount * this.recordThreshold) + 1)
                        + " {$mod_strings['LBL_STIC_IMPORT_VALIDATION_RECORDS_TO']} " + Math.min(((this.fileCount+1) * this.recordThreshold),this.recordCount)
                        + " {$mod_strings['LBL_STIC_IMPORT_VALIDATION_RECORDS_OF']} " + this.recordCount );
    }

    /*
     * begins the form submission process
     */
    this.begin = function()
    {
        datestarted = '{$mod_strings['LBL_STIC_IMPORT_VALIDATION_STARTED']} ' +
                YAHOO.util.Date.format('{$dateTimeFormat}');
        YAHOO.SUGAR.MessageBox.show({
            title: '{$stepTitle4}',
            msg: datestarted,
            width: 500,
            type: "progress",
            closable:false,
            animEl: 'importnow'
        });
        SUGAR.saveConfigureDupes();
        this.submit();
    }
}

//begin dragdrop code
	var enabled_dupes = {$enabled_dupes};
	var disabled_dupes = {$disabled_dupes};
	var lblEnabled = '{$lblUsed}';
	var lblDisabled = '{$lblNotUsed}';


	SUGAR.enabledDupesTable = new YAHOO.SUGAR.DragDropTable(
		"enabled_div",
		[{key:"label",  label: lblEnabled, width: 225, sortable: false},
		 {key:"module", label: lblEnabled, hidden:true}],
		new YAHOO.util.LocalDataSource(enabled_dupes, {
			responseSchema: {
			   resultsList : "dupeVal",
			   fields : [{key : "dupeVal"}, {key : "label"}]
			}
		}),
		{
			height: "300px",
			group: ["enabled_div", "disabled_div"]
		}
	);
	SUGAR.disabledDupesTable = new YAHOO.SUGAR.DragDropTable(
		"disabled_div",
		[{key:"label",  label: lblDisabled, width: 225, sortable: false},
		 {key:"module", label: lblDisabled, hidden:true}],
		new YAHOO.util.LocalDataSource(disabled_dupes, {
			responseSchema: {
			   resultsList : "dupeVal",
			   fields : [{key : "dupeVal"}, {key : "label"}]
			}
		}),
		{
			height: "300px",
		 	group: ["enabled_div", "disabled_div"]
		 }
	);
	SUGAR.enabledDupesTable.disableEmptyRows = true;
    SUGAR.disabledDupesTable.disableEmptyRows = true;
    SUGAR.enabledDupesTable.addRow({module: "", label: ""});
    SUGAR.disabledDupesTable.addRow({module: "", label: ""});
	SUGAR.enabledDupesTable.render();
	SUGAR.disabledDupesTable.render();


	SUGAR.saveConfigureDupes = function()
	{
		var enabledTable = SUGAR.enabledDupesTable;
		var dupeVal = [];
		for(var i=0; i < enabledTable.getRecordSet().getLength(); i++){
			var data = enabledTable.getRecord(i).getData();
			if (data.dupeVal && data.dupeVal != '')
			    dupeVal[i] = data.dupeVal;
		}
		    YAHOO.util.Dom.get('enabled_dupes').value = YAHOO.lang.JSON.stringify(dupeVal);

        var disabledTable = SUGAR.disabledDupesTable;
		var dupeVal = [];
		for(var i=0; i < disabledTable.getRecordSet().getLength(); i++){
			var data = disabledTable.getRecord(i).getData();
			if (data.dupeVal && data.dupeVal != '')
			    dupeVal[i] = data.dupeVal;
		}
			YAHOO.util.Dom.get('disabled_dupes').value = YAHOO.lang.JSON.stringify(dupeVal);
	}




document.getElementById('importnow').onclick = function(){
    SUGAR.saveConfigureDupes();

    var form = document.getElementById('importstepdup');
    // Move on to next step
    document.getElementById('importstepdup').action.value = 'Step4';
    ProcessImport.begin();
}


enableQS(false);

EOJAVASCRIPT;
    }
}
