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
 * interface for studio parsers
 */

#[\AllowDynamicProperties]
class StudioParser
{
    public $positions = array();
    public $rows = array();
    public $cols = array();
    public $curFile = '';
    public $curText = '';
    public $form;
    public $labelEditor = true;
    public $curType = 'detail';
    public $fieldEditor = true;
    public $oldMatches = array();

    public function getFileType($type, $setType=true)
    {
        switch ($type) {
            case 'EditView':$type = 'edit'; break;
            case 'SearchForm': $type= 'search';break;
            case 'ListView': $type= 'list';break;
            default: $type= 'detail';
        }

        if ($setType) {
            $this->curType = $type;
        }
        return $type;
    }

    public function getParsers($file)
    {
        if (substr_count((string) $file, 'DetailView.html') > 0 || substr_count((string) $file, 'EditView.html') > 0) {
            return array('default'=>'StudioParser', array('StudioParser', 'StudioRowParser'));
        }
        if (substr_count((string) $file, 'ListView.html') > 0) {
            return array('default'=>'XTPLListViewParser', array('XTPLListViewParser'));
        }
        return array('default'=>'StudioParser', array('StudioParser'));
    }


    public function parseRows($str)
    {
        preg_match_all("'(<tr[^>]*)>(.*?)(</tr[^>]*>)'si", (string) $str, $this->rows, PREG_SET_ORDER);
    }

    public function parseNames($str)
    {
        $results = array();
        preg_match_all("'name[ ]*=[ ]*[\'\"]+([a-zA-Z0-9\_]+)[\'\"]+'si", (string) $str, $results, PREG_SET_ORDER);
        return $results;
    }

    public function parseLabels($str)
    {
        $mod = array();
        $app = array();
        preg_match_all("'\{MOD\.([a-zA-Z0-9\_]+)\}'si", (string) $str, $mod, PREG_SET_ORDER);
        preg_match_all("'\{APP\.([a-zA-Z0-9\_]+)\}'si", (string) $str, $app, PREG_SET_ORDER);
        return array_merge($app, $mod);
    }

    public function getMaxPosition()
    {
        $max = 0;
        $positionsCount = count($this->positions);
        for ($i = 0; $i < $positionsCount ; $i++) {
            if ($this->positions[$i][2] >= $max) {
                $max = $this->positions[$i][2] + 1;
            }
        }
        return $max;
    }
    public function parsePositions($str, $output= false)
    {
        $results = array();
        preg_match_all("'<span[^>]*sugar=[\'\"]+([a-zA-Z\_]*)([0-9]+)([b]*)[\'\"]+[^>]*>(.*?)</span[ ]*sugar=[\'\"]+[a-zA-Z0-9\_]*[\'\"]+>'si", (string) $str, $results, PREG_SET_ORDER);
        if ($output) {
            return $results;
        }
        $this->positions = $results;
    }
    public function parseCols($str)
    {
        preg_match_all("'(<td[^>]*?)>(.*?)(</td[^>]*?>)'si", (string) $str, $this->cols, PREG_SET_ORDER);
    }
    public function parse($str)
    {
        $this->parsePositions($str);
    }
    public function positionCount($str)
    {
        $result = array();
        return preg_match_all("'<span[^>]*sugar=[\'\"]+([a-zA-Z\_]*)([0-9]+)([b]*)[\'\"]+[^>]*>(.*?)</span[ ]*sugar=[\'\"]+[a-zA-Z0-9\_]*[\'\"]+>'si", (string) $str, $result, PREG_SET_ORDER)/2;
    }
    public function rowCount($str)
    {
        $result = array();
        return preg_match_all("'(<tr[^>]*>)(.*?)(</tr[^>]*>)'si", (string) $str, $result);
    }

    public function loadFile($file)
    {
        $this->curFile = $file;
        $this->curText = file_get_contents($file);
        $this->form = <<<EOQ
		</form>
		<form name='studio'  method='POST'>
			<input type='hidden' name='action' value='save'>
			<input type='hidden' name='module' value='Studio'>

EOQ;
    }
    public static function buildImageButtons($buttons, $horizontal=true)
    {
        $text = '<table cellspacing=2><tr>';
        foreach ($buttons as $button) {
            if (!$horizontal) {
                $text .= '</tr><tr>';
            }
            if (!empty($button['plain'])) {
                $text .= <<<EOQ
				<td valign='center' {$button['actionScript']}>
EOQ;
            } else {
                $text .= <<<EOQ
				<td valign='center' class='button' style='cursor:default' onmousedown='this.className="buttonOn";return false;' onmouseup='this.className="button"' onmouseout='this.className="button"' {$button['actionScript']} >
EOQ;
            }
            if (!isset($button['image'])) {
                $text .= "{$button['text']}</td>";
            } else {
                $text .= "{$button['image']}&nbsp;{$button['text']}</td>";
            }
        }
        $text .= '</tr></table>';
        return $text;
    }

    public function generateButtons()
    {
        global $mod_strings;
        $imageSave = SugarThemeRegistry::current()->getImage('studio_save', '', null, null, '.gif', $mod_strings['LBL_SAVE']);
        $imagePublish = SugarThemeRegistry::current()->getImage('studio_publish', '', null, null, '.gif', $mod_strings['LBL_PUBLISH']);
        $imageHistory = SugarThemeRegistry::current()->getImage('studio_history', '', null, null, '.gif', $mod_strings['LBL_HISTORY']);
        $imageAddRows = SugarThemeRegistry::current()->getImage('studio_addRows', '', null, null, '.gif', $mod_strings['LBL_ADDROWS']);
        $imageUndo = SugarThemeRegistry::current()->getImage('studio_undo', '', null, null, '.gif', $mod_strings['LBL_UNDO']);
        $imageRedo = SugarThemeRegistry::current()->getImage('studio_redo', '', null, null, '.gif', $mod_strings['LBL_REDO']);
        $imageAddField = SugarThemeRegistry::current()->getImage('studio_addField', '', null, null, '.gif', $mod_strings['LBL_ADDFIELD']);
        $buttons = array();

        $buttons[] = array('image'=>$imageUndo,'text'=>$GLOBALS['mod_strings']['LBL_BTN_UNDO'],'actionScript'=>"onclick='jstransaction.undo()'" );
        $buttons[] = array('image'=>$imageRedo,'text'=>$GLOBALS['mod_strings']['LBL_BTN_REDO'],'actionScript'=>"onclick='jstransaction.redo()'" );
        $buttons[] = array('image'=>$imageAddField,'text'=>$GLOBALS['mod_strings']['LBL_BTN_ADDCUSTOMFIELD'],'actionScript'=>"onclick='studiopopup.display();return false;'" );
        $buttons[] = array('image'=>$imageAddRows,'text'=>$GLOBALS['mod_strings']['LBL_BTN_ADDROWS'],'actionScript'=>"onclick='if(!confirmNoSave())return false;document.location.href=\"index.php?module=Studio&action=EditLayout&parser=StudioRowParser\"'" ,);
        $buttons[] = array('image'=>$imageAddRows,'text'=>$GLOBALS['mod_strings']['LBL_BTN_TABINDEX'],'actionScript'=>"onclick='if(!confirmNoSave())return false;document.location.href=\"index.php?module=Studio&action=EditLayout&parser=TabIndexParser\"'" ,);
        $buttons[] = array('image'=>'', 'text'=>'-', 'actionScript'=>'', 'plain'=>true);

        $buttons[] = array('image'=>$imageSave,'text'=>$GLOBALS['mod_strings']['LBL_BTN_SAVE'],'actionScript'=>"onclick='studiojs.save(\"studio\", false);'");
        $buttons[] = array('image'=>$imagePublish,'text'=>$GLOBALS['mod_strings']['LBL_BTN_SAVEPUBLISH'],'actionScript'=>"onclick='studiojs.save(\"studio\", true);'");
        $buttons[] = array('image'=>$imageHistory,'text'=>$GLOBALS['mod_strings']['LBL_BTN_HISTORY'],'actionScript'=>"onclick='if(!confirmNoSave())return false;document.location.href=\"index.php?module=Studio&action=wizard&wizard=ManageBackups&setFile={$_SESSION['studio']['selectedFileId']}\"'");
        return $buttons;
    }
    public function getFormButtons()
    {
        $buttons = $this->generateButtons();
        return self::buildImageButtons($buttons);
    }
    public function getForm()
    {
        return $this->form  . <<<EOQ
		</form>


EOQ;
    }



    public function getFiles($module, $fileId=false)
    {
        if (empty($GLOBALS['studioDefs'][$module])) {
            require_once('modules/'. $module . '/metadata/studio.php');
        }
        if ($fileId) {
            return 	$GLOBALS['studioDefs'][$module][$fileId];
        }
        return $GLOBALS['studioDefs'][$module];
    }


    public function getWorkingFile($file, $refresh = false)
    {
        $workingFile = 'working/' . $file;
        $customFile = create_custom_directory($workingFile);
        if ($refresh || !file_exists($customFile)) {
            copy($file, $customFile);
        }
        return $customFile;
    }

    public function getSwapWith($value)
    {
        return $value * 2 - 1;
    }
    /**
     * takes the submited form and parses the file moving the fields around accordingly
     * it also checks if the original file has a matching field and uses that field instead of attempting to generate a new one
     */
    public function handleSave()
    {
        $this->parseOldestFile($this->curFile);
        $fileDef = $this->getFiles($_SESSION['studio']['module'], $_SESSION['studio']['selectedFileId']);
        $type = $this->getFileType($fileDef['type']);
        $view = $this->curText;
        $counter = 0;
        $return_view = '';
        $slotCount = 0;
        $slotLookup = array();
        $positionsCount = count($this->positions);
        for ($i = 0; $i < $positionsCount; $i ++) {
            //used for reverse lookups to figure out where the associated slot is
            $slotLookup[$this->positions[$i][2]][$this->positions[$i][3]] = array('position'=>$i, 'value'=>$this->positions[$i][4]);
        }

        $customFields = $this->focus->custom_fields->getAllBeanFieldsView($type, 'html');
        //now we set it to the new values
        $positionsCount = count($this->positions);

        //now we set it to the new values

        for ($i = 0; $i < $positionsCount; $i ++) {
            $slot = $this->positions[$i];

            if (empty($slot[3])) {
                $slotCount ++;

                //if the value in the request doesn't equal our current slot then something should be done
                if (isset($_REQUEST['slot_'.$slotCount]) && $_REQUEST['slot_'.$slotCount] != $slotCount) {
                    $swapValue = $_REQUEST['slot_'.$slotCount] ;
                    //if its an int then its a simple swap
                    if (is_numeric($swapValue)) {
                        $swapWith = $this->positions[$this->getSwapWith($swapValue)];

                        //label
                        $slotLookup[$slot[2]]['']['value'] = $this->positions[ $slotLookup[$swapWith[2]]['']['position']][4];
                        //html
                        $slotLookup[$slot[2]]['b']['value'] = $this->positions[ $slotLookup[$swapWith[2]]['b']['position']][4];
                    }
                    //now check if its a delete action
                    if (strcmp('add:delete', $swapValue) == 0) {
                        //label
                        $slotLookup[$slot[2]][$slot[3]]['value'] = '&nbsp;';
                        //html
                        $slotLookup[$slot[2]]['b']['value'] = '&nbsp;';
                    } else {

                        //now handle the adding of custom fields
                        if (substr_count((string) $swapValue, 'add:')) {
                            $addfield = explode('add:', $_REQUEST['slot_'.$slotCount], 2);

                            //label
                            $slotLookup[$slot[2]][$slot[3]]['value'] = $customFields[$addfield[1]]['label'] ;
                            //html
                            if (!empty($this->oldMatches[$addfield[1]])) {
                                //we have an exact match from the original file use that
                                $slotLookup[$slot[2]]['b']['value'] = $this->oldMatches[$addfield[1]];
                            } else {
                                if (!empty($this->oldLabels[$customFields[$addfield[1]]['label']])) {
                                    //we have matched the label from the original file use that
                                    $slotLookup[$slot[2]]['b']['value'] = $this->oldLabels[$customFields[$addfield[1]]['label']];
                                } else {
                                    //no matches so use what we are generating
                                    $slotLookup[$slot[2]]['b']['value'] = $customFields[$addfield[1]]['html'];
                                }
                            }
                        }
                    }
                }
            }
        }
        $positionsCount = count($this->positions);

        for ($i = 0; $i < $positionsCount; $i ++) {
            $slot = $this->positions[$i];
            $explode = explode($slot[0], $view, 2);
            $explode[0] .= "<span sugar='". $slot[1] . $slot[2]. $slot[3]. "'>";
            $explode[1] = "</span sugar='" .$slot[1] ."'>".$explode[1];

            $return_view .= $explode[0].$slotLookup[$slot[2]][$slot[3]]['value'];
            $view = $explode[1];
            $counter ++;
        }
        $return_view .= $view;

        $this->saveFile('', $return_view);
        return $return_view;
    }

    public function saveFile($file = '', $contents = false)
    {
        if (empty($file)) {
            $file = $this->curFile;
        }

        $output = $contents ? $contents : $this->curText;
        if (strpos((string) $file, 'SearchForm.html') > 0) {
            $fileparts = preg_split("'<!--\s*(BEGIN|END)\s*:\s*main\s*-->'", (string) $output);
            if (!empty($fileparts) && count($fileparts) > 1) {
                $function = function ($matches) {
                    $name = str_replace(array("[", "]"), "", (string) $matches[1]);
                    if ((strpos($name, "LBL_") === 0) && (strpos($name, "_basic") === 0)) {
                        return str_replace($name, $name . "_basic", (string) $matches[0]);
                    }
                    return  $matches[0];
                };
                //preg_replace_callback doesn't seem to work w/o anonymous method
                $output = preg_replace_callback(
                    "/name\s*=\s*[\"']([^\"']*)[\"']/Us",
                    $function,
                    $fileparts[1]
                );



                $output = $fileparts[0] . '<!-- BEGIN:main -->' . $output . '<!-- END:main -->' . $fileparts[2];
            }
        }

        sugar_file_put_contents($file, $output);
    }

    public function handleSaveLabels($module_name, $language)
    {
        require_once('modules/Studio/LabelEditor/LabelEditor.php');
        LabelEditor::saveLabels($_REQUEST, $module_name, $language);
    }

    /**
     * UTIL FUNCTIONS
     */
    /**
     * STATIC FUNCTION DISABLE INPUTS IN AN HTML STRING
     *
     */
    public function disableInputs($str)
    {
        $match = array("'(<input)([^>]*>)'si" => "\$1 disabled readonly $2",
    "'(<input)([^>]*?type[ ]*=[ ]*[\'\"]submit[\'\"])([^>]*>)'si" => "\$1 disabled readonly style=\"display:none\" $2",
     "'(<select)([^>]*)'si" => "\$1 disabled readonly $2",
        // "'<a .*>(.*)</a[^>]*>'siU"=>"\$1",
"'(href[\ ]*=[\ ]*)([\'])([^\']*)([\'])'si" => "href=\$2javascript:void(0);\$2 alt=\$2\$3\$2", "'(href[\ ]*=[\ ]*)([\"])([^\"]*)([\"])'si" => "href=\$2javascript:void(0)\$2 title=\$2\$3\$2");
        return preg_replace(array_keys($match), array_values($match), (string) $str);
    }

    public function enableLabelEditor($str)
    {
        global $mod_strings;
        $image = SugarThemeRegistry::current()->getImage('edit_inline', "onclick='studiojs.handleLabelClick(\"$2\", 1);' onmouseover='this.style.cursor=\"default\"'", null, null, '.gif', $mod_strings['LBL_EDIT']);
        $match = array("'>[^<]*\{(MOD.)([^\}]*)\}'si" => "$image<span id='label$2' onclick='studiojs.handleLabelClick(\"$2\", 2);' >{".'$1$2' . "}</span><span id='span$2' style='display:none'><input type='text' id='$2' name='$2' msi='label' value='{".'$1$2' . "}' onblur='studiojs.endLabelEdit(\"$2\")'></span>");
        $keys = array_keys($match);
        $matches = array();
        preg_match_all($keys[0], (string) $str, $matches, PREG_SET_ORDER);
        foreach ($matches as $labelmatch) {
            $label_name = 'label_' . $labelmatch[2];
            $this->form .= "\n<input type='hidden' name='$label_name'  id='$label_name' value='no_change'>";
        }
        return preg_replace(array_keys($match), array_values($match), (string) $str);
    }



    public function writeToCache($file, $view, $preview_file=false)
    {
        if (!is_writable($file)) {
            echo "<br><span style='color:red'>Warning: $file is not writeable. Please make sure it is writeable before continuing</span><br><br>";
        }

        if (!$preview_file) {
            $file_cache = create_cache_directory('studio/'.$file);
        } else {
            $file_cache = create_cache_directory('studio/'.$preview_file);
        }
        $view = $this->disableInputs($view);
        if (!$preview_file) {
            $view = $this->enableLabelEditor($view);
        }
        sugar_file_put_contents($file_cache, $view);
        return $this->cacheXTPL($file, $file_cache, $preview_file);
    }

    public function populateRequestFromBuffer($file)
    {
        $buffer = '';
        $results = array();
        $temp = sugar_file_get_contents($file);
        preg_match_all("'name[\ ]*=[\ ]*[\']([^\']*)\''si", (string) $buffer, $results);
        $res = $results[1];
        foreach ($res as $r) {
            $_REQUEST[$r] = $r;
        }
        preg_match_all("'name[\ ]*=[\ ]*[\"]([^\"]*)\"'si", (string) $buffer, $results);
        $res = $results[1];
        foreach ($res as $r) {
            $_REQUEST[$r] = $r;
        }

        $_REQUEST['query'] = true;
        $_REQUEST['advanced'] = true;
    }
    public function cacheXTPL($file, $cache_file, $preview_file = false)
    {
        global $beanList;
        //now if we have a backup_file lets use that instead of the original
        if ($preview_file) {
            $file  = $preview_file;
        }


        if (!isset($the_module)) {
            $the_module = $_SESSION['studio']['module'];
        }
        $files = (new StudioParser())->getFiles($the_module);
        $xtpl = $files[$_SESSION['studio']['selectedFileId']]['php_file'];
        $originalFile = $files[$_SESSION['studio']['selectedFileId']]['template_file'];
        $type = (new StudioParser())->getFileType($files[$_SESSION['studio']['selectedFileId']]['type']);
        $buffer = sugar_file_get_contents($xtpl);
        $cache_file = create_cache_directory('studio/'.$file);
        $xtpl_cache = create_cache_directory('studio/'.$xtpl);
        $module = $this->workingModule;

        $form_string = "require_once('modules/".$module."/Forms.php');";

        if ($type == 'edit' || $type == 'detail') {
            if (empty($_REQUEST['record'])) {
                $buffer = preg_replace('(\$xtpl[\ ]*=)', "\$focus->assign_display_fields('$module'); \$0", (string) $buffer);
            } else {
                $buffer = preg_replace('(\$xtpl[\ ]*=)', "\$focus->retrieve('".$_REQUEST['record']."');\n\$focus->assign_display_fields('$module');\n \$0", (string) $buffer);
            }
        }
        $_REQUEST['query'] = true;
        if (substr_count((string) $file, 'SearchForm') > 0) {
            $temp_xtpl = new XTemplate($file);
            if ($temp_xtpl->exists('advanced')) {
                global $current_language, $beanFiles, $beanList;
                $mods = return_module_language($current_language, 'DynamicLayout');
                $class_name = $beanList[$module];
                require_once($beanFiles[$class_name]);
                $mod = new $class_name();

                $this->populateRequestFromBuffer($file);
                $mod->assign_display_fields($module);
                $buffer = str_replace(array('echo $lv->display();','$search_form->parse("advanced");', '$search_form->out("advanced");', '$search_form->parse("main");', '$search_form->out("main");'), '', (string) $buffer);
                $buffer = str_replace('echo get_form_footer();', '$search_form->parse("main");'."\n".'$search_form->out("main");'."\necho '<br><b>".translate('LBL_ADVANCED', 'DynamicLayout')."</b><br>';".'$search_form->parse("advanced");'."\n".'$search_form->out("advanced");'."\n \$sugar_config['list_max_entries_per_page'] = 1;", $buffer);
            }
        } else {
            if ($type == 'detail') {
                $buffer = str_replace('header(', 'if(false) header(', (string) $buffer);
            }
        }

        $buffer = str_replace($originalFile, $cache_file, (string) $buffer);
        $buffer = "<?php\n\$sugar_config['list_max_entries_per_page'] = 1;\n ?>".$buffer;

        $buffer = str_replace($form_string, '', $buffer);
        $buffer = $this->disableInputs($buffer);
        sugar_file_put_contents($xtpl_cache, $buffer);
        return $xtpl_cache;
    }

    /**
     * Yahoo Drag & Drop Support
     */
    ////<script type="text/javascript" src="modules/Studio/studio.js" ></script>
    public function yahooJS()
    {
        $custom_module = $_SESSION['studio']['module'];
        $custom_type = $this->curType;
        $v = getVersionedPath('');
        return<<<EOQ
		<style type='text/css'>
		.slot {
		border-width:1px;border-color:#999999;border-style:solid;padding:0px 1px 0px 1px;margin:2px;cursor:move;

	}

	.slotB {
	border-width:0;cursor:move;

	}
	</style>

	<!-- Namespace source file -->

	<script type="text/javascript" src="modules/Studio/JSTransaction.js?v=$v" ></script>
	<script>
	var jstransaction = new JSTransaction();
	</script>

	<!-- Drag and Drop source file -->
	<script type="text/javascript" src="include/javascript/yui/build/dragdrop/dragdrop.js?v=$v" ></script>
	<script type="text/javascript" src="modules/Studio/studiodd.js?v=$v" ></script>
	<script type="text/javascript" src="modules/Studio/studio.js?v=$v" ></script>
	<script>

	var yahooSlots = [];

	function dragDropInit(){

	YAHOO.util.DDM.mode = YAHOO.util.DDM.POINT;

	for(mj = 0; mj <= $this->yahooSlotCount; mj++){
	yahooSlots["slot" + mj] = new ygDDSlot("slot" + mj, "studio");
	}
	for(mj = 0; mj < dyn_field_count; mj++){
	yahooSlots["dyn_field_" + mj] = new ygDDSlot("dyn_field_" + mj, "studio");
	}
	// initPointMode();
	yahooSlots['s_field_delete'] =  new YAHOO.util.DDTarget("s_field_delete", 'studio');
	}

	YAHOO.util.Event.addListener(window, "load", dragDropInit);
	var custom_module = '$custom_module';
	var custom_view = '$custom_type';

			</script>

EOQ;
    }

    /**
     * delete:-1
     * add:2000
     * swap: 0 - 1999
     *
     */
    public function addSlotToForm($slot_count, $display_count)
    {
        $this->form .= "\n<input type='hidden' name='slot_$slot_count'  id='slot_$display_count' value='$slot_count'>";
    }
    public function prepSlots()
    {
        $view = $this->curText;
        $counter = 0;
        $return_view = '';
        $slotCount = 0;
        $positionsCount = count($this->positions);
        for ($i = 0; $i < $positionsCount; $i ++) {
            $slot = $this->positions[$i];
            $class = '';

            if (empty($this->positions[$i][3])) {
                $slotCount ++;
                $class = " class='slot' ";
                $displayCount = $slotCount. $this->positions[$i][3];
                $this->addSlotToForm($slotCount, $displayCount);
            } else {
                $displayCount = $slotCount. $this->positions[$i][3];
            }


            $explode = explode($slot[0], $view, 2);
            $style = '';
            $explode[0] .= "<div id = 'slot$displayCount'  $class style='cursor: move$style'>";
            $explode[1] = "</div>".$explode[1];
            $return_view .= $explode[0].$slot[4];
            $view = $explode[1];
            $counter ++;
        }
        $this->yahooSlotCount = $slotCount;
        $newView = $return_view.$view;
        $newView = str_replace(array('<span>', '</span>'), array('', ''), $newView);

        return $newView;
    }

    public function parseOldestFile($file)
    {
        ob_clean();

        $slotLookup = [];

        require_once('modules/Studio/SugarBackup.php');
        $file = str_replace('custom/working/', '', (string) $file);

        $filebk = SugarBackup::oldestBackup($file);
        $oldMatches = array();
        $oldLabels = array();
        // echo $filebk;
        if ($filebk) {
            $content = file_get_contents($filebk);
            $positions = $this->parsePositions($content, true);
            $positionsCount = count($positions);
            for ($i = 0; $i < $positionsCount; $i ++) {
                $position = $positions[$i];
                //used for reverse lookups to figure out where the associated slot is
                $slotLookup[$position[2]][$position[3]] = array('position'=>$i, 'value'=>$position[4]);
                $names = $this->parseNames($position[4]);
                $labels = $this->parseLabels($position[4]);

                foreach ($names as $name) {
                    $oldMatches[$name[1]] = $position[0];
                }
                foreach ($labels as $label) {
                    $oldLabels[$label[0]] = $position[2];
                }
            }
        }
        foreach ($oldLabels as $key=>$value) {
            $oldLabels[$key] = $slotLookup[$value]['b']['value'];
        }

        $this->oldLabels = $oldLabels;
        $this->oldMatches = $oldMatches;
    }


    public function clearWorkingDirectory()
    {
        $file = 'custom/working/';
        if (file_exists($file)) {
            rmdir_recursive($file);
        }

        return true;
    }

    /**
     * UPGRADE TO SMARTY
     */
    public function upgradeToSmarty()
    {
        return str_replace('{', '{$', (string) $this->curText);
    }
}
