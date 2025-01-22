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







class Popup_Picker
{
    public $_popupMeta;
    public $_create = false;
    public $_hide_clear_button = false;

    /**
     * Creates a new Popup_Picker object. Controls displaying of single select and multi select popups
     *
     */
    public function __construct()
    {
        global $currentModule, $popupMeta;

        // cn: bug 12269 - directory navigation attack - detect and stop.
        if (isset($_REQUEST['metadata']) && strpos($_REQUEST['metadata'], "..") !== false) {
            die("Directory navigation attack denied.");
        }
        if (empty($popupMeta)) {
            if (!empty($_REQUEST['metadata']) && $_REQUEST['metadata'] != 'undefined') { // if custom metadata is requested
                require_once('modules/' . $currentModule . '/metadata/' . $_REQUEST['metadata'] . '.php');
            } else {
                require_once('modules/' . $currentModule . '/metadata/popupdefs.php');
            }
        }
        $this->_popupMeta = $popupMeta;

        require_once('modules/' . $currentModule . '/' . $this->_popupMeta['moduleMain'] . '.php');
        if (isset($this->_popupMeta['create']['formBase']) && isset($_REQUEST['create']) && $_REQUEST['create'] == 'true') { // include create form
            require_once('modules/' . $currentModule . '/' . $this->_popupMeta['create']['formBase']);
            $this->_create = true;
        }
    }



    /*
     *
     */
    public function _get_where_clause()
    {
        $where = '';
        $whereClauses = array();
        if (isset($_REQUEST['query'])) {
            foreach (array_keys($this->_popupMeta['whereClauses']) as $key) {
                append_where_clause($whereClauses, $key, $this->_popupMeta['whereClauses'][$key]);
            }

            $where = generate_where_statement($whereClauses);
        }
        if (!empty($this->_popupMeta['whereStatement'])) {
            if (!empty($where)) {
                $where .= ' AND ';
            }
            $where .= $this->_popupMeta['whereStatement'];
        }

        return $where;
    }

    /**
     *
     */
    public function process_page()
    {
        global $theme;
        global $mod_strings;
        global $app_strings;
        global $currentModule;
        global $app_list_strings, $sugar_version, $sugar_config;

        $output_html = "<script type=\"text/javascript\" src=\"" . getJSPath('include/javascript/sugar_3.js'). "\"></script>";
        $where = '';

        if (empty($_REQUEST[$currentModule . '_' . strtoupper($this->_popupMeta['moduleMain']) . '_offset'])) {
            $_POST[$currentModule . '_' . strtoupper($this->_popupMeta['moduleMain']) . '_offset'] = '';
        }
        if (empty($_REQUEST['saved_associated_data'])) {
            $_POST['saved_associated_data'] = '';
        }
        $where = $this->_get_where_clause();

        // CREATE STUFF
        if ($this->_create) {
            $formBase = new $this->_popupMeta['create']['formBaseClass']();
            if (isset($_REQUEST['doAction']) && $_REQUEST['doAction'] == 'save') {
                $formBase->handleSave('', false, true);
            }

            $lbl_save_button_title = $app_strings['LBL_SAVE_BUTTON_TITLE'];
            $lbl_save_button_key = $app_strings['LBL_SAVE_BUTTON_KEY'];
            $lbl_save_button_label = $app_strings['LBL_SAVE_BUTTON_LABEL'];

            // TODO: cleanup the construction of $addform
            $prefix = empty($this->_popupMeta['create']['getFormBodyParams'][0]) ? '' : $this->_popupMeta['create']['getFormBodyParams'][0];
            $mod = empty($this->_popupMeta['create']['getFormBodyParams'][1]) ? '' : $this->_popupMeta['create']['getFormBodyParams'][1];
            $formBody = empty($this->_popupMeta['create']['getFormBodyParams'][2]) ? '' : $this->_popupMeta['create']['getFormBodyParams'][2];

            $getFormMethod = (empty($this->_popupMeta['create']['getFormMethod']) ? 'getFormBody' : $this->_popupMeta['create']['getFormMethod']);
            $formbody = $formBase->$getFormMethod($prefix, $mod, $formBody);

            $addform = '<table><tr><td nowrap="nowrap" valign="top">'
                . str_replace('<br>', '</td><td nowrap="nowrap" valign="top">&nbsp;', $formbody)
                . '</td></tr></table>'
                . '<input type="hidden" name="action" value="Popup" />';
            $formSave = <<<EOQ
			<input type="hidden" name="create" value="true">
			<input type="hidden" name="popup" value="true">
			<input type="hidden" name="to_pdf" value="true">
			<input type="hidden" name="return_module" value="$currentModule">
			<input type="hidden" name="return_action" value="Popup">
			<input type="submit" name="button" class="button" title="$lbl_save_button_title" value="  $lbl_save_button_label  " />
			<input type="button" name="button" class="button" title="{$app_strings['LBL_CANCEL_BUTTON_TITLE']}" accesskey="{$app_strings['LBL_CANCEL_BUTTON_KEY']}" value="{$app_strings['LBL_CANCEL_BUTTON_LABEL']}" onclick="toggleDisplay('addform');" />
EOQ;
            // if metadata contains custom inputs for the quickcreate
            if (!empty($this->_popupMeta['customInput']) && is_array($this->_popupMeta['customInput'])) {
                foreach ($this->_popupMeta['customInput'] as $key => $value) {
                    $formSave .= '<input type="hidden" name="' . $key . '" value="'. $value .'">\n';
                }
            }
            $createButtonTranslation = translate($this->_popupMeta['create']['createButton']);
            $createButton = <<<EOQ
			<input type="button" id="showAdd" name="showAdd" class="button" value="{$createButtonTranslation}" onclick="toggleDisplay('addform');" />
EOQ;
            $addformheader = get_form_header($createButtonTranslation, $formSave, false);
        }
        // END CREATE STUFF


        // search request inputs
        $searchInputs = array();
        foreach ($this->_popupMeta['searchInputs'] as $input) {
            $searchInputs[$input] = empty($_REQUEST[$input]) ? '' : $_REQUEST[$input];
        }

        $request_data = empty($_REQUEST['request_data']) ? '' : $_REQUEST['request_data'];
        $hide_clear_button = empty($_REQUEST['hide_clear_button']) && empty($this->_hide_clear_button) ? false : true;
        $button = '<script>eval("var request_data = " + window.document.forms[\'popup_query_form\'].request_data.value);</script>';

        if (isset($_REQUEST['mass'])) {
            foreach (array_unique($_REQUEST['mass']) as $record) {
                $button .= "<input style='display: none' checked type='checkbox' name='mass[]' value='$record'>\n";
            }
        }

        //START:FOR MULTI-SELECT
        $multi_select = false;
        if (!empty($_REQUEST['mode']) && strtoupper($_REQUEST['mode']) == 'MULTISELECT') {
            $multi_select = true;
            $button .= "<input type='hidden' name='mode' value='MultiSelect'>";
            $button .= "<input type='button' name='button' class='button' onclick=\"send_back_selected('$currentModule',document.MassUpdate,'mass[]','" .$app_strings['ERR_NOTHING_SELECTED']."', request_data.field_to_name_array);\" title='"
                .$app_strings['LBL_SELECT_BUTTON_TITLE']."' value='  "
                .$app_strings['LBL_SELECT_BUTTON_LABEL']."  ' />\n";
        }

        //END:FOR MULTI-SELECT
        if (!$hide_clear_button) {
            $button .= "<input type='button' name='button' class='button' onclick=\"send_back('','');\" title='"
                .$app_strings['LBL_CLEAR_BUTTON_TITLE']."' value='  "
                .$app_strings['LBL_CLEAR_BUTTON_LABEL']."  ' />\n";
        }
        $button .= "<input type='submit' name='button' class='button' onclick=\"window.close();\" title='"
            .$app_strings['LBL_CANCEL_BUTTON_TITLE']."' value='  "
            .$app_strings['LBL_CANCEL_BUTTON_LABEL']."  ' />\n";

        if (isset($this->_popupMeta['templateForm'])) {
            $form = new XTemplate($this->_popupMeta['templateForm']);
        } else {
            $form = new XTemplate('modules/' . $currentModule . '/Popup_picker.html');
        }

        $form->assign('MOD', $mod_strings);
        $form->assign('APP', $app_strings);
        $form->assign('THEME', $theme);
        $form->assign('MODULE_NAME', $currentModule);
        $form->assign('request_data', $request_data);

        // CREATE STUFF
        if ($this->_create) {
            $form->assign('CREATEBUTTON', $createButton);
            $form->assign('ADDFORMHEADER', $addformheader);
            $form->assign('ADDFORM', $addform);
        }
        // CREATE STUFF

        if (isset($this->_popupMeta['className'])) {
            $seed_bean = new $this->_popupMeta['className']();
        } else {
            $seed_bean = new $this->_popupMeta['moduleMain']();
        }

        // assign search inputs to xtemplates
        foreach (array_keys($searchInputs) as $key) {
            if (!empty($_REQUEST[$key]) && (isset($seed_bean->field_name_map[$key]['type']) && $seed_bean->field_name_map[$key]['type'] == 'bool')) {
                $form->assign(strtoupper($key), ' checked ');
            } else {
                $form->assign(strtoupper($key), $searchInputs[$key]);
            }
        }

        if ($this->_create) {
            $form->assign('CREATE', 'true');
        } else {
            $form->assign('CREATE', 'false');
        }

        // fill any doms
        if (isset($this->_popupMeta['selectDoms'])) {
            foreach ($this->_popupMeta['selectDoms'] as $key => $value) {
                $form->assign($key, get_select_options_with_id($app_list_strings[$value['dom']], $value['searchInput']));
            }
        }

        $form->assign('MULTI_SELECT', !empty($_REQUEST['mode']) ? strtoupper($_REQUEST['mode']) : '');

        ob_start();
        insert_popup_header($theme);
        $output_html .= ob_get_contents();
        ob_end_clean();

        $output_html .= get_form_header($mod_strings['LBL_SEARCH_FORM_TITLE'], '', false);

        $form->parse('main.SearchHeader');
        $output_html .= $form->text('main.SearchHeader');

        // Reset the sections that are already in the page so that they do not print again later.
        $form->reset('main.SearchHeader');

        $ListView = new ListView();
        $ListView->show_select_menu = false;
        $ListView->show_delete_button = false;
        $ListView->show_export_button = false;
        $ListView->process_for_popups = true;
        $ListView->setXTemplate($form);

        $ListView->multi_select_popup = $multi_select;
        $ListView->xTemplate->assign('TAG_TYPE', 'A');
        if (isset($this->_popupMeta['listTitle'])) {
            $ListView->setHeaderTitle($this->_popupMeta['listTitle']);
        } else {
            $ListView->setHeaderTitle($mod_strings['LBL_LIST_FORM_TITLE']);
        }
        $ListView->setHeaderText($button);
        $ListView->setQuery($where, '', $this->_popupMeta['orderBy'], $this->_popupMeta['varName']);
        $ListView->setModStrings($mod_strings);

        ob_start();
        $ListView->processListView($seed_bean, 'main', $this->_popupMeta['varName']);
        $output_html .= ob_get_contents();
        ob_end_clean();
        $json = getJSONobj();

        // decode then encode to escape "'s
        $output_html .= "</form>
		<script type=\"text/javascript\">
		function save_checks(offset) {
			checked_ids = Array();
			for (i = 0; i < document.MassUpdate.elements.length; i++){
				if(document.MassUpdate.elements[i].name == 'mass[]' && document.MassUpdate.elements[i].checked) {
					temp_string = '';
					temp_string += '\"' + document.MassUpdate.elements[i].value + '\": {';
					for(the_key in associated_javascript_data[document.MassUpdate.elements[i].value]) {
						temp_string += '\"' + the_key + '\":\"' + associated_javascript_data[document.MassUpdate.elements[i].value][the_key] + '\",';
					}
					temp_string = temp_string.substring(0,temp_string.length - 1);
					temp_string += '}';
					checked_ids.push(temp_string);
				}
			}
			document.MassUpdate.saved_associated_data.value = escape('{' + checked_ids.join(',') + '}');

			document.MassUpdate.action.value = \"Popup\";
			document.MassUpdate.$currentModule" . '_' . strtoupper($this->_popupMeta['moduleMain']) . '_offset.value = offset;
			document.MassUpdate.submit();
		}
		// reassigned the saved data from the saved checks
		if(typeof(document.MassUpdate) != \'undefined\' && document.MassUpdate.saved_associated_data.value != \'\') {
			temp_array = ' . (!empty($_REQUEST['saved_associated_data']) ? $json->encode($json->decode(urldecode($_REQUEST['saved_associated_data']))) : '\'\'') . ';
			for(the_key in temp_array) {
				associated_javascript_data[the_key] = temp_array[the_key];
			}
		}

		// save checks across pages for multiselects
		if(typeof(document.MassUpdate) != "undefined") {
			checked_items = Array();
			inputs_array = document.MassUpdate.elements;

			for(wp = 0 ; wp < inputs_array.length; wp++) {
				if(inputs_array[wp].name == "mass[]" && inputs_array[wp].style.display == "none") {
					checked_items.push(inputs_array[wp].value);
				}
			}
			for(i in checked_items) {
				for(wp = 0 ; wp < inputs_array.length; wp++) {
					if(inputs_array[wp].name == "mass[]" && inputs_array[wp].value == checked_items[i]) {
						inputs_array[wp].checked = true;
					}
				}
			}
		}
		</script>';
        $output_html .= insert_popup_footer();
        return $output_html;
    }
} // end of class Popup_Picker
