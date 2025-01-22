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







#[\AllowDynamicProperties]
class SubPanelViewNotes
{
    public $notes_list = null;
    public $hideNewButton = false;
    public $focus;

    public function setFocus(&$value)
    {
        $this->focus =(object) $value;
    }


    public function setNotesList(&$value)
    {
        $this->notes_list =$value;
    }

    public function setHideNewButton($value)
    {
        $this->hideNewButton = $value;
    }

    public function __construct()
    {
        global $theme;
    }




    public function getHeaderText($action, $currentModule)
    {
        global $app_strings;
        $button  = "<table cellspacing='0' cellpadding='0' border='0'><form border='0' action='index.php' method='post' name='form' id='form'>\n";
        $button .= "<input type='hidden' name='module' value='Notes'>\n";
        if (!$this->hideNewButton) {
            $button .= "<td><input title='".$app_strings['LBL_NEW_BUTTON_TITLE']."' class='button' onclick=\"this.form.action.value='EditView'\" type='submit' name='button' value='  ".$app_strings['LBL_NEW_BUTTON_LABEL']."  '></td>\n";
        }
        $button .= "</tr></form></table>\n";
        return $button;
    }

    public function ProcessSubPanelListView($xTemplatePath, &$mod_strings, $action, $curModule='')
    {
        global $currentModule,$app_strings;
        if (empty($curModule)) {
            $curModule = $currentModule;
        }
        $ListView = new ListView();
        global $current_user;
        $header_text = '';
        if (is_admin($current_user) && $_REQUEST['module'] != 'DynamicLayout' && !empty($_SESSION['editinplace'])) {
            $header_text = "&nbsp;<a href='index.php?action=index&module=DynamicLayout&from_action=SubPanelView&from_module=Notes&record=". $this->focus->id."'>".SugarThemeRegistry::current()->getImage("EditLayout", "border='0' align='bottom'", null, null, '.gif', $mod_strings['LBL_EDITLAYOUT'])."</a>";
        }
        $ListView->initNewXTemplate($xTemplatePath, $mod_strings);
        $ListView->xTemplateAssign("RETURN_URL", "&return_module=".$curModule."&return_action=DetailView&return_id=".$this->focus->id);
        $ListView->xTemplateAssign("DELETE_INLINE_PNG", SugarThemeRegistry::current()->getImage('delete_inline', 'align="absmiddle" border="0"', null, null, '.gif', $app_strings['LNK_DELETE']));
        $ListView->xTemplateAssign("EDIT_INLINE_PNG", SugarThemeRegistry::current()->getImage('edit_inline', 'align="absmiddle"  border="0"', null, null, '.gif', $app_strings['LNK_EDIT']));
        $ListView->xTemplateAssign("RECORD_ID", $this->focus->id);
        $ListView->setHeaderTitle($mod_strings['LBL_MODULE_NAME']. $header_text);
        $ListView->setHeaderText($this->getHeaderText($action, $curModule));
        $ListView->processListView($this->notes_list, "notes", "NOTE");
    }
}
