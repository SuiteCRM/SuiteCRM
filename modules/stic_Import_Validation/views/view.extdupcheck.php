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
require_once('modules/stic_Import_Validation/ImportDuplicateCheck.php');

require_once('include/upload_file.php');

#[\AllowDynamicProperties]
class stic_Import_ValidationViewExtdupcheck extends stic_Import_ValidationView
{
    protected $pageTitleKey = 'LBL_STEP_DUP_TITLE';

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings, $app_strings, $current_user;
        global $sugar_config;

        $this->ss->assign("MODULE_TITLE", $this->getModuleTitle(false));
        $this->ss->assign("DELETE_INLINE_PNG", SugarThemeRegistry::current()->getImage('delete_inline', 'align="absmiddle" alt="'.$app_strings['LNK_DELETE'].'" border="0"'));
        $this->ss->assign("PUBLISH_INLINE_PNG", SugarThemeRegistry::current()->getImage('publish_inline', 'align="absmiddle" alt="'.$mod_strings['LBL_PUBLISH'].'" border="0"'));
        $this->ss->assign("UNPUBLISH_INLINE_PNG", SugarThemeRegistry::current()->getImage('unpublish_inline', 'align="absmiddle" alt="'.$mod_strings['LBL_UNPUBLISH'].'" border="0"'));
        $this->ss->assign("IMPORT_MODULE", $_REQUEST['import_module']);
        $this->ss->assign("JAVASCRIPT", $this->_getJS());
        $this->ss->assign("CURRENT_STEP", $this->currentStep);

        //BEGIN DRAG DROP WIDGET
        $idc = new ImportDuplicateCheck($this->bean);
        $dupe_indexes = $idc->getDuplicateCheckIndexes();

        $dupe_disabled =  array();

        foreach ($dupe_indexes as $dk=>$dv) {
            $dupe_disabled[] =  array("dupeVal" => $dk, "label" => $dv);
        }


        //set dragdrop value
        $this->ss->assign('enabled_dupes', json_encode(array()));
        $this->ss->assign('disabled_dupes', json_encode($dupe_disabled));
        //END DRAG DROP WIDGET

        $this->ss->assign("RECORDTHRESHOLD", $sugar_config['import_max_records_per_file']);

        $content = $this->ss->fetch('modules/stic_Import_Validation/tpls/extdupcheck.tpl');
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
<script type="text/javascript">

</script>

EOJAVASCRIPT;
    }
}
