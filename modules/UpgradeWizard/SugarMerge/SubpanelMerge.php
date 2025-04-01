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

/*********************************************************************************

 * Description:  Defines the English language pack for the base application.
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

require_once('modules/UpgradeWizard/SugarMerge/ListViewMerge.php');
/**
 * SubpanelMerge is a class for merging subpanel meta data together. This subpanel meta-data is a mix of the layouts seen in listviews and editviews
 *
 */

class SubpanelMerge extends ListViewMerge
{
    /**
     * @var mixed[]|array<string, mixed[]>
     */
    public $mergeData;
    protected $varName = 'subpanel_layout';
    protected $viewDefs = 'SubPanel';
    /**
     * Loads the meta data of the original, new, and custom file into the variables originalData, newData, and customData respectively it then transforms them into a structure that EditView Merge would understand
     *
     * @param STRING $module - name of the module's files that are to be merged
     * @param STRING $original_file - path to the file that originally shipped with sugar
     * @param STRING $new_file - path to the new file that is shipping with the patch
     * @param STRING $custom_file - path to the custom file
     */
    protected function loadData($module, $original_file, $new_file, $custom_file)
    {
        parent::loadData($module, $original_file, $new_file, $custom_file);
        $this->originalData = array($module=>array( $this->viewDefs=>array($this->panelName=>array('DEFAULT'=>$this->originalData[$module]['list_fields']))));
        $this->customData = array($module=>array( $this->viewDefs=>array($this->panelName=>array('DEFAULT'=>$this->customData[$module]['list_fields']))));
        $this->mergeData = $this->newData;
        $this->newData = array($module=>array( $this->viewDefs=>array($this->panelName=>array('DEFAULT'=>$this->newData[$module]['list_fields']))));
    }
    
    /**
     * We take mergeData which is a copy of the new meta data prior to merging and set it's list_fields variable to the merged panels
     *
     */
    protected function setPanels()
    {
        $this->mergeData['list_fields'] = $this->buildPanels();
    }
    
    /**
     * This will save the merged data to a file
     *
     * @param STRING $to - path of the file to save it to
     * @return BOOLEAN - success or failure of the save
     */
    public function save($to)
    {
        return write_array_to_file((string)$this->varName, $this->newData, $to);
    }
}
