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
 
require_once('modules/UpgradeWizard/SugarMerge/EditViewMerge.php');
/**
 * This class extends the EditViewMerge - since the meta data is relatively the same the only thing that needs to be changed is the parameter for viewdefs
 *
 */
#[\AllowDynamicProperties]
class DetailViewMerge extends EditViewMerge
{
    /**
     * Enter the name of the parameter used in the $varName for example in editviewdefs and detailviewdefs it is 'EditView' and 'DetailView' respectively - $viewdefs['EditView']
     *
     * @var STRING
     */
    protected $viewDefs = 'DetailView';
    /**
     * Determines if getFields should analyze panels to determine if it is a MultiPanel
     *
     * @var BOOLEAN
     */
    protected $scanForMultiPanel = true;	/**
     * Parses out the fields for each files meta data and then calls on mergeFields and setPanels
     *
     */
    protected function mergeMetaData()
    {
        $this->originalFields = $this->getFields($this->originalData[$this->module][$this->viewDefs][$this->panelName]);
        $this->originalPanelIds = $this->getPanelIds($this->originalData[$this->module][$this->viewDefs][$this->panelName]);
        $this->customFields = $this->getFields($this->customData[$this->module][$this->viewDefs][$this->panelName]);

        //Special handling to rename certain variables for DetailViews
        $rename_fields = array();
        foreach ($this->customFields as $field_id=>$field) {
            //Check to see if we need to rename the field for special cases
            if (!empty($this->fieldConversionMapping[$this->module][$field_id])) {
                $rename_fields[$field_id] = $this->fieldConversionMapping[$this->module][$field['data']['name']];
                $this->customFields[$field_id]['data']['name'] = $this->fieldConversionMapping[$this->module][$field['data']['name']];
            }
        }

        foreach ($rename_fields as $original_index=>$new_index) {
            $this->customFields[$new_index] = $this->customFields[$original_index];
            unset($this->customFields[$original_index]);
        }
        
        $this->customPanelIds = $this->getPanelIds($this->customData[$this->module][$this->viewDefs][$this->panelName]);
        $this->newFields = $this->getFields($this->newData[$this->module][$this->viewDefs][$this->panelName]);
        //echo var_export($this->newFields, true);
        $this->newPanelIds = $this->getPanelIds($this->newData[$this->module][$this->viewDefs][$this->panelName]);
        $this->mergeFields();
        $this->mergeTemplateMeta();
        $this->setPanels();
    }
}
