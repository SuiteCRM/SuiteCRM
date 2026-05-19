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

/**
 * This custom class, allows you to create a button in each record of the subpanel
 * which open the QuickCreate View to edit the record.
 */
class SugarWidgetCustomSubPanelTopButtonQuickCreate extends SugarWidgetField
{
        public function display($defines, $additionalFormFields = null, $asUrl = false)
	{
		global $app_strings;
        global $subpanel_item_count;
		$return_module = $_REQUEST['module'];
		$return_id = $_REQUEST['record']; 
		$module_name = $defines['module'];
		$unique_id = $defines['id'];


		// @see SugarWidgetSubPanelTopButtonQuickCreate::get_subpanel_relationship_name()
		$relationship_name = $this->get_subpanel_relationship_name($defines);
		
		$subpanel = $defines['subpanel_definition']->name;

        $labelText = $app_strings[$defines['title']];

		$html = '
		<form onsubmit="return SUGAR.subpanelUtils.sendAndRetrieve(this.id, \'subpanel_'.$subpanel.'\', \'Loading ...\');" action="index.php" method="post" name="form" id="'.$unique_id.'">
		<input type="hidden" name="target_module" value="'.$module_name.'">
		<input type="hidden" name="tpl" value="QuickCreate.tpl">
		<input type="hidden" name="return_module" value="'.$return_module.'">
		<input type="hidden" name="return_action" value="DetailView">
		<input type="hidden" name="return_id" value="'.$return_id.'">
		<input type="hidden" name="return_relationship" value="'.$relationship_name.'">
		<input type="hidden" name="action" value="SubpanelCreates">
		<input type="hidden" name="module" value="Home">
		<input type="hidden" name="target_action" value="QuickEdit">
		<input type="hidden" name="return_name" value="XXXX">
		<input type="hidden" name="parent_type" value="'.$return_module.'">
		<input type="hidden" name="parent_name" value="XXXX">
		<input type="hidden" name="parent_id" value="'.$return_id.'">
        <input type="hidden" name="stic_events_id" value="'.$return_id.'">
        <input type="hidden" name="stic_events_name" value="'.$relationship_name.'">
        <input type="hidden" name="getNotificationsFromParent_name" value="'.$defines['focus']->name.'">
        <input type="hidden" name="to_pdf" value="true">
        <input type="hidden" name="return_relationship" value="'.$relationship_name.'">
        <input type="hidden" name="record" value="">
        <input type="hidden" name="action" value="SubpanelCreates">
        <input type="hidden" name="module" value="Home">
        <input type="hidden" name="target_action" value="QuickCreate">
        <input type="hidden" name="return_name" value="'.$defines['focus']->name.'">
		<input title="'.$labelText.'" accesskey="N" class="button" type="submit" name="'.$unique_id.'_quickedit_button" id="'.$unique_id.'_quickedit_button" value="'.$labelText.'">';

        if ($module_name === 'Campaigns' && !empty($defines['additional_form_fields']['campaign_type'])) {
            $campaignType = $defines['additional_form_fields']['campaign_type'];
            if ($campaignType === 'Notification' || $campaignType === 'NotifMsg') {
                $html .= '<input type="hidden" name="relate_to" value="getNotificationsFromParent" />' . "\n";
                $html .= '<input type="hidden" name="relate_id" value="' . $return_id . '" />' . "\n";
            }
        }

        foreach ($defines['additional_form_fields'] as $key => $value) {
            if ($key != 'action') {
                $html .= '<input type="hidden" name="' . $key . '" value=\'' . $value . '\' />' . "\n";
            }
        }

		$html .= '</form>';
		

			
		return $html;

	}
	


    /**
     * get_subpanel_relationship_name
     * Get the relationship name based on the subapnel definition
     * @param mixed $defines The subpanel definition
     */
    public function get_subpanel_relationship_name($defines)
    {
        $relationship_name = '';
        if (!empty($defines)) {
            $relationship_name = isset($defines['module']) ? $defines['module'] : '';
            $dataSource = $defines['subpanel_definition']->get_data_source_name(true);
            if (!empty($dataSource)) {
                $relationship_name = $dataSource;
                //Try to set the relationship name to the real relationship, not the link.
                if (!empty($defines['subpanel_definition']->parent_bean->field_defs[$dataSource])
                 && !empty($defines['subpanel_definition']->parent_bean->field_defs[$dataSource]['relationship'])) {
                    $relationship_name = $defines['subpanel_definition']->parent_bean->field_defs[$dataSource]['relationship'];
                }
            }
        }
        return $relationship_name;
    }
}
