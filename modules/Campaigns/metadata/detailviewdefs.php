<?php
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

$viewdefs ['Campaigns'] =
array(
  'DetailView' =>
  array(
    'templateMeta' =>
    array(
      'form' =>
      array(
        'hidden' =>
        array(
          0 => '<input type="hidden" name="mode" value="">',
        ),
        'buttons' =>
        array(
              0 =>  array(
                  'customCode' => '<input type="button" class="button" onclick="window.location=\'index.php?module=Campaigns&action=WizardHome&record={$fields.id.value}\';" name="button" id="launch_wizard_button" value="{$MOD.LBL_TO_WIZARD_TITLE}" />',
              ),
              1 => array(
                 'customCode' => '{if $bean->aclAccess("edit")}<input title="{$APP.LBL_DUPLICATE_BUTTON_TITLE}" accessKey="{$APP.LBL_DUPLICATE_BUTTON_KEY}" class="button" onclick="var _form = (this.form) ? this.form : document.forms[0]; _form.return_module.value=\'Campaigns\'; _form.return_action.value=\'DetailView\'; _form.isDuplicate.value=true; _form.action.value=\'EditView\'; _form.return_id.value=\'{$id}\';SUGAR.ajaxUI.submitForm(_form);" type="button" name="Duplicate" value="{$APP.LBL_DUPLICATE_BUTTON_LABEL}" id="duplicate_button">{/if}'
              ),
            2 => array(
                'customCode' => '{if $bean->aclAccess("delete")}<input title="{$APP.LBL_DELETE_BUTTON_TITLE}" accessKey="{$APP.LBL_DELETE_BUTTON_KEY}" class="button" onclick="var _form = (this.form) ? this.form : document.forms[0]; _form.return_module.value=\'Campaigns\'; _form.return_action.value=\'ListView\'; _form.action.value=\'Delete\'; if(confirm(\'{$APP.NTC_DELETE_CONFIRMATION}\')) SUGAR.ajaxUI.submitForm(_form);" type="submit" name="Delete" value="{$APP.LBL_DELETE_BUTTON_LABEL}" id="delete_button">{/if} '
            ),
          3 =>
          array(
            'customCode' => '<input title="{$MOD.LBL_TEST_BUTTON_TITLE}"  class="button" onclick="this.form.return_module.value=\'Campaigns\'; this.form.return_action.value=\'TrackDetailView\';this.form.action.value=\'Schedule\';this.form.mode.value=\'test\';SUGAR.ajaxUI.submitForm(this.form);" type="{$ADD_BUTTON_STATE}" name="button" id="send_test_button" value="{$MOD.LBL_TEST_BUTTON_LABEL}">',
            'sugar_html' =>
            array(
              'type' => 'input',
              'value' => '{$MOD.LBL_TEST_BUTTON_LABEL}',
              'htmlOptions' =>
              array(
                'type' => '{$ADD_BUTTON_STATE}',
                'title' => '{$MOD.LBL_TEST_BUTTON_TITLE}',
                'class' => 'button',
                'onclick' => 'this.form = document.getElementById(\'formDetailView\'); this.form.return_module.value=\'Campaigns\'; this.form.return_action.value=\'TrackDetailView\';this.form.action.value=\'Schedule\';this.form.mode.value=\'test\';SUGAR.ajaxUI.submitForm(this.form);',
                'name' => 'button',
                'id' => 'send_test_button',
              ),
            ),
          ),
          4 =>
          array(
            'customCode' => '<input title="{$MOD.LBL_QUEUE_BUTTON_TITLE}" class="button" onclick="this.form.return_module.value=\'Campaigns\'; this.form.return_action.value=\'TrackDetailView\';this.form.action.value=\'Schedule\';SUGAR.ajaxUI.submitForm(this.form);" type="{$ADD_BUTTON_STATE}" name="button" id="send_emails_button" value="{$MOD.LBL_QUEUE_BUTTON_LABEL}">',
            'sugar_html' =>
            array(
              'type' => 'input',
              'value' => '{$MOD.LBL_QUEUE_BUTTON_LABEL}',
              'htmlOptions' =>
              array(
                'type' => '{$ADD_BUTTON_STATE}',
                'title' => '{$MOD.LBL_QUEUE_BUTTON_TITLE}',
                'class' => 'button',
                'onclick' => 'this.form.return_module.value=\'Campaigns\'; this.form.return_action.value=\'TrackDetailView\';this.form.action.value=\'Schedule\';SUGAR.ajaxUI.submitForm(this.form);',
                'name' => 'button',
                'id' => 'send_emails_button',
              ),
            ),
          ),
            /*
          5 =>
          array (
            'customCode' => '<input title="{$APP.LBL_MAILMERGE}" class="button" onclick="this.form.return_module.value=\'Campaigns\'; this.form.return_action.value=\'TrackDetailView\';this.form.action.value=\'MailMerge\';SUGAR.ajaxUI.submitForm(this.form);" type="submit" name="button" id="mail_merge_button" value="{$APP.LBL_MAILMERGE}">',
            'sugar_html' =>
            array (
              'type' => 'submit',
              'value' => '{$APP.LBL_MAILMERGE}',
              'htmlOptions' =>
              array (
                'title' => '{$APP.LBL_MAILMERGE}',
                'class' => 'button',
                'onclick' => 'this.form.return_module.value=\'Campaigns\'; this.form.return_action.value=\'TrackDetailView\';this.form.action.value=\'MailMerge\';SUGAR.ajaxUI.submitForm(this.form);',
                'name' => 'button',
                'id' => 'mail_merge_button',
              ),
            ),
          ),
            */
          5 =>
          array(
            'customCode' => '<input title="{$MOD.LBL_MARK_AS_SENT}" class="button" onclick="this.form.return_module.value=\'Campaigns\'; this.form.return_action.value=\'TrackDetailView\';this.form.action.value=\'DetailView\';this.form.mode.value=\'set_target\';SUGAR.ajaxUI.submitForm(this.form);" type="{$TARGET_BUTTON_STATE}" name="button" id="mark_as_sent_button" value="{$MOD.LBL_MARK_AS_SENT}">',
            'sugar_html' =>
            array(
              'type' => 'input',
              'value' => '{$MOD.LBL_MARK_AS_SENT}',
              'htmlOptions' =>
              array(
                'type' => '{$TARGET_BUTTON_STATE}',
                'title' => '{$MOD.LBL_MARK_AS_SENT}',
                'class' => 'button',
                'onclick' => 'this.form.return_module.value=\'Campaigns\'; this.form.return_action.value=\'TrackDetailView\';this.form.action.value=\'DetailView\';this.form.mode.value=\'set_target\';SUGAR.ajaxUI.submitForm(this.form);',
                'name' => 'button',
                'id' => 'mark_as_sent_button',
              ),
            ),
          ),
          6 =>
          array(
            'customCode' => '<script>{$MSG_SCRIPT}</script>',
          ),
        ),
        'links' =>
        array(
          0 => '<input type="button" class="button" onclick="javascript:window.location=\'index.php?module=Campaigns&action=TrackDetailView&record={$fields.id.value}\';" name="button" id="view_status_button" value="{$MOD.LBL_TRACK_BUTTON_LABEL}" />',
          1 => '<input id="viewRoiButtonId" type="button" class="button" onclick="javascript:window.location=\'index.php?module=Campaigns&action=RoiDetailView&record={$fields.id.value}\';" name="button" id="view_roi_button" value="{$MOD.LBL_TRACK_ROI_BUTTON_LABEL}" />',
        ),
      ),
      'maxColumns' => '2',
      'widths' =>
      array(
        0 =>
        array(
          'label' => '10',
          'field' => '30',
        ),
        1 =>
        array(
          'label' => '10',
          'field' => '30',
        ),
      ),
      'useTabs' => true,
      'tabDefs' =>
      array(
        'LBL_CAMPAIGN_INFORMATION' =>
        array(
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
        'LBL_NAVIGATION_MENU_GEN2' =>
        array(
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
        'LBL_PANEL_ASSIGNMENT' =>
        array(
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
      ),
    ),
    'panels' =>
    array(
      'lbl_campaign_information' =>
      array(
        0 =>
        array(
          0 => 'name',
        ),
        1 =>
        array(
          0 =>
              array(
                  'name' => 'status',
                  'label' => 'LBL_CAMPAIGN_STATUS',
              ),
          1 => 'campaign_type',
        ),
        2 =>
        array(
          0 =>
              array(
                  'name' => 'assigned_user_name',
                  'label' => 'LBL_ASSIGNED_TO',
              ),
          1 =>
          array(
            'name' => 'frequency',
            'customCode' => '{if $fields.campaign_type.value == "NewsLetter"}<div style=\'none\' id=\'freq_field\'>{$APP_LIST.newsletter_frequency_dom[$fields.frequency.value]}</div>{/if}&nbsp;',
            'customLabel' => '{if $fields.campaign_type.value == "NewsLetter"}<div style=\'none\' id=\'freq_label\'>{$MOD.LBL_CAMPAIGN_FREQUENCY}</div>{/if}&nbsp;',
          ),
        ),
        3 =>
        array(
          0 =>
          array(
            'name' => 'content',
            'label' => 'LBL_CAMPAIGN_CONTENT',
          ),
        ),
      ),
      'LBL_NAVIGATION_MENU_GEN2' =>
      array(
        0 =>
        array(
          0 =>
          array(
            'name' => 'currency_id',
            'comment' => 'Currency in use for the campaign',
            'label' => 'LBL_CURRENCY',
          ),
          1 =>
          array(
            'name' => 'impressions',
            'label' => 'LBL_CAMPAIGN_IMPRESSIONS',
          ),
        ),
        1 =>
        array(
          0 =>
          array(
            'name' => 'budget',
            'label' => '{$MOD.LBL_CAMPAIGN_BUDGET} ({$CURRENCY})',
          ),
          1 =>
          array(
            'name' => 'expected_cost',
            'label' => '{$MOD.LBL_CAMPAIGN_EXPECTED_COST} ({$CURRENCY})',
          ),
        ),
        2 =>
        array(
          0 =>
          array(
            'name' => 'actual_cost',
            'label' => '{$MOD.LBL_CAMPAIGN_ACTUAL_COST} ({$CURRENCY})',
          ),
          1 =>
          array(
            'name' => 'expected_revenue',
            'label' => '{$MOD.LBL_CAMPAIGN_EXPECTED_REVENUE} ({$CURRENCY})',
          ),
        ),
        3 =>
        array(
          0 =>
          array(
            'name' => 'objective',
            'label' => 'LBL_CAMPAIGN_OBJECTIVE',
          ),
        ),
      ),
      'LBL_PANEL_ASSIGNMENT' =>
      array(
        0 =>
        array(
          0 =>
          array(
            'name' => 'date_entered',
            'customCode' => '{$fields.date_entered.value} {$APP.LBL_BY} {$fields.created_by_name.value}',
          ),
          1 =>
          array(
            'name' => 'date_modified',
            'label' => 'LBL_DATE_MODIFIED',
            'customCode' => '{$fields.date_modified.value} {$APP.LBL_BY} {$fields.modified_by_name.value}',
          ),
        ),
      ),
    ),
  ),
);
