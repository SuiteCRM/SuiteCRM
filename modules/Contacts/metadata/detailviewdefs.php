<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */

// STIC-Custom - MHP - 20240201 - Override the core metadata files with the custom metadata files 
// https://github.com/SinergiaTIC/SinergiaCRM/pull/105
// $viewdefs ['Contacts'] =
// array(
//   'DetailView' =>
//   array(
//     'templateMeta' =>
//     array(
//       'form' =>
//       array(
//         'buttons' =>
//         array(
//             'SEND_CONFIRM_OPT_IN_EMAIL' => EmailAddress::getSendConfirmOptInEmailActionLinkDefs('Contacts'),
//           0 => 'EDIT',
//           1 => 'DUPLICATE',
//           2 => 'DELETE',
//           3 => 'FIND_DUPLICATES',
//           4 =>
//           array(
//             'customCode' => '<input type="submit" class="button" title="{$APP.LBL_MANAGE_SUBSCRIPTIONS}" onclick="this.form.return_module.value=\'Contacts\'; this.form.return_action.value=\'DetailView\'; this.form.return_id.value=\'{$fields.id.value}\'; this.form.action.value=\'Subscriptions\'; this.form.module.value=\'Campaigns\'; this.form.module_tab.value=\'Contacts\';" name="Manage Subscriptions" value="{$APP.LBL_MANAGE_SUBSCRIPTIONS}"/>',
//             'sugar_html' =>
//             array(
//               'type' => 'submit',
//               'value' => '{$APP.LBL_MANAGE_SUBSCRIPTIONS}',
//               'htmlOptions' =>
//               array(
//                 'class' => 'button',
//                 'id' => 'manage_subscriptions_button',
//                 'title' => '{$APP.LBL_MANAGE_SUBSCRIPTIONS}',
//                 'onclick' => 'this.form.return_module.value=\'Contacts\'; this.form.return_action.value=\'DetailView\'; this.form.return_id.value=\'{$fields.id.value}\'; this.form.action.value=\'Subscriptions\'; this.form.module.value=\'Campaigns\'; this.form.module_tab.value=\'Contacts\';',
//                 'name' => 'Manage Subscriptions',
//               ),
//             ),
//           ),
//           'AOS_GENLET' =>
//           array(
//             'customCode' => '<input type="button" class="button" onClick="showPopup();" value="{$APP.LBL_PRINT_AS_PDF}">',
//           ),
//           'AOP_CREATE' =>
//           array(
//             'customCode' => '{if !$fields.joomla_account_id.value && $AOP_PORTAL_ENABLED}<input type="submit" class="button" onClick="this.form.action.value=\'createPortalUser\';" value="{$MOD.LBL_CREATE_PORTAL_USER}"> {/if}',
//             'sugar_html' =>
//             array(
//               'type' => 'submit',
//               'value' => '{$MOD.LBL_CREATE_PORTAL_USER}',
//               'htmlOptions' =>
//               array(
//                 'title' => '{$MOD.LBL_CREATE_PORTAL_USER}',
//                 'class' => 'button',
//                 'onclick' => 'this.form.action.value=\'createPortalUser\';',
//                 'name' => 'buttonCreatePortalUser',
//                 'id' => 'createPortalUser_button',
//               ),
//               'template' => '{if !$fields.joomla_account_id.value && $AOP_PORTAL_ENABLED}[CONTENT]{/if}',
//             ),
//           ),
//           'AOP_DISABLE' =>
//           array(
//             'customCode' => '{if $fields.joomla_account_id.value && !$fields.portal_account_disabled.value && $AOP_PORTAL_ENABLED}<input type="submit" class="button" onClick="this.form.action.value=\'disablePortalUser\';" value="{$MOD.LBL_DISABLE_PORTAL_USER}"> {/if}',
//             'sugar_html' =>
//             array(
//               'type' => 'submit',
//               'value' => '{$MOD.LBL_DISABLE_PORTAL_USER}',
//               'htmlOptions' =>
//               array(
//                 'title' => '{$MOD.LBL_DISABLE_PORTAL_USER}',
//                 'class' => 'button',
//                 'onclick' => 'this.form.action.value=\'disablePortalUser\';',
//                 'name' => 'buttonDisablePortalUser',
//                 'id' => 'disablePortalUser_button',
//               ),
//               'template' => '{if $fields.joomla_account_id.value && !$fields.portal_account_disabled.value && $AOP_PORTAL_ENABLED}[CONTENT]{/if}',
//             ),
//           ),
//           'AOP_ENABLE' =>
//           array(
//             'customCode' => '{if $fields.joomla_account_id.value && $fields.portal_account_disabled.value && $AOP_PORTAL_ENABLED}<input type="submit" class="button" onClick="this.form.action.value=\'enablePortalUser\';" value="{$MOD.LBL_ENABLE_PORTAL_USER}"> {/if}',
//             'sugar_html' =>
//             array(
//               'type' => 'submit',
//               'value' => '{$MOD.LBL_ENABLE_PORTAL_USER}',
//               'htmlOptions' =>
//               array(
//                 'title' => '{$MOD.LBL_ENABLE_PORTAL_USER}',
//                 'class' => 'button',
//                 'onclick' => 'this.form.action.value=\'enablePortalUser\';',
//                 'name' => 'buttonENablePortalUser',
//                 'id' => 'enablePortalUser_button',
//               ),
//               'template' => '{if $fields.joomla_account_id.value && $fields.portal_account_disabled.value && $AOP_PORTAL_ENABLED}[CONTENT]{/if}',
//             ),
//           ),
//         ),
//       ),
//       'maxColumns' => '2',
//       'widths' =>
//       array(
//         0 =>
//         array(
//           'label' => '10',
//           'field' => '30',
//         ),
//         1 =>
//         array(
//           'label' => '10',
//           'field' => '30',
//         ),
//       ),
//       'includes' =>
//       array(
//         0 =>
//         array(
//           'file' => 'modules/Contacts/Contact.js',
//         ),
//       ),
//       'useTabs' => true,
//       'tabDefs' =>
//       array(
//         'LBL_CONTACT_INFORMATION' =>
//         array(
//           'newTab' => true,
//           'panelDefault' => 'expanded',
//         ),
//         'LBL_PANEL_ADVANCED' =>
//         array(
//           'newTab' => true,
//           'panelDefault' => 'expanded',
//         ),
//         'LBL_PANEL_ASSIGNMENT' =>
//         array(
//           'newTab' => true,
//           'panelDefault' => 'expanded',
//         ),
//       ),
//     ),
//     'panels' =>
//     array(
//       'lbl_contact_information' =>
//       array(
//         0 =>
//         array(
//           0 =>
//           array(
//             'name' => 'first_name',
//             'comment' => 'First name of the contact',
//             'label' => 'LBL_FIRST_NAME',
//           ),
//           1 =>
//           array(
//             'name' => 'last_name',
//             'comment' => 'Last name of the contact',
//             'label' => 'LBL_LAST_NAME',
//           ),
//         ),
//         1 =>
//         array(
//           0 =>
//           array(
//             'name' => 'phone_work',
//             'label' => 'LBL_OFFICE_PHONE',
//           ),
//           1 =>
//           array(
//             'name' => 'phone_mobile',
//             'label' => 'LBL_MOBILE_PHONE',
//           ),
//         ),
//         2 =>
//         array(
//           0 =>
//           array(
//             'name' => 'title',
//             'comment' => 'The title of the contact',
//             'label' => 'LBL_TITLE',
//           ),
//           1 =>
//           array(
//             'name' => 'department',
//             'label' => 'LBL_DEPARTMENT',
//           ),
//         ),
//         3 =>
//         array(
//           0 =>
//           array(
//             'name' => 'account_name',
//             'label' => 'LBL_ACCOUNT_NAME',
//           ),
//           1 =>
//           array(
//             'name' => 'phone_fax',
//             'label' => 'LBL_FAX_PHONE',
//           ),
//         ),
//         4 =>
//         array(
//           0 =>
//           array(
//             'name' => 'email1',
//             'studio' => 'false',
//             'label' => 'LBL_EMAIL_ADDRESS',
//           ),
//         ),
//         5 =>
//         array(
//           0 =>
//           array(
//             'name' => 'primary_address_street',
//             'label' => 'LBL_PRIMARY_ADDRESS',
//             'type' => 'address',
//             'displayParams' =>
//             array(
//               'key' => 'primary',
//             ),
//           ),
//           1 =>
//           array(
//             'name' => 'alt_address_street',
//             'label' => 'LBL_ALTERNATE_ADDRESS',
//             'type' => 'address',
//             'displayParams' =>
//             array(
//               'key' => 'alt',
//             ),
//           ),
//         ),
//         6 =>
//         array(
//           0 =>
//           array(
//             'name' => 'description',
//             'comment' => 'Full text of the note',
//             'label' => 'LBL_DESCRIPTION',
//           ),
//           1 => '',
//         ),
//         7 =>
//         array(
//           0 =>
//           array(
//             'name' => 'assigned_user_name',
//             'label' => 'LBL_ASSIGNED_TO_NAME',
//           ),
//         ),
//       ),
//       'LBL_PANEL_ADVANCED' =>
//       array(
//         0 =>
//         array(
//           0 =>
//           array(
//             'name' => 'lead_source',
//             'comment' => 'How did the contact come about',
//             'label' => 'LBL_LEAD_SOURCE',
//           ),
//         ),
//         1 =>
//         array(
//           0 =>
//           array(
//             'name' => 'report_to_name',
//             'label' => 'LBL_REPORTS_TO',
//           ),
//           1 =>
//           array(
//             'name' => 'campaign_name',
//             'label' => 'LBL_CAMPAIGN',
//           ),
//         ),
//       ),
//       'LBL_PANEL_ASSIGNMENT' =>
//       array(
//         0 =>
//         array(
//           0 =>
//           array(
//             'name' => 'date_entered',
//             'customCode' => '{$fields.date_entered.value} {$APP.LBL_BY} {$fields.created_by_name.value}',
//             'label' => 'LBL_DATE_ENTERED',
//           ),
//           1 =>
//           array(
//             'name' => 'date_modified',
//             'customCode' => '{$fields.date_modified.value} {$APP.LBL_BY} {$fields.modified_by_name.value}',
//             'label' => 'LBL_DATE_MODIFIED',
//           ),
//         ),
//       ),
//     ),
//   ),
// );

$viewdefs ['Contacts'] = 
array (
  'DetailView' => 
  array (
    'templateMeta' => 
    array (
      'form' => 
      array (
        'buttons' => 
        array (
          'SEND_CONFIRM_OPT_IN_EMAIL' => EmailAddress::getSendConfirmOptInEmailActionLinkDefs('Contacts'),
          0 => 'EDIT',
          1 => 'DUPLICATE',
          2 => 'DELETE',
          3 => 'FIND_DUPLICATES',
          4 => 
          array (
            'customCode' => '<input type="submit" class="button" title="{$APP.LBL_MANAGE_SUBSCRIPTIONS}" onclick="this.form.return_module.value=\'Contacts\'; this.form.return_action.value=\'DetailView\'; this.form.return_id.value=\'{$fields.id.value}\'; this.form.action.value=\'Subscriptions\'; this.form.module.value=\'Campaigns\'; this.form.module_tab.value=\'Contacts\';" name="Manage Subscriptions" value="{$APP.LBL_MANAGE_SUBSCRIPTIONS}"/>',
            'sugar_html' => 
            array (
              'type' => 'submit',
              'value' => '{$APP.LBL_MANAGE_SUBSCRIPTIONS}',
              'htmlOptions' => 
              array (
                'class' => 'button',
                'id' => 'manage_subscriptions_button',
                'title' => '{$APP.LBL_MANAGE_SUBSCRIPTIONS}',
                'onclick' => 'this.form.return_module.value=\'Contacts\'; this.form.return_action.value=\'DetailView\'; this.form.return_id.value=\'{$fields.id.value}\'; this.form.action.value=\'Subscriptions\'; this.form.module.value=\'Campaigns\'; this.form.module_tab.value=\'Contacts\';',
                'name' => 'Manage Subscriptions',
              ),
            ),
          ),
          'AOS_GENLET' => 
          array (
            'customCode' => '<input type="button" class="button" onClick="showPopup();" value="{$APP.LBL_PRINT_AS_PDF}">',
          ),
          'AOP_CREATE' => 
          array (
            'customCode' => '{if !$fields.joomla_account_id.value && $AOP_PORTAL_ENABLED}<input type="submit" class="button" onClick="this.form.action.value=\'createPortalUser\';" value="{$MOD.LBL_CREATE_PORTAL_USER}"> {/if}',
            'sugar_html' => 
            array (
              'type' => 'submit',
              'value' => '{$MOD.LBL_CREATE_PORTAL_USER}',
              'htmlOptions' => 
              array (
                'title' => '{$MOD.LBL_CREATE_PORTAL_USER}',
                'class' => 'button',
                'onclick' => 'this.form.action.value=\'createPortalUser\';',
                'name' => 'buttonCreatePortalUser',
                'id' => 'createPortalUser_button',
              ),
              'template' => '{if !$fields.joomla_account_id.value && $AOP_PORTAL_ENABLED}[CONTENT]{/if}',
            ),
          ),
          'AOP_DISABLE' => 
          array (
            'customCode' => '{if $fields.joomla_account_id.value && !$fields.portal_account_disabled.value && $AOP_PORTAL_ENABLED}<input type="submit" class="button" onClick="this.form.action.value=\'disablePortalUser\';" value="{$MOD.LBL_DISABLE_PORTAL_USER}"> {/if}',
            'sugar_html' => 
            array (
              'type' => 'submit',
              'value' => '{$MOD.LBL_DISABLE_PORTAL_USER}',
              'htmlOptions' => 
              array (
                'title' => '{$MOD.LBL_DISABLE_PORTAL_USER}',
                'class' => 'button',
                'onclick' => 'this.form.action.value=\'disablePortalUser\';',
                'name' => 'buttonDisablePortalUser',
                'id' => 'disablePortalUser_button',
              ),
              'template' => '{if $fields.joomla_account_id.value && !$fields.portal_account_disabled.value && $AOP_PORTAL_ENABLED}[CONTENT]{/if}',
            ),
          ),
          'AOP_ENABLE' => 
          array (
            'customCode' => '{if $fields.joomla_account_id.value && $fields.portal_account_disabled.value && $AOP_PORTAL_ENABLED}<input type="submit" class="button" onClick="this.form.action.value=\'enablePortalUser\';" value="{$MOD.LBL_ENABLE_PORTAL_USER}"> {/if}',
            'sugar_html' => 
            array (
              'type' => 'submit',
              'value' => '{$MOD.LBL_ENABLE_PORTAL_USER}',
              'htmlOptions' => 
              array (
                'title' => '{$MOD.LBL_ENABLE_PORTAL_USER}',
                'class' => 'button',
                'onclick' => 'this.form.action.value=\'enablePortalUser\';',
                'name' => 'buttonENablePortalUser',
                'id' => 'enablePortalUser_button',
              ),
              'template' => '{if $fields.joomla_account_id.value && $fields.portal_account_disabled.value && $AOP_PORTAL_ENABLED}[CONTENT]{/if}',
            ),
          ),
        ),
      ),
      'maxColumns' => '2',
      'widths' => 
      array (
        0 => 
        array (
          'label' => '10',
          'field' => '30',
        ),
        1 => 
        array (
          'label' => '10',
          'field' => '30',
        ),
      ),
      'includes' => 
      array (
        0 => 
        array (
          'file' => 'modules/Contacts/Contact.js',
        ),
      ),
      'useTabs' => true,
      'tabDefs' => 
      array (
        'LBL_CONTACT_INFORMATION' => 
        array (
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
        'LBL_STIC_PANEL_CONTACT_DATA' => 
        array (
          'newTab' => false,
          'panelDefault' => 'expanded',
        ),
        'LBL_STIC_PANEL_ADDRESSES' => 
        array (
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
        'LBL_STIC_PANEL_SOCIOECONOMIC_DATA' => 
        array (
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
        'LBL_STIC_PANEL_GDPR' => 
        array (
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
        'LBL_STIC_PANEL_SEPE' => 
        array (
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
        'LBL_STIC_PANEL_INCORPORA' => 
        array (
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
        'LBL_STIC_PANEL_INCORPORA_ADDRESS' => 
        array (
          'newTab' => false,
          'panelDefault' => 'expanded',
        ),
        'LBL_STIC_PANEL_INCORPORA_SYNC' => 
        array (
          'newTab' => false,
          'panelDefault' => 'expanded',
        ),
        'LBL_STIC_PANEL_RECORD_DETAILS' => 
        array (
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
      ),
      'syncDetailEditViews' => true,
    ),
    'panels' => 
    array (
      'lbl_contact_information' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'name' => 'photo',
            'studio' => 
            array (
              'listview' => true,
            ),
            'label' => 'LBL_PHOTO',
          ),
          1 => '',
        ),
        1 => 
        array (
          0 => 
          array (
            'name' => 'first_name',
            'label' => 'LBL_FIRST_NAME',
          ),
          1 => 
          array (
            'name' => 'last_name',
            'label' => 'LBL_LAST_NAME',
          ),
        ),
        2 => 
        array (
          0 => 
          array (
            'name' => 'assigned_user_name',
            'label' => 'LBL_ASSIGNED_TO_NAME',
          ),
          1 => 
          array (
            'name' => 'stic_relationship_type_c',
            'label' => 'LBL_STIC_RELATIONSHIP_TYPE',
          ),
        ),
        3 => 
        array (
          0 => 
          array (
            'name' => 'stic_identification_type_c',
            'label' => 'LBL_STIC_IDENTIFICATION_TYPE',
          ),
          1 => 
          array (
            'name' => 'stic_identification_number_c',
            'label' => 'LBL_STIC_IDENTIFICATION_NUMBER',
          ),
        ),
        4 => 
        array (
          0 => 
          array (
            'name' => 'stic_language_c',
            'label' => 'LBL_STIC_LANGUAGE',
          ),
          1 => 
          array (
            'name' => 'stic_gender_c',
            'label' => 'LBL_STIC_GENDER',
          ),
        ),
        5 => 
        array (
          0 => 
          array (
            'name' => 'birthdate',
            'label' => 'LBL_BIRTHDATE',
          ),
          1 => 
          array (
            'name' => 'stic_age_c',
            'label' => 'LBL_STIC_AGE',
          ),
        ),
        6 => 
        array (
          0 => 'campaign_name',
          1 => 
          array (
            'name' => 'stic_acquisition_channel_c',
            'label' => 'LBL_STIC_ACQUISITION_CHANNEL',
          ),
        ),
        7 => 
        array (
          0 => 
          array (
            'name' => 'stic_centers_contacts_name',
            'label' => 'LBL_STIC_CENTERS_CONTACTS_FROM_STIC_CENTERS_TITLE',
          ),
          1 => '',
        ),
        8 => 
        array (
          0 => 
          array (
            'name' => 'description',
            'label' => 'LBL_DESCRIPTION',
          ),
        ),
      ),
      'LBL_STIC_PANEL_CONTACT_DATA' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'name' => 'email1',
            'studio' => 'false',
            'label' => 'LBL_EMAIL_ADDRESS',
          ),
        ),
        1 => 
        array (
          0 => 
          array (
            'name' => 'phone_mobile',
            'label' => 'LBL_MOBILE_PHONE',
          ),
          1 => 
          array (
            'name' => 'phone_home',
            'label' => 'LBL_HOME_PHONE',
          ),
        ),
        2 => 
        array (
          0 => 
          array (
            'name' => 'phone_work',
            'label' => 'LBL_OFFICE_PHONE',
          ),
          1 => 
          array (
            'name' => 'phone_other',
            'label' => 'LBL_OTHER_PHONE',
          ),
        ),
        3 => 
        array (
          0 => 
          array (
            'name' => 'stic_preferred_contact_channel_c',
            'label' => 'LBL_STIC_PREFERRED_CONTACT_CHANNEL',
          ),
          1 => 
          array (
            'name' => 'stic_postal_mail_return_reason_c',
            'label' => 'LBL_STIC_POSTAL_MAIL_RETURN_REASON',
          ),
        ),
        4 => 
        array (
          0 => 
          array (
            'name' => 'do_not_call',
          ),
          1 => 
          array (
            'name' => 'stic_do_not_send_postal_mail_c',
            'label' => 'LBL_STIC_DO_NOT_SEND_POSTAL_MAIL',
          ),
        ),
      ),
      'LBL_STIC_PANEL_ADDRESSES' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'name' => 'stic_primary_address_type_c',
            'label' => 'LBL_STIC_PRIMARY_ADDRESS_TYPE',
          ),
          1 => 
          array (
            'name' => 'stic_alt_address_type_c',
            'label' => 'LBL_STIC_ALT_ADDRESS_TYPE',
          ),
        ),
        1 => 
        array (
          0 => 
          array (
            'name' => 'primary_address_street',
            // STIC - ART - Incorrect tag for the address in DetailView
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/180
            // 'label' => 'LBL_PRIMARY_ADDRESS',
            'label' => 'LBL_PRIMARY_ADDRESS_STREET',
            // END STIC
          ),
          1 => 
          array (
            'name' => 'alt_address_street',
            // STIC - ART - Incorrect tag for the address in DetailView
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/180
            // 'label' => 'LBL_ALTERNATE_ADDRESS',
            'label' => 'LBL_ALT_ADDRESS_STREET',
            // END STIC
          ),
        ),
        2 => 
        array (
          0 => 
          array (
            'name' => 'primary_address_postalcode',
            'label' => 'LBL_PRIMARY_ADDRESS_POSTALCODE',
          ),
          1 => 
          array (
            'name' => 'alt_address_postalcode',
            'label' => 'LBL_ALT_ADDRESS_POSTALCODE',
          ),
        ),
        3 => 
        array (
          0 => 
          array (
            'name' => 'primary_address_city',
            'label' => 'LBL_PRIMARY_ADDRESS_CITY',
          ),
          1 => 
          array (
            'name' => 'alt_address_city',
            'label' => 'LBL_ALT_ADDRESS_CITY',
          ),
        ),
        4 => 
        array (
          0 => 
          array (
            'name' => 'stic_primary_address_county_c',
            'label' => 'LBL_STIC_PRIMARY_ADDRESS_COUNTY',
          ),
          1 => 
          array (
            'name' => 'stic_alt_address_county_c',
            'label' => 'LBL_STIC_ALT_ADDRESS_COUNTY',
          ),
        ),
        5 => 
        array (
          0 => 
          array (
            'name' => 'primary_address_state',
            'label' => 'LBL_PRIMARY_ADDRESS_STATE',
          ),
          1 => 
          array (
            'name' => 'alt_address_state',
            'label' => 'LBL_ALT_ADDRESS_STATE',
          ),
        ),
        6 => 
        array (
          0 => 
          array (
            'name' => 'stic_primary_address_region_c',
            'label' => 'LBL_STIC_PRIMARY_ADDRESS_REGION',
          ),
          1 => 
          array (
            'name' => 'stic_alt_address_region_c',
            'label' => 'LBL_STIC_ALT_ADDRESS_REGION',
          ),
        ),
        7 => 
        array (
          0 => 
          array (
            'name' => 'primary_address_country',
            'label' => 'LBL_PRIMARY_ADDRESS_COUNTRY',
          ),
          1 => 
          array (
            'name' => 'alt_address_country',
            'label' => 'LBL_ALT_ADDRESS_COUNTRY',
          ),
        ),
      ),
      'LBL_STIC_PANEL_SOCIOECONOMIC_DATA' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'name' => 'stic_professional_profile_c',
            'studio' => 'visible',
            'label' => 'LBL_STIC_PROFESSIONAL_PROFILE',
          ),
          1 => 
          array (
            'name' => 'stic_referral_agent_c',
            'label' => 'LBL_STIC_REFERRAL_AGENT',
          ),
        ),
        1 => 
        array (
          0 => 
          array (
            'name' => 'stic_professional_sector_c',
            'label' => 'LBL_STIC_PROFESSIONAL_SECTOR',
          ),
          1 => 
          array (
            'name' => 'stic_professional_sector_other_c',
            'label' => 'LBL_STIC_PROFESSIONAL_SECTOR_OTHER',
          ),
        ),
        2 => 
        array (
          0 => 
          array (
            'name' => 'stic_employment_status_c',
            'label' => 'LBL_STIC_EMPLOYMENT_STATUS',
          ),
          1 => 
          array (
            'name' => 'account_name',
            'label' => 'LBL_ACCOUNT_NAME',
          ),
        ),
        3 => 
        array (
          0 => 
          array (
            'name' => 'title',
            'label' => 'LBL_TITLE',
          ),
          1 => 
          array (
            'name' => 'department',
            'label' => 'LBL_DEPARTMENT',
          ),
        ),
        4 => 
        array (
          0 => 
          array (
            'name' => 'stic_182_error_c',
            'label' => 'LBL_STIC_182_ERROR',
          ),
          1 => 
          array (
            'name' => 'stic_182_excluded_c',
            'label' => 'LBL_STIC_182_EXCLUDED',
          ),
        ),
        5 => 
        array (
          0 => 
          array (
            'name' => 'stic_tax_name_c',
            'label' => 'LBL_STIC_TAX_NAME',
          ),
          1 => 'stic_total_annual_donations_c',
        ),
      ),
      'LBL_STIC_PANEL_GDPR' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'name' => 'lawful_basis',
            'label' => 'LBL_LAWFUL_BASIS',
          ),
          1 => 
          array (
            'name' => 'lawful_basis_source',
            'label' => 'LBL_LAWFUL_BASIS_SOURCE',
          ),
        ),
        1 => 
        array (
          0 => 
          array (
            'name' => 'date_reviewed',
            'label' => 'LBL_DATE_REVIEWED',
          ),
          1 => '',
        ),
      ),
      'LBL_STIC_PANEL_SEPE' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'name' => 'sepe_education_level_c',
            'studio' => 'visible',
            'label' => 'LBL_SEPE_EDUCATION_LEVEL',
          ),
          1 => 
          array (
            'name' => 'sepe_immigrant_c',
            'studio' => 'visible',
            'label' => 'LBL_SEPE_IMMIGRANT',
          ),
        ),
        1 => 
        array (
          0 => 
          array (
            'name' => 'sepe_insertion_difficulties_c',
            'studio' => 'visible',
            'label' => 'LBL_SEPE_INSERTION_DIFFICULTIES',
          ),
          1 => 
          array (
            'name' => 'sepe_disability_c',
            'studio' => 'visible',
            'label' => 'LBL_SEPE_DISABILITY',
          ),
        ),
        2 => 
        array (
          0 => 
          array (
            'name' => 'sepe_benefit_perceiver_c',
            'studio' => 'visible',
            'label' => 'LBL_SEPE_BENEFIT_PERCEIVER',
          ),
          1 => '',
        ),
      ),
      'LBL_STIC_PANEL_INCORPORA' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'name' => 'inc_id_c',
            'label' => 'LBL_INC_ID',
          ),
          1 => 
          array (
            'name' => 'inc_incorpora_record_c',
            'label' => 'LBL_INC_INCORPORA_RECORD',
          ),
        ),
        1 => 
        array (
          0 => 
          array (
            'name' => 'inc_lopd_consent_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_LOPD_CONSENT',
          ),
          1 => 
          array (
            'name' => 'inc_incorporation_date_c',
            'label' => 'LBL_INC_INCORPORATION_DATE',
          ),
        ),
        2 => 
        array (
          0 => 
          array (
            'name' => 'inc_country_origin_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_COUNTRY_ORIGIN',
          ),
          1 => 
          array (
            'name' => 'inc_nationality_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_NATIONALITY',
          ),
        ),
        3 => 
        array (
          0 => 
          array (
            'name' => 'inc_collectives_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_COLLECTIVES',
          ),
          1 => 
          array (
            'name' => 'inc_derivation_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_DERIVATION',
          ),
        ),
        4 => 
        array (
          0 => 
          array (
            'name' => 'inc_disability_degree_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_DISABILITY_DEGREE',
          ),
          1 => 
          array (
            'name' => 'inc_disability_cert_id_c',
            'label' => 'LBL_INC_DISABILITY_CERT_ID',
          ),
        ),
        5 => 
        array (
          0 => 
          array (
            'name' => 'inc_economic_benefits_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_ECONOMIC_BENEFITS',
          ),
        ),
        6 => 
        array (
          0 => 
          array (
            'name' => 'inc_children_c',
            'label' => 'LBL_INC_CHILDREN',
          ),
          1 => 
          array (
            'name' => 'inc_disabled_children_c',
            'label' => 'LBL_INC_DISABLED_CHILDREN',
          ),
        ),
        7 => 
        array (
          0 => 
          array (
            'name' => 'inc_people_in_charge_c',
            'label' => 'LBL_INC_PEOPLE_IN_CHARGE',
          ),
          1 => 
          array (
            'name' => 'inc_requested_workday_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_REQUESTED_WORKDAY',
          ),
        ),
        8 => 
        array (
          0 => 
          array (
            'name' => 'inc_job_characteristics_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_JOB_CHARACTERISTICS',
          ),
          1 => 
          array (
            'name' => 'inc_communications_language_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_COMMUNICATIONS_LANGUAGE',
          ),
        ),
        9 => 
        array (
          0 => 
          array (
            'name' => 'inc_geographical_proximity_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_GEOGRAPHICAL_PROXIMITY',
          ),
          1 => 
          array (
            'name' => 'inc_max_commuting_time_c',
            'label' => 'LBL_INC_MAX_COMMUTING_TIME',
          ),
        ),
        10 => 
        array (
          0 => 
          array (
            'name' => 'inc_driving_licenses_c',
            'label' => 'LBL_INC_DRIVING_LICENSES',
          ),
          1 => 
          array (
            'name' => 'inc_own_vehicle_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_OWN_VEHICLE',
          ),
        ),
        11 => 
        array (
          0 => 
          array (
            'name' => 'inc_car_use_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_CAR_USE',
          ),
          1 => 
          array (
            'name' => 'inc_travel_availability_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_TRAVEL_AVAILABILITY',
          ),
        ),
        12 => 
        array (
          0 => 
          array (
            'name' => 'inc_employment_status_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_EMPLOYMENT_STATUS',
          ),
          1 => 
          array (
            'name' => 'inc_employ_office_reg_time_c',
            'label' => 'LBL_INC_EMPLOY_OFFICE_REG_TIME',
          ),
        ),
        13 => 
        array (
          0 => 
          array (
            'name' => 'inc_requested_employment_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_REQUESTED_EMPLOYMENT',
          ),
        ),
        14 => 
        array (
          0 => 
          array (
            'name' => 'inc_requested_employment_det_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_REQUESTED_EMPLOYMENT_DET',
          ),
        ),
        15 => 
        array (
          0 => 
          array (
            'name' => 'inc_unwanted_employments_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_UNWANTED_EMPLOYMENTS',
          ),
          1 => 
          array (
            'name' => 'inc_observations_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_OBSERVATIONS',
          ),
        ),
      ),
      'LBL_STIC_PANEL_INCORPORA_ADDRESS' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'name' => 'inc_address_street_type_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_ADDRESS_STREET_TYPE',
          ),
          1 => 
          array (
            'name' => 'inc_address_street_c',
            'label' => 'LBL_INC_ADDRESS_STREET',
          ),
        ),
        1 => 
        array (
          0 => 
          array (
            'name' => 'inc_address_num_a_c',
            'label' => 'LBL_INC_ADDRESS_NUM_A',
          ),
          1 => 
          array (
            'name' => 'inc_address_num_b_c',
            'label' => 'LBL_INC_ADDRESS_NUM_B',
          ),
        ),
        2 => 
        array (
          0 => 
          array (
            'name' => 'inc_address_block_c',
            'label' => 'LBL_INC_ADDRESS_BLOCK',
          ),
          1 => 
          array (
            'name' => 'inc_address_floor_c',
            'label' => 'LBL_INC_ADDRESS_FLOOR',
          ),
        ),
        3 => 
        array (
          0 => 
          array (
            'name' => 'inc_address_door_c',
            'label' => 'LBL_INC_ADDRESS_DOOR',
          ),
          1 => 
          array (
            'name' => 'inc_address_district_c',
            'label' => 'LBL_INC_ADDRESS_DISTRICT',
          ),
        ),
        4 => 
        array (
          0 => 
          array (
            'name' => 'inc_address_postal_code_c',
            'label' => 'LBL_INC_ADDRESS_POSTAL_CODE',
          ),
          1 => 
          array (
            'name' => 'inc_location_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_LOCATION',
          ),
        ),
        5 => 
        array (
          0 => 
          array (
            'name' => 'inc_municipality_c',
            'label' => 'LBL_INC_MUNICIPALITY',
          ),
          1 => 
          array (
            'name' => 'inc_town_c',
            'label' => 'LBL_INC_TOWN',
          ),
        ),
        6 => 
        array (
          0 => 
          array (
            'name' => 'inc_state_c',
            'label' => 'LBL_INC_STATE',
          ),
          1 => 
          array (
            'name' => 'inc_country_c',
            'studio' => 'visible',
            'label' => 'LBL_INC_COUNTRY',
          ),
        ),
      ),
      'LBL_STIC_PANEL_INCORPORA_SYNC' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'studio' => 
            array (
              'no_duplicate' => true,
              'editview' => false,
              'quickcreate' => false,
            ),
            'name' => 'inc_reference_officer_c',
            'label' => 'LBL_INC_REFERENCE_OFFICER',
          ),
          1 => 
          array (
            'studio' => 
            array (
              'no_duplicate' => true,
              'editview' => false,
              'quickcreate' => false,
            ),
            'name' => 'inc_reference_entity_c',
            'label' => 'LBL_INC_REFERENCE_ENTITYIN',
          ),
        ),
        1 => 
        array (
          0 => 
          array (
            'studio' => 
            array (
              'no_duplicate' => true,
              'editview' => false,
              'quickcreate' => false,
            ),
            'name' => 'inc_reference_group_c',
            'label' => 'LBL_INC_REFERENCE_GROUP',
          ),
          1 => '',
        ),
        2 => 
        array (
          0 => 
          array (
            'studio' => 
            array (
              'no_duplicate' => true,
              'editview' => false,
              'quickcreate' => false,
            ),
            'name' => 'inc_synchronization_date_c',
            'label' => 'LBL_INC_SYNCHRONIZATION_DATE',
          ),
          1 => 
          array (
            'studio' => 
            array (
              'no_duplicate' => true,
              'editview' => false,
              'quickcreate' => false,
            ),
            'name' => 'inc_synchronization_errors_c',
            'label' => 'LBL_INC_SYNCHRONIZATION_ERRORS',
          ),
        ),
        3 => 
        array (
          0 => 
          array (
            'studio' => 
            array (
              'no_duplicate' => true,
              'editview' => false,
              'quickcreate' => false,
            ),
            'name' => 'inc_synchronization_log_c',
            'label' => 'LBL_INC_SYNCHRONIZATION_LOG',
          ),
        ),
      ),
      'LBL_STIC_PANEL_RECORD_DETAILS' => 
      array (
        0 => 
        array (
          0 => 
          array (
            'name' => 'created_by_name',
            'label' => 'LBL_CREATED',
          ),
          1 => 
          array (
            'name' => 'date_entered',
            'customCode' => '{$fields.date_entered.value}',
            'label' => 'LBL_DATE_ENTERED',
          ),
        ),
        1 => 
        array (
          0 => 
          array (
            'name' => 'modified_by_name',
            'label' => 'LBL_MODIFIED_NAME',
          ),
          1 => 
          array (
            'name' => 'date_modified',
            'customCode' => '{$fields.date_modified.value}',
            'label' => 'LBL_DATE_MODIFIED',
          ),
        ),
      ),
    ),
  ),
);
// END STIC-Custom 