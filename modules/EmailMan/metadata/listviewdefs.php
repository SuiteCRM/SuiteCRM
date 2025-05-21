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




$listViewDefs['EmailMan'] = array(
    'CAMPAIGN_NAME' => array(
        'width' => '10',
        'label' => 'LBL_LIST_CAMPAIGN',
        'link' => true,
        'customCode' => '<a href="index.php?module=Campaigns&action=DetailView&record={$CAMPAIGN_ID}">{$CAMPAIGN_NAME}</a>',
        'default' => true),
    'RECIPIENT_NAME' => array(
        'sortable' => false,
        'width' => '10',
        'label' => 'LBL_LIST_RECIPIENT_NAME',
        'customCode' => '<a href="index.php?module={$RELATED_TYPE}&action=DetailView&record={$RELATED_ID}">{$RECIPIENT_NAME}</a>',
        'default' => true),
    'RECIPIENT_EMAIL' => array(
        'sortable' => false,
        'width' => '10',
        'label' => 'LBL_LIST_RECIPIENT_EMAIL',
        'customCode' => '{$RECIPIENT_EMAIL}</a>',
        'default' => true),
    'MESSAGE_NAME' => array(
        'sortable' => false,
        'width' => '10',
        'label' => 'LBL_LIST_MESSAGE_NAME',
        'customCode' => '<a href="index.php?module=EmailMarketing&action=DetailView&record={$MARKETING_ID}">{$MESSAGE_NAME}</a>',
        'default' => true),
    'SEND_DATE_TIME' => array(
        'width' => '10',
        'label' => 'LBL_LIST_SEND_DATE_TIME',
        'default' => true),
    'SEND_ATTEMPTS' => array(
        'width' => '10',
        'label' => 'LBL_LIST_SEND_ATTEMPTS',
        'default' => true),
    'IN_QUEUE' => array(
        'width' => '10',
        'label' => 'LBL_LIST_IN_QUEUE',
        'default' => true),
    'DATE_ENTERED' => array(
        'width' => '10',
        'label' => 'LBL_DATE_ENTERED',
        'default' => false),  
    'DATE_MODIFIED' => array(
        'width' => '10',
        'label' => 'LBL_DATE_MODIFIED',
        'default' => false),  
    'MODIFIED_USER_ID' => array(
        'width' => '10',
        'label' => 'LBL_MODIFIED_USER',
        'default' => false),                  
    'USER_ID' => array(
        'width' => '10',
        'label' => 'LBL_USER_ID',
        'default' => false),    
);
