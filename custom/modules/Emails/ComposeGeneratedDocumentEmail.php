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

// Ver el codigo de \modules\Emails\Compose.php

if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

if(isset($_REQUEST['recordId'])) {
   global $current_language;
   
   $emailData = getEmailData($_REQUEST['recordId']);
   
   $namePlusEmail = $emailData['toAddress_names'];
   $Email = $emailData['toAddress'];
  
   $ret = array(
      'to_email_addrs' => $namePlusEmail,
      'parent_type'    => $emailData['parent_type'],
      'parent_id'      => $emailData['parent_id'],
      'parent_name'    => $emailData['parent_name'],
      'subject'        => $emailData['subject'],
      'body'           => $emailData['body'],
      'attachments'    => $emailData['attachments'],
      'email_id'       => $emailData['email_id'],
   );
   
   initFullCompose($ret);
}

// ESTA FUNCION ES IDENTICA A LA QUE ENCONTRAMOS EN \modules\Emails\Compose.php
function initFullCompose($ret) {
   global $current_user;
   $json = getJSONobj();
   $composeOut = $json->encode($ret);

   //For listview 'Email' call initiated by subpanels, just returned the composePackage data, do not
   //include the entire Emails page
   if ( isset($_REQUEST['ajaxCall']) && $_REQUEST['ajaxCall']) {
      echo $composeOut;
   }
   else {
      //For normal full compose screen
      include('modules/Emails/index.php');
      echo "<script type='text/javascript' language='javascript'>\ncomposePackage = {$composeOut};\n</script>";
   }
}


// ESTA FUNCION ES CASI IDENTICA A LA FUNCION getQuotesRelatedData QUE ENCONTRAMOS EN \modules\Emails\Compose.php
function getEmailData($emailId) {
   $return = array();
   
   require_once("modules/Emails/EmailUI.php");
   $email = new Email();
   $email->retrieve($emailId);
   $return['subject'] = $email->name;
   $return['body'] = from_html($email->description_html);
   $return['toAddress'] = $email->to_addrs;   
   $ret = array();
   $ret['uid'] = $emailId;
   $ret = (new EmailUI())->getDraftAttachments($ret);
   $return['attachments'] = $ret['attachments'];
   $return['email_id'] = $emailId;
   
   $return['toAddress_names'] = $email->to_addrs_names;
   $return['parent_id'] = $email->parent_id;
   $return['parent_type'] = $email->parent_type;
   $return['parent_name'] = $email->parent_name;
   
   return $return;
}
