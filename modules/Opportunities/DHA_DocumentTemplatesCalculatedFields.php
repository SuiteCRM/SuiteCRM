<?php
/**
 * This file is part of Mail Merge Reports by Izertis.
 * Copyright (C) 2015 Izertis. 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * You can contact Izertis at email address info@izertis.com.
 */
if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('modules/DHA_PlantillasDocumentos/DHA_DocumentTemplatesCalculatedFields_base.php');

// El prefijo "Custom" en el nombre de la clase no es imprescindible, a menos que ya exista otra clase para el modulo en /modules y �sta tenga que descender de la que ya existe (el funcionamiento es igual que con otras clases de Sugar)
class Opportunities_DocumentTemplatesCalculatedFields extends DHA_DocumentTemplatesCalculatedFields {


   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function __construct($module, $bean) {
      parent::__construct($module, $bean);
   }
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   protected function UndefFieldsDefs() {

      // Hide fields example ...

      /*
      if (isset($this->bean->field_name_map['sales_stage']))
         unset($this->bean->field_name_map['sales_stage']);

      if (isset($this->bean->field_name_map['next_step']))
         unset($this->bean->field_name_map['next_step']);
      */
   }      
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   protected function SetCalcFieldsDefs() {
   
      global $app_strings;
      
      $this->CalcFieldsDefs = array (
         'account-phone_office' => array (
            'name' => 'account-phone_office',
            'vname' => $this->translate('LBL_ACCOUNT', 'Accounts') . ' - ' . $this->translate('LBL_PHONE_OFFICE', 'Accounts'),
            'type' => 'phone',
            'help' => '',
         ),
         
         
         'last-meeting-name' => array (
            'name' => 'last-meeting-name',
            'vname' => 'Last Meeting - ' . $this->translate('LBL_SUBJECT', 'Meetings'),
            'type' => 'name',
            'help' => '',
         ),
         
         'last-meeting-location' => array (
            'name' => 'last-meeting-location',
            'vname' => 'Last Meeting - ' . $this->translate('LBL_LOCATION', 'Meetings'),
            'type' => 'varchar',
            'help' => '',
         ),
         
         'last-meeting-status' => array (
            'name' => 'last-meeting-status',
            'vname' => 'Last Meeting - ' . $this->translate('LBL_STATUS', 'Meetings'),
            'type' => 'varchar',
            'help' => '',
         ),
         
         'last-meeting-date_start' => array (
            'name' => 'last-meeting-date_start',
            'vname' => 'Last Meeting - ' . $this->translate('LBL_DATE', 'Meetings'),
            'type' => 'datetime',
            'help' => '',
         ),
         
         'last-meeting-date_end' => array (
            'name' => 'last-meeting-date_end',
            'vname' => 'Last Meeting - ' . $this->translate('LBL_DATE_END', 'Meetings'),
            'type' => 'datetime',
            'help' => '',
         ),
         
         'last-meeting-duration_hours' => array (
            'name' => 'last-meeting-duration_hours',
            'vname' => 'Last Meeting - ' . $this->translate('LBL_DURATION_HOURS', 'Meetings'),
            'type' => 'int',
            'help' => '',
         ),
         
         'last-meeting-duration_minutes' => array (
            'name' => 'last-meeting-duration_minutes',
            'vname' => 'Last Meeting - ' . $this->translate('LBL_DURATION_MINUTES', 'Meetings'),
            'type' => 'int',
            'help' => '',
         ),
         
      );  
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function CalcFields() {
      parent::CalcFields();

      global $db, $app_list_strings;

      // Related Account fields ...  
      $phone_office = '';      
      $id = $this->bean->id;       
      if ($id) {
         $sql = "select t2.phone_office from accounts_opportunities t1, accounts t2 
                 where t1.opportunity_id = '{$id}' and t2.id = t1.account_id and t1.deleted = 0 and t2.deleted = 0";
         $dataset = $db->query($sql);
         if (!empty($dataset)) {
            $row = $db->fetchByAssoc($dataset);
            if (!empty($row)) {
               $phone_office = $row['phone_office'];               
            }
         }
      }      
      $this->SetCalcValue('account-phone_office', $phone_office); 


      $last_meeting_name = '';
      $last_meeting_location = '';
      $last_meeting_status = '';
      $last_meeting_date_start = null;
      $last_meeting_date_end = null;
      $last_meeting_duration_hours = null;
      $last_meeting_duration_minutes = null;
      if ($id) {
         $sql = "select * from meetings 
                 where parent_type = 'Opportunities' and parent_id = '{$id}' and deleted = 0
                 order by date_start desc
                 limit 1 ";
         $dataset = $db->query($sql);
         if (!empty($dataset)) {
            $row = $db->fetchByAssoc($dataset);
            if (!empty($row)) {
               $last_meeting_name = $row['name'];               
               $last_meeting_location = $row['location'];
               $last_meeting_status = $row['status'];
               $last_meeting_date_start = $row['date_start'];
               $last_meeting_date_end = $row['date_end'];
               $last_meeting_duration_hours = $row['duration_hours'];
               $last_meeting_duration_minutes = $row['duration_minutes'];
            }
         }
      }      
      $this->SetCalcValue('last-meeting-name', $last_meeting_name); 
      $this->SetCalcValue('last-meeting-location', $last_meeting_location); 
      $this->SetCalcValue('last-meeting-status', $app_list_strings['meeting_status_dom'][$last_meeting_status] ?? '');
      $this->SetCalcValue('last-meeting-date_start', $last_meeting_date_start); 
      $this->SetCalcValue('last-meeting-date_end', $last_meeting_date_end); 
      $this->SetCalcValue('last-meeting-duration_hours', $last_meeting_duration_hours);
      $this->SetCalcValue('last-meeting-duration_minutes', $last_meeting_duration_minutes);


      // Format examples ...
      // $num_value = $this->Generate_Document_Instance->FormatearNumero($row['num_value'], false);  
      // $num_value = $this->Generate_Document_Instance->FormatearNumero($row['num_value'], true);  // integer
      // global $timedate;
      // $date_value = $timedate->swap_formats($row['date_value'], $timedate->dbDayFormat, $this->Generate_Document_Instance->userDateFormat);
   }   
   
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function ShowRow() {
      // Filter row example ...
      
      $show = true;
      
      /*
      if ($this->MainModule == 'Accounts') {
         $id_opportunity = $this->bean->id;
         $id_account = $this->Generate_Document_Instance->bean_datos->id; 
         
         if ($id_account == '5375becd-b4ae-b3c1-4f49-5223c9196434') {
            $show = false;
         }       
      }
      */      

      return $show;
   }  

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function OrderRows() {
      // Order rows example (note: OrderRows event is only for MainModule, not for submodules. To order submodules rows, use BeforeMergeBlock event) 

      global $db;

      /*
      if ($this->inMainModule() && !empty($this->Generate_Document_Instance->ids)) {
         $ids_condition = implode ("','", $this->Generate_Document_Instance->ids);
         $ids_condition = "'".$ids_condition."'";

         $this->Generate_Document_Instance->ids = array();
         $sql = "select id from opportunities where id in ({$ids_condition}) order by date_entered desc";
         $dataset = $db->query($sql);
         while ($row = $db->fetchByAssoc($dataset)) {
            $this->Generate_Document_Instance->ids[] = $row['id'];
         }
      }
      */
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function BeforeMergeBlock() { 
      global $db;

      // Order rows example for submodule data. In this example we order opportunities related to account (in account report). 
      // Please note that this event for this example must be placed in Accounts calculated fields class, not here
      // To order rows in MainModule, use OrderRows event (easier to use)

      /*
      if ($this->inMainModule() && !empty($this->Generate_Document_Instance->datos) && $this->Generate_Document_Instance->plantilla_id == '6a449730-a66a-056e-97dd-52303a505802') {
         foreach ($this->Generate_Document_Instance->datos as $main_key => $main_data) {
            if (isset($main_data['opportunities']) && is_array($main_data['opportunities']) && count($main_data['opportunities']) > 1) {
               $data_array = $main_data['opportunities'];
               $data_array_ids = array();
               $data_array_keys = array();
               $data_array_ordered_keys = array();

               foreach ($data_array as $key => $data) {
                  $data_array_ids[] = $data['id'];
                  $data_array_keys[$data['id']] = $key;
               }

               $ids_condition = implode ("','", $data_array_ids);
               $ids_condition = "'".$ids_condition."'";
               $sql = "select id from opportunities where id in ({$ids_condition}) order by date_entered desc";
               $dataset = $db->query($sql);
               $i = 0;
               while ($row = $db->fetchByAssoc($dataset)) {
                  $data_array_ordered_keys[$row['id']] = $i;
                  $i += 1;                  
               }

               $this->Generate_Document_Instance->datos[$main_key]['opportunities'] = array();  // empty array
               foreach ($data_array_ordered_keys as $id => $data) {
                  $this->Generate_Document_Instance->datos[$main_key]['opportunities'][] = $data_array[$data_array_keys[$id]];
               }
               unset($data_array);
            }
         }
      }
      */




      // Filter rows example for submodule data. In this example we will filter Contacts from Denver related to Opportunities ('primary_address_city' field)
      // To filter rows in MainModule, use ShowRow event (easier to use)

      /*
      if ($this->inMainModule() && !empty($this->Generate_Document_Instance->datos) && $this->Generate_Document_Instance->plantilla_id == '50bfbe8b-2eb8-bb03-b68d-5431d3858a7c') {
         foreach ($this->Generate_Document_Instance->datos as $main_key => $main_data) {
            if (isset($main_data['contacts']) && is_array($main_data['contacts']) && count($main_data['contacts']) > 1) {
               $data_array = $main_data['contacts'];
               $data_array_new = array();

               foreach ($data_array as $key => $data) {
                  if ($data['primary_address_city'] == 'Denver') {
                     $data_array_new[] = $data;
                  }
               }

               $this->Generate_Document_Instance->datos[$main_key]['contacts'] = $data_array_new;
               unset($data_array);
            }
         }
      }
      */
      
   }
   
}

?>
