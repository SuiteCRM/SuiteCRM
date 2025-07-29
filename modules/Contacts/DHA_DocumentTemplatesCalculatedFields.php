<?php
if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('modules/DHA_PlantillasDocumentos/DHA_DocumentTemplatesCalculatedFields_base.php');

class Contacts_DocumentTemplatesCalculatedFields extends DHA_DocumentTemplatesCalculatedFields {
   // This variable string should be modified with the name of the image field of the module.
   private $imageField = 'photo';
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function __construct($module, $bean) {
      parent::__construct($module, $bean);
   }
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function CalcFields() {
      global $db, $app_list_strings;
   
      parent::CalcFields();
      
      if ($this->MainModule == 'Opportunities') {         
         $id_contact = $this->bean->id;
         $id_opportunity = $this->Generate_Document_Instance->bean_datos->id; 
         
         $sql = " select contact_role from opportunities_contacts where opportunity_id = '{$id_opportunity}' and contact_id = '{$id_contact}' "; 
         $role = $db->getOne($sql);
         $this->bean->opportunity_role = $role;  // $app_list_strings['opportunity_relationship_type_dom']
      }  
      
      $this->SetCalcValue('image_file_path', $this->get_image_file_path()); 
   } 
   
   protected function SetCalcFieldsDefs() {
      $this->CalcFieldsDefs['image_file_path'] = array (
            'name' => 'image_file_path',
            'vname' => $this->translate('LBL_IMAGE_FILE_PATH', 'DHA_PlantillasDocumentos'),
            'type' => 'image_path',
            'help' => $this->translate('LBL_IMAGE_FILE_PATH_HELP', 'DHA_PlantillasDocumentos'),
      );  
   }

   function get_image_file_path() {
      global $sugar_config;

      $image_path = '';
      $image_path_original = '';
      $image_ext = '';
      $mime_type = '';
      
      $mime_types = array("image/jpeg", "image/gif", "image/png", "image/tiff", "image/pjpeg", "image/x-png", "image/x-tiff");
      $imageField = $this->imageField;
      $id = $this->bean->id . '_'.$imageField;
      $image_path_original = "upload://{$id}";
      $mime_type = '';
      if(file_exists($image_path_original)) {
         $mime_type = strtolower(mime_content_type($image_path_original));
      }
      if (!in_array($mime_type, $mime_types)) {
         return '';
      }
      
      $image_path_original = "upload://{$id}";
      
      $templates_dir = $sugar_config['DHA_templates_dir'];
      $templates_dir = $this->Generate_Document_Instance->includeTrailingCharacter ($templates_dir, '/');
      if (!file_exists($templates_dir)) {
         return '';
      } 

      if ($mime_type == "image/jpeg" || $mime_type == "image/pjpeg")
         $image_ext = 'jpg';
      elseif ($mime_type == "image/gif")
         $image_ext = 'gif';
      elseif ($mime_type == "image/png" || $mime_type == "image/x-png")
         $image_ext = 'png';
      elseif ($mime_type == "image/tiff" || $mime_type == "image/x-tiff")
         $image_ext = 'tiff';

      $image_path = $templates_dir . $id . '.' . $image_ext; 
      
      if (copy ($image_path_original, $image_path)) {
         $this->Generate_Document_Instance->temp_files[] = $image_path;  // esto es para que se borre automaticamente la imagen temporal cuando ya se ha generado el documento
         return $image_path; 
      }
      else {
         return '';
      }
   }    

   
}

?>