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


require_once('modules/DHA_PlantillasDocumentos/Generate_Document.php');

#[\AllowDynamicProperties]
class DHA_DocumentTemplatesCalculatedFields {

   // NOTA: En cada modulo que se necesita, se creará un fichero extendiendo esta clase cuyo nombre de clase será "NOMBREMODULO_DocumentTemplatesCalculatedFields", y se situará bien en /modules/NOMBREMODULO/DHA_DocumentTemplatesCalculatedFields.php o bien en /custom/modules/NOMBREMODULO/DHA_DocumentTemplatesCalculatedFields.php

   // NOTA: Las funciones que obligatoriamente deben de sobreescribirse en los hijos son SetCalcFieldsDefs y CalcFields (esta ultima llamando siempre al parent::CalcFields() )
   //       Las funciones que no son obligatorias de llamar en los hijos (dependiendo solo de si se necesitan) son BeforeMergeBlock y UndefFieldsDefs

   public $bean = NULL;
   public $module = '';      // modulo del que se están requiriendo los datos (por un campo related o un subpanel)
   public $MainModule = '';  // modulo principal, para el que se está lanzando una plantilla
   public $relationship_name = '';  // nombre de la relacion del bean padre para la que estamos obteniendo datos (si viene vacia es que estamos obteniendo datos del modulo principal)
   public $Generate_Document_Instance = NULL;  // desde aqui podemos acceder a $Generate_Document_Instance->datos, al bean padre, etc.
   
   public $CalcFieldsDefs = array();
   private $CalcFieldsDefs_base = array();
   
   public $prefix = 'cf_';
   public $suffix_inWords = '_inWords';

   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function __construct($module, $bean) {
      // Nota: El bean ya debe de estar creado
      
      $this->module = $module;
      $this->bean = $bean;
   }
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   public function SetGenerate_Document_Instance($value){

      $this->Generate_Document_Instance = $value;
      $this->MainModule = $value->modulo;
   }   
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   protected function SetCalcFieldsDefs() {
      // Esta funcion debe de sobreescribirse en los hijos. Para añadir campos calculados
   }   

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   protected function UndefFieldsDefs() {
      // Esta funcion debe de sobreescribirse en los hijos. Para quitar campos de $this->bean->field_name_map
   } 

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function ShowRow() {
      // Con esta funcion se puede evitar que se muestre un registro determinado. Sobreescribir en los hijos si se necesita
      return true;
   }    
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function OrderRows() {
      // Con esta funcion se puede reordenar el array de ids. Si se quiere reordenar ya directamente sobre el array de datos, usar BeforeMergeBlock. 
      // Esta funcion solo se llama de momento para el modulo principal. Si se quiere reordenar los datos de modulos relacionados, usar el array de datos en el evento BeforeMergeBlock.  
      // Sobreescribir en los hijos si se necesita
   }     
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function BeforeMergeBlock() { 
      // Evento. Esta funcion debe de sobreescribirse en los hijos. Se lanza justo antes de hacer el MergeBlock de $this->Generate_Document_Instance->datos al OpenTbs, por si se quiere hacer una ultima modificacion o quitar alguno de los datos que no deben de ir, etc.
      // Tener en cuenta que esta funcion se disparará solo si el modulo es el modulo principal de la plantilla 
   }  

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function AfterMergeBlock() { 
      // Evento. Esta funcion debe de sobreescribirse en los hijos. Se lanza justo despues de hacer el MergeBlock de $this->Generate_Document_Instance->datos al OpenTbs, por si se quiere hacer una ultima modificacion 
      // Tener en cuenta que esta funcion se disparará solo si el modulo es el modulo principal de la plantilla       
   }   

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function AfterShow() { 
      // Evento. Esta funcion debe de sobreescribirse en los hijos. Se lanza justo despues de hacer el Show de la plantilla (despues de haberse generado ya). 
      // Se puede usar para borrar ficheros temporales usados en la generación de la plantilla
      // Tener en cuenta que esta funcion se disparará solo si el modulo es el modulo principal de la plantilla       
   }     
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   private function SetCalcFieldsDefs_base() {
      
      // Solo si el modulo tiene gestion de moneda
      if (isset($this->bean->field_defs['currency_id'])) {
         $this->CalcFieldsDefs_base['currency_name'] = array (
            'name' => 'currency_name',
            'vname' => 'LBL_CURRENCY_NAME',
            'type' => 'varchar',
            'help' => 'Nombre de Moneda',         
         );    
         $this->CalcFieldsDefs_base['currency_symbol'] = array (
            'name' => 'currency_symbol',
            'vname' => 'LBL_CURRENCY_SYMBOL',
            'type' => 'varchar',
            'help' => 'Símbolo de Moneda',         
         );  
         $this->CalcFieldsDefs_base['currency_code'] = array (
            'name' => 'currency_code',
            'vname' => 'LBL_CURRENCY_CODE',
            'type' => 'varchar',
            'help' => 'Código de Moneda',         
         );  

         $this->CalcFieldsDefs_base['currency_name_base'] = array (
            'name' => 'currency_name_base',
            'vname' => 'LBL_CURRENCY_NAME_BASE',
            'type' => 'varchar',
            'help' => 'Nombre de Moneda base en el sistema',         
         );    
         $this->CalcFieldsDefs_base['currency_symbol_base'] = array (
            'name' => 'currency_symbol_base',
            'vname' => 'LBL_CURRENCY_SYMBOL_BASE',
            'type' => 'varchar',
            'help' => 'Símbolo de Moneda base en el sistema',         
         );  
         $this->CalcFieldsDefs_base['currency_code_base'] = array (
            'name' => 'currency_code_base',
            'vname' => 'LBL_CURRENCY_CODE_BASE',
            'type' => 'varchar',
            'help' => 'Código de Moneda base en el sistema',         
         );          
      }
      
      // Solo para el módulo principal
      if ($this->inMainModule()) { 
         $this->CalcFieldsDefs_base['current_date'] = array (
            'name' => 'current_date',
            'vname' => 'LBL_CURRENT_DATE',
            'type' => 'calculated_date',
            'help' => 'Fecha actual',         
         ); 
         $this->CalcFieldsDefs_base['current_date'.$this->suffix_inWords] = array (
            'name' => 'current_date'.$this->suffix_inWords,
            'vname' => 'LBL_CURRENT_DATE',
            'type' => 'calculated_date',
            'help' => 'Fecha actual en letras',         
         );                  
         $this->CalcFieldsDefs_base['current_weekday'] = array (
            'name' => 'current_weekday',
            'vname' => 'LBL_CURRENT_WEEKDAY',
            'type' => 'calculated_date',
            'help' => 'Día de la semana actual',         
         );
      }

      // Solo para el modulo de Notas y el de Documentos. Para sacar la ruta de las imagenes adjuntas, caso de que el archivo adjunto sea una imagen
      if ($this->module == 'Notes' || $this->module == 'Documents') { 
         $this->CalcFieldsDefs_base['image_file_path'] = array (
            'name' => 'image_file_path',
            'vname' => $this->translate('LBL_IMAGE_FILE_PATH', 'DHA_PlantillasDocumentos'),
            'type' => 'image_path',
            'help' => 'Attached image path (only if attached file type is image)',
         ); 
      }      
   }      

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function fName($fieldName) {
      return $this->prefix . $fieldName;
   }
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function SetCalcValue($fieldName, $value) {
      $campo = $this->fName($fieldName);
      $this->bean->$campo = $value;
   }
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function GetCalcValue($fieldName) {
      $campo = $this->fName($fieldName);
      return $this->bean->$campo;
   }   
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function UpdateBeanFieldNameMap() {
      
      $this->UndefFieldsDefs();
   
      unset ($this->CalcFieldsDefs); 
      $this->CalcFieldsDefs = array();
      $this->SetCalcFieldsDefs();
      
      unset ($this->CalcFieldsDefs_base); 
      $this->CalcFieldsDefs_base = array();
      $this->SetCalcFieldsDefs_base();
      
      $this->CalcFieldsDefs = array_merge($this->CalcFieldsDefs_base, $this->CalcFieldsDefs);
      foreach ($this->CalcFieldsDefs as $key => $field) {
         $field['calculated'] = true;
         $field['calculated_MMR'] = true;
         $field['source'] = 'non-db';
         $field['name'] = $this->fName($field['name']);
         $this->bean->field_defs[$field['name']] = $field;
      }      
   }  

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function inMainModule() {
      $returnValue = false;
      if (empty($this->relationship_name))
         $returnValue = true;
         
      return $returnValue;
   }
   
   ///////////////////////////////////////////////////////////////////////////
   function translate($string, $modulo){
      return $this->Generate_Document_Instance->translate($string, $modulo);
   }   
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function CalcFields() {
      if (isset($this->bean->currency_id)) { 
      
         $curr_id = $this->bean->currency_id;
         if (empty($curr_id))
            $curr_id = '-99';   
         $curr = $this->Generate_Document_Instance->currency($curr_id);
         
         $this->SetCalcValue('currency_name', $curr['name']);
         $this->SetCalcValue('currency_symbol', $curr['symbol']);
         $this->SetCalcValue('currency_code', $curr['iso4217']);
         
         
         $curr = $this->Generate_Document_Instance->currency('-99');
         
         $this->SetCalcValue('currency_name_base', $curr['name']);
         $this->SetCalcValue('currency_symbol_base', $curr['symbol']);
         $this->SetCalcValue('currency_code_base', $curr['iso4217']);         
      }
      
      if ($this->inMainModule()) {
         $this->SetCalcValue('current_date', $this->Generate_Document_Instance->FormatearFecha(time()));
         $this->SetCalcValue('current_date'.$this->suffix_inWords, $this->Generate_Document_Instance->FormatearFechaLarga(time()));  
         $this->SetCalcValue('current_weekday', $this->Generate_Document_Instance->FormatearFechaADiaSemana(time())); 
      } 

      if ($this->module == 'Notes' || $this->module == 'Documents') { 
         $this->SetCalcValue('image_file_path', $this->get_image_file_path());
      }       
   }
   

   ///////////////////////////////////////////////////////////////////////////////////////////////////   
   function get_image_file_path() {
      global $sugar_config;
      
      $image_path = '';
      $image_path_original = '';
      
      $mime_types = array("image/jpeg", "image/gif", "image/png", "image/tiff", "image/pjpeg", "image/x-png", "image/x-tiff");
      
      if ($this->module == 'Notes') {
         $mime_type = strtolower($this->bean->file_mime_type);
         $id = $this->bean->id;
      }
      elseif ($this->module == 'Documents') {
         $mime_type = strtolower($this->bean->last_rev_mime_type);
         $id = $this->bean->document_revision_id;
      }
      else {
         return '';
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
      
      $image_ext = '';
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
