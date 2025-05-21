<?PHP
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
require_once('include/SugarObjects/templates/file/File.php');
require_once ('include/upload_file.php');

#[\AllowDynamicProperties]
class DHA_PlantillasDocumentosParent extends File {

   public $new_schema = true;
   public $module_dir = 'DHA_PlantillasDocumentos';
   public $object_name = 'DHA_PlantillasDocumentos';
   public $table_name = 'dha_plantillasdocumentos';
   public $importable = false;
   public $disable_row_level_security = true ; // to ensure that modules created and deployed under CE will continue to function under team security if the instance is upgraded to PRO
   //var $disable_custom_fields = true;  // Ver comentarios en la funcion MailMergeReports_get_all_modules (en UI_Hooks.php)

   public $id;
   public $date_entered;
   public $date_modified;
   public $modified_user_id;
   public $modified_by_name;
   public $created_by;
   public $created_by_name;
   public $description;
   public $deleted;
   public $created_by_link;
   public $modified_user_link;
   public $assigned_user_id;
   public $assigned_user_name;
   public $assigned_user_link;
   public $document_name;
   public $filename;
   public $file_ext;
   public $file_mime_type;
   public $uploadfile;
   public $active_date;
   public $exp_date;
   public $category_id;
   public $subcategory_id;
   public $status_id;
   public $status;
   public $file_url;
   public $modulo_url;
   public $doc_url;
   public $doc_local_location;
   public $aclroles;
   public $idioma;
   public $modulo;


   ///////////////////////////////////////////////////////////////////////////////////////////////////
   public function __construct(){
      // global $sugar_version;
      // if (version_compare($sugar_version, '7.8.0', '>=')){
      //    parent::__construct();
      // }
      // else{
      //    parent::File();
      // }

      parent::__construct();

      $this->get_dynamic_list_strings();
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function bean_implements($interface){
      switch($interface){
         case 'ACL': return true;
      }
      return false;
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function get_summary_text() {
      return "$this->document_name";
   }

/*
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function populateFromRow(array $row, $convert = false) {
      // Nota: En Sugar 6.5 no existe el parámetro $convert. Ese parámetro se introduce en Sugar 7
      //       En esa versión tampoco devuelve ningun valor

      global $sugar_version;

      $sugar_7 = version_compare($sugar_version, '7.0.0', '>=');

      if ($sugar_7) {
         $row = parent::populateFromRow($row, $convert);
      }
      else {
         parent::populateFromRow($row);
      }

      if (!empty($this->document_name) && empty($this->name)) {
         $this->name = $this->document_name;
      }

      if ($sugar_7) {
         return $row;
      }
   }
*/

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   public function get_dynamic_list_strings (){

      // Problemas con el desinstalador en SugarCRM 7. Se añade el siguiente código para que no de problemas
      global $sugar_version;
      $sugar_7 = version_compare($sugar_version, '7.0.0', '>=');
      if ($sugar_7 && !file_exists('modules/DHA_PlantillasDocumentos/UI_Hooks.php')) {
         return true;
      }


      require_once('modules/DHA_PlantillasDocumentos/UI_Hooks.php');
      global $db, $app_list_strings, $current_user;

      // Rellenamos dinamicamente la lista de modulos (solo aquellos modulos a los que se tiene acceso y que estén habilitados) ...
      unset ($app_list_strings['dha_plantillasdocumentos_module_dom']);
      $app_list_strings['dha_plantillasdocumentos_module_dom'] = array();
      $acl_modules = ACLAction::getUserActions($current_user->id);
      //$enabled_modules = MailMergeReports_after_ui_frame_hook_enabled_modules();  // esto causa un error de recursión
      foreach($acl_modules as $key => $mod){
         if(isset($mod['module']['access']['aclaccess']) && $mod['module']['access']['aclaccess'] >= 0 && MailMergeReports_after_ui_frame_hook_module_enabled($key)){
            $app_list_strings['dha_plantillasdocumentos_module_dom'][$key] = (isset($app_list_strings['moduleList'][$key])) ? $app_list_strings['moduleList'][$key] : $key;
         }
      }
      natcasesort($app_list_strings['dha_plantillasdocumentos_module_dom']);


      // Rellenamos dinamicamente la lista de roles
      unset ($app_list_strings['dha_plantillasdocumentos_roles_dom']);
      $app_list_strings['dha_plantillasdocumentos_roles_dom'] = array();
      $sql = 'select * from acl_roles where deleted = 0 order by name ';
      $dataset = $db->query($sql);
      while ($row = $db->fetchByAssoc($dataset)) {
         $app_list_strings['dha_plantillasdocumentos_roles_dom'][$row['id']] = $row['name'];
      }

      return true;
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function get_file_name($id, $file_ext) {
      global $sugar_config;

      //$template_dir = $sugar_config['upload_dir'];
      $template_dir = $sugar_config['DHA_templates_dir'];

      $file_name = $template_dir . $id;

      if ($file_ext) {
         $file_name = $file_name . '.' . $file_ext;
      }

      return $file_name;
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   // Ver \include\utils\sugar_file_utils.php  y   \include\utils\file_utils.php
   function asegurarDirectorios() {
      global $sugar_config;

      $templates_dir = getcwd() . "/". $sugar_config['DHA_templates_dir'];

      if (!is_dir($templates_dir)) {
         sugar_mkdir($templates_dir, 0775); //mkdir($templates_dir);
      }
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function save($check_notify = false) {
      global $mod_strings;

      // Bloqueo inicial, por el tipo de archivo
      if (isset($this->file_ext) && !empty($this->file_ext)) {
         if (!in_array($this->file_ext, array('docx', 'odt', 'docm', 'xlsx', 'ods', 'xlsm'))) {
            sugar_die($mod_strings['MSG_ERROR_EXTENSION_FICHERO_NO_PERMITIDA']);
         }
      }

      $plantilla = getcwd() . "/". $this->get_file_name($this->id, $this->file_ext);

      // Nota: la plantilla fisica se puede haber borrado en el detailview, por eso se pone esa condicion tambien
      if (empty($this->id) || $this->new_with_id || !is_file($plantilla)) {
         if (!empty($this->uploadfile)) {
            //Move file saved during populatefrompost
            $fichero_original = UploadFile :: get_url($this->filename, $this->id);
            $fichero_destino = getcwd() . "/". $this->get_file_name($this->id, $this->file_ext);

            $this->asegurarDirectorios();
            $OK = rename($fichero_original, $fichero_destino);

            if (!$OK) {
               $this->filename = '';
               $this->file_ext = '';
               $this->file_mime_type = '';
               $this->uploadfile = '';
            }

            //$this->process_save_dates=false; //make sure that conversion does not happen again.
         } else {
            $this->filename = '';
            $this->file_ext = '';
            $this->file_mime_type = '';
            $this->uploadfile = '';
         }
      }

      return parent::save($check_notify);
   }


   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function BorraArchivoPlantilla($id) {
      // borrado de los ficheros de plantilla fisicos asociados
      $plantilla = getcwd() . "/". $this->get_file_name($id, $this->file_ext);

      // tambien borramos los datos relativos al fichero fisico de la tabla
      global $db;
      $SQL = " update dha_plantillasdocumentos set filename = null, file_ext = null, file_mime_type = null, uploadfile = null where id = '{$id}' ";
      $db->query($SQL);

      if (is_file($plantilla)) {
         return unlink ($plantilla);
      }
      return true;  // si el fichero no existe devolvemos un true
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   public function deleteAttachment($isduplicate = "false") {
      // Para el boton de "Quitar" desde el Editview. Solo para version 6.4.0 y superiores. En versiones anteriores no se necesita

      if ($isduplicate == "true") {
         return true;
      }

      return $this->BorraArchivoPlantilla($this->id);
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function mark_deleted($id) {
      $this->BorraArchivoPlantilla($id);

      parent::mark_deleted($id);
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function fill_in_additional_detail_fields() {
      global $mod_strings, $app_list_strings, $sugar_version;

      parent::fill_in_additional_detail_fields();

      // Ver include\SugarObjects\templates\file\File.php ... puesto que para status_id en este modulo se usa dha_plantillasdocumentos_status_dom en lugar de document_status_dom, tenemos que tenerlo en cuenta aqui, ajustando el codigo original (nota: el codigo original de Sugar hará que se genere un Notice en el log de PHP cada vez que se llame a esta funcion)
      if(!empty($this->status_id)) {
         $this->status = $app_list_strings['dha_plantillasdocumentos_status_dom'][$this->status_id];
      }

      $plantilla = getcwd() . "/". $this->get_file_name($this->id, $this->file_ext);
      if (is_file($plantilla)) {
         $ExistePlantilla = true;
      } else {
         $ExistePlantilla = false;
      }

      if(isset($this->document_name)) {
         $this->name = $this->document_name;
      }

      //ICONO Y LINK DE DESCARGA EN EL LISTVIEW ...
      $this->file_url = "";
      if ($ExistePlantilla) {
         $img_name = '';
         $img_name_bare = '';
         if (!empty ($this->file_ext)) {
             $img_name = SugarThemeRegistry::current()->getImageURL(strtolower($this->file_ext)."_image_inline.gif", false);
             $img_name_bare = strtolower($this->file_ext)."_image_inline";
         }

         //set default file name.
         if (!empty ($img_name) && file_exists($img_name)) {
            $img_name = $img_name_bare;
         } else {
            $img_name = "def_image_inline";
         }

         if($this->ACLAccess('DetailView')){
            // Nota Sugar 7: El entry_point debe de empezar por download_xxxx para que Sugar no cambie la url a modo #bwc. Ver la funcion javascript "_rewriteNewWindowLinks"
            //$this->file_url = "<a href='index.php?action=download&record=".$this->id."&module=DHA_PlantillasDocumentos' target='_blank'>".SugarThemeRegistry::current()->getImage($img_name, 'alt="'.$mod_strings['LBL_LIST_VIEW_DOCUMENT'].'"  border="0"')."</a>";
            $this->file_url = "<a href='index.php?entryPoint=download_dha_document_template&type=DHA_PlantillasDocumentos&id=".$this->id."&v=".date('YmdHis')."' target='_blank'>".SugarThemeRegistry::current()->getImage($img_name, 'alt="'.($mod_strings['LBL_LIST_VIEW_DOCUMENT']??'').'"  border="0"')."</a>";
         }
      }


      //ICONO Y LINK EN EL LISTVIEW DEL MODULO...
      $this->modulo_url = '';
      if (version_compare($sugar_version, '7.0', '<')) {
         $img_name = SugarThemeRegistry::current()->getImageURL($this->modulo.".gif", false);
         if (!empty ($img_name) && file_exists($img_name)) {
            $this->modulo_url = '<img align="absmiddle" src="'.getJSPath($img_name).'">';
         } else {
            $img_name = SugarThemeRegistry::current()->getImageURL($this->modulo.".png", false);
            if (!empty ($img_name) && file_exists($img_name)) {
               $this->modulo_url = '<img align="absmiddle" src="'.getJSPath($img_name).'">';
            }
         }
      }
      else {
         $img_name = SugarThemeRegistry::current()->getImageURL('icon_'.$this->modulo.'_32.png', false);
         if (!empty ($img_name) && file_exists($img_name)) {
            $this->modulo_url = '<img align="absmiddle" src="'.getJSPath($img_name).'">';
         } else {
            $img_name = SugarThemeRegistry::current()->getImageURL('icon_'.ucfirst($this->modulo).'_32.png', false);
            if (!empty ($img_name) && file_exists($img_name)) {
               $this->modulo_url = '<img align="absmiddle" src="'.getJSPath($img_name).'">';
            }
         }
      }


      // Esto es para el entryPoint='DHA_download_template' (ver /download_template.php)
      if ($ExistePlantilla) {
         $this->doc_local_location = $this->get_file_name($this->id, $this->file_ext);
      } else {
         $this->doc_local_location = '';
      }

   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function fill_in_additional_list_fields() {
      $this->fill_in_additional_detail_fields();
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function get_list_view_data(){
      $temp_array = $this->get_list_view_array();

      $temp_array['DESCRIPTION'] = nl2br(wordwrap($this->description,100,'<br>'));
      $temp_array['FILE_URL'] = $this->file_url;
      $temp_array['MODULO_URL'] = $this->modulo_url;

      return $temp_array;
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function create_new_list_query($order_by, $where,$filter=array(),$params=array(), $show_deleted = 0,$join_type='', $return_array = false,$parentbean=null, $singleSelect = false, $ifListForExport = false) {

      // FILTRO DE LOS REGISTROS POR LOS MODULOS A LOS QUE TIENE ACCESO EL USUARIO.
      // Fijarse que $app_list_strings['dha_plantillasdocumentos_module_dom'] se rellena dinamicamente en este mismo bean, al crearse

      require_once ('modules/DHA_PlantillasDocumentos/librerias/dharma_utils.php');
      global $app_list_strings;


      $filtro_modulos = '';
      foreach($app_list_strings['dha_plantillasdocumentos_module_dom'] as $key => $mod){
         if ($key) {
            $filtro_modulos = dha_strconcat($filtro_modulos, "'".$key."'", ",");
         }
      }
      if (!$filtro_modulos) {
         $filtro_modulos = "@";
      };
      $filtro_modulos = ' modulo in (' . $filtro_modulos . ') ';
      $where = dha_strconcat($where, $filtro_modulos, ' and ');


      $ret_array = parent::create_new_list_query($order_by, $where, $filter, $params, $show_deleted, $join_type, true, $parentbean, $singleSelect, $ifListForExport);

      if ( !$return_array )
         return  $ret_array['select'] . $ret_array['from'] . $ret_array['where']. $ret_array['order_by'];
      return $ret_array;
   }

}
?>