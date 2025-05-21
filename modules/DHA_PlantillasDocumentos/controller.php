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
require_once('include/MVC/Controller/SugarController.php');
  
#[\AllowDynamicProperties]
class DHA_PlantillasDocumentosController extends SugarController {

   ///////////////////////////////////////////////////////////////////////////
   function action_editview(){
      $this->view = 'edit';
      $GLOBALS['view'] = $this->view;
      
      // Nota: para que aparezca el boton de "Quitar" en el editview (y por lo tanto funcione lo siguiente), hay que
      //       quitar la propiedad 'noChange' del vardef del campo "uploadfile" (que es de tipo file)
      if(!empty($_REQUEST['deleteAttachment'])){
         ob_clean();
         //echo $this->bean->deleteAttachment($_REQUEST['isDuplicate']) ? 'true' : 'false';
         echo $this->bean->BorraArchivoPlantilla($this->bean->id) ? 'true' : 'false';
         sugar_cleanup(true);
      }
   }
   
   ///////////////////////////////////////////////////////////////////////////
   function action_download(){

      $this->view = '';
      $GLOBALS['view'] = '';

      require_once('modules/DHA_PlantillasDocumentos/download_template.php');
   } 

   ///////////////////////////////////////////////////////////////////////////   
   function action_Configuration(){
      if(is_admin($GLOBALS['current_user'])) {
         $this->view = 'config';
         $GLOBALS['view'] = $this->view;    
      }
      else {
         //sugar_die("Unauthorized access to administration");
         sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']); 
      }
      return true;
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function action_generatedocument(){
      // Ver la funcion action_massupdate en include\MVC\Controller\SugarController.php
      // Esta accion será llamada tanto desde el listview (como si fuera un massupdate) como desde el detailview (con un boton)
      
      // Si no se anula la vista por defecto, devuelve tambien el html de una vista y no es lo que se requiere aqui
      $this->view = '';
      $GLOBALS['view'] = '';      

      $bean = SugarModule::get($_REQUEST['moduloplantilladocumento'])->loadBean();
      
      if(!$bean->ACLAccess('detail')){
         ACLController::displayNoAccess(true);
         sugar_cleanup(true);
      }

      set_time_limit(0);
      $GLOBALS['db']->setQueryLimit(0);

      require_once('modules/DHA_PlantillasDocumentos/MassGenerateDocument.php');
      $massDoc = new MassGenerateDocument();      
      $massDoc->setSugarBean($bean);     
      $massDoc->handleMassGenerateDocument();
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function action_varlist(){

      // Se ha creado una vista nueva. Ver modules\DHA_PlantillasDocumentos\views\view.varlist.php
      $this->view = 'varlist';
      $GLOBALS['view'] = $this->view;      
   }

   ///////////////////////////////////////////////////////////////////////////
   function action_modulevarlist(){

      $this->view = '';
      $GLOBALS['view'] = '';

      require_once('modules/DHA_PlantillasDocumentos/Generate_Document.php');
      $GD = new Generate_Document($_REQUEST['moduloPlantilla'], NULL, NULL);  
      $GD->ObtenerHtmlListaVariables();
   }

   ///////////////////////////////////////////////////////////////////////////
   function action_crearplantillabasica(){
   
      require_once('modules/DHA_PlantillasDocumentos/Generate_Document.php');
      $GD = new Generate_Document($_REQUEST['moduloPlantilla'], NULL, NULL);
      $GD->CrearPlantillaBasica();
   } 

   ///////////////////////////////////////////////////////////////////////////   
   public function action_saveconfig(){
      require_once('include/utils.php');
      require_once('modules/DHA_PlantillasDocumentos/UI_Hooks.php');

      global $app_strings, $current_user, $moduleList, $sugar_config;
      
      $this->view = '';
      $GLOBALS['view'] = '';      

      if (!is_admin($current_user)) 
         sugar_die($app_strings['ERR_NOT_ADMIN']);

      require_once('modules/Configurator/Configurator.php');
      $configurator = new Configurator();
      $configurator->loadConfig();  // no es necesario
      
      
      $repair = false;
      $repair_modules = Array();
      
      //if (isset( $_REQUEST['enabled_modules'] )) {
         $DHA_templates_historical_enabled_modules = array();
         $enabled_modules = array ();         
         foreach ( explode (',', $_REQUEST['enabled_modules'] ) as $module_name ) {
            $enabled_modules [$module_name] = $module_name;
         }
         
         $modules = MailMergeReports_get_all_modules();
         $disabled_modules = array();
         foreach ( $modules as $module_name => $def) {
            if (!isset($enabled_modules[$module_name])) {
               $disabled_modules[$module_name] = $module_name;
            }
         }

         foreach ($disabled_modules as $module_name) {
            if (MailMergeReports_after_ui_frame_hook_module_enabled($module_name)) {
               $repair = true;
               $repair_modules[] = $module_name;
            }
            MailMergeReports_after_ui_frame_hook_module_remove($module_name, false);
            $DHA_templates_historical_enabled_modules[$module_name] = false;
         }
         foreach ($enabled_modules as $module_name) {
            if (!MailMergeReports_after_ui_frame_hook_module_enabled($module_name)) {
               $repair = true;
               $repair_modules[] = $module_name;
            }         
            MailMergeReports_after_ui_frame_hook_module_install($module_name, false);
            $DHA_templates_historical_enabled_modules[$module_name] = true;
         } 

         // Guardamos histórico de los módulos habilitados (esto solo sirve para el instalador del componente, para que recupere los modulos habilitados en caso de reinstalacion)
         $configurator->config['DHA_templates_historical_enabled_modules'] = $DHA_templates_historical_enabled_modules;        
      //}
      
      if ( isset( $_REQUEST['templates_roles_enabled_levels'] ) && isset( $_REQUEST['templates_roles_enabled_levels_ids'] )) {
         $DHA_templates_enabled_roles = array();
         $role_ids = explode (',', $_REQUEST['templates_roles_enabled_levels_ids']);
         $role_levels = explode (',', $_REQUEST['templates_roles_enabled_levels']);
         
         foreach($role_ids as $key => $value) {
            $DHA_templates_enabled_roles[$value] = $role_levels[$key];
         } 
         
         $configurator->config['DHA_templates_enabled_roles'] = $DHA_templates_enabled_roles;         
      }      
      
      $configurator->saveConfig();
      
      if ($repair) {
         // ANULADO DE MOMENTO, NO ESTÁ FUNCIONANDO BIEN (SUGAR SE CUELGA, ETC.)
         //MailMergeReports_repairAndClear($repair_modules);
      }
      
      //$jj = MailMergeReports_getClientFileContents('Opportunities', 'view', 'recordlist');

      echo "true";  // necesario
   }   
   
   
   ///////////////////////////////////////////////////////////////////////////
   function action_composeEmail(){
      // Se necesita en SugarCRM 7
      // Ver /custom/modules/Emails/ComposeGeneratedDocumentEmail.php
   
      $this->view = '';
      $GLOBALS['view'] = '';
      
      $email_id = $_REQUEST['recordId'];
      $module = $_REQUEST['return_module'];

      // Redirect ...
      if($email_id) {
         $dURL = "index.php?action=ComposeGeneratedDocumentEmail&module=Emails&return_module=".$module."&recordId=".$email_id; 
         SugarApplication::redirect($dURL);
      }      
   }    
}
?>
