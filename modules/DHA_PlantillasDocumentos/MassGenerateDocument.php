<?php
/**
 * This file is part of Mail Merge Reports by Izertis.
 * Copyright (C) 2015 Izertis. 
 *
 * This file has been modified by SinergiaTIC in SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/EditView/EditView2.php');
#[\AllowDynamicProperties]
class MassGenerateDocument {

   public $sugarbean = NULL;
   public $where_clauses = NULL;
   public $searchFields = NULL;
   public $use_old_search = NULL;

 
   ///////////////////////////////////////////////////////////////////////////////////////////////////      
   function setSugarBean($sugar) {
      $this->sugarbean = $sugar;
   }
   
/**
 * @deprecated
 */     
   ///////////////////////////////////////////////////////////////////////////////////////////////////         
   function buildMassGenerateDocumentLink(){
      // [OBSOLETO]   
      // Esta funcion sirve para añadir una entrada al menu de acciones del listview
      // A partir de la versión 6.5.0 usar la función buildMassGenerateDocumentLink_2
      
      global $app_strings;

      $html = "<a href='#massgeneratedocument_form' style='width: 150px' class='menuItem' onmouseover='hiliteItem(this,\"yes\");' onmouseout='unhiliteItem(this);' onclick=\"document.getElementById('massgeneratedocument_form').style.display = '';\">{$app_strings['LBL_GENERAR_DOCUMENTO']}</a>";

      return $html;
   }   
   
/**
 * @deprecated
 */     
   ///////////////////////////////////////////////////////////////////////////////////////////////////         
   function buildMassGenerateDocumentLink_2($loc = 'top'){
      // [OBSOLETO]
      // Esta funcion sirve para añadir una entrada al menu de acciones del listview
      // A partir de la versión 6.5.0 hay que usar esta en lugar de buildMassGenerateDocumentLink
      
      global $app_strings;

      //$onClick = "document.getElementById('massgeneratedocument_form').style.display = ''; var yLoc = YAHOO.util.Dom.getY('massgeneratedocument_form'); scroll(0,yLoc);";
      $onClick = "showMassGenerateDocumentForm(); var yLoc = YAHOO.util.Dom.getY('massgeneratedocument_form'); scroll(0,yLoc);";
      $html = "<a href='#massgeneratedocument_form' id=\"massgeneratedocument_listview_". $loc ."\" onclick=\"$onClick\">{$app_strings['LBL_GENERAR_DOCUMENTO']}</a>";

      return $html;
   }
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////         
   function current_user_has_access_to_template($template_id, $roles_with_access){
      global $current_user, $db;
      
      // STIC-Custom 20220516 AAM - This code needs to be run after the Security Suite check
      // STIC#734
      // // No tener ningún rol asignado equivale a tener acceso a todos los roles
      // if ((count($roles_with_access) == 0) || (count($roles_with_access) == 1 && empty($roles_with_access[0])))
      // return true;
      // END STIC

      if ($current_user->isAdmin()){
         return true;
      }
      
      if ($current_user->isAdminForModule('DHA_PlantillasDocumentos')){
         return true;
      }
      
      if ($current_user->isDeveloperForModule('DHA_PlantillasDocumentos')){
         return true;
      }

      // STIC-Custom 20220516 AAM - Adding Security Suite access check and the non-role access check
      // STIC#734
      $mmrBean = BeanFactory::getBean('DHA_PlantillasDocumentos', $template_id);
      if (!$mmrBean->ACLAccess('ListView', $mmrBean->isOwner($current_user->id))) {
         return false;
      }

      // Following lines have been moved here from above
      // No tener ningún rol asignado equivale a tener acceso a todos los roles
      if ((count($roles_with_access) == 0) || (count($roles_with_access) == 1 && empty($roles_with_access[0])))
          return true;
      // END STIC
      
      $sql = "SELECT t1.id 
              FROM acl_roles t1 
              INNER JOIN acl_roles_users t2 ON 
                t2.user_id = '{$current_user->id}' AND t2.role_id = t1.id AND t2.deleted = 0 
              WHERE t1.deleted = 0 ";
      $dataset = $db->query($sql);
      while ($row = $db->fetchByAssoc($dataset)) {
         if (in_array($row['id'], $roles_with_access)) { 
            return true;
         }
      }
    
      return false;
   }   
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////         
   function role_enabled_level($role_id){
      // Ver lista dha_plantillasdocumentos_enable_roles_dom
      
      $configurator = new Configurator();
      if (!isset($configurator->config['DHA_templates_enabled_roles'])) {
         return 'ALLOW_ALL';
      }
      elseif (!isset($configurator->config['DHA_templates_enabled_roles'][$role_id])) {
         return 'ALLOW_ALL';
      } 
      else {
         return $configurator->config['DHA_templates_enabled_roles'][$role_id];
      }      
   }   
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////         
   function current_user_enabled_level(){
      // If a User has multiple Roles assigned, we will use the more restrictive Role
      // If a User don't have any Role assigned or User is admin, the permission level will be 'All'

      // Ver lista dha_plantillasdocumentos_enable_roles_dom
      
      global $current_user, $db;
      
      if (empty($current_user)) {
         return 'ALLOW_NONE';
      }      

      if ($current_user->isAdmin()){
         return 'ALLOW_ALL';
      }      
      
      $roles = array();
      $sql = "SELECT acl_roles.id FROM acl_roles ".
             "INNER JOIN acl_roles_users ON ".
             "   acl_roles_users.user_id = '{$current_user->id}' AND acl_roles_users.role_id = acl_roles.id AND acl_roles_users.deleted = 0 ".
             "WHERE acl_roles.deleted = 0 ";
      $dataset = $db->query($sql);
      while ($row = $db->fetchByAssoc($dataset)) {
         $role_id = $row['id'];
         $roles[$role_id] = $this->role_enabled_level($role_id);
      }
      
      $permissions_weights = array (
        'ALLOW_ALL' => 100,
        'ALLOW_DOCX' => 75,  
        'ALLOW_PDF' => 25,
        'ALLOW_NONE' => 0,
      );      
      
      $user_enabled_level = 'ALLOW_ALL';
      foreach($roles as $role_id => $role_enabled_level) {
         if (isset($permissions_weights[$role_enabled_level]) && $permissions_weights[$role_enabled_level] < $permissions_weights[$user_enabled_level]) {
            $user_enabled_level = $role_enabled_level;
         }
      }
      
      return $user_enabled_level;
   }   


   ///////////////////////////////////////////////////////////////////////////////////////////////////         
   function getMassGenerateDocumentForm_Params($action) {

      // Se contemplan permisos de usuario en los valores devueltos. Para ello se usa el usuario activo en el sistema
      
      global $app_strings, $app_list_strings, $db, $current_user, $sugar_config, $sugar_version;
    
      $result = array();
      $result['action'] = $action;
      $result['module'] = $this->sugarbean->module_dir;
      $result['templates'] = array();
      $result['templates_count'] = 0;
      // Not available at this moment for no bwc modules ...
      $result['templates_count_max'] = 10000000; // empty($sugar_config['list_max_entries_per_subpanel']) ? 10 : $sugar_config['list_max_entries_per_subpanel'];
      $result['show_more_templates_link'] = false;
      
      // Permisos
      $result['can_generate_something'] = false;
      $result['can_generate_documents'] = false;
      $result['can_generate_pdf_documents'] = false;
      $result['can_generate_notes'] = false;
      $result['can_generate_emails'] = false;

      
      $result['can_view_templates'] = true;
      if( ACLController::moduleSupportsACL('DHA_PlantillasDocumentos') && !ACLController::checkAccess('DHA_PlantillasDocumentos', 'view', true)){
         $result['can_view_templates'] = false;
      } 

      $result['can_edit_templates'] = true;
      if( ACLController::moduleSupportsACL('DHA_PlantillasDocumentos') && !ACLController::checkAccess('DHA_PlantillasDocumentos', 'edit', true)){
         $result['can_edit_templates'] = false;
      }       

      if (!($action == 'ListView' || $action == 'DetailView')){
         return $result;
      }
      
      if($this->sugarbean->bean_implements('ACL') && !ACLController::checkAccess($this->sugarbean->module_dir, 'view', true)){
         $user_enabled_level = 'ALLOW_NONE';
      }
      else {
         $user_enabled_level = $this->current_user_enabled_level();
      }
      
      $show_docx_button = ($user_enabled_level == 'ALLOW_ALL' || $user_enabled_level == 'ALLOW_DOCX');
      
      $show_pdf_button = $this->canGeneratePDF($user_enabled_level);
      
      $result['can_generate_documents'] = $show_docx_button;
      $result['can_generate_pdf_documents'] = $show_pdf_button;
      $result['can_generate_something'] = $result['can_generate_documents'] || $result['can_generate_pdf_documents'];
      

      // Lo siguiente es redundante (no necesario)
      if ($user_enabled_level == 'ALLOW_NONE') {
         $result['can_generate_something'] = false;
         $result['can_generate_documents'] = false;
         $result['can_generate_pdf_documents'] = false;
      }
      

      if ($result['can_generate_something']) {

         $sql = "select * from dha_plantillasdocumentos 
                 where modulo = '{$result['module']}' and filename is not null and deleted = 0 
                 order by document_name";
         //$sql = $db->limitQuery($sql, 0, $result['templates_count_max'], false, '', false);     

         $dataset = $db->query($sql);
         while ($row = $db->fetchByAssoc($dataset)) {

            if (!$this->current_user_has_access_to_template($row['id'], unencodeMultienum($row['aclroles']))) {
               continue;
            }

            $result['templates_count'] += 1;

            if ($result['templates_count'] > $result['templates_count_max']){
               $result['show_more_templates_link'] = true;
               continue;
            }

            $template = array();
            $template['id'] = $row['id'];
            $template['name'] = $row['document_name'];
            $template['file_ext'] = $row['file_ext'];
            $template['file_mime_type'] = $row['file_mime_type'];
            $template['language'] = $row['idioma'];
            $template['status'] = $row['status_id'];
            $template['description'] = $row['description'];

            $template['language_formated'] = $row['idioma'];
            if (isset($app_list_strings['dha_plantillasdocumentos_idiomas_dom'][$row['idioma']]))
               $template['language_formated'] = $app_list_strings['dha_plantillasdocumentos_idiomas_dom'][$row['idioma']];

            $template['status_formated'] = $row['status_id'];
            if (isset($app_list_strings['dha_plantillasdocumentos_status_dom'][$row['status_id']]))
               $template['status_formated'] = $app_list_strings['dha_plantillasdocumentos_status_dom'][$row['status_id']];

            $template['description_formated'] = $row['description'];
            if (isset($row['description']))
               $template['description_formated'] = nl2br(wordwrap($row['description'],100,'<br>')); 

            $template['file_icon'] = SugarThemeRegistry::current()->getImage($row['file_ext'].'_image_inline', '', null, null, '.gif');

            $result['templates'][] = $template;    
         }

         //if ($result['templates_count'] > 0) {

            // Adjuntar documento generado a Email
            $result['can_generate_emails'] = true;

            // Adjuntar documento generado a Nota
            if (version_compare($sugar_version, '7.0.0', '>=')) {
               // Solo si el modulo principal puede ser padre de una Nota
               if (isset($app_list_strings['record_type_display_notes'][$result['module']])) {
                  $result['can_generate_notes'] = true;
               }            
            }
            else {
               // Solo si estamos en la vista de Detalle y el modulo principal puede ser padre de una Nota
               if ($action == 'DetailView' && isset($app_list_strings['record_type_display_notes'][$result['module']])) {
                  $result['can_generate_notes'] = true;
               }
            }
         //}
         
      }

      return $result;
   }  

   ///////////////////////////////////////////////////////////////////////////////////////////////////         
   function getMassGenerateDocumentForm_Sugar7($action) {
      // Nota: Al contrario de la versión antigua, el formulario devuelto no incluirá los botones de acción. 
      //       Esos botones se incluirán en el código del view massgeneratedocument 
      //       (ver \custom\clients\base\views\massgeneratedocument\massgeneratedocument.js , \custom\clients\base\views\massgeneratedocument\massgeneratedocument.php y \custom\clients\base\views\massgeneratedocument\massgeneratedocument.hbs)
   
      global $app_strings, $sugar_config;
   
      $params = $this->getMassGenerateDocumentForm_Params ($action);
      
      $result = '';
      
      if ($params['can_generate_something']) {
      
         if ($params['templates_count'] > 0) {
         
            if ($params['can_generate_emails'] || $params['can_generate_notes']) {
               $result .= "<div class='pull-right' style='height: 25px;'>";

               if ($params['can_generate_emails']) {
                  $result .= "<label id='MMR_AttachToEmailGeneratedDocument_label' style='vertical-align: top; margin: 4px 0; display: block; float:left;'>
                                 <input type='checkbox' onclick='selectOnlyThisGenerateDocumentOption(this)' 
                                    style='width: 19px; position: static;' 
                                    name='MMR_AttachToEmailGeneratedDocument' id='MMR_AttachToEmailGeneratedDocument' value='1'>
                                 &nbsp; {$GLOBALS['app_strings']['LBL_ADJUNTAR_DOCUMENTO_GENERADO_A_EMAIL']}
                              </label>";
               }               
               
               if ($params['can_generate_notes']) {
                  $result .= "<label style='vertical-align: top; margin: 4px 0; display: block; float:left;'>&nbsp;&nbsp;&nbsp;&nbsp;</label>";
                  $result .= "<label id='MMR_AttachToNoteGeneratedDocument_label' style='vertical-align: top; margin: 4px 0; display: block; float:left;'>
                                 <input type='checkbox' onclick='selectOnlyThisGenerateDocumentOption(this)' 
                                    style='width: 19px; position: static;' 
                                    name='MMR_AttachToNoteGeneratedDocument' id='MMR_AttachToNoteGeneratedDocument' value='1' >
                                 &nbsp; {$GLOBALS['app_strings']['LBL_ADJUNTAR_DOCUMENTO_GENERADO_A_NOTA']}
                              </label>";
               }
               
               $result .= "</div>";
            }         
         
            $field_count = 0;   
            foreach ($params['templates'] as $template_param) {
               $field_count += 1;
               
               $checked = ''; 
               if($field_count == 1) 
                  $checked = 'checked';
               
               if ($params['can_view_templates']) {
                  $template_param['name'] = "<a href='index.php?module=DHA_PlantillasDocumentos&action=DetailView&record={$template_param['id']}'>{$template_param['name']}</a>";
               }
               
               $result = $result . "
                  <tr class='single'>
                     <td>
                       <span class='list'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                           <input type='radio' name='MMR_plantilladocumento_id' value='{$template_param['id']}' {$checked}> {$template_param['name']}
                        </div> </span>
                     </td>
                     <td>
                       <span class='list'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                           {$template_param['file_icon']}&nbsp;&nbsp;{$template_param['file_ext']}
                        </div> </span>
                     </td>
                     <td>
                       <span class='list'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                           {$template_param['language_formated']}
                        </div> </span>
                     </td>
                     <td>
                       <span class='list'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                           {$template_param['status_formated']}
                        </div> </span>
                     </td>
                     <td>
                       <span class='list'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                           {$template_param['description_formated']}
                        </div> </span>
                     </td>
                  </tr>
               ";
            }
            

            if ($params['show_more_templates_link']) {
               $imagen_tipo = SugarThemeRegistry::current()->getImage('docx_image_inline', " id='hidden_template_file_ext_image' ", null, null, '.gif');
               $result = $result . "
                  <tr class='single MMR_hidden_select_template_row' id='hidden_template_row' style='display:none;'>
                     <td>
                       <span class='list'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                           <input type='radio' name='MMR_plantilladocumento_id' id='hidden_template_id' value=''>
                           <span id='hidden_template_name' style='margin-left:4px;'></span>
                        </div> </span>
                     </td>
                     <td>
                       <span class='list' id='hidden_template_file_ext'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                           {$imagen_tipo}&nbsp;&nbsp;
                        </div> </span>
                     </td>
                     <td>
                       <span class='list' id='hidden_template_idioma'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                        </div> </span>
                     </td>
                     <td>
                       <span class='list' id='hidden_template_status'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                        </div> </span>
                     </td>
                     <td>
                       <span class='list' id='hidden_template_description'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                        </div> </span>
                     </td>
                  </tr>
                  
                  <tr class='single MMR_hidden_select_template_row' id='hidden_template_row''>
                     <td>
                       <span class='list'> <div title='' data-placement='bottom' class='ellipsis_inline' style='font-weight: bold;'>
                           <a href='javascript:void(0)' onclick='openTemplatesSelectPopup();'>".translate('LBL_MORE_TEMPLATES', 'DHA_PlantillasDocumentos')."</a>
                        </div> </span>
                     </td>
                     <td>
                       <span class='list' id='hidden_template_file_ext'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                        </div> </span>
                     </td>
                     <td>
                       <span class='list' id='hidden_template_idioma'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                        </div> </span>
                     </td>
                     <td>
                       <span class='list' id='hidden_template_status'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                        </div> </span>
                     </td>
                     <td>
                       <span class='list' id='hidden_template_description'> <div title='' data-placement='bottom' class='ellipsis_inline'>
                        </div> </span>
                     </td>
                  </tr>
               ";
            }

            $result = "
               <div class='flex-list-view-content'>
                  <table class='table table-striped dataTable'>
                     <thead>
                        <tr>
                           <th data-fieldname='XXX' tabindex='-1'>
                              <span>
                                 ".translate('LBL_NAME', 'DHA_PlantillasDocumentos')."
                              </span>
                           </th>
                           <th data-fieldname='XXX' tabindex='-1'>
                              <span>
                                 ".translate('LBL_FORMATO_PLANTILLA', 'DHA_PlantillasDocumentos')."
                              </span>
                           </th>
                           <th data-fieldname='XXX' tabindex='-1'>
                              <span>
                                 ".translate('LBL_IDIOMA_PLANTILLA', 'DHA_PlantillasDocumentos')."
                              </span>
                           </th>
                           <th data-fieldname='XXX' tabindex='-1'>
                              <span>
                                 ".translate('LBL_STATUS', 'DHA_PlantillasDocumentos')."
                              </span>
                           </th>
                           <th data-fieldname='XXX' tabindex='-1'>
                              <span>
                                 ".translate('LBL_DESCRIPTION', 'DHA_PlantillasDocumentos')."
                              </span>
                           </th>
                        </tr>
                     </thead>
                     <tbody>
                        ".$result."
                     </tbody>
                  </table>
               </div>
               ";

            $default_image_path = SugarThemeRegistry::current()->getDefaultImagePath();
            $javascript = <<<EOJS
<script>
   function selectOnlyThisGenerateDocumentOption(optionCheck){
      if (optionCheck.checked) {
         if (optionCheck.id == 'MMR_AttachToEmailGeneratedDocument') {
            if (document.getElementById('MMR_AttachToNoteGeneratedDocument') != null ) {
               document.getElementById('MMR_AttachToNoteGeneratedDocument').checked = false;
            }         
         }
         else if (optionCheck.id == 'MMR_AttachToNoteGeneratedDocument') {
            if (document.getElementById('MMR_AttachToEmailGeneratedDocument') != null ) {
               document.getElementById('MMR_AttachToEmailGeneratedDocument').checked = false;
            }
         }         
      }
   }
   
   function openTemplatesSelectPopup(){
      open_popup( 'DHA_PlantillasDocumentos', 600, 400, '&modulo_advanced={$params['module']}', true, false, {'call_back_function':'templates_popup_set_return', 'form_name':'MassGenerateDocument','field_to_name_array':{'id':'hidden_template_id','name':'hidden_template_name','file_ext':'hidden_template_file_ext','idioma':'hidden_template_idioma','status':'hidden_template_status','description':'hidden_template_description'}}, 'single', true );
   }
   
   function templates_popup_set_return(popup_reply_data){
      set_return(popup_reply_data);
      
      var form_name = 'MassGenerateDocument';
      if ($('#MassGenerateDocument_subpanel').length == 1) {
         form_name = 'MassGenerateDocument_subpanel';
      }
      form_name = '#' + form_name;

      var name_to_value_array = popup_reply_data.name_to_value_array;

      $("#hidden_template_id", form_name).prop('checked', true); 
      $('#hidden_template_id', form_name).val(name_to_value_array['hidden_template_id']); 
      
      $('#hidden_template_name', form_name).text(name_to_value_array['hidden_template_name']); 
      $('#hidden_template_file_ext', form_name).text(name_to_value_array['hidden_template_file_ext']); 
      
      $('#hidden_template_file_ext_image', form_name).attr('src', 'custom/{$default_image_path}/'+name_to_value_array['hidden_template_file_ext']+'_image_inline.png');
      
      $('#hidden_template_idioma', form_name).text(name_to_value_array['hidden_template_idioma']); 
      $('#hidden_template_status', form_name).text(name_to_value_array['hidden_template_status']); 
      
      var str_description = name_to_value_array['hidden_template_description'];
      $('#hidden_template_description', form_name).html(str_description); 
      
      $('#hidden_template_row', form_name).show();
      $('.MMR_select_template_row', form_name).hide();
   }
   
</script>
EOJS;
            $result .= $javascript; 
         
         }
         else {
         
            $result = '<br>' . $app_strings['LBL_NO_HAY_PLANTILLAS_DISPONIBLES_PARA_GENERAR_DOCUMENTO'] . '.   ';
            if ($params['can_edit_templates']) {
               $result .= '<br> <span style="white-space:nowrap;"><a href="#bwc/index.php?module=DHA_PlantillasDocumentos&action=EditView&return_module=DHA_PlantillasDocumentos&return_action=DetailView">
               <img width="16" height="16" border="0" align="absmiddle" src="'.SugarThemeRegistry::current()->getImageURL("CreateDHA_PlantillasDocumentos.png", false).'">
               <span>'.translate('LNK_NEW_RECORD', 'DHA_PlantillasDocumentos').'</span>
               </a></span>';
            }
         }
      }
      else {
         $result = '<br><span class="text-error">' . $app_strings['LBL_NO_ACCESS'] . '</span>';
      }
      
      $result .= '<br><br>';
      
      return $result;
   }
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////         
   function canGeneratePDF($user_enabled_level = 'ALLOW_ALL') {
      global $sugar_config; 
      
      $result = (isset($sugar_config['DHA_OpenOffice_exe']) && $sugar_config['DHA_OpenOffice_exe'] && file_exists($sugar_config['DHA_OpenOffice_exe']) && is_readable($sugar_config['DHA_OpenOffice_exe']) ) ||
                          (!is_windows() && isset($sugar_config['DHA_OpenOffice_cde']) && $sugar_config['DHA_OpenOffice_cde'] && file_exists($sugar_config['DHA_OpenOffice_cde']) && is_readable($sugar_config['DHA_OpenOffice_cde']) );

      $result = $result && ($user_enabled_level == '' || $user_enabled_level == 'ALLOW_ALL' || $user_enabled_level == 'ALLOW_PDF');
      
      return $result;
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////         
   function getMassGenerateDocumentForm() {

      // NOTA: las variables internas del formulario se rellenan desde javascript, y se envian como Post (ver modules\DHA_PlantillasDocumentos\MassGenerateDocument.js)
      // NOTA: Esta funcion se usa tanto en el ListView como en el DetailView
      
      global $app_strings, $app_list_strings, $db, $current_user, $sugar_config; 
    

      if (isset($_REQUEST['action']) && $_REQUEST['action'] == 'DetailView'){
         $accion = 'DetailView';
      } elseif (isset($_REQUEST['action']) && ($_REQUEST['action'] == 'index' || $_REQUEST['action'] == 'ListView')){
         $accion = 'ListView';
      } else {
         return '';
      }
      
      if($this->sugarbean->bean_implements('ACL') && !ACLController::checkAccess($this->sugarbean->module_dir, 'view', true)){
         return '';
      }
      
      $user_enabled_level = $this->current_user_enabled_level();
      if ($user_enabled_level == 'ALLOW_NONE') {
         return '';
      }      
      
      $permiso_ver_plantillas = true;
      if( ACLController::moduleSupportsACL('DHA_PlantillasDocumentos') && !ACLController::checkAccess('DHA_PlantillasDocumentos', 'view', true)){
         $permiso_ver_plantillas = false;
      }      
      
      $html = '';
      $html .= "<form action='index.php' method='post' name='MassGenerateDocument'  id='MassGenerateDocument'>\n";
      
      $row_count = 0;      
      $no_visible = "style='display:none;'";
            
      // Cabecera      
      $html .= "<div id='massgeneratedocument_form' {$no_visible}'>";
      $html .= "<br>";       
      $html .= "<table width='100%' cellpadding='0' cellspacing='0' border='0' class='formHeader h3Row'><tr><td nowrap><h3><span>" . $app_strings['LBL_GENERAR_DOCUMENTO']."</h3></td></tr></table>";
      $html .= "<table cellpadding='0' cellspacing='1' border='0' width='100%' class='edit view' id='mass_update_table'>";
      $html .= "<thead align='left'>";
      $html .= "   <tr>";        
      $html .= "      <td width='40%'><b><u>".translate('LBL_NAME', 'DHA_PlantillasDocumentos')  ."</u></b></td>";
      $html .= "      <td width='10%'><b><u>".translate('LBL_FORMATO_PLANTILLA', 'DHA_PlantillasDocumentos')  ."</u></b></td>";
      $html .= "      <td width='10%'><b><u>".translate('LBL_IDIOMA_PLANTILLA', 'DHA_PlantillasDocumentos')  ."</u></b></td>";
      $html .= "      <td width='10%'><b><u>".translate('LBL_STATUS', 'DHA_PlantillasDocumentos')  ."</u></b></td>";
      $html .= "      <td width='30%'><b><u>".translate('LBL_DESCRIPTION', 'DHA_PlantillasDocumentos')  ."</u></b></td>";
      $html .= "   </tr>";  
      $html .= "</thead>";
      $html .= "<tbody>";

      // Variables internas
      $tablename = $this->sugarbean->getTableName();
      $modulename = $this->sugarbean->module_dir;
      
      $html .= "<input type='hidden' name='moduloplantilladocumento' id='MGD_moduloplantilladocumento' value='{$modulename}'>\n"; // realmente este sería innecesario, se puede obtener a través de plantilladocumento_id
      $html .= "<input type='hidden' name='module' id='MGD_module' value='DHA_PlantillasDocumentos'>\n";
      $html .= "<input type='hidden' name='action' id='MGD_action' value='generatedocument'>\n";  // acción de Sugar, ver modules\DHA_PlantillasDocumentos\controller.php
      $html .= "<input type='hidden' name='mode' id='MGD_mode' value=''>\n";     // este se rellena por el javascript (entire o selected)
      $html .= "<input type='hidden' name='enPDF' id='MGD_enPDF' value='false'>\n";     // este se rellena por el javascript (true o false)
      if ($accion == 'ListView')       
         $html .= "<input type='hidden' name='uid' id='MGD_uid' value=''>\n";   // este se rellena por el javascript (lista de las ids seleccionadas, a no ser que el mode sea igual a entire)
      if ($accion == 'DetailView') 
         $html .= "<input type='hidden' name='uid' id='MGD_uid' value='{$this->sugarbean->id}'>\n";  // si estamos en el DetailView, no se rellena por javascript, ya sabemos su valor
      
      $current_query_by_page = base64_encode(json_encode($_REQUEST));
      
      $html .= "<input type='hidden' name='current_query_by_page' id='MGD_current_query_by_page' value='{$current_query_by_page}' />\n";
      
      $order_by_name = $this->sugarbean->module_dir.'2_'.strtoupper($this->sugarbean->object_name).'_ORDER_BY' ;
      $request_order_by_name = isset($_REQUEST[$order_by_name])? $_REQUEST[$order_by_name] : "";    
      
      $html .= "<input type='hidden' name='{$order_by_name}' id='MGD_{$order_by_name}' value='{$request_order_by_name}' />\n";
      
      $templates_count = 0;
      $templates_count_max = empty($sugar_config['list_max_entries_per_subpanel']) ? 10 : $sugar_config['list_max_entries_per_subpanel'];
      $show_more_templates_link = false;
      
      // Recorrido por las plantillas      
      $sql = "select * from dha_plantillasdocumentos 
              where modulo = '{$modulename}' and filename is not null and deleted = 0 
              order by document_name";      
      //$sql = $db->limitQuery($sql, 0, $templates_count_max, false, '', false);

      $dataset = $db->query($sql);
      while ($row = $db->fetchByAssoc($dataset)) {
         $id = $row['id'];
         
         if (!$this->current_user_has_access_to_template($id, unencodeMultienum($row['aclroles'])))
            continue;
      
         $templates_count += 1;
         
         if ($templates_count > $templates_count_max){
            $show_more_templates_link = true;
            break;
         }
         
         $row_count += 1;
         
         $nombre = $row['document_name'];
         $idioma = '';
         if (isset($app_list_strings['dha_plantillasdocumentos_idiomas_dom'][$row['idioma']]))
            $idioma = $app_list_strings['dha_plantillasdocumentos_idiomas_dom'][$row['idioma']];
         $estado = '';
         if (isset($app_list_strings['dha_plantillasdocumentos_status_dom'][$row['status_id']]))
            $estado = $app_list_strings['dha_plantillasdocumentos_status_dom'][$row['status_id']];
         $descripcion = '';
         if (isset($row['description']))
            $descripcion = nl2br(wordwrap($row['description'],100,'<br>')); 

         $imagen_tipo = SugarThemeRegistry::current()->getImage($row['file_ext'].'_image_inline', '', null, null, '.gif');
         $formato = $imagen_tipo . '&nbsp;&nbsp;' . $row['file_ext'];     
            
         $checked = ''; 
         if($row_count == 1) 
            $checked = 'checked';         
            
         if ($row_count % 2) {   
           $html .= "<tr class='oddListRowS1 MMR_select_template_row''>";  
         } else {
           $html .= "<tr class='evenListRowS1 MMR_select_template_row''>";  
         }    

         if ($permiso_ver_plantillas) {         
            $html .= "<td width='40%'><input type='radio' name='plantilladocumento_id' value='{$id}' {$checked}>  <a target='_blank' href='index.php?module=DHA_PlantillasDocumentos&action=DetailView&record={$id}'>{$nombre}</a></td>";
         } else {
            $html .= "<td width='40%'><input type='radio' name='plantilladocumento_id' value='{$id}' {$checked}>  {$nombre}</td>";         
         }         
         
         $html .= "<td width='10%'>{$formato}</td>";
         $html .= "<td width='10%'>{$idioma}</td>";
         $html .= "<td width='10%'>{$estado}</td>";
         $html .= "<td width='30%'>{$descripcion}</td>";
         $html .= "</tr>";  
      }
      
      // Selección de mas plantillas
      if ($show_more_templates_link) {
         $imagen_tipo = SugarThemeRegistry::current()->getImage('docx_image_inline', " id='hidden_template_file_ext_image' ", null, null, '.gif');
         
         $html .= "<tr id='hidden_template_row' class='MMR_hidden_select_template_row' style='display:none;'>";
         $html .= "<td width='40%'><input type='radio' name='plantilladocumento_id' id='hidden_template_id' value=''><span id='hidden_template_name' style='margin-left:4px;'></span></td>";
         $html .= "<td width='10%'>{$imagen_tipo}&nbsp;&nbsp;<span id='hidden_template_file_ext'></td>";
         $html .= "<td width='10%'><span id='hidden_template_idioma'></span></td>";
         $html .= "<td width='10%'><span id='hidden_template_status'></span></td>";
         $html .= "<td width='30%'><span id='hidden_template_description'></span></td>";
         $html .= "</tr>";
         
         $html .= "<tr><td width='40%'><a href='javascript:void(0)' onclick='openTemplatesSelectPopup();'><img src='custom/themes/default/images/icon_relate_16.png'>".translate('LBL_MORE_TEMPLATES', 'DHA_PlantillasDocumentos')."</a></td></tr>";  
      }
      
      $html .="</tbody></table>";
      
      // Botones (el boton de generar tiene javascript asociado que asigna variables internas y hace submit del form, ver modules\DHA_PlantillasDocumentos\MassGenerateDocument.js )
      $html .= "<table cellpadding='0' cellspacing='0' border='0' width='100%'><tr><td class='buttons' width='400px'>";
      
      $show_docx_button = ($user_enabled_level == 'ALLOW_ALL' || $user_enabled_level == 'ALLOW_DOCX');
      
      if ($show_docx_button) {      
         $html .= "<input type='button' id='MassGenerateDocument_button_{$accion}' name='MassGenerateDocument_button_{$accion}' value='{$GLOBALS['app_strings']['LBL_GENERAR_DOCUMENTO']}' class='button'>&nbsp";
      }
                
      $show_pdf_button = $this->canGeneratePDF($user_enabled_level);
                          
      if ($show_pdf_button){  
         $html .= "<input type='button' id='MassGenerateDocument_button_{$accion}_pdf' name='MassGenerateDocument_button_{$accion}_pdf' value='{$GLOBALS['app_strings']['LBL_GENERAR_DOCUMENTO_PDF']}' class='button'>&nbsp";      
      }      
                
      $html .= "<input onclick='javascript:hideMassGenerateDocumentForm();' type='button' id='cancel_MassGenerateDocument_button' name='cancel_MassGenerateDocument_button' value='{$GLOBALS['app_strings']['LBL_CANCEL_BUTTON_LABEL']}' class='button'>";                

      $html .= "</td><td>";
      
      if ($show_docx_button || $show_pdf_button) {
      
         $default_image_path = SugarThemeRegistry::current()->getDefaultImagePath();
         $javascript = <<<EOJS
<script>
   function selectOnlyThisGenerateDocumentOption(optionCheck){
      if (optionCheck.checked) {
         if (optionCheck.id == 'AttachToEmailGeneratedDocument') {
            if (document.getElementById('AttachToNoteGeneratedDocument') != null ) {
               document.getElementById('AttachToNoteGeneratedDocument').checked = false;
            }         
         }
         else if (optionCheck.id == 'AttachToNoteGeneratedDocument') {
            if (document.getElementById('AttachToEmailGeneratedDocument') != null ) {
               document.getElementById('AttachToEmailGeneratedDocument').checked = false;
            }
         }         
      }
   }
   
   function openTemplatesSelectPopup(){
      open_popup( 'DHA_PlantillasDocumentos', 600, 400, '&modulo_advanced={$modulename}', true, false, {'call_back_function':'templates_popup_set_return', 'form_name':'MassGenerateDocument','field_to_name_array':{'id':'hidden_template_id','name':'hidden_template_name','file_ext':'hidden_template_file_ext','idioma':'hidden_template_idioma','status':'hidden_template_status','description':'hidden_template_description'}}, 'single', true );
   }
   
   function templates_popup_set_return(popup_reply_data){
      set_return(popup_reply_data);
      
      var form_name = 'MassGenerateDocument';
      if ($('#MassGenerateDocument_subpanel').length == 1) {
         form_name = 'MassGenerateDocument_subpanel';
      }
      form_name = '#' + form_name;

      var name_to_value_array = popup_reply_data.name_to_value_array;

      $("#hidden_template_id", form_name).prop('checked', true); 
      $('#hidden_template_id', form_name).val(name_to_value_array['hidden_template_id']); 
      
      $('#hidden_template_name', form_name).text(name_to_value_array['hidden_template_name']); 
      $('#hidden_template_file_ext', form_name).text(name_to_value_array['hidden_template_file_ext']); 
      
      $('#hidden_template_file_ext_image', form_name).attr('src', 'custom/{$default_image_path}/'+name_to_value_array['hidden_template_file_ext']+'_image_inline.png');
      
      $('#hidden_template_idioma', form_name).text(name_to_value_array['hidden_template_idioma']); 
      $('#hidden_template_status', form_name).text(name_to_value_array['hidden_template_status']); 
      
      var str_description = name_to_value_array['hidden_template_description'];
      $('#hidden_template_description', form_name).html(str_description); 
      
      $('#hidden_template_row', form_name).show();
      $('.MMR_select_template_row', form_name).hide();
   }
   
</script>
EOJS;

         $html .= $javascript;
         
         $html .= "<label style='vertical-align: top; line-height: 19px; height: 19px; margin: 2px 0; display: block; float:left;'><input type='checkbox' onclick='selectOnlyThisGenerateDocumentOption(this)' style='width: 19px; height: 19px; vertical-align: bottom;' name='AttachToEmailGeneratedDocument' id='AttachToEmailGeneratedDocument' value='1'>&nbsp; {$GLOBALS['app_strings']['LBL_ADJUNTAR_DOCUMENTO_GENERADO_A_EMAIL']}</label>";
         
         // Adjuntar documento generado a Nota - Solo si estamos en la vista de Detalle y el modulo principal puede ser padre de una Nota
         if ($accion == 'DetailView' && isset($app_list_strings['record_type_display_notes'][$modulename])) {
            $html .= "<label style='vertical-align: top; line-height: 19px; height: 19px; margin: 2px 0; display: block; float:left;'>&nbsp;&nbsp;&nbsp;&nbsp;</label>";
            $html .= "<label style='vertical-align: top; line-height: 19px; height: 19px; margin: 2px 0; display: block; float:left;'><input type='checkbox' onclick='selectOnlyThisGenerateDocumentOption(this)' style='width: 19px; height: 19px; vertical-align: bottom;' name='AttachToNoteGeneratedDocument' id='AttachToNoteGeneratedDocument' value='1' >&nbsp; {$GLOBALS['app_strings']['LBL_ADJUNTAR_DOCUMENTO_GENERADO_A_NOTA']}</label>";
         }         
      }      
      
      $html .= "</td></tr></table>";
      
      global $sugar_version;
      //$match_version = "6\.5\.*";
      //if(!preg_match("/$match_version/", $sugar_version)) {
      if (version_compare($sugar_version, '6.5.0', '<')) {
         $html .= '<script type="text/javascript" src="' . getJSPath('modules/DHA_PlantillasDocumentos/librerias/jQuery/jquery.min.js') . '"></script>';
      }      
      $html .= '<script type="text/javascript" src="' . getJSPath('modules/DHA_PlantillasDocumentos/MassGenerateDocument.js') . '"></script>';       

      $javascript = <<<EOJS
<script>
   function hideMassGenerateDocumentForm(){
      document.getElementById('massgeneratedocument_form').style.display = 'none';
   }
   function showMassGenerateDocumentForm(){
      document.getElementById('massgeneratedocument_form').style.display = '';
   }   
</script>
EOJS;

      $html .= $javascript;
      if ($accion == 'DetailView')
         $html .= "<br><br><br><br><br><br><br><br><br><br><br><br>";  
      $html .= "</div></form>";      

      //$GLOBALS['log']->fatal("HTML ".$html);      
      
      if($row_count > 0) {
         return $html;
      } else {     
         // Si no hay registros de plantillas validas asociadas al modulo se presenta un texto informativo
         
         $permiso_editar_plantillas = true;
         if( ACLController::moduleSupportsACL('DHA_PlantillasDocumentos') && !ACLController::checkAccess('DHA_PlantillasDocumentos', 'edit', true)){
            $permiso_editar_plantillas = false;
         }            
         
         $html = '';
         $html .= "<div id='massgeneratedocument_form' {$no_visible}>";
         $html .= "<br>";   
         $html .= "<table width='100%' cellpadding='0' cellspacing='0' border='0' class='formHeader h3Row'><tr><td nowrap><h3><span>" . $app_strings['LBL_GENERAR_DOCUMENTO']."</h3></td></tr></table>";       
         $html .= $app_strings['LBL_NO_HAY_PLANTILLAS_DISPONIBLES_PARA_GENERAR_DOCUMENTO'] . '.   ';
         if ($permiso_editar_plantillas) {         
            $html .= '<span style="white-space:nowrap;"><a target="_blank" href="index.php?module=DHA_PlantillasDocumentos&action=EditView&return_module=DHA_PlantillasDocumentos&return_action=DetailView">
            <img width="16" height="16" border="0" align="absmiddle" src="'.SugarThemeRegistry::current()->getImageURL("CreateDHA_PlantillasDocumentos.png", false).'">
            <span>'.translate('LNK_NEW_RECORD', 'DHA_PlantillasDocumentos').'</span>
            </a></span>';
         }
         $html .= $javascript;
         $html .= "<table cellpadding='0' cellspacing='0' border='0' width='100%'><tr><td class='buttons'>
                   <input onclick='javascript:hideMassGenerateDocumentForm();' type='button' id='cancel_MassGenerateDocument_button' name='cancel_MassGenerateDocument_button' value='{$GLOBALS['app_strings']['LBL_CANCEL_BUTTON_LABEL']}' class='button'>";                
         $html .= "</td></tr></table>";  
         if ($accion == 'DetailView')
            $html .= "<br><br><br><br><br><br><br><br><br>";          
         $html .= "</div>";         
         return $html;
      }
   }
   

   ///////////////////////////////////////////////////////////////////////////////////////////////////    
   function generateSearchWhere($module, $query) {
      require_once ("include/MassUpdate.php");
      $mass = new MassUpdate();
      $mass->setSugarBean($this->sugarbean);
      $mass->generateSearchWhere($module, base64_decode($query));
      
      if (isset($mass->searchFields))
         $this->searchFields = $mass->searchFields;
      if (isset($mass->where_clauses))
         $this->where_clauses = $mass->where_clauses;
      if (isset($mass->use_old_search))
         $this->use_old_search = $mass->use_old_search;   

      unset($mass);
   }
   
   // STIC-Custom EPS 20241210 Generate order by from current query
   // https://github.com/SinergiaTIC/SinergiaCRM/pull/515
   function generateOrderBy($queryBin64) {
      $queryData = base64_decode($queryBin64);
      $query = json_decode(html_entity_decode($queryData), true);

      // The name of the request field that contains the order by field
      $var_name = $this->sugarbean->module_dir.'2_'.strtoupper($this->sugarbean->object_name);
      // $order_by_name = $this->sugarbean->module_dir.'2_'.strtoupper($this->sugarbean->object_name).'_ORDER_BY' ;
      $order_by_name = $var_name.'_ORDER_BY' ;
      if (empty($query[$order_by_name])) {
         // If there is no order by field, then the default order must be applied. We retrieve it from user preferences
         $userPreferenceOrder = $GLOBALS['current_user']->getPreference('listviewOrder', $var_name);
         return $userPreferenceOrder['orderBy'].' '.$userPreferenceOrder['sortOrder'];
      }

      // the order of the field is contained on lvso field.
      return $query[$order_by_name] . ' ' . $query['lvso'];
   }
   // END STIC-Custom


   ///////////////////////////////////////////////////////////////////////////////////////////////////      
   function handleMassGenerateDocument(){
      
      global $db, $sugar_version;
      
      static $boolean_false_values = array('off', 'false', '0', 'no', '');

      if(isset($_REQUEST['mode']) && $_REQUEST['mode'] == 'selected' && !empty($_REQUEST['uid'])) {
         $_POST['MassGenerateDocument_ids'] = explode(',', $_REQUEST['uid']); 
      }
      elseif(isset($_REQUEST['mode']) && $_REQUEST['mode'] == 'entire') {
         $this->generateSearchWhere($_REQUEST['moduloplantilladocumento'], $_REQUEST['current_query_by_page']);
         // STIC-Custom EPS 20241210 Generate order_by from current_query
         // https://github.com/SinergiaTIC/SinergiaCRM/pull/515
         $order_by = $this->generateOrderBy($_REQUEST['current_query_by_page']);
         // END STIC-Custom
         
         if (version_compare($sugar_version, '6.4.5', '<')) {
            if(empty($order_by)) $order_by = '';
            $ret_array = create_export_query_relate_link_patch($_REQUEST['module'], $this->searchFields, $this->where_clauses);
            if(!isset($ret_array['join'])) {
               $ret_array['join'] = '';
            }
            $query = $this->sugarbean->create_export_query($order_by, $ret_array['where'], $ret_array['join']);
         }
         else {
            if(empty($order_by)) $order_by = '';
            $query = $this->sugarbean->create_new_list_query($order_by, $this->where_clauses, array(), array(), 0, '', false, $this, true, true);
         }
         
         $result = $db->query($query, true);
         $new_arr = array();
         while($val = $db->fetchByAssoc($result, false)) {
            array_push($new_arr, $val['id']);
         }
         $_POST['MassGenerateDocument_ids'] = $new_arr;
      }
      // STIC AAM 20210511 - Avoiding duplicate records: STIC#227
      $_POST['MassGenerateDocument_ids'] = array_unique($_POST['MassGenerateDocument_ids']);
      // END STIC
      
      $enPDF = false;
      if(isset($_REQUEST['enPDF']) && !in_array(strval($_REQUEST['enPDF']), $boolean_false_values)) 
         $enPDF = true;

      if(isset($_POST['MassGenerateDocument_ids']) && is_array($_POST['MassGenerateDocument_ids'])){
         require_once("modules/DHA_PlantillasDocumentos/Generate_Document.php");

         if (isset($_REQUEST['AttachToEmailGeneratedDocument']) && $_REQUEST['AttachToEmailGeneratedDocument']) {
            GenerateDocumentAndAttachToEmail ($_REQUEST['moduloplantilladocumento'], $_REQUEST['plantilladocumento_id'], $_POST['MassGenerateDocument_ids'], $enPDF);
         }
         else if (isset($_REQUEST['AttachToNoteGeneratedDocument']) && $_REQUEST['AttachToNoteGeneratedDocument']) {
            GenerateDocumentAndAttachToNote ($_REQUEST['moduloplantilladocumento'], $_REQUEST['plantilladocumento_id'], $_POST['MassGenerateDocument_ids'], $enPDF);
         }         
         else {
            GenerateDocument ($_REQUEST['moduloplantilladocumento'], $_REQUEST['plantilladocumento_id'], $_POST['MassGenerateDocument_ids'], $enPDF);
         }         
      }

   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////      
   function handleMassGenerateDocument_Sugar7($api, $args){
      // Ver la funcion antigua handleMassGenerateDocument
      
      static $boolean_false_values = array('off', 'false', '0', 'no', '' );
      
      $modulename = $args['module'];
      $uid = $args['uid'];
      $template_id = $args['template_id'];
      
      $inPDF = false;
      if(isset($args['pdf']) && !in_array(strval($args['pdf']), $boolean_false_values)) 
         $inPDF = true; 

      $attach_to_email = false;
      if(isset($args['attach_to_email']) && !in_array(strval($args['attach_to_email']), $boolean_false_values)) 
         $attach_to_email = true;
         
      $attach_to_note = false;
      if(isset($args['attach_to_note']) && !in_array(strval($args['attach_to_note']), $boolean_false_values)) 
         $attach_to_note = true;
      
      if(is_array($uid) && count($uid) > 0){
         require_once("modules/DHA_PlantillasDocumentos/Generate_Document.php");

         if ($attach_to_email) {
            $email_id = GenerateDocumentAndAttachToEmail ($modulename, $template_id, $uid, $inPDF);
            return array('email_id' => $email_id);
         }
         else if ($attach_to_note) {
            $note_id = GenerateDocumentAndAttachToNote ($modulename, $template_id, $uid, $inPDF);
            return array('note_id' => $note_id);
         }         
         else {
            $content = GenerateDocument_Sugar7 ($api, $modulename, $template_id, $uid, $inPDF);
            return $content;
         }
      }

      return "";
   }   

}

?>
