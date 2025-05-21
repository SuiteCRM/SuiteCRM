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
require_once('include/MVC/View/SugarView.php');

#[\AllowDynamicProperties]
class DHA_PlantillasDocumentosViewVarList extends SugarView {

   //////////////////////////////////////////////////////////////////////////   
   public function __construct() {
      //$this->options['show_title'] = false;
      // $this->options['show_header'] = false;
      // $this->options['show_footer'] = false;
      // $this->options['show_javascript'] = false; 
      // $this->options['show_subpanels'] = false; 
      // $this->options['show_search'] = false;
      
      //parent::SugarView();
      parent::__construct();
   }
   
   //////////////////////////////////////////////////////////////////////////   
   public function display() {
      // Nota: cuando se entra aqui, sugar ya ha creado una instancia del bean de las plantillas, con lo cual la lista dinamica de modulos ya está cargada en $app_list_strings['dha_plantillasdocumentos_module_dom']  (ver el constructor de modules\DHA_PlantillasDocumentos\DHA_PlantillasDocumentos.php)
      
      global $app_list_strings, $mod_strings;
      
      $opciones_modulos = "<option label='' value=''></option>";
      foreach ($app_list_strings['dha_plantillasdocumentos_module_dom'] as $key => $value) {
         $opciones_modulos .= "<option label='{$value}' value='{$key}'>{$value}</option>";  
      }
      
      $etiqueta_modulo = $mod_strings['LBL_SELECCIONAR_MODULO'];
      
      $html = <<<EOQ
      <div id="EditView_tabs">
         <table width="100%" border="0" cellspacing="1" cellpadding="0" class="edit view">
            <tbody>      
               <tr>
                  <td valign="top" id="modulo_label" width="7%" scope="row">{$etiqueta_modulo}</td>
                  <td valign="top" width="37.5%">
                     <select name="modulo" id="modulo" title="" tabindex="104">
                        {$opciones_modulos}
                     </select>
                  </td>
               </tr>
            </tbody> 
         </table>
      </div>
      <div id="VarList_tabs">
      </div>
EOQ;
      
      echo getClassicModuleTitle('DHA_PlantillasDocumentos', array(translate('LBL_LISTA_VARIABLES')), false);
      echo $html; 
      
      global $sugar_version;
      $match_version = "6\.5\.*";
      if(!preg_match("/$match_version/", $sugar_version)) {
         echo '<script type="text/javascript" src="' . getJSPath('modules/DHA_PlantillasDocumentos/librerias/jQuery/jquery.min.js') . '"></script>';
      }
      echo '<script type="text/javascript" src="' . getJSPath('modules/DHA_PlantillasDocumentos/views/view.varlist.js') . '"></script>';      
      echo '<script type="text/javascript" src="' . getJSPath('modules/DHA_PlantillasDocumentos/librerias/TableSorter/jquery.tablesorter.min.js') . '"></script>';
      echo '<link rel="stylesheet" type="text/css" href="' . getJSPath('modules/DHA_PlantillasDocumentos/librerias/TableSorter/themes/sugar/style.css') . '" />'; 
   }
}
?>
