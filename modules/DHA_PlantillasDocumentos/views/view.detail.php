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

require_once('include/MVC/View/views/view.detail.php');

#[\AllowDynamicProperties]
class DHA_PlantillasDocumentosViewDetail extends ViewDetail{


   ///////////////////////////////////////////////////////////////////////////////////////////////////   
   protected function _displayJavascript() {  
      parent::_displayJavascript(); 
      
      // Modificamos la url que genera Sugar para el campo de tipo 'file'
      // Se a�ade cache buster para al menos Firefox 62.0. En esa versi�n se ha visto un problema de cache en la descarga
      
      //$url = "index.php?action=download&record=".$this->bean->id."&module=DHA_PlantillasDocumentos";
      $url = "index.php?entryPoint=download_dha_document_template&type=DHA_PlantillasDocumentos&id=".$this->bean->id."&v=".date('YmdHis');
      
      global $sugar_version;
      $javascript_jQuery = '';
      if (version_compare($sugar_version, '6.5.0', '<')) {
         $javascript_jQuery = '<script type="text/javascript" src="' . getJSPath('modules/DHA_PlantillasDocumentos/librerias/jQuery/jquery.min.js') . '"></script>';
      }        
      
      $javascript =  <<<EOHTML
   {$javascript_jQuery}
   <script type="text/javascript" language="JavaScript">
      jQuery( document ).ready(function() {
         jQuery("span#uploadfile a.tabDetailViewDFLink").attr("href", "{$url}"); 
      });            
   </script>
EOHTML;

      echo $javascript;
   } 
   
   //////////////////////////////////////////////////////////////////////////
   public function preDisplay() {
      parent::preDisplay();
   }

   //////////////////////////////////////////////////////////////////////////
   function display(){
      parent::display();
   }
}

?>