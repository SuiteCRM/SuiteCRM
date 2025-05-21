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

#[\AllowDynamicProperties]
class DHA_PlantillasDocumentosViewEdit extends ViewEdit{

   //////////////////////////////////////////////////////////////////////////
   function __construct(){
      //parent::ViewEdit();
      parent::__construct();
      
      $this->useForSubpanel = true; 
   }
   
   //////////////////////////////////////////////////////////////////////////   
   function display(){
   
      if (isset($this->bean->id)) {
         $this->ss->assign("FILE_OR_HIDDEN", "hidden");
         if (empty($_REQUEST['isDuplicate']) || $_REQUEST['isDuplicate'] == 'false') {
            $this->ss->assign("DISABLED", "disabled");
         }
      } else {
         $this->ss->assign("FILE_OR_HIDDEN", "file");
      }
      
      
      parent::display();
      
      
      // Modificamos la url que genera Sugar para el campo de tipo 'file' (si se trata de una edicion)
      // Se a�ade cache buster para al menos Firefox 62.0. En esa versi�n se ha visto un problema de cache en la descarga
      if ($this->bean->id && $this->ev->view == 'EditView'){
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
            jQuery("span#uploadfile_old a.tabDetailViewDFLink").attr("href", "{$url}"); 
         });            
      </script>
EOHTML;

         echo $javascript; 
      }      
   }
}

?>