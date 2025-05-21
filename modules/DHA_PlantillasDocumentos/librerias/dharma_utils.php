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

require_once('include/utils.php');


//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
function dha_strconcat($LaCadena1, $LaCadena2, $ElSeparador) {
  if (($LaCadena1) && ($LaCadena2)) {
      return $LaCadena1 . $ElSeparador . $LaCadena2;
  } else if ($LaCadena1) {
      return $LaCadena1;
  } else if ($LaCadena2) {
      return $LaCadena2;
  }
}


//-------------------------------------------------------------------
//------------------------------------------------------------------- 
/**
 * Conversion a la moneda base, para usar en los beans (por problema de conversion visto con los campos de tipo decimal, el que crea por defecto Sugar para los campos currency)
 *
 * @param string $amount Cantidad a convertir
 * @param object $currency objeto de tipo currency, ya debidamente inicializado
 * @return double 
 */
function dha_ConvertToDollar($amount, $currency) {
   if (is_string($amount)) {
      $amount = floatval($amount);
   }    

   if(isset($amount) && !number_empty($amount)){
      return $currency->convertToDollar(unformat_number ($amount));
   } else {
      return $amount;
   }  
}

//-------------------------------------------------------------------
//------------------------------------------------------------------- 
/**
 * Convierte un array o string a utf8
 * Con pequeñas modificaciones. Ver la funcion php iconv, por si es necesaria.
 * This function may be useful do encode array keys and values [and checks first to see if it's already in UTF format]
 *
 */
function dha_utf8_encode($in) {
   if (is_array($in)) {
      foreach ($in as $key => $value) {
         $out[dha_utf8_encode($key)] = dha_utf8_encode($value);
      }
   } elseif(is_string($in)) {
      if(mb_detect_encoding($in, 'UTF-8', true) != "UTF-8")
         // Avoid Deprecated warning: Function utf8_encode() is deprecated since 8.2
         // return utf8_encode($in);
         return mb_convert_encoding($in, 'UTF-8', 'ISO-8859-1');
      else
         return $in;
   } else {
      return $in;
   }
   return $out;
} 

//-------------------------------------------------------------------
//------------------------------------------------------------------- 
/**
 * Esta funcion se encarga de limpiar los tpls (limpiar la cache) de un módulo determinado
 */
function dha_clearTpls($module_name) {

   require_once('modules/Administration/QuickRepairAndRebuild.php');
   global $current_user;
   
   $modules = Array($module_name);
   
   if(is_admin($current_user)){
      $selected_actions = Array('clearTpls');      
      $RepairAndClear = new RepairAndClear();
      $RepairAndClear->repairAndClearAll($selected_actions, $modules, $autoexecute = true, $show_output = false);  // si se hace asi (usando repairAndClearAll), la funcion llama automáticamente a la accion repairDatabase 
   } 
   else {
      $RepairAndClear = new RepairAndClear();
      $RepairAndClear->module_list = $modules;
      $RepairAndClear->show_output = false;
      $RepairAndClear->execute = true;
      $RepairAndClear->clearTpls();
   }
} 


//-------------------------------------------------------------------
//------------------------------------------------------------------- 
/**
 * Handles displaying the header for classic view modules
 * !!! A partir de la version 6.5.0 la función original getClassicModuleTitle no funciona bien. Copio esta nueva funcion desde la version 6.4.4 !!!  (Ver include\utils\layout_utils.php)
 *
 * @param $module String value of the module to create the title section for
 * @param $params Array of arguments used to create the title label.  Typically this is just the current language string label for the section
 * These should be in the form of array('label' => '<THE LABEL>', 'link' => '<HREF TO LINK TO>');
 * the first breadcrumb should be index at 0, and built from there e.g.
 * <code>
 * array(
 *    '<a href="index.php?module=Contacts&amp;action=index">Contacts</a>',
 *    '<a href="index.php?module=Contacts&amp;action=DetailView&amp;record=123">John Smith</a>',
 *    'Edit',
 *    );
 * </code>
 * would display as:
 * <a href='index.php?module=Contacts&amp;action=index'>Contacts</a> >> <a href='index.php?module=Contacts&amp;action=DetailView&amp;record=123'>John Smith</a> >> Edit
 * @param $show_create boolean flag indicating whether or not to display the create link (defaults to false)
 * @param $index_url_override String value of url to override for module index link (defaults to module's index action if none supplied)
 * @param $create_url_override String value of url to override for module create link (defaults to EditView action if none supplied)
 *
 * @return String HTML content for a classic module title section
 */
function dha_getClassicModuleTitle($module, $params, $show_create=false, $index_url_override='', $create_url_override='')
{
	global $sugar_version, $sugar_flavor, $server_unique_key, $current_language, $action;
    global $app_strings;

	$module_title = '';
	$count = count($params);
	$index = 0;

    $module = preg_replace("/ /","",$module);
    $iconPath = "";
    $the_title = "<div class='moduleTitle'>\n";

    if(is_file(SugarThemeRegistry::current()->getImageURL('icon_'.$module.'_32.png',false)))
    {
    	$iconPath = SugarThemeRegistry::current()->getImageURL('icon_'.$module.'_32.png');
    } else if (is_file(SugarThemeRegistry::current()->getImageURL('icon_'.ucfirst($module).'_32.png',false)))
    {
        $iconPath = SugarThemeRegistry::current()->getImageURL('icon_'.ucfirst($module).'_32.png');
    }
    if (!empty($iconPath)) {
    	//$url = (!empty($index_url_override)) ? $index_url_override : "index.php?module={$module}&action=index";
    	//array_unshift ($params,"<a href='{$url}'><img src='{$iconPath}' ". "alt='".$module."' title='".$module."' align='absmiddle'></a>");
      array_unshift ($params,"<img src='{$iconPath}' ". "alt='".$module."' title='".$module."' align='absmiddle' style='display:inline'>");
	}

	$new_params = array();
	$i = 0;
	foreach ($params as $value) {
	  if ((!is_null($value)) && ($value !== "")) {
	    $new_params[$i] = $value;
	    $i++;
	  }
	}

	if(SugarThemeRegistry::current()->directionality == "rtl") {
		$new_params = array_reverse($new_params);
	}

    // STIC-Custom AAM 20220907 - Call to non-static function generates error after several instances
    // $module_title = join(SugarView::getBreadCrumbSymbol(),$new_params);
    $sv = new SugarView();
    $module_title = join($sv->getBreadCrumbSymbol(),$new_params);
    // END STIC

    if(!empty($module_title)){
        $the_title .= "<h2 style='font-size:15px'>".$module_title."</h2>\n";//removing empty H2 tag for 508 compliance
    }


    if ($show_create) {
        $the_title .= "<span class='utils'>";
        $createRecordImage = SugarThemeRegistry::current()->getImageURL('create-record.gif');
        if(empty($create_url_override))
        {
            $create_url_override = "index.php?module={$module}&action=EditView&return_module={$module}&return_action=DetailView";
        }

        $the_title .= <<<EOHTML
&nbsp;
<a href="{$create_url_override}" class="utilsLink">
<img src='{$createRecordImage}' alt='{$GLOBALS['app_strings']['LNK_CREATE']}'></a>
<a href="{$create_url_override}" class="utilsLink">
{$GLOBALS['app_strings']['LNK_CREATE']}
</a>
EOHTML;

        $the_title .= '</span>';
    }

    $the_title .= "</div>\n";
    return $the_title;

}


?>
