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


//////////////////////////////////////////////////////////////////////////////
#[\AllowDynamicProperties]
class Generate_Document {

   public $modulo = ''; 
   public $plantilla_id = '';
   public $ids = array();
   public $datos = array();  // los datos que se envian al OpenTBS
   public $bean_datos = NULL;  // bean principal   
   public $bean_plantilla = NULL;
   public $TBS = NULL;
   public $relaciones = array();
   public $relaciones_related = array();
   public $mod_strings_cache = array();
   public $currencies_cache = array();
   
   public $template_download_location = '';
   public $template_filename = '';
   public $template_file_ext = 'docx';  // tambien puede ser 'odt', 'ods', 'xlsx', 'xlsm' o 'docm'
   public $isOfficeOpenXML = true;
   public $isOpenDocument = false;   
   public $is_Document = true;
   public $is_SpreadSheet = false;     
   public $generated_file_prefix = 'GEN-';

   public $separador_campo_related = '@@';
   public $separador_campo_relacion = '^';
   public $enPDF = false;
   public $Download = true;  // si es false no se descargará automaticamente
   public $Download_filename = '';  // si $Download es false, se dejará aqui el nombre del fichero a descargar una vez generado
   public $Download_mimetype = '';  // si $Download es false, se dejará aqui el mime type del fichero a descargar una vez generado   
   public $CalculatedFieldsClass = NULL;
   
   public $userDateTimePrefs = NULL;
   public $userDateFormat = '';
   public $userDateTimeFormat = '';
   public $userTZ = '';
   public $userTZlabel = '';
   public $thousands_sep = '';
   public $decimal_point = '';
   public $decimals = '';   
   
   public $idiomaDateFormat = ''; 
   public $idiomaDateTimeFormat = '';
   public $idiomaLongDateFormat = '';
   public $idiomaThousands_sep = '';
   public $idiomaDecimal_point = '';
   public $idiomaBool_0 = ''; 
   public $idiomaBool_1 = '';
   public $idiomaWeek_days = NULL;
   public $idiomaMonth_names = NULL;
   public $idiomaNumberToWords = false;
   public $idiomaCurrencyToWords = false;
   
   public $acl_modules = NULL;  

   public $idioma = '';  // los posibles valores para esta variable vienen de $app_list_strings['dha_plantillasdocumentos_idiomas_dom'] 
   private $idiomaDefecto = '';
   public $lang_format_config = NULL;
   
   public $temp_files = array();   // paths de ficheros temporales usados durante la generacion de un documento (por ejemplo imagenes generadas temporalmente desde una clase de campos calculados o en la exportacion a pdf). Ver la funcion GetTempFileName
   public $template_source = '';   // codigo fuente de la propia plantilla   


   ///////////////////////////////////////////////////////////////////////////
   function __construct($modulo, $plantilla_id, $ids){
      global $current_user;
      
      // Indicamos al PHP que este proceso no debe de tener limite de tiempo
      set_time_limit(0);
      $GLOBALS['db']->setQueryLimit(0);

      $this->modulo = $modulo;
      $this->plantilla_id = $plantilla_id;
      $this->ids = $ids;

      $this->bean_datos = SugarModule::get($this->modulo)->loadBean();

      // Clase de los campos calculados del bean principal de datos
      $this->CalculatedFieldsClass = $this->GetCalculatedFieldsClass($this->modulo, $this->bean_datos, '');
      if (!is_null($this->CalculatedFieldsClass))
         $this->CalculatedFieldsClass->UpdateBeanFieldNameMap();

      // Variables de formatos:
      $this->SetVariablesFormatos();
      
      // Variables de permisos
      $this->acl_modules = ACLAction::getUserActions($current_user->id);        
   }
   
   ///////////////////////////////////////////////////////////////////////////   
   function __destruct() {
   
      // Borrado de archivos temporales usados
      foreach ($this->temp_files as $temp_file) {
         if ($temp_file && is_file($temp_file) && is_writable($temp_file) ) {
            unlink ($temp_file);
         }
      }
   }      
   
   ///////////////////////////////////////////////////////////////////////////
   function SetVariablesFormatos () { 
      global $current_user, $timedate, $locale, $sugar_config;
      
      $this->userDateTimePrefs = $current_user->getUserDateTimePreferences();
      $this->userDateFormat = $timedate->get_date_format();
      $this->userDateTimeFormat = $timedate->get_date_time_format();

      $this->userTZ = $current_user->getPreference("timezone");
      $this->userTZlabel = $this->userTZ." ".$this->userDateTimePrefs["userGmt"];

      $seps = get_number_separators();
      $this->thousands_sep = $seps[0];
      $this->decimal_point = $seps[1];
      $this->decimals = $locale->getPrecision();
      
      
      // Formatos internacionales
      $SourceFile = 'modules/DHA_PlantillasDocumentos/lang_format_config.php';
      if (file_exists('custom/'. $SourceFile)) {
         $SourceFile = 'custom/'. $SourceFile;
      }
      require($SourceFile);  // no require_once     
      $this->lang_format_config = $lang_format_config;     
      
      
      $this->idiomaDefecto = $sugar_config['DHA_templates_default_lang'];
      if (empty($this->idioma))
         $this->idioma = $this->idiomaDefecto;
      if (!isset($this->lang_format_config[$this->idioma]))
         $this->idioma = $this->idiomaDefecto;

      if (isset($this->lang_format_config[$this->idioma]['thousands_sep']))
         $this->idiomaThousands_sep = $this->lang_format_config[$this->idioma]['thousands_sep'];
      else 
         $this->idiomaThousands_sep = $this->lang_format_config[$this->idiomaDefecto]['thousands_sep'];    

      if (isset($this->lang_format_config[$this->idioma]['decimal_point']))
         $this->idiomaDecimal_point = $this->lang_format_config[$this->idioma]['decimal_point'];
      else 
         $this->idiomaDecimal_point = $this->lang_format_config[$this->idiomaDefecto]['decimal_point'];  

      if (isset($this->lang_format_config[$this->idioma]['date_format']))
         $this->idiomaDateFormat = $this->lang_format_config[$this->idioma]['date_format'];
      else 
         $this->idiomaDateFormat = $this->lang_format_config[$this->idiomaDefecto]['date_format'];           
        
      if ($this->idioma != 'he') {        
         if (isset($this->lang_format_config[$this->idioma]['time_format']))
            $this->idiomaDateTimeFormat = $this->idiomaDateFormat . ' ' . $this->lang_format_config[$this->idioma]['time_format'];
         else 
            $this->idiomaDateTimeFormat = $this->idiomaDateFormat . ' ' . $this->lang_format_config[$this->idiomaDefecto]['time_format'];
      }
      else {
         if (isset($this->lang_format_config[$this->idioma]['time_format']))
            $this->idiomaDateTimeFormat = $this->lang_format_config[$this->idioma]['time_format'] . ' ' . $this->idiomaDateFormat;
         else 
            $this->idiomaDateTimeFormat = $this->lang_format_config[$this->idiomaDefecto]['time_format'] . ' ' . $this->idiomaDateFormat;
      }      

      if (isset($this->lang_format_config[$this->idioma]['long_date_format']))
         $this->idiomaLongDateFormat = $this->lang_format_config[$this->idioma]['long_date_format'];
      else 
         $this->idiomaLongDateFormat = $this->lang_format_config[$this->idiomaDefecto]['long_date_format'];  
         
      if (isset($this->lang_format_config[$this->idioma]['bool_0']))
         $this->idiomaBool_0 = $this->lang_format_config[$this->idioma]['bool_0'];
      else 
         $this->idiomaBool_0 = $this->lang_format_config[$this->idiomaDefecto]['bool_0'];        

      if (isset($this->lang_format_config[$this->idioma]['bool_1']))
         $this->idiomaBool_1 = $this->lang_format_config[$this->idioma]['bool_1'];
      else 
         $this->idiomaBool_1 = $this->lang_format_config[$this->idiomaDefecto]['bool_1'];       

      if (isset($this->lang_format_config[$this->idioma]['week_days']))
         $this->idiomaWeek_days = $this->lang_format_config[$this->idioma]['week_days'];
      else 
         $this->idiomaWeek_days = $this->lang_format_config[$this->idiomaDefecto]['week_days'];            
         
      if (isset($this->lang_format_config[$this->idioma]['months']))
         $this->idiomaMonth_names = $this->lang_format_config[$this->idioma]['months'];
      else 
         $this->idiomaMonth_names = $this->lang_format_config[$this->idiomaDefecto]['months'];
         
      $this->idiomaNumberToWords = false;
      if (isset($this->lang_format_config[$this->idioma]['NumberToWords']))
         $this->idiomaNumberToWords = $this->lang_format_config[$this->idioma]['NumberToWords'];     

      $this->idiomaCurrencyToWords = false;
      if (isset($this->lang_format_config[$this->idioma]['CurrencyToWords']))
         $this->idiomaCurrencyToWords = $this->lang_format_config[$this->idioma]['CurrencyToWords'];            
         
      
      //Set default timezone for php date/datetime functions
      date_default_timezone_set($this->userTZ);      
   }   

   
   ///////////////////////////////////////////////////////////////////////////
   function AsignarVariablesTipoPlantilla ($file_ext) {
      $this->isOfficeOpenXML = ($file_ext == 'docx' || $file_ext == 'docm' || $file_ext == 'xlsx' || $file_ext == 'xlsm');
      $this->isOpenDocument = ($file_ext == 'odt' || $file_ext == 'ods');  

      $this->is_Document = ($file_ext == 'docx' || $file_ext == 'docm' || $file_ext == 'odt');
      $this->is_SpreadSheet = ($file_ext == 'xlsx' || $file_ext == 'xlsm' || $file_ext == 'ods');
   }   

   ///////////////////////////////////////////////////////////////////////////
   function CargarPlantilla () {
   
      if (!$this->ModuloConPermiso ($this->modulo))
         return false;    
      
      require_once ('modules/DHA_PlantillasDocumentos/librerias/TinyButStrong/tbs_class.php');
      require_once ('modules/DHA_PlantillasDocumentos/librerias/OpenTBS/tbs_plugin_opentbs.php');

      // Datos de la plantilla
      $this->bean_plantilla = SugarModule::get('DHA_PlantillasDocumentos')->loadBean();
      $this->bean_plantilla->retrieve($this->plantilla_id);
      
      if ($this->idioma != $this->bean_plantilla->idioma) { // en el constructor ya se ha llamado a SetVariablesFormatos y se ha asignado el idioma por defecto
         $this->idioma = $this->bean_plantilla->idioma;
         
         // Variables de formatos:
         $this->SetVariablesFormatos();
      }

      $this->template_download_location = $this->bean_plantilla->get_file_name($this->bean_plantilla->id, $this->bean_plantilla->file_ext);
      $this->template_filename = $this->bean_plantilla->filename;
      $this->template_file_ext = strtolower($this->bean_plantilla->file_ext);

      $this->AsignarVariablesTipoPlantilla($this->template_file_ext);
   

      $this->TBS = new clsTinyButStrong; // new instance of TBS
      $this->TBS->Plugin(TBS_INSTALL, OPENTBS_PLUGIN); // load OpenTBS plugin
      $this->TBS->fct_prefix = 'f_'; // Prefijo obligatorio permitido para las funciones llamadas con 'onformat' y 'ondata'
      $this->TBS->var_prefix = '';   // Prefijo obligatorio permitido para las variables de tipo [onload] [onshow] y [var] usadas en las plantillas. Vacío significa que no hay restriccion
      
      // Load the template
      $this->TBS->LoadTemplate($this->template_download_location, OPENTBS_ALREADY_UTF8);  // Nota: sin el segundo parametro los datos aparecen mal, porque en el crm ya se guardan en UTF-8. Ver documentacion del OpenTBS
      
      $this->TBS->ObjectRef = $this;
      
      // Alias de bloques (por si quieren usarse)
      if ($this->is_Document) {
         $this->TBS->SetOption('block_alias', 'parrafo', $this->BloqueFormatoFichero($this->template_file_ext, 'parrafo')); 
         $this->TBS->SetOption('block_alias', 'paragraph', $this->BloqueFormatoFichero($this->template_file_ext, 'paragraph'));
         $this->TBS->SetOption('block_alias', 'p', $this->BloqueFormatoFichero($this->template_file_ext, 'paragraph'));
      
         $this->TBS->SetOption('block_alias', 'tabla', $this->BloqueFormatoFichero($this->template_file_ext, 'tabla')); 
         $this->TBS->SetOption('block_alias', 'table', $this->BloqueFormatoFichero($this->template_file_ext, 'table')); 
         $this->TBS->SetOption('block_alias', 't', $this->BloqueFormatoFichero($this->template_file_ext, 'table')); 
      
         $this->TBS->SetOption('block_alias', 'linea_tabla', $this->BloqueFormatoFichero($this->template_file_ext, 'linea_tabla'));  
         $this->TBS->SetOption('block_alias', 'table_row', $this->BloqueFormatoFichero($this->template_file_ext, 'table_row')); 
         $this->TBS->SetOption('block_alias', 'tr', $this->BloqueFormatoFichero($this->template_file_ext, 'table_row'));

         $this->TBS->SetOption('block_alias', 'celda_tabla', $this->BloqueFormatoFichero($this->template_file_ext, 'celda_tabla')); 
         $this->TBS->SetOption('block_alias', 'table_cell', $this->BloqueFormatoFichero($this->template_file_ext, 'table_cell')); 
         $this->TBS->SetOption('block_alias', 'tc', $this->BloqueFormatoFichero($this->template_file_ext, 'table_cell')); 
       
         $this->TBS->SetOption('block_alias', 'cuerpo', $this->BloqueFormatoFichero($this->template_file_ext, 'cuerpo')); 
         $this->TBS->SetOption('block_alias', 'body', $this->BloqueFormatoFichero($this->template_file_ext, 'body'));            
         $this->TBS->SetOption('block_alias', 'bo', $this->BloqueFormatoFichero($this->template_file_ext, 'body'));
      
         $this->TBS->SetOption('block_alias', 'linea_lista', $this->BloqueFormatoFichero($this->template_file_ext, 'linea_lista')); 
         $this->TBS->SetOption('block_alias', 'list_item', $this->BloqueFormatoFichero($this->template_file_ext, 'list_item'));       
         $this->TBS->SetOption('block_alias', 'li', $this->BloqueFormatoFichero($this->template_file_ext, 'list_item')); 
      }
      
      if ($this->is_SpreadSheet) {
         $this->TBS->SetOption('block_alias', 'linea', $this->BloqueFormatoFichero($this->template_file_ext, 'linea')); 
         if ($this->isOpenDocument)
            $this->TBS->SetOption('block_alias', 'row', $this->BloqueFormatoFichero($this->template_file_ext, 'linea'));  // Excel ya tiene 'row' como definicion de fila, esta definicion no debe de ir para excel. Ademas, si va Apache se queda colgado.
         $this->TBS->SetOption('block_alias', 'r', $this->BloqueFormatoFichero($this->template_file_ext, 'linea'));

         $this->TBS->SetOption('block_alias', 'celda', $this->BloqueFormatoFichero($this->template_file_ext, 'celda')); 
         $this->TBS->SetOption('block_alias', 'cell', $this->BloqueFormatoFichero($this->template_file_ext, 'celda'));          
         if ($this->isOpenDocument)
            $this->TBS->SetOption('block_alias', 'c', $this->BloqueFormatoFichero($this->template_file_ext, 'celda'));    // Idem a la definicion de 'row' para excel     
      }      


      // obtenemos el codigo fuente de la propia plantilla del bloque principal
      $this->template_source = $this->TBS->GetBlockSource('a', false, true, false); 

      // Obtenemos las relaciones
      if ($this->is_Document) {
         if ($this->is_Document) {
            $reg_exp = "/\[a;block={$this->BloqueFormatoFichero($this->template_file_ext, 'body')};([^]]*)\]/i";
         }
         if ($this->is_SpreadSheet) {
            // Esto realmente no sirve de nada por el momento, porque no es posible mostrar datos de subpaneles en hojas de calculo
            $reg_exp = "/\[a;block=tbs:row;([^]]*)\]/i";  // un bloque de fila
            //$reg_exp = "/\[a;block=tbs:row\+tbs:row;([^]]*)\]/i";  // dos bloques
         }      
         if (preg_match($reg_exp, $this->template_source, $coincidencias)) {
            $relaciones = trim($coincidencias[1]);
            $relaciones = explode(";", $relaciones);

            $fin = count($relaciones);
            for($i = 0; $i < $fin; $i++) {
               $relaciones[$i] = explode("=", $relaciones[$i]);
               $relaciones[$i] = str_replace('(', '', $relaciones[$i]);
               $relaciones[$i] = str_replace(')', '', $relaciones[$i]);
               $relaciones[$i] = trim($relaciones[$i][1]);
            }
            $relaciones = array_unique ($relaciones);
         
            foreach ($relaciones as $relacion) {
               $this->relaciones[] = $relacion;
            }
         }
      }
      
      // Buscamos todas las relaciones de tipo related, que se encontrarán entre el texto
      if (preg_match_all("/a\.(\w*){$this->separador_campo_related}/i", $this->template_source, $coincidencias)) {
         $relaciones_related = $coincidencias[1];

         $relaciones_related = array_unique ($relaciones_related);
         
         foreach ($relaciones_related as $relacion) {
            $this->relaciones_related[] = $relacion;
         }
      }
      
      // Buscamos todas las relaciones de tipo related con notación especial de tipo serial, que se encontrarán entre el texto
      if (preg_match_all("/a_\d\.(\w*){$this->separador_campo_related}/i", $this->template_source, $coincidencias)) {
         $relaciones_related = $coincidencias[1];

         $relaciones_related = array_unique ($relaciones_related);
         
         foreach ($relaciones_related as $relacion) {
            $this->relaciones_related[] = $relacion;
         }
      }
      
      $this->relaciones_related = array_unique ($this->relaciones_related);
     
   }
   
   ///////////////////////////////////////////////////////////////////////////
   function GetMimeTypeFromFileName($file_name) {
      $partes_ruta = pathinfo($file_name);
      return $this->GetMimeType($partes_ruta['extension']);
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function GetMimeType($file_ext) {
      $file_ext = strtolower($file_ext);
      $file_mime_type = '';
      
      if ($file_ext == 'docx') 
         $file_mime_type = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document';
      elseif ($file_ext == 'docm') 
         $file_mime_type = 'application/vnd.ms-word.document.macroEnabled.12';
      elseif ($file_ext == 'odt') 
         $file_mime_type = 'application/vnd.oasis.opendocument.text';
      elseif ($file_ext == 'pdf') 
         $file_mime_type = 'application/pdf';
      elseif ($file_ext == 'ods') 
         $file_mime_type = 'application/vnd.oasis.opendocument.spreadsheet';         
      elseif ($file_ext == 'xlsx') 
         $file_mime_type = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';  
      elseif ($file_ext == 'xlsm') 
         $file_mime_type = 'application/vnd.ms-excel.sheet.macroEnabled.12';          

      return $file_mime_type;         
   }      
   
   ///////////////////////////////////////////////////////////////////////////
   function includeTrailingCharacter($string, $character) {
      if (strlen($string) > 0) {
         if (substr($string, -1) !== $character) {
            return $string . $character;
         } else {
            return $string;
        }
      } else {
        return $character;
      }
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function GetTempFileName ($name = '', $extension = '', $dir = '') { 
      // Funcion centralizada para la obtencion de un nombre de fichero temporal.
      // Este nombre se añadirá la lista de ficheros temporales para ser borrados en el destructor de la clase
      
      global $sugar_config;
      
      $TempFileName = '';
      
      if (!$name)
         $name = create_guid();
         
      if (substr($name, 0, strlen($this->generated_file_prefix)) !== $this->generated_file_prefix)   
         $TempFileName = $this->generated_file_prefix . $name;
      else
         $TempFileName = $name;
      
      if ($extension)
         $TempFileName = $TempFileName . '.' . $extension;

      if (!$dir) {
         //$dir = trim(sys_get_temp_dir());
         //if (!$dir)
         //   $dir = getcwd() . "/". $sugar_config['DHA_templates_dir'];                           
         $dir = getcwd() . "/". $sugar_config['DHA_templates_dir'];       
      }
      $dir = $this->includeTrailingCharacter ($dir, '/');
      
      //empty($dir) and exit(__FUNCTION__.'(): could not get system temp dir');
      //is_dir($dir) or exit(__FUNCTION__."(): \"$dir\" is not a directory");         
      
      $TempFileName = $dir . $TempFileName;
      
      $this->temp_files[] = $TempFileName;
      
      return $TempFileName;
   }      


   ///////////////////////////////////////////////////////////////////////////
   function GenerarInforme () {
      global $mod_strings, $sugar_config;

      // Merge data
      $this->CalculatedFieldsClass->BeforeMergeBlock();  // Evento
      $this->TBS->MergeBlock('a', $this->datos);
      $this->CalculatedFieldsClass->AfterMergeBlock();   // Evento

      // delete comments
      $this->TBS->PlugIn(OPENTBS_DELETE_COMMENTS);

      if ($this->enPDF){

         // Issue found by Bob Caverly with LibreOffice working output dir
         if (!is_windows() && isset($sugar_config['DHA_OpenOffice_HOME']) && $sugar_config['DHA_OpenOffice_HOME']) {   
            $HOME = getenv('HOME');
            $old_HOME = '';
            $new_HOME = $sugar_config['DHA_OpenOffice_HOME'];
            if (file_exists($new_HOME) && is_writable($new_HOME)) {
               $old_HOME = $HOME;
               putenv("HOME={$new_HOME}");
            }
         }      

         if (isset($sugar_config['DHA_OpenOffice_exe']) && $sugar_config['DHA_OpenOffice_exe'] && file_exists($sugar_config['DHA_OpenOffice_exe']) && is_readable($sugar_config['DHA_OpenOffice_exe'])){

            $file_name = $this->GetTempFileName('', $this->template_file_ext, '');
            $info = pathinfo($file_name);
            $out_dir = $info['dirname'];
            $file_name_pdf = $this->GetTempFileName($info['filename'], 'pdf', $info['dirname']);                   

            $this->TBS->Show(OPENTBS_FILE, $file_name);   

            $pdf_command = '"'.$sugar_config['DHA_OpenOffice_exe'].'" --headless --nologo --nofirststartwizard --convert-to pdf "'.$file_name.'" --outdir "'.$out_dir.'"';
            // Run exec only if is allowed
            if (function_exists('exec') && !in_array('exec', explode(',', ini_get('disable_functions')))) {
               exec($pdf_command, $return_output, $return_var);
            }

            if ($this->Download) {
               $this->DescargarInforme ($file_name_pdf);
            }
            else {
               $this->Download_filename = $file_name_pdf;
               $this->Download_mimetype = $this->GetMimeTypeFromFileName($this->Download_filename);
            }            

            if (!is_windows() && isset($old_HOME) && $old_HOME) {   
               putenv("HOME={$old_HOME}");
            }            
         }
         elseif (!is_windows() && isset($sugar_config['DHA_OpenOffice_cde']) && $sugar_config['DHA_OpenOffice_cde'] && file_exists($sugar_config['DHA_OpenOffice_cde']) && is_readable($sugar_config['DHA_OpenOffice_cde'])){            

            // Recordar que el cde se ejecutara dentro de un sandbox, con lo cual el /home que se le pasa al comando no es el del sistema, sino el del paquete cde del libreoffice
            // En cambio, si se decidiera poner la salida en /tmp, este directorio si que seria el del sistema y no el del paquete (ver en options del paquete cde "ignore_prefix=/tmp/" y "ignore_exact=/tmp" )

            // De momento se desactiva la generacion de pdf con el cde para Hojas de calculo. Habría que ampliar el paquete cde para permitirlo (ahora mismo no funciona, y no se considera necesario que funcione).
            if ($this->is_SpreadSheet) {
               if (!is_windows() && isset($old_HOME) && $old_HOME) {   
                  putenv("HOME={$old_HOME}");
               }

               dha_die ('Can not export Spreadsheets to PDF with cde package');
            }

            $info = pathinfo($sugar_config['DHA_OpenOffice_cde']);
            $out_dir = $info['dirname'] . '/cde-root/home';

            $file_name = $this->GetTempFileName('', $this->template_file_ext, $out_dir);
            $info = pathinfo($file_name);
            $file_name_sandbox = $info['basename'];
            $file_name_pdf = $this->GetTempFileName($info['filename'], 'pdf', $out_dir);

            $this->TBS->Show(OPENTBS_FILE, $file_name);

            $GLOBALS['log']->debug('PDF - Path env before: ' . getenv("PATH") ); 
            $env_path = $_ENV["PATH"];
            $env_path_parts = explode(':', $env_path);
            if (!in_array('/usr/local/sbin', $env_path_parts))
               $env_path .= ':/usr/local/sbin';
            if (!in_array('/usr/local/bin', $env_path_parts))
               $env_path .= ':/usr/local/bin';
            if (!in_array('/sbin', $env_path_parts))
               $env_path .= ':/sbin';
            if (!in_array('/bin', $env_path_parts))
               $env_path .= ':/bin';
            if (!in_array('/usr/sbin', $env_path_parts))
               $env_path .= ':/usr/sbin';
            if (!in_array('/usr/bin', $env_path_parts))
               $env_path .= ':/usr/bin';
            putenv("PATH=" .$env_path);
            $GLOBALS['log']->debug('PDF - Path env after: ' . getenv("PATH") );             

            $pdf_command = $sugar_config['DHA_OpenOffice_cde'].' --headless --nologo --nofirststartwizard --convert-to pdf /home/'.$file_name_sandbox.' --outdir /home';
            // Run exec only if is allowed
            if (function_exists('exec') && !in_array('exec', explode(',', ini_get('disable_functions')))) {
               exec($pdf_command, $return_output, $return_var);
            }

            $GLOBALS['log']->debug('PDF - file_name: ' . $file_name );              
            $GLOBALS['log']->debug('PDF - file_name_sandbox: ' . $file_name_sandbox );         
            $GLOBALS['log']->debug('PDF - file_name_pdf: ' . $file_name_pdf );         
            $GLOBALS['log']->debug('PDF - pdf_command: ' . $pdf_command );              
            $GLOBALS['log']->debug('PDF - pdf_command - return_output: ' . print_r($return_output, true) );   
            $GLOBALS['log']->debug('PDF - pdf_command - return_var: ' . $return_var );            

            if ($this->Download) {
               $this->DescargarInforme ($file_name_pdf);
            }
            else {
               $this->Download_filename = $file_name_pdf;
               $this->Download_mimetype = $this->GetMimeTypeFromFileName($this->Download_filename);
            }            

            if (!is_windows() && isset($old_HOME) && $old_HOME) {   
               putenv("HOME={$old_HOME}");
            }            
         }         
         else {         
            if (!is_windows() && isset($old_HOME) && $old_HOME) {   
               putenv("HOME={$old_HOME}");
            }

            sugar_die($mod_strings['MSG_ERROR_NO_SE_ENCUENTRA_EXE_OPENOFFICE']);
         }
      }
      else {
         if ($this->Download) {
            $this->TBS->Show(OPENTBS_DOWNLOAD, $this->template_filename);
         }
         else {
            $file_name = $this->GetTempFileName('', $this->template_file_ext, '');
            $this->TBS->Show(OPENTBS_FILE, $file_name);
            $this->Download_filename = $file_name;
            $this->Download_mimetype = $this->GetMimeTypeFromFileName($this->Download_filename);
         }         
      }

      $this->CalculatedFieldsClass->AfterShow();  // Evento
   }


   ///////////////////////////////////////////////////////////////////////////
   function DescargarInforme ($file_name) {
      // El parametro $file_name debe pasarse con el path completo

      $base_name = basename($file_name);
      
      ini_set('zlib.output_compression','Off');

      if(isset($_SERVER['HTTP_USER_AGENT']) && preg_match("/MSIE/", $_SERVER['HTTP_USER_AGENT'])) {
         $base_name = urlencode($base_name);
         $base_name = str_replace("+", "_", $base_name);
      }

      header("Pragma: public");
      header("Cache-Control: maxage=1, post-check=0, pre-check=0");
      header("Content-Type: application/force-download");
      header("Content-type: application/octet-stream");
      header("Content-Disposition: attachment; filename=\"".$base_name."\";");
      // disable content type sniffing in MSIE
      header("X-Content-Type-Options: nosniff");
      header("Content-Length: " . filesize($file_name));
      header("Expires: 0");

      @ob_end_clean();
      ob_start();
      readfile($file_name);
      @ob_flush();
   }
   
   ///////////////////////////////////////////////////////////////////////////
   function FormateaCampo ($valor_sin_formatear, &$definicion_campo, $formatear_fechas) {   
      if ($this->is_Document) {
         return $this->FormatDocumentField($valor_sin_formatear, $definicion_campo, $formatear_fechas);
      }
      
      if ($this->is_SpreadSheet) {
         return $this->FormatSpreadSheetField($valor_sin_formatear, $definicion_campo, $formatear_fechas);
      }
   }  

   ///////////////////////////////////////////////////////////////////////////
   function FormatSpreadSheetField ($valor_sin_formatear, &$definicion_campo, $formatear_fechas) {

      require_once('modules/DHA_PlantillasDocumentos/librerias/dharma_utils.php');
      global $current_user, $timedate, $mod_strings, $app_strings, $app_list_strings, $current_language, $sugar_config;
      static $boolean_false_values = array('off', 'false', '0', 'no');

      $valor_formateado = $valor_sin_formatear;
      $tipo = $definicion_campo['type'];
      $varchar_type = false;
      
      switch($tipo) {
         case 'varchar':
         case 'text':
         case 'longtext':
         case 'name':
         case 'username':
         case 'fullname':
         case 'user_name':
         case 'relate':
            $varchar_type = true;
            break;
            
         case 'date':
            if ($valor_sin_formatear == 'NULL' || empty($valor_sin_formatear)) {
               $valor_formateado = '';
               break;
            } 
            
            if($formatear_fechas) {
               null;
            } else {
               $valor_sin_formatear = $timedate->swap_formats($valor_sin_formatear, $this->userDateFormat, $timedate->dbDayFormat);            
            }
            
            if ($this->isOfficeOpenXML) {
               $date_default_timezone = date_default_timezone_get();
               date_default_timezone_set('UTC');
            }
            
            $fecha = explode("-", $valor_sin_formatear);
            $valor_formateado = mktime(0,0,0,$fecha[1],$fecha[2],$fecha[0]);
            
            if ($this->isOfficeOpenXML) {
               date_default_timezone_set($date_default_timezone);
            }
            
            break;
            
         case 'datetime':
         case 'datetimecombo':
            if ($valor_sin_formatear == 'NULL' || empty($valor_sin_formatear)) {
               $valor_formateado = '';
               break;
            }     
            
            if($formatear_fechas) {
               // Handle Offset (en BD se guarda en UTC). Otra forma de hacerlo en lugar de usar $timedate->handle_offset         
               $gmtTimezone = new DateTimeZone("UTC");
               $usertimezone = new DateTimeZone($this->userTZ);
               $dateobj = new SugarDateTime($valor_sin_formatear, $gmtTimezone);
               $dateobj->setTimezone($usertimezone);
               //$valor_formateado = $dateobj->getTimestamp();
               //$valor_formateado = (int)$dateobj->format("U");  // equivale a getTimestamp(), necesario en versiones anteriores a PHP 5.3. El problema es que el timestamp es siempre en UTC, no se puede usar directamente
               $valor_sin_formatear = $dateobj->format($timedate->get_db_date_time_format());
            }
            else {
               $valor_sin_formatear = $timedate->swap_formats($valor_sin_formatear, $this->userDateTimeFormat, $timedate->get_db_date_time_format());
            }

            // Ver explicacion en el tipo Date (arriba)
            if ($this->isOfficeOpenXML) {            
               $date_default_timezone = date_default_timezone_get();
               date_default_timezone_set('UTC');
            }
            
            $temp = explode(" ", $valor_sin_formatear);
            $fecha = explode("-", $temp[0]);
            $hora = explode(":", $temp[1]);
            $valor_formateado = mktime($hora[0],$hora[1],$hora[2],$fecha[1],$fecha[2],$fecha[0]);
            
            if ($this->isOfficeOpenXML) {
               date_default_timezone_set($date_default_timezone);
            }
            
            break;
            
         case 'enum':
         case 'radioenum':
         // STIC-Custom 20220103 AAM - Adding dynamicenum field type for translating item values into their labels
         // STIC#530
         case 'dynamicenum':
         // END STIC
            $valor_formateado = '';
            if($valor_sin_formatear != "")
               $valor_formateado = $app_list_strings[$definicion_campo['options']][$valor_sin_formatear];
            $varchar_type = true;   
            break;
            
         case 'multienum':
            $valor_formateado = '';
            if($valor_sin_formatear == "^^" || empty($valor_sin_formatear)) {
               break;            
            }
            if($valor_sin_formatear != ""){
               $valor_sin_formatear = unencodeMultienum($valor_sin_formatear); // esto devuelve un array
               foreach($valor_sin_formatear as $val){
                  if ($val) {
                     $val = $app_list_strings[$definicion_campo['options']][$val];
                     $valor_formateado = dha_strconcat ($valor_formateado, $val, ', ');
                  }
               }
            }
            $varchar_type = true;
            break;
            
         case 'double':
         case 'decimal':
         case 'currency':
         case 'float':
            if ($this->isOpenDocument) { 
               $valor_formateado = (float) $valor_sin_formatear;
               break;               
            }
         
            if ($valor_sin_formatear === '' || $valor_sin_formatear == NULL || $valor_sin_formatear == 'NULL') {
               $valor_formateado = '';
               break;
            }         
            $valor_formateado = (float) $valor_sin_formatear;
            break;
            
         case 'integer':
         case 'uint':
         case 'ulong':
         case 'long':
         case 'short':
         case 'tinyint':
         case 'bigint':
         case 'int':
            if ($this->isOpenDocument) { 
               $valor_formateado = (integer) $valor_sin_formatear;
               break;               
            }
            
            if ($valor_sin_formatear === '' || $valor_sin_formatear == NULL || $valor_sin_formatear == 'NULL') {
               $valor_formateado = '';
               break;
            }         
            $valor_formateado = (integer) $valor_sin_formatear;
            break;
           
         case 'bool':
            if (empty($valor_sin_formatear)) {
               $valor_formateado = false; //''; 
            } else if(true === $valor_sin_formatear || 1 == $valor_sin_formatear) {
               $valor_formateado = true;
            } else if(in_array(strval($valor_sin_formatear), $boolean_false_values)) {
               $valor_formateado = false;
            } else {
               $valor_formateado = true;
            }
            break;
            
         case 'parent_type':
            if ($valor_sin_formatear && isset($app_list_strings['moduleList'][$valor_sin_formatear])) {
               $valor_formateado = $app_list_strings['moduleList'][$valor_sin_formatear];
            }
            $varchar_type = true;
            break;         
      } 

      if ($varchar_type) {
         $valor_formateado = html_entity_decode($valor_formateado, ENT_QUOTES);
      }

      return $valor_formateado;   
   }   
   

   ///////////////////////////////////////////////////////////////////////////
   function FormatDocumentField ($valor_sin_formatear, &$definicion_campo, $formatear_fechas) {
      // Nota: Los beans de los campos relacionados vienen formateados (al menos las fechas), mientras que el bean principal no
      // Ver la funcion fixUpFormatting

      require_once('modules/DHA_PlantillasDocumentos/librerias/dharma_utils.php');
      global $current_user, $timedate, $mod_strings, $app_strings, $app_list_strings, $current_language, $sugar_config;
      static $boolean_false_values = array('off', 'false', '0', 'no');

      $valor_formateado = $valor_sin_formatear;
      $tipo = $definicion_campo['type'];
      $varchar_type = false;
      
      switch($tipo) {
         case 'varchar':
         case 'text':
         case 'longtext':
         case 'name':
         case 'username':
         case 'fullname':
         case 'user_name':
         case 'relate':
            $varchar_type = true;
            break;
            
         case 'date':
            if ($valor_sin_formatear == 'NULL' || empty($valor_sin_formatear)) {
               $valor_formateado = '';
               break;
            }         
            // if($formatear_fechas)
               // $valor_formateado = $timedate->swap_formats($valor_sin_formatear, $timedate->dbDayFormat, $this->userDateFormat);
            if($formatear_fechas) {
               $valor_formateado = $timedate->swap_formats($valor_sin_formatear, $timedate->dbDayFormat, $this->idiomaDateFormat);
            } else {
               $valor_formateado = $timedate->swap_formats($valor_sin_formatear, $this->userDateFormat, $this->idiomaDateFormat);            
            }
            break;
            
         case 'datetime':
         case 'datetimecombo':
            if ($valor_sin_formatear == 'NULL' || empty($valor_sin_formatear)) {
               $valor_formateado = '';
               break;
            }         
            // if($formatear_fechas) {
               // $valor_sin_formatear = $timedate->handle_offset($valor_sin_formatear, $timedate->get_db_date_time_format(), true, null, $this->userTZ);
               // $valor_formateado = $timedate->swap_formats($valor_sin_formatear, $timedate->get_db_date_time_format(), $this->userDateTimeFormat);
            // }
            if($formatear_fechas) {
               $valor_sin_formatear = $timedate->handle_offset($valor_sin_formatear, $timedate->get_db_date_time_format(), true, null, $this->userTZ);
               $valor_formateado = $timedate->swap_formats($valor_sin_formatear, $timedate->get_db_date_time_format(), $this->idiomaDateTimeFormat);
            } else {
               $valor_formateado = $timedate->swap_formats($valor_sin_formatear, $this->userDateTimeFormat, $this->idiomaDateTimeFormat);
            }            
            break;
            
         case 'enum':
         case 'radioenum':
         // STIC-Custom 20220103 AAM - Adding dynamicenum field type for translating item values into their labels
         // STIC#530
         case 'dynamicenum':
         // END STIC
            $valor_formateado = '';
            if($valor_sin_formatear != "")
               $valor_formateado = $app_list_strings[$definicion_campo['options']][$valor_sin_formatear];
            $varchar_type = true; 
            break;
            
         case 'multienum':
            $valor_formateado = '';
            if($valor_sin_formatear == "^^" || empty($valor_sin_formatear)) {
               break;            
            }
            if($valor_sin_formatear != ""){
               $valor_sin_formatear = unencodeMultienum($valor_sin_formatear); // esto devuelve un array
               foreach($valor_sin_formatear as $val){
                  if ($val) {
                     $val = $app_list_strings[$definicion_campo['options']][$val];
                     $valor_formateado = dha_strconcat ($valor_formateado, $val, ', ');
                  }
               }                
            }
            $varchar_type = true; 
            break;            
            
         case 'double':
         case 'decimal':
         case 'currency':
         case 'float':
            if ($valor_sin_formatear === '' || $valor_sin_formatear == NULL || $valor_sin_formatear == 'NULL') {
               $valor_formateado = '';
               break;
            }         
            $valor_formateado = $this->FormatearNumero($valor_sin_formatear, false);
            break;
          
         case 'integer':
         case 'uint':
         case 'ulong':
         case 'long':
         case 'short':
         case 'tinyint':
         case 'int':
            if ($valor_sin_formatear === '' || $valor_sin_formatear == NULL || $valor_sin_formatear == 'NULL') {
               $valor_formateado = '';
               break;
            }         
            $valor_formateado = $this->FormatearNumero($valor_sin_formatear, true);
            break;
           
         case 'bool':
            if (empty($valor_sin_formatear)) {
               $valor_formateado = ''; 
            } else if(true === $valor_sin_formatear || 1 == $valor_sin_formatear) {
               $valor_formateado = $this->idiomaBool_1; //$app_strings['LBL_YES'];
            } else if(in_array(strval($valor_sin_formatear), $boolean_false_values)) {
               $valor_formateado = $this->idiomaBool_0; //$app_strings['LBL_NO'];
            } else {
               $valor_formateado = $this->idiomaBool_1;  //$app_strings['LBL_YES'];
            }
            break;
            
         case 'parent_type':
            if ($valor_sin_formatear && isset($app_list_strings['moduleList'][$valor_sin_formatear])) {
               $valor_formateado = $app_list_strings['moduleList'][$valor_sin_formatear];
            }
            $varchar_type = true;
            break;         
      }  

      if ($varchar_type) {
         $valor_formateado = html_entity_decode($valor_formateado, ENT_QUOTES);
      }
      
      return $valor_formateado;
   }
   
   ///////////////////////////////////////////////////////////////////////////
   function currency($currency_id){
      
      $returnValue = null;
      
      if (isset($this->currencies_cache[$currency_id])) {
         $returnValue = $this->currencies_cache[$currency_id];
      }
      else {
         $curr_obj = new Currency();
         $curr_obj->retrieve($currency_id);
         
         $returnValue = array();
         $returnValue['name'] = $curr_obj->name;
         $returnValue['symbol'] = $curr_obj->symbol;
         $returnValue['iso4217'] = $curr_obj->iso4217;
         $returnValue['conversion_rate'] = $curr_obj->conversion_rate;
         
         $this->currencies_cache[$currency_id] = $returnValue;
         
         unset ($curr_obj);
      }
      
      return $returnValue;
   }     
   
   ///////////////////////////////////////////////////////////////////////////
   function translate($string, $modulo = ''){
      
      $returnValue = '';
      
      if ($modulo) {
         if (!isset($this->mod_strings_cache[$modulo])) {
            global $current_language;
            $this->mod_strings_cache[$modulo] = return_module_language($current_language, $modulo);
         }
               
         if(isset($this->mod_strings_cache[$modulo][$string])) {
            $returnValue = $this->mod_strings_cache[$modulo][$string];
         }
      }
      
      if(empty($returnValue)){
         global $app_strings, $app_list_strings;

         if(isset($app_strings[$string]))
            $returnValue = $app_strings[$string];
         else if(isset($app_list_strings[$string]))
            $returnValue = $app_list_strings[$string];
      }

      $returnValue = trim($returnValue);
      
      if(empty($returnValue)){
         return $string;
      }
      
      if (substr($returnValue, -1, 1) == ':')
         $returnValue = substr($returnValue, 0, -1);

      return $returnValue;
   }  
   
   ///////////////////////////////////////////////////////////////////////////
   function ImagenTipoDato ($tipodato) {      
      
      // Aqui solo hay que especificar los tipos de datos especiales, o aquellos que no tienen icono asociado
      
      if ($tipodato == 'name' || $tipodato == 'username' || $tipodato == 'fullname' || $tipodato == 'user_name')
         $tipodato = 'varchar';
      elseif ($tipodato == 'double' || $tipodato == 'decimal')
         $tipodato = 'float';    
      elseif ($tipodato == 'datetimecombo')
         $tipodato = 'datetime';   
      elseif ($tipodato == 'calculated_date')
         $tipodato = 'date';           
      elseif ($tipodato == 'integer' || $tipodato == 'uint' || $tipodato == 'ulong' || $tipodato == 'long' || $tipodato == 'short' || $tipodato == 'tinyint')
         $tipodato = 'int'; 
      elseif ($tipodato == 'parent')
         $tipodato = 'relate';
      elseif ($tipodato == 'assigned_user_name')
         $tipodato = 'id';   
      elseif ($tipodato == 'image_path')
         $tipodato = 'image';  
      elseif ($tipodato == 'longtext')
         $tipodato = 'text';          
         
      
      if (!$imagen_tipo = SugarThemeRegistry::current()->getImage('icon_'.$tipodato.'_16', '', null, null, '.png'))
         $imagen_tipo = SugarThemeRegistry::current()->getImage('icon_generic_16', '', null, null, '.png'); 
      return $imagen_tipo;         
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function TipoDatoValido ($tipodato, $nombrevariable) {
      $OK = true;  
      
      if ($tipodato == 'link' || $tipodato == 'id' || $tipodato == 'assigned_user_name' || $tipodato == 'iframe' || $tipodato == 'download' || 
          $tipodato == 'encrypt' || $tipodato == 'password' || $tipodato == 'image' || $tipodato == 'collection' || $tipodato == 'function')
         $OK = false;   

      elseif (in_array($nombrevariable, array('deleted', 'currency_name', 'currency_symbol', 'password', 'user_hash', 'pwd_last_changed', 'system_generated_password', 'external_auth_only', 'authenticate_id')))
         $OK = false;          
      
      return $OK;
   }  
   
   ///////////////////////////////////////////////////////////////////////////
   function CampoLinkValido ($NombreCampoLink, &$PropiedadesCampoLink) {
      // Nota: Los campos de relacion que se envian aqui siempre son del modulo de la plantilla, con lo cual hay que mirar sobre $this->modulo
      $OK = true; 

      if (isset($PropiedadesCampoLink['reportable']) && !$PropiedadesCampoLink['reportable']) {
         $OK = false;    
      }      
      
      return $OK;
   }        
   
   ///////////////////////////////////////////////////////////////////////////
   function ModuloValido ($modulo) {
      // Aqui se indica si un modulo es valido para poder obtener relaciones del mismo. Esta funcion no cortará el proceso (si se necesitar cortar el proceso, usar ModuloConPermiso)
      
      $OK = true; 
      
      if (empty($modulo)) {
         $OK = false;        
      }      
      
      // if (isset($this->acl_modules[$modulo]) && $this->acl_modules[$modulo]['module']['access']['aclaccess'] < ACL_ALLOW_ENABLED) {
         // $OK = false;       
      // }

      // ¿deberia de controlarse tambien el permiso "export"?
      if( ACLController::moduleSupportsACL($modulo) && !ACLController::checkAccess($modulo, 'view', true)){
         $OK = false;     
      }        

      if ($OK && in_array($modulo, array("EAPM", "acl_fields"))) {
         $OK = false;       
      }       
      
      return $OK;
   }    

   ///////////////////////////////////////////////////////////////////////////
   function ModuloConPermiso ($modulo) {
      // Aqui se indica si se tiene permiso para acceder a un módulo determinado. Si no es asi, se corta la ejecucion del proceso (si se necesitar no cortar el proceso, usar ModuloValido)
      
      $OK = true; 
      
      // ¿deberia de controlarse tambien el permiso "export"?
      if( ACLController::moduleSupportsACL($modulo) && !ACLController::checkAccess($modulo, 'view', true)){
         $OK = false;     
         sugar_die($this->translate('LBL_MODULE') . ' "' . $this->translate($modulo) . '". ' . $this->translate('LBL_NO_ACCESS', 'ACL'));
      }        
      
      return $OK;
   }       


   ///////////////////////////////////////////////////////////////////////////
   private function ObtenerHtmlListaVariables_modulo ($modulo, $bean, $titulo_adicional, $nivel_tabla, $nombre_campo_relacion, $EsRelacionDeSubPanel) {
      // Esta funcion se usa para la funcion ObtenerHtmlListaVariables
      
      if (!$bean)
         return '';      
      
      global $app_list_strings, $mod_strings;
      require_once('modules/DHA_PlantillasDocumentos/librerias/dharma_utils.php');
      
      if ($nivel_tabla > 0) {
         $style_display_div = 'none';
         $style_display_toggle_expand = '';
         $nombre_div = 'VarListDiv_' . $nombre_campo_relacion;
      } 
      else {
         $style_display_div = '';
         $style_display_toggle_expand = 'none';
         $nombre_div = 'VarListDiv_Main';
      }

      
      $html = '';
      
      $html .= SugarThemeRegistry::current()->getImage('toggle-expand', "id=\"{$nombre_div}-expand\" style=\"cursor:pointer;cursor:hand;display:{$style_display_toggle_expand}\" onclick=\"javascript:toggleVarListVisibility('{$nombre_div}', true);\"", null, null, '.png');
      $html .= SugarThemeRegistry::current()->getImage('toggle-shrink', "id=\"{$nombre_div}-shrink\" style=\"cursor:pointer;cursor:hand;display:{$style_display_div}\" onclick=\"javascript:toggleVarListVisibility('{$nombre_div}', false);\"", null, null, '.png');
      
      $titulo_modulo = $modulo; 
      if (isset($app_list_strings['moduleList'][$modulo]))
         $titulo_modulo = $app_list_strings['moduleList'][$modulo];
      
      //$html .= getClassicModuleTitle($modulo, array($titulo_modulo, $titulo_adicional), false);  // En la versión 6.5.0 la funcion original no va bien
      $html .= dha_getClassicModuleTitle($modulo, array($titulo_modulo, $titulo_adicional), false);
      
      $html .= <<<EOQ
      <div id="{$nombre_div}" style="clear:left; display:{$style_display_div}">
         <table width="100%" id="{$nombre_div}_table" class="tablesorter">
            <thead>  
               <tr>       
                  <th width="1%"><input type='checkbox' class='SelectAllModuleCheckBoxes' value='$nombre_div'></th>   
                  <th width="25%">{$mod_strings['LBL_ETIQUETA']}</th>               
                  <th width="20%">{$mod_strings['LBL_CAMPO']}</th>
                  <th width="15%">{$mod_strings['LBL_TIPO']}</th>
                  <th width="39%">{$mod_strings['LBL_DESCRIPTION']}</th>
               </tr>                  
            </thead>
            <tbody>
EOQ;
   
   
      $separador_campo_relacion = '';
      $modulo_relacion = '';
      if ($nombre_campo_relacion) {
         $separador_campo_relacion = $this->separador_campo_relacion;
         $modulo_relacion = $modulo;
      }
      
      // Vardefs de los campos calculados
      if ($nivel_tabla > 0) {
         $CalculatedFieldsClass = $this->GetCalculatedFieldsClass($modulo, $bean, $nombre_campo_relacion);
         if (!is_null($CalculatedFieldsClass)) {
            $CalculatedFieldsClass->UpdateBeanFieldNameMap();
            unset($CalculatedFieldsClass);
         }
      } 
      else {
         if (!is_null($this->CalculatedFieldsClass)) {
            $this->CalculatedFieldsClass->UpdateBeanFieldNameMap(); // (aunque esta llamada ya se ha realizado en el __construct, se repite aqui para que actue la funcion UndefFieldsDefs, ya que se ha visto que ciertos campos se vuelven a cargar, curiosamente en la funcion ModuloConPermiso, parece que son aquellos que en la propiedad studio se indica que solo son para el DetailView)
         }
      }

      // $field_defs = $bean->getFieldDefinitions();
      
      $cuenta = 0;
      foreach($bean->field_defs as $campo){
         
         $nombre_campo = $campo['name'];
         $tipo = '';
         if (isset($campo['type']))
            $tipo = $campo['type'];
         
         if ($this->TipoDatoValido($tipo, $nombre_campo)){    
         
            $etiqueta = $nombre_campo;
            if (isset($campo['vname']))
               $etiqueta = $this->translate ($campo['vname'], $modulo);
               
            if ($nivel_tabla == 0 && isset($campo['link']) && $campo['link']){
               $link_imagen = $this->ImagenTipoDato('relate');
               $etiqueta = "<a href='#VarListDiv_{$campo['link']}_name'>{$etiqueta} {$link_imagen}</a>";
            } elseif ($nivel_tabla == 0 && isset($campo['ext2']) && $campo['ext2'] && !isset($campo['link']) ){
               $link_imagen = $this->ImagenTipoDato('relate');
               $etiqueta = "<a href='#VarListDiv_{$campo['name']}_name'>{$etiqueta} {$link_imagen}</a>";   
            } elseif ($tipo == 'relate' || $tipo == 'parent') {
               $link_imagen = $this->ImagenTipoDato('relate');
               $etiqueta = $etiqueta . $link_imagen;
            }
               
            $comment = '';
            if (isset($campo['comment']))
               $comment = $campo['comment'] . '. ';
               
            // $comments = '';
            // if (isset($campo['comments']))
               // $comments = $campo['comments'];
               
            $help = '';
            if (isset($campo['help']))
               $help = $campo['help'];
               
            $descripcion = $comment . ' ' . $help;  // Nota: "comment" es una propiedad interna de documentacion, y "help" es una propiedad de ayuda en el editview al pasar el raton por encima.
            
            $imagen_tipo = $this->ImagenTipoDato($tipo);
            
            $nombre_campo_compuesto = $nombre_campo;
            if ($nivel_tabla > 0 && !$EsRelacionDeSubPanel)
               $nombre_campo_compuesto = $nombre_campo_relacion . $this->separador_campo_related . $nombre_campo;
               
            if (isset($campo['calculated_MMR']) && $campo['calculated_MMR'])
               $nombre_campo = $nombre_campo . '&nbsp;&nbsp;&nbsp;' . $this->ImagenTipoDato('calculated');        

            $html .= "<tr>"; 
            $html .= "<td valign='top' style='text-align: center; vertical-align: middle;'><input type='checkbox' class='fieldselector' name='CamposSeleccionados[]' value='{$nombre_campo_relacion}{$separador_campo_relacion}{$modulo_relacion}{$separador_campo_relacion}{$nombre_campo_compuesto}'></td>";
            $html .= "<td valign='top'>{$etiqueta}</td>";             
            $html .= "<td valign='top'><b>{$nombre_campo}</b></td>";             
            $html .= "<td valign='top'>{$imagen_tipo} {$tipo}</td>";        
            $html .= "<td valign='top'>{$descripcion}</td>";        
            $html .= "</tr>";

            $cuenta += 1;
         }
      }
      
      $html .= <<<EOQ
            </tbody> 
         </table>
      </div>
EOQ;

      // Enlace con el componente TableSorter
      $html .= '<script type="text/javascript">$("#'.$nombre_div.'_table").tablesorter({sortList: [[1,0]], widgets: ["zebra"], headers: {0: {sorter: false }, 4: {sorter: false }}}); </script>';

      $ancho_separador = $nivel_tabla * 7;
      $ancho_div = 100 - $ancho_separador;
      
      $html = <<<EOQ
      <br>
      <a class='tablesorter_anchor' name='{$nombre_div}_name'>
      <div>
         <table width="100%">
            <thead>  
               <tr>       
                  <th width="{$ancho_separador}%"></th>
                  <th width="{$ancho_div}%"></th>
               </tr>                  
            </thead>
            <tbody>
               <tr>
                  <td valign='top'></td>
                  <td valign='top'>{$html}</td>
               </tr>
            </tbody> 
         </table>
      </div>
      </a>
EOQ;

      if ($cuenta == 0)
         $html = '';
      
      return $html; 
   }   
   

   ///////////////////////////////////////////////////////////////////////////
   function ObtenerHtmlListaVariables () {
   
      if (!$this->ModuloConPermiso ($this->modulo)) 
         return false;
         
      global $app_list_strings, $mod_strings, $sugar_config;      
      
      $html = <<<EOJS
<script>
   function toggleVarListVisibility(element_id, visible){
      if (visible) {
         document.getElementById(element_id).style.display = '';
         document.getElementById(element_id + '-expand').style.display = 'none';
         document.getElementById(element_id + '-shrink').style.display = '';
         
         $('table').trigger('applyWidgetId', ['zebra']);
      } else {
         document.getElementById(element_id).style.display = 'none';
         document.getElementById(element_id + '-expand').style.display = '';
         document.getElementById(element_id + '-shrink').style.display = 'none';         
      }
   }
</script>
EOJS;

      // CAMPOS MODULO PRINCIPAL
      $html .= $this->ObtenerHtmlListaVariables_modulo ($this->modulo, $this->bean_datos, '', 0, '', false);
      
      // MODULOS RELACIONADOS
      $linked_fields = $this->bean_datos->get_linked_fields();
      
      $linked_fields_CamposDeSubpanel = array();
      $linked_fields_CamposRelated = array();
      
      foreach ($linked_fields as $name => $properties) {
         if ($this->CampoLinkValido($name, $properties) && $this->bean_datos->load_relationship($name)) {
            $LinkObject = $this->bean_datos->$name;
            if ($this->EsRelacionDeSubPanel($name, $LinkObject)) {
               $linked_fields_CamposDeSubpanel[$name] = $properties;
               $linked_fields_CamposDeSubpanel[$name]['have_link_field'] = true;
            } else {
               $linked_fields_CamposRelated[$name] = $properties;
               $linked_fields_CamposRelated[$name]['have_link_field'] = true;
            }
         }
      }
      
      // Modulos relacionados a través de campos que no usan link, sino la propiedad ext2. Por ejemplo, los campos related creados con el estudio
      $fieldDefs = $this->bean_datos->getFieldDefinitions();
      if (!empty($fieldDefs)) {
         foreach ($fieldDefs as $name => $properties) {
            if ($properties['type'] == 'relate' && isset($properties['ext2']) && (array_search('link', $properties) !== 'type')) {
               $linked_fields_CamposRelated[$name] = $properties;
               $linked_fields_CamposRelated[$name]['have_link_field'] = false;
            }
         }
      }
      
      
      unset($linked_fields);
      $linked_fields = array_merge($linked_fields_CamposRelated, $linked_fields_CamposDeSubpanel);  // esto es para que aparezcan primero los campos de tipo related
      
      $inicio_bloque_campos_related = false;
      $fin_bloque_campos_related = false;
      $inicio_bloque_campos_related_gestionado = false;
      $fin_bloque_campos_related_gestionado = false;      
      
      foreach ($linked_fields as $name => $properties) {

         if ($properties['have_link_field']) {
            $LinkObject = $this->bean_datos->$name;
            $modulo_relacionado = $LinkObject->getRelatedModuleName();            
         }
         else {
            $LinkObject = null;
            $modulo_relacionado = $properties['ext2'];
         }
         
         if ($this->ModuloValido($modulo_relacionado)) {
            $modulo_relacionado_bean = SugarModule::get($modulo_relacionado)->loadBean();
            $tipoRelacionReal = $this->TipoRelacionReal($LinkObject);
            $EsRelacionDeSubPanel = $this->EsRelacionDeSubPanel($name, $LinkObject);
            
            $titulo_adicional = '<span style="color:CornflowerBlue;"><i>['. $name . ']</i></span>';
            
            if ($EsRelacionDeSubPanel) {
               if (!$fin_bloque_campos_related_gestionado)
                  $fin_bloque_campos_related = true;
               $nivel = 1;

               $etiqueta = $this->EtiquetaLink ($name, $properties, $LinkObject);
               if ($etiqueta) {
                  $titulo_adicional = $etiqueta . '  ' .  $titulo_adicional;
               }               

               if ($tipoRelacionReal == "many-to-many")
                  $titulo_adicional = $titulo_adicional . '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:red;">(many-to-many)</span>';

            } else {
               if (!$inicio_bloque_campos_related_gestionado)
                  $inicio_bloque_campos_related = true;
               $nivel = 1;
               
               $etiqueta = $this->EtiquetaLink ($name, $properties, $LinkObject);
               if ($etiqueta) {
                  $titulo_adicional = $etiqueta . '  ' .  $titulo_adicional;
               } 

               $marca_campo_ext2 = '';
               if (!$properties['have_link_field']) {
                  $marca_campo_ext2 = ' *';
               }                
               
               $titulo_adicional = $titulo_adicional . '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:red;">(relate'.$marca_campo_ext2.' '.$this->ImagenTipoDato('relate').')</span>';
          
            }              
               

            // Nota: recordar que los modulos relacionados se han ordenado para que aparezcan primero los de tipo relate   
            if ($inicio_bloque_campos_related) { 
               $html .= '<div style="border-bottom:1px solid;border-top:2px solid;color: #000000;background-color: #f6f6f6;border-bottom-color: #abc3d7 !important;border-top-color: #4e8ccf !important;">';
               $inicio_bloque_campos_related = false;
               $inicio_bloque_campos_related_gestionado = true;
            } elseif ($fin_bloque_campos_related && $inicio_bloque_campos_related_gestionado) { 
               $html .= '</div>';
               $fin_bloque_campos_related = false;
               $fin_bloque_campos_related_gestionado = true;
            }            
            
            
            $html .= $this->ObtenerHtmlListaVariables_modulo ($modulo_relacionado, $modulo_relacionado_bean, $titulo_adicional, $nivel, $name, $EsRelacionDeSubPanel);

            unset($modulo_relacionado_bean);
         }
      }
      

      if ( (ACLController::moduleSupportsACL('DHA_PlantillasDocumentos') && ACLController::checkAccess('DHA_PlantillasDocumentos', 'edit', true)) || !ACLController::moduleSupportsACL('DHA_PlantillasDocumentos') ){
         $nombre_plantilla = $app_list_strings['moduleList'][$this->modulo] . ' ' . date($sugar_config['datef']. ' ' . $sugar_config['default_time_format']);
         
         $formatos = '';
         foreach ($app_list_strings['dha_plantillasdocumentos_formatos_ficheros_dom'] as $key => $value) {
            $selected = "";
            if ($key == 'DOCX')
                 $selected = "selected='selected'";
            $formatos .= "<option label='$value' value='$key' $selected>$value</option>";
         }  

         $idiomas = '';
         foreach ($app_list_strings['dha_plantillasdocumentos_idiomas_dom'] as $key => $value) {
            $selected = "";
            if ($key == $this->idiomaDefecto)
                 $selected = "selected='selected'";
            $idiomas .= "<option label='$value' value='$key' $selected>$value</option>";
         }           

         $html = <<<EOF
            <form name="CrearPlantillaBasica" action="index.php?module=DHA_PlantillasDocumentos&action=crearplantillabasica" method="post">
               {$html}
               <br>
               <table width="100%" border="0" cellspacing="1" cellpadding="0" class="edit view">
                  <tbody>      
                     <tr>
                        <td valign="top" id="nombre_plantilla_label" width="7%" scope="row">{$mod_strings['LBL_NAME']}</td>
                        <td valign="top" width="37.5%">
                           <input type="text" style="width: 300px;" id="nombrePlantilla" name="nombrePlantilla" value="{$nombre_plantilla}" maxlength="255">
                        </td>
                     </tr>
                     
                     <tr>
                        <td valign="top" id="formato_plantilla_label" width="7%" scope="row">{$mod_strings['LBL_FORMATO_PLANTILLA']}</td>
                        <td valign="top" width="37.5%">
                           <select style="width: 300px;" name="formatoPlantilla" id="formatoPlantilla">{$formatos}</select>   
                        </td>
                     </tr>
                     
                     <tr>
                        <td valign="top" id="idioma_plantilla_label" width="7%" scope="row">{$mod_strings['LBL_IDIOMA_PLANTILLA']}</td>
                        <td valign="top" width="37.5%">
                           <select style="width: 300px;" name="idiomaPlantilla" id="idiomaPlantilla">{$idiomas}</select>   
                        </td>
                     </tr>                     
                     
                  </tbody> 
               </table>            
               <input type="hidden" value="{$this->modulo}" id="moduloPlantilla" name="moduloPlantilla">
               <input type="submit" value="{$mod_strings['LBL_GENERAR_PLANTILLA_BASICA']}"> 
            </form>
EOF;
      }
      

      $html .= <<<EOJS
<script>

   $(".SelectAllModuleCheckBoxes").click(function(){
      var div_id=this.value;
      $("#" + div_id + " INPUT[type='checkbox']").attr('checked',this.checked);
      
      $("#" + div_id + " .fieldselector").each(function(index, value) {
         if ($(this).is(':checked')) {
            $(this).parent().parent().addClass('selected');
         }
         else {
            $(this).parent().parent().removeClass('selected');
         }      
      });      
   });
   
   $(".fieldselector").click(function(){
      if ($(this).is(':checked')) {
         $(this).parent().parent().addClass('selected');
      }
      else {
         $(this).parent().parent().removeClass('selected');
      }
   });
   
</script>
EOJS;
      
      echo $html; 
   } 


   ///////////////////////////////////////////////////////////////////////////
   function EtiquetaLink ($LinkFieldName, $LinkFieldProperties, $LinkObject) { 
      // A partir de la version 6.3 se empieza a usar la clase Link2 en lugar de la Link
      
      $etiqueta = '';
      
      // Campos sin link (que usan ext2)
      if (is_null($LinkObject)) {
         return $this->translate ($LinkFieldProperties['vname'], $this->modulo);
      }
      
      if (get_class($LinkObject) == 'Link') {
      
         $RelationshipObject = $LinkObject->getRelationshipObject();
         $EsRelacionDeSubPanel = $this->EsRelacionDeSubPanel($LinkFieldName, $LinkObject);         
         $bean_is_lhs = $LinkObject->_get_bean_position();
         
         if (!$EsRelacionDeSubPanel) {
            foreach ($this->bean_datos->field_defs as $field_name => $field_properties) {   
               if ($field_properties['type'] == 'relate' && isset($field_properties['link']) && $field_properties['link'] == $LinkFieldName) {
                  $etiqueta = $this->translate ($field_properties['vname'], $this->modulo);
                  break;
               }
            }              
         }     
         
         
         if (!$etiqueta && isset($LinkFieldProperties['vname'])) {
            $etiqueta = $this->translate ($LinkFieldProperties['vname'], $this->modulo);
            if ($etiqueta == $LinkFieldProperties['vname']) {
               $etiqueta = '';            
            }            
         }            
         
      } elseif (get_class($LinkObject) == 'Link2') {
      
         $RelationshipObject = $LinkObject->getRelationshipObject();
         $side = $LinkObject->getSide();                  
         $EsRelacionDeSubPanel = $this->EsRelacionDeSubPanel($LinkObject->name, $LinkObject);
         
         if ($side == REL_RHS) {  
            $campo_relacionado = $RelationshipObject->rhs_key;
            $modulo_relacionado = $RelationshipObject->def['rhs_module'];
            $LinkDef = $RelationshipObject->rhsLinkDef;
         } else {
            $campo_relacionado = $RelationshipObject->lhs_key;
            $modulo_relacionado = $RelationshipObject->def['lhs_module'];
            $LinkDef = $RelationshipObject->lhsLinkDef;
         }          

         if (!$EsRelacionDeSubPanel) {
            foreach ($this->bean_datos->field_defs as $field_name => $field_properties) {   
               if ($field_properties['type'] == 'relate' && isset($field_properties['link']) && $field_properties['link'] == $LinkObject->name) {
                  $etiqueta = $this->translate ($field_properties['vname'], $this->modulo);
                  break;
               }
            }            
         }

         if (!$etiqueta && isset($LinkDef['vname'])) {
            $etiqueta = $this->translate ($LinkDef['vname'], $modulo_relacionado);
            if ($etiqueta == $LinkDef['vname']) {
               $etiqueta = '';            
            }            
         }         

      }

      return $etiqueta;      
   }
   
   ///////////////////////////////////////////////////////////////////////////
   function TipoRelacionReal ($LinkObject) {
      // A partir de la version 6.3 se empieza a usar la clase Link2 en lugar de la Link

      // Campos sin link (que usan ext2). En este caso, y por ahora asumiremos que son de tipo one-to-many (campos related). Ya se extenderá si es necesario
      if (is_null($LinkObject)) {
         return "one-to-many";
      }      
      
      if (get_class($LinkObject) == 'Link') {
         
         $RelationshipObject = $LinkObject->getRelationshipObject();
         $tipoRelacion = $RelationshipObject->relationship_type;
         
         if ($tipoRelacion == "many-to-many" &&
             !empty($GLOBALS['dictionary'][$RelationshipObject->relationship_name]) &&
             !empty($GLOBALS['dictionary'][$RelationshipObject->relationship_name]['true_relationship_type']) ) {
             
                $tipoRelacion = $GLOBALS['dictionary'][$RelationshipObject->relationship_name]['true_relationship_type'];
         }             
         
         return $tipoRelacion;

      } elseif (get_class($LinkObject) == 'Link2') {
      
         $RelationshipObject = $LinkObject->getRelationshipObject();
         $tipoRelacion = $RelationshipObject->relationship_type;
         $nombreClase = get_class($RelationshipObject);
         
         if ($nombreClase == 'One2MRelationship' || $nombreClase == 'One2MBeanRelationship')
            $tipoRelacionReal = "one-to-many";
         elseif ($nombreClase == 'M2MRelationship' || $nombreClase == 'EmailAddressRelationship')
            $tipoRelacionReal = "many-to-many";    
         elseif ($nombreClase == 'One2OneRelationship' || $nombreClase == 'One2OneBeanRelationship')
            $tipoRelacionReal = "one-to-one";  
         else
            $tipoRelacionReal = $tipoRelacion;
         
         return $tipoRelacionReal;
      }         
   }   


   ///////////////////////////////////////////////////////////////////////////
   function EsRelacionDeSubPanel ($LinkFieldName, $LinkObject) {
      // En la funcion CampoLinkValido se desestiman los links que tengan la propiedad "reportable" a false
      // A partir de la version 6.3 se empieza a usar la clase Link2 en lugar de la Link

      // Campos sin link (que usan ext2)
      if (is_null($LinkObject)) {
         return false;
      }
      
      if (get_class($LinkObject) == 'Link') {
         
         $tipoRelacionReal = $this->TipoRelacionReal($LinkObject);    
      
         if ($tipoRelacionReal == "one-to-one") {
            return false;               
         } elseif ($tipoRelacionReal == "many-to-many") {
            return true;         
         } elseif ($tipoRelacionReal == "one-to-many") {
            if ($LinkObject->_get_bean_position()) {  
               return true;
            } else {
               return false;
            } 
         }

      } elseif (get_class($LinkObject) == 'Link2') { 
         
         $side = $LinkObject->getSide();         
         $tipoRelacionReal = $this->TipoRelacionReal($LinkObject);
            
         if ($tipoRelacionReal == "one-to-one") {
            return false;               
         } elseif ($tipoRelacionReal == "many-to-many") {
            return true;         
         } elseif ($tipoRelacionReal == "one-to-many") {
            if ($side == REL_RHS) {  
               return false;
            } else {
               return true;
            } 
         }            
         
      }
      
      return false;
   }   

   ///////////////////////////////////////////////////////////////////////////
   function ObtenerDatos () {
      // Es necesario haber llamado previamente a CargarPlantilla antes de llamar a esta función
      // En esta funcion se rellena la estructura $this->datos, que es la que se usa para rellenar la plantilla. Ver tambien la funcion CargarPlantilla
      
      if (!$this->ModuloConPermiso ($this->modulo))
         return false;           
      
      global $current_user, $db;
      
      // CARGA DE RELACIONES 
      // ------------------------------------
      
      $linked_fields_temp = $this->bean_datos->get_linked_fields();
      $fieldDefs = $this->bean_datos->getFieldDefinitions();

      // solo se cargan las relaciones que se piden en la plantilla, para ello en la funcion CargarPlantilla se leen de la plantilla los registros relacionados que se piden
      $linked_fields = array();
      $linked_fields_beans_names = array();
      $linked_fields_modulo = array();

      foreach ($linked_fields_temp as $name => $properties) {
         if (in_array($name, $this->relaciones)) {  
            if ($this->CampoLinkValido($name, $properties) && $this->bean_datos->load_relationship($name)) {
               $linked_fields[] = $name;
               $Link = $this->bean_datos->$name;
               $modulo_relacionado = $Link->getRelatedModuleName();
               
               if ($this->ModuloConPermiso($modulo_relacionado)) {
                  $modulo_relacionado_bean = SugarModule::get($modulo_relacionado)->loadBean();
                  $linked_fields_beans_names[] = $modulo_relacionado_bean->object_name;
                  $linked_fields_modulo[] = $modulo_relacionado;
                  unset($modulo_relacionado_bean);
               }
            }
         }
      }
      
      // solo se cargan las relaciones de campos related que se piden en la plantilla, para ello en la funcion CargarPlantilla se leen de la plantilla los registros relacionados que se piden
      $linked_related_fields = array();
      $linked_related_fields_beans_names = array();
      $linked_related_fields_modulo = array();
      $linked_related_fields_are_link_field = array();

      foreach ($linked_fields_temp as $name => $properties) {
         if (in_array($name, $this->relaciones_related)) {  
            if ($this->CampoLinkValido($name, $properties) && $this->bean_datos->load_relationship($name)) {
               $linked_related_fields[] = $name;
               $linked_related_fields_are_link_field[] = true;
               $Link = $this->bean_datos->$name;
               $modulo_relacionado = $Link->getRelatedModuleName();
               
               if ($this->ModuloConPermiso($modulo_relacionado)) {
                  $modulo_relacionado_bean = SugarModule::get($modulo_relacionado)->loadBean();
                  $linked_related_fields_beans_names[] = $modulo_relacionado_bean->object_name;
                  $linked_related_fields_modulo[] = $modulo_relacionado;
                  unset($modulo_relacionado_bean);
               }
            }
         }
      } 

      // Campos related de tipo ext2 (sin link asociado)      
      if (!empty($fieldDefs)) {
         foreach ($fieldDefs as $name => $properties) {
            if ($properties['type'] == 'relate' && isset($properties['ext2']) && (array_search('link', $properties) !== 'type') && in_array($name, $this->relaciones_related)) {
               $linked_related_fields[] = $name;
               $linked_related_fields_are_link_field[] = false;
               $modulo_relacionado = $properties['ext2'];
               
               if ($this->ModuloConPermiso($modulo_relacionado)) {
                  $modulo_relacionado_bean = SugarModule::get($modulo_relacionado)->loadBean();
                  $linked_related_fields_beans_names[] = $modulo_relacionado_bean->object_name;
                  $linked_related_fields_modulo[] = $modulo_relacionado;
                  unset($modulo_relacionado_bean);
               }
            }
         }
      }

      
      // Reordenacion de los ids si se necesita
      if (!is_null($this->CalculatedFieldsClass)) {
         $this->CalculatedFieldsClass->OrderRows();
      }      
      
      // OBTENCION DE LOS DATOS
      // ------------------------------------      

      $cuenta = 0;
      foreach($this->ids as $id){
         
         // DATOS DEL MODULO PRINCIPAL
         $this->bean_datos->retrieve($id);
         
         // comprobamos los permisios para poder ver el registro
         if (!$this->bean_datos->ACLAccess('view')){
            continue;
         }
         
         // comprobamos aqui si realmente se quiere mostrar el registro (a traves de la clase de campos calculados del modulo se pueden ocultar ciertos registros)
         $mostrar_registro = true;
         if (!is_null($this->CalculatedFieldsClass)) {
            $mostrar_registro = $this->CalculatedFieldsClass->ShowRow();
         }
         if (!$mostrar_registro){
            continue;
         }         
         
         $cuenta += 1;
         $this->datos[$cuenta] = array(); 
         
         $this->bean_datos->fixUpFormatting();  // En la declaracion de esta funcion se dice que será borrada del codigo en una version futura !!!
         
         // Campos calculados
         if (!is_null($this->CalculatedFieldsClass)) {
            $this->CalculatedFieldsClass->CalcFields();
         }         
         
         foreach($this->bean_datos->field_defs as $campo){
            $nombre_campo = $campo['name'];
            $tipo = '';
            if (isset($campo['type']))
               $tipo = $campo['type'];
            
            if ($this->TipoDatoValido($tipo, $nombre_campo)){
               $this->datos[$cuenta][$nombre_campo] = '';
               if (isset($this->bean_datos->$nombre_campo)){
                  $this->datos[$cuenta][$nombre_campo] = $this->FormateaCampo($this->bean_datos->$nombre_campo, $campo, true);
               }

               // $etiqueta = $nombre_campo;
               // if (isset($campo['vname']))
                  // $etiqueta = $this->translate ($campo['vname'], $this->modulo);                  
            }
         }
         
         
         
         // DATOS DE LOS MODULOS RELACIONADOS CON CAMPOS DE TIPO RELATED
         foreach ($linked_related_fields as $key => $related_field_name) {
         
            $modulo_relacionado = $linked_related_fields_modulo[$key];
            if ($linked_related_fields_are_link_field[$key]) {
               $related_beans_temp = $this->bean_datos->get_linked_beans($related_field_name, $linked_related_fields_beans_names[$key]); // solo devolverá un registro aqui
            } 
            else {
               // Obtenemos el registro relacionado (en este caso será único) de un campo related de tipo ext2
               $related_beans_temp = array();
               if ($fieldDefs[$related_field_name]['id_name']) {
                  $id_name = $fieldDefs[$related_field_name]['id_name'];
                  $id_value = $this->bean_datos->$id_name;
                  if ($id_value) {
                     $related_beans_temp[0] = SugarModule::get($modulo_relacionado)->loadBean();
                     $related_beans_temp[0]->retrieve($id_value);
                  }
               }
            }
            
            // comprobamos los permisos para poder ver el registro y componemos el array definitivo con todos los beans que puede ver el usuario activo
            if (isset($related_beans))
               unset($related_beans);
            $related_beans = array();
            foreach($related_beans_temp as $key => $related_bean){
               if (!$related_bean->ACLAccess('view')){
                  continue;
               } 
               $related_beans[] = $related_bean;
            }
            unset($related_beans_temp);
            
            // Primero comprobamos si el conjunto está vacío (hay que tenerlo en cuenta o saltará error)            
            if (empty($related_beans)) {
               $modulo_bean = SugarModule::get($modulo_relacionado)->loadBean();
               
               // Campos calculados
               $CalculatedFieldsClass = $this->GetCalculatedFieldsClass($modulo_relacionado, $modulo_bean, $related_field_name);
               if (!is_null($CalculatedFieldsClass)) {
                  $CalculatedFieldsClass->UpdateBeanFieldNameMap();
                  unset($CalculatedFieldsClass);
               }
               
               foreach($modulo_bean->field_defs as $campo){
                  $nombre_campo = $campo['name'];
                  $nombre_campo_TBS = $related_field_name . $this->separador_campo_related . $campo['name'];
                  $tipo = '';
                  if (isset($campo['type']))
                     $tipo = $campo['type'];
            
                  if ($this->TipoDatoValido($tipo, $nombre_campo)){
                     $this->datos[$cuenta][$nombre_campo_TBS] = '';
                     // if (isset($campo['vname']))
                        // $etiqueta = $this->translate ($campo['vname'], $modulo_relacionado);
                     // else
                        // $etiqueta = $nombre_campo;
                  }
               }               
            }
            else {
               foreach($related_beans as $key => $related_bean){
               
                  // Campos calculados
                  $CalculatedFieldsClass = $this->GetCalculatedFieldsClass($modulo_relacionado, $related_bean, $related_field_name);
                  if (!is_null($CalculatedFieldsClass)) {
                     $CalculatedFieldsClass->UpdateBeanFieldNameMap();
                     $CalculatedFieldsClass->CalcFields();
                     unset($CalculatedFieldsClass);
                  }
                  
                  foreach($related_bean->field_defs as $campo){
                     $nombre_campo = $campo['name'];
                     $nombre_campo_TBS = $related_field_name . $this->separador_campo_related . $campo['name'];
                     $tipo = '';
                     if (isset($campo['type']))
                        $tipo = $campo['type'];
               
                     if ($this->TipoDatoValido($tipo, $nombre_campo)){
                        $this->datos[$cuenta][$nombre_campo_TBS] = '';
                        if (isset($related_bean->$nombre_campo)){
                           $this->datos[$cuenta][$nombre_campo_TBS] = $this->FormateaCampo($related_bean->$nombre_campo, $campo, false);
                        }

                        // if (isset($campo['vname']))
                           // $etiqueta = $this->translate ($campo['vname'], $modulo_relacionado);
                        // else
                           // $etiqueta = $nombre_campo;
                     }
                  }
               }
            }
            unset($related_beans);
         }     

         
         
         // DATOS DE LOS MODULOS RELACIONADOS
         foreach ($linked_fields as $key => $related_field_name) {
            $related_beans_temp = $this->bean_datos->get_linked_beans($related_field_name, $linked_fields_beans_names[$key]);
            $modulo_relacionado = $linked_fields_modulo[$key];
            
            // comprobamos los permisos para poder ver el registro y componemos el array definitivo con todos los beans que puede ver el usuario activo
            if (isset($related_beans))
               unset($related_beans);
            $related_beans = array();
            foreach($related_beans_temp as $key => $related_bean){
               if (!$related_bean->ACLAccess('view')){
                  continue;
               }

               // Parche para tabla de Grupos en SuiteCRM
               // En esta tabla no se está marcando el deleted = 1 como es habitual, sino que se muestra el grupo si todas sus
               // lineas están con deleted = 0 (ver en modules/AOS_Products_Quotes/Line_Items.php la sql usada para llamar a insertLineItems).
               // Se trataría de un bug, puesto que al borrar un grupo, debería de poner tambien como deleted = 1 al grupo, y no solo
               // a las lineas que componen ese grupo
               if ($modulo_relacionado == 'AOS_Line_Item_Groups'){
                  $aos_group_line_count = 0;
                  $sql = "SELECT COUNT(1) AS count FROM aos_products_quotes WHERE group_id = '{$related_bean->id}' AND deleted = 0 ";
                  $dataset = $db->query($sql);
                  while ($row = $db->fetchByAssoc($dataset)) {
                      $aos_group_line_count = $row['count'];
                  }
                  if ($aos_group_line_count == 0){
                      continue;
                  }
               }               

               // comprobamos aqui si realmente se quiere mostrar el registro (a traves de la clase de campos calculados del modulo se pueden ocultar ciertos registros)
               $mostrar_registro = true;
               $CalculatedFieldsClass = $this->GetCalculatedFieldsClass($modulo_relacionado, $related_bean, $related_field_name);
               if (!is_null($CalculatedFieldsClass)) {
                  $mostrar_registro = $CalculatedFieldsClass->ShowRow();
                  unset($CalculatedFieldsClass);
               }
               if (!$mostrar_registro){
                  continue;
               }                
               
               $related_beans[] = $related_bean;
            }
            unset($related_beans_temp);            
            
            
            // Primero comprobamos si el conjunto está vacío (hay que tenerlo en cuenta o saltará error)            
            if (empty($related_beans)) {
               //$this->datos[$cuenta][$related_field_name][] = array();  // asi da errores cuando viene vacío
               $this->datos[$cuenta][$related_field_name] = null;
            }
            else {
               foreach($related_beans as $key => $related_bean){
               
                  // Campos calculados
                  $CalculatedFieldsClass = $this->GetCalculatedFieldsClass($modulo_relacionado, $related_bean, $related_field_name);
                  if (!is_null($CalculatedFieldsClass)) {
                     $CalculatedFieldsClass->UpdateBeanFieldNameMap();
                     $CalculatedFieldsClass->CalcFields();
                     unset($CalculatedFieldsClass);
                  }
                  
                  $datos_relacionados = array();
                  $datos_relacionados['id'] = $related_bean->id;  // este dato lo añadimos manualmente, porque la funcion TipoDatoValido impedirá que se añada el id del registro
                  foreach($related_bean->field_defs as $campo){
                     $nombre_campo = $campo['name'];
                     $tipo = '';
                     if (isset($campo['type']))
                        $tipo = $campo['type'];
               
                     if ($this->TipoDatoValido($tipo, $nombre_campo)){
                        $datos_relacionados[$nombre_campo] = '';
                        if (isset($related_bean->$nombre_campo)){
                           $datos_relacionados[$nombre_campo] = $this->FormateaCampo($related_bean->$nombre_campo, $campo, false);
                        }

                        // if (isset($campo['vname']))
                           // $etiqueta = $this->translate ($campo['vname'], $modulo_relacionado);
                        // else
                           // $etiqueta = $nombre_campo;
                     }
                  }
                  $this->datos[$cuenta][$related_field_name][] = $datos_relacionados;
               }
            }
            unset($related_beans);
         }
               
      }
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function BloqueFormatoFichero ($formato, $nombre_bloque) {
      $returnValue = '';
      if ($formato == 'docx' || $formato == 'docm') {
         if ($nombre_bloque == 'parrafo' || $nombre_bloque == 'paragraph') 
            $returnValue = 'w:p';
         elseif ($nombre_bloque == 'linea_tabla' || $nombre_bloque == 'table_row')
            $returnValue = 'w:tr';
         elseif ($nombre_bloque == 'celda_tabla' || $nombre_bloque == 'table_cell')
            $returnValue = 'w:tc';                
         elseif ($nombre_bloque == 'tabla' || $nombre_bloque == 'table')
            $returnValue = 'w:tbl';
         elseif ($nombre_bloque == 'linea_lista' || $nombre_bloque == 'list_item') 
            $returnValue = 'w:p';            
         elseif ($nombre_bloque == 'cuerpo' || $nombre_bloque == 'body')
            $returnValue = 'w:body';
      } 
      elseif ($formato == 'odt') {
         if ($nombre_bloque == 'parrafo' || $nombre_bloque == 'paragraph')
            $returnValue = 'text:p';
         elseif ($nombre_bloque == 'linea_tabla' || $nombre_bloque == 'table_row')
            $returnValue = 'table:table-row';
         elseif ($nombre_bloque == 'celda_tabla' || $nombre_bloque == 'table_cell')
            $returnValue = 'table:table-cell';             
         elseif ($nombre_bloque == 'tabla' || $nombre_bloque == 'table')
            $returnValue = 'table:table';
         elseif ($nombre_bloque == 'linea_lista' || $nombre_bloque == 'list_item') 
            $returnValue = 'text:list-item';            
         elseif ($nombre_bloque == 'cuerpo' || $nombre_bloque == 'body')
            $returnValue = 'office:body';
      }
      elseif ($formato == 'ods') {
         if ($nombre_bloque == 'linea' || $nombre_bloque == 'row')
            $returnValue = 'table:table-row';    //'tbs:row'
         elseif ($nombre_bloque == 'celda' || $nombre_bloque == 'cell')
            $returnValue = 'table:table-cell';   //'tbs:cell'
      } 
      elseif ($formato == 'xlsx' || $formato == 'xlsm') {
         if ($nombre_bloque == 'linea' || $nombre_bloque == 'row')
            $returnValue = 'row';  //'tbs:row'
         elseif ($nombre_bloque == 'celda' || $nombre_bloque == 'cell')
            $returnValue = 'c';    //'tbs:cell'
      }      
      
      return $returnValue;
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function SpreadSheetFieldType (&$definicion_campo) { 
      $type = '';
      $field_type = $definicion_campo['type'];
      
      if (isset($definicion_campo['options']) && $definicion_campo['options'] == 'dha_boolean_list') {
         $field_type = 'bool';
      }      
      
      if ($this->is_SpreadSheet && $field_type) {
         $field_type = strtolower($field_type);
         
         switch($field_type) {
            case 'date':
            case 'datetime':
            case 'datetimecombo':
               $type = 'tbs:date';
               break;

            case 'double':
            case 'decimal':
            case 'currency':
            case 'float':
            case 'integer':
            case 'uint':
            case 'ulong':
            case 'long':
            case 'short':
            case 'tinyint':
            case 'int':
               $type = 'tbs:num';
               break;
              
            case 'bool':
               $type = 'tbs:bool';
               break;
         }
      }
      
      return $type;
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function CrearPlantillaBasica () { 
      // Nota: esta funcion se llama exclusivamente desde el post que se puede realizar en la ventana de la lista de variables
      // Ver modules\DHA_PlantillasDocumentos\controller.php la funcion action_crearplantillabasica, desde ahi se llama a esta funcion (el modulo de la plantilla ya se asigna en esa llamada, en el constructor de la clase). Ver tambien la funcion ObtenerHtmlListaVariables y ObtenerHtmlListaVariables_modulo, en esta misma clase, que es donde se construye el formulario que se envia aqui
      
      if ( (ACLController::moduleSupportsACL('DHA_PlantillasDocumentos') && !ACLController::checkAccess('DHA_PlantillasDocumentos', 'edit', true)) ) 
         return false;       
         
      if (!$this->ModuloConPermiso ($this->modulo))
         return false;             
         
      require_once('modules/DHA_PlantillasDocumentos/librerias/dharma_utils.php');
      global $app_list_strings, $mod_strings, $current_user, $db, $sugar_config;       
      
      // RECOGEMOS LOS CAMPOS Y RELACIONES SELECCIONADAS
      $CamposTemp = $_REQUEST['CamposSeleccionados'];
      $CamposDeRelacion = array();
      $Relaciones = array();
      $Modulos = array();
      $Campos = array();
      $cuenta_relaciones = 1;
      foreach ($CamposTemp as $Campo) {
         if (strpos($Campo, (string) $this->separador_campo_relacion) === false) {
            $Campos[] = $Campo;
         }
         else {
            $temp = explode($this->separador_campo_relacion, $Campo);
            $nombre_campo_relacion = $temp[0];
            $nombre_modulo = $temp[1];
            $nombre_campo = $temp[2];
            
            if (strpos($nombre_campo, (string) $this->separador_campo_related) === false) {
               $CamposDeRelacion[$nombre_campo_relacion][] = $nombre_campo;
               if (!in_array($nombre_campo_relacion, $Relaciones)) {
                  $Relaciones['sub'.$cuenta_relaciones] = $nombre_campo_relacion;
                  $Modulos[$nombre_campo_relacion] = $nombre_modulo;
                  $cuenta_relaciones += 1;
               }               
            }
            else {
               $Campos[] = $nombre_campo;
               $Modulos[$nombre_campo] = $nombre_modulo;
            }
         }
      }
      
      if(empty($Campos) && empty($CamposDeRelacion)) {
         sugar_die($mod_strings['MSG_ERROR_GENERAR_PLANTILLA_BASICA_NO_SELECCIONADO_NINGUN_CAMPO']);
      }
      
      
      // RECOGEMOS EL RESTO DE VARIABLES NECESARIAS PARA CREAR UN REGISTRO DE PLANTILLA
      $NombrePlantilla = $_REQUEST['nombrePlantilla'];
      $IdiomaPlantilla = $_REQUEST['idiomaPlantilla']; //$this->idiomaDefecto; //$this->idioma;
      $EstadoPlantilla = 'Draft'; //'BORRADOR';
      
      
      // GUARDAMOS EL REGISTRO DE LA PLANTILLA
      $bean_plantilla = SugarModule::get('DHA_PlantillasDocumentos')->loadBean();
      $bean_plantilla->id = create_guid();
      $bean_plantilla->new_with_id = true;   
      $bean_plantilla->document_name = $NombrePlantilla;      
      $bean_plantilla->modulo = $this->modulo; // tambien podría ser $_REQUEST['moduloPlantilla']
      $bean_plantilla->status_id = $EstadoPlantilla;     
      $bean_plantilla->idioma = $IdiomaPlantilla;
      $bean_plantilla->assigned_user_id = $current_user->id;
      $bean_plantilla->save();
      $bean_plantilla->new_with_id = false;  
      
      $extension = strtolower($_REQUEST['formatoPlantilla']); //'docx', 'odt', 'ods', 'xlsx', 'xlsm' o 'docm'
      $filename = $bean_plantilla->id . '.' . $extension;
      $this->AsignarVariablesTipoPlantilla($extension);
      
      
      
      // VARIABLES PARA LA PLANTILLA
      // Nota: en las variables de la plantilla que se vayan a sustituir a su vez por otra variable se ha tenido que poner el parametro "protect=no" para que internamente no sustituya el caracter "[" por "&#91;"
      
      $subbloques = '';
      foreach ($Relaciones as $key => $Relacion) {
         $subbloques = dha_strconcat ($subbloques, $key . '=' . $Relacion, ';');
      }
      if ($subbloques)
         $subbloques = ';' . $subbloques;
      
      // En hojas de calculo no es posible mostrar datos de subpaneles
      if ($this->is_SpreadSheet) {
         $subbloques = '';
      }   
      
      if ($this->is_Document){
         $GLOBALS["declaracion_bloques_plantilla"] = "[a;block={$this->BloqueFormatoFichero($extension, 'body')}{$subbloques}]";
      }
      if ($this->is_SpreadSheet) {
         $GLOBALS["declaracion_bloques_plantilla"] = "[a;block=tbs:row{$subbloques}]";
      }
      $GLOBALS["titulo_plantilla"] = $app_list_strings['moduleList'][$this->modulo];
      $GLOBALS["etiqueta_modulo"] = $app_list_strings['moduleList'][$this->modulo];
      
      $a = array();
      $at = array(); // etiquetas de campos para excel
      $cuenta = 0;
      foreach ($Campos as $Campo) {
         $SpreadSheetFieldType = '';
         if (strpos($Campo, (string) $this->separador_campo_related) === false) {
            $etiqueta = $Campo;
            if (isset($this->bean_datos->field_defs[$Campo]['vname'])) {
               $etiqueta = $this->translate ($this->bean_datos->field_defs[$Campo]['vname'], $this->modulo);
               $SpreadSheetFieldType = $this->SpreadSheetFieldType($this->bean_datos->field_defs[$Campo]);
            }
            $a[$cuenta]['etiqueta_modulo'] = '';  // este valor ya está en GLOBALS
         }
         else {
            // Variables de modulos de campos relate
            $temp = explode($this->separador_campo_related, $Campo);
            $nombre_campo_relacion = $temp[0];
            $nombre_campo = $temp[1];  
            $modulo = $Modulos[$Campo];
            $modulo_bean = SugarModule::get($modulo)->loadBean();
            
            // Campos calculados
            $CalculatedFieldsClass = $this->GetCalculatedFieldsClass($modulo, $modulo_bean, $nombre_campo_relacion);
            if (!is_null($CalculatedFieldsClass)) {
               $CalculatedFieldsClass->UpdateBeanFieldNameMap();
               unset($CalculatedFieldsClass);    
            }            
            
            $etiqueta = $nombre_campo;
            if (isset($modulo_bean->field_defs[$nombre_campo]['vname'])) {
               $etiqueta = $this->translate ($modulo_bean->field_defs[$nombre_campo]['vname'], $modulo);            
               $SpreadSheetFieldType = $this->SpreadSheetFieldType($modulo_bean->field_defs[$nombre_campo]);
            }
            unset($modulo_bean);
            
            if (isset($this->bean_datos->field_defs[$nombre_campo_relacion]['vname'])) {
               $etiqueta = $this->translate ($this->bean_datos->field_defs[$nombre_campo_relacion]['vname'], $this->modulo) . ' - ' .  $etiqueta;
               if (!$SpreadSheetFieldType)
                  $SpreadSheetFieldType = $this->SpreadSheetFieldType($this->bean_datos->field_defs[$nombre_campo_relacion]);
            }
            else {
               $etiqueta = $nombre_campo_relacion . ' - ' .  $etiqueta;
            }
            
            $a[$cuenta]['etiqueta_modulo'] = '(' . $app_list_strings['moduleList'][$modulo] . ')';
         }

         if ($this->is_SpreadSheet && $SpreadSheetFieldType) {
            $SpreadSheetFieldType = ';ope=' . $SpreadSheetFieldType;
         }
         else {
            $SpreadSheetFieldType = '';
         }         

         $a[$cuenta]['etiqueta_campo'] = $etiqueta;
         $a[$cuenta]['variable_campo'] = "[a.{$Campo}{$SpreadSheetFieldType}]";
         $at[$cuenta] = $a[$cuenta];
         $cuenta += 1;
      }
      
      
      // Variables de relaciones de subpanel (uno a muchos o muchos a muchos)
      // En hojas de calculo no es posible mostrar datos de subpaneles
      if ($this->is_Document){
         $b = array();    
         $cuenta = 0;      
         foreach ($Relaciones as $key => $Relacion) {
            $modulo = $Modulos[$Relacion];
            $modulo_bean = SugarModule::get($modulo)->loadBean();
            
            // Campos calculados
            $CalculatedFieldsClass = $this->GetCalculatedFieldsClass($modulo, $modulo_bean, $Relacion);
            if (!is_null($CalculatedFieldsClass)) {
               $CalculatedFieldsClass->UpdateBeanFieldNameMap();
               unset($CalculatedFieldsClass);
            }         
            
            $b[$cuenta]['etiqueta_modulo'] = $app_list_strings['moduleList'][$modulo];
            $b[$cuenta]['bloque_no_data'] = "[a_{$key};block={$this->BloqueFormatoFichero($extension, 'parrafo')};nodata]No hay datos para a_{$key}";
            
            $cuenta_2 = 0;
            foreach ($CamposDeRelacion[$Relacion] as $Campo) {
               $etiqueta = $Campo;
               if (isset($modulo_bean->field_defs[$Campo]['vname'])) 
                  $etiqueta = $this->translate ($modulo_bean->field_defs[$Campo]['vname'], $modulo);
                  
               $bloque = '';
               if ($cuenta_2 == 0)
                  $bloque = "[a_{$key};block={$this->BloqueFormatoFichero($extension, 'tabla')}+{$this->BloqueFormatoFichero($extension, 'parrafo')}]";   
                  
               $b[$cuenta]['campos'][] = array('etiqueta_campo'=>$etiqueta, 'variable_campo'=>"{$bloque}[a_{$key}.{$Campo}]");
               $cuenta_2 += 1;      
            }
            $cuenta += 1;      
         }
      }
      
      
      
      
      
      // GENERAMOS LA PLANTILLA
      if (!isset($this->bean_plantilla))
         $this->bean_plantilla = SugarModule::get('DHA_PlantillasDocumentos')->loadBean();
      $this->bean_plantilla->asegurarDirectorios();
      
      $fichero_plantilla_basica = getcwd() . "/modules/DHA_PlantillasDocumentos/plantilla_basica.". $extension;
      $fichero_plantilla = getcwd() . "/". $sugar_config['DHA_templates_dir'] . $filename;

      require_once ('modules/DHA_PlantillasDocumentos/librerias/TinyButStrong/tbs_class.php');
      require_once ('modules/DHA_PlantillasDocumentos/librerias/OpenTBS/tbs_plugin_opentbs.php');

      $this->TBS = new clsTinyButStrong; // new instance of TBS
      $this->TBS->Plugin(TBS_INSTALL, OPENTBS_PLUGIN); // load OpenTBS plugin
      
      $this->TBS->LoadTemplate($fichero_plantilla_basica, OPENTBS_ALREADY_UTF8);

      $this->TBS->MergeBlock('a', $a);
      if ($this->is_SpreadSheet) {
         $this->TBS->MergeBlock('at', $at);  // solo se usa para excel
      }
      if ($this->is_Document){
         $this->TBS->MergeBlock('b', $b);
      }
      
      $this->TBS->PlugIn(OPENTBS_DELETE_COMMENTS);

      //$this->TBS->Plugin(OPENTBS_DEBUG_XML_CURRENT);  
      $this->TBS->Show(OPENTBS_FILE, $fichero_plantilla);
      //$this->TBS->Plugin(OPENTBS_DEBUG_XML_SHOW);
      


      // ACTUALIZAMOS LOS DATOS DEL REGISTRO CON EL FICHERO CREADO
      $file_ext = $extension;
      $file_mime_type= $this->GetMimeType($file_ext);

      $sql = "UPDATE dha_plantillasdocumentos set filename='{$filename}', file_ext='{$file_ext}', file_mime_type='{$file_mime_type}', uploadfile='{$filename}' WHERE id='{$bean_plantilla->id}' ";
      $db->query($sql);
      
      // REDIRECCIONAMOS AL DETALLE DEL REGISTRO CREADO
      $dURL = "index.php?module=DHA_PlantillasDocumentos&action=DetailView&record={$bean_plantilla->id}";
      SugarApplication::redirect($dURL);  
      
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function f_onformat($FieldName, &$CurrVal, &$CurrPrm, &$TBS) {
      // funcion generica para poder ser usada como evento para el TiniButStrong para el evento "onformat" de los campos
      // para ser usada, se añade a las variables el parametro siguiente (ejemplo "[a.name;onformat=Generate_Document.f_onformat]
      // La funcion llamada por el evento no tiene porque ser de clase como esta, podría ser una funcion normal (ver documentacion)
      // Tambien puede ser usada asi : "[a.name;onformat=~f_onformat]" pero para ello se ha tenido que realizar la siguiente asignacion $this->TBS->ObjectRef = $this;  (ver Object Oriented Programming en la documentacion del TinyButStrong)
      $CurrVal= $CurrVal;
   } 
   
   ///////////////////////////////////////////////////////////////////////////   
   private function tstamptotime($tstamp) {
      // Convierte fecha en formato de base de datos en timestamp
      
      sscanf($tstamp,"%u-%u-%u %u:%u:%u",$year,$month,$day,$hour,$min,$sec);
      $newtstamp = mktime($hour,$min,$sec,$month,$day,$year);
      return $newtstamp;
   }        
   
   ///////////////////////////////////////////////////////////////////////////
   function f_LongDate($FieldName, &$CurrVal, &$CurrPrm, &$TBS) {
      // funcion para formatear fechas a letras (formato largo), para ser usada como parametro onformat. Ver comentarios en la funcion generica f_onformat para su uso
      // Ejemplo de uso:    [a.fecha;onformat=~f_LongDate]
     
      if (!empty($CurrVal)) {
         global $timedate;
         if (strpos($CurrVal, ':') === false) {
            $fecha = $timedate->swap_formats($CurrVal, $this->idiomaDateFormat, $timedate->dbDayFormat) . ' 00:00:00';
         }
         else {
            $fecha = $timedate->swap_formats($CurrVal, $this->idiomaDateTimeFormat, $timedate->get_db_date_time_format());   
         }
         $fecha = $this->tstamptotime ($fecha);
         $CurrVal = $this->FormatearFechaLarga($fecha); 
      }
   }   
   
   ///////////////////////////////////////////////////////////////////////////
   function f_Decimals($FieldName, &$CurrVal, &$CurrPrm, &$TBS) {
      // funcion para formatear números con un número determinado de decimales, para ser usada como parametro onformat. 
      // El parámetro de número de decimales se pasará entre paréntesis a la función
      // Por defecto los números se formatean usando el número de decimales devuelto por $locale->getPrecision()
      // Ver comentarios en la funcion generica f_onformat para su uso
      // Ejemplo de uso:    [a.numvar;onformat=~f_Decimals(4)]
      
      // Importante: Si el número de decimales superara el valor de $locale->getPrecision() se rellenará con ceros, puesto que en este punto no tenemos
      //             el valor real completo del número 

      if (trim($CurrVal) !== '' && isset($CurrPrm['onformat'])) {
         $decimals = $this->decimals;
         
         // Buscamos el numero pasado entre parentesis en el propio parámetro
         if (preg_match('/\(([0-9]+)\)/', $CurrPrm['onformat'], $coincidencias)) {
            $decimals = trim($coincidencias[1]);
            
            $CurrVal = str_replace($this->idiomaThousands_sep, "", $CurrVal);
            $CurrVal = str_replace($this->idiomaDecimal_point, ".", $CurrVal);
            $CurrVal = (float)$CurrVal;
            $CurrVal = $this->FormatearNumeroDecimales($CurrVal, $decimals);
         }
      }
   }     

   ///////////////////////////////////////////////////////////////////////////
   function GetCalculatedFieldsClass($modulo, $bean, $nombre_relacion = '') {
      // Nota: El bean ya debe de estar creado
      $returnClass = null;
      
      $SourceFile = 'modules/' . $modulo . '/DHA_DocumentTemplatesCalculatedFields.php';
      if (file_exists('custom/'. $SourceFile)) {
         $SourceFile = 'custom/'. $SourceFile;
      }

      if(file_exists($SourceFile)) {
         require_once($SourceFile); 
         $c = $modulo . '_DocumentTemplatesCalculatedFields';

         $customClass = 'Custom' . $c;
         if(class_exists($customClass)) {
             $c = $customClass;
         }
         
         if(class_exists($c)) {
            $returnClass = new $c($modulo, $bean);
            $returnClass->relationship_name = $nombre_relacion;            
            $returnClass->SetGenerate_Document_Instance($this);
         }
      }
      else {
         require_once('modules/DHA_PlantillasDocumentos/DHA_DocumentTemplatesCalculatedFields_base.php');
         $returnClass = new DHA_DocumentTemplatesCalculatedFields ($modulo, $bean);
         $returnClass->relationship_name = $nombre_relacion;
         $returnClass->SetGenerate_Document_Instance($this);
      }
      
      return $returnClass;
   }
   
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function FormatearFechaLarga($fecha) {
      // El parámetro $fecha será un timestamp
      
      $mes = (int)date('n', $fecha);
      $mes = $this->idiomaMonth_names[$mes-1];
      
      $formato = str_replace ('F', '\%', $this->idiomaLongDateFormat);  // ver documentacion de la funcion date
      $fecha = date($formato, $fecha);
      $fecha = str_replace ('%', $mes, $fecha);
      
      return $fecha;
   }       

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function FormatearFecha($fecha) {
      // El parámetro $fecha será un timestamp
      
      $fecha = date($this->idiomaDateFormat, $fecha);
      return $fecha;
   }
    

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function FormatearFechaADiaSemana($fecha) {
      // El parámetro $fecha será un timestamp

      $dia_semana = (int)date('N', $fecha);
      $dia_semana = $this->idiomaWeek_days[$dia_semana-1];
      
      return $dia_semana;
   }
   
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function FormatearNumero($numero, $esEntero) {
      // El parámetro $numero se espera en formato php , y $esEntero es un booleano   
      
      $decimales = $this->decimals;
      if ($esEntero)
         $decimales = 0;
      $value = $this->FormatearNumeroDecimales($numero, $decimales);
      return $value;      
   }  

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function FormatearNumeroDecimales($numero, $decimales) {
      // El parámetro $numero se espera en formato php , y $decimales es el número de decimales requerido  
      
      $value = number_format ($numero, $decimales, $this->idiomaDecimal_point, $this->idiomaThousands_sep);
      return $value;      
   }

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function Sugar7Module($module_name) {

      require_once('modules/DHA_PlantillasDocumentos/UI_Hooks.php');
      return MailMergeReports_Sugar7Module($module_name);
   }
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function IsSuiteCRM() {

      require_once('modules/DHA_PlantillasDocumentos/UI_Hooks.php');
      return MailMergeReports_isSuiteCRM();
   }
   
   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function SuiteCRMVersion() {

      require_once('modules/DHA_PlantillasDocumentos/UI_Hooks.php');
      return MailMergeReports_SuiteCRMVersion();
   }
   
}



//////////////////////////////////////////////////////////////////////////////
function getEmailRelationshipFieldName ($modulo){
   $m1 = 'Emails';
   $m2 = $modulo;
   
   global $db, $dictionary, $beanList;
   $rel = new Relationship;
   if($rel_info = $rel->retrieve_by_sides($m1, $m2, $db)){
      $bean = BeanFactory::getBean($m1);
      $rel_name = $rel_info['relationship_name'];
      foreach($bean->field_defs as $field => $def){
         if(isset($def['relationship']) && $def['relationship'] == $rel_name && $def['type'] == 'link') {
            return $def['name'];
         }
      }
   }
   return false;
}

//////////////////////////////////////////////////////////////////////////////
function GenerateDocument ($modulo, $plantilla_id, $ids, $enPDF) {   
   $GD = new Generate_Document($modulo, $plantilla_id, $ids);
   $GD->enPDF = $enPDF;
   $GD->CargarPlantilla ();
   $GD->ObtenerDatos ();
   $GD->GenerarInforme ();
}

//////////////////////////////////////////////////////////////////////////////
function GenerateDocument_Sugar7 ($api, $modulo, $plantilla_id, $ids, $enPDF) {
   $GD = new Generate_Document($modulo, $plantilla_id, $ids);
   $GD->enPDF = $enPDF;
   $GD->Download = false;
   $GD->CargarPlantilla ();
   $GD->ObtenerDatos ();
   $GD->GenerarInforme ();
   
   if ($GD->Download_filename) {
      require_once('include/utils/sugar_file_utils.php');
      
      $filename = $GD->template_filename; 
      if ($GD->enPDF){
         $filename = preg_replace("#\.{$GD->template_file_ext}$#", '.pdf', $filename);
      }
      
      //$content = sugar_file_get_contents($GD->Download_filename);
      $content = file_get_contents($GD->Download_filename);
      
      // Ver \clients\base\api\ExportApi.php
      ob_end_clean();

      $api->setHeader("Pragma", "cache");
      $api->setHeader("Content-Type", "application/octet-stream");
      $api->setHeader("Content-Disposition", "attachment; filename=\"{$filename}\"");
      $api->setHeader("Content-transfer-encoding", "binary");
      $api->setHeader("Expires", "Mon, 26 Jul 1997 05:00:00 GMT");
      $api->setHeader("Last-Modified", TimeDate::httpTime());
      $api->setHeader("Cache-Control", "post-check=0, pre-check=0");         

      return $content;
   }
   else {
      return "";
   }
}

/*
//////////////////////////////////////////////////////////////////////////////
function GenerateDocument_Test () {
    require_once('modules/DHA_PlantillasDocumentos/librerias/TinyButStrong/tbs_class.php');
    require_once('modules/DHA_PlantillasDocumentos/librerias/OpenTBS/tbs_plugin_opentbs.php');

    $data = array();
    $data[1] = array();
    $data[1]['var1'] = 'test var 1';
    $data[1]['var2'] = 'test var 2';

    $TBS = new clsTinyButStrong;
    $TBS->Plugin(TBS_INSTALL, OPENTBS_PLUGIN);
    $TBS->LoadTemplate('C:\sugar\document_templates\test.docx', OPENTBS_ALREADY_UTF8);

    $TBS->MergeBlock('a', $data);
    $TBS->Show(OPENTBS_FILE, 'C:\sugar\document_templates\GEN-test.docx');
}
*/

//////////////////////////////////////////////////////////////////////////////
function GenerateDocumentAndAttachToEmail ($modulo, $plantilla_id, $ids, $enPDF) {
   $GD = new Generate_Document($modulo, $plantilla_id, $ids);
   $GD->enPDF = $enPDF;
   $GD->Download = false;
   $GD->CargarPlantilla ();
   $GD->ObtenerDatos ();
   $GD->GenerarInforme ();
   
   $email_id = '';
   
   if ($GD->Download_filename) {
      global $app_list_strings, $current_user, $mod_strings, $sugar_config, $timedate;
      require_once('modules/Emails/Email.php');
      require_once('include/utils/sugar_file_utils.php');
      
      // Create e-mail draft
      $email = new Email();
      $email->id = create_guid();
      $email->new_with_id = true;
      
      $namePlusEmail = '';
      if (count($ids) == 1){
         $namePlusEmail = $email->getNamePlusEmailAddressesForCompose($modulo, array($ids[0]));
      }
      $subject = $mod_strings['LBL_DOC_NAME'] . ' - ' . $GD->bean_plantilla->name . ' - ' . date('Y-m-d G:i');
      
      if (isset($app_list_strings['record_type_display'][$modulo]) && count($ids) == 1){
         $email->parent_type = $modulo; 
         $email->parent_id = $ids[0];
         $email->parent_name = '';
      }
      
      //subject      
      $email->name = $subject; 
      //body
      $email->description_html = '';
      $email->description = '';
      $email->type = "out"; //"draft";
      $email->status = "draft";
      $email->mailbox_id = '';  // al menos en SuiteCRM es necesario incluirlo para que salgan los emails en el listview
      $email->from_name = $current_user->full_name;
      $email->from_addr = $current_user->email1;
      $email->to_addrs = $namePlusEmail;
      $email->to_addrs_names = $namePlusEmail;
      $email->to_addrs_ids = "";
      $email->to_addrs_emails = "";      
      $email->assigned_user_id = $current_user->id;
      $email->date_start = $timedate->nowDbDate();
      $email->time_start = $timedate->asDbTime($timedate->getNow());
      
      //Save the email object
      $email->save(FALSE);
      $email_id = $email->id;
      
      // Relationship
      $EmailRelationshipFieldName = getEmailRelationshipFieldName($modulo);
      if ($EmailRelationshipFieldName !== false) {
         if (count($ids) == 1) {
            $email->load_relationship($EmailRelationshipFieldName);
            foreach ($ids as $id) {
               $email->$EmailRelationshipFieldName->add($id);
            }    
         }
      }
      
      $NoteFileName = $GD->template_filename; 
      if ($GD->enPDF){
         $NoteFileName = preg_replace("#\.{$GD->template_file_ext}$#", '.pdf', $NoteFileName);
      }
      
      // attach generated document
      $note = new Note();
      $note->modified_user_id = $current_user->id;
      $note->created_by = $current_user->id;
      $note->name = $subject;
      $note->parent_type = 'Emails';
      $note->parent_id = $email_id;
      $note->file_mime_type = $GD->Download_mimetype;
      $note->filename = $NoteFileName;
      $NoteId = $note->save();
      
      $fichero_destino = $GD->includeTrailingCharacter ($sugar_config['upload_dir'], '/') . $NoteId;
      copy($GD->Download_filename, $fichero_destino);
      sugar_chmod($fichero_destino);      
      
      if (!$GD->Sugar7Module($modulo)) {
         // Redirect ...
         if($email_id == "") {
            echo "Unable to initiate Email Client";
            exit; 
         } else {
            
            $action_param = 'ComposeGeneratedDocumentEmail';
            $record_param_name = 'recordId';
            
            if ($GD->IsSuiteCRM() && version_compare($GD->SuiteCRMVersion(), '7.9.0', '>=')) {
               // IMPORTANTE: Esta acción solo funciona a partir de la versión 7.9.10
               // Ver https://github.com/salesagility/SuiteCRM/issues/4884
               // Ver https://suitecrm.com/wiki/index.php/Release_notes_7.9.10
               
               $action_param = 'ComposeView';
               //$action_param = 'DetailDraftView';  // Como alternativa a abrir la vista ComposeView, en este caso abriría la vista detalle del email
               $record_param_name = 'record';
            }
            
            if (count($ids) == 1) {
               $dURL = "index.php?action=".$action_param."&module=Emails&return_module=".$modulo."&return_action=DetailView&return_id=".$ids[0]."&".$record_param_name."=".$email_id;
            }
            else {
               $dURL = "index.php?action=".$action_param."&module=Emails&".$record_param_name."=".$email_id."&return_module=".$modulo."&return_action=index";
            }
            SugarApplication::redirect($dURL);
         }
      }
   }
   
   return $email_id;
}

//////////////////////////////////////////////////////////////////////////////
function GenerateDocumentAndAttachToNote ($modulo, $plantilla_id, $ids, $enPDF) {
   // Nota: Cuando se llama a esta función, solo deben de haber un id y el módulo debe de estar relacionado con Notas
   //       Esto se traduce en que la llamada solo se puede producir desde el DetailView, y que el módulo debe de estar dentro de la lista $app_list_strings['record_type_display_notes']
   
   $GD = new Generate_Document($modulo, $plantilla_id, $ids);
   $GD->enPDF = $enPDF;
   $GD->Download = false;
   $GD->CargarPlantilla ();
   $GD->ObtenerDatos ();
   $GD->GenerarInforme ();
   
   $NoteId = '';
   
   if ($GD->Download_filename) {
      global $app_list_strings, $current_user, $mod_strings, $sugar_config, $timedate;
      require_once('include/utils/sugar_file_utils.php');
      
      $parent_type = $modulo;
      $parent_id = $ids[0];
      $contact_id = '';
      
      if ($modulo == 'Contacts') {
         $bean_contacts = SugarModule::get('Contacts')->loadBean();
         $bean_contacts->retrieve($parent_id);
         
         $contact_id = $parent_id;
         
         if (isset($bean_contacts->account_id) && $bean_contacts->account_id) {
            $parent_type = 'Accounts';
            $parent_id = $bean_contacts->account_id;
         }
      }      
      
      $NoteFileName = $GD->template_filename; 
      if ($GD->enPDF){
         $NoteFileName = preg_replace("#\.{$GD->template_file_ext}$#", '.pdf', $NoteFileName);
      }
      
      $note = new Note();
      $note->modified_user_id = $current_user->id;
      $note->created_by = $current_user->id;
      $note->name = $GD->bean_plantilla->name;
      $note->parent_type = $parent_type;
      $note->parent_id = $parent_id;
      $note->contact_id = $contact_id;
      $note->file_mime_type = $GD->Download_mimetype;
      $note->filename = $NoteFileName;
      $NoteId = $note->save();
      
      $fichero_destino = $GD->includeTrailingCharacter ($sugar_config['upload_dir'], '/') . $NoteId;
      copy($GD->Download_filename, $fichero_destino);
      sugar_chmod($fichero_destino);      
      
      if (!$GD->Sugar7Module ($modulo)) {
         // Redirect ...
         if($NoteId == "") {
            echo "Error creating Note '{$GD->bean_plantilla->name}'";
            exit; 
         } else {
            $dURL = "index.php?action=DetailView&module=Notes&record=".$NoteId; 
            SugarApplication::redirect($dURL);
         }
      }
   }
   
   return $NoteId;
}

?>