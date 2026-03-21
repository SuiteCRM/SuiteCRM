<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class AOS_PDF_TemplatesViewEdit extends ViewEdit
{
    public $tinymceElementIds = [
        'description' => '#description',
        'pdfheader' => '#pdfheader',
        'pdffooter'=> '#pdffooter',
    ];
    public function __construct()
    {
        parent::__construct();
    }

    public function display()
    {
        $this->setFields();
        parent::display();
        $this->displayTMCE();
    }

    public function setFields()
    {
        global $app_list_strings, $mod_strings, $beanList, $log;

        //Loading Sample Files
        $json = getJSONobj();
        $samples = array();
        $sample_options_array = [];

        if ($handle = opendir('modules/AOS_PDF_Templates/samples')) {
            while (false !== ($file = readdir($handle))) {
                if ($value = ltrim(rtrim($file, '.php'), 'smpl_')) {
                    require_once('modules/AOS_PDF_Templates/samples/'.$file);
                    $file = rtrim($file, '.php');
                    $file = new $file();
                    $fileArray =
                        array(
                            $file->getType(),
                            $file->getBody(),
                            $file->getHeader(),
                            $file->getFooter()
                        );
                    $fileArray = $json->encode($fileArray);
                    $value = $mod_strings['LBL_'.strtoupper($value)];
                    $sample_options_array[$fileArray] = $value;
                }
            }
            $samples = get_select_options($sample_options_array, '');
            closedir($handle);
        }

        $this->ss->assign('CUSTOM_SAMPLE', '<select id="sample" name="sample" onchange="insertSample(this.options[this.selectedIndex].value)">'.
            $samples.
            '</select>');


        $insert_fields_js ="<script>var moduleOptions = {\n";
        $insert_fields_js2 ="<script>var regularOptions = {\n";
        $modules = $app_list_strings['pdf_template_type_dom'];

        foreach ($modules as $moduleName => $value) {
            $options_array = array(''=>'');
            $mod_options_array = array();

            //Getting Fields
            if (!$beanList[$moduleName]) {
                continue;
            }

            $module = new $beanList[$moduleName]();

            foreach ($module->field_defs as $name => $arr) {
                if (!((isset($arr['dbType']) && strtolower($arr['dbType']) == 'id') || (isset($arr['type']) && $arr['type'] == 'id') || (isset($arr['type']) && $arr['type'] == 'link'))) {
                    if (!isset($arr['reportable']) || $arr['reportable']) {
                        $options_array['$'.$module->table_name.'_'.$name] = translate($arr['vname'] ?? '', $module->module_dir);
                    }
                }
            } //End loop.

            $options = json_encode($options_array);
            $mod_options_array[$module->module_dir] = translate('LBL_MODULE_NAME', $module->module_dir);
            $insert_fields_js2 .="'$moduleName':$options,\n";
            $firstOptions = $options;

            $fmod_options_array = array();
            foreach ($module->field_defs as $module_name => $module_arr) {
                if (isset($module_arr['type']) && $module_arr['type'] == 'relate' && isset($module_arr['source']) && $module_arr['source'] == 'non-db') {
                    $options_array = array(''=>'');
                    if (isset($module_arr['module']) &&  $module_arr['module'] != '' && $module_arr['module'] != 'EmailAddress') {
                        $relate_module_name = $beanList[$module_arr['module']];
                        $relate_module = new $relate_module_name();

                        foreach ($relate_module->field_defs as $relate_name => $relate_arr) {
                            if (!((isset($relate_arr['dbType']) && strtolower($relate_arr['dbType']) == 'id') || $relate_arr['type'] == 'id' || $relate_arr['type'] == 'link')) {
                                if ((!isset($relate_arr['reportable']) || $relate_arr['reportable']) && isset($relate_arr['vname'])) {
                                    $options_array['$'.$module_arr['name'].'_'.$relate_name] = translate($relate_arr['vname'], $relate_module->module_dir);
                                }
                            }
                        } //End loop.

                        $options = json_encode($options_array);

                        if ($module_arr['vname'] != 'LBL_DELETED') {
                            $options_array['$'.$module->table_name.'_'.$name] = translate($module_arr['vname'], $module->module_dir);
                            $fmod_options_array[$module_arr['vname']] = translate($relate_module->module_dir).' : '.translate($module_arr['vname'], $module->module_dir);
                        }
                        $test = $module_arr['vname'];
                        $insert_fields_js2 .="'$test':$options,\n";
                    }
                }
            }

            //LINE ITEMS CODE!
            if (isset($module->lineItems) && $module->lineItems) {

                //add group fields
                $options_array = array(''=>'');
                $group_quote = BeanFactory::newBean('AOS_Line_Item_Groups');
                foreach ($group_quote->field_defs as $line_name => $line_arr) {
                    if (!((isset($line_arr['dbType']) && strtolower($line_arr['dbType']) == 'id') || $line_arr['type'] == 'id' || $line_arr['type'] == 'link')) {
                        if ((!isset($line_arr['reportable']) || $line_arr['reportable'])) {//&& $line_arr['vname']  != 'LBL_NAME'
                            $options_array['$'.$group_quote->table_name.'_'.$line_name] = translate($line_arr['vname'], $group_quote->module_dir);
                        }
                    }
                }

                $options = json_encode($options_array);

                $line_module_name = $beanList['AOS_Line_Item_Groups'];
                $fmod_options_array[$line_module_name] = translate('LBL_LINE_ITEMS', 'AOS_Quotes').' : '.translate('LBL_MODULE_NAME', 'AOS_Line_Item_Groups');
                $insert_fields_js2 .="'$line_module_name':$options,\n";

                //PRODUCTS
                $options_array = array(''=>'');

                $product_quote = BeanFactory::newBean('AOS_Products_Quotes');
                foreach ($product_quote->field_defs as $line_name => $line_arr) {
                    if (!((isset($line_arr['dbType']) && strtolower($line_arr['dbType']) == 'id') || $line_arr['type'] == 'id' || $line_arr['type'] == 'link')) {
                        if (!isset($line_arr['reportable']) || $line_arr['reportable']) {
                            $options_array['$'.$product_quote->table_name.'_'.$line_name] = translate($line_arr['vname'], $product_quote->module_dir);
                        }
                    }
                }

                $product_quote = BeanFactory::newBean('AOS_Products');
                foreach ($product_quote->field_defs as $line_name => $line_arr) {
                    if (!((isset($line_arr['dbType']) && strtolower($line_arr['dbType']) == 'id') || $line_arr['type'] == 'id' || $line_arr['type'] == 'link')) {
                        if ((!isset($line_arr['reportable']) || $line_arr['reportable']) && $line_arr['vname']  != 'LBL_NAME') {
                            $options_array['$'.$product_quote->table_name.'_'.$line_name] = translate($line_arr['vname'], $product_quote->module_dir);
                        }
                    }
                }

                $options = json_encode($options_array);

                $line_module_name = $beanList['AOS_Products_Quotes'];
                $fmod_options_array[$line_module_name] = translate('LBL_LINE_ITEMS', 'AOS_Quotes').' : '.translate('LBL_MODULE_NAME', 'AOS_Products');
                $insert_fields_js2 .="'$line_module_name':$options,\n";

                //Services
                $options_array = array(''=>'');
                $options_array['$aos_services_quotes_name'] = translate('LBL_SERVICE_NAME', 'AOS_Quotes');
                $options_array['$aos_services_quotes_number'] = translate('LBL_LIST_NUM', 'AOS_Products_Quotes');
                $options_array['$aos_services_quotes_service_list_price'] = translate('LBL_SERVICE_LIST_PRICE', 'AOS_Quotes');
                $options_array['$aos_services_quotes_service_discount'] = translate('LBL_SERVICE_DISCOUNT', 'AOS_Quotes');
                $options_array['$aos_services_quotes_service_unit_price'] = translate('LBL_SERVICE_PRICE', 'AOS_Quotes');
                $options_array['$aos_services_quotes_vat_amt'] = translate('LBL_VAT_AMT', 'AOS_Quotes');
                $options_array['$aos_services_quotes_vat'] = translate('LBL_VAT', 'AOS_Quotes');
                $options_array['$aos_services_quotes_service_total_price'] = translate('LBL_TOTAL_PRICE', 'AOS_Quotes');

                $options = json_encode($options_array);

                $s_line_module_name = 'AOS_Service_Quotes';
                $fmod_options_array[$s_line_module_name] = translate('LBL_LINE_ITEMS', 'AOS_Quotes').' : '.translate('LBL_SERVICE_MODULE_NAME', 'AOS_Products_Quotes');
                $insert_fields_js2 .="'$s_line_module_name':$options,\n";


                $options_array = array(''=>'');
                $currencies = new currency();
                foreach ($currencies->field_defs as $name => $arr) {
                    if (!((isset($arr['dbType']) && strtolower($arr['dbType']) == 'id') || $arr['type'] == 'id' || $arr['type'] == 'link' || $arr['type'] == 'bool' || $arr['type'] == 'datetime' || (isset($arr['link_type']) && $arr['link_type'] == 'relationship_info'))) {
                        if (isset($arr['vname']) && $arr['vname'] != 'LBL_DELETED' && $arr['vname'] != 'LBL_CURRENCIES_HASH' && $arr['vname'] != 'LBL_LIST_ACCEPT_STATUS' && $arr['vname'] != 'LBL_AUTHENTICATE_ID' && $arr['vname'] != 'LBL_MODIFIED_BY' && $arr['name'] != 'created_by_name') {
                            $options_array['$currencies_'.$name] = translate($arr['vname'], 'Currencies');
                        }
                    }
                }
                $options = json_encode($options_array);

                $line_module_name = $beanList['Currencies'];
                $fmod_options_array[$line_module_name] = translate('LBL_MODULE_NAME', 'Currencies').' : '.translate('LBL_MODULE_NAME', 'Currencies');
                $insert_fields_js2 .="'$line_module_name':$options,\n";
            }
            array_multisort($fmod_options_array, SORT_ASC, $fmod_options_array);
            $mod_options_array = array_merge($mod_options_array, $fmod_options_array);
            $module_options = json_encode($mod_options_array);


            $insert_fields_js .="'$moduleName':$module_options,\n";
            $moduleOptions[$moduleName] = array("module" => $module_options,"option" => $firstOptions);
        } //End loop.

        //Sets options to original options on load.
        $insert_fields_js .= "} ;</script>";
        $insert_fields_js2 .= "} ;</script>";
        //echo $this->bean->type;
        if ($this->bean->type=='') {
            $type = key($app_list_strings['pdf_template_type_dom']);
        } else {
            $type = $this->bean->type;
        }

        //Start of insert_fields
        $insert_fields = '';
        $insert_fields .= <<<HTML

		$insert_fields_js
		$insert_fields_js2
		<select name='module_name' id='module_name' tabindex="50" onchange="populateVariables(this.options[this.selectedIndex].value);">
		</select>
		<select name='variable_name' id='variable_name' tabindex="50" onchange="showVariable(this.options[this.selectedIndex].value);">
		</select>
		<input type="text" size="30" tabindex="60" name="variable_text" id="variable_text" />
		<input type='button' tabindex="70" onclick='insert_variable(document.EditView.variable_text.value, "email_template_editor");' class='button' value='{$mod_strings['LBL_BUTTON_INSERT']}'>
		<script type="text/javascript">
			populateModuleVariables("$type");
	</script>


HTML;

        $this->ss->assign('INSERT_FIELDS', $insert_fields);
    }

    public function displayTMCE()
    {
        require_once("include/SugarTinyMCE.php");
        global $locale, $log;

        try {
            $tiny = new SugarTinyMCE();
            $tinyConfig = $tiny->getConfig();
            $extendedTinyConfig = json_encode($tiny->getConfigArray('extended'));
            $standardTinyConfig = json_encode($tiny->getConfigArray());

            if (empty($extendedTinyConfig)) {
                $log?->error("[AOS_PDF_Templates][displayTMCE] Empty TinyMCE configuration returned");
                return '';
            }

            $defaultDateFormat = $locale->getPrecedentPreference('default_date_format');
            $descriptionTinyMceElementIds = $this->tinymceElementIds['description'];
            $pdfHeaderTinyMceElementIds = $this->tinymceElementIds['pdfheader'];
            $pdfFooterTinyMceElementIds = $this->tinymceElementIds['pdffooter'];

            $js = '<script language="javascript" type="text/javascript">';
            $js .= $tinyConfig;
            $js .= "var dateFormat = '$defaultDateFormat';";
            // PDF Template Body Editor'
            // SECURITY NOTE: entity_encoding is set to \'raw\' for PDF templates because:'
            // 1. HTML entities must be preserved for proper PDF rendering via TCPDF'
            // 2. Access to PDF template editing is restricted by ACL controls'
            // 3. Template output is sanitized during PDF generation'
            // 4. This editor requires raw HTML for textblock/barcode custom elements'
            $js .= "var bodyConfig = Object.assign({}, $extendedTinyConfig);";
            $js .= "bodyConfig.selector = '$descriptionTinyMceElementIds';";
            $js .= "bodyConfig.inline_styles = true;";
            $js .= "bodyConfig.remove_redundant_brs = true;";
            $js .= "bodyConfig.entity_encoding = 'raw';";
            $js .= "bodyConfig.cleanup_on_startup = true;";
            $js .= "bodyConfig.strict_loading_mode = true;";
            $js .= "bodyConfig.pagebreak_separator = '<div style=\"page-break-before: always;\">&nbsp;</div>';";
            $js .= "bodyConfig.extended_valid_elements = 'textblock,barcode[*]';";
            $js .= "bodyConfig.custom_elements = 'textblock';";
            $js .= "bodyConfig.insertdatetime_formats = ['{DATE '+dateFormat+'}'];";
            $js .= 'tinymce.init(bodyConfig);';
            // PDF Template Header/Footer Editor'
            // SECURITY NOTE: entity_encoding is set to \'raw\' (see comment above)'
            $js .= "var headerFooterConfig = Object.assign({}, $standardTinyConfig);";
            $js .= "headerFooterConfig.selector = '$pdfHeaderTinyMceElementIds, $pdfFooterTinyMceElementIds';";
            $js .= "headerFooterConfig.inline_styles = true;";
            $js .= "headerFooterConfig.entity_encoding = 'raw';";
            $js .= "headerFooterConfig.cleanup_on_startup = true;";
            $js .= "headerFooterConfig.strict_loading_mode = true;";
            $js .= "headerFooterConfig.remove_redundant_brs = true;";
            $js .= "headerFooterConfig.extended_valid_elements = 'textblock,barcode[*]';";
            $js .= "headerFooterConfig.custom_elements = 'textblock';";
            $js .= "headerFooterConfig.insertdatetime_formats = ['{DATE '+dateFormat+'}'];";
            $js .= 'tinymce.init(headerFooterConfig);';
            $js .= '</script>';

            echo $js;

        } catch (Throwable $e) {
            $log?->error("[AOS_PDF_Templates][displayTMCE] Failed to initialize TinyMCE: " . $e->getMessage());
            throw $e;
        }
    }
}
