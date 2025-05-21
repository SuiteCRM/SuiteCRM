<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

require_once 'modules/stic_Web_Forms/Assistant/AssistantView.php';

class stic_Web_FormsViewStepDownload extends stic_Web_FormsAssistantView
{
    protected $downloadFileName = 'webform.html'; // Name of the file to download
    protected $downloadLabel = 'LBL_WEBFORMS_DOWNLOAD_LABEL'; // Label of the file to download

    /**
     * Do what is needed before showing the view
     */
    public function preDisplay()
    {
        parent::preDisplay();

        // Prepare the embedded javascript code
        $this->view_object_map['EMBEDDED_JS'] = $this->prepareJS();
        $this->view_object_map['LANG_VARS'] = $this->prepareLangVars();

        $html = $this->renderForm();
        $this->view_object_map['RAW_SOURCE'] = mb_convert_encoding($html, 'ISO-8859-1', 'UTF-8');
        $this->view_object_map['LINK_TO_WEB_FORM'] = $this->createDownloadLink($html);
        $this->view_object_map['DOWNLOAD_LABEL'] = $this->downloadLabel;
        $this->tpl = "StepDownload.tpl";
    }

    /**
     * Display the view
     */
    public function display()
    {
        parent::display();
    }

    /**
     * Render the final form
     * @return Object
     */
    public function renderForm()
    {
        $this->ssform = new Sugar_Smarty();
        $this->ssform->assign('MOD', $GLOBALS['mod_strings']);
        $this->ssform->assign('APP', $GLOBALS['app_strings']);
        $this->ssform->assign('MAP', $this->view_object_map);
        $this->ssform->assign('TEMPLATE_DIR', $this->templateDir);
        $this->ssform->assign('INCLUDE_TEMPLATE_DIR', $this->commonTemplateDir);

        return $this->ssform->fetch("{$this->commonTemplateDir}/FormResult.tpl");
    }

    /**
     * Create a document and return the link for download
     * @param $html
     * @return String
     */
    public function createDownloadLink($html)
    {
        $guid = create_guid();
        $formFile = "upload://$guid";
        file_put_contents($formFile, $html);

        return "index.php?entryPoint=download&id={$guid}&isTempFile=1&tempName=" . urlencode($this->downloadFileName) . "&type=temp";
    }

    /**
     * Includes the javascript code necessary for the operation of the form
     * @return String
     */
    public function prepareJS()
    {
        // Generate the necessary javascript code
        $jsForm = '';
        ob_start();
        require_once "{$this->javascriptDir}/WebForm.js";
        $jsForm = ob_get_contents();
        ob_end_clean();

        if (isset($this->recaptchaConfiguration['VERSION']) && 
            $this->recaptchaConfiguration['VERSION'] == '2') {
            $jsForm .= $this->prepareSubmitRecaptcha2JS();
        } else if (isset($this->recaptchaConfiguration['VERSION']) && 
            $this->recaptchaConfiguration['VERSION'] == '3') {
            $jsForm .= $this->prepareSubmitRecaptcha3JS();
        } else {
           $jsForm .= $this->prepareSubmitJS();
        }

        return $jsForm;
    }

    /**
     * Includes the javascript Submit code (without reCAPTCHA) for the form
     * @return String
     */
    private function prepareSubmitJS()
    {
        $jsSubmit = '';
        ob_start();
        require_once "{$this->javascriptDir}/WebFormSubmit.js";
        $jsSubmit = ob_get_contents();
        ob_end_clean();

        return $jsSubmit;
    }

    /**
     * Includes the javascript Submit code with reCAPTCHA v2 for the form
     * @return String
     */
    private function prepareSubmitRecaptcha2JS()
    {
        $jsSubmit = '';
        ob_start();
        require_once "{$this->javascriptDir}/WebFormSubmitRecaptcha2.js";
        $jsSubmit = ob_get_contents();
        ob_end_clean();

        return $jsSubmit;
    }

    /**
     * Includes the javascript Submit code with reCAPTCHA v3 for the form
     * @return string
     */
    private function prepareSubmitRecaptcha3JS()
    {
        $jsSubmit = '';
        ob_start();
        require_once "{$this->javascriptDir}/WebFormSubmitRecaptcha3.js";
        $jsSubmit = ob_get_contents();
        ob_end_clean();

        return str_replace('<SITE_KEY>', $this->recaptchaConfiguration['WEBKEY'], $jsSubmit);
    }

    
    /**
     * It includes the necessary language variables
     * @return Array
     */
    public function prepareLangVars()
    {
        global $app_strings, $timedate;

        $langVars = array();
        $langVars['stic_Web_Forms_LBL_PROVIDE_WEB_FORM_FIELDS'] = translate('LBL_PROVIDE_WEB_FORM_FIELDS', 'stic_Web_Forms');
        $langVars['stic_Web_Forms_LBL_INVALID_FORMAT'] = translate('LBL_INVALID_FORMAT', 'stic_Web_Forms');
        $langVars['stic_Web_Forms_LBL_SERVER_CONNECTION_ERROR'] = translate('LBL_SERVER_CONNECTION_ERROR', 'stic_Web_Forms');
        $langVars['stic_Web_Forms_LBL_SIZE_FILE_EXCEED'] = translate('LBL_SIZE_FILE_EXCEED', 'stic_Web_Forms');
        $langVars['stic_Web_Forms_LBL_SUM_SIZE_FILES_EXCEED'] = translate('LBL_SUM_SIZE_FILES_EXCEED', 'stic_Web_Forms');
        $langVars['APP_LBL_REQUIRED_SYMBOL'] = $app_strings['LBL_REQUIRED_SYMBOL'];
        $langVars['APP_DATE_FORMAT'] = $timedate->get_cal_date_format();

        return $langVars;
    }
}
