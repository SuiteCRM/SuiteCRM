<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

require_once('include/TemplateHandler/TemplateHandler.php');
require_once('include/EditView/EditView2.php');

/**
 * DetailView - display single record
 * New implementation
 * @api
 */
#[\AllowDynamicProperties]
class DetailView2 extends EditView
{
    /**
     * @var string $view
     */
    public $view = 'DetailView';
    /**
     * @var array $defs
     */
    public $defs;

    /**
     * DetailView constructor
     * This is the DetailView constructor responsible for processing the new
     * Meta-Data framework
     *
     * @param string $module String value of module this detail view is for
     * @param SugarBean|null $focus An empty sugarbean object of module
     * @param string|null $metadataFile String value of file location to use in overriding default metadata file
     * @param string $tpl tpl String value of file location to use in overriding default Smarty template
     * @param bool $createFocus
     * @param string $metadataFileName specifies the name of the metadata file eg 'detailviewdefs'
     */
    public function setup(
        $module,
        $focus  = null,
        $metadataFile = null,
        $tpl = 'include/DetailView/DetailView.tpl',
        $createFocus = true,
        $metadataFileName = 'detailviewdefs'
        ) {
        global $sugar_config;

        $this->th = new TemplateHandler();
        $this->th->ss = $this->ss;
        $viewdefs = array();

        //Check if inline editing is enabled for detail view.
        if (!isset($sugar_config['enable_line_editing_detail']) || $sugar_config['enable_line_editing_detail']) {
            $this->ss->assign('inline_edit', true);
        }
        $this->focus = $focus;
        $this->tpl = get_custom_file_if_exists($tpl);
        $this->module = $module;
        $this->metadataFile = $metadataFile;
        if (isset($GLOBALS['sugar_config']['disable_vcr'])) {
            $this->showVCRControl = !$GLOBALS['sugar_config']['disable_vcr'];
        }
        if (!empty($this->metadataFile) && file_exists($this->metadataFile)) {
            require($this->metadataFile);
        } else {
            //If file doesn't exist we create a best guess
            if (!file_exists("modules/$this->module/metadata/$metadataFileName.php") &&
                file_exists("modules/$this->module/DetailView.html")) {
                global $dictionary;
                $htmlFile = "modules/" . $this->module . "/DetailView.html";
                $parser = new DetailViewMetaParser();
                if (!file_exists('modules/'.$this->module.'/metadata')) {
                    sugar_mkdir('modules/'.$this->module.'/metadata');
                }
                sugar_file_put_contents(
                    'modules/'.$this->module.'/metadata/$metadataFileName.php',
                    $parser->parse($htmlFile, $dictionary[$focus->object_name]['fields'], $this->module)
                );
            }

            //Flag an error... we couldn't create the best guess meta-data file
            if (!file_exists("modules/$this->module/metadata/$metadataFileName.php")) {
                global $app_strings;
                $error = str_replace("[file]", "modules/$this->module/metadata/$metadataFileName.php", (string) $app_strings['ERR_CANNOT_CREATE_METADATA_FILE']);
                $GLOBALS['log']->fatal($error);
                echo $error;
                die();
            }
            require("modules/$this->module/metadata/$metadataFileName.php");
        }

        $this->defs = $viewdefs[$this->module][$this->view];
    }

    /**
     * @param array $request
     * @return void
     * @see EditView::populateBean()
     */
    public function populateBean($request = array())
    {
        parent::populateBean($request);
    }
}
