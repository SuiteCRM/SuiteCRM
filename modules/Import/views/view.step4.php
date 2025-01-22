<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

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

/**

 * Description: view handler for step 4 of the import process
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

require_once('include/MVC/View/SugarView.php');
require_once('modules/Import/Importer.php');

#[\AllowDynamicProperties]
class ImportViewStep4 extends SugarView
{
    private $currentStep;

    public function __construct($bean = null, $view_object_map = array())
    {
        parent::__construct($bean, $view_object_map);
        $this->currentStep = isset($_REQUEST['current_step']) ? ($_REQUEST['current_step'] + 1) : 1;
    }

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings, $sugar_config;

        // Check to be sure we are getting an import file that is in the right place
        $uploadFile = "upload://".basename((string) $_REQUEST['tmp_file']);
        if (!file_exists($uploadFile)) {
            trigger_error($mod_strings['LBL_CANNOT_OPEN'], E_USER_ERROR);
        }

        // Open the import file
        $importSource = new ImportFile($uploadFile, $_REQUEST['custom_delimiter'], html_entity_decode((string) $_REQUEST['custom_enclosure'], ENT_QUOTES));

        //Ensure we have a valid file.
        if (!$importSource->fileExists()) {
            trigger_error($mod_strings['LBL_CANNOT_OPEN'], E_USER_ERROR);
        }

        if (!ImportCacheFiles::ensureWritable()) {
            trigger_error($mod_strings['LBL_ERROR_IMPORT_CACHE_NOT_WRITABLE'], E_USER_ERROR);
        }

        $importer = new Importer($importSource, $this->bean);
        $importer->import();
    }
}
