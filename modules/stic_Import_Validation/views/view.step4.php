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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/MVC/View/SugarView.php');
require_once('modules/stic_Import_Validation/Importer.php');

#[\AllowDynamicProperties]
class stic_Import_ValidationViewStep4 extends SugarView
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
            sugar_die($mod_strings['LBL_CANNOT_OPEN']);
        }

        // Open the import file
        $importSource = new ImportFile($uploadFile, $_REQUEST['custom_delimiter'], html_entity_decode((string) $_REQUEST['custom_enclosure'], ENT_QUOTES));

        //Ensure we have a valid file.
        if (!$importSource->fileExists()) {
            sugar_die($mod_strings['LBL_CANNOT_OPEN']);
        }

        if (!ImportCacheFiles::ensureWritable()) {
            sugar_die($mod_strings['LBL_ERROR_IMPORT_CACHE_NOT_WRITABLE']);
        }

        $importer = new Importer($importSource, $this->bean);
        $importer->importValidation();
    }
}
