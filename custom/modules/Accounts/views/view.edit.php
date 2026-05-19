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

require_once 'modules/Accounts/views/view.edit.php';
require_once 'SticInclude/Views.php';

class CustomAccountsViewEdit extends AccountsViewEdit
{
    public function __construct()
    {
        parent::__construct();
        $this->useForSubpanel = true;
        $this->useModuleQuickCreateTemplate = true;
        // Since the suite base modules name the bean in the singular, we configure in the view the name of the module in the plural. This property will be used by the SticViews class to load the language files
        $this->moduleName = 'Accounts';
    }

    public function preDisplay()
    {
        parent::preDisplay();

        SticViews::preDisplay($this);

        // Write here you custom code
    }

    public function display()
    {
        parent::display();

        SticViews::display($this);

        // Write here you custom code

        // Provide Private Area password status for frontend placeholder handling
        $hasStoredPassword = false;
        if (!empty($this->bean->id)) {
            global $db;
            $idQuoted = $db->quote($this->bean->id);
            $storedPassword = $db->getOne("SELECT stic_pa_password_c FROM accounts_cstm WHERE id_c = '{$idQuoted}'");
            $hasStoredPassword = !empty($storedPassword);
        }

        global $app_strings;
        $passwordPlaceholder = $app_strings['LBL_PASSWORD_SET_NEW_VALUE_TO_RESET'] ?? '';

        $privateAreaPasswordConfig = json_encode([
            'hasStoredPassword' => $hasStoredPassword,
            'placeholder' => $passwordPlaceholder,
        ]);

        echo <<<SCRIPT
        <script>
            window.STIC = window.STIC || {};
            STIC.privateAreaPassword = $privateAreaPasswordConfig;
        </script>
    SCRIPT;

        // We need to add manually to the frontend the required Incorpora fields
        require_once('modules/stic_Incorpora/utils/FieldsDef.php');
        $incorporaRequiredFieldsArray = json_encode(array_filter($accountDef, function ($var) { return $var['required'] ?? false; }));
        $incorporaAgreementRequiredFieldsArray = json_encode(array_filter($accountDef, function ($var) { return $var['agreementRequired'] ?? false; }));

        echo <<<SCRIPT
        <script>
            STIC.incorporaRequiredFieldsArray = $incorporaRequiredFieldsArray;
            STIC.incorporaAgreementRequiredFieldsArray = $incorporaAgreementRequiredFieldsArray;
        </script>
    SCRIPT;

        echo getVersionedScript("custom/modules/Accounts/SticUtils.js");
    }

}
