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

class stic_RemittancesController extends SugarController {
    public function action_generateSEPADirectDebit() {
        require 'modules/stic_Remittances/GenerateSEPADirectDebits.php';
        generateSEPADirectDebits($this);
    }

    public function action_generateSEPACreditTransfer() {
        require 'modules/stic_Remittances/GenerateSEPACreditTransfers.php';
        generateSEPACreditTransfers($this);
    }
    
    public function action_processRedsysCardPayments() {
        require 'modules/stic_Remittances/ProcessRedsysCardPayments.php';
        processRedsysCardPayments($this);
    }

    public function action_loadSEPAReturns() {
        require_once 'modules/stic_Remittances/LoadSEPAReturns.php';
        SepaReturns::loadSEPAReturns();
    }

    public function action_loadFile() {
        $this->view = 'load_file';
    }

    /**
     * This actions comes from the ViewList of stic_Payments. It runs the function that will add the relationship between the payments and the remittance using a
     * SQL query.
     */
    public function action_addPaymentsToRemittance() {
        require 'modules/stic_Remittances/AddPaymentsToRemittance.php';
        addPaymentsToRemittance();
    }

}
