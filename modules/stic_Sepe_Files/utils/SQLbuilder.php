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

class SQLbuilder {

    /**
     * Function used as a nested SQL query in order to select only the people that are subscribed into the SEPE job applications 
     * program
     *
     * @param string $month The month that will be used in the Where sentence
     * @param string $year The year that will be used in the Where sentence
     * @param boolean $agreement true if file if accd
     * @return query SQL query result
     */
    protected function getContactsNestedSQL($year, $month = false, $agreement = false) {

        $date = $month == 12 ? ($year+1)."-01-01" : "$year-".($month+1)."-01";
        if (!$month) $date = ($year+1)."-01-01";
        return "
        (
            SELECT
                contacts.last_name,
                contacts.first_name,
                contacts.birthdate,
                contacts.deleted,
                contacts.id,
                stic_sepe_actions.start_date as 'sepe_start_date',
                stic_sepe_actions.end_date as 'sepe_end_date'
            FROM
                contacts
            INNER JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            INNER JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            WHERE
                stic_sepe_actions.type = 'A'
                AND stic_sepe_actions.start_date < '$date'
                ".($agreement ? "AND stic_sepe_actions.agreement = '$agreement'" : 
                "AND (stic_sepe_actions.agreement = '' OR stic_sepe_actions.agreement IS NULL)")."
                AND contacts.deleted = 0
                AND stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0) as contacts ";
    }

    /**
     * Used to build the queries with COLOCACION = N
     */
    public const joinTablesNoCol = " 
    INNER JOIN contacts_cstm ON
        contacts.id = contacts_cstm.id_c
    INNER JOIN stic_sepe_actions_contacts_c ON
        contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
    INNER JOIN stic_sepe_actions ON
        stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id";

    /**
     * Used to build the queries with COLOCACION = S
     */
    public const joinTablesCol = "
    INNER JOIN stic_job_applications_contacts_c ON 
        contacts.id = stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida
    INNER JOIN stic_job_applications ON 
        stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
    INNER JOIN stic_job_applications_stic_job_offers_c ON 
        stic_job_applications.id = stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb
    INNER JOIN stic_job_offers ON 
        stic_job_offers.id = stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida
    INNER JOIN stic_job_offers_accounts_c ON 
        stic_job_offers.id = stic_job_offers_accounts_c.stic_job_offers_accountsstic_job_offers_idb 
    INNER JOIN accounts_cstm ON 
        accounts_cstm.id_c = stic_job_offers_accounts_c.stic_job_offers_accountsaccounts_ida 
    INNER JOIN accounts ON 
        accounts_cstm.id_c = accounts.id ";

    /**
     * Used to build the queries for SEPE incidents
     */
    public const joinTablesACCI = "
    INNER JOIN contacts_cstm ON 
        contacts_cstm.id_c = contacts.id 
    INNER JOIN stic_sepe_incidents_contacts_c ON 
        stic_sepe_incidents_contacts_c.stic_sepe_incidents_contactscontacts_ida = contacts.id 
    INNER JOIN stic_sepe_incidents ON 
        stic_sepe_incidents_contacts_c.stic_sepe_incidents_contactsstic_sepe_incidents_idb = stic_sepe_incidents.id";

    /**
     * Used to build the WHERE clause of the queries AC for COLOCACION = N
     *
     * @param string $month The month that will be used in the Where sentence
     * @param string $year The year that will be used in the Where sentence
     * @return query SQL query result
     */
    protected function getWhereQueryACNoCol($month, $year) {
        return "
        contacts.deleted = 0
        AND (stic_sepe_actions.agreement = '' OR stic_sepe_actions.agreement IS NULL)
        AND stic_sepe_actions_contacts_c.deleted = 0
        AND stic_sepe_actions.deleted = 0
        AND stic_sepe_actions.type = 'A'
        AND MONTH(stic_sepe_actions.start_date) = $month
        AND YEAR(stic_sepe_actions.start_date) = $year";
    }

    /**
     * Used to build the WHERE clause of the queries AC for COLOCACION = N
     *
     * @param string $month The month that will be used in the Where sentence
     * @param string $year The year that will be used in the Where sentence
     * @return query SQL query result
     */
    protected function getWhereQueryACCDNoCol($month, $year) {
        return "
        contacts.deleted = 0
        AND stic_sepe_actions_contacts_c.deleted = 0
        AND stic_sepe_actions.deleted = 0
        AND stic_sepe_actions.type != 'A'
        AND MONTH(stic_sepe_actions.start_date) = $month
        AND YEAR(stic_sepe_actions.start_date) = $year
        AND contacts.sepe_start_date <= stic_sepe_actions.start_date
        AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_sepe_actions.start_date)";
    }

    /**
     * Used to build the Where clause of the queries for COLOCACION = S
     * 
     * @param string $month The month that will be used in the Where sentence
     * @param string $year The year that will be used in the Where sentence
     * @return query SQL query result
     */
    protected function getWhereQueryCol($month, $year) {
        $date = $month == 12 ? ($year+1)."-01-01" : "$year-".($month+1)."-01";
        return "
        stic_job_applications.deleted = 0 
        AND stic_job_offers.deleted = 0 
        AND accounts.deleted = 0 
        AND stic_job_offers_accounts_c.deleted = 0 
        AND stic_job_applications_stic_job_offers_c.deleted = 0 
        AND stic_job_applications_contacts_c.deleted = 0 
        AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
        AND MONTH(stic_job_applications.contract_start_date) = $month
        AND YEAR(stic_job_applications.contract_start_date) = $year
        AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
        AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)";
    }

    /**
     * Used to build the where clause of the queries for ACCI files
     * 
     * @param string $month The month that will be used in the Where sentence
     * @param string $year The year that will be used in the Where sentence
     * @return query SQL query result
     */
    protected function getWhereQueryACCI($month, $year) {
        return "
        stic_sepe_incidents.deleted = 0
        AND stic_sepe_incidents_contacts_c.deleted = 0
        AND MONTH(stic_sepe_incidents.incident_date) = $month
        AND YEAR(stic_sepe_incidents.incident_date) = $year
        AND contacts.sepe_start_date <= stic_sepe_incidents.incident_date
        AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_sepe_incidents.incident_date)";
    }

    /**
     * It builds the complete queries to retrieve the individual data of the AC files
     * It uses the definition of the fields specified in SEPEgetter.php
     *
     * @param integer $month
     * @param integer $year
     * @param array $defACindividual Array with the fields defined to retrieve data with COLOCACION = N
     * @param array $defACindividualCol Array with the rest of the fields to retrieve data with COLOCACION = S
     * @return sqlQuery
     */
    public function getQueryMonthlyAC($month, $year, $defACindividual, $defACindividualCol) {
        $sqlIndiv = '';
        $sqlIndiv = $this->buildSelectParams($defACindividual, $sqlIndiv);
        
        $sqlQuery['NOCOL'] = 'SELECT DISTINCT ' .$sqlIndiv. ", 'N' as COLOCACION FROM contacts". SQLbuilder::joinTablesNoCol ." WHERE ".$this->getWhereQueryACNoCol($month, $year) ;

        $sqlIndiv = $sqlIndiv .", 'S' as COLOCACION ";

        $sqlIndiv = $this->buildSelectParams($defACindividualCol, $sqlIndiv);

        $sqlQuery['COL'] = 'SELECT DISTINCT '.$sqlIndiv." FROM ".$this->getContactsNestedSQL($year, $month).SQLbuilder::joinTablesNoCol." ".SQLbuilder::joinTablesCol."WHERE".$this->getWhereQueryCol($month, $year); 

        return $sqlQuery;
    }

    /**
     * It builds the complete queries to retrieve the individual data of the ACCD files
     * It uses the definition of the fields specified in SEPEgetter.php
     *
     * @param integer $month
     * @param integer $year
     * @param string $agreement
     * @param array $defACCDindividual Array with the fields defined to retrieve data with COLOCACION = N
     * @param array $defACCDactions Array with the fields defined to retrieve data of the SEPE actions
     * @param array $defACCDindividualCol Array with the rest of the fields to retrieve data with COLOCACION = S
     * @return sqlQuery
     */
    public function getQueryMonthlyACCD($month, $year, $agreement, $defACCDindividual, $defACCDactions, $defACCDindividualCol) {
        $sqlIndiv = '';
        $defACCD = array_merge($defACCDindividual, $defACCDactions);
        $sqlIndiv = $this->buildSelectParams($defACCD, $sqlIndiv);    
        $sqlQuery['NOCOL'] = 'SELECT ' .$sqlIndiv. ", 'N' as COLOCACION FROM ".$this->getContactsNestedSQL($year, $month, $agreement). SQLbuilder::joinTablesNoCol ." WHERE ".$this->getWhereQueryACCDNoCol($month, $year) ;

        $sqlIndiv = '';
        $sqlIndiv = $this->buildSelectParams($defACCDindividual, $sqlIndiv);
        $sqlIndiv = $sqlIndiv .", 'S' as COLOCACION ";
        $sqlIndiv = $this->buildSelectParams($defACCDindividualCol, $sqlIndiv);
        $sqlQuery['COL'] = 'SELECT DISTINCT '.$sqlIndiv." FROM ".$this->getContactsNestedSQL($year, $month, $agreement)." ".SQLbuilder::joinTablesNoCol.SQLbuilder::joinTablesCol."WHERE".$this->getWhereQueryCol($month, $year); 

        return $sqlQuery;
    }

    /**
     * It buildes the complete query to retrieve the incidents data of the ACCI file
     * It uses the definition of the fields specified in SEPEgetter.php
     * 
     * @param integer $month
     * @param integer $year
     * @param string $agreement
     * @param array $defACCI Array with the fields defined to retrieve data of the SEPE incidents
     * @return sqlQuery
     */
    public function getQueryMonthlyACCI($month, $year, $agreement, $defACCI) {
        $sqlIndiv = $this->buildSelectParams($defACCI);    
        $sqlQuery = 'SELECT ' .$sqlIndiv. " FROM ".$this->getContactsNestedSQL($year, $month, $agreement). SQLbuilder::joinTablesACCI ." WHERE ".$this->getWhereQueryACCI($month, $year);
        return $sqlQuery;
    }

    /**
     * It builds the complete queries to retrieve the aggregated data for AC and ACCD files
     * One query per element needed in the XML. The queries are written in the order that need to appear in the XML file
     * 
     * @param integer $month
     * @param integer $year
     * @param string $file Used to define if the data is for AC or ACCD
     * @return query
     */
    public function getQueryAggregated($type, $year, $month = false, $agreement = false) {
        $date = $month == 12 ? ($year+1)."-01-01" : "$year-".($month+1)."-01";
        if (!$month) $date = ($year+1)."-01-01";
        if ($type != 'ACCD') {
            $query['TOTAL_PERSONAS'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_PERSONAS FROM "
                .$this->getContactsNestedSQL($year, $month).
            " LEFT JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            LEFT JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            LEFT JOIN stic_job_applications_contacts_c ON
                stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
            LEFT JOIN stic_job_applications ON
                stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
            LEFT JOIN stic_job_applications_stic_job_offers_c ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
            LEFT JOIN stic_job_offers ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
            WHERE
                contacts.deleted = 0
                AND ((stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0
                AND stic_sepe_actions.type = 'A'
                AND YEAR(stic_sepe_actions.start_date) = $year "
                .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month " : null) .")". 
                "OR (
                    stic_job_applications.deleted = 0
                    AND stic_job_applications_contacts_c.deleted = 0
                    AND stic_job_offers.deleted = 0
                    AND stic_job_applications_stic_job_offers_c.deleted = 0
                    AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
                    AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
                    AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
                    AND YEAR(stic_job_applications.contract_start_date) = $year "
                    .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month": null)."))";
            $query['TOTAL_PERSONAS_PERCEPTORES'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_PERSONAS_PERCEPTORES
            FROM
            ".$this->getContactsNestedSQL($year, $month, $agreement)."
            INNER JOIN contacts_cstm ON
                contacts.id = contacts_cstm.id_c
            LEFT JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            LEFT JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            LEFT JOIN stic_job_applications_contacts_c ON
                stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
            LEFT JOIN stic_job_applications ON
                stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
            LEFT JOIN stic_job_applications_stic_job_offers_c ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
            LEFT JOIN stic_job_offers ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
            WHERE
                contacts.deleted = 0
                AND contacts_cstm.sepe_benefit_perceiver_c = 'S'
                AND ((stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0
                AND stic_sepe_actions.type = 'A'
                AND YEAR(stic_sepe_actions.start_date) = $year "
                .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month " : null) .")". 
                "OR (
                    stic_job_applications.deleted = 0
                    AND stic_job_applications_contacts_c.deleted = 0
                    AND stic_job_offers.deleted = 0
                    AND stic_job_applications_stic_job_offers_c.deleted = 0
                    AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
                    AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
                    AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
                    AND YEAR(stic_job_applications.contract_start_date) = $year "
                    .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month": null)."))";

            $query['TOTAL_PERSONAS_INSERCION'] = "SELECT
            count(DISTINCT contacts.id) as TOTAL_PERSONAS_INSERCION
            FROM
            ".$this->getContactsNestedSQL($year, $month, $agreement)."
            INNER JOIN contacts_cstm ON
                contacts.id = contacts_cstm.id_c
            LEFT JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            LEFT JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            LEFT JOIN stic_job_applications_contacts_c ON
                stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
            LEFT JOIN stic_job_applications ON
                stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
            LEFT JOIN stic_job_applications_stic_job_offers_c ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
            LEFT JOIN stic_job_offers ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
            WHERE
                contacts.deleted = 0
                AND contacts_cstm.sepe_insertion_difficulties_c = 'S'
                AND ((stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0
                AND stic_sepe_actions.type = 'A'
                AND YEAR(stic_sepe_actions.start_date) = $year "
                .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month " : null) .")". 
                "OR (
                    stic_job_applications.deleted = 0
                    AND stic_job_applications_contacts_c.deleted = 0
                    AND stic_job_offers.deleted = 0
                    AND stic_job_applications_stic_job_offers_c.deleted = 0
                    AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
                    AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
                    AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
                    AND YEAR(stic_job_applications.contract_start_date) = $year "
                    .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month": null)."))";

            $query['TOTAL_NUEVAS_REGISTRADAS'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_NUEVAS_REGISTRADAS
            FROM
                contacts
            INNER JOIN contacts_cstm ON
                contacts.id = contacts_cstm.id_c
            INNER JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            INNER JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            WHERE
                contacts.deleted = 0
                AND stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0
                AND stic_sepe_actions.type = 'A'
                ".($agreement ? "AND stic_sepe_actions.agreement = '$agreement'" : "AND (stic_sepe_actions.agreement = '' OR stic_sepe_actions.agreement IS NULL)")."
                AND YEAR(stic_sepe_actions.start_date) = $year "
                .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month" : null);
        } else {
            $query['TOTAL_PERSONAS_ATENDIDAS'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_PERSONAS_ATENDIDAS
                FROM
            ".$this->getContactsNestedSQL($year, $month, $agreement)."
            LEFT JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            LEFT JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            LEFT JOIN stic_job_applications_contacts_c ON
                stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
            LEFT JOIN stic_job_applications ON
                stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
            LEFT JOIN stic_job_applications_stic_job_offers_c ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
            LEFT JOIN stic_job_offers ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
            WHERE
                contacts.deleted = 0
                AND ((stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0
                AND stic_sepe_actions.type != 'A'
                AND contacts.sepe_start_date <= stic_sepe_actions.start_date
                AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_sepe_actions.start_date)
                AND YEAR(stic_sepe_actions.start_date) = $year "
                .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month " : null) .")". 
                "OR (
                    stic_job_applications.deleted = 0
                    AND stic_job_applications_contacts_c.deleted = 0
                    AND stic_job_offers.deleted = 0
                    AND stic_job_applications_stic_job_offers_c.deleted = 0
                    AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
                    AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
                    AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
                    AND YEAR(stic_job_applications.contract_start_date) = $year "
                    .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month": null)."))";
            $query['TOTAL_PERSONAS_PERCEPTORES'] = "SELECT
                    count(DISTINCT contacts.id) as TOTAL_PERSONAS_PERCEPTORES
                FROM
                ".$this->getContactsNestedSQL($year, $month, $agreement)."
                INNER JOIN contacts_cstm ON
                    contacts.id = contacts_cstm.id_c
                LEFT JOIN stic_sepe_actions_contacts_c ON
                    contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
                LEFT JOIN stic_sepe_actions ON
                    stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
                LEFT JOIN stic_job_applications_contacts_c ON
                    stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
                LEFT JOIN stic_job_applications ON
                    stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
                LEFT JOIN stic_job_applications_stic_job_offers_c ON
                    stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
                LEFT JOIN stic_job_offers ON
                    stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
                WHERE
                    contacts.deleted = 0
                    AND contacts_cstm.sepe_benefit_perceiver_c = 'S'
                    AND ((stic_sepe_actions_contacts_c.deleted = 0
                    AND stic_sepe_actions.deleted = 0
                    AND stic_sepe_actions.type != 'A'
                    AND contacts.sepe_start_date <= stic_sepe_actions.start_date
                    AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_sepe_actions.start_date)
                    AND YEAR(stic_sepe_actions.start_date) = $year "
                    .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month " : null) .")". 
                    "OR (
                        stic_job_applications.deleted = 0
                        AND stic_job_applications_contacts_c.deleted = 0
                        AND stic_job_offers.deleted = 0
                        AND stic_job_applications_stic_job_offers_c.deleted = 0
                        AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
                        AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
                        AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
                        AND YEAR(stic_job_applications.contract_start_date) = $year "
                        .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month": null)."))";
        
            $query['TOTAL_PERSONAS_INSERCION'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_PERSONAS_INSERCION
                FROM
                ".$this->getContactsNestedSQL($year, $month, $agreement)."
                INNER JOIN contacts_cstm ON
                    contacts.id = contacts_cstm.id_c
                LEFT JOIN stic_sepe_actions_contacts_c ON
                    contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
                LEFT JOIN stic_sepe_actions ON
                    stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
                LEFT JOIN stic_job_applications_contacts_c ON
                    stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
                LEFT JOIN stic_job_applications ON
                    stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
                LEFT JOIN stic_job_applications_stic_job_offers_c ON
                    stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
                LEFT JOIN stic_job_offers ON
                    stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
                WHERE
                    contacts.deleted = 0
                    AND contacts_cstm.sepe_insertion_difficulties_c = 'S'
                    AND ((stic_sepe_actions_contacts_c.deleted = 0
                    AND stic_sepe_actions.deleted = 0
                    AND stic_sepe_actions.type != 'A'
                    AND contacts.sepe_start_date <= stic_sepe_actions.start_date
                    AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_sepe_actions.start_date)
                    AND YEAR(stic_sepe_actions.start_date) = $year "
                    .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month " : null) .")". 
                    "OR (
                        stic_job_applications.deleted = 0
                        AND stic_job_applications_contacts_c.deleted = 0
                        AND stic_job_offers.deleted = 0
                        AND stic_job_applications_stic_job_offers_c.deleted = 0
                        AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
                        AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
                        AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
                        AND YEAR(stic_job_applications.contract_start_date) = $year "
                        .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month": null)."))";

            $query['TOTAL_PERSONAS_ENVIADAS'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_PERSONAS_ENVIADAS
            FROM
                ".$this->getContactsNestedSQL($year, $month, $agreement)."
            INNER JOIN stic_job_applications_contacts_c ON
                stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
            INNER JOIN stic_job_applications ON
                stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
            INNER JOIN stic_job_applications_stic_job_offers_c ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
            INNER JOIN stic_job_offers ON
                stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
            WHERE
                stic_job_applications_contacts_c.deleted = 0 
                AND stic_job_applications.deleted = 0
                AND stic_job_offers.sepe_activation_date <= stic_job_applications.start_date
                AND contacts.sepe_start_date <= stic_job_applications.start_date
                AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.start_date)
                AND	YEAR(stic_job_applications.start_date) = $year "
                .($month ? "AND	MONTH(stic_job_applications.start_date) = $month" : null);

            $query['TOTAL_PERSONAS_ACCIONES'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_PERSONAS_ACCIONES
            FROM
                ".$this->getContactsNestedSQL($year, $month, $agreement)."
            INNER JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            INNER JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            WHERE
                contacts.deleted = 0
                AND stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0
                AND stic_sepe_actions.type != 'A'
                AND contacts.sepe_start_date <= stic_sepe_actions.start_date
                AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_sepe_actions.start_date)
                AND YEAR(stic_sepe_actions.start_date) = $year "
                .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month" : null); 
            
            $query['TOTAL_PERSONAS_ACCION_ORIENTACION'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_PERSONAS_ACCION_ORIENTACION
            FROM
                ".$this->getContactsNestedSQL($year, $month, $agreement)."
            INNER JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            INNER JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            WHERE
                contacts.deleted = 0
                AND stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0
                AND stic_sepe_actions.type = 'O'
                AND contacts.sepe_start_date <= stic_sepe_actions.start_date
                AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_sepe_actions.start_date)
                AND YEAR(stic_sepe_actions.start_date) = $year "
                .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month" : null); 

            $query['TOTAL_PERSONAS_ACCION_INFORMACION'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_PERSONAS_ACCION_INFORMACION
            FROM
                ".$this->getContactsNestedSQL($year, $month, $agreement)."
            INNER JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            INNER JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            WHERE
                contacts.deleted = 0
                AND stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0
                AND stic_sepe_actions.type = 'I'
                AND contacts.sepe_start_date <= stic_sepe_actions.start_date
                AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_sepe_actions.start_date)
                AND YEAR(stic_sepe_actions.start_date) = $year "
                .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month" : null); 

            $query['TOTAL_PERSONAS_ACCION_FORMACION'] = "SELECT
                count(DISTINCT contacts.id) as TOTAL_PERSONAS_ACCION_FORMACION
            FROM
                ".$this->getContactsNestedSQL($year, $month, $agreement)."
            INNER JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            INNER JOIN stic_sepe_actions ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
            WHERE
                contacts.deleted = 0
                AND stic_sepe_actions_contacts_c.deleted = 0
                AND stic_sepe_actions.deleted = 0
                AND stic_sepe_actions.type = 'F'
                AND contacts.sepe_start_date <= stic_sepe_actions.start_date
                AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_sepe_actions.start_date)
                AND YEAR(stic_sepe_actions.start_date) = $year "
                .($month ? "AND MONTH(stic_sepe_actions.start_date) = $month" : null); 
        }

        $query['TOTAL_OFERTAS'] = "SELECT
            count(DISTINCT stic_job_offers.id) as TOTAL_OFERTAS
        FROM
            stic_job_offers
        WHERE
            stic_job_offers.deleted = 0
            AND stic_job_offers.deleted = 0
            AND YEAR(stic_job_offers.sepe_activation_date) = $year "
            .($month ? "AND MONTH(stic_job_offers.sepe_activation_date) = $month " : null);

        $query['TOTAL_OFERTAS_CUBIERTAS'] = "SELECT
            count(DISTINCT stic_job_offers.id) as TOTAL_OFERTAS_CUBIERTAS
        FROM
            stic_job_offers 
        WHERE
            stic_job_offers.deleted = 0
            AND stic_job_offers.sepe_activation_date < '$date'
            AND YEAR(stic_job_offers.sepe_covered_date) = $year "
            .($month ? "AND MONTH(stic_job_offers.sepe_covered_date) = $month": null);

        $query['TOTAL_PUESTOS_CUBIERTOS'] = "SELECT
            count(DISTINCT stic_job_applications.id) as TOTAL_PUESTOS_CUBIERTOS
        FROM
            ".$this->getContactsNestedSQL($year, $month, $agreement)."
        INNER JOIN stic_job_applications_contacts_c ON
            stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
        INNER JOIN stic_job_applications ON
            stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
        INNER JOIN stic_job_applications_stic_job_offers_c ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
        INNER JOIN stic_job_offers ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
        WHERE
            stic_job_applications_contacts_c.deleted = 0
            AND stic_job_applications.deleted = 0
            AND stic_job_offers.deleted = 0
            AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
            AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
            AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
            AND YEAR(stic_job_applications.contract_start_date) = $year "
            .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month" : null);

        $query['TOTAL_CONTRATOS'] = "SELECT
            count(DISTINCT stic_job_applications.id) as TOTAL_CONTRATOS
        FROM
            ".$this->getContactsNestedSQL($year, $month, $agreement)."
        INNER JOIN stic_job_applications_contacts_c ON
            stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
        INNER JOIN stic_job_applications ON
            stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
        INNER JOIN stic_job_applications_stic_job_offers_c ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
        INNER JOIN stic_job_offers ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
        WHERE
            stic_job_applications_contacts_c.deleted = 0
            AND stic_job_applications.deleted = 0
            AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
            AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
            AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
            AND YEAR(stic_job_applications.contract_start_date) = $year "
            .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month" : null);

        $query['TOTAL_CONTRATOS_INDEFINIDOS'] = "SELECT
            count(DISTINCT stic_job_applications.id) as TOTAL_CONTRATOS_INDEFINIDOS
        FROM
            ".$this->getContactsNestedSQL($year, $month, $agreement)."
        INNER JOIN stic_job_applications_contacts_c ON
            stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
        INNER JOIN stic_job_applications ON
            stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
        INNER JOIN stic_job_applications_stic_job_offers_c ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
        INNER JOIN stic_job_offers ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
        WHERE
            stic_job_applications.deleted = 0
            AND stic_job_applications_contacts_c.deleted = 0
            AND stic_job_applications_stic_job_offers_c.deleted = 0
            AND stic_job_offers.deleted = 0
            AND (stic_job_offers.sepe_contract_type = '001' OR stic_job_offers.sepe_contract_type = '003')
            AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
            AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
            AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
            AND YEAR(stic_job_applications.contract_start_date) = $year "
            .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month" : null);

        $query['TOTAL_PUESTOS'] = "SELECT
            COALESCE(SUM(stic_job_offers.offered_positions),0) as TOTAL_PUESTOS
        FROM
            stic_job_offers
        WHERE
            stic_job_offers.deleted = 0
            AND YEAR(stic_job_offers.sepe_activation_date) = $year "
            .($month ? "AND MONTH(stic_job_offers.sepe_activation_date) = $month" : null);

        $query['TOTAL_OFERTAS_ENVIADAS'] = "SELECT
            count(DISTINCT contacts.id) as TOTAL_OFERTAS_ENVIADAS
        FROM
            ".$this->getContactsNestedSQL($year, $month, $agreement)."
        INNER JOIN stic_job_applications_contacts_c ON
            stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
        INNER JOIN stic_job_applications ON
            stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
        INNER JOIN stic_job_applications_stic_job_offers_c ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
        INNER JOIN stic_job_offers ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
        WHERE
            stic_job_applications_contacts_c.deleted = 0 
            AND stic_job_applications.deleted = 0
            AND stic_job_offers.sepe_activation_date <= stic_job_applications.start_date
            AND contacts.sepe_start_date <= stic_job_applications.start_date
            AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.start_date)
            AND	YEAR(stic_job_applications.start_date) = $year "
            .($month ? "AND	MONTH(stic_job_applications.start_date) = $month" : null);

        $query['TOTAL_PERSONAS_COLOCADAS'] = "SELECT
            count(DISTINCT contacts.id) as TOTAL_PERSONAS_COLOCADAS
        FROM
            ".$this->getContactsNestedSQL($year, $month, $agreement)."
        INNER JOIN stic_job_applications_contacts_c ON
            stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida = contacts.id
        INNER JOIN stic_job_applications ON
            stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
        INNER JOIN stic_job_applications_stic_job_offers_c ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb = stic_job_applications.id
        INNER JOIN stic_job_offers ON
            stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida = stic_job_offers.id
        WHERE
            stic_job_applications_contacts_c.deleted = 0
            AND stic_job_applications.deleted = 0
            AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
            AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
            AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date)
            AND YEAR(stic_job_applications.contract_start_date) = $year "
            .($month ? "AND MONTH(stic_job_applications.contract_start_date) = $month" : null); 
        return $query;
    }

    /**
     * These are the queries that are used to validate which date might not be consistent and won't be included in the XML File
     * Some are the cases are:
     *  - A Person with a SEPE but it is not activated with a SEPE action
     *  - A Person that is activated with a SEPE action but the OFFERS it applied are not SEPE
     *  - A Person that has ACCD Actions but hasn't a SEPE activation action
     *  - A Person that has Incidents but hasn't a SEPE activation action
     *
     * @param integer $month
     * @param integer $year
     * @param string $agreement true if file has an agreement
     * @param boolean $acci true if the file is acci
     * @return $query
     */
    public function dataConsistencyValidationQueries($year, $month, $agreement = false, $acci = false) {
        $date = $month == 12 ? ($year+1)."-01-01" : "$year-".($month+1)."-01";
        if (!$month) $date = ($year+1)."-01-01";

        $query['JOB_OFFER_NOT_SEPE_ACTIVATED']['module'] = 'stic_Job_Offers';
        $query['JOB_OFFER_NOT_SEPE_ACTIVATED']['query'] = "
        SELECT
            DISTINCT stic_job_offers.id as ID,
            stic_job_offers.name as NAME 
        FROM "
            .$this->getContactsNestedSQL($year, $month, $agreement)."
        INNER JOIN stic_job_applications_contacts_c ON
            contacts.id = stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida
        INNER JOIN stic_job_applications ON
            stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
        INNER JOIN stic_job_applications_stic_job_offers_c ON
            stic_job_applications.id = stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb
        INNER JOIN stic_job_offers ON
            stic_job_offers.id = stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida
        WHERE
            stic_job_applications.deleted = 0
            AND stic_job_applications_contacts_c.deleted = 0
            AND contacts.deleted = 0
            AND (
                (MONTH(stic_job_applications.start_date) = $month
                AND YEAR(stic_job_applications.start_date) = $year
                AND (stic_job_offers.sepe_activation_date IS NULL OR stic_job_offers.sepe_activation_date > stic_job_applications.start_date)
                AND contacts.sepe_start_date <= stic_job_applications.start_date
                AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.start_date))
                OR
                (MONTH(stic_job_applications.contract_start_date) = $month
                AND YEAR(stic_job_applications.contract_start_date) = $year
                AND (stic_job_offers.sepe_activation_date IS NULL OR stic_job_offers.sepe_activation_date > stic_job_applications.contract_start_date)
                AND contacts.sepe_start_date <= stic_job_applications.contract_start_date
                AND (contacts.sepe_end_date IS NULL OR contacts.sepe_end_date >= stic_job_applications.contract_start_date))
                )
        ";

        $query['JOB_OFFER_COVERED_NOT_ACTIVATED']['module'] = 'stic_Job_Offers';
        $query['JOB_OFFER_COVERED_NOT_ACTIVATED']['query'] = "
        SELECT 
            stic_job_offers.id as ID,
            stic_job_offers.name as NAME
        FROM
            stic_job_offers 
        WHERE 
            stic_job_offers.deleted = 0
            AND (
                (MONTH(stic_job_offers.sepe_covered_date) = $month
                AND YEAR(stic_job_offers.sepe_covered_date) = $year)
                OR
                (MONTH(stic_job_offers.sepe_activation_date) = $month
                AND YEAR(stic_job_offers.sepe_activation_date) = $year)
                )
            AND (stic_job_offers.sepe_covered_date < stic_job_offers.sepe_activation_date
            OR (stic_job_offers.sepe_activation_date is NULL AND stic_job_offers.sepe_covered_date))";

        $query['CONTACT_AND_JOB_OFFER_NOT_SEPE_ACTIVATED']['module'] = 'stic_Job_Applications';
        $query['CONTACT_AND_JOB_OFFER_NOT_SEPE_ACTIVATED']['query'] = "
        SELECT
            DISTINCT stic_job_applications.id as ID,
            stic_job_applications.name as NAME 
        FROM 
            contacts
        INNER JOIN stic_job_applications_contacts_c ON
            contacts.id = stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida
        INNER JOIN stic_job_applications ON
            stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
        INNER JOIN stic_job_applications_stic_job_offers_c ON
            stic_job_applications.id = stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb
        INNER JOIN stic_job_offers ON
            stic_job_offers.id = stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida
        WHERE
            stic_job_applications.deleted = 0
            AND stic_job_applications_contacts_c.deleted = 0
            AND contacts.deleted = 0
            AND (
                (MONTH(stic_job_applications.start_date) = $month
                AND YEAR(stic_job_applications.start_date) = $year
                AND MONTH(stic_job_offers.sepe_activation_date) = $month
                AND YEAR(stic_job_offers.sepe_activation_date) = $year
                AND stic_job_offers.sepe_activation_date > stic_job_applications.start_date
                AND contacts.id not in (
                    SELECT contacts.id from contacts
                    INNER JOIN stic_sepe_actions_contacts_c ON
                        contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
                    INNER JOIN stic_sepe_actions ON
                        stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
                    WHERE
                        contacts.deleted = 0
                        AND stic_sepe_actions_contacts_c.deleted = 0
                        AND stic_sepe_actions.deleted = 0
                        AND stic_sepe_actions.type = 'A'
                        ".($agreement ? "AND stic_sepe_actions.agreement = '$agreement'" : 
                        "AND (stic_sepe_actions.agreement = '' OR stic_sepe_actions.agreement IS NULL)")."
                        AND ( 
                            stic_sepe_actions.start_date <= stic_job_applications.start_date
                            OR (
                                YEAR(stic_sepe_actions.start_date) != $year
                                AND MONTH(stic_sepe_actions.start_date) != $month
                                )
                            )
                        AND (stic_sepe_actions.end_date IS NULL OR stic_sepe_actions.end_date >= stic_job_applications.start_date)))
                OR
                (MONTH(stic_job_applications.contract_start_date) = $month
                AND YEAR(stic_job_applications.contract_start_date) = $year
                AND MONTH(stic_job_offers.sepe_activation_date) = $month
                AND YEAR(stic_job_offers.sepe_activation_date) = $year
                AND (stic_job_offers.sepe_activation_date IS NULL OR stic_job_offers.sepe_activation_date > stic_job_applications.contract_start_date)
                AND contacts.id not in (
                    SELECT contacts.id from contacts
                    INNER JOIN stic_sepe_actions_contacts_c ON
                        contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
                    INNER JOIN stic_sepe_actions ON
                        stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
                        WHERE
                        contacts.deleted = 0
                        AND stic_sepe_actions_contacts_c.deleted = 0
                        AND stic_sepe_actions.deleted = 0
                        AND stic_sepe_actions.type = 'A'
                        ".($agreement ? "AND stic_sepe_actions.agreement = '$agreement'" : 
                        "AND (stic_sepe_actions.agreement = '' OR stic_sepe_actions.agreement IS NULL)")."
                        AND ( 
                            stic_sepe_actions.start_date <= stic_job_applications.contract_start_date
                            OR (
                                YEAR(stic_sepe_actions.start_date) != $year
                                AND MONTH(stic_sepe_actions.start_date) != $month
                                )
                            )
                        AND (stic_sepe_actions.end_date IS NULL OR stic_sepe_actions.end_date >= stic_job_applications.contract_start_date)))
                )
            ";

        if (!$agreement) {
            $query['CONTACT_NOT_SEPE_ACTIVATED']['module'] = 'Contacts';
            $query['CONTACT_NOT_SEPE_ACTIVATED']['query'] = "
            SELECT
                DISTINCT contacts.id as ID,
                CONCAT(contacts.first_name, ' ', contacts.last_name) as NAME 
            FROM 
                contacts
            INNER JOIN stic_job_applications_contacts_c ON
                contacts.id = stic_job_applications_contacts_c.stic_job_applications_contactscontacts_ida
            INNER JOIN stic_job_applications ON
                stic_job_applications.id = stic_job_applications_contacts_c.stic_job_applications_contactsstic_job_applications_idb
            INNER JOIN stic_job_applications_stic_job_offers_c ON
                stic_job_applications.id = stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_applications_idb
            INNER JOIN stic_job_offers ON
                stic_job_offers.id = stic_job_applications_stic_job_offers_c.stic_job_applications_stic_job_offersstic_job_offers_ida
            WHERE
                stic_job_applications.deleted = 0
                AND stic_job_applications_contacts_c.deleted = 0
                AND contacts.deleted = 0
                AND (
                    (MONTH(stic_job_applications.start_date) = $month
                    AND YEAR(stic_job_applications.start_date) = $year
                    AND stic_job_offers.sepe_activation_date <= stic_job_applications.start_date
                    AND contacts.id not in (
                        SELECT contacts.id from contacts
                        INNER JOIN stic_sepe_actions_contacts_c ON
                            contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
                        INNER JOIN stic_sepe_actions ON
                            stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
                            WHERE
                            contacts.deleted = 0
                            AND stic_sepe_actions_contacts_c.deleted = 0
                            AND stic_sepe_actions.deleted = 0
                            AND stic_sepe_actions.type = 'A'
                            AND stic_sepe_actions.start_date <= stic_job_applications.start_date
                            AND (stic_sepe_actions.end_date IS NULL OR stic_sepe_actions.end_date >= stic_job_applications.start_date)))
                    OR
                    (MONTH(stic_job_applications.contract_start_date) = $month
                    AND YEAR(stic_job_applications.contract_start_date) = $year
                    AND stic_job_offers.sepe_activation_date <= stic_job_applications.contract_start_date
                    AND contacts.id not in (
                        SELECT contacts.id from contacts
                        INNER JOIN stic_sepe_actions_contacts_c ON
                            contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
                        INNER JOIN stic_sepe_actions ON
                            stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
                            WHERE
                            contacts.deleted = 0
                            AND stic_sepe_actions_contacts_c.deleted = 0
                            AND stic_sepe_actions.deleted = 0
                            AND stic_sepe_actions.type = 'A'
                            AND stic_sepe_actions.start_date <= stic_job_applications.contract_start_date
                            AND (stic_sepe_actions.end_date IS NULL OR stic_sepe_actions.end_date >= stic_job_applications.contract_start_date)))
                    )
            ";
            
        } else if ($agreement && !$acci) {
            $query['ACCD_ACTION_SEPE_NOT_ACTIVATED']['module'] = "Contacts";
            $query['ACCD_ACTION_SEPE_NOT_ACTIVATED']['query'] = "
            SELECT
                DISTINCT contacts.id as ID,
                CONCAT(contacts.first_name, ' ', contacts.last_name) as NAME 
            FROM
                contacts
            INNER JOIN stic_sepe_actions_contacts_c ON
                contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
            INNER JOIN stic_sepe_actions ssp ON
                stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = ssp.id
            WHERE
                stic_sepe_actions_contacts_c.deleted = 0
                AND ssp.deleted = 0
                AND ssp.type != 'A'
                AND ssp.type IS NOT NULL
                AND YEAR(ssp.start_date) = $year "
                .($month ? "AND MONTH(ssp.start_date) = $month ": " ").
                "AND contacts.id not in (
                    SELECT contacts.id from contacts
                    INNER JOIN stic_sepe_actions_contacts_c ON
                        contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
                    INNER JOIN stic_sepe_actions ON
                        stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
                        WHERE
                        contacts.deleted = 0
                        AND stic_sepe_actions_contacts_c.deleted = 0
                        AND stic_sepe_actions.deleted = 0
                        AND stic_sepe_actions.start_date <= ssp.start_date
                        AND (ssp.end_date IS NULL OR ssp.end_date >= stic_sepe_actions.start_date)
                        AND stic_sepe_actions.type = 'A'
                        AND stic_sepe_actions.agreement IS NOT NULL)"; 
        }
        if ($acci) {
            $query['ACCI_ACTION_SEPE_NOT_ACTIVATED']['module'] = 'Contacts';
            $query['ACCI_ACTION_SEPE_NOT_ACTIVATED']['query'] = "
            SELECT
                DISTINCT contacts.id as ID,
                CONCAT(contacts.first_name, ' ', contacts.last_name) as NAME 
            FROM
                contacts
            INNER JOIN stic_sepe_incidents_contacts_c ON
                contacts.id = stic_sepe_incidents_contacts_c.stic_sepe_incidents_contactscontacts_ida
            INNER JOIN stic_sepe_incidents ON
                stic_sepe_incidents_contacts_c.stic_sepe_incidents_contactsstic_sepe_incidents_idb = stic_sepe_incidents.id
            WHERE
                stic_sepe_incidents_contacts_c.deleted = 0
                AND stic_sepe_incidents.deleted = 0
                AND stic_sepe_incidents.type IS NOT NULL
                AND YEAR(stic_sepe_incidents.incident_date) = $year
                AND MONTH(stic_sepe_incidents.incident_date) = $month
                AND contacts.id not in (
                    SELECT contacts.id from contacts
                    INNER JOIN stic_sepe_actions_contacts_c ON
                        contacts.id = stic_sepe_actions_contacts_c.stic_sepe_actions_contactscontacts_ida
                    INNER JOIN stic_sepe_actions ON
                        stic_sepe_actions_contacts_c.stic_sepe_actions_contactsstic_sepe_actions_idb = stic_sepe_actions.id
                    WHERE
                        contacts.deleted = 0
                        AND stic_sepe_actions_contacts_c.deleted = 0
                        AND stic_sepe_actions.deleted = 0
                        AND stic_sepe_actions.type = 'A'
                        AND stic_sepe_actions.start_date <= stic_sepe_incidents.incident_date
                        AND (stic_sepe_actions.end_date IS NULL OR stic_sepe_actions.end_date >= stic_sepe_incidents.incident_date)
                        AND stic_sepe_actions.agreement IS NOT NULL)";
            return $query;
        }
        return $query;
    }

    /**
     * It build the SELECT clause, using the arrays of fields defined in SEPEgetter.php
     *
     * @param array $defData Array that contains the field definition
     * @param string $sqlIndiv It is used to build incrementally the SQL query
     * @return sqlIndiv SQL query result
     */
    protected function buildSelectParams($defData, $sqlIndiv = '') {
        foreach ($defData as $name => $fieldData) {
            if ($fieldData[2] === 'date')
                $sqlIndiv = $sqlIndiv . ($sqlIndiv ? "," : "") ." date_format(" .$fieldData[0]."." .$fieldData[1] .", '%Y%m%d') as " . $name;
            else if ($fieldData[2] === 'empty')
                $sqlIndiv = $sqlIndiv . ($sqlIndiv ? "," : "") ." '' as " . $name;
            else if ($fieldData[1])
                $sqlIndiv = $sqlIndiv . ($sqlIndiv ? "," : "") ." " .$fieldData[0]."." .$fieldData[1] ." as " . $name;
        }
        return $sqlIndiv;
    }
}