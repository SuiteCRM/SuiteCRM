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

if (file_exists('custom/include/AdvancedTabConfig.php')) {
    include_once 'custom/include/AdvancedTabConfig.php';
}

// If the menu is not defined in the instance, the default menu is loaded
if (empty($GLOBALS["SticTabStructure"])) {
    $GLOBALS["SticTabStructure"] = array(
        0 => array(
            'id' => 'LBL_GROUPTAB_MAIN',
            'children' => array(
                0 => array(
                    'id' => 'Contacts',
                ),
                1 => array(
                    'id' => 'stic_Contacts_Relationships',
                ),
                2 => array(
                    'id' => 'Accounts',
                    'children' => array(
                        0 => array(
                            'id' => 'stic_Centers',
                        ),
                    ),
                ),
                3 => array(
                    'id' => 'stic_Accounts_Relationships',
                ),
                4 => array(
                    'id' => 'Leads',
                ),
                5 => array(
                    'id' => 'Project',
                ),
            ),
        ),
        1 => array(
            'id' => 'LBL_GROUPTAB_ACTIVITIES',
            'children' => array(
                0 => array(
                    'id' => 'Calendar',
                ),
                1 => array(
                    'id' => 'Calls',
                ),
                2 => array(
                    'id' => 'Meetings',
                ),
                3 => array(
                    'id' => 'Tasks',
                ),
                4 => array(
                    'id' => 'Notes',
                ),
                5 => array(
                    'id' => 'Documents',
                ),
                6 => array(
                    'id' => 'Emails',
                ),
                7 => array(
                    'id' => 'stic_Work_Calendar',
                ),
                8 => array(
                    'id' => 'stic_Time_Tracker',
                ),
            ),
        ),
        2 => array(
            'id' => 'LBL_GROUPTAB_CAMPAIGNS',
            'children' => array(
                0 => array(
                    'id' => 'Campaigns',
                ),
                1 => array(
                    'id' => 'ProspectLists',
                ),
                2 => array(
                    'id' => 'Surveys',
                ),
            ),
        ),
        3 => array(
            'id' => 'LBL_GROUPTAB_ECONOMY',
            'children' => array(
                0 => array(
                    'id' => 'Opportunities',
                    'children' => array(
                        0 => array(
                            'id' => 'stic_Group_Opportunities',
                        ),
                    ),
                ),
                1 => array(
                    'id' => 'stic_Payment_Commitments',
                ),
                2 => array(
                    'id' => 'stic_Payments',
                ),
                3 => array(
                    'id' => 'stic_Remittances',
                ),
                4 => array(
                    'id' => 'LBL_GROUPTAB_SALES',
                    'children' => array(
                        0 => array(
                            'id' => 'AOS_Products',
                        ),
                        1 => array(
                            'id' => 'AOS_Product_Categories',
                        ),
                        2 => array(
                            'id' => 'AOS_Quotes',
                        ),
                        3 => array(
                            'id' => 'AOS_Invoices',
                        ),
                        4 => array(
                            'id' => 'AOS_Contracts',
                        ),
                    ),
                ),
            ),
        ),
        4 => array(
            'id' => 'LBL_GROUPTAB_DIRECTCARE',
            'children' => array(
                0 => array(
                    'id' => 'LBL_GROUPTAB_DC_PERSONALINFO',
                    'children' => array(
                        0 => array(
                            'id' => 'stic_Personal_Environment',
                        ),
                        1 => array(
                            'id' => 'stic_Families',
                        ),
                        2 => array(
                            'id' => 'stic_Skills',
                        ),
                        3 => array(
                            'id' => 'stic_Work_Experience',
                        ),
                        4 => array(
                            'id' => 'stic_Training',
                        ),
                        5 => array(
                            'id' => 'stic_Grants',
                        ),
                    ),
                ),
                1 => array(
                    'id' => 'LBL_GROUPTAB_DC_WORKPLAN',
                    'children' => array(
                        0 => array(
                            'id' => 'stic_Assessments',
                        ),
                        1 => array(
                            'id' => 'stic_Goals',
                        ),
                        2 => array(
                            'id' => 'stic_FollowUps',
                        ),
                    ),
                ),
                2 => array(
                    'id' => 'stic_Journal',
                ),
                3 => array(
                    'id' => 'LBL_GROUPTAB_LABOURINSERTION',
                    'children' => array(
                        0 => array(
                            'id' => 'stic_Job_Offers',
                        ),
                        1 => array(
                            'id' => 'stic_Job_Applications',
                        ),
                        2 => array(
                            'id' => 'stic_Sepe_Actions',
                        ),
                        3 => array(
                            'id' => 'stic_Sepe_Incidents',
                        ),
                        4 => array(
                            'id' => 'stic_Sepe_Files',
                        ),
                        5 => array(
                            'id' => 'stic_Incorpora_Locations',
                        ),
                    ),
                ),
                4 => array(
                    'id' => 'LBL_GROUPTAB_DC_MEDICATION',
                    'children' => array(
                        0 => array(
                            'id' => 'stic_Medication_Log',
                        ),
                        1 => array(
                            'id' => 'stic_Prescription',
                        ),
                        2 => array(
                            'id' => 'stic_Medication',
                        ),
                    ),
                ),
            ),
        ),
        5 => array(
            'id' => 'LBL_GROUPTAB_EVENTS',
            'children' => array(
                0 => array(
                    'id' => 'stic_Events',
                    'children' => array(
                        0 => array(
                            'id' => 'FP_Event_Locations',
                        ),
                    ),
                ),
                1 => array(
                    'id' => 'stic_Registrations',
                ),
                2 => array(
                    'id' => 'stic_Sessions',
                ),
                3 => array(
                    'id' => 'stic_Attendances',
                ),
            ),
        ),
        6 => array(
            'id' => 'LBL_GROUPTAB_BOOKINGS',
            'children' => array(
                0 => array(
                    'id' => 'stic_Resources',
                ),
                1 => array(
                    'id' => 'stic_Bookings',
                ),
                2 => array(
                    'id' => 'stic_Bookings_Calendar',
                ),
                3 => array(
                    'id' => 'stic_Places',
                ),
                4 => array(
                    'id' => 'stic_Bookings_Places_Calendar',
                ),
            ),
        ),
        7 => array(
            'id' => 'LBL_GROUPTAB_EXPLOITATION',
            'children' => array(
                0 => array(
                    'id' => 'AOS_PDF_Templates',
                ),
                1 => array(
                    'id' => 'DHA_PlantillasDocumentos',
                ),
                2 => array(
                    'id' => 'KReports',
                ),
                3 => array(
                    'id' => 'AOW_WorkFlow',
                ),
                4 => array(
                    'id' => 'SinergiaDA',
                    'url' => 'index.php?module=Home&action=sdaRedirect',
                ),
            ),
        ),
    );
}
