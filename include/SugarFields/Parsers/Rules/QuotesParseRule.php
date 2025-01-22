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


require_once('include/SugarFields/Parsers/Rules/BaseRule.php');

class QuotesParseRule extends BaseRule
{
    public function __construct()
    {
    }




    public function preParse($panels, $view)
    {
        if ($view == 'DetailView') {
            foreach ($panels as $name=>$panel) {
                if ($name == 'default') {
                    foreach ($panel as $rowCount=>$row) {
                        foreach ($row as $key=>$column) {
                            if ($this->matches($column, '/billing_address_country/')) {
                                $column['label'] = 'LBL_BILL_TO';
                                $column['name'] = 'billing_address_street';
                                $panels[$name][$rowCount][$key] = $column;
                            } else {
                                if ($this->matches($column, '/shipping_address_country/')) {
                                    $column['label'] = 'LBL_SHIP_TO';
                                    $column['name'] = 'shipping_address_street';
                                    $panels[$name][$rowCount][$key] = $column;
                                } else {
                                    if ($this->matches($column, '/^date_quote_closed$/')) {
                                        $panels[$name][$rowCount][$key] = 'date_quote_expected_closed';
                                    } else {
                                        if ($this->matches($column, '/^tag\.opportunity$/')) {
                                            $panels[$name][$rowCount][$key] = 'opportunity_name';
                                        }
                                    }
                                }
                            }
                        } //foreach
                    } //foreach
                } //if
            } //foreach
        }

        if ($view == 'EditView') {
            $processedBillToPanel = false;

            foreach ($panels as $name=>$panel) {
                // This panel is an exception in that it has nested tables...
                if ($name == 'lbl_bill_to' && !$processedBillToPanel) {
                    $billToPanel = $panel;
                    $newBillPanel = array();
                    foreach ($billToPanel as $subpanel) {
                        $col = array();
                        foreach ($subpanel as $rowCount=>$row) {
                            if (!is_array($row)) {
                                if (!$this->matches($row, '/^(shipping|billing)_address_(street|city|state|country|postalcode)$/si')) {
                                    $col[] = $row;
                                }
                            } else {
                                foreach ($row as $key=>$column) {
                                    if (is_array($column)) {
                                        continue;
                                    }

                                    if ($this->matches($column, '/^(billing|shipping)_(account|contact)_name$/')) {
                                        $match = $this->getMatch($column, '/^(billing|shipping)_(account|contact)_name$/');
                                        $col[$match[0]] = $match[0];
                                    } else {
                                        if (!$this->matches($column, '/^(shipping|billing)_address_(street|city|state|country|postalcode)$/si')) {
                                            $col[] = $column;
                                        }
                                    }
                                } //foreach
                            }
                        } //foreach
                        if (!empty($col)) {
                            $newBillPanel[] = $col;
                        }
                    } //foreach
                    $panels['lbl_bill_to'] = $newBillPanel;
                    $processedBillToPanel = true;
                    continue;
                } //if

                foreach ($panel as $rowCount=>$row) {
                    foreach ($row as $key=>$column) {
                        //We are just going to clean up address fields since we have
                        //to insert a new address panel anyway
                        if ($this->matches($column, '/^(shipping|billing)_address_(street|city|state|country|postalcode)$/si')) {
                            $panels[$name][$rowCount][$key] = '';
                        }
                    } //foreach
                } //foreach
            } //foreach
        }

        return $panels;
    }
}
