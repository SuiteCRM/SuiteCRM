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





global $current_user;

$global_currency_obj = null;

function get_currency()
{
    global $current_user,$global_currency_obj;
    if (empty($global_currency_obj)) {
        $global_currency_obj = BeanFactory::newBean('Currencies');
        //  $global_currency_symbol = '$';

        if ($current_user->getPreference('currency')) {
            $global_currency_obj->retrieve($current_user->getPreference('currency'));
        } else {
            $global_currency_obj->retrieve('-99');
        }
    }
    return $global_currency_obj;
}


class SugarWidgetFieldCurrency extends SugarWidgetFieldInt
{
    public function __construct(&$layout_manager)
    {
        parent::__construct($layout_manager);
        $this->reporter = $this->layout_manager->getAttribute('reporter');
    }

    public function & displayList(&$layout_def)
    {
        global $locale;
        $symbol = $locale->getPrecedentPreference('default_currency_symbol');
        $currency_id = $locale->getPrecedentPreference('currency');

        // If it's not grouped, or if it's grouped around a system currency column, look up the currency symbol so we can display it next to the amount
        if (empty($layout_def['group_function']) || $this->isSystemCurrency($layout_def)) {
            $c = $this->getCurrency($layout_def);
            if (!empty($c['currency_id']) && !empty($c['currency_symbol'])) {
                $symbol = $c['currency_symbol'];
                $currency_id = $c['currency_id'];
            }
        }
        $layout_def['currency_symbol'] = $symbol;
        $layout_def['currency_id'] = $currency_id;
        $display = $this->displayListPlain($layout_def);

        $field_def = [];
        if (!empty($layout_def['column_key'])) {
            $field_def = $this->reporter->all_fields[$layout_def['column_key']];
        } else {
            if (!empty($layout_def['fields'])) {
                $field_def = $layout_def['fields'];
            }
        }
        $record = '';
        if ($layout_def['table_key'] == 'self' && isset($layout_def['fields']['PRIMARYID'])) {
            $record = $layout_def['fields']['PRIMARYID'];
        } else {
            if (isset($layout_def['fields'][strtoupper($layout_def['table_alias']."_id")])) {
                $record = $layout_def['fields'][strtoupper($layout_def['table_alias']."_id")];
            }
        }
        if (!empty($record)) {
            $field_name = $layout_def['name'];
            $field_type = $field_def['type'];
            $module = $field_def['module'];

            $div_id = $module ."&$record&$field_name";
            $str = "<div id='$div_id'>".$display;
            $value = '';
            global $sugar_config;
            if (isset($sugar_config['enable_inline_reports_edit']) && $sugar_config['enable_inline_reports_edit']) {
                $str .= "&nbsp;" .SugarThemeRegistry::current()->getImage("edit_inline", "border='0' alt='Edit Layout' align='bottom' onClick='SUGAR.reportsInlineEdit.inlineEdit(\"$div_id\",\"$value\",\"$module\",\"$record\",\"$field_name\",\"$field_type\",\"$currency_id\",\"$symbol\");'");
            }
            $str .= "</div>";
            return $str;
        } else {
            return $display;
        }
    }

    public function displayListPlain($layout_def)
    {
        $value = currency_format_number(
            parent::displayListPlain($layout_def),
            array_merge(
                array(
                    'convert' => false,
                ),
                $this->getCurrency($layout_def)
            )
        );
        return $value;
    }
    public function queryFilterEquals(&$layout_def)
    {
        return $this->_get_column_select($layout_def)."=".DBManagerFactory::getInstance()->quote(unformat_number($layout_def['input_name0']))."\n";
    }

    public function queryFilterNot_Equals(&$layout_def)
    {
        return $this->_get_column_select($layout_def)."!=".DBManagerFactory::getInstance()->quote(unformat_number($layout_def['input_name0']))."\n";
    }

    public function queryFilterGreater(&$layout_def)
    {
        return $this->_get_column_select($layout_def)." > ".DBManagerFactory::getInstance()->quote(unformat_number($layout_def['input_name0']))."\n";
    }

    public function queryFilterLess(&$layout_def)
    {
        return $this->_get_column_select($layout_def)." < ".DBManagerFactory::getInstance()->quote(unformat_number($layout_def['input_name0']))."\n";
    }

    public function queryFilterBetween(&$layout_def)
    {
        return $this->_get_column_select($layout_def)." > ".DBManagerFactory::getInstance()->quote(unformat_number($layout_def['input_name0'])). " AND ". $this->_get_column_select($layout_def)." < ".DBManagerFactory::getInstance()->quote(unformat_number($layout_def['input_name1']))."\n";
    }

    public function isSystemCurrency(&$layout_def)
    {
        if (strpos((string) $layout_def['name'], '_usdoll') === false) {
            return false;
        } else {
            return true;
        }
    }

    public function querySelect(&$layout_def)
    {
        // add currency column to select
        $table = $this->getCurrencyIdTable($layout_def);
        if ($table) {
            return $this->_get_column_select($layout_def)." ".$this->_get_column_alias($layout_def)." , ".$table.".currency_id ". $this->getTruncatedColumnAlias($this->_get_column_alias($layout_def)."_currency") . "\n";
        }
        return $this->_get_column_select($layout_def)." ".$this->_get_column_alias($layout_def)."\n";
    }

    public function queryGroupBy($layout_def)
    {
        // add currency column to group by
        $table = $this->getCurrencyIdTable($layout_def);
        if ($table) {
            return $this->_get_column_select($layout_def)." , ".$table.".currency_id \n";
        }
        return $this->_get_column_select($layout_def)." \n";
    }

    public function getCurrencyIdTable($layout_def)
    {
        // We need to fetch the currency id as well
        if (!$this->isSystemCurrency($layout_def) && empty($layout_def['group_function'])) {
            if (!empty($layout_def['table_alias'])) {
                $table = $layout_def['table_alias'];
            } else {
                $table = '';
            }

            $real_table = '';
            if (!empty($this->reporter->all_fields[$layout_def['column_key']]['real_table'])) {
                $real_table = $this->reporter->all_fields[$layout_def['column_key']]['real_table'];
            }

            $add_currency_id = false;
            if (!empty($table)) {
                $cols = DBManagerFactory::getInstance()->getHelper()->get_columns($real_table);
                $add_currency_id = isset($cols['currency_id']) ? true : false;

                if (!$add_currency_id && preg_match('/.*?_cstm$/i', (string) $real_table)) {
                    $table = str_replace('_cstm', '', (string) $table);
                    $cols = DBManagerFactory::getInstance()->getHelper()->get_columns($table);
                    $add_currency_id = isset($cols['currency_id']) ? true : false;
                }
                if ($add_currency_id) {
                    return $table;
                }
            }
        }
        return false;
    }

    /**
     * Return currency for layout_def
     * @param mixed $layout_def
     * @return array Array with currency symbol and currency ID
     */
    protected function getCurrency($layout_def)
    {
        $currency_id = false;
        $currency_symbol = false;
        if (isset($layout_def['currency_symbol']) && isset($layout_def['currency_id'])) {
            $currency_symbol = $layout_def['currency_symbol'];
            $currency_id = $layout_def['currency_id'];
        } else {
            $key = strtoupper(isset($layout_def['varname']) ? $layout_def['varname'] : $this->_get_column_alias($layout_def));
            if ($this->isSystemCurrency($layout_def)) {
                $currency_id = '-99';
            } elseif (isset($layout_def['fields'][$key.'_CURRENCY'])) {
                $currency_id = $layout_def['fields'][$key.'_CURRENCY'];
            } elseif (isset($layout_def['fields'][$this->getTruncatedColumnAlias($this->_get_column_alias($layout_def)."_currency")])) {
                $currency_id = $layout_def['fields'][$this->getTruncatedColumnAlias($this->_get_column_alias($layout_def)."_currency")];
            }
            if ($currency_id) {
                $currency = BeanFactory::getBean('Currencies', $currency_id);
                if (!empty($currency ->symbol)) {
                    $currency_symbol = $currency ->symbol;
                }
            }
        }
        return array('currency_symbol' => $currency_symbol, 'currency_id' => $currency_id);
    }
}
