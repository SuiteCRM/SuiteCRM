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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class StoreQuery
{
    public $query = array();

    public function addToQuery($name, $val)
    {
        $this->query[$name] = $val;
    }

    /**
     * SaveQuery
     *
     * This function handles saving the query parameters to the user preferences.
     * SavedSearch.php does something very similar when saving saved searches as well.
     *
     * @see SavedSearch
     * @param $name String name  to identify this query
     */
    public function SaveQuery($name)
    {
        global $current_user, $timedate;
        if (isset($this->query['module'])) {
            $bean = loadBean($this->query['module']);
            if (!empty($bean)) {
                foreach ($this->query as $key => $value) {
                    //Filter date fields to ensure it is saved to DB format, but also avoid empty values
                    if (!empty($value) && preg_match('/^(start_range_|end_range_|range_)?(.*?)(_advanced|_basic)$/', (string) $key, $match)) {
                        $field = $match[2];
                        if (isset($bean->field_defs[$field]['type']) && empty($bean->field_defs[$field]['disable_num_format'])) {
                            $type = $bean->field_defs[$field]['type'];

                            if (($type == 'date' || $type == 'datetime' || $type == 'datetimecombo') && !preg_match('/^\[.*?\]$/', (string) $value)) {
                                // If the value is already in the db date format (e.g. '2019-03-21'), don't re-convert
                                // it as that causes $db_format to be set to nothing. If the value isn't in
                                // the format that the db wants (e.g. '3/21/2019'), then we can convert it.
                                if (preg_match('/^\d{4}-\d{2}-\d{2}$/', (string) $value)) {
                                    $db_format = $value;
                                } else {
                                    $db_format = $timedate->to_db_date($value, false);
                                }
                                $this->query[$key] = $db_format;
                            } else {
                                if ($type == 'int' || $type == 'currency' || $type == 'decimal' || $type == 'float') {
                                    if (preg_match('/[^\d]/', (string) $value)) {
                                        require_once('modules/Currencies/Currency.php');
                                        $this->query[$key] = unformat_number($value);
                                        //Flag this value as having been unformatted
                                        $this->query[$key . '_unformatted_number'] = true;
                                        //If the type is of currency and there was a currency symbol (non-digit), save the symbol
                                        if ($type == 'currency' && preg_match('/^([^\d])/', (string) $value, $match)) {
                                            $this->query[$key . '_currency_symbol'] = $match[1];
                                        }
                                    } else {
                                        //unset any flags
                                        if (isset($this->query[$key . '_unformatted_number'])) {
                                            unset($this->query[$key . '_unformatted_number']);
                                        }

                                        if (isset($this->query[$key . '_currency_symbol'])) {
                                            unset($this->query[$key . '_currency_symbol']);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        $current_user->setPreference($name . 'Q', $this->query);
    }

    public function clearQuery($name)
    {
        $this->query = array();
        $this->saveQuery($name);
    }

    public function loadQuery($name)
    {
        $saveType = $this->getSaveType($name);
        if ($saveType == 'all' || $saveType == 'myitems') {
            global $current_user;
            $this->query = StoreQuery::getStoredQueryForUser($name);
            if (empty($this->query)) {
                $this->query = array();
            }
            if (!empty($this->populate_only) && !empty($this->query['query'])) {
                $this->query['query'] = 'MSI';
            }
        }
    }

    public function populateRequest()
    {
        global $timedate;

        if (isset($this->query['module'])) {
            $bean = loadBean($this->query['module']);
        }


        foreach ($this->query as $key => $value) {
            // todo wp: remove this
            if ($key != 'advanced' && $key != 'module' && $key != 'lvso') {
                //Filter date fields to ensure it is saved to DB format, but also avoid empty values
                if (!empty($value) && !empty($bean) && preg_match('/^(start_range_|end_range_|range_)?(.*?)(_advanced|_basic)$/', (string) $key, $match)) {
                    $field = $match[2];
                    if (isset($bean->field_defs[$field]['type']) && empty($bean->field_defs[$field]['disable_num_format'])) {
                        $type = $bean->field_defs[$field]['type'];

                        if (($type == 'date' || $type == 'datetime' || $type == 'datetimecombo') && preg_match('/^\d{4}-\d{2}-\d{2}$/', (string) $value) && !preg_match('/^\[.*?\]$/', (string) $value)) {
                            $value = $timedate->to_display_date($value, false);
                        } else {
                            if (($type == 'int' || $type == 'currency' || $type == 'decimal' || $type == 'float') && isset($this->query[$key . '_unformatted_number']) && preg_match('/^\d+$/', (string) $value)) {
                                require_once('modules/Currencies/Currency.php');
                                $value = format_number($value);
                                if ($type == 'currency' && isset($this->query[$key . '_currency_symbol'])) {
                                    $value = $this->query[$key . '_currency_symbol'] . $value;
                                }
                            }
                        }
                    }
                }

                // cn: bug 6546 storequery stomps correct value for 'module' in Activities
                $_REQUEST[$key] = $value;
                $_GET[$key] = $value;
            }
        }
    }

    public function getSaveType($name)
    {
        global $sugar_config;
        $save_query = empty($sugar_config['save_query']) ?
            'all' : $sugar_config['save_query'];

        if (is_array($save_query)) {
            if (isset($save_query[$name])) {
                $saveType = $save_query[$name];
            } elseif (isset($save_query['default'])) {
                $saveType = $save_query['default'];
            } else {
                $saveType = 'all';
            }
        } else {
            $saveType = $save_query;
        }
        if ($saveType == 'populate_only') {
            $saveType = 'all';
            $this->populate_only = true;
        }

        return $saveType;
    }


    public function saveFromRequest($name)
    {
        if (isset($_REQUEST['query'])) {
            if (!empty($_REQUEST['clear_query']) && $_REQUEST['clear_query'] == 'true') {
                $this->loadQuery($name);
                $_REQUEST['displayColumns'] =
                    !empty($this->query['displayColumns']) ? $this->query['displayColumns'] : null;
                $this->clearQuery($name);
                $this->query['displayColumns'] = $_REQUEST['displayColumns'];
                $this->saveQuery($name);

                return;
            }
            $saveType = $this->getSaveType($name);

            if ($saveType == 'myitems') {
                if (!empty($_REQUEST['current_user_only'])) {
                    $this->query['current_user_only'] = $_REQUEST['current_user_only'];
                    $this->query['query'] = true;
                }
                $this->saveQuery($name);
            } else {
                if ($saveType == 'all') {
                    // Bug 39580 - Added 'EmailTreeLayout','EmailGridWidths' to the list as these are added merely as side-effects of the fact that we store the entire
                    // $_REQUEST object which includes all cookies.  These are potentially quite long strings as well.
                    $blockVariables = array('mass', 'uid', 'massupdate', 'delete', 'merge', 'selectCount', 'current_query_by_page', 'EmailTreeLayout', 'EmailGridWidths');
                    if (isset($_REQUEST['use_store_query']) && $_REQUEST['use_stored_query']) {
                        $this->query = array_merge(StoreQuery::getStoredQueryForUser($name), $_REQUEST);
                    } else {
                        $this->query = $_REQUEST;
                    }
                    foreach ($blockVariables as $block) {
                        unset($this->query[$block]);
                    }
                    $this->saveQuery($name);
                }
            }
        }
    }

    public function saveFromGet($name)
    {
        if (isset($_GET['query'])) {
            if (!empty($_GET['clear_query']) && $_GET['clear_query'] == 'true') {
                $this->clearQuery($name);

                return;
            }
            $saveType = $this->getSaveType($name);

            if ($saveType == 'myitems') {
                if (!empty($_GET['current_user_only'])) {
                    $this->query['current_user_only'] = $_GET['current_user_only'];
                    $this->query['query'] = true;
                }
                $this->saveQuery($name);
            } else {
                if ($saveType == 'all') {
                    $this->query = $_GET;
                    $this->saveQuery($name);
                }
            }
        }
    }

    /**
     * Static method to retrieve the user's stored query for a particular module
     *
     * @param string $module
     * @return array
     */
    public static function getStoredQueryForUser($module)
    {
        global $current_user;

        return $current_user->getPreference($module . 'Q');
    }
}
