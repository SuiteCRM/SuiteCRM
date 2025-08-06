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

#[\AllowDynamicProperties]
class UpgradeSavedSearch
{
    public function __construct()
    {
        $searchdefs = [];
        $searchFields = [];
        $result = DBManagerFactory::getInstance()->query("SELECT id FROM saved_search");
        while ($row = DBManagerFactory::getInstance()->fetchByAssoc($result)) {
            $focus = BeanFactory::newBean('SavedSearch');
            $focus->retrieve($row['id']);
            $contents = unserialize(base64_decode($focus->contents), ['allowed_classes' => false]);
            $has_team_name_saved = isset($contents['team_name_advanced']) || isset($contents['team_name_basic']) ? true : false;
            //If $contents['searchFormTab'] is set then this is coming from a 4.x saved search
            if (isset($contents['searchFormTab']) && $contents['searchFormTab'] == 'saved_views') {
                $new_contents = array();
                $module = $contents['search_module'];
                $advanced = !empty($contents['advanced']);
                $field_map = array();

                if (file_exists("custom/modules/{$module}/metadata/searchdefs.php")) {
                    require("custom/modules/{$module}/metadata/searchdefs.php");
                    $field_map = $advanced ? $searchdefs[$module]['layout']['advanced_search'] : $searchdefs[$module]['layout']['basic_search'];
                } else {
                    if (file_exists("modules/{$module}/metadata/SearchFields.php")) {
                        require("modules/{$module}/metadata/SearchFields.php");
                        $field_map = $searchFields[$module];
                    } else {
                        $bean = loadBean($module);
                        $field_map = $bean->field_name_map;
                    }
                }

                //Special case for team_id field (from 4.5.x)
                if (isset($contents['team_id'])) {
                    $contents['team_name'] = $contents['team_id'];
                    unset($contents['team_id']);
                }

                foreach ($contents as $key=>$value) {
                    if (isset($field_map[$key])) {
                        $new_key = $key . ($advanced ? '_advanced' : '_basic');
                        if (preg_match('/^team_name_(advanced|basic)$/', $new_key)) {
                            if (!is_array($value)) {
                                $temp_value = array();
                                $teap_value[] = $value;
                                $value = $temp_value;
                            }

                            $team_results = DBManagerFactory::getInstance()->query("SELECT id, name FROM teams where id in ('" . implode("','", $value) . "')");
                            if (!empty($team_results)) {
                                $count = 0;
                                while ($team_row = DBManagerFactory::getInstance()->fetchByAssoc($team_results)) {
                                    $team_key = $new_key . '_collection_' . $count;
                                    $new_contents[$team_key] = $team_row['name'];
                                    $new_contents['id_' . $team_key] = $team_row['id'];
                                    $count++;
                                } //while
                            } //if


                           //Unset the original key
                            unset($new_contents[$key]);

                            //Add the any switch
                            $new_contents[$new_key . '_type'] = 'any';
                        } else {
                            $new_contents[$new_key] = $value;
                        }
                    } else {
                        $new_contents[$key] = $value;
                    }
                }
                $new_contents['searchFormTab'] = $advanced ? 'advanced_search' : 'basic_search';
                $content = base64_encode(serialize($new_contents));
                DBManagerFactory::getInstance()->query("UPDATE saved_search SET contents = '{$content}' WHERE id = '{$row['id']}'");
            } else {
                if ($has_team_name_saved) {
                    //Otherwise, if the boolean has_team_name_saved is set to true, we also need to parse (coming from 5.x)
                    if (isset($contents['team_name_advanced'])) {
                        $team_results = DBManagerFactory::getInstance()->query("SELECT name FROM teams where id = '{$contents['team_name_advanced']}'");
                        if (!empty($team_results)) {
                            $team_row = DBManagerFactory::getInstance()->fetchByAssoc($team_results);
                            $contents['team_name_advanced_collection_0'] = $team_row['name'];
                            $contents['id_team_name_advanced_collection_0'] = $contents['team_name_advanced'];
                            $contents['team_name_advanced_type'] = 'any';
                            unset($contents['team_name_advanced']);
                            $content = base64_encode(serialize($contents));
                            DBManagerFactory::getInstance()->query("UPDATE saved_search SET contents = '{$content}' WHERE id = '{$row['id']}'");
                        }
                    }
                }
            }
        } //while
    }


}
