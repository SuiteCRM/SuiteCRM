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
#[\AllowDynamicProperties]
class Favorites extends Basic
{
    public $new_schema = true;
    public $module_dir = 'Favorites';
    public $object_name = 'Favorites';
    public $table_name = 'favorites';
    public $importable = false;
    public $tracker_visibility = false;
    public $disable_row_level_security = true;
    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $parent_id;
    public $parent_type;

    /**
     * @param $id
     * @return bool
     */
    public function deleteFavorite($id)
    {
        if ($id) {
            $favorite_record = BeanFactory::getBean('Favorites', $id);
            $favorite_record->deleted = 1;
            $favorite_record->save();

            return true;
        } else {
            return false;
        }
    }

    /**
     * @param $module
     * @param $record_id
     * @return array
     */
    public function getFavoriteID($module, $record_id)
    {
        global $current_user;
        $db = DBManagerFactory::getInstance();

        $recordIdQuote = $db->quote($record_id);
        $moduleQuote = $db->quote($module);
        $currentUserIdQuote = $db->quote($current_user->id);

        $query = "SELECT id FROM favorites WHERE parent_id= '" . $recordIdQuote .
                "' AND parent_type = '" . $moduleQuote . "' AND assigned_user_id = '" .
                $currentUserIdQuote . "' AND deleted = 0 ORDER BY date_entered DESC";

        return $db->getOne($query);
    }

    /**
     * @param string $id
     * @return array
     */
    public function getCurrentUserSidebarFavorites($id = null)
    {
        global $current_user;
        $db = DBManagerFactory::getInstance();

        $return_array = array();

        $currentUserIdQuote = $db->quote($current_user->id);
        if ($id) {
            $idQuote = $db->quote($id);
            $query = "SELECT parent_id, parent_type FROM favorites WHERE assigned_user_id = '" .
                    $currentUserIdQuote . "' AND parent_id = '" . $idQuote .
                    "' AND deleted = 0 ORDER BY date_entered DESC";
        } else {
            $query = "SELECT parent_id, parent_type FROM favorites WHERE assigned_user_id = '" .
                    $currentUserIdQuote . "' AND deleted = 0 ORDER BY date_entered DESC";
        }

        $result = $db->query($query);

        $i = 0;
        while ($row = $db->fetchByAssoc($result)) {
            $bean = BeanFactory::getBean($row['parent_type'], $row['parent_id']);
            if ($bean) {
                $return_array[$i]['item_summary'] = $bean->name;
                $return_array[$i]['item_summary_short'] = to_html(getTrackerSubstring($bean->name));
                $return_array[$i]['id'] = $row['parent_id'];
                $return_array[$i]['module_name'] = $row['parent_type'];

                // Change since 7.7 side bar icons can exist in images/sidebar.
                $sidebarPath = 'themes/' . SugarThemeRegistry::current() . '/images/sidebar/modules/';
                if (file_exists($sidebarPath)) {
                    $return_array[$i]['image'] = SugarThemeRegistry::current()->getImage('sidebar/modules/' . $row['parent_type'], 'border="0" align="absmiddle"', null, null, '.gif', $bean->name);
                } else {
                    $return_array[$i]['image'] = SugarThemeRegistry::current()->getImage($row['parent_type'], 'border="0" align="absmiddle"', null, null, '.gif', $bean->name);
                }

                ++$i;
            }
        }

        return $return_array;
    }

    /**
     * @parm string $module
     * @return array Representing an array of \SuiteCRM\API\JsonApi\Resource\Resource
     */
    public function getCurrentUserFavoritesForModule($module)
    {
        $db = DBManagerFactory::getInstance();
        global $current_user;
        global $moduleList;

        if (empty($module)) {
            throw new \SuiteCRM\Exception\Exception(
                '[Favorites] [module not specified]',
                \SuiteCRM\Enumerator\ExceptionCode::APPLICATION_UNHANDLED_BEHAVIOUR
            );
        }

        if (in_array($module, $moduleList) === false) {
            throw new \SuiteCRM\Exception\Exception(
                '[Favorites] [module not found] ' . $module,
                \SuiteCRM\Enumerator\ExceptionCode::APPLICTAION_MODULE_NOT_FOUND
            );
        }

        $response = array();

        $currentUserIdQuote = $db->quote($current_user->id);
        $moduleQuote = $db->quote($module);
        $dbResult = $db->query(
            "SELECT parent_id, parent_type FROM favorites " .
            " WHERE assigned_user_id = '" . $currentUserIdQuote . "'" .
            " AND deleted = 0 " .
            " AND parent_type = '" . $moduleQuote . "'" .
            " ORDER BY date_entered DESC "
        );

        while ($row = $db->fetchByAssoc($dbResult)) {
            /** @var \SugarBean $sugarBean */
            $sugarBean = BeanFactory::getBean($row['parent_type'], $row['parent_id']);
            if ($sugarBean !== false) {
                $response[] = array(
                    'id' => $sugarBean->id,
                    'type' => $sugarBean->module_name,
                    'attributes' => array(
                        'name' => $sugarBean->name
                    )
                );
            }
        }

        return $response;
    }

    public function save($notify = false)
    {
        global $current_user;

        if (empty($this->assigned_user_id)) {
            $this->assigned_user_id = $current_user->id;
        }
        parent::save($notify);
    }
    /**
     * @param string $interface
     * @return bool
     */
    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return false;
            default:
                return false;
        }
    }
}
