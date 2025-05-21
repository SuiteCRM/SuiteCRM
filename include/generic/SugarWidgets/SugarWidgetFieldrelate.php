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
class SugarWidgetFieldRelate extends SugarWidgetReportField
{
    /**
     * Method returns HTML of input on configure dashlet page
     *
     * @param array $layout_def definition of a field
     * @return string HTML of select for edit page
     */
    public function displayInput($layout_def)
    {
        $values = array();
        if (is_array($layout_def['input_name0'])) {
            $values = $layout_def['input_name0'];
        } else {
            $values[] = $layout_def['input_name0'];
        }
        $html = '<select name="' . $layout_def['name'] . '[]" multiple="true">';

        $query = $this->displayInputQuery($layout_def);
        $result = $this->reporter->db->query($query);
        while ($row = $this->reporter->db->fetchByAssoc($result)) {
            $html .= '<option value="' . $row['id'] . '"';
            if (in_array($row['id'], $values)) {
                $html .= ' selected="selected"';
            }
            $html .= '>' . htmlspecialchars((string) $row['title']) . '</option>';
        }

        $html .= '</select>';
        return $html;
    }

    /**
     * Method returns database query for generation HTML of input on configure dashlet page
     *
     * @param array $layout_def definition of a field
     * @return string database query HTML of select for edit page
     */
    private function displayInputQuery($layout_def)
    {
        $title = $layout_def['rname'];
        $bean = isset($layout_def['module']) ? BeanFactory::getBean($layout_def['module']) : null;
        $table = empty($bean) ? $layout_def['table'] : $bean->table_name;
        $concat_fields = isset($layout_def['db_concat_fields']) ? $layout_def['db_concat_fields'] : '';

        if (empty($concat_fields) && !empty($bean) && isset($bean->field_defs[$title]['db_concat_fields'])) {
            $concat_fields = $bean->field_defs[$title]['db_concat_fields'];
        }
        if (!empty($concat_fields)) {
            $title = $this->reporter->db->concat($table, $concat_fields);
        }

        $query = "SELECT
                id,
                $title title
            FROM $table
            WHERE deleted = 0
            ORDER BY title ASC";
        return $query;
    }

    /**
     * Method returns part of where in style table_alias.id IN (...) because we can't join of relation
     *
     * @param array $layout_def definition of a field
     * @param bool $rename_columns unused
     * @return string SQL where part
     */
    public function queryFilterStarts_With($layout_def, $rename_columns = true)
    {
        $ids = array();

        $relation = BeanFactory::newBean('Relationships');
        $relation->retrieve_by_name($layout_def['link']);

        global $beanList;
        $beanClass = $beanList[$relation->lhs_module];
        $seed = new $beanClass();
        $seed->retrieve($layout_def['input_name0']);

        $link = new Link2($layout_def['link'], $seed);
        $sql = $link->getQuery();
        $result = $this->reporter->db->query($sql);
        while ($row = $this->reporter->db->fetchByAssoc($result)) {
            $ids[] = $row['id'];
        }
        $layout_def['name'] = 'id';
        return $this->_get_column_select($layout_def) . " IN ('" . implode("', '", $ids) . "')";
    }

    /**
     * Method returns part of where in style table_alias.id IN (...) because we can't join of relation
     *
     * @param array $layout_def definition of a field
     * @param bool $rename_columns unused
     * @return string SQL where part
     */
    public function queryFilterone_of($layout_def, $rename_columns = true)
    {
        $ids = array();
        if (isset($layout_def['link'])) {
            $relation = BeanFactory::newBean('Relationships');
            $relation->retrieve_by_name($layout_def['link']);
        }
        $module = isset($layout_def['custom_module']) ? $layout_def['custom_module'] : $layout_def['module'];
        $seed = BeanFactory::getBean($module);

        foreach ($layout_def['input_name0'] as $beanId) {
            if (!empty($relation->lhs_module) && !empty($relation->rhs_module)
                && $relation->lhs_module == $relation->rhs_module) {
                $filter = array('id');
            } else {
                $filter = array('id', $layout_def['name']);
            }
            $where = $layout_def['id_name']."='$beanId' ";
            $sql = $seed->create_new_list_query('', $where, $filter, array(), 0, '', false, $seed, true);
            $result = $this->reporter->db->query($sql);
            while ($row = $this->reporter->db->fetchByAssoc($result)) {
                $ids[] = $row['id'];
            }
        }
        $ids = array_unique($ids);
        $layout_def['name'] = 'id';
        return $this->_get_column_select($layout_def) . " IN ('" . implode("', '", $ids) . "')";
    }

    //for to_pdf/to_csv
    public function displayListPlain($layout_def)
    {
        $reporter = $this->layout_manager->getAttribute("reporter");
        $field_def = $reporter->all_fields[$layout_def['column_key']];
        $display = strtoupper($field_def['secondary_table'].'_name');
        //#31797  , we should get the table alias in a global registered array:selected_loaded_custom_links
        if (!empty($reporter->selected_loaded_custom_links) && !empty($reporter->selected_loaded_custom_links[$field_def['secondary_table']])) {
            $display = strtoupper($reporter->selected_loaded_custom_links[$field_def['secondary_table']]['join_table_alias'].'_name');
        } elseif (isset($field_def['rep_rel_name']) && isset($reporter->selected_loaded_custom_links) && !empty($reporter->selected_loaded_custom_links[$field_def['secondary_table'].'_'.$field_def['rep_rel_name']])) {
            $display = strtoupper($reporter->selected_loaded_custom_links[$field_def['secondary_table'].'_'.$field_def['rep_rel_name']]['join_table_alias'].'_name');
        } elseif (!empty($reporter->selected_loaded_custom_links) && !empty($reporter->selected_loaded_custom_links[$field_def['secondary_table'].'_'.$field_def['name']])) {
            $display = strtoupper($reporter->selected_loaded_custom_links[$field_def['secondary_table'].'_'.$field_def['name']]['join_table_alias'].'_name');
        }
        $cell = $layout_def['fields'][$display];
        return $cell;
    }

    public function displayList(&$layout_def)
    {
        $reporter = $this->layout_manager->getAttribute("reporter");
        $field_def = $reporter->all_fields[$layout_def['column_key']];
        $display = strtoupper($field_def['secondary_table'].'_name');

        //#31797  , we should get the table alias in a global registered array:selected_loaded_custom_links
        if (!empty($reporter->selected_loaded_custom_links) && !empty($reporter->selected_loaded_custom_links[$field_def['secondary_table']])) {
            $display = strtoupper($reporter->selected_loaded_custom_links[$field_def['secondary_table']]['join_table_alias'].'_name');
        } elseif (isset($field_def['rep_rel_name']) && isset($reporter->selected_loaded_custom_links) && !empty($reporter->selected_loaded_custom_links[$field_def['secondary_table'].'_'.$field_def['rep_rel_name']])) {
            $display = strtoupper($reporter->selected_loaded_custom_links[$field_def['secondary_table'].'_'.$field_def['rep_rel_name']]['join_table_alias'].'_name');
        } elseif (!empty($reporter->selected_loaded_custom_links) && !empty($reporter->selected_loaded_custom_links[$field_def['secondary_table'].'_'.$field_def['name']])) {
            $display = strtoupper($reporter->selected_loaded_custom_links[$field_def['secondary_table'].'_'.$field_def['name']]['join_table_alias'].'_name');
        }
        $recordField = $this->getTruncatedColumnAlias(strtoupper($layout_def['table_alias']).'_'.strtoupper($layout_def['name']));

        $record = $layout_def['fields'][$recordField];
        $cell = "<a target='_blank' class=\"listViewTdLinkS1\" href=\"index.php?action=DetailView&module=".$field_def['ext2']."&record=$record\">";
        $cell .= $layout_def['fields'][$display];
        $cell .= "</a>";
        return $cell;
    }
}
