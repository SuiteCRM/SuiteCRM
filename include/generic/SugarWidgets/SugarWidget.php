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


//TODO move me out of generic



/**
 * Generic Sugar widget
 * @api
 */
#[\AllowDynamicProperties]
class SugarWidget
{
    public $layout_manager = null;
    public $widget_id;
    protected $form_value;
    protected $parent_bean;

    public function __construct(&$layout_manager)
    {
        $this->layout_manager = $layout_manager;
    }

    public function display($layout_def)
    {
        return 'display class undefined';
    }

    /**
     * getSubpanelWidgetId
     * This is a utility function to return a widget's unique id
     * @return id String label of the widget's unique id
     */
    public function getWidgetId()
    {
        return $this->widget_id;
    }

    /**
     * setSubpanelWidgetId
     * This is a utility function to set the id for a widget
     * @param id String value to set the widget's unique id
     */
    public function setWidgetId($id='')
    {
        $this->widget_id = $id;
    }

    public function getDisplayName()
    {
        return $this->form_value;
    }
    public function getParentBean()
    {
        return $this->parent_bean;
    }

    public function setParentBean($parent_bean)
    {
        $this->parent_bean = $parent_bean;
    }
    /**
     * getTruncatedColumnAlias
     * This function ensures that a column alias is no more than 28 characters.  Should the column_name
     * argument exceed 28 charcters, it creates an alias using the first 22 characters of the column_name
     * plus an md5 of the first 6 characters of the lowercased column_name value.
     *
     */
    protected function getTruncatedColumnAlias($column_name)
    {
        if (empty($column_name) || !is_string($column_name) || strlen($column_name) < 28) {
            return $column_name;
        }
        return strtoupper(substr($column_name, 0, 22) . substr(md5(strtolower($column_name)), 0, 6));
    }
}
