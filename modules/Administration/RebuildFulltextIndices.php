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

if (!is_admin($current_user)) {
    die('Unauthorized Access. Aborting.');
}

//find  modules that have a full-text index and rebuild it.
global $beanFiles;
foreach ($beanFiles as $beanname=>$beanpath) {
    require_once($beanpath);
    $focus= new $beanname();

    //skips beans based on same tables. user, employee and group are an example.
    if (empty($focus->table_name) || isset($processed_tables[$focus->table_name])) {
        continue;
    } else {
        $processed_tables[$focus->table_name]=$focus->table_name;
    }

    //skips beans based on same tables. user, employee and group are an example.
    if (empty($focus->table_name) || isset($processed_tables[$focus->table_name])) {
        continue;
    }
    $processed_tables[$focus->table_name]=$focus->table_name;

    if (!empty($dictionary[$focus->object_name]['indices'])) {
        $indices=$dictionary[$focus->object_name]['indices'];
    } else {
        $indices=array();
    }

    //clean vardef definitions.. removed indexes not value for this dbtype.
    //set index name as the key.
    $var_indices=array();
    foreach ($indices as $definition) {
        //database helpers do not know how to handle full text indices
        if ($definition['type']=='fulltext') {
            if (isset($definition['db']) && $definition['db'] != DBManagerFactory::getInstance()->dbType) {
                continue;
            }

            echo "Rebuilding Index {$definition['name']} <BR/>";
            DBManagerFactory::getInstance()->query('alter index ' .$definition['name'] . " REBUILD");
        }
    }
}
