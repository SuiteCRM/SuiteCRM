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


define('MB_BASEMETADATALOCATION', 'base');
define('MB_CUSTOMMETADATALOCATION', 'custom');
define('MB_WORKINGMETADATALOCATION', 'working');
define('MB_HISTORYMETADATALOCATION', 'history');
define('MB_GRIDLAYOUTMETADATA', 'gridLayoutMetaData');
define('MB_LISTLAYOUTMETADATA', 'listLayoutMetaData');
define('MB_LISTVIEW', 'listview');
define('MB_SEARCHVIEW', 'searchview');
define('MB_BASICSEARCH', 'basic_search');
define('MB_ADVANCEDSEARCH', 'advanced_search');
define('MB_DASHLET', 'dashlet');
define('MB_DASHLETSEARCH', 'dashletsearch');
define('MB_EDITVIEW', 'editview');
define('MB_DETAILVIEW', 'detailview');
define('MB_QUICKCREATE', 'quickcreate');
define('MB_POPUPLIST', 'popuplist');
define('MB_POPUPSEARCH', 'popupsearch');
define('MB_LABEL', 'label');
define('MB_ONETOONE', 'one-to-one');
define('MB_ONETOMANY', 'one-to-many');
define('MB_MANYTOONE', 'many-to-one');
define('MB_MANYTOMANY', 'many-to-many');
define('MB_MAXDBIDENTIFIERLENGTH', 30); // maximum length of any identifier in our supported databases
define('MB_EXPORTPREPEND', 'project_');
define('MB_VISIBILITY', 'visibility');

#[\AllowDynamicProperties]
class MBConstants
{
    public static $EMPTY = array( 'name' => '(empty)' , 'label' => '(empty)' ) ;
    public static $FILLER = array( 'name' => '(filler)' , 'label' => 'LBL_FILLER' ) ; // would prefer to have label => translate('LBL_FILLER') but can't be done in a static, and don't want to require instantiating a new object to get these constants
}
