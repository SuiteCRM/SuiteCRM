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

 if (!defined('ACL_ALLOW_NONE')) {
     define('ACL_ALLOW_ADMIN_DEV', 100);
     define('ACL_ALLOW_ADMIN', 99);
     define('ACL_ALLOW_ALL', 90);
     define('ACL_ALLOW_ENABLED', 89);
     /* BEGIN - SECURITY GROUPS */
     define('ACL_ALLOW_GROUP', 80); //securitygroup
     /* END - SECURITY GROUPS */
     define('ACL_ALLOW_OWNER', 75);
     define('ACL_ALLOW_NORMAL', 1);
     define('ACL_ALLOW_DEFAULT', 0);
     define('ACL_ALLOW_DISABLED', -98);
     define('ACL_ALLOW_NONE', -99);
     define('ACL_ALLOW_DEV', 95);
 }
 /**
  * $GLOBALS['ACLActionAccessLevels
  * these are rendering descriptions for Access Levels giving information such as the label, color, and text color to use when rendering the access level
  */
 $GLOBALS['ACLActionAccessLevels'] = array(
    ACL_ALLOW_ALL=>array('color'=>'#008000', 'label'=>'LBL_ACCESS_ALL', 'text_color'=>'white'),
    ACL_ALLOW_OWNER=>array('color'=>'#6F6800', 'label'=>'LBL_ACCESS_OWNER', 'text_color'=>'white'),
    ACL_ALLOW_NONE=>array('color'=>'#FF0000', 'label'=>'LBL_ACCESS_NONE', 'text_color'=>'white'),
    ACL_ALLOW_ENABLED=>array('color'=>'#008000', 'label'=>'LBL_ACCESS_ENABLED', 'text_color'=>'white'),
    ACL_ALLOW_DISABLED=>array('color'=>'#FF0000', 'label'=>'LBL_ACCESS_DISABLED', 'text_color'=>'white'),
    ACL_ALLOW_ADMIN=>array('color'=>'#0000FF', 'label'=>'LBL_ACCESS_ADMIN', 'text_color'=>'white'),
    ACL_ALLOW_NORMAL=>array('color'=>'#008000', 'label'=>'LBL_ACCESS_NORMAL', 'text_color'=>'white'),
    ACL_ALLOW_DEFAULT=>array('color'=>'#008000', 'label'=>'LBL_ACCESS_DEFAULT', 'text_color'=>'white'),
    ACL_ALLOW_DEV=>array('color'=>'#0000FF', 'label'=>'LBL_ACCESS_DEV', 'text_color'=>'white'),
    ACL_ALLOW_ADMIN_DEV=>array('color'=>'#0000FF', 'label'=>'LBL_ACCESS_ADMIN_DEV', 'text_color'=>'white'),
    /* BEGIN - SECURITY GROUPS */
    ACL_ALLOW_GROUP=>array('color'=>'#0000A0', 'label'=>'LBL_ACCESS_GROUP', 'text_color'=>'white'), //securitygroup
    /* END - SECURITY GROUPS */
    );
/**
 * $GLOBALS['ACLActions
 * These are the actions for a given type. It includes the ACCESS Levels for that action and the label for that action. Every an object of the category (e.g. module) is added all associated actions are added for that object
 */
/* BEGIN - SECURITY GROUPS */
$GLOBALS['ACLActions'] = array(
    'module'=>array('actions'=>
                        array(
                        'access'=>
                                array(
                                    'aclaccess'=>array(ACL_ALLOW_ENABLED,ACL_ALLOW_DEFAULT, ACL_ALLOW_DISABLED),
                                    'label'=>'LBL_ACTION_ACCESS',
                                    'default'=>ACL_ALLOW_ENABLED,
                                ),

                        'view'=>
                                array(
                                    'aclaccess'=>array(ACL_ALLOW_ALL,ACL_ALLOW_GROUP,ACL_ALLOW_OWNER,ACL_ALLOW_DEFAULT, ACL_ALLOW_NONE),
                                    'label'=>'LBL_ACTION_VIEW',
                                    'default'=>ACL_ALLOW_ALL,
                                ),

                        'list'=>
                                array(
                                    'aclaccess'=>array(ACL_ALLOW_ALL,ACL_ALLOW_GROUP,ACL_ALLOW_OWNER,ACL_ALLOW_DEFAULT, ACL_ALLOW_NONE),
                                    'label'=>'LBL_ACTION_LIST',
                                    'default'=>ACL_ALLOW_ALL,
                                ),
                        'edit'=>
                                array(
                                    'aclaccess'=>array(ACL_ALLOW_ALL,ACL_ALLOW_GROUP,ACL_ALLOW_OWNER,ACL_ALLOW_DEFAULT, ACL_ALLOW_NONE),
                                    'label'=>'LBL_ACTION_EDIT',
                                    'default'=>ACL_ALLOW_ALL,

                                ),
                        'delete'=>
                            array(
                                    'aclaccess'=>array(ACL_ALLOW_ALL,ACL_ALLOW_GROUP,ACL_ALLOW_OWNER,ACL_ALLOW_DEFAULT, ACL_ALLOW_NONE),
                                    'label'=>'LBL_ACTION_DELETE',
                                    'default'=>ACL_ALLOW_ALL,

                                ),
                        'import'=>
                            array(
                                    'aclaccess'=>array(ACL_ALLOW_ALL,ACL_ALLOW_DEFAULT, ACL_ALLOW_NONE),
                                    'label'=>'LBL_ACTION_IMPORT',
                                    'default'=>ACL_ALLOW_ALL,
                                ),
                        'export'=>
                            array(
                                    'aclaccess'=>array(ACL_ALLOW_ALL,ACL_ALLOW_GROUP,ACL_ALLOW_OWNER,ACL_ALLOW_DEFAULT, ACL_ALLOW_NONE),
                                    'label'=>'LBL_ACTION_EXPORT',
                                    'default'=>ACL_ALLOW_ALL,
                                ),
                        'massupdate'=>
                            array(
                                    'aclaccess'=>array(ACL_ALLOW_ALL,ACL_ALLOW_DEFAULT, ACL_ALLOW_NONE),
                                    'label'=>'LBL_ACTION_MASSUPDATE',
                                    'default'=>ACL_ALLOW_ALL,
                                ),


                ),),
);
/* END - SECURITY GROUPS */
