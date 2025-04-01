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





function checkDBSettings($silent=false)
{
    $sugar_db_version = '';
    installLog("Begin DB Check Process *************");
    global $mod_strings;
    $errors = array();
    copyInputsIntoSession();

    $db = getInstallDbInstance();

    installLog("testing with {$db->dbType}:{$db->variant}");


    if (trim($_SESSION['setup_db_database_name']) == '') {
        $errors['ERR_DB_NAME'] = $mod_strings['ERR_DB_NAME'];
        installLog("ERROR::  {$errors['ERR_DB_NAME']}");
    }


    if (!$db->isDatabaseNameValid($_SESSION['setup_db_database_name'])) {
        $errIdx = 'ERR_DB_' . strtoupper($_SESSION['setup_db_type']) . '_DB_NAME_INVALID';
        $errors[$errIdx] = $mod_strings[$errIdx];
        installLog("ERROR::  {$errors[$errIdx]}");
    }

    if ($_SESSION['setup_db_type'] != 'oci8') {
        // Oracle doesn't need host name, others do
        if (trim($_SESSION['setup_db_host_name']) == '') {
            $errors['ERR_DB_HOSTNAME'] = $mod_strings['ERR_DB_HOSTNAME'];
            installLog("ERROR::  {$errors['ERR_DB_HOSTNAME']}");
        }
    }

    //check to see that password and retype are same, if needed
    if (!empty($_SESSION['dbUSRData']) && ($_SESSION['dbUSRData']=='create' || $_SESSION['dbUSRData']=='provide')) {
        if ($_SESSION['setup_db_sugarsales_password'] != $_SESSION['setup_db_sugarsales_password_retype']) {
            $errors['ERR_DBCONF_PASSWORD_MISMATCH'] = $mod_strings['ERR_DBCONF_PASSWORD_MISMATCH'];
            installLog("ERROR::  {$errors['ERR_DBCONF_PASSWORD_MISMATCH']}");
        }
    }

    // bail if the basic info isn't valid
    if (count($errors) > 0) {
        installLog("Basic form info is INVALID, exit Process.");
        return printErrors($errors);
    } else {
        installLog("Basic form info is valid, continuing Process.");
    }

    $dbconfig = array(
                "db_host_name" => $_SESSION['setup_db_host_name'],
                "db_host_instance" => isset($_SESSION['setup_db_host_instance']) ? $_SESSION['setup_db_host_instance'] : null,
        );

    if (!empty($_SESSION['setup_db_port_num'])) {
        $dbconfig["db_port"] = $_SESSION['setup_db_port_num'];
    } else {
        $_SESSION['setup_db_port_num'] = '';
    }

    // Needed for database implementation that do not allow connections to the server directly
    // and that typically require the manual setup of a database instances such as DB2
    if (empty($_SESSION['setup_db_create_database'])) {
        $dbconfig["db_name"] = $_SESSION['setup_db_database_name'];
    }

    // check database name validation in different database types (default is mssql)
    switch (strtolower($db->dbType)) {

            case 'mysql':
                if (preg_match("![/\\.]+!i", (string) $_SESSION['setup_db_database_name'])) {
                    $errors['ERR_DB_MYSQL_DB_NAME'] = $mod_strings['ERR_DB_MYSQL_DB_NAME_INVALID'];
                    installLog("ERROR::  {$errors['ERR_DB_MYSQL_DB_NAME']}");
                }
                break;

            case 'mssql':
            default:
                // Bug 29855 - Check to see if given db name is valid
                if (preg_match("![\"'*/\\?:<>-]+!i", (string) $_SESSION['setup_db_database_name'])) {
                    $errors['ERR_DB_MSSQL_DB_NAME'] = $mod_strings['ERR_DB_MSSQL_DB_NAME_INVALID'];
                    installLog("ERROR::  {$errors['ERR_DB_MSSQL_DB_NAME']}");
                }
                break;
        }

    // test the account that will talk to the db if we're not creating it
    if ($_SESSION['setup_db_sugarsales_user'] != '' && !$_SESSION['setup_db_create_sugarsales_user']) {
        $dbconfig["db_user_name"] = $_SESSION['setup_db_sugarsales_user'];
        $dbconfig["db_password"] = $_SESSION['setup_db_sugarsales_password'];
        installLog("Testing user account...");

        // try connecting to the DB
        if (!$db->connect($dbconfig, false)) {
            $error = $db->lastError();
            $errors['ERR_DB_LOGIN_FAILURE'] = $mod_strings['ERR_DB_LOGIN_FAILURE'];
            installLog("ERROR::  {$errors['ERR_DB_LOGIN_FAILURE']}");
        } else {
            installLog("Connection made using  host: {$_SESSION['setup_db_host_name']}, usr: {$_SESSION['setup_db_sugarsales_user']}");
            $db->disconnect();
        }
    }

    // privileged account tests
    else {
        if (empty($_SESSION['setup_db_admin_user_name'])) {
            $errors['ERR_DB_PRIV_USER'] = $mod_strings['ERR_DB_PRIV_USER'];
            installLog("ERROR:: {$errors['ERR_DB_PRIV_USER']}");
        } else {
            installLog("Testing priviliged account...");
            $dbconfig["db_user_name"] = $_SESSION['setup_db_admin_user_name'];
            $dbconfig["db_password"] = $_SESSION['setup_db_admin_password'];
            if (!$db->connect($dbconfig, false)) {
                $error = $db->lastError();
                $errors['ERR_DB_LOGIN_FAILURE'] = $mod_strings['ERR_DB_LOGIN_FAILURE'];
                installLog("ERROR::  {$errors['ERR_DB_LOGIN_FAILURE']}");
            } else {
                installLog("Connection made using  host: {$_SESSION['setup_db_host_name']}, usr: {$_SESSION['setup_db_sugarsales_user']}");
                $db_selected = $db->dbExists($_SESSION['setup_db_database_name']);
                if ($silent==false && $db_selected && $_SESSION['setup_db_create_database'] && empty($_SESSION['setup_db_drop_tables'])) {
                    // DB exists but user didn't agree to overwrite it
                    $errStr = $mod_strings['ERR_DB_EXISTS_PROCEED'];
                    $errors['ERR_DB_EXISTS_PROCEED'] = $errStr;
                    installLog("ERROR:: {$errors['ERR_DB_EXISTS_PROCEED']}");
                } elseif ($silent==false && !$db_selected && !$_SESSION['setup_db_create_database']) {
                    // DB does not exist but user did not allow to create it
                    $errors['ERR_DB_EXISTS_NOT'] = $mod_strings['ERR_DB_EXISTS_NOT'];
                    installLog("ERROR:: {$errors['ERR_DB_EXISTS_NOT']}");
                } else {
                    if ($db_selected) {
                        installLog("DB Selected, will reuse {$_SESSION['setup_db_database_name']}");
                        if ($db->tableExists('config')) {
                            include('sugar_version.php');
                            $versions = $db->getOne("SELECT COUNT(*) FROM config WHERE category='info' AND name='sugar_version' AND VALUE LIKE '$sugar_db_version'");
                            if ($versions > 0 && $silent==false) {
                                $errors['ERR_DB_EXISTS_WITH_CONFIG'] = $mod_strings['ERR_DB_EXISTS_WITH_CONFIG'];
                                installLog("ERROR:: {$errors['ERR_DB_EXISTS_WITH_CONFIG']}");
                            }
                        }
                    } else {
                        installLog("DB not selected, will create {$_SESSION['setup_db_database_name']}");
                    }
                    if ($_SESSION['setup_db_create_sugarsales_user'] && $_SESSION['setup_db_sugarsales_user'] != '' && $db_selected) {
                        if ($db->userExists($_SESSION['setup_db_sugarsales_user'])) {
                            $errors['ERR_DB_USER_EXISTS'] = $mod_strings['ERR_DB_USER_EXISTS'];
                            installLog("ERROR:: {$errors['ERR_DB_USER_EXISTS']}");
                        }
                    }
                }

                // DB SPECIFIC
                $check = $db->canInstall();
                if ($check !== true) {
                    $error = array_shift($check);
                    array_unshift($check, $mod_strings[$error]);
                    $errors[$error] = call_user_func_array('sprintf', $check);
                    installLog("ERROR:: {$errors[$error]}");
                } else {
                    installLog("Passed DB install check");
                }

                $db->disconnect();
            }
        }
    }


    if ($silent) {
        return $errors;
    } else {
        printErrors($errors);
    }
    installLog("End DB Check Process *************");
}

function printErrors($errors)
{
    global $mod_strings;
    if ((is_countable($errors) ? count($errors) : 0) == 0) {
        echo 'dbCheckPassed';
        installLog("SUCCESS:: no errors detected!");
    } else {
        if (((is_countable($errors) ? count($errors) : 0) == 1 && (isset($errors["ERR_DB_EXISTS_PROCEED"])||isset($errors["ERR_DB_EXISTS_WITH_CONFIG"])))  ||
    ((is_countable($errors) ? count($errors) : 0) == 2 && isset($errors["ERR_DB_EXISTS_PROCEED"]) && isset($errors["ERR_DB_EXISTS_WITH_CONFIG"]))) {
            ///throw alert asking to overwwrite db
            echo 'preexeest';
            installLog("WARNING:: no errors detected, but DB tables will be dropped!, issuing warning to user");
        } else {
            installLog("FATAL:: errors have been detected!  User will not be allowed to continue.  Errors are as follow:");
            //print out errors
            $validationErr  = "<p><b>{$mod_strings['ERR_DBCONF_VALIDATION']}</b></p>";
            $validationErr .= '<ul>';

            foreach ($errors as $key =>$erMsg) {
                if ($key != "ERR_DB_EXISTS_PROCEED" && $key != "ERR_DB_EXISTS_WITH_CONFIG") {
                    if ($_SESSION['dbUSRData'] == 'same' && $key == 'ERR_DB_ADMIN') {
                        installLog(".. {$erMsg}");
                        break;
                    }
                    $validationErr .= '<li class="error">' . $erMsg . '</li>';
                    installLog(".. {$erMsg}");
                }
            }
            $validationErr .= '</ul>';
            $validationErr .= '</div>';

            echo $validationErr;
        }
    }
}


function copyInputsIntoSession()
{
    if (isset($_REQUEST['setup_db_type'])) {
        $_SESSION['setup_db_type']                        = $_REQUEST['setup_db_type'];
    }
    if (isset($_REQUEST['setup_db_admin_user_name'])) {
        $_SESSION['setup_db_admin_user_name']  = $_REQUEST['setup_db_admin_user_name'];
    }
    if (isset($_REQUEST['setup_db_admin_password'])) {
        $_SESSION['setup_db_admin_password']    = $_REQUEST['setup_db_admin_password'];
    }
    if (isset($_REQUEST['setup_db_database_name'])) {
        $_SESSION['setup_db_database_name']      = $_REQUEST['setup_db_database_name'];
    }
    if (isset($_REQUEST['setup_db_host_name'])) {
        $_SESSION['setup_db_host_name']              = $_REQUEST['setup_db_host_name'];
    }

    //FTS Support
    if (isset($_REQUEST['setup_fts_type'])) {
        $_SESSION['setup_fts_type'] = $_REQUEST['setup_fts_type'];
    }
    if (isset($_REQUEST['setup_fts_host'])) {
        $_SESSION['setup_fts_host'] = $_REQUEST['setup_fts_host'];
    }
    if (isset($_REQUEST['setup_fts_port'])) {
        $_SESSION['setup_fts_port'] = $_REQUEST['setup_fts_port'];
    }

    if (isset($_SESSION['setup_db_type']) && (!isset($_SESSION['setup_db_manager']) || isset($_REQUEST['setup_db_type']))) {
        $_SESSION['setup_db_manager'] = DBManagerFactory::getManagerByType($_SESSION['setup_db_type']);
    }

    if (isset($_REQUEST['setup_db_host_instance'])) {
        $_SESSION['setup_db_host_instance'] = $_REQUEST['setup_db_host_instance'];
    }

    if (isset($_REQUEST['setup_db_port_num'])) {
        $_SESSION['setup_db_port_num'] = $_REQUEST['setup_db_port_num'];
    }

    // on a silent install, copy values from $_SESSION into $_REQUEST
    if (isset($_REQUEST['goto']) && $_REQUEST['goto'] == 'SilentInstall') {
        if (isset($_SESSION['dbUSRData']) && !empty($_SESSION['dbUSRData'])) {
            $_REQUEST['dbUSRData'] = $_SESSION['dbUSRData'];
        } else {
            $_REQUEST['dbUSRData'] = 'same';
        }

        if (isset($_SESSION['setup_db_sugarsales_user']) && !empty($_SESSION['setup_db_sugarsales_user'])) {
            $_REQUEST['setup_db_sugarsales_user'] = $_SESSION['setup_db_sugarsales_user'];
        } else {
            $_REQUEST['dbUSRData'] = 'same';
        }

        $_REQUEST['setup_db_sugarsales_password'] = $_SESSION['setup_db_sugarsales_password'];
        $_REQUEST['setup_db_sugarsales_password_retype'] = $_SESSION['setup_db_sugarsales_password'];
    }

    //make sure we are creating or using provided user for app db connections
            $_SESSION['setup_db_create_sugarsales_user']  = true;//get_boolean_from_request('setup_db_create_sugarsales_user');
            $db = getInstallDbInstance();
    if (!$db->supports("create_user")) {
        //if the DB doesn't support creating users, make the admin user/password same as connecting user/password
        $_SESSION['setup_db_sugarsales_user']             = $_SESSION['setup_db_admin_user_name'];
        $_SESSION['setup_db_sugarsales_password']         = $_SESSION['setup_db_admin_password'];
        $_SESSION['setup_db_sugarsales_password_retype']  = $_SESSION['setup_db_sugarsales_password'];
        $_SESSION['setup_db_create_sugarsales_user']      = false;
        $_SESSION['setup_db_create_database']             = false;
    } else {
        $_SESSION['setup_db_create_database']             = true;
        //retrieve the value from dropdown in order to know what settings the user
        //wants to use for the sugar db user.

        //use provided db admin by default
        $_SESSION['dbUSRData'] = 'same';

        if (isset($_REQUEST['dbUSRData'])  && !empty($_REQUEST['dbUSRData'])) {
            $_SESSION['dbUSRData'] = $_REQUEST['dbUSRData'];
        }


        if ($_SESSION['dbUSRData'] == 'auto') {
            //create user automatically
            $_SESSION['setup_db_create_sugarsales_user']          = true;
            $_SESSION['setup_db_sugarsales_user']                 = "sugar".create_db_user_creds(5);
            $_SESSION['setup_db_sugarsales_password']             = create_db_user_creds(10);
            $_SESSION['setup_db_sugarsales_password_retype']      = $_SESSION['setup_db_sugarsales_password'];
        } elseif ($_SESSION['dbUSRData'] == 'provide') {
            //use provided user info
            $_SESSION['setup_db_create_sugarsales_user']          = false;
            $_SESSION['setup_db_sugarsales_user']                 = $_REQUEST['setup_db_sugarsales_user'];
            $_SESSION['setup_db_sugarsales_password']             = $_REQUEST['setup_db_sugarsales_password'];
            $_SESSION['setup_db_sugarsales_password_retype']      = $_REQUEST['setup_db_sugarsales_password_retype'];
        } elseif ($_SESSION['dbUSRData'] == 'create') {
            // create user with provided info
            $_SESSION['setup_db_create_sugarsales_user']        = true;
            $_SESSION['setup_db_sugarsales_user']               = $_REQUEST['setup_db_sugarsales_user'];
            $_SESSION['setup_db_sugarsales_password']           = $_REQUEST['setup_db_sugarsales_password'];
            $_SESSION['setup_db_sugarsales_password_retype']    = $_REQUEST['setup_db_sugarsales_password_retype'];
        } else {
            //Use the same login as provided admin user
            $_SESSION['setup_db_create_sugarsales_user']      = false;
            $_SESSION['setup_db_sugarsales_user']             = $_SESSION['setup_db_admin_user_name'];
            $_SESSION['setup_db_sugarsales_password']         = $_SESSION['setup_db_admin_password'];
            $_SESSION['setup_db_sugarsales_retype']           = $_SESSION['setup_db_admin_password'];
        }
    }

    if (!isset($_SESSION['demoData']) || empty($_SESSION['demoData'])) {
        $_SESSION['demoData'] = 'no';
    }
    if (isset($_REQUEST['demoData'])) {
        $_SESSION['demoData'] = $_REQUEST['demoData'] ;
    }

    if ($db->supports('create_db')) {
        if (!empty($_SESSION['setup_db_create_database'])) {
            // if we're dropping DB, no need to drop tables
            $_SESSION['setup_db_drop_tables']  = false;
        }
    } else {
        // we can't create DB, so can't drop it
        $_SESSION['setup_db_create_database'] = false;
    }

    if (isset($_REQUEST['goto']) && $_REQUEST['goto'] == 'SilentInstall' && isset($_SESSION['setup_db_drop_tables'])) {
        //set up for Oracle Silent Installer
        $_REQUEST['setup_db_drop_tables'] = $_SESSION['setup_db_drop_tables'] ;
    }
}

////    END PAGEOUTPUT
///////////////////////////////////////////////////////////////////////////////
