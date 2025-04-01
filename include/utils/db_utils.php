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




/**
 * @deprecated use DBManager::convert() instead.
 */
function db_convert($string, $type, $additional_parameters=array(), $additional_parameters_oracle_only=array())
{
    return DBManagerFactory::getInstance()->convert($string, $type, $additional_parameters, $additional_parameters_oracle_only);
}

/**
 * @deprecated use DBManager::concat() instead.
 */
function db_concat($table, $fields)
{
    $db = DBManagerFactory::getInstance();
    return $db->concat($table, $fields);
}

/**
 * @deprecated use DBManager::fromConvert() instead.
 */
function from_db_convert($string, $type)
{
    return DBManagerFactory::getInstance()->fromConvert($string, $type);
}

$toHTML = array(
    '"' => '&quot;',
    '<' => '&lt;',
    '>' => '&gt;',
    "'" => '&#039;',
);
$GLOBALS['toHTML_keys'] = array_keys($toHTML);
$GLOBALS['toHTML_values'] = array_values($toHTML);
$GLOBALS['toHTML_keys_set'] = implode("", $GLOBALS['toHTML_keys']);
/**
 * Replaces specific characters with their HTML entity values
 * @param string $string String to check/replace
 * @param bool $encode Default true
 * @return string
 *
 * @todo Make this utilize the external caching mechanism after re-testing (see
 *       log on r25320).
 *
 * Bug 49489 - removed caching of to_html strings as it was consuming memory and
 * never releasing it
 */
function to_html($string, $encode=true)
{
    if (empty($string)) {
        return $string;
    }

    global $toHTML;

    if ($encode && is_string($string)) {
        if (is_array($toHTML)) {
            $string = str_ireplace($GLOBALS['toHTML_keys'], $GLOBALS['toHTML_values'] ?? [], $string);
        } else {
            $string = htmlentities($string, ENT_HTML401|ENT_QUOTES, 'UTF-8');
        }
    }

    return $string;
}


/**
 * Replaces specific HTML entity values with the true characters
 * @param string $string String to check/replace
 * @param bool $encode Default true
 * @return string
 */
function from_html($string, $encode=true)
{
    if ($string === null || is_array($string)){
        return '';
    }

    if (!is_string($string) || !$encode) {
        return $string;
    }

    global $toHTML;
    static $toHTML_values = null;
    static $toHTML_keys = null;
    static $cache = array();
    if (!empty($toHTML) && is_array($toHTML) && (!isset($toHTML_values) || !empty($GLOBALS['from_html_cache_clear']))) {
        $toHTML_values = array_values($toHTML);
        $toHTML_keys = array_keys($toHTML);
    }

    // Bug 36261 - Decode &amp; so we can handle double encoded entities
    $string = html_entity_decode($string, ENT_HTML401|ENT_QUOTES, 'UTF-8') ?? '';

    if (!isset($cache[$string])) {
        $cache[$string] = str_ireplace($toHTML_values ?? '', $toHTML_keys ?? '', $string);
    }
    return $cache[$string] ?? '';
}

/*
 * Return a version of $proposed that can be used as a column name in any of our supported databases
 * Practically this means no longer than 25 characters as the smallest identifier length for our supported DBs is 30 chars for Oracle plus we add on at least four characters in some places (for indices for example)
 * @param string $name Proposed name for the column
 * @param string $ensureUnique
 * @param int $maxlen Deprecated and ignored
 * @return string Valid column name trimmed to right length and with invalid characters removed
 */
function getValidDBName($name, $ensureUnique = false, $maxLen = 30)
{
    return DBManagerFactory::getInstance()->getValidDBName($name, $ensureUnique);
}


/**
 * isValidDBName
 *
 * Utility to perform the check during install to ensure a database name entered by the user
 * is valid based on the type of database server
 * @param string $name Proposed name for the DB
 * @param string $dbType Type of database server
 * @return bool true or false based on the validity of the DB name
 */
function isValidDBName($name, $dbType)
{
    $db = DBManagerFactory::getTypeInstance($dbType);
    return $db->isDatabaseNameValid($name);
}
