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

?>
<form action='index.php' method="POST">
<input type='hidden' name='action' value='updateTimezonePrefs'>
<input type='hidden' name='module' value='Administration'>

<table>
<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
$prompt_users = 'checked';
if (isset($_POST['preview']) && !isset($_POST['prompt_users'])) {
    $prompt_users = '';
}


$result = $db->query("SELECT id, user_preferences, user_name FROM users");
$execute = false;
// loop through user preferences and check for "bad" elements; rebuild preferences array and update database
if (isset($_POST['execute'])) {
    $execute = true;
}
$serverTimeZone = lookupTimezone(0);
while ($row = $db->fetchByAssoc($result)) {
    $adjustment = 'none';

    if (isset($_POST[$row['id'].'adjust'])) {
        $adjustment = $_POST[$row['id'].'adjust'];
    }

    $string = "Preview";
    if ($execute) {
        $string = "Updating";
    }
    echo "<tr><td> $string timezone preferences for user <b>{$row['user_name']}</b>...</td><td>";


    $prefs = array();
    $newprefs = array();

    $prefs = unserialize(base64_decode($row['user_preferences']));
    $setTo = '';
    $alreadySet = '';
    if (!empty($prefs)) {
        foreach ($prefs as $key => $val) {
            if ($key == 'timez') {
                if (empty($prefs['timezone']) && $val != '') {
                    $hourAdjust = $adjustment;
                    if ($hourAdjust == 'none') {
                        $hourAdjust = 0;
                    }
                    $selectedZone = lookupTimezone($prefs['timez'] + $hourAdjust);

                    if (!empty($selectedZone)) {
                        $newprefs['timezone'] = $selectedZone;
                        $newprefs['timez']  = $val;
                        $setTo = $selectedZone;
                        if (empty($prompt_users)) {
                            $newprefs['ut']=1;
                        } else {
                            $newprefs['ut']=0;
                        }
                    } else {
                        $newprefs['timezone'] = $serverTimeZone;
                        $newprefs['timez']  = $val;
                        $setTo = $serverTimeZone;
                        if (empty($prompt_users)) {
                            $newprefs['ut']=1;
                        } else {
                            $newprefs['ut']=0;
                        }
                    }
                } else {
                    $newprefs[$key] = $val;
                    if (!empty($prefs['timezone'])) {
                        $alreadySet = 'Previously Set - '. $prefs['timezone'];
                    }
                }
            } else {
                $newprefs[$key] = $val;
            }
        }
        if ($execute) {
            $newStr = $db->quoted(base64_encode(serialize($newprefs)));
            $db->query("UPDATE users SET user_preferences = {$newStr} WHERE id = '{$row['id']}'");
        }
    }
    if (!empty($setTo)) {
        echo $setTo;
    } else {
        if (!empty($alreadySet)) {
            echo $alreadySet;
        } else {
            echo $serverTimeZone;
            $prefs['timezone'] = $serverTimeZone;
        }
    }
    echo "</td><td>";
    if (!empty($setTo)) {
        echo "Adjust: ";
        if ($execute) {
            if (isset($_POST[$row['id'].'adjust'])) {
                echo  $adjustment;
            }
        } else {
            echo "<select name='{$row['id']}adjust'>";

            echo get_select_options_with_id(array('-1'=>'-1', 'none'=>'0', '1'=>'+1'), $adjustment.'');
            echo '</select>';
        }
        echo ' hour';
    }
    echo ' </td><td>';
    echo "</tr>";

    $the_old_prefs[] = $prefs;
    $the_new_prefs[] = $newprefs;

    unset($prefs);
    unset($newprefs);
    unset($newStr);
}

echo "</table>";

if ($execute) {
    echo "<br>All timezone preferences updated!<br><br>";
} else {
    echo "Prompt users on login to confirm:<input type='checkbox' name='prompt_users' value='1' $prompt_users><br>";
    echo "<input class='button' type='submit' name='execute' value='Execute'>&nbsp; <input class='button' type='submit' name='preview' value='Preview'>";
}
echo "&nbsp;<input class='button' type='button' name='Done' value='Done' onclick='document.location.href=\"index.php?action=DstFix&module=Administration\"'>";
?>
</form>
