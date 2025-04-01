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
class HelpItem
{
    public $associated_field = '';
    public $title = '';
    public $text = '';
}

function &help_menu_html()
{
    $str =<<<HEREDOC_END
		<div>SugarCRM Install Help</div>
		<ul>
		<li><a href="$_SERVER[PHP_SELF]?step=1">Step 1: Prerequisite checks</a></li>
		<li><a href="$_SERVER[PHP_SELF]?step=2">Step 2: Database configuration</a></li>
		<li><a href="$_SERVER[PHP_SELF]?step=3">Step 3: Site configuration</a></li>
		<li><a href="$_SERVER[PHP_SELF]?step=4">Step 4: Saving config file and setting up the database</a></li>
		<li><a href="$_SERVER[PHP_SELF]?step=5">Step 5: Registration</a></li>
		</ul>
HEREDOC_END;
    return $str;
}

function &format_help_items(&$help_items)
{
    $str = '<table>';

    foreach ($help_items as $help_item) {
        $str .= <<< HEREDOC_END
<tr><td><b>$help_item->title</b></td></tr>
<tr><td>$help_item->text</td></tr>
HEREDOC_END;
    }

    $str .= '</table>';

    return $str;
}

function &help_step_1_html()
{
    $help_items = array();

    $help_item = new HelpItem();
    $help_item->title = 'PHP Version';
    $help_item->text = <<< HEREDOC_END
The version of PHP installed must be 4.3.x or 5.x.
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'MySQL Database';
    $help_item->text = <<< HEREDOC_END
Checking that the MySQL API is accessible.
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'SugarCRM Configuration File';
    $help_item->text = <<< HEREDOC_END
The configuration file (config.php) must be writable.
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Cache Sub-Directories';
    $help_item->text = <<< HEREDOC_END
All the sub-directories beneath the cache directory (cache) must be
writable.
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Session Save Path';
    $help_item->text = <<< HEREDOC_END
The session save path specified in the PHP initialization file (php.ini)
as session_save_path must exist and be writable.
HEREDOC_END;

    $help_items[] = $help_item;

    $str =format_help_items($help_items);
    return $str;
}

function &help_step_2_html()
{
    $help_items = array();

    $help_item = new HelpItem();
    $help_item->title = 'Host Name';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Database Name';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Create Database';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'User Name for SugarCRM';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Create User';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Password for SugarCRM';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Re-Type Password for SugarCRM';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Drop and recreate existing SugarCRM tables?';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Populate database with demo data?';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Database Admin User Name';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Database Admin Password';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $str =format_help_items($help_items);
    return $str;
}

function &help_step_3_html()
{
    $help_items = array();

    $help_item = new HelpItem();
    $help_item->title = 'URL';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'SugarCRM Admin Password';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Re-type SugarCRM Admin Password';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Allow SugarCRM to collect anonymous usage information?';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Use a Custom Session Directory for SugarCRM';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Path to Session Directory';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Provide Your Own Application ID';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $help_item = new HelpItem();
    $help_item->title = 'Application ID';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $str =format_help_items($help_items);
    return $str;
}

function &help_step_4_html()
{
    $help_items = array();

    $help_item = new HelpItem();
    $help_item->title = 'Perform Install';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $str =format_help_items($help_items);
    return $str;
}

function &help_step_5_html()
{
    $help_items = array();

    $help_item = new HelpItem();
    $help_item->title = 'Registration';
    $help_item->text = <<< HEREDOC_END
TODO
HEREDOC_END;

    $help_items[] = $help_item;

    $str =format_help_items($help_items);
    return $str;
}

?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html <?php get_language_header(); ?>>
<head>
   <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
   <meta http-equiv="Content-Style-Type" content="text/css">
   <title>SugarCRM Install Help</title>
   <link rel="stylesheet" href="install/install.css" type="text/css" />
</head>
<body>
<?php
if (isset($_GET['step'])) {
    switch ($_GET['step']) {
      case 1:
         echo help_step_1_html();
      break;
      case 2:
         echo help_step_2_html();
      break;
      case 3:
         echo help_step_3_html();
      break;
      case 4:
         echo help_step_4_html();
      break;
      case 5:
         echo help_step_5_html();
      break;
      default:
         echo help_menu_html();
      break;
   }
} else {
    echo help_menu_html();
}
?>

<form>
   <input type="button" value="Close" onclick="javascript:window.close();" />
</form>
</body>
</html>
