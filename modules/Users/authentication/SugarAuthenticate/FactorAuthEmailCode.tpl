


{*
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
*}
<!-- !!!1
<form method="post" action="">
    Email Code: <input type="text" name="factor_token"> <input type="submit" value="Send">
</form>
<a href="index.php?module=Users&action=Logout">logout</a>
-->
<!DOCTYPE html>
<html>
<head>
    <link rel="SHORTCUT ICON" href="{$favicon}">
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="user-scalable=no, initial-scale=1, maximum-scale=1, minimum-scale=1">
    <!-- Bootstrap -->
    <link href="{$cssPath}/normalize.css" rel="stylesheet" type="text/css">
    <link href="{$cssPath}/bootstrap.min.css" rel="stylesheet">
    <link href="{$cssPath}/fonts.css" rel="stylesheet" type="text/css">
    <link href="{$cssPath}/grid.css" rel="stylesheet" type="text/css">
    <link href="t{$cssPath}/footable.core.css" rel="stylesheet" type="text/css">

    <link rel="stylesheet" type="text/css" media="all" href="modules/Users/login.css">
    <title>SuiteCRM</title>

    {$css}
</head>
<body>
<div class="p_login">
    <div class="p_login_top">
        <a title="SuiteCRM" href="https://www.suitecrm.com">SuiteCRM</a>
    </div>

    <div class="p_login_middle">
        <div id="loginform">
            <div class="error message">{$factor_message}</div>
            <form method="post">
                {$APP.LBL_EMAIL_CODE} <input type="text" name="factor_token">
                <input type="submit" value="{$APP.LBL_VERIFY}">
            </form>
            <a href="index.php?module=Users&action=Logout">{$APP.LBL_CANCEL}</a>&nbsp;&nbsp;&nbsp;
            <a href="index.php?module=Users&action=Resend">{$APP.LBL_RESEND}</a>
        </div>

        <div class="p_login_bottom">
            <a id="admin_options">© Supercharged by SuiteCRM</a>
            <a id="powered_by">© Powered By SugarCRM</a>
        </div>
    </div>
</body>
</html>




