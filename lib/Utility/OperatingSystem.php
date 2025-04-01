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
namespace SuiteCRM\Utility;

#[\AllowDynamicProperties]
class OperatingSystem
{
    /**
     * @return bool true when operating system is BSD
     */
    public function isOsBSD()
    {
        return stristr(php_uname('s'), 'BSD') !== false;
    }

    /**
     * @return bool true when operating system is Linux
     */
    public function isOsLinux()
    {
        return stristr(php_uname('s'), 'Linux') !== false;
    }

    /**
     * @return bool true when operating system is Mac OS X
     */
    public function isOsMacOSX()
    {
        return stristr(php_uname('s'), 'Darwin') !== false;
    }

    /**
     * @return bool true when operating system is Solaris
     */
    public function isOsSolaris()
    {
        return stristr(php_uname('s'), 'Solaris') !== false;
    }

    /**
     * @return bool true when operating system is Unknown
     */
    public function isOsUnknown()
    {
        return php_uname('s') === 'Unknown';
    }

    /**
     * @return bool true when operating system is Windows
     */
    public function isOsWindows()
    {
        return stristr(php_uname('s'), 'Windows') !== false;
    }

    /**
     * @param string $path
     * @param string $ds separator to convert to (used for testing purposes)
     * @return string path converted for the current operating system eg Linux, Mac OS, Windows
     */
    public function toOsPath($path, $ds = DIRECTORY_SEPARATOR)
    {
        // strip " - windows can use double quotes instead of escaping strings
        $trimmedPath = trim($path, '"');

        $removeEscapedSpace = str_replace('\ ', ' ', $trimmedPath);
        $removeEscapedTab = str_replace('\	', '	', $removeEscapedSpace);

        $replaceSeparator = preg_replace('/[\\\\\/]/', $ds, $removeEscapedTab);

        if ($ds === '/') {
            $addEscapedSpace = str_replace(' ', '\\ ', $replaceSeparator);
            $addEscapedTab = str_replace('	', '\\	', $addEscapedSpace);
            $newPath = $addEscapedTab;
        } else {
            $newPath = $replaceSeparator;
        }

        return $newPath;
    }
}
