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





#[\AllowDynamicProperties]
class SugarSecure
{
    public $results = array();
    public function display()
    {
        echo '<table>';
        foreach ($this->results as $result) {
            echo '<tr><td>' . nl2br($result) . '</td></tr>';
        }
        echo '</table>';
    }

    public function save($file='')
    {
        $fp = fopen($file, 'ab');
        foreach ($this->results as $result) {
            fwrite($fp, $result);
        }
        fclose($fp);
    }

    public function scan($path= '.', $ext = '.php')
    {
        $dir = dir($path);
        while ($entry = $dir->read()) {
            if (is_dir($path . '/' . $entry) && $entry != '.' && $entry != '..') {
                $this->scan($path .'/' . $entry);
            }
            if (is_file($path . '/'. $entry) && substr($entry, strlen($entry) - strlen((string) $ext), strlen((string) $ext)) == $ext) {
                $contents = file_get_contents($path .'/'. $entry);
                $this->scanContents($contents, $path .'/'. $entry);
            }
        }
    }

    public function scanContents($contents)
    {
        return;
    }
}

#[\AllowDynamicProperties]
class ScanFileIncludes extends SugarSecure
{
    public function scanContents($contents, $file = null)
    {
        $results = array();
        $found = '';
        /*preg_match_all("'(require_once\([^\)]*\\$[^\)]*\))'si", $contents, $results, PREG_SET_ORDER);
        foreach($results as $result){

        	$found .= "\n" . $result[0];
        }
        $results = array();
        preg_match_all("'include_once\([^\)]*\\$[^\)]*\)'si", $contents, $results, PREG_SET_ORDER);
        foreach($results as $result){
        	$found .= "\n" . $result[0];
        }
        */
        $results = array();
        preg_match_all("'require\([^\)]*\\$[^\)]*\)'si", (string) $contents, $results, PREG_SET_ORDER);
        foreach ($results as $result) {
            $found .= "\n" . $result[0];
        }
        $results = array();
        preg_match_all("'include\([^\)]*\\$[^\)]*\)'si", (string) $contents, $results, PREG_SET_ORDER);
        foreach ($results as $result) {
            $found .= "\n" . $result[0];
        }
        $results = array();
        preg_match_all("'require_once\([^\)]*\\$[^\)]*\)'si", (string) $contents, $results, PREG_SET_ORDER);
        foreach ($results as $result) {
            $found .= "\n" . $result[0];
        }
        $results = array();
        preg_match_all("'fopen\([^\)]*\\$[^\)]*\)'si", (string) $contents, $results, PREG_SET_ORDER);
        foreach ($results as $result) {
            $found .= "\n" . $result[0];
        }
        $results = array();
        preg_match_all("'file_get_contents\([^\)]*\\$[^\)]*\)'si", (string) $contents, $results, PREG_SET_ORDER);
        foreach ($results as $result) {
            $found .= "\n" . $result[0];
        }
        if (!empty($found)) {
            $this->results[] = $file . $found."\n\n";
        }
    }
}



#[\AllowDynamicProperties]
class SugarSecureManager
{
    public $scanners = array();
    public function registerScan($class)
    {
        $this->scanners[] = new $class();
    }

    public function scan()
    {
        while ($scanner = current($this->scanners)) {
            $scanner->scan();
            $scanner = next($this->scanners);
        }
        reset($this->scanners);
    }

    public function display()
    {
        while ($scanner = current($this->scanners)) {
            echo 'Scan Results: ';
            $scanner->display();
            $scanner = next($this->scanners);
        }
        reset($this->scanners);
    }

    public function save()
    {
        $scanner = null;
        //reset($this->scanners);
        $name = 'SugarSecure'. time() . '.txt';
        while ($this->scanners  = next($this->scanners)) {
            $scanner->save($name);
        }
    }
}
$secure = new SugarSecureManager();
$secure->registerScan('ScanFileIncludes');
$secure->scan();
$secure->display();
