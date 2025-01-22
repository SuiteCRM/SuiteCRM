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
 * Class for parsing title from RSS feed, and keep default encoding (UTF-8)
 * Created: Sep 12, 2011
 */
#[\AllowDynamicProperties]
class DashletRssFeedTitle
{
    public $defaultEncoding = "UTF-8";
    public $readBytes = 8192;
    public $url;
    public $cut = 70;
    public $contents = "";
    public $title = "";
    public $endWith = "...";
    public $xmlEncoding = false;
    public $fileOpen = false;

    public function __construct($url)
    {
        $this->url = $url;
    }

    public function generateTitle()
    {
        if ($this->readFeed()) {
            $this->getTitle();
            if (!empty($this->title)) {
                $this->convertEncoding();
                $this->cutLength();
            }
        }
        return $this->title;
    }

    /**
     * @todo use curl with waiting timeout instead of fopen
     */
    public function readFeed()
    {
        if ($this->url) {
            if (!in_array(strtolower(parse_url((string) $this->url, PHP_URL_SCHEME)), array("http", "https"), true)) {
                return false;
            }
            $fileOpen = @fopen($this->url, 'rb');
            if ($fileOpen) {
                $this->fileOpen = true;
                $this->contents = fread($fileOpen, $this->readBytes);
                fclose($fileOpen);
                return true;
            }
        }
        return false;
    }

    /**
     *
     */
    public function getTitle()
    {
        $matches = array();
        preg_match("/<title>(.*?)<\/title>/i", (string) $this->contents, $matches);
        if (isset($matches[0])) {
            $match = $matches[0];
            if (isset($matches[1])) {
                $match = '<title>' . htmlentities($matches[1] ?? '') . '</title>';
            }
            $this->title = str_replace(array('<![CDATA[', '<title>', '</title>', ']]>'), '', $match);
        }
    }

    public function cutLength()
    {
        if (mb_strlen(trim($this->title), $this->defaultEncoding) > $this->cut) {
            $this->title = mb_substr($this->title, 0, $this->cut, $this->defaultEncoding) . $this->endWith;
        }
    }

    private function _identifyXmlEncoding()
    {
        $matches = array();
        preg_match('/encoding\=*\".*?\"/', (string) $this->contents, $matches);
        if (isset($matches[0])) {
            $this->xmlEncoding = trim(str_replace('encoding="', '"', $matches[0]), '"');
        }
    }

    public function convertEncoding()
    {
        $this->_identifyXmlEncoding();
        if ($this->xmlEncoding && $this->xmlEncoding != $this->defaultEncoding) {
            $this->title = iconv($this->xmlEncoding, $this->defaultEncoding, $this->title);
        }
    }
}
