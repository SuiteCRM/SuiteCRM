<?php
if (!defined('sugarEntry')) {
    define('sugarEntry', true);
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


require_once('service/core/REST/SugarRest.php');

/**
 * This class is a serialize implementation of REST protocol
 * @api
 */
class SugarRestRSS extends SugarRest
{
	/**
	 * It will serialize the input object and echo's it
	 *
	 * @param array $input - assoc array of input values: key = param name, value = param type
	 * @return String - echos serialize string of $input
	 */
	public function generateResponse($input)
	{
		global $app_strings;
        $app_strings = $app_strings ?? [];

        if(!isset($input['entry_list'])) {
		    $this->fault($app_strings['ERR_RSS_INVALID_RESPONSE']);
		}
		ob_clean();
		$this->generateResponseHeader(is_countable($input['entry_list']) ? count($input['entry_list']) : 0);
		$this->generateItems($input);
		$this->generateResponseFooter();
	} // fn

	protected function generateResponseHeader($count)
	{
	    global $app_strings, $sugar_version, $sugar_flavor;

		$date = TimeDate::httpTime();

		echo <<<EORSS
<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0">
<channel>
    <title>{$app_strings['LBL_RSS_FEED']} &raquo; {$app_strings['LBL_BROWSER_TITLE']}</title>
    <link>{$GLOBALS['sugar_config']['site_url']}</link>
    <description>{$count} {$app_strings['LBL_RSS_RECORDS_FOUND']}</description>
    <pubDate>{$date}</pubDate>
    <generator>SugarCRM $sugar_version $sugar_flavor</generator>

EORSS;
	}

	protected function generateItems($input)
	{
        global $app_strings;

	    if(!empty($input['entry_list'])){
            foreach($input['entry_list'] as $item){
                $this->generateItem($item);
            }
        }
    }

    protected function generateItem($item)
    {
        $name = !empty($item['name_value_list']['name']['value'])?htmlentities( (string) $item['name_value_list']['name']['value']): '';
        $url = $GLOBALS['sugar_config']['site_url']  . htmlentities('/index.php?module=' . $item['module_name']. '&action=DetailView&record=' . $item['id']);
        $date = TimeDate::httpTime(TimeDate::getInstance()->fromDb($item['name_value_list']['date_modified']['value'])->getTimestamp());
        $description = '';
        $displayFieldNames = true;
        if((is_countable($item['name_value_list']) ? count($item['name_value_list']) : 0) == 2 &&isset($item['name_value_list']['name']))$displayFieldNames = false;
        foreach($item['name_value_list'] as $k=>$v){
            if ( $k == 'name' || $k == 'date_modified') {
                continue;
            }
            if($displayFieldNames) $description .= '<b>' .htmlentities( (string) $k) . ':<b>&nbsp;';
            $description .= htmlentities( (string) $v['value']) . "<br>";
        }

        echo <<<EORSS
    <item>
        <title>$name</title>
        <link>$url</link>
        <description><![CDATA[$description]]></description>
        <pubDate>$date GMT</pubDate>
        <guid>{$item['id']}</guid>
    </item>

EORSS;
    }

    protected function generateResponseFooter()
    {
		echo <<<EORSS
</channel>
</rss>
EORSS;
	}

	/**
	 * Returns a fault since we cannot accept RSS as an input type
	 *
	 * @see SugarRest::serve()
	 */
	public function serve()
	{
	    global $app_strings;

	    $this->fault($app_strings['ERR_RSS_INVALID_INPUT']);
	}

	/**
	 * @see SugarRest::fault()
	 */
	public function fault($errorObject)
	{
		ob_clean();
		$this->generateResponseHeader();
		echo '<item><name>';
		if(is_object($errorObject)){
			$error = $errorObject->number . ': ' . $errorObject->name . '<br>' . $errorObject->description;
			$GLOBALS['log']->error($error);
		}else{
			$GLOBALS['log']->error(var_export($errorObject, true));
			$error = var_export($errorObject, true);
		} // else
		echo $error;
		echo '</name></item>';
		$this->generateResponseFooter();
	}
}
