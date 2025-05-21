<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

require_once('modules/stic_Import_Validation/sources/ImportDataSource.php');


#[\AllowDynamicProperties]
class ExternalSourceEAPMAdapter extends ImportDataSource
{

    /**
     * @var string The name of the EAPM object.
     */
    private $_eapmName = 'Google';

    /**
     * @var int Total record count of rows that will be imported
     */
    private $_totalRecordCount = -1;

    /**
     * @var The record set loaded from the external source
     */
    private $_recordSet = array();

    protected $_localeSettings = array('importlocale_charset' => 'UTF-8',
                                       'importlocale_dateformat' => 'Y-m-d',
                                       'importlocale_timeformat' => 'H:i',
                                       'importlocale_timezone' => '',
                                       'importlocale_currency' => '',
                                       'importlocale_default_currency_significant_digits' => '',
                                       'importlocale_num_grp_sep' => '',
                                       'importlocale_dec_sep' => '',
                                       'importlocale_default_locale_name_format' => '');


    public function __construct($eapmName)
    {
        global $current_user, $locale;
        $this->_eapmName = $eapmName;
      
        $this->_localeSettings['importlocale_num_grp_sep'] = $current_user->getPreference('num_grp_sep');
        $this->_localeSettings['importlocale_dec_sep'] = $current_user->getPreference('dec_sep');
        $this->_localeSettings['importlocale_default_currency_significant_digits'] = $locale->getPrecedentPreference('default_currency_significant_digits', $current_user);
        $this->_localeSettings['importlocale_default_locale_name_format'] = $locale->getLocaleFormatMacro($current_user);
        $this->_localeSettings['importlocale_currency'] = $locale->getPrecedentPreference('currency', $current_user);
        $this->_localeSettings['importlocale_timezone'] = $current_user->getPreference('timezone');

        $this->setSourceName();
    }
    /**
     * Return a feed of google contacts using the EAPM and Connectors farmework.
     *
     * @throws Exception
     * @param  $maxResults
     * @return array
     */
    public function loadDataSet($maxResults = 0)
    {
        if (!$eapmBean = EAPM::getLoginInfo($this->_eapmName, true)) {
            throw new Exception("Authentication error with {$this->_eapmName}");
        }

        $api = ExternalAPIFactory::loadAPI($this->_eapmName);
        $api->loadEAPM($eapmBean);
        $conn = $api->getConnector();

        $feed = $conn->getList(array('maxResults' => $maxResults, 'startIndex' => $this->_offset));
        if ($feed !== false) {
            $this->_totalRecordCount = $feed['totalResults'];
            $this->_recordSet = $feed['records'];
        } else {
            throw new Exception("Unable to retrieve {$this->_eapmName} feed.");
        }
    }

    public function getHeaderColumns()
    {
        return '';
    }
    
    public function getTotalRecordCount()
    {
        return $this->_totalRecordCount;
    }

    public function setSourceName($sourceName = '')
    {
        $this->_sourcename = $this->_eapmName;
    }

    //Begin Implementation for SPL's Iterator interface
    public function current()
    {
        $this->_currentRow =  current($this->_recordSet);
        return $this->_currentRow;
    }

    public function key()
    {
        return key($this->_recordSet);
    }
    
    public function rewind()
    {
        reset($this->_recordSet);
    }

    public function next()
    {
        $this->_rowsCount++;
        next($this->_recordSet);
    }

    public function valid()
    {
        return (current($this->_recordSet) !== false);
    }
}
