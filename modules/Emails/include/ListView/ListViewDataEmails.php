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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use SuiteCRM\Utility\SuiteValidator;

require_once('include/ListView/ListViewData.php');

include_once 'include/Exceptions/SuiteException.php';
include_once 'modules/Emails/Folder.php';
include_once 'modules/Emails/include/ListView/ListViewDataEmailsSearchOnCrm.php';
include_once 'modules/Emails/include/ListView/ListViewDataEmailsSearchOnIMap.php';



#[\AllowDynamicProperties]
class ListViewDataEmails extends ListViewData
{


    /**
     * enum('crm', 'imap')
     *
     * @var string
     */
    protected $searchType;

    /**
     * @var array
     * when searching the imap filter map the crm fields
     * to the IMap fields.
     */
    protected static $mapServerFields = array(
        // bean field => IMap field
        'from_addr_name' => 'FROM',
        'to_addrs_names' => 'TO',
        'cc_addrs_names' => 'CC',
        'bcc_addrs_names' => 'BCC',
        'name' => 'SUBJECT',
        'subject' => 'SUBJECT',
        'description' => 'BODY',
        'imap_keywords' => 'KEYWORD'
    );

    /**
     * @var array
     * never select these fields during crm filter
     */
    protected static $mapIgnoreFields = array(
        'date_entered',
        'indicator',
        'flagged',
        'has_attachment',
        'imap_keywords'
    );

    /**
     * @var array
     * always include these fields during crm filter
     */
    protected static $alwaysIncludeSearchFields = array(
        'id',
        'flagged',
        'name',
        'subject',
        'has_attachment',
        'status',
    );

    /**
     * @var array
     * during crm filter map these fields so that the correct
     * SQL query is created
     */
    protected static $mapEmailFieldsToEmailTextFields = array(
        // emails field => email_text field
        'id' => 'emails.id',
        'from_addr_name' => 'emails_text.from_addr',
        'to_addrs_names' => 'emails_text.to_addrs',
        'cc_addrs_names' => 'emails_text.cc_addrs',
        'bcc_addrs_names' => 'emails_text.bcc_addrs',
        'description' => 'emails_text.description',
        'name' => 'name',
        'subject' => 'name',
        'has_attachment' => 'has_attachment',
        'status' => 'emails.status',
        'date_sent_received' => 'emails.date_sent_received'
    );


    /**
     * Constructor
     */
    public function __construct()
    {
        parent::__construct();
    }


    /**
     * @param User $currentUser
     * @param Folder $folder
     * @return InboundEmail
     * @throws SuiteException
     */
    protected function getInboundEmail($currentUser, $folder)
    {
        $inboundEmailID = $currentUser->getPreference('defaultIEAccount', 'Emails');
        $id = $folder->getId();
        if (!empty($id)) {
            $inboundEmailID = $folder->getId();
        }

        $isValidator = new SuiteValidator();
        if ($inboundEmailID && !$isValidator->isValidId($inboundEmailID)) {
            throw new SuiteException("Invalid Inbound Email ID" . ($inboundEmailID ? " ($inboundEmailID)" : ''));
        }

        /**
         * @var InboundEmail $inboundEmail
         */
        $inboundEmail = BeanFactory::getBean('InboundEmail', $inboundEmailID);

        if (!$inboundEmail) {
            throw new SuiteException("Error: InboundEmail not loaded (id:{$inboundEmailID})");
        }

        return $inboundEmail;
    }

    /**
     * set $inboundEmail->mailbox and return $this->searchType
     *
     * @param Folder $folder
     * @return string $this->searchType
     */
    protected function getSearchType($folder)
    {
        switch ($folder->getType()) {

            case "sent":
                $this->searchType = "imap";
                break;

            case "draft":
                $this->searchType = "crm";
                break;

            case "trash":
                $this->searchType = "imap";
                break;

            default:
                $GLOBALS['log']->warn("unknown or undefined folder type (we will use 'imap' instead): " . $folder->getType());
                $this->searchType = "imap";
                break;
        }

        return $this->searchType;
    }

    /**
     * @param Folder $folder
     * @param InboundEmail $inboundEmail
     */
    private function setInboundEmailMailbox(Folder $folder, InboundEmail $inboundEmail)
    {
        switch ($folder->getType()) {
            case "inbound":
                // Use the mailbox associated with $folder rather than the option string
                // Used in the IMAP connection string later
                $inboundEmail->mailbox = $folder->getMailbox();
                break;

            case "draft":
                $inboundEmail->mailbox = $inboundEmail->get_stored_options('draftFolder');
                break;

            case "sent":
                $inboundEmail->mailbox = $inboundEmail->get_stored_options('sentFolder');
                break;

            case "trash":
                $inboundEmail->mailbox = $inboundEmail->get_stored_options('trashFolder');
                break;

            default:
                $inboundEmail->mailbox = empty($folder->id) ? '' : $folder->getMailbox();
                break;
        }
    }


    /**
     * it returns filter fields,
     * set $this->searchType to 'crm' if any field is not IMap field
     *
     * @param array $filterFields
     * @param string $where
     * @param array $request
     * @return array
     */
    protected function getFilter($filterFields, $where, $request)
    {
        // Create a list of fields to filter and decide based on the field which type of filter to carry out

        $filter = array();

        // $searchType = "imap"; it searches the IMap headers and then searches the crm to see which messages are imported.
        // $searchType = "crm"; it uses the usual crm search and handles the indicator and attachment fields.

        if (!empty($where)) {
            foreach ($filterFields as $filteredField => $filteredFieldValue) {

                // Ignore blank fields
                if (empty($filteredField)) {
                    continue;
                }

                if (!isset($request[$filteredField.'_advanced']) && !isset($request[$filteredField.'_basic'])) {
                    continue;
                }

                if (empty($request[$filteredField.'_advanced']) && empty($request[$filteredField.'_basic'])) {
                    continue;
                }

                // strip out the suffix to the field names
                if ((stristr($filteredField, 'advanced') !== false) || (stristr($filteredField, 'basic') !== false)) {
                    $f = str_ireplace('_advanced', '', $filteredField);
                    $f = str_ireplace('_basic', '', $f);
                    if (isset(self::$mapServerFields[$f])) {
                        $filter[self::$mapServerFields[$f]] = $filteredFieldValue;
                    }

                    if (array_key_exists($filteredField, $f)) {
                        continue;
                    }

                    // if field name is not an IMap field
                    if (!array_key_exists($f, self::$mapServerFields)) {
                        $this->searchType = 'crm';
                    }
                } else {
                    // use the field names
                    if (in_array($filteredField, self::$mapIgnoreFields)) {
                        continue;
                    }

                    // if field name is not an IMap field
                    if (!array_key_exists($filteredField, self::$mapServerFields)) {
                        $this->searchType = 'crm';
                    } else {
                        if (!empty($request[$filteredField.'_advanced'])) {
                            $filter[self::$mapServerFields[$filteredField]] = $request[$filteredField.'_advanced'];
                        } else {
                            if (!empty($request[$filteredField.'_basic'])) {
                                $filter[self::$mapServerFields[$filteredField]] = $request[$filteredField.'_basic'];
                            } else {
                                $f = str_ireplace('_advanced', '', $filteredField);
                                $f = str_ireplace('_basic', '', $f);
                                $filter[self::$mapServerFields[$filteredField]] = $f;
                            }
                        }
                    }
                }
            }
        }

        return $filter;
    }


    /**
     * Fix fields in filter fields and repair sql $where
     *
     * @param array $filterFields
     * @param array $request $_REQUEST
     * @param string $where
     * @return array
     */
    public function fixFieldsInFilter($filterFields, $request, &$where)
    {
        // Fix fields in filter fields
        foreach (self::$mapEmailFieldsToEmailTextFields as $EmailSearchField => $EmailTextSearchField) {
            if (array_search($EmailSearchField, self::$alwaysIncludeSearchFields, true) !== false) {
                $filterFields[$EmailSearchField] = true;
                continue;
            } else {
                if (
                array_key_exists($EmailSearchField . '_advanced', $request) &&
                empty($request[$EmailSearchField . '_advanced'])
            ) {
                    $pos = array_search($EmailSearchField, $filterFields, true);
                    unset($filterFields[$pos]);
                    continue;
                } else {
                    if (
                array_key_exists($EmailSearchField . '_basic', $request) &&
                empty($request[$EmailSearchField . '_basic'])
            ) {
                        $pos = array_search($EmailSearchField, $filterFields, true);
                        unset($filterFields[$pos]);
                        continue;
                    }
                }
            }

            if (!array_key_exists($EmailSearchField, $filterFields)) {
                $filterFields[$EmailTextSearchField] = true;
            } else {
                $pos = array_search($EmailSearchField, $filterFields, true);
                if ($pos !== false) {
                    unset($filterFields[$pos]);
                    $filterFields[$EmailTextSearchField] = true;
                }
            }

            // since the where is hard coded at this point we need to map the fields in the where
            // clause of the SQL
            $where = str_replace($EmailSearchField, $EmailTextSearchField, $where);
        }

        return $filterFields;
    }


    /**
     * Populates CRM fields
     *
     * @param string $crmWhere
     * @param array $filterFields
     * @param array $params
     * @param Email $seed
     * @param bool $singleSelect
     * @return array|string
     */
    public function getCrmQueryArray($crmWhere, $filterFields, $params, $seed, $singleSelect)
    {
        $crmQueryArray = $seed->create_new_list_query(
            'id',
            $crmWhere,
            $filterFields,
            $params,
            0,
            '',
            true,
            $seed,
            $singleSelect
        );

        $crmQueryArray['inner_join'] = '';
        if (!empty($this->seed->listview_inner_join)) {
            $crmQueryArray['inner_join'] = ' ' . implode(' ', $this->seed->listview_inner_join) . ' ';
        }

        // force join with email_text
        $crmQueryArray['from'] .= ' LEFT JOIN emails_text ON emails_text.email_id = emails.id ';

        return $crmQueryArray;
    }


    /**
     * get crm emails query and fix params custom values
     *
     * @param array $crmQueryArray
     * @param array $params
     * @return string
     */
    public function getCrmEmailsQuery($crmQueryArray, &$params)
    {
        if (!is_array($params)) {
            $params = array();
        }

        if (!isset($params['custom_select'])) {
            $params['custom_select'] = '';
        }

        if (!isset($params['custom_from'])) {
            $params['custom_from'] = '';
        }

        if (!isset($params['custom_where'])) {
            $params['custom_where'] = '';
        }

        if (!isset($params['custom_order_by'])) {
            $params['custom_order_by'] = '';
        }

        $crmEmailsQuery = $crmQueryArray['select'] .
            $params['custom_select'] .
            $crmQueryArray['from'] .
            $params['custom_from'] .
            $crmQueryArray['inner_join'].
            $crmQueryArray['where'] .
            $params['custom_where'] .
            $crmQueryArray['order_by'] .
            $params['custom_order_by'];

        return $crmEmailsQuery;
    }


    /**
     * get email header status
     *
     * possible return variables:
     * enum('replied', 'draft', 'read', 'unread')
     *
     * @param array $emailHeader
     * @return string
     */
    protected function getEmailHeaderStatus($emailHeader)
    {
        if (isset($emailHeader['answered']) && $emailHeader['answered'] != 0) {
            $ret = 'replied';
        } else {
            if (isset($emailHeader['draft']) && $emailHeader['draft'] != 0) {
                $ret = 'draft';
            } else {
                if (isset($emailHeader['seen']) && $emailHeader['seen'] != 0) {
                    $ret = 'read';
                } else {
                    $ret = 'unread';
                }
            }
        }

        return $ret;
    }


    /**
     * @param string $field
     * @param array $emailHeader
     * @param InboundEmail $inboundEmail
     * @param User $currentUser
     * @param string $folder
     * @param Folder $folderObj
     * @return bool|string
     */
    protected function getEmailRecordFieldValue($field, $emailHeader, $inboundEmail, $currentUser, $folder, $folderObj)
    {
        switch ($field) {
            case 'from_addr_name':
                $ret = html_entity_decode((string) $inboundEmail->handleMimeHeaderDecode($emailHeader['from']));
                break;
            case 'to_addrs_names':
                $ret = mb_decode_mimeheader((string) $emailHeader['to']);
                break;
            case 'has_attachments':
                $ret = false;
                break;
            case 'flagged':
                $ret = $emailHeader['flagged'];
                break;
            case 'name':
                $ret = html_entity_decode(purify_html((string) $inboundEmail->handleMimeHeaderDecode($emailHeader['subject'])));
                break;
            case 'date_entered':
                $db = DBManagerFactory::getInstance();

                $ret = '';
                $uid = $emailHeader['uid'];

                $emailBean = BeanFactory::getBean('Emails');

                $emails = $emailBean->get_full_list(
                    '',
                    'emails.uid LIKE ' . $db->quoted($uid) . ' AND emails.mailbox_id = ' . $db->quoted($inboundEmail->id)
                );

                if (!empty($emails) && !empty($emails[0]->date_entered)) {
                    $date = preg_replace('/(\ \([A-Z]+\))/', '', (string) $emails[0]->date_entered);

                    $dateTime = DateTime::createFromFormat(
                        'Y-m-d H:i:s',
                        $date
                    );

                    if ($dateTime) {
                        $timeDate = new TimeDate();
                        $ret = $timeDate->asUser($dateTime, $currentUser);
                    }
                }
                break;
            case 'date_sent_received':
                if (!isset($emailHeader['date'])) {
                    LoggerManager::getLogger()->warn('Given email header does not contains date field.');
                    $ret = '';
                } else {
                    $ret = '';
                    $dateTime = false;

                    $date = preg_replace('/(\ \([A-Z]+\))/', '', (string) $emailHeader['date']);

                    $formats = array(
                        'D, d M Y H:i:s O',
                        'd M Y H:i:s O'
                    );

                    foreach ($formats as $format) {
                        $dateTime = DateTime::createFromFormat(
                            $format,
                            $date
                        );

                        if ($dateTime) {
                            $timeDate = new TimeDate();
                            $ret = $timeDate->asUser($dateTime, $currentUser);
                            break;
                        }
                    }
                }

                break;
            case 'is_imported':
                $db = DBManagerFactory::getInstance();

                $uid = $emailHeader['uid'];
                $importedEmailBeans = BeanFactory::getBean('Emails');
                $is_imported = $importedEmailBeans->get_full_list(
                    '',
                    'emails.uid LIKE ' . $db->quoted($uid) . ' AND emails.mailbox_id = ' . $db->quoted($inboundEmail->id)
                );

                if (null === $is_imported) {
                    $is_imported = [];
                }

                if (is_countable($is_imported)) {
                    $count = count($is_imported);
                } else {
                    LoggerManager::getLogger()->warn('ListViewDataEmails::getEmailRecordFieldValue: email list should be a Countable');
                    $count = count((array)$is_imported);
                }

                if ($count > 0) {
                    $ret = true;
                } else {
                    $ret = false;
                }
                break;
            case 'folder':
                $ret = $folder;
                break;
            case 'folder_type':
                $ret = $folderObj->getType();
                break;
            case 'inbound_email_record':
                $ret = $inboundEmail->id;
                break;
            case 'uid':
                $ret = $emailHeader['uid'];
                break;
            case 'msgno':
                $ret = $emailHeader['msgno'];
                break;
            case 'has_attachment':
                $ret = isset($emailHeader['has_attachment']) ? $emailHeader['has_attachment'] : false;
                break;
            case 'status':
                $ret = $this->getEmailHeaderStatus($emailHeader);
                break;
            default:
                $ret = '';
                break;
        }

        return $ret;
    }


    /**
     * @param Folder $folderObj
     * @param array $emailHeader
     * @param Email $seed
     * @param InboundEmail $inboundEmail
     * @param User $currentUser
     * @param string $folder
     * @return array|bool
     */
    public function getEmailRecord($folderObj, $emailHeader, $seed, $inboundEmail, $currentUser, $folder)
    {
        $emailRecord = array();

        if ($folderObj->getType() === 'draft' && $emailHeader['draft'] === 0) {
            return false;
        }

        foreach ($seed->column_fields as $c => $field) {
            $emailRecord[strtoupper($field)] = $this->getEmailRecordFieldValue($field, $emailHeader, $inboundEmail, $currentUser, $folder, $folderObj);
        }

        return $emailRecord;
    }

    /**
     * @param array $request $_REQUEST
     * @return bool
     */
    public function isRequestedSearchAdvanced($request)
    {
        return
            (isset($request["searchFormTab"]) && $request["searchFormTab"] == "advanced_search") ||
            (
                isset($request["type_basic"]) && ((is_countable($request["type_basic"]) ? count($request["type_basic"]) : 0) > 1 ||
                $request["type_basic"][0] != "")
            ) ||
            (isset($request["module"]) && $request["module"] == "MergeRecords");
    }

    /**
     * @param array $request $_REQUEST
     * @return bool
     */
    public function isRequestedSearchBasic($request)
    {
        return isset($request["searchFormTab"]) && $request["searchFormTab"] == "basic_search";
    }



    /**
     * @param array $data
     * @return array
     */
    public function getEmailUIds($data)
    {
        $emailUIds = array();

        foreach ((array)$data as $row) {
            $emailUIds[] = $row['UID'];
        }

        return $emailUIds;
    }

    /**
     * @inheritdoc
     */
    public function getListViewData(
        $seed,
        $where,
        $offset = -1,
        $limit = -1,
        $filter_fields = array(),
        $params = array(),
        $id_field = 'id',
        $singleSelect = true,
        $id = null
    ) {
        global $current_user;
        global $sugar_config;
        global $mod_strings;

        $data = array();
        $pageData = array();
        $queryString = 'basic_search';
        $ret = array(
            'data' => $data,
            'pageData' => $pageData,
            'query' => $queryString,
            'message' => $mod_strings['WARNING_SETTINGS_NOT_CONF'],
        );

        $request = $_REQUEST;

        try {
            $folderObj = new Folder();

            $folderObj->loadMailboxFolder($request ?? []);

            if (empty($folderObj->getId())) {
                LoggerManager::getLogger()->warn('Unable get Inbound Email for List View. Please check your settings and try again.');
                return false;
            }

            $inboundEmail = $this->getInboundEmail($current_user, $folderObj);
            if (!$inboundEmail || $inboundEmail && !$inboundEmail->id) {
                LoggerManager::getLogger()->warn('Unable get Inbound Email for List View. Please check your settings and try again.');
                return false;
            }


            $this->searchType = $this->getSearchType($folderObj);
            $this->setInboundEmailMailbox($folderObj, $inboundEmail);
            if (!$inboundEmail) {
                $folder = null;
                LoggerManager::getLogger()->warn('Unable to set Inbound Email mailbox: Inbound Email is not found.');
            }


            // search in draft in CRM db?

            if ($folderObj->getType() === 'draft' && !array_key_exists('status', $filter_fields)) {
                if (!empty($where)) {
                    $where .= ' AND ';
                }
                $where .= ' emails.status LIKE "draft" AND emails.deleted LIKE "0" ';
            }


            $folder = $inboundEmail->mailbox;
            if (!$inboundEmail) {
                $folder = null;
                LoggerManager::getLogger()->warn('Unable to retrive mailbox folder: Inbound Email is not found.');
            }

            $filter = $this->getFilter($filter_fields, $where, $request);


            // carry out the filter type
            switch ($this->searchType) {

                case 'crm':

                    $limit = $sugar_config['list_max_entries_per_page'];

                    $search = new ListViewDataEmailsSearchOnCrm($this);
                    $ret = $search->search(
                        $filter_fields,
                        $request,
                        $where,
                        $inboundEmail,
                        $params,
                        $seed,
                        $singleSelect,
                        $id,
                        $limit,
                        $current_user,
                        $id_field,
                        $offset
                    );

                    break;

                case 'imap':

                    $limitPerPage = isset($sugar_config['list_max_entries_per_page']) && (int)$sugar_config['list_max_entries_per_page'] ? $sugar_config['list_max_entries_per_page'] : 10;

                    $search = new ListViewDataEmailsSearchOnIMap($this);
                    $ret = $search->search(
                        $seed,
                        $request,
                        $where,
                        $id,
                        $inboundEmail,
                        $filter,
                        $folderObj,
                        $current_user,
                        $folder,
                        $limit,
                        $limitPerPage,
                        $params,
                        $pageData,
                        $filter_fields
                    );

                    break;

                default:

                    // handle default case

                    $GLOBALS['log']->fatal("Unknown or undefined search type" . ($this->searchType ? " ($this->searchType)" : ''));

                    break;
            }
        } catch (SuiteException $e) {
            $GLOBALS['log']->warn(
                'Exception (class ' . get_class($e) .
                ') message was: ' . $e->getMessage() .
                " (code: " . $e->getCode() .
                ")\ntrace info:\n" . $e->getTraceAsString()
            );
        }

        // TODO: don't override the superglobals!!!!
        $_REQUEST = $request;

        return $ret;
    }

    /**
     * generates queries for use by the display layer
     *
     * @param int $sortOrder
     * @param int $offset
     * @param int $prevOffset
     * @param int $nextOffset
     * @param int $endOffset
     * @param int $totalCounted
     * @return array of queries orderBy and baseURL are always returned the others are only returned  according to values passed in.
     */
    public function callGenerateQueries($sortOrder, $offset, $prevOffset, $nextOffset, $endOffset, $totalCounted)
    {
        return $this->generateQueries($sortOrder, $offset, $prevOffset, $nextOffset, $endOffset, $totalCounted);
    }

    /**
     * generates urls as a string for use by the display layer
     *
     * @param array $queries
     * @return array of urls orderBy and baseURL are always returned the others are only returned  according to values passed in.
     */
    public function callGenerateURLS($queries)
    {
        return $this->generateURLS($queries);
    }
}
