<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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
 * THIS CLASS IS FOR DEVELOPERS TO MAKE CUSTOMIZATIONS IN
 */
require_once('modules/AOD_Index/AOD_Index_sugar.php');
require_once('modules/AOD_Index/LuceneUtils.php');

/**
 * @deprecated since v7.12.0
 * Class AOD_Index
 */
#[\AllowDynamicProperties]
class AOD_Index extends AOD_Index_sugar
{
    /**
     * @deprecated since v7.12.0
     * AOD_Index constructor.
     */
    public function __construct()
    {
        parent::__construct();
        Zend_Search_Lucene_Search_QueryParser::setDefaultEncoding('utf-8');
        Zend_Search_Lucene_Analysis_Analyzer::setDefault(new Zend_Search_Lucene_Analysis_Analyzer_Common_Utf8Num_CaseInsensitive());
    }

    /**
     * @deprecated since v7.12.0
     * @return bool
     */
    public function isEnabled()
    {
        global $sugar_config;
        return !empty($sugar_config['aod']['enable_aod']);
    }

    /**
     * @deprecated since v7.12.0
     * @param $queryString
     * @return mixed
     */
    public function find($queryString)
    {
        $queryString = strtolower($queryString);
        $hits = $this->getLuceneIndex()->find($queryString);
        return $hits;
    }

    /**
     * @deprecated since v7.12.0
     */
    public function optimise()
    {
        if (!$this->isEnabled()) {
            return;
        }
        global $timedate;
        $this->getLuceneIndex()->optimize();
        $this->last_optimised = $timedate->getNow()->asDb();
        $this->save();
    }

    /**
     * @deprecated since v7.12.0
     * @return bool|SugarBean
     */
    public function getIndex()
    {
        $index = BeanFactory::getBean('AOD_Index', 1);
        if (!empty($index) && !empty($index->id)) {
            return $index;
        }
        $index = BeanFactory::newBean('AOD_Index');
        $index->id = 1;
        $index->new_with_id = true;
        $index->name = "Index";
        $index->location = "modules/AOD_Index/Index/Index";
        $index->save();
        return $index;
    }

    /**
     * @deprecated since v7.12.0
     * @param $revision
     * @return bool|Zend_Search_Lucene_Document
     */
    private function getDocumentForRevision($revision)
    {
        $path = getDocumentRevisionPath($revision->id);
        if (!file_exists($path)) {
            return array("error"=>"File not found");
        }
        //Convert the file to a lucene document
        $mime = $revision->file_mime_type;
        switch ($mime) {
            case 'application/pdf':
                $document = createPDFDocument($path);
                break;
            case 'application/msword':
                $document = createDocDocument($path);
                break;
            case 'application/vnd.oasis.opendocument.text':
                $document = createOdtDocument($path);
                break;
            case 'application/vnd.openxmlformats-officedocument.wordprocessingml.document':
                $document = createDocXDocument($path);
                break;
            case 'text/html':
                $document = createHTMLDocument($path);
                break;
            case 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet':
                $document = createXLSXDocument($path);
                break;
            case 'application/rtf':
                $document = createRTFDocument($path);
                // no break
            case 'text/csv':
            case 'text/plain':
                $document = createTextDocument($path);
                break;
            case 'application/vnd.openxmlformats-officedocument.presentationml.presentation':
                $document = createPPTXDocument($path);
                break;
            case 'application/vnd.oasis.opendocument.spreadsheet':
            case 'application/vnd.ms-powerpoint':
            case 'application/vnd.ms-excel':
            default:
                return array("error"=>"Mime type $mime not supported");
        }
        if (!$document) {
            return array("error"=>"Failed to parse document");
        }
        $document->addField(Zend_Search_Lucene_Field::text("filename", $revision->filename));
        return array("error"=>false,"document"=>$document);
    }

    /**
     * @deprecated since v7.12.0
     * @param SugarBean $bean
     * @return array|bool|string[]|Zend_Search_Lucene_Document
     */
    public function getDocumentForBean(SugarBean $bean)
    {
        if ($bean->module_name == 'DocumentRevisions') {
            $document = $this->getDocumentForRevision($bean);
        } else {
            $document = array("error"=>false,"document"=>new Zend_Search_Lucene_Document());
        }
        if ($document["error"]) {
            return $document;
        }
        $document["document"]->addField(Zend_Search_Lucene_Field::Keyword("aod_id", $bean->module_name." ".$bean->id));
        $document["document"]->addField(Zend_Search_Lucene_Field::UnIndexed("record_id", $bean->id));
        $document["document"]->addField(Zend_Search_Lucene_Field::UnIndexed("record_module", $bean->module_name));
        foreach ($GLOBALS['dictionary'][$bean->getObjectName()]['fields'] as $key => $field) {
            switch ($field['type']) {
                case "enum":
                    if (property_exists($bean, $key)) {
                        $document["document"]->addField(Zend_Search_Lucene_Field::Keyword($key, strtolower($bean->$key), 'UTF-8'));
                    }
                    break;

                case "multienum":
                    if (property_exists($bean, $key)) {
                        $vals = unencodeMultienum($bean->$key);
                        $document["document"]->addField(Zend_Search_Lucene_Field::unStored($key, strtolower(implode(" ", $vals)), 'UTF-8'));
                    }
                    break;
                case "name":
                case "phone":
                case "html":
                case "text":
                case "url":
                case "varchar":
                    if (property_exists($bean, $key)) {
                        $val = strtolower($bean->$key);
                    } else {
                        $val = '';
                    }
                    $field = Zend_Search_Lucene_Field::unStored($key, $val, 'UTF-8');
                    $field->boost = $this->getBoost($bean->module_name, $key);
                    $document["document"]->addField($field);
                    break;
                case "address":
                case "bool":
                case "currency":
                case "date":
                case "datetimecombo":
                case "decimal":
                case "float":
                case "iframe":
                case "int":
                case "radioenum":
                case "relate":
                default:
                    break;
            }
        }

        return $document;
    }

    /**
     * @deprecated since v7.12.0
     * @param $module
     * @param $field
     * @return float|int
     */
    private function getBoost($module, $field)
    {
        $fieldBoosts = array('name' =>0.5, 'first_name' => 0.5, 'last_name' => 0.5);
        $moduleBoosts = array('Accounts' => 0.5, 'Contacts' => 0.5, 'Leads' => 0.5, 'Opportunities' => 0.5);
        $boost = 1;
        if (!empty($fieldBoosts[$field])) {
            $boost += $fieldBoosts[$field];
        }
        if (!empty($moduleBoosts[$module])) {
            $boost += $moduleBoosts[$module];
        }
        return $boost;
    }

    /**
     * @deprecated since v7.12.0
     * @param $module
     * @param $beanId
     * @return bool|SugarBean
     */
    private function getIndexEvent($module, $beanId)
    {
        global $timedate;
        $indexEventBean = BeanFactory::getBean("AOD_IndexEvent");
        $indexEvents = $indexEventBean->get_full_list('', "aod_indexevent.record_id = '".$beanId."' AND aod_indexevent.record_module = '".$module."'");
        if ($indexEvents) {
            $indexEvent = $indexEvents[0];
            if ((is_countable($indexEvents) ? count($indexEvents) : 0) > 1) {
                $indexEventsCount = is_countable($indexEvents) ? count($indexEvents) : 0;
                for ($x = 1; $x < $indexEventsCount; $x++) {
                    $duplicateIE = $indexEvents[$x];
                    $duplicateIE->mark_deleted($duplicateIE->id);
                }
            }
        } else {
            $indexEvent = BeanFactory::newBean("AOD_IndexEvent");
            $indexEvent->record_id = $beanId;
            $indexEvent->record_module = $module;
        }
        /*
         * "Now" is cached in the SugarBean which means for long running processes (such as the indexing scheduler) that
         * the date_modified could be in the past. This caused issues when comparing the date modified of the event with that
         * of a bean. Here we explicitly set the date modified to be the current date.
         */
        $indexEvent->update_date_modified = false;
        $indexEvent->date_modified = $timedate->asDb(new DateTime());
        return $indexEvent;
    }

    public function commit()
    {
        if (!$this->isEnabled()) {
            return;
        }
        $this->getLuceneIndex()->commit();
    }

    /**
     * @deprecated since v7.12.0
     * @param $module
     * @param $beanName
     * @return bool
     */
    public static function isModuleSearchable($module, $beanName)
    {
        $whiteList = array("DocumentRevisions","Cases");
        if (in_array($module, $whiteList)) {
            return true;
        }
        $blackList = array("AOD_IndexEvent","AOD_Index","AOW_Actions","AOW_Conditions","AOW_Processed","SchedulersJobs");
        if (in_array($module, $blackList)) {
            return false;
        }
        $manager = new VardefManager();
        $manager->loadVardef($module, $beanName);
        if (empty($GLOBALS['dictionary'][$beanName]['unified_search'])) {
            return false;
        }
        return true;
    }

    /**
     * @deprecated since v7.12.0
     * @param $module
     * @param $beanId
     * @return bool|void
     */
    public function index($module, $beanId)
    {
        try {
            if (!$this->isEnabled()) {
                return;
            }

            LoggerManager::getLogger()->deprecated('AOD Lucene search is deprecated since v7.12.0');

            if (empty($GLOBALS['beanList'][$module])) {
                return false;
            }
            $bean_name = $GLOBALS['beanList'][$module];
            $bean = new $bean_name();
            if (!$bean || ! $bean instanceof SugarBean) {
                return false;
            }

            if (!self::isModuleSearchable($module, BeanFactory::getObjectName($module))) {
                return false;
            }

            $bean = $bean->retrieve($beanId);
            if (!$bean) {
                return false;
            }

            $indexEvent = $this->getIndexEvent($module, $beanId);
            $indexEvent->name = $bean->get_summary_text();

            $document = $this->getDocumentForBean($bean);
            //Index name, id, date, filename
            if (!$document['error']) {
                $this->remove($module, $beanId);
                $this->getLuceneIndex()->addDocument($document['document']);
                $indexEvent->success = true;
            } else {
                $indexEvent->success = false;
                $indexEvent->error = $document['error'];
            }
            $indexEvent->save();
        } catch (Exception $ex) {
            $GLOBALS['log']->error($ex->getMessage());
            return false;
        }
        return true;
    }

    /**
     * @deprecated since v7.12.0
     * @param $module
     * @param $beanId
     * @return string
     */
    private function getIdForDoc($module, $beanId)
    {
        return $module . " " . $beanId;
    }

    /**
     * @deprecated since v7.12.0
     * @param $module
     * @param $beanId
     */
    public function remove($module, $beanId)
    {
        $term  = new Zend_Search_Lucene_Index_Term($module.' '.$beanId, 'aod_id');
        $query = new Zend_Search_Lucene_Search_Query_Term($term);
        $hits = $this->getLuceneIndex()->find($query);
        foreach ($hits as $hit) {
            $this->getLuceneIndex()->delete($hit->id);
        }
    }

    /**
     * Returns a handle on the actual lucene index.
     * @deprecated since v7.12.0
     * @return Zend_Search_Lucene_Interface
     */
    private function getLuceneIndex()
    {
        if (file_exists($this->location)) {
            $this->index = new Zend_Search_Lucene($this->location);
        } else {
            $this->index = Zend_Search_Lucene::create($this->location);
        }
        $this->index->setMaxBufferedDocs(64);
        //$this->index->setMaxMergeDocs(50);
        Zend_Search_Lucene_Search_Query_Fuzzy::setDefaultPrefixLength(1);
        $this->index->setMergeFactor(5);
        return $this->index;
    }

    /**
     * @deprecated since v7.12.0
     * @return array
     */
    public function getIndexableModules()
    {
        $modules = array();
        $beanList = $GLOBALS['beanList'];
        ksort($beanList);
        foreach ($beanList as $beanModule => $beanName) {
            if (self::isModuleSearchable($beanModule, $beanName)) {
                $modules[$beanModule] = $beanName;
            }
        }
        return $modules;
    }
}
