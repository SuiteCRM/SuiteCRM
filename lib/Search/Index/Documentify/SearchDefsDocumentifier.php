<?php
/**
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

namespace SuiteCRM\Search\Index\Documentify;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use Exception;
use LoggerManager;
use Monolog\Logger;
use ParserSearchFields;
use SuiteCRM\Log\CliLoggerHandler;
use SuiteCRM\Log\SugarLoggerHandler;
use SuiteCRM\Utility\ArrayMapper;

require_once 'modules/ModuleBuilder/parsers/parser.searchfields.php';

/**
 * This class converts a SugarBean into a document using the customisable Search Defs framework.
 *
 * @see ParserSearchFields
 */
class SearchDefsDocumentifier extends AbstractDocumentifier
{
    /** @var array a cache with fields definition */
    protected $fields = [];
    /** @var ArrayMapper */
    protected $mapper = null;
    /** @var Logger */
    protected $logger;

    /**
     * SearchDefsDocumentifier constructor.
     */
    public function __construct()
    {
        try {
            $this->logger = new Logger('SearchDefsDocumentifier', [
                new CliLoggerHandler(),
                new SugarLoggerHandler(),
            ]);
        } catch (Exception $exception) {
            LoggerManager::getLogger()->error('Failed to start Monolog loggers');
        }

        $this->mapper = ArrayMapper::make()
            ->loadYaml(__DIR__ . '/SearchDefsDocumentifier.yml')
            ->setHideEmptyValues(true);
    }

    /** @inheritdoc */
    public function documentify(\SugarBean $bean, ParserSearchFields $parser = null)
    {
        $fields = &$this->getFieldsToIndexCached($bean, $parser);

        $body = &$this->parseBeans($bean, $fields);

        $body = $this->mapper
            ->setMappable($body)
            ->map();

        $this->fixPhone($body);
        $this->fixEmails($bean, $body);

        return $body;
    }

    /**
     * Parses the Search Defs files and creates a map of fields to index for a given module.
     *
     * The mapping is cached in the class property `$fields`.
     *
     * @param string                  $module
     * @param ParserSearchFields|null $parser
     *
     * @return string[]
     */
    protected function getFieldsToIndex($module, ParserSearchFields $parser = null)
    {
        if (empty($parser)) {
            $parser = new ParserSearchFields($module);
        }

        $fields = $parser->getSearchFields()[$module];

        $parsedFields = [];

        $badKeys = ['favorites_only', 'open_only', 'do_not_call', 'email', 'optinprimary'];
        $goodOperators = ['=', 'in'];

        foreach ($fields as $key => $field) {
            if (in_array($key, $badKeys)) {
                continue;
            }

            if (isset($field['query_type']) && $field['query_type'] != 'default') {
                $this->logger->warn("[$module]->$key is not a supported query type [{$field['query_type']}]");
                continue;
            };

            if (!empty($field['operator']) && !in_array($field['operator'], $goodOperators)) {
                $this->logger->warn("[$module]->$key has an unsupported operator [{$field['operator']}]");
                $this->logger->warn("field:\n" . json_encode($field, JSON_PRETTY_PRINT));
                continue;
            }

            if (strpos((string) $key, 'range_date') !== false) {
                continue;
            }

            if (empty($field['db_field'])) {
                $parsedFields[] = $key;
                continue;
            }

            foreach ($field['db_field'] as $db_field) {
                $parsedFields[$key][] = $db_field;
            }
        }

        // injects the standard metadata fields as they are not present in the searchdefs
        $parsedFields = array_merge($parsedFields, $this->getMetaData());

        return $parsedFields;
    }

    /**
     * Cached version of getFieldsToIndex().
     *
     * @see getFieldsToIndex
     *
     * @param \SugarBean         $bean
     * @param ParserSearchFields|null $parser
     *
     * @return array
     */
    private function &getFieldsToIndexCached(\SugarBean $bean, ParserSearchFields $parser = null)
    {
        $module_name = $bean->module_name;

        if (empty($this->fields[$module_name])) {
            $this->fields[$module_name] = $this->getFieldsToIndex($module_name, $parser);
        }

        return $this->fields[$module_name];
    }

    /**
     * @param \SugarBean $bean
     * @param array      $fields
     *
     * @return mixed
     */
    private function &parseBeans(\SugarBean $bean, array &$fields)
    {
        $body = [];

        foreach ($fields as $key => $value) {
            if (is_array($value)) {
                foreach ($value as $subvalue) {
                    if (property_exists($bean, $subvalue) && !empty($bean->$subvalue)) {
                        $body[$key][$subvalue] = $bean->$subvalue;
                    }
                }
                continue;
            }

            if (property_exists($bean, $value) && !empty($bean->$value)) {
                $body[$value] = $bean->$value;
            }
        }

        return $body;
    }
}
