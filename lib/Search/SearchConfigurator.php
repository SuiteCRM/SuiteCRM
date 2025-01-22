<?php
/**
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

namespace SuiteCRM\Search;

use Configurator;
use InvalidArgumentException;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once __DIR__ . '/../../modules/Configurator/Configurator.php';

/**
 * Class SearchConfigurator handles the configuration calls for the Search Framework.
 *
 * All the methods are fluent and save() must be called at the end to make the changes permanent.
 */
#[\AllowDynamicProperties]
class SearchConfigurator
{
    /** @var Configurator */
    private $configurator;

    /**
     * SearchConfigurator constructor.
     *
     * @param null|Configurator $configurator
     */
    public function __construct(Configurator $configurator = null)
    {
        if ($configurator === null) {
            $configurator = new Configurator();
        }

        $this->configurator = $configurator;
    }

    /**
     * Factory method for nice fluent syntax.
     *
     * @return SearchConfigurator
     */
    public static function make(): SearchConfigurator
    {
        return new self();
    }

    /**
     * Configure the Search Framework configuration only based on the search engine.
     *
     * This supports the fake engine names used to refer to the legacy search.
     *
     * @param string $engine
     *
     * @return SearchConfigurator
     */
    public function setEngine(string $engine): SearchConfigurator
    {
        if (empty($engine)) {
            throw new InvalidArgumentException('Search Engine cannot be empty');
        }

        $searchController = 'UnifiedSearch';
        $enableAod = false;

        switch ($engine) {
            case 'BasicSearchEngine':
                // Only basic search
                break;
            case 'BasicAndAodEngine':
                // Basic search and AOD
                $enableAod = true;
                break;
            default:
                // SearchWrapper with a specific engine
                $searchController = 'Search';
        }

        $this->configurator->config['search']['controller'] = $searchController;
        $this->configurator->config['search']['defaultEngine'] = $engine;
        $this->configurator->config['aod']['enable_aod'] = $enableAod;

        return $this;
    }

    /**
     * Saves the current configuration.
     *
     * @return SearchConfigurator
     */
    public function save(): SearchConfigurator
    {
        $this->configurator->saveConfig();

        return $this;
    }
}
